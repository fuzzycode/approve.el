;;; approve-commands.el --- The commands that change a pull request -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/fuzzycode/approve.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; The commands of a PR buffer that change the pull request on GitHub
;; (docs/ui.org, "Keys"), each a run of the action pipeline (F00):
;;
;;   v   `approve-toggle-viewed'       F04  the file at point, viewed or not
;;   c   `approve-comment'             F05  on the lines, region or file at
;;                                          point; elsewhere on the PR (F10)
;;   s   `approve-suggest'             F06  a suggested change of the lines
;;   r   `approve-reply'               F07  to the thread at point; on a
;;                                          conversation comment, quote it
;;   R   `approve-resolve'             F07  resolve or unresolve the thread
;;   e   `approve-edit'                F08  your comment or review body
;;   k   `approve-delete'              F08  your comment
;;   C   `approve-comment-on-pr'       F10  in the conversation, quoting
;;   +   `approve-react'               F11  to the comment, review or
;;                                          description at point
;;   H   `approve-hide'                hide the comment or review at
;;                                          point, or show it again
;;
;; S (submit) and X (discard) act on the pending review as a whole:
;; approve-review.el and approve-menus.el.  Evil's keys are in
;; approve-evil.el.
;;
;; Each command starts with its preflight, `approve-commands--NAME-preflight':
;; what it acts on, or a `user-error' saying why it can't run here.
;; Its `approve-preflight' property names it, so the dispatch menu
;; dims the command where it can't run (`approve-action-why-not').
;;
;; Layer rules: this is the actions layer.  The commands read the
;; context at point and the store, and leave the rest to the pipeline
;; and the mutations, which keep the store true (decision D35).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-model)
(require 'approve-context)
(require 'approve-diff)
(require 'approve-input)
(require 'approve-mutations)
(require 'approve-actions)
(require 'approve-review)
(require 'approve-pr)
(require 'approve-nav)
(require 'approve-render)
(require 'approve-manage)

(defcustom approve-viewed-advance t
  "Whether marking a file viewed in the diff moves to the next one.
A file marked viewed is folded in the diff.  When it was marked from
its diff (its heading or a line of it) and this is non-nil, point
then moves to the next file that is not viewed, as in a queue; when
nil, it stays on the folded file.  Marking a file in the file list
moves nothing.

  (setq approve-viewed-advance nil)"
  :type 'boolean
  :group 'approve)

;;; Helpers

(defun approve-commands--location (target)
  "Return where TARGET is, such as \"a.go:13–14\", \"a.go:7\" or \"a.go\".
A range from a removed line to an added one says which side each end
is on, as GitHub does: \"a.go:-13–+14\"."
  (let ((path (plist-get target :path))
        (line (plist-get target :line))
        (start (plist-get target :start-line))
        (side (plist-get target :side))
        (start-side (or (plist-get target :start-side) (plist-get target :side))))
    (cond ((null line) path)
          ((and start (not (equal start-side side)))
           (format "%s:%s%s–%s%s" path (if (equal start-side "LEFT") "-" "+") start
                   (if (equal side "LEFT") "-" "+") line))
          ((and start (not (eql start line)))
           (format "%s:%s–%s" path start line))
          (t (format "%s:%s" path line)))))

(defun approve-commands--author (typename id)
  "Return the login of the author of the entity TYPENAME ID, or \"ghost\"."
  (or (alist-get 'login (approve-model-get typename id 'author)) "ghost"))

(defun approve-commands--body (typename id)
  "Return the markdown body of TYPENAME ID, with Unix line ends."
  (replace-regexp-in-string "\r\n" "\n" (or (approve-model-get typename id 'body) "")))

(defun approve-commands--quote (text)
  "Return TEXT quoted as markdown, followed by an empty line."
  (concat (mapconcat (lambda (line) (if (string-empty-p line) ">" (concat "> " line)))
                     (split-string (string-trim-right text) "\n")
                     "\n")
          "\n\n"))

(defun approve-commands--region-text ()
  "Return the text of the region as copying gives it, or nil.
Copying leaves out decorations such as line numbers (decision D28).
The region is deactivated: the command has taken it (and evil leaves
its visual state)."
  (when (use-region-p)
    (prog1 (filter-buffer-substring (region-beginning) (region-end))
      (deactivate-mark))))

(defun approve-commands--comment-entity (context)
  "Return (TYPENAME . ID) of the comment or review CONTEXT is on, or nil."
  (pcase (plist-get context :kind)
    ('issue-comment (cons "IssueComment" (plist-get context :comment-id)))
    ('review-comment (cons "PullRequestReviewComment" (plist-get context :comment-id)))
    ('review (cons "PullRequestReview" (plist-get context :review-id)))))

(defun approve-commands--thread-of (comment-id)
  "Return the id of the thread holding the review comment COMMENT-ID, or nil."
  (let ((comment (approve-model-make-ref "PullRequestReviewComment" comment-id)))
    (approve-model-ref-id
     (seq-find (lambda (thread)
                 (member comment (approve-model-nodes (approve-model-raw-field thread 'comments))))
               (approve-model-nodes (approve-model-raw-field (approve-model-root-ref)
                                                             'reviewThreads))))))

(defun approve-commands--show-at (section)
  "Move point to SECTION in every window of the buffer, showing it."
  (let ((parent (oref section parent)))
    (while parent
      (when (oref parent hidden)
        (magit-section-show parent))
      (setq parent (oref parent parent))))
  (goto-char (oref section start))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-point window (point))))

(defun approve-commands--goto-after-redraw (type value)
  "After the next redraw, move point to the section of TYPE with VALUE."
  (let ((buffer (current-buffer))
        (go nil))
    (setq go (lambda ()
               (remove-hook 'approve-pr-redraw-hook go t)
               (when-let* ((section (and magit-root-section
                                         (approve-commands--find-section type value))))
                 (with-current-buffer buffer
                   (approve-commands--show-at section)))))
    (add-hook 'approve-pr-redraw-hook go nil t)))

(defun approve-commands--find-section (type value)
  "Return the first section of TYPE whose value is VALUE, or nil."
  (let ((found nil))
    (magit-map-sections (lambda (section)
                          (when (and (not found) (eq (oref section type) type)
                                     (equal (oref section value) value))
                            (setq found section)))
                        magit-root-section)
    found))

;;; Viewed (v)

(defun approve-commands--viewed-state (path)
  "Return the viewed state of the changed file PATH."
  (alist-get 'viewerViewedState
             (seq-find (lambda (file) (equal (alist-get 'path file) path))
                       (approve-pr-list-fields 'files '(path viewerViewedState)))))

(defun approve-commands--file-section-at-point ()
  "Return the diff's file section point is in, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'approve-file-section)))
      (setq section (oref section parent)))
    section))

(defun approve-commands--fold-viewed (path move)
  "Fold the diff of PATH, now viewed; with MOVE, move on from it.
Moving on goes to the next unviewed file when
`approve-viewed-advance' says so, else to the folded file's heading."
  (when-let* ((section (approve-commands--find-section 'approve-file-section path)))
    (magit-section-hide section)
    (when move
      (approve-commands--show-at section)
      (when approve-viewed-advance
        (ignore-error user-error
          (approve-next-unviewed-file)
          (approve-commands--show-at (magit-current-section)))))))

(defun approve-commands--viewed-preflight ()
  "Return the context of the file `approve-toggle-viewed' acts on.
Signal a `user-error' when point is on no file."
  (approve-action-context (lambda (context)
                            (and (plist-get context :path)
                                 (memq (plist-get context :kind) '(file line lines thread))))
                          "Put point on a file, in the file list or the diff"))

(put 'approve-toggle-viewed 'approve-preflight #'approve-commands--viewed-preflight)

(defun approve-toggle-viewed ()
  "Mark the file at point viewed, or not viewed when it is (flow F04).
Point may be on the file in the file list, on its heading in the
diff, or on any line of its diff.  Marking it viewed from the diff
folds it; see `approve-viewed-advance'."
  (interactive)
  (let* ((context (approve-commands--viewed-preflight))
         (path (plist-get context :path))
         (viewed (equal (approve-commands--viewed-state path) "VIEWED"))
         (in-diff (and (approve-commands--file-section-at-point) t)))
    (approve-action-run
     :what (format "mark %s %s" path (if viewed "not viewed" "viewed"))
     :marks `((section approve-file-section ,path) (section approve-file-entry-section ,path))
     :send (lambda (_text callback errorback)
             (approve-mutation-set-file-viewed path (not viewed)
                                               :callback callback :errorback errorback))
     :done (lambda (_state)
             (unless viewed
               (approve-commands--fold-viewed path in-diff))))))

;;; Comments on code (c, s)

(defun approve-commands--check-region (context)
  "Signal a `user-error' when the region is on diff lines CONTEXT can't hold.
A region over diff lines gives a `lines' context only when it stays
in one file; it must stay in one hunk too (decision D32)."
  (when (use-region-p)
    (let ((on-diff (lambda (pos)
                     (save-excursion
                       (goto-char pos)
                       (get-text-property (line-beginning-position) 'approve-line)))))
      (cond
       ((eq (plist-get context :kind) 'lines)
        (unless (approve-diff-range-lines context)
          (user-error "A range must stay in one hunk of the diff")))
       ((or (funcall on-diff (region-beginning)) (funcall on-diff (region-end)))
        (user-error "A range must start and end on lines of one file's diff"))))))

(defun approve-commands--code-target (context)
  "Return the target of a comment on CONTEXT: lines, or a whole file."
  (if (eq (plist-get context :kind) 'file)
      (list :kind 'file :path (plist-get context :path))
    context))

(defun approve-commands--target-key (target)
  "Return the draft target of a comment on TARGET."
  (list 'comment (plist-get target :path) (plist-get target :side) (plist-get target :line)
        (plist-get target :start-side) (plist-get target :start-line)))

(defun approve-commands--comment-on-code (context single suggest)
  "Comment on the code of CONTEXT: a line, lines or a file.
SINGLE asks for a single comment; SUGGEST starts the text with a
suggested change of the lines."
  (deactivate-mark)
  (let* ((target (approve-commands--code-target context))
         (lines (and (memq (plist-get context :kind) '(line lines)) context))
         (location (approve-commands--location target))
         (path (plist-get target :path)))
    (approve-action-run
     :what (format "comment on %s" location)
     :input (list :prompt (format "%s on %s" (if suggest "Suggest a change" "Comment") location)
                  :context (approve-review-comment-note single)
                  :target (approve-commands--target-key target)
                  :name (format "comment %s" (replace-regexp-in-string "–" "-" location))
                  :lines lines :suggest suggest)
     :marks (if lines
                `((lines ,lines))
              `((section approve-file-section ,path) (section approve-file-entry-section ,path)))
     :send (lambda (text callback errorback)
             (approve-review-send-comment target text :single single
                                          :callback callback :errorback errorback)))))

(defun approve-commands--comment-preflight ()
  "Return the context `approve-comment' acts on.
On code (lines, a region over them, a file) it is that; elsewhere
the comment goes to the conversation.  Signal a `user-error' when
the region is on lines a comment can't hold, or, off code, when the
conversation is locked."
  (let ((context (approve-action-context)))
    (approve-commands--check-region context)
    (unless (memq (plist-get context :kind) '(line lines file))
      (approve-commands--conversation-preflight))
    context))

(put 'approve-comment 'approve-preflight #'approve-commands--comment-preflight)

(defun approve-comment (&optional prefix)
  "Comment on what is at point (flow F05).
On a line of the diff, or a region over lines of one hunk (evil's
visual lines too), comment on those lines; on a file, on the whole
file.  The comment joins your pending review or starts one; with a
PREFIX argument it is a single comment, published at once (see
`approve-comment-default').  Anywhere else, comment on the pull
request (`approve-comment-on-pr'), where PREFIX quotes the comment
at point."
  (interactive "P")
  (let ((context (approve-commands--comment-preflight)))
    (if (memq (plist-get context :kind) '(line lines file))
        (approve-commands--comment-on-code context (approve-review-single-p prefix) nil)
      (approve-comment-on-pr prefix))))

(defun approve-commands--suggest-preflight ()
  "Return the context of the lines `approve-suggest' acts on.
Signal a `user-error' when point is on no lines of the diff, or the
region is on lines a comment can't hold."
  (let ((context (approve-action-context
                  '(line lines) "Put point on added or unchanged lines of the diff")))
    (approve-commands--check-region context)
    context))

(put 'approve-suggest 'approve-preflight #'approve-commands--suggest-preflight)

(defun approve-suggest (&optional prefix)
  "Suggest a change of the lines at point, or in the region (flow F06).
The comment starts with a suggestion block holding their new
version, for you to edit into what it should be.  PREFIX is as for
`approve-comment'."
  (interactive "P")
  (approve-commands--comment-on-code (approve-commands--suggest-preflight)
                                     (approve-review-single-p prefix) t))

;;; Threads (r, R)

(defun approve-commands--thread-location (thread)
  "Return where THREAD, an alist from the store, is."
  (let-alist thread
    (approve-commands--location
     (list :path .path
           :line (and (not (equal .subjectType "FILE")) .line)
           :start-line .startLine :side .diffSide
           :start-side (or .startDiffSide .diffSide)))))

(defun approve-commands--reply-preflight ()
  "Return what `approve-reply' acts on, as a plist.
:context is the context at point.  On a thread or a comment in one,
:thread-id is the thread and :thread its fields; on a comment of the
conversation there is no thread, and the reply is a conversation
comment.  Signal a `user-error' when point is on neither, the
viewer may not reply to the thread, or the conversation is locked."
  (let ((context (approve-action-context '(thread review-comment issue-comment review)
                                         "Put point on a thread or a comment")))
    (if (memq (plist-get context :kind) '(issue-comment review))
        (progn (approve-commands--conversation-preflight)
               (list :context context))
      (let* ((thread-id (or (plist-get context :thread-id)
                            (approve-commands--thread-of (plist-get context :comment-id))
                            (user-error "That comment is in no thread")))
             (thread (approve-model-get "PullRequestReviewThread" thread-id
                                        '(path line startLine diffSide startDiffSide
                                               subjectType viewerCanReply comments))))
        (unless (alist-get 'viewerCanReply thread)
          (user-error "%s" "You can't reply to this thread"))
        (list :context context :thread-id thread-id :thread thread)))))

(put 'approve-reply 'approve-preflight #'approve-commands--reply-preflight)

(defun approve-reply (&optional quote)
  "Reply to the thread at point (flow F07).
With a QUOTE prefix argument, the reply starts by quoting the
comment at point; with an active region, by quoting the region.  On
a comment of the conversation, comment on the pull request quoting
it (`approve-comment-on-pr')."
  (interactive "P")
  (let* ((target (approve-commands--reply-preflight))
         (thread-id (plist-get target :thread-id)))
    (if (null thread-id)
        (approve-comment-on-pr t)
      (let* ((thread (plist-get target :thread))
             (comment-id (or (plist-get (plist-get target :context) :comment-id)
                             (alist-get 'id (car (approve-model-nodes
                                                  (alist-get 'comments thread))))))
             (location (approve-commands--thread-location thread))
             (quoted (or (approve-commands--region-text)
                         (and quote comment-id
                              (approve-commands--body "PullRequestReviewComment" comment-id)))))
        (approve-action-run
         :what "reply"
         :input (list :prompt (format "Reply to @%s on %s"
                                      (approve-commands--author "PullRequestReviewComment"
                                                                comment-id)
                                      location)
                      :context (if (approve-pr-pending-review) "pending review" "published at once")
                      :target (list 'reply thread-id)
                      :name (format "reply %s" (replace-regexp-in-string "–" "-" location))
                      :initial (and quoted (approve-commands--quote quoted)))
         :marks `((section approve-thread-section ,thread-id))
         :send (lambda (text callback errorback)
                 (approve-mutation-add-reply thread-id text
                                             :callback callback :errorback errorback)))))))

(defun approve-commands--resolve-preflight ()
  "Return (THREAD-ID . RESOLVED) of the thread `approve-resolve' acts on.
Signal a `user-error' when point is on no thread, or the viewer may
not resolve it (or unresolve it, when it is resolved)."
  (let* ((context (approve-action-context
                   (lambda (context) (plist-get context :thread-id))
                   "Put point on a thread"))
         (id (plist-get context :thread-id))
         (thread (approve-model-get "PullRequestReviewThread" id
                                    '(isResolved viewerCanResolve viewerCanUnresolve)))
         (resolved (alist-get 'isResolved thread)))
    (unless (alist-get (if resolved 'viewerCanUnresolve 'viewerCanResolve) thread)
      (user-error "%s" (format "You can't %s this thread" (if resolved "unresolve" "resolve"))))
    (cons id resolved)))

(put 'approve-resolve 'approve-preflight #'approve-commands--resolve-preflight)

(defun approve-resolve ()
  "Resolve the thread at point, or unresolve it when it is resolved (flow F07)."
  (interactive)
  (pcase-let ((`(,id . ,resolved) (approve-commands--resolve-preflight)))
    (approve-action-run
     :what (if resolved "unresolve the thread" "resolve the thread")
     :marks `((section approve-thread-section ,id))
     :send (lambda (_text callback errorback)
             (funcall (if resolved
                          #'approve-mutation-unresolve-thread
                        #'approve-mutation-resolve-thread)
                      id :callback callback :errorback errorback)))))

;;; Own text (e, k)

(defun approve-commands--comment-marks (typename id)
  "Return the marks of the comment or review TYPENAME ID."
  (pcase typename
    ("IssueComment" `((section approve-issue-comment-section ,id)))
    ("PullRequestReview" `((section approve-review-section ,id)))
    (_ `((section approve-comment-section ,id)
         (section approve-pending-comment-section ,id)))))

(defun approve-commands--edit-preflight ()
  "Return the context `approve-edit' acts on.
Signal a `user-error' when point is on no comment, review or
description, or on text the viewer may not edit."
  (let* ((context (approve-action-context
                   '(issue-comment review-comment review pending-review description)
                   "Put point on a comment or a review you wrote, or the description")))
    (pcase (plist-get context :kind)
      ('description (approve-manage--update-preflight))
      ('pending-review nil)
      (_ (pcase-let ((`(,typename . ,id) (approve-commands--comment-entity context)))
           (unless (approve-model-get typename id 'viewerCanUpdate)
             (user-error "%s" (format "You can't edit this %s"
                                      (if (equal typename "PullRequestReview")
                                          "review"
                                        "comment")))))))
    context))

(put 'approve-edit 'approve-preflight #'approve-commands--edit-preflight)

(defun approve-edit ()
  "Edit your comment or review body at point (flow F08).
The text starts as you wrote it; sending it unchanged sends nothing.
On the pending review, write the body you will submit it with
\(`approve-edit-review-body'); on the description, edit it
\(`approve-edit-description')."
  (interactive)
  (let ((context (approve-commands--edit-preflight)))
    (pcase (plist-get context :kind)
      ('pending-review (approve-edit-review-body))
      ('description (approve-edit-description))
      (_
       (pcase-let* ((`(,typename . ,id) (approve-commands--comment-entity context))
                    (review (equal typename "PullRequestReview"))
                    (body (approve-commands--body typename id)))
         (approve-action-run
          :what (if review "edit the review" "edit the comment")
          :input (list :prompt (if review "Edit the review" "Edit the comment")
                       :target (list 'edit id) :name (format "edit %s" id)
                       :initial body)
          :marks (approve-commands--comment-marks typename id)
          :send (lambda (text callback errorback)
                  (if (equal text (string-trim-right body))
                      (progn (message "Approve: the text is unchanged")
                             (funcall callback nil))
                    (funcall (pcase typename
                               ("IssueComment" #'approve-mutation-update-issue-comment)
                               ("PullRequestReview" #'approve-mutation-update-review)
                               (_ #'approve-mutation-update-review-comment))
                             id text :callback callback :errorback errorback)))))))))

(defun approve-commands--delete-preflight ()
  "Return (TYPENAME . ID) of the comment `approve-delete' acts on.
Signal a `user-error' when point is on no comment, or the viewer may
not delete it."
  (let ((entity (approve-commands--comment-entity
                 (approve-action-context '(issue-comment review-comment)
                                         "Put point on a comment"))))
    (unless (approve-model-get (car entity) (cdr entity) 'viewerCanDelete)
      (user-error "You can't delete this comment"))
    entity))

(put 'approve-delete 'approve-preflight #'approve-commands--delete-preflight)

(defun approve-delete ()
  "Delete the comment at point, after asking (flow F08).
Deleting the last comment of a review without a body deletes the
review too, as GitHub does."
  (interactive)
  (pcase-let* ((`(,typename . ,id) (approve-commands--delete-preflight))
               (comment (approve-model-get typename id '(viewerDidAuthor author state))))
    (when (y-or-n-p (format "Delete %s%s comment? "
                            (if (alist-get 'viewerDidAuthor comment)
                                "your"
                              (format "@%s's" (or (let-alist comment .author.login) "ghost")))
                            (if (equal (alist-get 'state comment) "PENDING") " pending" "")))
      (approve-action-run
       :what "delete the comment"
       :marks (approve-commands--comment-marks typename id)
       :send (lambda (_text callback errorback)
               (funcall (if (equal typename "IssueComment")
                            #'approve-mutation-delete-issue-comment
                          #'approve-mutation-delete-review-comment)
                        id :callback callback :errorback errorback))))))

;;; Reactions (+)

(defconst approve-commands--reactions
  '(("THUMBS_UP" . "thumbs up") ("THUMBS_DOWN" . "thumbs down") ("LAUGH" . "laugh")
    ("HOORAY" . "hooray") ("CONFUSED" . "confused") ("HEART" . "heart")
    ("ROCKET" . "rocket") ("EYES" . "eyes"))
  "GitHub's reactions, in its order, with their names: (CONTENT . NAME).")

(defun approve-commands--react-preflight ()
  "Return (TYPENAME . ID) of what `approve-react' reacts to.
Signal a `user-error' when point is on no comment, review or
description, or the viewer may not react to it."
  (let* ((context (approve-action-context
                   '(issue-comment review-comment review description)
                   "Put point on a comment, a review or the description"))
         (entity (if (eq (plist-get context :kind) 'description)
                     (cons "PullRequest" (approve-model-ref-id (approve-model-root-ref)))
                   (approve-commands--comment-entity context))))
    (unless (approve-model-get (car entity) (cdr entity) 'viewerCanReact)
      (user-error "%s" "You can't react to this"))
    entity))

(put 'approve-react 'approve-preflight #'approve-commands--react-preflight)

(defun approve-commands--reaction-note (given count)
  "Return the note of a reaction COUNT people gave, the viewer too when GIVEN.
Example: \"✓ you and 2 others\"."
  (let ((others (1- count)))
    (cond
     (given (concat (approve-manage-mark t) " "
                    (approve-manage-note
                     (cond ((<= others 0) "you")
                           ((= others 1) "you and 1 other")
                           (t (format "you and %d others" others))))))
     ((> count 0) (concat "  " (approve-manage-note
                                (if (= count 1) "1 person" (format "%d people" count))))))))

(defun approve-commands--react-prompt (typename id)
  "Return the prompt for reacting to TYPENAME ID."
  (pcase typename
    ("PullRequest" "React to the description: ")
    ("PullRequestReview" (format "React to @%s's review: "
                                 (approve-commands--author typename id)))
    (_ (format "React to @%s's comment: " (approve-commands--author typename id)))))

(defun approve-react ()
  "React to the comment, review or description at point (flow F11).
Choose one of GitHub's reactions; one you gave already, marked ✓,
is taken back."
  (interactive)
  (pcase-let* ((`(,typename . ,id) (approve-commands--react-preflight))
               (groups (approve-model-get typename id 'reactionGroups))
               (candidates
                (mapcar (lambda (reaction)
                          (let ((group (seq-find (lambda (group)
                                                   (equal (alist-get 'content group)
                                                          (car reaction)))
                                                 groups)))
                            (list (concat (approve-render-reaction-icon (car reaction)) "  "
                                          (cdr reaction))
                                  (car reaction)
                                  (alist-get 'viewerHasReacted group)
                                  (or (let-alist group .reactors.totalCount) 0))))
                        approve-commands--reactions))
               (choice (approve-manage-read-one
                        (approve-commands--react-prompt typename id)
                        (mapcar #'car candidates)
                        :notes (mapcar (lambda (candidate)
                                         (cons (car candidate)
                                               (approve-commands--reaction-note
                                                (nth 2 candidate) (nth 3 candidate))))
                                       candidates)
                        :category 'approve-reaction))
               (`(,_ ,content ,given ,_) (assoc choice candidates)))
    (approve-action-run
     :what (if given "take the reaction back" "react")
     :marks (if (equal typename "PullRequest")
                '((section approve-description-section nil))
              (approve-commands--comment-marks typename id))
     :send (lambda (_text callback errorback)
             (approve-mutation-react id content :remove given
                                     :callback callback :errorback errorback)))))

;;; Hidden comments (H)

(defconst approve-commands--hide-reasons
  '(("Spam" . "SPAM") ("Abuse" . "ABUSE") ("Off topic" . "OFF_TOPIC")
    ("Outdated" . "OUTDATED") ("Duplicate" . "DUPLICATE") ("Resolved" . "RESOLVED")
    ("Low quality" . "LOW_QUALITY"))
  "The reasons GitHub shows for a hidden comment: (NAME . CLASSIFIER).")

(defun approve-commands--hide-preflight ()
  "Return (TYPENAME ID HIDDEN) of the comment `approve-hide' acts on.
HIDDEN is non-nil when it is hidden, to be shown again.  Signal a
`user-error' when point is on no comment or review, or the viewer
may not hide it (or show it, when it is hidden)."
  (pcase-let* ((`(,typename . ,id)
                (approve-commands--comment-entity
                 (approve-action-context '(issue-comment review-comment review)
                                         "Put point on a comment or a review")))
               (comment (approve-model-get typename id
                                           '(isMinimized viewerCanMinimize viewerCanUnminimize)))
               (hidden (alist-get 'isMinimized comment)))
    (unless (alist-get (if hidden 'viewerCanUnminimize 'viewerCanMinimize) comment)
      (user-error "%s" (if hidden "You can't show this again" "You can't hide this")))
    (list typename id hidden)))

(put 'approve-hide 'approve-preflight #'approve-commands--hide-preflight)

(defun approve-hide ()
  "Hide the comment or review at point, or show it again when it is hidden.
Hiding asks for the reason GitHub shows instead of the text, such as
off topic or outdated.  Those who may moderate the repository can
hide what others wrote."
  (interactive)
  (pcase-let* ((`(,typename ,id ,hidden) (approve-commands--hide-preflight))
               (reason (unless hidden
                         (cdr (assoc (approve-manage-read-one
                                      (format "Hide @%s's %s, because: "
                                              (approve-commands--author typename id)
                                              (if (equal typename "PullRequestReview")
                                                  "review"
                                                "comment"))
                                      (mapcar #'car approve-commands--hide-reasons))
                                     approve-commands--hide-reasons)))))
    (approve-action-run
     :what (if hidden "show the comment" "hide the comment")
     :marks (approve-commands--comment-marks typename id)
     :send (lambda (_text callback errorback)
             (approve-mutation-set-hidden id (not hidden) :reason reason
                                          :callback callback :errorback errorback)))))

;;; The conversation (C)

(defun approve-commands--conversation-preflight ()
  "Return the context at point, where `approve-comment-on-pr' may comment.
Signal a `user-error' when the conversation is locked to the viewer."
  (prog1 (approve-action-context)
    (when (and (approve-pr-field 'locked) (not (approve-pr-viewer-can 'update)))
      (user-error "The conversation is locked"))))

(put 'approve-comment-on-pr 'approve-preflight #'approve-commands--conversation-preflight)

(defun approve-comment-on-pr (&optional quote)
  "Comment on the pull request, in its conversation (flow F10).
With an active region, the comment starts by quoting it; with a
QUOTE prefix argument, by quoting the comment at point.  Point goes
to the comment once GitHub has it."
  (interactive "P")
  (let* ((context (approve-commands--conversation-preflight))
         (entity (approve-commands--comment-entity context))
         (quoted (or (approve-commands--region-text)
                     (and quote entity (approve-commands--body (car entity) (cdr entity)))))
         (number (approve-pr-field 'number)))
    (approve-action-run
     :what "comment on the pull request"
     :input (list :prompt (format "Comment on #%s" number) :context "conversation"
                  :target '(conversation) :name (format "comment #%s" number)
                  :initial (and quoted (not (string-blank-p quoted))
                                (approve-commands--quote quoted)))
     :marks '((section approve-conversation-section nil))
     :send (lambda (text callback errorback)
             (approve-mutation-add-comment text :callback callback :errorback errorback))
     :done (lambda (comment)
             (approve-commands--goto-after-redraw 'approve-issue-comment-section
                                                  (approve-model-ref-id comment))))))

;;; Keys

(keymap-set approve-pr-mode-map "v" #'approve-toggle-viewed)
(keymap-set approve-pr-mode-map "c" #'approve-comment)
(keymap-set approve-pr-mode-map "s" #'approve-suggest)
(keymap-set approve-pr-mode-map "r" #'approve-reply)
(keymap-set approve-pr-mode-map "R" #'approve-resolve)
(keymap-set approve-pr-mode-map "e" #'approve-edit)
(keymap-set approve-pr-mode-map "k" #'approve-delete)
(keymap-set approve-pr-mode-map "C" #'approve-comment-on-pr)
(keymap-set approve-pr-mode-map "+" #'approve-react)
(keymap-set approve-pr-mode-map "H" #'approve-hide)

(provide 'approve-commands)

;;; approve-commands.el ends here
