;;; approve-suggestions.el --- Commit the changes reviewers suggest -*- lexical-binding: t; -*-

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

;; A reviewer's suggested change is committed to the pull request's
;; branch from Emacs, as GitHub's "Commit suggestion" and "Add
;; suggestion to batch" do (decision D41, flow F23):
;;
;;   x   `approve-commit-suggestion'   commit the suggestion at point, or
;;                                     the batch when there is one
;;   b   `approve-batch-suggestion'    add the suggestion at point to the
;;                                     batch, or take it out; with a
;;                                     prefix argument, empty the batch
;;
;; A suggestion is a fenced block whose info string starts with
;; "suggestion", in a published comment of a thread on the new side
;; that is not outdated.  `approve-suggestions-parse' finds the blocks
;; in a comment's markdown; GitHub's bodyHTML draws each as a box,
;; which approve-html marks with its place (the context at point is
;; then a `suggestion').  A suggestion is named by its comment and that
;; place: (COMMENT-ID . INDEX).
;;
;; Committing fetches the files at the head the store holds, replaces
;; each suggestion's lines with its block from the bottom up (the file
;; keeps its line ends and its last newline, or lack of one), commits
;; them in one commit with GitHub's message and Co-authored-by trailers
;; for the other suggesters (`approve-mutation-commit-files'), resolves
;; the threads, and loads the PR again.  The commit message is written
;; in an input buffer first, unless a prefix argument takes the
;; default.
;;
;; The batch is the session's :suggestions.  The boxes of the
;; suggestions say whether they are in it, and how to commit them, by
;; overlays put back after each redraw; the header line counts it.
;;
;; Layer rules: this is the actions layer.  It reads the context at
;; point, the store and the session, and leaves the writes to the
;; pipeline (approve-actions.el) and the operations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'approve-model)
(require 'approve-session)
(require 'approve-context)
(require 'approve-diff)
(require 'approve-faces)
(require 'approve-render)
(require 'approve-queries)
(require 'approve-mutations)
(require 'approve-actions)
(require 'approve-pr)
(require 'approve-eldoc)

;;; Finding suggestions in markdown

(defun approve-suggestions--fence (line)
  "Return (INDENT CHAR LENGTH INFO) if LINE opens a code fence, else nil.
A fence is three or more backticks or tildes, indented by at most
three spaces; a backtick fence's info string has no backtick."
  (when (string-match "\\`\\( \\{0,3\\}\\)\\(`\\{3,\\}\\|~\\{3,\\}\\)\\(.*\\)\\'" line)
    (let ((indent (length (match-string 1 line)))
          (fence (match-string 2 line))
          (info (string-trim (match-string 3 line))))
      (unless (and (eq (aref fence 0) ?`) (string-search "`" info))
        (list indent (aref fence 0) (length fence) info)))))

(defun approve-suggestions--closes-p (line char length)
  "Return non-nil if LINE closes a fence of CHAR at least LENGTH long."
  (string-match-p (format "\\` \\{0,3\\}%s\\{%d,\\}[ \t]*\\'"
                          (regexp-quote (string char)) length)
                  line))

(defun approve-suggestions-parse (body)
  "Return the suggested changes in BODY, a comment's markdown, in order.
Each is the list of its lines, nil for an empty suggestion (which
deletes the lines).  A suggestion is a fenced code block whose info
string starts with the word \"suggestion\", as GitHub reads it:
backticks or tildes, three or more, and a block left open runs to
the end.  Other code blocks are skipped whole, so a suggestion
quoted inside one is none.

Example:
  (approve-suggestions-parse \"Use this:\\n```suggestion\\n(cl-incf n)\\n```\")
  => ((\"(cl-incf n)\"))"
  (let ((lines (split-string (replace-regexp-in-string "\r\n" "\n" (or body "")) "\n"))
        (found nil))
    (while lines
      (let ((fence (approve-suggestions--fence (pop lines))))
        (when fence
          (pcase-let ((`(,indent ,char ,length ,info) fence)
                      (content nil))
            (while (and lines (not (approve-suggestions--closes-p (car lines) char length)))
              (let ((line (pop lines)))
                (string-match "\\` *" line)
                (push (substring line (min indent (match-end 0))) content)))
            (pop lines)
            (when (equal (car (split-string info)) "suggestion")
              (push (nreverse content) found))))))
    (nreverse found)))

(defun approve-suggestions--html-count (html)
  "Return how many suggested changes GitHub drew in HTML."
  (let ((count 0) (start 0))
    (while (string-match "js-suggested-changes-blob" (or html "") start)
      (setq count (1+ count) start (match-end 0)))
    count))

;;; Changing a file's text

(defun approve-suggestions--split (text)
  "Return TEXT's lines, each (CONTENT . END), END its line end.
The last line's END is \"\" when TEXT does not end with a newline;
TEXT ending with one has no empty line after it."
  (let ((lines nil) (start 0))
    (while (string-match "\r?\n" text start)
      (push (cons (substring text start (match-beginning 0)) (match-string 0 text)) lines)
      (setq start (match-end 0)))
    (when (< start (length text))
      (push (cons (substring text start) "") lines))
    (nreverse lines)))

(defun approve-suggestions-lines (text start end)
  "Return lines START to END of TEXT, counted from 1, without their ends.
Nil when they are not all in TEXT."
  (let ((lines (approve-suggestions--split text)))
    (when (and (<= 1 start end) (<= end (length lines)))
      (mapcar #'car (seq-subseq lines (1- start) end)))))

(defun approve-suggestions-splice (text edits)
  "Return TEXT with EDITS made, each (START END LINES).
Lines START to END, counted from 1, are replaced by LINES, a list of
strings without line ends; no LINES deletes them.  The new lines end
as the lines they replace do (\"\\r\\n\" stays), and the file keeps
its last newline, or the lack of one.  EDITS must not overlap; they
are made from the bottom up, so each one's numbers are TEXT's.
Signal an error when an edit is outside TEXT.

Example:
  (approve-suggestions-splice \"a\\r\\nb\\r\\nc\" \\='((2 3 (\"B\"))))
  => \"a\\r\\nB\""
  (let* ((lines (approve-suggestions--split text))
         (count (length lines)))
    (dolist (edit (sort (copy-sequence edits) (lambda (a b) (> (car a) (car b)))))
      (pcase-let ((`(,start ,end ,new) edit))
        (unless (and (<= 1 start end) (<= end count))
          (error "Lines %d to %d are not in the file, which has %d" start end count))
        (let* ((old (seq-subseq lines (1- start) end))
               (last-end (cdr (car (last old))))
               (line-end (or (seq-find (lambda (end) (not (string-empty-p end)))
                                       (mapcar #'cdr old))
                             (seq-find (lambda (end) (not (string-empty-p end)))
                                       (mapcar #'cdr lines))
                             "\n"))
               (replacement (cl-loop for (line . more) on new
                                     collect (cons line (if more line-end last-end)))))
          (setq lines (append (seq-take lines (1- start)) replacement (seq-drop lines end)))
          ;; Deleting the last line, which had no newline, leaves the
          ;; line before it last, without one too.
          (when (and (null new) (= end count) (string-empty-p last-end) (> start 1))
            (setcdr (car (last lines)) ""))
          (setq count (length lines)))))
    (mapconcat (lambda (line) (concat (car line) (cdr line))) lines "")))

;;; Suggestions in the store

(defun approve-suggestions--thread-of (comment-id)
  "Return the id of the thread holding the review comment COMMENT-ID, or nil."
  (let ((comment (approve-model-make-ref "PullRequestReviewComment" comment-id)))
    (approve-model-ref-id
     (seq-find (lambda (thread)
                 (member comment (approve-model-nodes
                                  (approve-model-raw-field thread 'comments))))
               (approve-model-nodes (approve-model-raw-field (approve-model-root-ref)
                                                             'reviewThreads))))))

(defun approve-suggestions-get (id &optional thread-id)
  "Return the suggestion ID, (COMMENT-ID . INDEX), as a plist, or nil.
THREAD-ID is its thread when known.  The plist has :id, :comment
\(the comment's fields), :thread (its thread's fields), :lines (the
suggested lines, or :missing when the comment's text has no such
block), :path, :start and :end (the lines it replaces).  Nil when
the comment is not in the store."
  (pcase-let ((`(,comment-id . ,index) id))
    (when (approve-model-has-entity-p "PullRequestReviewComment" comment-id)
      (let* ((comment (approve-model-get "PullRequestReviewComment" comment-id
                                         '(id body bodyHTML state author viewerDidAuthor
                                              url)))
             (thread-id (or thread-id (approve-suggestions--thread-of comment-id)))
             (thread (and thread-id
                          (approve-model-has-entity-p "PullRequestReviewThread" thread-id)
                          (approve-model-get "PullRequestReviewThread" thread-id
                                             '(id path line startLine diffSide startDiffSide
                                                  subjectType isOutdated isResolved))))
             (blocks (approve-suggestions-parse (alist-get 'body comment)))
             (drawn (approve-suggestions--html-count (alist-get 'bodyHTML comment))))
        (let-alist thread
          (list :id id :comment comment :thread thread
                :lines (if (and (< index (length blocks)) (= drawn (length blocks)))
                           (nth index blocks)
                         :missing)
                :path .path :start (or .startLine .line) :end .line))))))

(defun approve-suggestions--pr-why-not ()
  "Return why no suggestion can be committed to this PR, or nil."
  (let-alist (approve-pr-fields '(state viewerCanApplySuggestion headRepository))
    (cond ((equal .state "MERGED") "Suggestions can't be applied: the pull request is merged")
          ((not (equal .state "OPEN")) "Suggestions can't be applied: the pull request is closed")
          ((not .viewerCanApplySuggestion) "You can't change the pull request's branch")
          ((null .headRepository) "The pull request's branch is gone"))))

(defun approve-suggestions--unchanged-p (suggestion current)
  "Return non-nil if SUGGESTION's lines are CURRENT, the lines it replaces.
Line ends don't count."
  (equal (mapcar (lambda (line) (string-trim-right line "\r")) current)
         (plist-get suggestion :lines)))

(defun approve-suggestions--patch-lines (suggestion)
  "Return the lines SUGGESTION replaces as the diff has them, or nil.
Nil when the diff does not hold them all (its patch has not loaded,
or they are not in one hunk)."
  (when-let* ((range (approve-diff-range-lines
                      (list :path (plist-get suggestion :path)
                            :side "RIGHT" :line (plist-get suggestion :end)
                            :start-side "RIGHT" :start-line (plist-get suggestion :start))))
              (lines (mapcar (lambda (line) (nth 3 line))
                             (seq-filter (lambda (line) (memq (car line) '(add ctx))) range))))
    (and (= (length lines) (1+ (- (plist-get suggestion :end) (plist-get suggestion :start))))
         lines)))

(defun approve-suggestions-why-not (suggestion)
  "Return why SUGGESTION, from `approve-suggestions-get', can't be committed.
Nil when it can, as far as the store tells: GitHub's reasons, in its
words where it has them (decision D41).  The text of the file is
checked again when the suggestion is committed."
  (let ((comment (plist-get suggestion :comment))
        (thread (plist-get suggestion :thread)))
    (or (approve-suggestions--pr-why-not)
        (and (equal (alist-get 'state comment) "PENDING")
             "This suggestion is part of a pending comment and cannot be applied")
        (and (null thread) "This suggestion is in no thread")
        (let-alist thread
          (cond ((equal .subjectType "FILE") "A suggestion on a whole file can't be applied")
                ((or .isOutdated (null .line))
                 "This suggestion is outdated: its lines have changed since")
                ((or (equal .diffSide "LEFT") (equal .startDiffSide "LEFT"))
                 "Unable to apply suggestions on deleted lines")))
        (and (eq (plist-get suggestion :lines) :missing)
             "Approve can't find this suggestion in the comment's text")
        (when-let* ((current (approve-suggestions--patch-lines suggestion)))
          (and (approve-suggestions--unchanged-p suggestion current)
               "This suggestion is invalid because no changes were made to the code")))))

(defun approve-suggestions--overlap-p (a b)
  "Return non-nil if suggestions A and B change a line in common."
  (and (equal (plist-get a :path) (plist-get b :path))
       (<= (plist-get a :start) (plist-get b :end))
       (<= (plist-get b :start) (plist-get a :end))))

;;; The commit message

(defun approve-suggestions--login (author)
  "Return the login of AUTHOR as Git and GitHub's mentions write it.
A bot's has \"[bot]\" after it, as its noreply address does."
  (let-alist author
    (if (equal .__typename "Bot") (concat .login "[bot]") .login)))

(defun approve-suggestions--trailer (author)
  "Return the Co-authored-by trailer of AUTHOR, or nil without a database id.
The address is AUTHOR's noreply one, which GitHub always knows
\(decision D41)."
  (let-alist author
    (when (and .login .databaseId)
      (let ((login (approve-suggestions--login author)))
        (format "Co-authored-by: %s <%s+%s@users.noreply.github.com>"
                (if (and .name (not (string-empty-p .name))) .name login)
                .databaseId login)))))

(defun approve-suggestions-message (suggestions)
  "Return the default commit message of SUGGESTIONS, as GitHub writes it.
One suggestion's is \"Apply suggestion from @login\", a batch's
\"Apply batched suggestions from code review\".  A Co-authored-by
trailer follows for each suggester but the viewer, who is the
commit's author (decision D41)."
  (let* ((authors (mapcar (lambda (suggestion)
                            (alist-get 'author (plist-get suggestion :comment)))
                          suggestions))
         (first (car authors))
         (headline (cond ((cdr suggestions) "Apply batched suggestions from code review")
                         ((alist-get 'login first)
                          (format "Apply suggestion from @%s"
                                  (approve-suggestions--login first)))
                         (t "Apply suggestion from code review")))
         (trailers (delete-dups
                    (delq nil (cl-mapcar (lambda (suggestion author)
                                           (unless (alist-get 'viewerDidAuthor
                                                              (plist-get suggestion :comment))
                                             (approve-suggestions--trailer author)))
                                         suggestions authors)))))
    (if trailers
        (concat headline "\n\n" (string-join trailers "\n") "\n")
      headline)))

;;; The batch

(defun approve-suggestions-batch ()
  "Return the ids of the suggestions in the batch, in the order added."
  (approve-session-get :suggestions))

(defun approve-suggestions-batched-at-point-p ()
  "Return non-nil if the suggestion at point is in the batch."
  (when-let* ((batch (approve-suggestions-batch))
              (id (ignore-error user-error
                    (approve-suggestions--at-point-id (approve-suggestions--context)))))
    (and (member id batch) t)))

(defun approve-suggestions--set-batch (ids)
  "Make IDS the batch, and show it."
  (approve-session-put :suggestions ids)
  (with-current-buffer (approve-model-owner-buffer)
    (when (derived-mode-p 'approve-pr-mode)
      (approve-pr-redraw))))

;;; Committing

(defun approve-suggestions--fetch (paths head callback errorback)
  "Fetch the text of each of PATHS at HEAD, exactly as it is.
CALLBACK gets an alist (PATH . TEXT) once all have come; ERRORBACK the
first error."
  (let ((texts nil)
        (left (length paths))
        (failed nil))
    (dolist (path paths)
      (approve-query-blob
       head path :exact t
       :callback (lambda (text)
                   (unless failed
                     (push (cons path text) texts)
                     (when (zerop (cl-decf left))
                       (funcall callback texts))))
       :errorback (lambda (err)
                    (unless failed
                      (setq failed t)
                      (funcall errorback err)))))))

(defun approve-suggestions--files (suggestions texts)
  "Return the new text of each file SUGGESTIONS change, as (PATH . TEXT).
TEXTS is the alist (PATH . TEXT) of their text at the head.  Signal
`approve-mutation-refused' when a suggestion's lines are not in its
file, or are what it suggests already."
  (mapcar
   (lambda (path)
     (let ((text (cdr (assoc path texts)))
           (mine (seq-filter (lambda (suggestion) (equal (plist-get suggestion :path) path))
                             suggestions)))
       (dolist (suggestion mine)
         (let ((current (approve-suggestions-lines text (plist-get suggestion :start)
                                                   (plist-get suggestion :end))))
           (cond ((null current)
                  (signal 'approve-mutation-refused
                          (list (format "Lines %s to %s are not in %s"
                                        (plist-get suggestion :start)
                                        (plist-get suggestion :end) path))))
                 ((approve-suggestions--unchanged-p suggestion current)
                  (signal 'approve-mutation-refused
                          (list "This suggestion is invalid because no changes were made to the code"))))))
       (cons path (approve-suggestions-splice
                   text (mapcar (lambda (suggestion)
                                  (list (plist-get suggestion :start) (plist-get suggestion :end)
                                        (plist-get suggestion :lines)))
                                mine)))))
   (delete-dups (mapcar (lambda (suggestion) (plist-get suggestion :path)) suggestions))))

(defun approve-suggestions--resolve (thread-ids callback errorback)
  "Resolve THREAD-IDS one after the other, then call CALLBACK.
ERRORBACK gets the first error; the threads after it stay as they are."
  (if (null thread-ids)
      (funcall callback)
    (approve-mutation-resolve-thread
     (car thread-ids)
     :callback (lambda (_thread)
                 (approve-suggestions--resolve (cdr thread-ids) callback errorback))
     :errorback errorback)))

(cl-defun approve-suggestions-commit (suggestions message &key callback errorback)
  "Commit SUGGESTIONS to the PR's branch with MESSAGE, in one commit.
SUGGESTIONS are plists from `approve-suggestions-get'.  The files they
change are fetched at the head the store holds, their lines replaced,
and committed (`approve-mutation-commit-files'); then their threads
that are not resolved yet are resolved, as GitHub's button does.
CALLBACK gets the commit's oid once GitHub's PR has it as its head
\(`approve-query-await-head'); the caller loads the PR again, which
shows the commit and the threads gone outdated.  ERRORBACK gets the
first error: GitHub's, or `approve-mutation-refused' when a
suggestion changes nothing, and nothing is committed then.  A thread
that could not be resolved after the commit says so in the error."
  (let ((errorback (or errorback #'approve-api-default-errorback))
        (buffer (current-buffer)))
    (approve-suggestions--fetch
     (delete-dups (mapcar (lambda (suggestion) (plist-get suggestion :path)) suggestions))
     (approve-pr-field 'headRefOid)
     (lambda (texts)
       (with-current-buffer buffer
         (pcase (condition-case err
                    (cons t (approve-suggestions--files suggestions texts))
                  (approve-mutation-refused (cons nil err)))
           (`(nil . ,err) (funcall errorback err))
           (`(t . ,files)
            (approve-mutation-commit-files
             files message
             :errorback errorback
             :callback
             (lambda (oid)
               (approve-suggestions--resolve
                (delete-dups
                 (delq nil (mapcar (lambda (suggestion)
                                     (let-alist (plist-get suggestion :thread)
                                       (and (not .isResolved) .id)))
                                   suggestions)))
                (lambda ()
                  (approve-query-await-head
                   oid :callback (lambda () (when callback (funcall callback oid)))))
                (lambda (err)
                  (funcall errorback
                           (list 'approve-api-error
                                 (format "committed %s, but a thread was not resolved: %s"
                                         (substring oid 0 (min 7 (length oid)))
                                         (approve-api-error-message err))))))))))))
     errorback)))

;;; Commands

(defun approve-suggestions--at-point-id (context)
  "Return the id of the one suggestion CONTEXT is on, or signal why not.
On a suggested change, it is that one; on a comment or a thread with
a single suggestion, that one."
  (pcase (plist-get context :kind)
    ('suggestion (cons (plist-get context :comment-id) (plist-get context :index)))
    (kind
     (let* ((comments (if (eq kind 'thread)
                          (mapcar (lambda (comment) (alist-get 'id comment))
                                  (approve-model-nodes
                                   (approve-model-get "PullRequestReviewThread"
                                                      (plist-get context :thread-id)
                                                      'comments)))
                        (list (plist-get context :comment-id))))
            (ids (seq-mapcat
                  (lambda (comment-id)
                    (seq-map-indexed (lambda (_lines index) (cons comment-id index))
                                     (approve-suggestions-parse
                                      (approve-model-get "PullRequestReviewComment" comment-id
                                                         'body))))
                  comments)))
       (pcase (length ids)
         (0 (user-error "%s" (if (eq kind 'thread)
                                 "This thread has no suggested change"
                               "This comment has no suggested change")))
         (1 (car ids))
         (_ (user-error "Put point on one of the suggested changes")))))))

(defun approve-suggestions--context ()
  "Return the context at point of a command on a suggestion."
  (approve-action-context
   (lambda (context)
     (memq (plist-get context :kind) '(suggestion review-comment thread)))
   "Put point on a suggested change"))

(defun approve-suggestions--checked (id &optional thread-id)
  "Return the suggestion ID, a plist, or signal a `user-error' why not.
It must be one that can be committed (`approve-suggestions-why-not').
THREAD-ID is its thread when known."
  (let ((suggestion (or (approve-suggestions-get id thread-id)
                        (user-error "That comment is gone"))))
    (when-let* ((why (approve-suggestions-why-not suggestion)))
      (user-error "%s" why))
    suggestion))

(defun approve-suggestions--at-point ()
  "Return the suggestion at point, a plist, or signal a `user-error' why not.
It must be one that can be committed (`approve-suggestions-why-not')."
  (let ((context (approve-suggestions--context)))
    (approve-suggestions--checked (approve-suggestions--at-point-id context)
                                  (plist-get context :thread-id))))

(defun approve-suggestions--batched ()
  "Return the suggestions of the batch, or signal a `user-error' why not.
Each must still be one that can be committed."
  (mapcar (lambda (id)
            (let ((suggestion (or (approve-suggestions-get id)
                                  (user-error "%s" (substitute-command-keys "\
A suggestion in the batch is gone; \\[universal-argument] \\[approve-batch-suggestion] \
empties the batch")))))
              (when-let* ((why (approve-suggestions-why-not suggestion)))
                (user-error "A suggestion in the batch can't be committed: %s" why))
              suggestion))
          (approve-suggestions-batch)))

(defun approve-suggestions--commit-preflight ()
  "Return the suggestions `approve-commit-suggestion' would commit.
They are the batch when there is one, else the suggestion at point.
Signal a `user-error' saying why when there are none it can commit."
  (unless (approve-model-root-ref)
    (approve-action-context))
  (if (approve-suggestions-batch)
      (progn (when-let* ((why (approve-suggestions--pr-why-not)))
               (user-error "%s" why))
             (approve-suggestions--batched))
    (list (approve-suggestions--at-point))))

(put 'approve-commit-suggestion 'approve-preflight #'approve-suggestions--commit-preflight)

(defun approve-suggestions--location (suggestion)
  "Return where SUGGESTION is, such as \"a.go:13–14\"."
  (let ((start (plist-get suggestion :start))
        (end (plist-get suggestion :end)))
    (format "%s:%s" (plist-get suggestion :path)
            (if (eql start end) end (format "%s–%s" start end)))))

(defun approve-commit-suggestion (&optional default)
  "Commit the suggested change at point to the pull request's branch.
When the batch has suggestions (\\[approve-batch-suggestion]), commit them
all in one commit instead, wherever point is, as GitHub does.  Write
the commit message first; it starts as GitHub's, with a
Co-authored-by trailer for each suggester but you.  With a DEFAULT
prefix argument, commit with that message at once.

The commit goes on the branch as you have it loaded: when it has new
commits, GitHub refuses, and refreshing the buffer shows them.  Then
the threads are resolved and the pull request is loaded again.
\(Flow F23, decision D41.)"
  (interactive "P")
  (let* ((suggestions (approve-suggestions--commit-preflight))
         (batch (approve-suggestions-batch))
         (ids (mapcar (lambda (suggestion) (plist-get suggestion :id)) suggestions))
         (default-message (approve-suggestions-message suggestions))
         (branch (approve-pr-field 'headRefName))
         (what (if (cdr suggestions)
                   (format "%d suggestions" (length suggestions))
                 (format "the suggestion on %s" (approve-suggestions--location
                                                  (car suggestions))))))
    (approve-action-run
     :what (format "commit %s" what)
     :input (unless default
              (list :prompt (format "Commit %s" what)
                    :context (format "to %s" branch)
                    :target (list 'commit-suggestions ids)
                    :name "commit suggestion"
                    :initial default-message
                    :commit-label "commit"))
     :marks (mapcar (lambda (suggestion)
                      `(section approve-thread-section
                                ,(alist-get 'id (plist-get suggestion :thread))))
                    suggestions)
     :send (lambda (text callback errorback)
             (approve-suggestions-commit suggestions (or text default-message)
                                         :callback callback :errorback errorback))
     :done (lambda (oid)
             (when batch
               (approve-session-put :suggestions nil))
             (message "Approve: committed %s as %s" what (substring oid 0 (min 7 (length oid))))
             (approve-pr-refresh)))))

(defun approve-suggestions--batch-preflight ()
  "Return the id of the suggestion `approve-batch-suggestion' acts on.
It is the suggestion at point.  One in the batch can always be taken
out; one to add must be one that can be committed, and changes no
line that one in the batch changes, else signal a `user-error'."
  (let* ((context (approve-suggestions--context))
         (id (approve-suggestions--at-point-id context))
         (batch (approve-suggestions-batch)))
    (unless (member id batch)
      (let ((suggestion (approve-suggestions--checked id (plist-get context :thread-id))))
        (when (seq-some (lambda (other)
                          (when-let* ((other (approve-suggestions-get other)))
                            (approve-suggestions--overlap-p suggestion other)))
                        batch)
          (user-error "Unable to commit due to other pending changes affecting this line"))))
    id))

(put 'approve-batch-suggestion 'approve-preflight #'approve-suggestions--batch-preflight)

(defun approve-batch-suggestion (&optional empty)
  "Add the suggested change at point to the batch, or take it out.
The batch is committed in one commit by \\[approve-commit-suggestion],
as GitHub's \"Add suggestion to batch\"; two suggestions on the same
lines can't both be in it.  With an EMPTY prefix argument, empty the
batch instead.  The batch lives as long as the buffer."
  (interactive "P")
  (if empty
      (progn
        (unless (approve-suggestions-batch)
          (user-error "The batch is empty"))
        (approve-suggestions--set-batch nil)
        (message "Approve: the batch is empty"))
    (let* ((id (approve-suggestions--batch-preflight))
           (batch (approve-suggestions-batch))
           (batch (if (member id batch) (remove id batch) (append batch (list id)))))
      (approve-suggestions--set-batch batch)
      (message "Approve: %s" (approve-suggestions--count-text batch)))))

;;; Showing the batch

(defun approve-suggestions--count-text (batch)
  "Return what BATCH, a list of ids, holds, for the echo area."
  (pcase (length batch)
    (0 "the batch is empty")
    (1 "1 suggestion in the batch")
    (n (format "%d suggestions in the batch" n))))

(defun approve-suggestions-header-note ()
  "Return the header line's note of the batch, or nil when it is empty.
Example: \"2 suggestions in the batch · x commits them\"."
  (when-let* ((batch (approve-suggestions-batch)))
    (approve-render-join
     (list (approve-face (approve-suggestions--count-text batch) 'approve-suggestion-batched)
           (approve-render-key-hints
            `((approve-commit-suggestion ,(if (cdr batch) "commits them" "commits it"))))))))

(add-hook 'approve-pr-header-line-functions #'approve-suggestions-header-note)

(defvar-local approve-suggestions--overlays nil
  "The overlays on the suggested changes of this buffer.")

(defun approve-suggestions--note (id)
  "Return the note on the box of the suggestion ID, or nil.
In the batch: \"✓ in the batch\"; else the keys that commit it or add
it, when it can be committed."
  (let ((batch (approve-suggestions-batch)))
    (if (member id batch)
        (approve-render-join
         (list (approve-face "✓ in the batch" 'approve-suggestion-batched)
               (approve-render-hints '(approve-batch-suggestion "take out"))))
      (when-let* ((approve-show-hints)
                  (suggestion (approve-suggestions-get id))
                  ((not (approve-suggestions-why-not suggestion)))
                  ((not (seq-some (lambda (other)
                                    (when-let* ((other (approve-suggestions-get other)))
                                      (approve-suggestions--overlap-p suggestion other)))
                                  batch))))
        (approve-render-hints (and (null batch) '(approve-commit-suggestion "commit"))
                              '(approve-batch-suggestion "add to batch"))))))

(defun approve-suggestions--decorate-box (start end id)
  "Put the note of the suggestion ID on the first line of its box.
The box is drawn between START and END."
  (when-let* ((note (approve-suggestions--note id)))
    (save-excursion
      (goto-char start)
      (when (re-search-forward "Suggested change \\(─+\\)$" end t)
        (let* ((rule-start (match-beginning 1))
               (room (- (match-end 1) rule-start))
               (width (+ 3 (string-width note))))
          (when (> room width)
            ;; Right after the title, so a narrow window shows it too.
            (let ((overlay (make-overlay rule-start (+ rule-start width))))
              (overlay-put overlay 'evaporate t)
              (overlay-put overlay 'display
                           (concat (approve-face "· " 'approve-dim) note " "))
              (push overlay approve-suggestions--overlays))))))))

(defun approve-suggestions--show ()
  "Mark the suggested changes of the current buffer, anew.
A redraw draws the text anew, so it runs after each one."
  (mapc #'delete-overlay approve-suggestions--overlays)
  (setq approve-suggestions--overlays nil)
  (when (approve-model-root-ref)
    (let ((pos (point-min)))
      (while (setq pos (next-single-property-change pos 'approve-thing))
        (let ((thing (get-text-property pos 'approve-thing)))
          (when (eq (plist-get thing :kind) 'suggestion)
            (when-let* ((context (approve-context-at-point pos)))
              (when (eq (plist-get context :kind) 'suggestion)
                (approve-suggestions--decorate-box
                 pos (next-single-property-change pos 'approve-thing nil (point-max))
                 (cons (plist-get context :comment-id) (plist-get context :index)))))))))))

(add-hook 'approve-pr-redraw-hook #'approve-suggestions--show)

;;; Eldoc

(defun approve-suggestions-describe (context _details)
  "Describe the suggested change of CONTEXT for eldoc: whose, and what to do."
  (let* ((id (cons (plist-get context :comment-id) (plist-get context :index)))
         (suggestion (approve-suggestions-get id (plist-get context :thread-id))))
    (when suggestion
      (let* ((author (alist-get 'author (plist-get suggestion :comment)))
             (why (approve-suggestions-why-not suggestion))
             (state (cond ((member id (approve-suggestions-batch)) "in the batch")
                          (why (approve-face why 'approve-dim))
                          (t (approve-render-key-hints
                              (list (and (null (approve-suggestions-batch))
                                         '(approve-commit-suggestion "commits it"))
                                    '(approve-batch-suggestion "adds it to the batch"))))))
             (where (if (plist-get suggestion :end)
                        (approve-suggestions--location suggestion)
                      (or (plist-get suggestion :path) "no file")))
             (title (format "Suggested change by @%s on %s"
                            (or (approve-suggestions--login author) "ghost") where)))
        (list :echo (approve-render-join (list title state))
              :doc (concat title "\n" (or state "") "\n"))))))

(add-to-list 'approve-eldoc-describers '(suggestion . approve-suggestions-describe) t)

;;; Keys

(keymap-set approve-pr-mode-map "x" #'approve-commit-suggestion)
(keymap-set approve-pr-mode-map "b" #'approve-batch-suggestion)

(provide 'approve-suggestions)

;;; approve-suggestions.el ends here
