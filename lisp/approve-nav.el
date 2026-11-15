;;; approve-nav.el --- Moving around and leaving a pull request buffer -*- lexical-binding: t; -*-

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

;; The read-only commands of a PR buffer (docs/flows.org, F03):
;;
;;   RET   `approve-visit'      follow the thing at point
;;   o     `approve-browse'     open it (or the PR) on GitHub
;;   y     `approve-copy'       copy its URL, sha, path:line or name
;;   ] u   `approve-next-unviewed-file'      [ u  previous
;;   ] t   `approve-next-thread'             [ t  previous (unresolved)
;;   TAB   `approve-section-toggle'          fold the section, or a line's threads
;;         `approve-section-show', `approve-section-hide'  the same, one way
;;         only (evil's zo and zc)
;;   T     `approve-toggle-threads'          fold or unfold every thread in the diff
;;   O     `approve-fold-others'             fold every section but the one at point
;;
;; and the jumps the `approve-jump' transient (j) offers, defined in
;; approve-menus.el.  Moving into a file's diff or a thread expands
;; what hides it.  This file binds its keys in `approve-pr-mode-map'.
;; The dashboard binds RET, o, y, TAB and O to the same commands: on a
;; row of a list, RET opens its pull request and TAB folds the list.
;;
;; A commit is shown by the first of `approve-visit-commit-functions'
;; that can, else on GitHub.  approve.el puts `approve-clone-show-commit'
;; there, which shows it in Magit from a local clone (decision D37), so
;; this file never needs a clone.
;;
;; Layer rules: this is the buffers layer.  It reads the context, the
;; store and the sections and moves point; it never changes the store
;; or calls the network (opening a URL hands it to `browse-url').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'browse-url)
(require 'magit-section)
(require 'approve-model)
(require 'approve-session)
(require 'approve-context)
(require 'approve-pr)
(require 'approve-sections)
(require 'approve-diff)

;;; Moving to sections

(defun approve-nav--top-section (type)
  "Return the top-level section of TYPE, or nil."
  (and magit-root-section
       (seq-find (lambda (section) (eq (oref section type) type))
                 (oref magit-root-section children))))

(defun approve-nav--goto (section)
  "Move to SECTION, showing it and every section around it."
  (let ((parent (oref section parent)))
    (while parent
      (when (oref parent hidden)
        (magit-section-show parent))
      (setq parent (oref parent parent))))
  (goto-char (oref section start))
  (when-let* ((window (get-buffer-window (current-buffer))))
    (unless (pos-visible-in-window-p (oref section start) window)
      (set-window-start window (oref section start)))))

(defun approve-nav-goto-section (type &optional show)
  "Move to the top-level section of TYPE; expand it when SHOW.
Signal a `user-error' when the buffer has no such section."
  (let ((section (or (approve-nav--top-section type)
                     (user-error "There is no such section here"))))
    (approve-nav--goto section)
    (when (and show (oref section hidden))
      (magit-section-show section))
    section))

(defun approve-nav--file-sections ()
  "Return the file sections of the diff, in order."
  (when-let* ((diff (approve-nav--top-section 'approve-diff-section)))
    (oref diff children)))

(defun approve-nav-goto-file (path)
  "Move to the diff of PATH and expand it.  Return its section."
  (let ((section (or (seq-find (lambda (section) (equal (oref section value) path))
                               (approve-nav--file-sections))
                     (user-error "No diff of %s here" path))))
    (approve-nav--goto section)
    (magit-section-show section)
    section))

(defun approve-nav--find-in (section predicate)
  "Return the first section in SECTION's subtree that PREDICATE accepts."
  (catch 'found
    (magit-map-sections (lambda (child)
                          (when (funcall predicate child)
                            (throw 'found child)))
                        section)
    nil))

(defun approve-nav-goto-thread (thread-id)
  "Move to the thread THREAD-ID in the diff, expanding what hides it."
  (let* ((path (and (approve-model-has-entity-p "PullRequestReviewThread" thread-id)
                    (approve-model-get "PullRequestReviewThread" thread-id 'path)))
         (file (and path (approve-nav-goto-file path)))
         (thread (and file
                      (approve-nav--find-in
                       file (lambda (section)
                              (and (eq (oref section type) 'approve-thread-section)
                                   (equal (oref section value) thread-id)))))))
    (if thread
        (approve-nav--goto thread)
      (user-error "That thread is not in the diff"))))

;;; Files and threads

(defun approve-nav--viewed-states ()
  "Return a hash table from path to its viewed state."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file (approve-pr-list-fields 'files '(path viewerViewedState)))
      (puthash (alist-get 'path file) (alist-get 'viewerViewedState file) table))
    table))

(defun approve-nav--current-file ()
  "Return the file section point is in, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'approve-file-section)))
      (setq section (oref section parent)))
    section))

(defun approve-nav--unviewed-file (forward)
  "Return the next unviewed file section, after point if FORWARD."
  (let* ((states (approve-nav--viewed-states))
         (current (approve-nav--current-file))
         (here (if current (oref current start) (point)))
         (files (seq-filter (lambda (section)
                              (not (equal (gethash (oref section value) states) "VIEWED")))
                            (approve-nav--file-sections))))
    (if forward
        (seq-find (lambda (section) (> (oref section start) here)) files)
      (seq-find (lambda (section) (< (oref section start) here)) (reverse files)))))

(defun approve-next-unviewed-file ()
  "Move to the next file that is not marked as viewed."
  (interactive)
  (approve-nav-goto-file (oref (or (approve-nav--unviewed-file t)
                                   (user-error "No unviewed file after this"))
                               value)))

(defun approve-previous-unviewed-file ()
  "Move to the previous file that is not marked as viewed."
  (interactive)
  (approve-nav-goto-file (oref (or (approve-nav--unviewed-file nil)
                                   (user-error "No unviewed file before this"))
                               value)))

(defun approve-nav--unresolved-paths ()
  "Return a hash table of the paths that have unresolved threads."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (thread (approve-pr-list-fields 'reviewThreads '(path isResolved)))
      (unless (alist-get 'isResolved thread)
        (puthash (alist-get 'path thread) t table)))
    table))

(defun approve-nav--unresolved-thread-p (section)
  "Return non-nil if SECTION is an unresolved thread."
  (and (eq (oref section type) 'approve-thread-section)
       (approve-model-has-entity-p "PullRequestReviewThread" (oref section value))
       (not (approve-model-get "PullRequestReviewThread" (oref section value)
                               'isResolved))))

(defun approve-nav--thread (forward)
  "Return the next unresolved thread section, after point if FORWARD.
Files are expanded to find it; a file that turns out not to hold one
is collapsed again."
  (let* ((pos (point))
         (paths (approve-nav--unresolved-paths))
         (files (seq-filter (lambda (section) (gethash (oref section value) paths))
                            (approve-nav--file-sections)))
         (files (if forward
                    (seq-filter (lambda (section) (> (oref section end) pos)) files)
                  (reverse (seq-filter (lambda (section) (< (oref section start) pos))
                                       files)))))
    (catch 'found
      (dolist (file files)
        (let ((hidden (oref file hidden)))
          (when hidden
            (magit-section-show file))
          (let ((threads nil))
            (magit-map-sections (lambda (section)
                                  (when (approve-nav--unresolved-thread-p section)
                                    (push section threads)))
                                file)
            (setq threads (nreverse threads))
            (when-let* ((thread (if forward
                                    (seq-find (lambda (s) (> (oref s start) pos)) threads)
                                  (seq-find (lambda (s) (< (oref s start) pos))
                                            (reverse threads)))))
              (throw 'found thread)))
          (when hidden
            (magit-section-hide file))))
      nil)))

(defun approve-next-thread ()
  "Move to the next unresolved review thread in the diff."
  (interactive)
  (approve-nav--goto (or (approve-nav--thread t)
                         (user-error "No unresolved thread after this"))))

(defun approve-previous-thread ()
  "Move to the previous unresolved review thread in the diff."
  (interactive)
  (approve-nav--goto (or (approve-nav--thread nil)
                         (user-error "No unresolved thread before this"))))

;;; Jumps

(defun approve-jump-description ()
  "Move to the description."
  (interactive)
  (approve-nav-goto-section 'approve-description-section t))

(defun approve-jump-conversation ()
  "Move to the conversation."
  (interactive)
  (approve-nav-goto-section 'approve-conversation-section t))

(defun approve-jump-checks ()
  "Move to the checks and expand them."
  (interactive)
  (approve-nav-goto-section 'approve-checks-section t))

(defun approve-jump-commits ()
  "Move to the commits and expand them."
  (interactive)
  (approve-nav-goto-section 'approve-commits-section t))

(defun approve-jump-files ()
  "Move to the list of changed files."
  (interactive)
  (approve-nav-goto-section 'approve-files-section t))

(defun approve-jump-pending-review ()
  "Move to the viewer's pending review."
  (interactive)
  (approve-nav-goto-section 'approve-pending-review-section t))

(defun approve-jump-file (path)
  "Move to the diff of the changed file PATH, read with completion."
  (interactive
   (list (completing-read "File: " (mapcar (lambda (file) (alist-get 'path file))
                                           (approve-pr-files))
                          nil t)))
  (approve-nav-goto-file path))

;;; Visiting, opening, copying

(defun approve-nav--thread-of-comment (comment-id)
  "Return the id of the thread that holds the review comment COMMENT-ID."
  (alist-get 'id (seq-find (lambda (thread)
                             (seq-some (lambda (comment)
                                         (equal (alist-get 'id comment) comment-id))
                                       (approve-model-nodes (alist-get 'comments thread))))
                           (approve-pr-threads))))

(defun approve-nav--thread-of-review (review-id)
  "Return the id of the first thread with a comment of the review REVIEW-ID."
  (alist-get 'id (seq-find (lambda (thread)
                             (seq-some (lambda (comment)
                                         (equal (let-alist comment .pullRequestReview.id)
                                                review-id))
                                       (approve-model-nodes (alist-get 'comments thread))))
                           (approve-pr-threads))))

(defun approve-nav--open-url (url)
  "Open URL: a pull request in Approve, anything else in the browser."
  (if-let* ((pr (approve-parse-url url)))
      (approve-pr-open pr)
    (browse-url url)))

(defvar approve-visit-commit-functions nil
  "Functions that show a commit for `approve-visit', tried in order.
Each is called with the commit's sha (perhaps abbreviated) and its
URL on GitHub, and returns non-nil if it showed the commit.  When
none does, the commit opens in the browser.  approve.el adds
`approve-clone-show-commit', which shows it in Magit from a local
clone that has it (decision D37).")

(defun approve-nav--visit-commit (oid url)
  "Show the commit OID: by `approve-visit-commit-functions', else at URL."
  (unless (run-hook-with-args-until-success 'approve-visit-commit-functions oid url)
    (browse-url url)))

(defconst approve-nav--fold-through
  '(approve-thread-section approve-comment-section approve-list-pr-section)
  "Sections that toggling passes to their parent.
Threads do not fold by themselves: in a thread, toggling folds the
diff line it is on, or its group (\"On this file\", \"Outdated\").  A
row of a list of pull requests folds its list.")

(defun approve-nav--foldable-section ()
  "Return the section at point that toggling folds, or nil.
That is the current section, or for one of
`approve-nav--fold-through' the first parent of another type."
  (let ((section (magit-current-section)))
    (while (and section (memq (oref section type) approve-nav--fold-through))
      (setq section (oref section parent)))
    (and section (not (eq section magit-root-section)) section)))

(defun approve-section-toggle ()
  "Fold or unfold the section at point.
On a diff line with threads, that folds its threads; in a thread, the
threads of its line (or its group); on another diff line, its hunk;
on a row of a list of pull requests, its list."
  (interactive)
  (magit-section-toggle (or (approve-nav--foldable-section)
                            (user-error "Nothing to fold here"))))

(defun approve-section-show ()
  "Unfold the section at point, the one `approve-section-toggle' folds."
  (interactive)
  (magit-section-show (or (approve-nav--foldable-section)
                          (user-error "Nothing to unfold here"))))

(defun approve-section-hide ()
  "Fold the section at point, the one `approve-section-toggle' folds.
Point in its body goes to its heading."
  (interactive)
  (magit-section-hide (or (approve-nav--foldable-section)
                          (user-error "Nothing to fold here"))))

(defconst approve-nav--fold-others-within '(approve-diff-section)
  "Sections whose own parts `approve-fold-others' folds too.
In the diff, only the file at point stays open.")

(defun approve-nav--fold-siblings (parent keep)
  "Fold the children of PARENT that have a body, but KEEP, which unfolds.
KEEP may be nil, to fold them all."
  (dolist (child (oref parent children))
    (unless (or (eq child keep) (null (oref child content)))
      (magit-section-hide child)))
  (when (and keep (oref keep content))
    (magit-section-show keep)))

(defun approve-fold-others ()
  "Fold every section but the one at point, which unfolds.
In a PR buffer, those are the top-level sections (the description,
the conversation, the files, the diff…), and in the diff every file
but the one at point.  In the dashboard, they are the lists.  Like
any folding, it holds when the buffer is drawn again."
  (interactive)
  (unless magit-root-section
    (user-error "Nothing to fold here"))
  ;; The sections from the root down to the one at point.
  (let ((path nil)
        (section (magit-current-section)))
    (while section
      (push section path)
      (setq section (oref section parent)))
    (unless (eq (car path) magit-root-section)
      (setq path (cons magit-root-section path)))
    (approve-nav--fold-siblings magit-root-section (cadr path))
    (cl-loop for (outer inner) on (cdr path)
             when (memq (oref outer type) approve-nav--fold-others-within)
             do (approve-nav--fold-siblings outer inner))))

(defconst approve-nav--thread-folds '(approve-line-section approve-file-threads-section)
  "Sections that fold threads: diff lines, \"On this file\" and \"Outdated\".")

(defun approve-nav--thread-fold-p (section)
  "Return non-nil if SECTION folds threads."
  (memq (oref section type) approve-nav--thread-folds))

(defun approve-nav--set-threads (state)
  "Fold every thread in the diff when STATE is `folded', else unfold them.
The choice holds for lines drawn later too: it is the session's
:threads, which the diff reads, and no line keeps a folding of its
own."
  (approve-session-put :threads state)
  (setq magit-section-visibility-cache
        (seq-remove (lambda (entry) (memq (car (caar entry)) approve-nav--thread-folds))
                    magit-section-visibility-cache))
  ;; Point in a thread about to be folded goes to its line.
  (when (eq state 'folded)
    (let ((section (magit-current-section)))
      (while (and section (not (approve-nav--thread-fold-p section)))
        (setq section (oref section parent)))
      (when (and section (> (point) (oref section content)))
        (goto-char (oref section start)))))
  (when magit-root-section
    (magit-map-sections (lambda (section)
                          (when (approve-nav--thread-fold-p section)
                            (if (eq state 'folded)
                                (magit-section-hide section)
                              (magit-section-show section))))
                        magit-root-section)))

(defun approve-fold-threads ()
  "Fold every thread in the diff, leaving each line's speech bubble.
Files expanded later show theirs folded too, until
\\<approve-pr-mode-map>\\[approve-toggle-threads] or `approve-unfold-threads'."
  (interactive)
  (approve-nav--set-threads 'folded)
  (message "Folded every thread in the diff"))

(defun approve-unfold-threads ()
  "Unfold every thread in the diff, resolved and outdated ones too.
Files expanded later show theirs unfolded too."
  (interactive)
  (approve-nav--set-threads 'shown)
  (message "Unfolded every thread in the diff"))

(defun approve-toggle-threads ()
  "Fold every thread in the diff, or unfold them all if they are folded.
The choice holds for files expanded later.  \\<approve-pr-mode-map>\\[approve-section-toggle] still folds
the threads of one line."
  (interactive)
  (if (eq (approve-session-get :threads) 'folded)
      (approve-unfold-threads)
    (approve-fold-threads)))

(defun approve-nav--toggle-or (message)
  "Toggle the section at point if it has a body, else say MESSAGE.
The section is the one `approve-section-toggle' folds."
  (let ((section (approve-nav--foldable-section)))
    (if (and section (oref section content))
        (magit-section-toggle section)
      (message "%s" message))))

(defun approve-visit ()
  "Follow the thing at point (flow F03).
A file goes to its diff, a pending comment or a review to its thread,
a link to its target (a pull request in Approve, anything else in the
browser), a check to its details.  A commit shows in Magit from a
local clone that has it (`approve-clone-directories'), else on
GitHub.  A diff GitHub did not send is computed from the file's
versions.  A row of a list of pull requests opens its pull request.
Elsewhere, toggle the section."
  (interactive)
  (let ((context (approve-context-at-point)))
    (pcase (plist-get context :kind)
      ('file
       (let ((section (magit-current-section)))
         (if (eq (oref section type) 'approve-file-section)
             (magit-section-toggle section)
           (approve-nav-goto-file (plist-get context :path)))))
      ('review-comment
       (if (eq (oref (magit-current-section) type) 'approve-pending-comment-section)
           (approve-nav-goto-thread (or (approve-nav--thread-of-comment
                                         (plist-get context :comment-id))
                                        (user-error "That comment's thread is not loaded")))
         (approve-nav--toggle-or "Nothing to visit here")))
      ('review
       (approve-nav-goto-thread (or (approve-nav--thread-of-review
                                     (plist-get context :review-id))
                                    (user-error "This review has no comments on code"))))
      ((or 'link 'ref 'check 'user 'team 'label 'milestone 'pull-request)
       (if-let* ((url (plist-get context :url)))
           (approve-nav--open-url url)
         (user-error "Nothing to visit here")))
      ('commit (approve-nav--visit-commit (plist-get context :value)
                                          (plist-get context :url)))
      ('load-diff (approve-pr-load-local-patch (plist-get context :path)))
      (_ (approve-nav--toggle-or "Nothing to visit here")))))

(defun approve-browse ()
  "Open the thing at point on GitHub, or the pull request."
  (interactive)
  (let ((url (or (plist-get (approve-context-at-point) :url)
                 (approve-pr-field 'url)
                 (user-error "Nothing to open here"))))
    (browse-url url)
    (message "Opened %s" url)))

(defun approve-nav--copy-text (context)
  "Return what `approve-copy' copies for CONTEXT."
  (pcase (plist-get context :kind)
    ('line (format "%s:%s" (plist-get context :path) (plist-get context :line)))
    ('lines (let ((start (plist-get context :start-line))
                  (end (plist-get context :line)))
              (if (equal start end)
                  (format "%s:%s" (plist-get context :path) end)
                (format "%s:%s-%s" (plist-get context :path) start end))))
    ('file (plist-get context :path))
    ((or 'commit 'user 'team 'label 'branch 'milestone 'git-actor 'search)
     (format "%s" (plist-get context :value)))
    ('ref (format "#%s" (plist-get context :value)))
    (_ (plist-get context :url))))

(defun approve-copy (&optional url)
  "Copy the thing at point: a path:line, sha, login, name, search or URL.
With a prefix argument URL, copy its URL instead."
  (interactive "P")
  (let* ((context (approve-context-at-point))
         (text (if url (plist-get context :url) (approve-nav--copy-text context))))
    (unless text
      (user-error "Nothing to copy here"))
    (kill-new text)
    (when (use-region-p)
      (deactivate-mark))
    (message "Copied %s" text)))

;;; Keys

(keymap-set approve-pr-mode-map "RET" #'approve-visit)
(keymap-set approve-pr-mode-map "<remap> <magit-section-toggle>" #'approve-section-toggle)
(keymap-set approve-pr-mode-map "o" #'approve-browse)
(keymap-set approve-pr-mode-map "y" #'approve-copy)
(keymap-set approve-pr-mode-map "] u" #'approve-next-unviewed-file)
(keymap-set approve-pr-mode-map "[ u" #'approve-previous-unviewed-file)
(keymap-set approve-pr-mode-map "] t" #'approve-next-thread)
(keymap-set approve-pr-mode-map "[ t" #'approve-previous-thread)
(keymap-set approve-pr-mode-map "T" #'approve-toggle-threads)
(keymap-set approve-pr-mode-map "O" #'approve-fold-others)

(provide 'approve-nav)

;;; approve-nav.el ends here
