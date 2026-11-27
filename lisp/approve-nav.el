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
;;   RET   `approve-visit'      follow the thing at point: a file or a
;;                              diff line opens the whole file
;;                              (approve-file.el), a line of a file
;;                              view goes to the diff
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
;; approve-menus.el, among them `approve-next-new-comment' and
;; `approve-previous-new-comment' (j n, j N), to the comments new since
;; the viewer's last review (decision D44), and `approve-jump-thread'
;; (j j), which picks any thread of the pull request with completion,
;; grouped by file (decision D52).  Moving into a file's diff,
;; a thread or a comment expands what hides it.  The jumps go through
;; the files the diff draws: those its filters hide (decision D54) are
;; skipped, and not offered by j / and j j.  This file binds its keys in `approve-pr-mode-map',
;; and RET, o and y in `approve-file-mode-map' too.
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
(require 'approve-conversation)
(require 'approve-diff)
(require 'approve-render)
(require 'approve-thread)
(require 'approve-input)
(require 'approve-file)

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
  "Move to the diff of PATH and expand it.  Return its section.
Signal a `user-error' when the diff doesn't show PATH, as when its
filters hide it (decision D54)."
  (let ((section (or (seq-find (lambda (section) (equal (oref section value) path))
                               (approve-nav--file-sections))
                     (if (and (approve-sections-filtered-p) (approve-pr-file path))
                         (user-error "The filter of the diff hides %s; %s changes it"
                                     path (or (approve-render-key 'approve-filter-menu)
                                              "M-x approve-filter-menu"))
                       (user-error "No diff of %s here" path)))))
    (approve-nav--goto section)
    (magit-section-show section)
    section))

(defun approve-nav--shown-paths ()
  "Return a table of the paths the diff shows, or nil when no filter is on.
The files the diff's filters hide (decision D54) are skipped by the
jumps of the PR buffer."
  (and (approve-sections-filtered-p)
       (approve-sections-path-table (approve-sections-filter-files (approve-pr-files)))))

(defun approve-nav--shown-p (path shown)
  "Return non-nil if PATH is in SHOWN, from `approve-nav--shown-paths'."
  (or (null shown) (gethash path shown)))

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

;;; What is new since the last review

(defun approve-nav--new-marks (from to)
  "Return the positions of the new marks between FROM and TO, in order.
A mark is the \"new\" of a comment new since the viewer's last
review, whose `approve-new' property is the comment's id
\(`approve-render-new')."
  (let ((pos from)
        (marks nil))
    (while (< pos to)
      (when-let* ((id (get-text-property pos 'approve-new)))
        (unless (and (> pos (point-min))
                     (equal (get-text-property (1- pos) 'approve-new) id))
          (push pos marks)))
      (setq pos (next-single-property-change pos 'approve-new nil to)))
    (nreverse marks)))

(defun approve-nav--new-comment (forward)
  "Return the position of the next new mark, after point if FORWARD.
The conversation is searched as drawn, the diff file by file: a
file with new comments is expanded to search it, and collapsed
again when the mark isn't there."
  (let* ((here (if forward (line-end-position) (line-beginning-position)))
         (diff (approve-nav--top-section 'approve-diff-section))
         (diff-start (if diff (oref diff start) (point-max)))
         (paths (mapcar (lambda (comment) (alist-get 'path comment))
                        (approve-pr-new-comments)))
         (files (seq-filter (lambda (section) (member (oref section value) paths))
                            (approve-nav--file-sections)))
         (in-file
          (lambda (file)
            (let ((hidden (oref file hidden)))
              (when hidden
                (magit-section-show file))
              (let ((marks (if forward
                               (approve-nav--new-marks (max here (oref file start))
                                                       (oref file end))
                             (approve-nav--new-marks (oref file start)
                                                     (min here (oref file end))))))
                (when (and hidden (null marks))
                  (magit-section-hide file))
                (if forward (car marks) (car (last marks))))))))
    (if forward
        (or (and (< here diff-start) (car (approve-nav--new-marks here diff-start)))
            (seq-some in-file (seq-filter (lambda (file) (> (oref file end) here)) files)))
      (or (seq-some in-file (reverse (seq-filter (lambda (file) (< (oref file start) here))
                                                 files)))
          (car (last (approve-nav--new-marks (point-min) (min here diff-start))))))))

(defun approve-nav-new-count ()
  "Return how many comments are new since the viewer's last review.
They are the entries of the conversation and the comments in threads
that `approve-next-new-comment' stops at: those in files the diff's
filters hide don't count."
  (if-let* ((since (approve-pr-last-review-time)))
      (+ (seq-count (lambda (run) (approve-conversation-new-p run since))
                    (approve-conversation-items))
         (let ((shown (approve-nav--shown-paths)))
           (seq-count (lambda (comment)
                        (approve-nav--shown-p (alist-get 'path comment) shown))
                      (approve-pr-new-comments))))
    0))

(defun approve-nav--goto-new (forward)
  "Move to the next comment new since the last review; back unless FORWARD."
  (unless (approve-pr-last-review-time)
    (user-error "You haven't reviewed this pull request: nothing is new since"))
  (let ((pos (or (approve-nav--new-comment forward)
                 (user-error (if forward "No new comment after this"
                               "No new comment before this")))))
    (approve-nav--goto (magit-section-at pos))))

(defun approve-next-new-comment ()
  "Move to the next comment new since your last review (decision D44).
It is an entry of the conversation, or a comment in a thread of the
diff, that someone else published after your last review.  What
hides it opens: the conversation, a file, a line's threads."
  (interactive)
  (approve-nav--goto-new t))

(defun approve-previous-new-comment ()
  "Move to the previous comment new since your last review.
See `approve-next-new-comment'."
  (interactive)
  (approve-nav--goto-new nil))

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
  "Move to the changed files: their list, else the diff.
The list is there only when `approve-pr-sections-hook' has
`approve-insert-files-section', which it hasn't by default."
  (interactive)
  (approve-nav-goto-section (if (approve-nav--top-section 'approve-files-section)
                                'approve-files-section
                              'approve-diff-section)
                            t))

(defun approve-jump-pending-review ()
  "Move to the viewer's pending review."
  (interactive)
  (approve-nav-goto-section 'approve-pending-review-section t))

(defun approve-jump-file (path)
  "Move to the diff of the changed file PATH, read with completion.
The files the diff's filters hide are not offered."
  (interactive
   (list (approve-input-with-submit-key
           (completing-read "File: " (mapcar (lambda (file) (alist-get 'path file))
                                             (approve-sections-filter-files
                                              (approve-pr-files)))
                            nil t))))
  (approve-nav-goto-file path))

;;; Picking a thread

(defvar-local approve-goto-thread-function nil
  "How the current buffer goes to a thread `approve-jump-thread' picked, or nil.
A function of the thread's id.  Nil goes to the thread in the PR
buffer's diff, showing the PR buffer if need be.  The feedback mode
sets it in a file of a clone, where a thread opens at its line in the
clone (decision D52).")

(defun approve-nav--thread-lines (thread)
  "Return the lines of THREAD as GitHub names them, or nil on the whole file.
Such as \"55\" or \"13–14\"; lines of the base have a minus, \"-32\",
and a range from a removed line to an added one says each end's side,
\"-13–+14\".  An outdated thread's lines are those it was written on."
  (let-alist thread
    (let* ((outdated (null .line))
           (line (if outdated .originalLine .line))
           (start (if outdated .originalStartLine .startLine))
           (start-side (or .startDiffSide .diffSide)))
      (cl-flet ((sign (side) (if (equal side "LEFT") "-" "")))
        (cond ((or (equal .subjectType "FILE") (null line)) nil)
              ((or (null start) (eql start line)) (format "%s%d" (sign .diffSide) line))
              ((equal start-side .diffSide)
               (format "%s%d–%s%d" (sign start-side) start (sign .diffSide) line))
              (t (format "%s%d–%s%d" (if (equal start-side "LEFT") "-" "+") start
                         (if (equal .diffSide "LEFT") "-" "+") line)))))))

(defun approve-nav--thread-notes (thread asking since)
  "Return what the thread picker says after THREAD, or nil.
That is how many comments it has, how many are new since SINCE, the
viewer's last review (decision D44), and whether it is pending, asks
for changes (its id is in ASKING, decision D34), is resolved or
outdated."
  (let* ((comments (approve-model-nodes (alist-get 'comments thread)))
         (new (seq-count (lambda (comment) (approve-pr-new-p comment since)) comments))
         (notes (delq nil
                      (list (and (cdr comments)
                                 (approve-face (format "%d comments" (length comments))
                                               'approve-dim))
                            (and (> new 0) (approve-face (format "%d new" new) 'approve-new))
                            (and (equal (alist-get 'state (car comments)) "PENDING")
                                 (approve-face "pending" 'approve-pending))
                            (and (member (alist-get 'id thread) asking)
                                 (approve-face "asks for changes"
                                               'approve-thread-mark-changes-requested))
                            (and (alist-get 'isResolved thread)
                                 (approve-face "resolved" 'approve-resolved))
                            (and (alist-get 'isOutdated thread)
                                 (approve-face "outdated" 'approve-outdated))))))
    (and notes (string-join notes (approve-face " · " 'approve-dim)))))

(defun approve-nav--rank< (a b)
  "Return non-nil if the list of numbers A sorts before B, element by element."
  (cond ((or (null a) (null b)) nil)
        ((< (car a) (car b)) t)
        ((> (car a) (car b)) nil)
        (t (approve-nav--rank< (cdr a) (cdr b)))))

(defun approve-nav--thread-candidates (first)
  "Return the pull request's threads for the thread picker, in order.
Each is (LABEL :id ID :path PATH :notes NOTES).  They go by file:
the file FIRST, when it has threads, then the others in the diff's
order.  In a file, the threads that are neither resolved nor
outdated come first, then those, dimmed; each part by line.  LABEL
is the path, the lines and the first comment's author and words:
\"src/a.go:13–14  @bob: Why a map?\"; NOTES what follows it
\(`approve-nav--thread-notes').  In the PR buffer, the threads of the
files the diff's filters hide are left out (decision D54)."
  (let* ((shown (and (derived-mode-p 'approve-pr-mode) (approve-nav--shown-paths)))
         (threads (seq-filter (lambda (thread)
                                (approve-nav--shown-p (alist-get 'path thread) shown))
                              (approve-pr-threads)))
         (asking (mapcar (lambda (thread) (alist-get 'id thread))
                         (approve-pr-threads-asking-for-changes)))
         (since (approve-pr-last-review-time))
         (order (delete-dups (append (and first (list first))
                                     (mapcar (lambda (file) (alist-get 'path file))
                                             (approve-pr-files))
                                     (mapcar (lambda (thread) (alist-get 'path thread))
                                             threads))))
         (sorted (sort (copy-sequence threads)
                       (lambda (a b)
                         (cl-flet ((rank (thread)
                                     (let-alist thread
                                       (list (seq-position order .path)
                                             (if (or .isResolved .isOutdated) 1 0)
                                             (or .line .originalLine 0)))))
                           (approve-nav--rank< (rank a) (rank b))))))
         (lines (mapcar (lambda (thread) (or (approve-nav--thread-lines thread) "file"))
                        sorted))
         (width (apply #'max 0 (mapcar #'string-width lines)))
         (seen (make-hash-table :test #'equal)))
    (cl-mapcar
     (lambda (thread lines)
       (let-alist thread
         (let* ((label (concat .path ":" (approve-face (approve-render-pad lines width)
                                                       'approve-number)
                               "  " (approve-thread-summary thread)))
                (count (puthash label (1+ (gethash label seen 0)) seen))
                (label (if (> count 1) (format "%s (%d)" label count) label))
                (label (if (or .isResolved .isOutdated)
                           (approve-face-add label 'approve-dim)
                         label)))
           (list label :id .id :path .path
                 :notes (approve-nav--thread-notes thread asking since)))))
     sorted lines)))

(defun approve-nav--jump-thread-preflight ()
  "Return the path of the file at point, or nil, for `approve-jump-thread'.
Signal a `user-error' outside a buffer of a pull request, before it
has loaded, or when it has no threads."
  (unless (or (derived-mode-p 'approve-pr-mode) approve-model-store-buffer)
    (user-error "Not in a buffer of a pull request"))
  (unless (approve-model-root-ref)
    (user-error "Wait until the pull request has loaded"))
  (unless (approve-pr-list-fields 'reviewThreads '(id))
    (user-error "No threads in this pull request"))
  (when-let* (((derived-mode-p 'approve-pr-mode))
              (shown (approve-nav--shown-paths))
              ((not (seq-some (lambda (thread)
                                (approve-nav--shown-p (alist-get 'path thread) shown))
                              (approve-pr-list-fields 'reviewThreads '(path))))))
    (user-error "The filter of the diff hides every thread"))
  (or (ignore-errors (plist-get (approve-context-at-point) :path))
      (when-let* (((derived-mode-p 'approve-pr-mode))
                  (section (approve-nav--current-file)))
        (oref section value))))

(defun approve-nav-read-thread (prompt &optional first)
  "Read one of the pull request's threads with PROMPT; return its id.
The threads are grouped by file, FIRST's first, as
`approve-nav--thread-candidates' orders them; under a file's group
each shows its lines and first words, followed by its notes.  Its
completion category is `approve-thread'."
  (let* ((candidates (approve-nav--thread-candidates first))
         (entry (lambda (candidate) (cdr (assoc candidate candidates))))
         (collection
          (lambda (string predicate action)
            (if (eq action 'metadata)
                `(metadata
                  (category . approve-thread)
                  (group-function
                   . ,(lambda (candidate transform)
                        (let ((path (plist-get (funcall entry candidate) :path)))
                          (cond ((not transform) path)
                                ((and path (string-prefix-p (concat path ":") candidate))
                                 (substring candidate (1+ (length path))))
                                (t candidate)))))
                  (annotation-function
                   . ,(lambda (candidate)
                        (when-let* ((notes (plist-get (funcall entry candidate) :notes)))
                          (concat "  " notes))))
                  (display-sort-function . identity)
                  (cycle-sort-function . identity))
              (complete-with-action action candidates string predicate))))
         (choice (approve-input-with-submit-key
                   (completing-read prompt collection nil t))))
    (or (plist-get (funcall entry choice) :id)
        (user-error "No such thread"))))

(defun approve-nav-show-thread (thread-id)
  "Go to the thread THREAD-ID in the PR buffer's diff.
Outside the PR buffer, it is shown where a window shows it, else in
the selected window."
  (unless (derived-mode-p 'approve-pr-mode)
    (pop-to-buffer (approve-model-owner-buffer)
                   '(display-buffer-reuse-window display-buffer-same-window)))
  (approve-nav-goto-thread thread-id))

(defun approve-jump-thread (thread-id)
  "Go to a thread of the pull request, THREAD-ID, picked with completion.
Every thread is offered, grouped by file: the file at point first,
then the others in the diff's order.  In each, the threads still
open come by line, then the resolved and outdated ones, dimmed.
Each shows its lines and its first comment's author and words,
then how many comments it has, how many are new since your last
review, and whether it is pending, asks for changes, is resolved or
outdated.  Typing a path narrows to its file.

In the PR buffer and the file view, the thread is shown in the diff;
in a file of a clone (`approve-feedback-mode'), at its line in the
clone, its folds opened and the thread unfolded
\(`approve-goto-thread-function', decision D52).  The mark is set
where point was."
  (interactive
   (list (approve-nav-read-thread "Thread: " (approve-nav--jump-thread-preflight))))
  (push-mark)
  (funcall (or approve-goto-thread-function #'approve-nav-show-thread) thread-id))

(put 'approve-jump-thread 'approve-preflight #'approve-nav--jump-thread-preflight)

;;; Visiting, opening, copying

(defun approve-nav-thread-of-comment (comment-id)
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
  '(approve-thread-section approve-comment-section approve-list-pr-section
    approve-check-section)
  "Sections that toggling passes to their parent.
Threads do not fold by themselves: in a thread, toggling folds the
diff line it is on, or its group (\"On this file\", \"Outdated\").  A
row of a list of pull requests folds its list, and a check its group
or the checks.")

(defun approve-nav--foldable-section ()
  "Return the section at point that toggling folds, or nil.
That is the current section, or for one of
`approve-nav--fold-through' the first parent of another type.  A
section of sections without a heading of its own, which has nothing
to fold, passes it on too: the checks that were under \"… N more\"
fold the checks."
  (let ((section (magit-current-section)))
    (while (and section
                (or (memq (oref section type) approve-nav--fold-through)
                    (and (oref section parent) (oref section children)
                         (null (oref section content)))))
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
  (approve-forget-visibility (lambda (ident) (memq (caar ident) approve-nav--thread-folds)))
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

(defvar approve-visit-row-commands nil
  "Commands \\[approve-visit] runs on a metadata row: (ROW . COMMAND).
ROW names the row, such as `labels' (see `approve-metadata-rows').
approve-manage.el adds the rows it changes, so RET on Labels edits
the labels; this file knows none of them.")

(defun approve-nav--row-command ()
  "Return the command of the metadata row point is on, or nil.
Point may be on a thing of the row, such as a label: the row
counts, not the thing."
  (let ((context (approve-context-of-sections)))
    (and (eq (plist-get context :kind) 'meta)
         (alist-get (plist-get context :row) approve-visit-row-commands))))

(defun approve-nav-goto-diff-line (context)
  "Show the PR buffer at the line of CONTEXT in the diff, its threads unfolded.
CONTEXT is a `line' or `lines' in a file view, the line's place in
the diff, or `code', a line not in it: that goes to the file's diff.
The PR buffer is shown where it is already, else in this window."
  (let ((path (plist-get context :path))
        (range (and (memq (plist-get context :kind) '(line lines))
                    (list :path (plist-get context :path) :side (plist-get context :side)
                          :line (plist-get context :line)))))
    (pop-to-buffer (approve-model-owner-buffer)
                   '(display-buffer-reuse-window display-buffer-same-window))
    (approve-nav-goto-file path)
    (when-let* ((start (car (and range (approve-diff-range-positions range)))))
      (goto-char start)
      (let ((section (magit-section-at)))
        (when (and section (eq (oref section type) 'approve-line-section)
                   (oref section hidden))
          (magit-section-show section))))))

(defun approve-visit ()
  "Follow the thing at point (flow F03).
A file, its heading in the diff or its entry in the file list, opens
whole (`approve-visit-file'; TAB folds it), as does a line of the
diff, at that line; a pending comment or a review goes to its thread,
a link to its target (a pull request in Approve, anything else in the
browser), a check to its details.  A commit shows in Magit from a
local clone that has it (`approve-clone-directories'), else on
GitHub.  A diff GitHub did not send is computed from the file's
versions.  A row of a list of pull requests opens its pull request.
In a file view, a line goes to the diff: to its own place when it is
in the diff, its threads unfolded, else to the file.
On a metadata row that can be changed, such as Labels, change it
\(`approve-visit-row-commands'); \\[approve-browse] opens the label
or person at point.  Elsewhere, toggle the section."
  (interactive)
  (if-let* ((command (approve-nav--row-command)))
      (call-interactively command)
    (approve-nav--visit (approve-context-at-point))))

(defun approve-nav--visit (context)
  "Follow CONTEXT, the thing at point; see `approve-visit'."
  (pcase (plist-get context :kind)
    ('file
     (if (memq (when-let* ((section (magit-current-section))) (oref section type))
               '(approve-file-section approve-file-entry-section))
         (approve-visit-file)
       (approve-nav-goto-file (plist-get context :path))))
    ('review-comment
     (if (eq (oref (magit-current-section) type) 'approve-pending-comment-section)
         (approve-nav-goto-thread (or (approve-nav-thread-of-comment
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
    ((or 'line 'lines)
     (if approve-context-file
         (approve-nav-goto-diff-line context)
       (approve-file-show (plist-get context :path) (plist-get context :side)
                          (plist-get context :line))))
    ('code (approve-nav-goto-diff-line context))
    (_ (approve-nav--toggle-or "Nothing to visit here"))))

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
    ('code (format "%s:%s" (plist-get context :name) (plist-get context :line)))
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

(keymap-set approve-file-mode-map "RET" #'approve-visit)
(keymap-set approve-file-mode-map "<return>" #'approve-visit)
(keymap-set approve-file-mode-map "o" #'approve-browse)
(keymap-set approve-file-mode-map "y" #'approve-copy)

(provide 'approve-nav)

;;; approve-nav.el ends here
