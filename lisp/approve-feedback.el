;;; approve-feedback.el --- A pull request's threads in the buffers of its clone -*- lexical-binding: t; -*-

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

;; The feedback mode: the review threads of a branch's pull request in
;; the user's own buffers of its files, in a clone with the branch
;; checked out (docs/flows.org, F21; docs/ui.org, "A file's buffer in
;; your clone"; decisions D45 and D46).  The buffer stays the user's:
;; its major mode, its hooks and its keys.  Approve adds overlays and a
;; mode-line lighter, and never changes the text.
;;
;; - `approve-feedback-mode' finds the buffer's branch and the pull
;;   request Approve shows for it (approve-clone.el): the open one
;;   updated last, or the one picked with `approve-clone-pick-pull-request'.
;;   It asks before showing a closed or merged one.
;; - The pull request's store is its PR buffer's, shared as the file
;;   views share it (D45, amended): when the PR is not open, the mode
;;   opens its buffer without showing it and loads it there, once for
;;   every buffer of the clone.  A redraw of the PR buffer redraws them,
;;   and when it is killed they open and load it again.
;; - GitHub's text of the file at the pull request's head comes from the
;;   clone when it has the commit, else from GitHub (the file view's
;;   cache).  approve-lines.el compares it with the buffer's, at once
;;   and after edits once Emacs is idle, and the threads go under the
;;   lines it says: their own, the line now in place of one edited here,
;;   or where deleted lines were.  Threads on the whole file go above
;;   the first line.
;; - The threads are overlays: a bubble in the right margin, red while
;;   one asks for changes (D34), gold when the line was edited here,
;;   dim when all are resolved; the threads after the line's text, the
;;   file view's boxes, folded at first (D46).  Between comparisons they
;;   move with the text.
;; - The lighter says the pull request's number, the file's unresolved
;;   threads, which of the branch's open pull requests it is, how the
;;   clone stands to GitHub's head, and what is loading or changed; its
;;   tooltip says it in words (D46).
;; - `approve-feedback-command-map' holds the mode's commands, for the
;;   user to bind: the mode binds no key of its own (D46).  TAB folds
;;   the threads of a line, T all of them, n and p go to the next and
;;   previous unresolved thread; mouse-1 on a bubble folds its line.
;;
;; Layer rules: this is the buffers layer, and a module for a local
;; clone (D36), which the core never requires; approve.el does.  It
;; reads the store through selectors and changes only its own overlays
;; and the mode line of the buffers it is on.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'approve-log)
(require 'approve-api)
(require 'approve-model)
(require 'approve-session)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-pr)
(require 'approve-thread)
(require 'approve-file)
(require 'approve-clone)
(require 'approve-lines)

;;; State of a buffer

(defvar-local approve-feedback--branch nil
  "The buffer's branch, as `approve-clone-branch-head' gives it.")

(defvar-local approve-feedback--pull-request nil
  "The pull request shown here: a row of `approve-clone-branch-pull-requests'.")

(defvar-local approve-feedback--open nil
  "The branch's open pull requests, the latest updated first.")

(defvar-local approve-feedback--path nil
  "The path of the buffer's file in its repository.")

(defvar-local approve-feedback--clone-state nil
  "How the clone stands to GitHub's head: `approve-clone-state''s plist.
Its :oid is the head it was compared with.")

(defvar-local approve-feedback--lighter nil
  "The lighter of the mode, computed when something it says changes.")
;; Without this, the mode line would drop the lighter's faces and tooltip.
(put 'approve-feedback--lighter 'risky-local-variable t)

(defvar-local approve-feedback--text nil
  "GitHub's text of the file, compared with the buffer: (OID . TEXT).
OID is the head it is at; TEXT is nil when GitHub has no such file.")

(defvar-local approve-feedback--fetching nil
  "The head whose text is being fetched from GitHub, or nil.")

(defvar-local approve-feedback--error nil
  "Why GitHub's text of the file could not be had, or nil.")

(defvar-local approve-feedback--shown nil
  "The ids of the threads unfolded here; the others are folded (D46).")

(defvar-local approve-feedback--margin nil
  "The buffer's own right margin before the mode widened it: (LOCAL . WIDTH).
LOCAL is non-nil when `right-margin-width' had a local value.")

;;; Mode

(defvar-keymap approve-feedback-command-map
  :doc "The commands of `approve-feedback-mode', for a key of your choice.
The mode binds no key in your buffers (decision D46); bind this map
to one, under a leader key for example:

  (keymap-set approve-feedback-mode-map \"C-c a\" approve-feedback-command-map)"
  "g" #'approve-feedback-refresh
  "P" #'approve-clone-pick-pull-request
  "TAB" #'approve-feedback-toggle-threads
  "<tab>" #'approve-feedback-toggle-threads
  "T" #'approve-feedback-toggle-all-threads
  "n" #'approve-feedback-next-thread
  "p" #'approve-feedback-previous-thread)

(defvar-keymap approve-feedback-mode-map
  :doc "Keymap of `approve-feedback-mode', empty: the mode binds no key.
Bind `approve-feedback-command-map' here to reach its commands.")

;;;###autoload
(define-minor-mode approve-feedback-mode
  "Show the review threads of the branch's pull request in this file's buffer.
The buffer's file is in a clone with a branch that heads a pull
request on GitHub.  The mode shows its open pull request updated
last, or the one picked with \\[approve-clone-pick-pull-request]; it asks before showing a
closed or merged one.  It changes no text, and binds no key: its
commands are in `approve-feedback-command-map'.  The mode line says
the pull request's number and state, and how the clone stands to it."
  :lighter approve-feedback--lighter
  :keymap approve-feedback-mode-map
  :group 'approve
  (if approve-feedback-mode
      (condition-case err
          (unless (approve-feedback--start)
            (approve-feedback--stop)
            (setq approve-feedback-mode nil))
        ((error quit)
         (approve-feedback--stop)
         (setq approve-feedback-mode nil)
         (signal (car err) (cdr err))))
    (approve-feedback--stop)))

;;; Turning it on and off

(defun approve-feedback--choose (head prs)
  "Return the pull request of HEAD to show among PRS, or nil.
HEAD is the branch on GitHub; PRS its pull requests.  It is the one
`approve-clone-shown-pull-request' shows; when none is open, the
closed or merged one updated last, after asking."
  (or (approve-clone-shown-pull-request head prs)
      (let ((pr (or (approve-clone-shown-pull-request head prs t)
                    (user-error "No pull request for %s" (plist-get head :branch)))))
        (and (y-or-n-p (format "#%s is %s: show its threads anyway? "
                               (alist-get 'number pr) (downcase (alist-get 'state pr))))
             pr))))

(defun approve-feedback--start ()
  "Find the pull request of the current buffer's branch and show it.
Return non-nil when shown; nil when the user declined a closed or
merged one.  Signal a `user-error' when the buffer has no file in a
clone of a GitHub repository, or its branch no pull request."
  (unless buffer-file-name
    (user-error "This buffer visits no file"))
  (let* ((branch (or (approve-clone-branch-head (file-name-directory buffer-file-name))
                     (user-error "%s is on no branch of a GitHub repository"
                                 (file-name-nondirectory buffer-file-name))))
         (head (plist-get branch :head))
         (prs (approve-clone-wait-pull-requests head))
         (pr (approve-feedback--choose head prs)))
    (when pr
      (approve-feedback--show branch pr prs)
      t)))

(defun approve-feedback--store-buffer (pr)
  "Return the PR buffer of PR, opening and loading it unseen if need be.
PR is a row of `approve-clone-branch-pull-requests'.  The buffer is
the one `approve' shows for the pull request (D45, amended)."
  (let ((target (or (approve-parse-url (alist-get 'url pr))
                    (error "Not a pull request's URL: %s" (alist-get 'url pr)))))
    (or (approve-pr-buffer target)
        (with-current-buffer (approve-pr-setup-buffer target)
          (approve-pr-load)
          (current-buffer)))))

(defun approve-feedback--show (branch pr prs)
  "Show PR, a pull request of BRANCH among its pull requests PRS, here.
BRANCH is as `approve-clone-branch-head' gives it."
  (setq approve-feedback--branch branch
        approve-feedback--pull-request pr
        approve-feedback--open (seq-filter (lambda (each) (equal (alist-get 'state each) "OPEN"))
                                           prs)
        approve-feedback--path (file-relative-name (file-truename buffer-file-name)
                                                   (file-truename (plist-get branch :clone)))
        approve-feedback--clone-state nil
        approve-feedback--text nil
        approve-feedback--fetching nil
        approve-feedback--error nil
        approve-feedback--shown nil
        approve-model-store-buffer (approve-feedback--store-buffer pr))
  (approve-lines-unwatch)
  (approve-feedback--draw nil)
  (approve-feedback-redraw))

(defun approve-feedback--stop ()
  "Take what the mode added away from the current buffer."
  (approve-lines-unwatch)
  (approve-feedback--draw nil)
  (dolist (variable '(approve-feedback--branch approve-feedback--pull-request
                      approve-feedback--open approve-feedback--path
                      approve-feedback--clone-state approve-feedback--lighter
                      approve-feedback--text approve-feedback--fetching
                      approve-feedback--error approve-feedback--shown
                      approve-model-store-buffer))
    (kill-local-variable variable)))

;;; The pull request's store

(defun approve-feedback--loaded-p ()
  "Return non-nil if the current buffer's pull request is in its store."
  (and (buffer-live-p approve-model-store-buffer)
       (approve-model-root-ref)))

(defun approve-feedback--buffers (&optional owner)
  "Return the buffers in the feedback mode, those of the PR buffer OWNER if given."
  (seq-filter (lambda (buffer)
                (and (buffer-local-value 'approve-feedback-mode buffer)
                     (or (null owner)
                         (eq (buffer-local-value 'approve-model-store-buffer buffer) owner))))
              (buffer-list)))

(defun approve-feedback-follow-pr-buffer ()
  "Redraw the clone's buffers of the current PR buffer, as it was redrawn.
approve.el puts it on `approve-pr-redraw-hook'."
  (when (derived-mode-p 'approve-pr-mode)
    (dolist (buffer (approve-feedback--buffers (current-buffer)))
      (with-current-buffer buffer
        (condition-case-unless-debug err
            (approve-feedback-redraw)
          (error (approve-log-condition "Redrawing the threads of a clone's buffer" err)))))))

(defun approve-feedback--reload (buffers)
  "Open and load the pull request of BUFFERS again: its PR buffer was killed."
  (dolist (buffer buffers)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'approve-feedback-mode buffer))
      (with-current-buffer buffer
        (unless (buffer-live-p approve-model-store-buffer)
          (setq approve-model-store-buffer
                (approve-feedback--store-buffer approve-feedback--pull-request))
          (approve-feedback-redraw))))))

(defun approve-feedback--pr-buffer-killed ()
  "Have the clone's buffers of the PR buffer being killed load it again.
It is on the `kill-buffer-hook' of PR buffers; the new buffer is
made once this one is gone."
  (when-let* (((derived-mode-p 'approve-pr-mode))
              (buffers (approve-feedback--buffers (current-buffer))))
    (run-at-time 0 nil #'approve-feedback--reload buffers)))

(defun approve-feedback-setup-pr-buffer ()
  "Make the current PR buffer tell the clone's buffers when it is killed.
approve.el puts it on `approve-pr-mode-hook'."
  (add-hook 'kill-buffer-hook #'approve-feedback--pr-buffer-killed nil t))

(defun approve-feedback--same-head-p (one other)
  "Return non-nil if the branches on GitHub ONE and OTHER are the same.
Each is (:host :owner :repo :branch)."
  (and (approve-clone-same-repository-p one other)
       (equal (plist-get one :branch) (plist-get other :branch))))

(defun approve-feedback--picked (head pr)
  "Show PR, just picked for the branch HEAD, in the buffers of that branch.
It is on `approve-clone-picked-functions'."
  (dolist (buffer (approve-feedback--buffers))
    (with-current-buffer buffer
      (when (and (approve-feedback--same-head-p (plist-get approve-feedback--branch :head) head)
                 (not (equal (alist-get 'id pr) (alist-get 'id approve-feedback--pull-request))))
        (approve-feedback--show approve-feedback--branch pr
                                (or (approve-clone-wait-pull-requests head) (list pr)))))))

;;; GitHub's text of the file

(defun approve-feedback--relevant-p ()
  "Return non-nil if the pull request changes the file, or has threads on it.
Otherwise the lighter is all the mode shows, and no text is compared."
  (cl-flet ((here-p (entry) (equal (alist-get 'path entry) approve-feedback--path)))
    (or (seq-some #'here-p (approve-pr-list-fields 'files '(path)))
        (seq-some #'here-p (approve-pr-list-fields 'reviewThreads '(path))))))

(defun approve-feedback--compare (oid text)
  "Compare TEXT, GitHub's file at OID, with the buffer, now and after edits.
TEXT is \"\" when GitHub has no such file.  Each answer draws the
threads again (`approve-feedback--decorate')."
  (setq approve-feedback--text (cons oid text)
        approve-feedback--fetching nil
        approve-feedback--error nil)
  (approve-lines-watch 'head text
                       :callback (lambda (_map) (approve-feedback--decorate))
                       :errorback (lambda (err)
                                    (setq approve-feedback--error (cadr err))
                                    (approve-feedback--update-lighter))))

(defun approve-feedback--fetched (buffer oid text &optional err)
  "Compare TEXT, GitHub's file at OID, with BUFFER, or note ERR."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and approve-feedback-mode (equal approve-feedback--fetching oid))
        (if err
            (setq approve-feedback--fetching nil
                  approve-feedback--error (approve-api-error-message err))
          (approve-feedback--compare oid text))
        (approve-feedback--update-lighter)))))

(defun approve-feedback--fetch (oid)
  "Get GitHub's text of the buffer's file at OID, and compare it with the buffer.
It comes from the clone when it has the commit, else from GitHub,
kept for the session as the file view keeps it (decision D45)."
  (if-let* ((text (approve-clone-file-text (plist-get approve-feedback--branch :clone)
                                           oid approve-feedback--path)))
      (approve-feedback--compare oid text)
    (unless (equal approve-feedback--fetching oid)
      (setq approve-feedback--fetching oid)
      (let ((buffer (current-buffer)))
        (approve-file-fetch oid approve-feedback--path
                            :callback (lambda (text)
                                        (approve-feedback--fetched buffer oid text))
                            :errorback (lambda (err)
                                         (approve-feedback--fetched buffer oid nil err)))))))

(defun approve-feedback--follow-head ()
  "Follow GitHub's head: compare the clone and the file with it when it moved."
  (let ((oid (approve-pr-field 'headRefOid)))
    (unless (equal oid (plist-get approve-feedback--clone-state :oid))
      (setq approve-feedback--clone-state
            (append (list :oid oid)
                    (approve-clone-state (plist-get approve-feedback--branch :clone) oid))))
    (cond
     ((not (approve-feedback--relevant-p))
      (when approve-feedback--text
        (approve-lines-unwatch)
        (setq approve-feedback--text nil)))
     ((not (equal oid (car approve-feedback--text)))
      (approve-feedback--fetch oid)))))

(defun approve-feedback--github-line (line)
  "Return GitHub's text of LINE of the file, or nil."
  (when-let* ((text (cdr approve-feedback--text))
              ((> line 0)))
    (let ((start 0))
      (while (and start (> line 1))
        (setq start (string-search "\n" text start)
              start (and start (1+ start))
              line (1- line)))
      (when (and start (< start (length text)))
        (string-remove-suffix
         "\r" (substring text start (or (string-search "\n" text start) (length text))))))))

;;; Where the threads go

(defun approve-feedback--where (map thread)
  "Return where THREAD goes in the buffer by MAP: (LINE . WHERE), or nil.
A thread on the whole file goes above the first line (LINE 0, WHERE
`file'); one on a line of the head where `approve-lines-to-buffer'
says, WHERE being `same', `edited' or `deleted'."
  (let-alist thread
    (cond
     ((equal .subjectType "FILE") (cons 0 'file))
     ((and .line (equal .diffSide "RIGHT")) (approve-lines-to-buffer map .line)))))

(defun approve-feedback--place (map)
  "Return the threads of the buffer's file by the lines they go under, by MAP.
The value is ((LINE . ENTRIES) ...), in order of LINE, 0 above the
first line.  Each of ENTRIES is (THREAD . WHERE), as
`approve-feedback--where' says, in the store's order."
  (let ((lines nil))
    (dolist (thread (approve-pr-threads-for-file approve-feedback--path))
      (when-let* ((where (approve-feedback--where map thread)))
        (if-let* ((line (assq (car where) lines)))
            (setcdr line (append (cdr line) (list (cons thread (cdr where)))))
          (push (list (car where) (cons thread (cdr where))) lines))))
    (sort lines (lambda (a b) (< (car a) (car b))))))

;;; Drawing the threads

(defvar-keymap approve-feedback-bubble-map
  :doc "Keymap of a thread's bubble in the right margin."
  "<right-margin> <mouse-1>" #'approve-feedback-mouse-toggle-threads
  "<mouse-1>" #'approve-feedback-mouse-toggle-threads)

(defun approve-feedback--edited-p (entry)
  "Return non-nil if the line of ENTRY's thread was edited or deleted here."
  (memq (cdr entry) '(edited deleted)))

(defun approve-feedback--unfolded-p (entries)
  "Return non-nil if the threads of ENTRIES are unfolded here."
  (seq-some (lambda (entry) (member (alist-get 'id (car entry)) approve-feedback--shown))
            entries))

(defun approve-feedback--bubble (entries asking)
  "Return the bubble of a line with the threads of ENTRIES.
It is the speech bubble and the count of unresolved threads, or of
all when none is.  Its colour says what matters most (decision D46):
red when one asks for changes, its id in ASKING (D34); gold when the
line was edited here; dim when all are resolved."
  (let* ((threads (mapcar #'car entries))
         (unresolved (seq-count (lambda (thread) (not (alist-get 'isResolved thread)))
                                threads)))
    (approve-face-add
     (concat (approve-icon 'comment)
             (number-to-string (if (> unresolved 0) unresolved (length threads))))
     (cond ((seq-some (lambda (thread) (member (alist-get 'id thread) asking)) threads)
            'approve-thread-mark-changes-requested)
           ((seq-some #'approve-feedback--edited-p entries) 'approve-edited-here)
           ((> unresolved 0) 'approve-diff-bubble)
           (t 'approve-dim)))))

(defun approve-feedback--bubble-help (entries unfolded)
  "Return the tooltip of the bubble of ENTRIES, UNFOLDED or not.
Such as \"1 thread · edited here: GitHub has  (retries 3))\"."
  (let ((edited (seq-find #'approve-feedback--edited-p entries)))
    (concat (approve-feedback--count (length entries) "thread")
            (and edited
                 (format " · %s here: GitHub has %s" (cdr edited)
                         (or (approve-feedback--github-line (alist-get 'line (car edited)))
                             "")))
            (if unfolded "\nmouse-1: fold" "\nmouse-1: unfold"))))

(defun approve-feedback--threads-text (entries)
  "Return the threads of ENTRIES drawn as the file view draws them.
The thread of a line edited here starts with GitHub's text of it.
Their boxes name no keys: the dispatch does (decision D46)."
  (let ((owner approve-model-store-buffer)
        (github (mapcar (lambda (entry)
                          (and (approve-feedback--edited-p entry)
                               (approve-feedback--github-line (alist-get 'line (car entry)))))
                        entries)))
    (with-temp-buffer
      ;; New comments are marked as in the PR buffer (D44).
      (setq approve-model-store-buffer owner)
      (cl-loop for entry in entries
               for text in github
               do (let ((start (point)))
                    (when text
                      (insert (approve-face (if (eq (cdr entry) 'deleted)
                                                "┊ deleted here: "
                                              "┊ on GitHub: ")
                                            'approve-dim)
                              (approve-face text 'approve-edited-here) "\n"))
                    (approve-thread-insert (car entry))
                    (approve-render-prefix start (point) "  ")))
      (buffer-string))))

(defun approve-feedback--paint (overlay &optional asking)
  "Give OVERLAY, the threads of a line, its bubble, and its threads unless folded.
ASKING are the ids of the threads that ask for changes, asked of
the store when nil.  The bubble is in the right margin; a line's
threads follow it, a string after its text, so the line numbers of
the lines after them stay theirs.  The threads above the first line
have a heading of their own, with their bubble."
  (let* ((entries (overlay-get overlay 'approve-feedback-entries))
         (asking (or asking (mapcar (lambda (thread) (alist-get 'id thread))
                                    (approve-pr-threads-asking-for-changes))))
         (unfolded (approve-feedback--unfolded-p entries))
         (bubble (approve-feedback--bubble entries asking))
         (margin (propertize " " 'display
                             `((margin right-margin)
                               ,(propertize bubble
                                            'help-echo (approve-feedback--bubble-help
                                                        entries unfolded)
                                            'mouse-face 'highlight
                                            'keymap approve-feedback-bubble-map)))))
    (overlay-put overlay 'approve-feedback-width (string-width bubble))
    (if (overlay-get overlay 'approve-feedback-top)
        (overlay-put overlay 'before-string
                     (concat margin
                             (approve-face (if (seq-some (lambda (entry) (eq (cdr entry) 'file))
                                                         entries)
                                               "  On this file"
                                             "  Above the first line")
                                           'approve-heading)
                             "\n"
                             (and unfolded (approve-feedback--threads-text entries))))
      (overlay-put overlay 'before-string margin)
      (overlay-put overlay 'after-string
                   (and unfolded
                        ;; Point at the end of the line stays there, not
                        ;; after the threads.
                        (concat (propertize " " 'cursor t) "\n"
                                (string-remove-suffix
                                 "\n" (approve-feedback--threads-text entries))))))))

(defun approve-feedback--set-margin (width)
  "Make the right margin WIDTH columns wide, or give it its own width back if nil.
The margin is there only while the mode is on and the file has
threads (decision D46)."
  (cond
   (width
    (unless approve-feedback--margin
      (setq approve-feedback--margin (cons (local-variable-p 'right-margin-width)
                                           right-margin-width)))
    (setq-local right-margin-width (max width (or (cdr approve-feedback--margin) 0))))
   (approve-feedback--margin
    (if (car approve-feedback--margin)
        (setq right-margin-width (cdr approve-feedback--margin))
      (kill-local-variable 'right-margin-width))
    (setq approve-feedback--margin nil)))
  (dolist (window (get-buffer-window-list nil nil t))
    (unless (eql (cdr (window-margins window)) right-margin-width)
      (set-window-margins window (car (window-margins window)) right-margin-width))))

(defun approve-feedback--overlays ()
  "Return the overlays of the threads here, in order."
  (save-restriction
    (widen)
    (sort (seq-filter (lambda (overlay) (overlay-get overlay 'approve-feedback-entries))
                      (overlays-in (point-min) (point-max)))
          (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun approve-feedback--draw (lines)
  "Show LINES, as `approve-feedback--place' gives them, under their lines.
Nil takes the threads and the margin away."
  (save-excursion
    (save-restriction
      (widen)
      (remove-overlays (point-min) (point-max) 'approve-feedback t)
      (let ((asking (and lines (mapcar (lambda (thread) (alist-get 'id thread))
                                       (approve-pr-threads-asking-for-changes))))
            (width nil))
        (pcase-dolist (`(,line . ,entries) lines)
          (goto-char (point-min))
          (when (or (zerop line) (zerop (forward-line (1- line))))
            (let ((overlay (if (zerop line)
                               (make-overlay (point-min) (point-min))
                             (make-overlay (line-beginning-position) (line-end-position)
                                           nil t t))))
              (overlay-put overlay 'approve-feedback t)
              (overlay-put overlay 'approve-feedback-entries entries)
              (overlay-put overlay 'approve-feedback-top (zerop line))
              (approve-feedback--paint overlay asking)
              (setq width (max (or width 3) (overlay-get overlay 'approve-feedback-width))))))
        (approve-feedback--set-margin width)))))

(defun approve-feedback--decorate ()
  "Draw the threads under their lines again, by the latest map.
A map older than the buffer's text asks for another comparison,
and the threads stay where the text took them until it answers."
  (let ((map (and approve-feedback--text (approve-lines-watched-map 'head))))
    (cond
     ((null approve-feedback--text) (approve-feedback--draw nil))
     ((null map))
     ((not (approve-lines-current-p map)) (approve-lines-update))
     (t (approve-feedback--draw (approve-feedback--place map))))
    (approve-feedback--update-lighter)))

(defun approve-feedback-redraw ()
  "Draw what the current buffer shows of its pull request again, from the store.
Every redraw of the PR buffer calls it in the buffers of its clone."
  (when approve-feedback-mode
    (if (approve-feedback--loaded-p)
        (progn
          (approve-feedback--follow-head)
          (approve-feedback--decorate))
      (approve-feedback--update-lighter))))

;;; The lighter

(defun approve-feedback--count (count noun)
  "Return \"COUNT NOUN\", NOUN in the plural unless COUNT is 1."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun approve-feedback--unresolved ()
  "Return the unresolved threads: (IN-THIS-FILE . IN-THE-PULL-REQUEST)."
  (let ((here 0) (all 0))
    (dolist (thread (approve-pr-list-fields 'reviewThreads '(path isResolved)))
      (unless (alist-get 'isResolved thread)
        (cl-incf all)
        (when (equal (alist-get 'path thread) approve-feedback--path)
          (cl-incf here))))
    (cons here all)))

(defun approve-feedback--clone-arrows (state)
  "Return the lighter's arrows of the clone's STATE, or nil.
STATE is `approve-clone-state''s: \"↑N\" commits not pushed, \"↓N\"
commits not pulled, both when diverged, \"↓?\" when GitHub's head is
missing from the clone."
  (pcase (plist-get state :state)
    ('ahead (format "↑%d" (plist-get state :ahead)))
    ('behind (format "↓%d" (plist-get state :behind)))
    ('diverged (format "↑%d↓%d" (plist-get state :ahead) (plist-get state :behind)))
    ('missing "↓?")))

(defun approve-feedback--clone-words (state)
  "Return what the clone's STATE means, in words, or nil when it is GitHub's head."
  (pcase (plist-get state :state)
    ('ahead (format "%s not pushed" (approve-feedback--count (plist-get state :ahead) "commit")))
    ('behind (format "GitHub has %s the clone hasn't: pull to see %s"
                     (approve-feedback--count (plist-get state :behind) "commit")
                     (if (= (plist-get state :behind) 1) "it" "them")))
    ('diverged (format "The branch and GitHub's differ: %d not pushed, %d not pulled"
                       (plist-get state :ahead) (plist-get state :behind)))
    ('missing "GitHub's head isn't in the clone: fetch to compare")))

(defun approve-feedback--state-icon (state draft)
  "Return the icon of a pull request in STATE, a draft if DRAFT."
  (pcase state
    ("MERGED" (approve-icon 'merged 'approve-state-merged))
    ("CLOSED" (approve-icon 'closed 'approve-state-closed))
    (_ (if draft
           (approve-icon 'draft 'approve-state-draft)
         (approve-icon 'open 'approve-state-open)))))

(defun approve-feedback--tooltip (fields unresolved loading changes)
  "Return the lighter's tooltip: what it says, in words.
FIELDS are the pull request's (`number', `title', `state', `isDraft',
`baseRefName'); UNRESOLVED the counts of `approve-feedback--unresolved',
or nil before the store is loaded; LOADING and CHANGES whether it
loads, and whether GitHub has changes."
  (let-alist fields
    (let* ((open approve-feedback--open)
           (index (seq-position open approve-feedback--pull-request
                                (lambda (a b) (equal (alist-get 'id a) (alist-get 'id b)))))
           (head (plist-get approve-feedback--branch :head)))
      (string-join
       (delq nil
             (list (format "%s#%s  %s · %s"
                           (alist-get 'nameWithOwner (alist-get 'repository
                                                                approve-feedback--pull-request))
                           .number .title
                           (if (and (eq .isDraft t) (equal .state "OPEN")) "draft" (downcase .state)))
                   (concat (format "%s into %s" (plist-get head :branch) .baseRefName)
                           (and index (cdr open)
                                (format " · %d of %d open pull requests of this branch"
                                        (1+ index) (length open))))
                   (approve-feedback--clone-words approve-feedback--clone-state)
                   (and unresolved
                        (if (zerop (cdr unresolved))
                            "No unresolved thread in the pull request"
                          (format "%s in this file, %d in the pull request"
                                  (approve-feedback--count (car unresolved) "unresolved thread")
                                  (cdr unresolved))))
                   (and loading "Loading the pull request…")
                   (and approve-feedback--error
                        (format "Could not compare the file with GitHub's: %s"
                                approve-feedback--error))
                   (and changes "Updated on GitHub: refresh to see it")))
       "\n"))))

(defun approve-feedback--lighter-text ()
  "Return the lighter of the current buffer (decision D46).
It is the pull request's icon and number; then … while it loads,
the file's unresolved threads, \"1/2\" when the branch has more open
pull requests, the clone's arrows, ↻ when GitHub has changes, and the
state of a closed or merged pull request.  Its tooltip says it in
words."
  (let* ((loaded (approve-feedback--loaded-p))
         (fields (if loaded
                     (approve-pr-fields '(number title state isDraft baseRefName))
                   approve-feedback--pull-request))
         (loading (or (not loaded) (approve-pr-loading-p) approve-feedback--fetching))
         (unresolved (and loaded (approve-feedback--unresolved)))
         (changes (and loaded (approve-session-get :changes)))
         (open approve-feedback--open)
         (index (seq-position open approve-feedback--pull-request
                              (lambda (a b) (equal (alist-get 'id a) (alist-get 'id b))))))
    (let-alist fields
      (propertize
       (concat " "
               (string-join
                (delq nil
                      (list (concat (approve-feedback--state-icon .state (eq .isDraft t))
                                    (format "#%s" .number))
                            (and loading "…")
                            (and unresolved (> (car unresolved) 0)
                                 (concat (approve-icon 'comment)
                                         (number-to-string (car unresolved))))
                            (and index (cdr open) (format "%d/%d" (1+ index) (length open)))
                            (approve-feedback--clone-arrows approve-feedback--clone-state)
                            (and changes (approve-face "↻" 'approve-updated))
                            (pcase .state
                              ("MERGED" (approve-face "merged" 'approve-state-merged))
                              ("CLOSED" (approve-face "closed" 'approve-state-closed)))))
                " "))
       'help-echo (approve-feedback--tooltip fields unresolved loading changes)
       'mouse-face 'mode-line-highlight))))

(defun approve-feedback--update-lighter ()
  "Compute the lighter of the current buffer again."
  (setq approve-feedback--lighter (approve-feedback--lighter-text))
  (force-mode-line-update))

;;; Commands

(defun approve-feedback--check ()
  "Signal a `user-error' unless the feedback mode is on here."
  (unless approve-feedback-mode
    (user-error "Approve shows no pull request here: turn on `approve-feedback-mode'")))

(defun approve-feedback-refresh (&optional clear-caches)
  "Load the pull request of this buffer again, and redraw its buffers.
That is \\<approve-pr-mode-map>\\[approve-pr-refresh] in its PR buffer (flow F17); with a prefix
argument CLEAR-CACHES, the session's caches are forgotten too."
  (interactive "P")
  (approve-feedback--check)
  (unless (buffer-live-p approve-model-store-buffer)
    (user-error "The pull request is loading again"))
  (with-current-buffer approve-model-store-buffer
    (approve-pr-refresh clear-caches)))

(defun approve-feedback--line-of (overlay)
  "Return the line OVERLAY's threads are under, 0 above the first line."
  (if (overlay-get overlay 'approve-feedback-top)
      0
    (save-restriction
      (widen)
      (line-number-at-pos (overlay-start overlay)))))

(defun approve-feedback--overlay-at (position)
  "Return the overlay of the threads of the line at POSITION, or nil.
On the first line without threads of its own, that is the overlay of
the threads above it."
  (save-excursion
    (goto-char position)
    (let ((overlays (seq-filter (lambda (overlay) (overlay-get overlay 'approve-feedback-entries))
                                (overlays-in (line-beginning-position) (line-end-position)))))
      (or (seq-find (lambda (overlay) (not (overlay-get overlay 'approve-feedback-top))) overlays)
          (car overlays)))))

(defun approve-feedback--toggle (overlay &optional show)
  "Unfold the threads of OVERLAY if folded, else fold them.
With SHOW `show', unfold them either way; with `hide', fold them."
  (let* ((entries (overlay-get overlay 'approve-feedback-entries))
         (ids (mapcar (lambda (entry) (alist-get 'id (car entry))) entries))
         (show (pcase show
                 ('show t)
                 ('hide nil)
                 (_ (not (approve-feedback--unfolded-p entries))))))
    (setq approve-feedback--shown (if show
                                      (seq-union ids approve-feedback--shown)
                                    (seq-difference approve-feedback--shown ids)))
    (approve-feedback--paint overlay)))

(defun approve-feedback-toggle-threads ()
  "Fold the threads of the line at point, or unfold them.
A folded line keeps its bubble in the right margin.  On the first
line, the threads above it when the line has none of its own."
  (interactive)
  (approve-feedback--check)
  (approve-feedback--toggle (or (approve-feedback--overlay-at (point))
                                (user-error "No threads on this line"))))

(defun approve-feedback-mouse-toggle-threads (event)
  "Fold the threads of the line whose bubble was clicked, EVENT, or unfold them."
  (interactive "e")
  (let ((position (event-start event)))
    (with-current-buffer (window-buffer (posn-window position))
      (when-let* ((overlay (approve-feedback--overlay-at (posn-point position))))
        (approve-feedback--toggle overlay)))))

(defun approve-feedback-toggle-all-threads ()
  "Unfold every thread of the file, or fold them all when none is folded."
  (interactive)
  (approve-feedback--check)
  (let* ((overlays (or (approve-feedback--overlays)
                       (user-error "No threads in this file")))
         (show (if (seq-some (lambda (overlay)
                               (not (approve-feedback--unfolded-p
                                     (overlay-get overlay 'approve-feedback-entries))))
                             overlays)
                   'show
                 'hide)))
    (dolist (overlay overlays)
      (approve-feedback--toggle overlay show))))

(defun approve-feedback--unresolved-overlays ()
  "Return the overlays of the lines with an unresolved thread, in order."
  (seq-filter (lambda (overlay)
                (seq-some (lambda (entry) (not (alist-get 'isResolved (car entry))))
                          (overlay-get overlay 'approve-feedback-entries)))
              (approve-feedback--overlays)))

(defun approve-feedback--here ()
  "Return the line of point, as `approve-feedback--line-of' counts lines.
At the very start of the buffer, point is on the threads above the
first line: line 0."
  (if (= (point) (save-restriction (widen) (point-min)))
      0
    (line-number-at-pos nil t)))

(defun approve-feedback--goto (overlay)
  "Move to the line of OVERLAY's threads, and unfold them."
  (approve-feedback--toggle overlay 'show)
  (goto-char (overlay-start overlay)))

(defun approve-feedback-next-thread ()
  "Move to the next line with an unresolved thread, and unfold its threads."
  (interactive)
  (approve-feedback--check)
  (let ((here (approve-feedback--here)))
    (approve-feedback--goto
     (or (seq-find (lambda (overlay) (> (approve-feedback--line-of overlay) here))
                   (approve-feedback--unresolved-overlays))
         (user-error "No unresolved thread after this")))))

(defun approve-feedback-previous-thread ()
  "Move to the previous line with an unresolved thread, and unfold its threads.
From the first line, that can be the threads above it."
  (interactive)
  (approve-feedback--check)
  (let ((here (approve-feedback--here)))
    (approve-feedback--goto
     (or (seq-find (lambda (overlay) (< (approve-feedback--line-of overlay) here))
                   (reverse (approve-feedback--unresolved-overlays)))
         (user-error "No unresolved thread before this")))))

(add-hook 'approve-clone-picked-functions #'approve-feedback--picked)

(provide 'approve-feedback)

;;; approve-feedback.el ends here
