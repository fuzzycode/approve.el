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
;; your clone"; decisions D45, D46 and D50).  The buffer stays the user's:
;; its major mode, its hooks and its keys, but a prefix key and evil's
;; z keys.  Approve adds overlays and an item in the mode line, and
;; never changes the text.
;;
;; - `approve-feedback-mode' finds the buffer's branch and the pull
;;   request Approve shows for it (approve-clone.el): the open one
;;   updated last, or the one picked with `approve-clone-pick-pull-request'.
;;   It asks before showing a closed or merged one.
;;   `approve-feedback-global-mode', off by default, turns it on by
;;   itself in the buffers of a branch with an open pull request,
;;   without waiting and never asking.
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
;; - The mode line shows a speech bubble and the file's threads, red
;;   when one asks for changes, dim when none is unresolved, then ↻
;;   when GitHub has changes and … while loading: an item in
;;   `global-mode-string', which doom-modeline shows too (D50).  Its
;;   tooltip says the pull request, which of the branch's open pull
;;   requests it is, how the clone stands to GitHub's head, and the
;;   counts, in words.
;; - Selecting the buffer checks the pull request for changes as
;;   selecting its PR buffer does, and a window showing it keeps it
;;   polled (D23, amended): the mode line says ↻ when GitHub has some.
;;   Selecting it, or Emacs getting the focus back, also reads HEAD from
;;   the clone's files: when it moved, the clone is compared with
;;   GitHub's head again, or another branch's pull request shown.
;; - `approve-feedback-command-map' holds the mode's commands, under
;;   `approve-feedback-prefix-key', C-c ' by default (D50).  TAB folds
;;   the threads of a line, T all of them, n and p go to the next and
;;   previous unresolved thread; mouse-1 on a bubble folds its line.
;;   In evil's normal state ]t and [t move too, and za, zo, zc, zR and
;;   zM fold threads, the buffer's other folds where there are none.
;;   `approve-feedback-dispatch' lists them all; mouse-1 on the
;;   mode-line item opens it.
;; - The actions of a PR buffer work here, on the threads of the line
;;   at point (point never enters them; asking which when there are
;;   several) and on GitHub's lines: r R e k + reply, resolve, edit,
;;   delete and react; c and s comment and suggest on a line, or the
;;   region's lines, that map unchanged to a line of the pull request's
;;   diff (decisions D32, D45), and say why not on others ("Line 41 was
;;   edited here…").  The buffer says what is at point
;;   (`approve-context-function'), which threads a line has
;;   (`approve-context-threads-function') and where a range's lines are
;;   (`approve-diff-range-positions-function'); the actions run in the
;;   PR buffer's store, which redraws them all.
;; - Between the buffers: f in a PR buffer or a file view
;;   (`approve-feedback-visit-file') opens the line, thread or file at
;;   point in the worktree that has the pull request's branch, point on
;;   GitHub's line where the buffer has it now; RET in the map
;;   (`approve-feedback-visit-pr') shows the PR buffer at the line at
;;   point.  n and p go on to the pull request's next file with an
;;   unresolved thread, in the clone.
;;
;; Layer rules: a module for a local clone (D36), which the core never
;; requires; approve.el does.  It spans the layers above the clone's
;; own (approve-clone.el, approve-lines.el): its buffers, and the
;; actions and menu of the feedback mode, which arrange the core's
;; commands.  It reads the store through selectors and changes only
;; its own overlays and the mode line of the buffers it is on; the
;; actions change GitHub through the core's pipeline.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'approve-log)
(require 'approve-api)
(require 'approve-model)
(require 'approve-session)
(require 'approve-context)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-pr)
(require 'approve-changes)
(require 'approve-thread)
(require 'approve-diff)
(require 'approve-file)
(require 'approve-nav)
(require 'approve-actions)
(require 'approve-review)
(require 'approve-commands)
(require 'approve-menus)
(require 'approve-clone)
(require 'approve-lines)

(declare-function approve-evil-setup-feedback "approve-evil" ())
(declare-function evil-normalize-keymaps "evil-core" (&optional state))

;;; Options

(defcustom approve-feedback-display-action '(display-buffer-same-window)
  "How your file is shown when a pull request's line opens it in a clone.
A `display-buffer' action, for \\<approve-pr-mode-map>\\[approve-feedback-visit-file] in a PR buffer or a file view.
By default the file takes the window of the pull request, as the
file view does, and \\<approve-feedback-command-map>\\[approve-feedback-visit-pr] in `approve-feedback-command-map' comes back.
To show it in another window:

  (setq approve-feedback-display-action
        \\='((display-buffer-reuse-window display-buffer-pop-up-window)))"
  :type 'sexp
  :group 'approve)

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

(defvar-local approve-feedback--mode-line nil
  "The mode-line item of the buffer, computed when something it says changes.
It is shown through `global-mode-string' (decision D50).")
;; Without this, the mode line would drop the item's faces and tooltip.
(put 'approve-feedback--mode-line 'risky-local-variable t)

(defvar-local approve-feedback--head nil
  "The head the threads are placed from, or nil when the file isn't compared.")

(defvar-local approve-feedback--texts nil
  "GitHub's texts of the file compared with the buffer: ((OID . TEXT) ...).
They are the head's, and the file at each commit an outdated thread
was written on (decision D45); TEXT is \"\" when GitHub has no such file.")

(defvar-local approve-feedback--fetching nil
  "The commits whose text of the file is being fetched from GitHub.")

(defvar-local approve-feedback--error nil
  "Why GitHub's text of the file could not be had, or nil.")

(defvar-local approve-feedback--shown nil
  "The ids of the threads unfolded here; the others are folded (D46).")

(defvar-local approve-feedback--margin nil
  "The buffer's own right margin before the mode widened it: (LOCAL . WIDTH).
LOCAL is non-nil when `right-margin-width' had a local value.")

(defvar-local approve-feedback--head-ref nil
  "What HEAD was in the clone when last read: `approve-clone-head''s value.")

(defvar-local approve-feedback--destination nil
  "Where point goes once the threads are placed, or nil: (TARGET . POSITION).
TARGET is as `approve-feedback--place-of' takes it.  POSITION is
where point was put meanwhile: when it has moved since, it stays.")

;;; Mode

(defvar-keymap approve-feedback-command-map
  :doc "The commands of `approve-feedback-mode', under its prefix key.
The mode binds this map to `approve-feedback-prefix-key', C-c \\='
by default (decision D50); bind it elsewhere too, under a leader key
for example:

  (keymap-set approve-feedback-mode-map \"C-c a\" approve-feedback-command-map)

\\[approve-feedback-dispatch] in it lists them all."
  "g" #'approve-feedback-refresh
  "P" #'approve-clone-pick-pull-request
  "RET" #'approve-feedback-visit-pr
  "<return>" #'approve-feedback-visit-pr
  "TAB" #'approve-feedback-toggle-threads
  "<tab>" #'approve-feedback-toggle-threads
  "T" #'approve-feedback-toggle-all-threads
  "n" #'approve-feedback-next-thread
  "p" #'approve-feedback-previous-thread
  "c" #'approve-comment
  "s" #'approve-suggest
  "C" #'approve-comment-on-pr
  "r" #'approve-reply
  "R" #'approve-resolve
  "e" #'approve-edit
  "k" #'approve-delete
  "+" #'approve-react
  "o" #'approve-browse
  "S" #'approve-submit-review
  "X" #'approve-discard-review
  "?" #'approve-feedback-dispatch)

(defvar-keymap approve-feedback-mode-map
  :doc "Keymap of `approve-feedback-mode'.
It holds `approve-feedback-command-map' under `approve-feedback-prefix-key'.
In evil's normal state, ]t and [t go to the next and previous
unresolved thread, and za, zo, zc, zR and zM fold threads too
\(approve-evil.el, decision D50).")

(defun approve-feedback--set-prefix-key (symbol key)
  "Set SYMBOL, `approve-feedback-prefix-key', to KEY, and bind it.
The old prefix key no longer holds `approve-feedback-command-map' in
`approve-feedback-mode-map'; KEY does, unless nil."
  (when-let* ((old (and (boundp symbol) (symbol-value symbol))))
    (keymap-unset approve-feedback-mode-map old t))
  (set-default symbol key)
  (when key
    (keymap-set approve-feedback-mode-map key approve-feedback-command-map)))

(defcustom approve-feedback-prefix-key "C-c '"
  "The key that holds the feedback mode's commands, or nil for none.
In a buffer with `approve-feedback-mode' on, it is the prefix of
`approve-feedback-command-map', in Emacs and in evil's states alike:
\\<approve-feedback-mode-map>\\[approve-feedback-toggle-threads] unfolds the threads of the line at point,
\\[approve-reply] replies to one and \\[approve-feedback-dispatch] lists every command (decision
D50).  It is a key as `keymap-set' takes it.  Set it with
`setopt' or the Customize interface, which bind the new key:

  (setopt approve-feedback-prefix-key \"C-c a\")"
  :type '(choice (const :tag "None" nil) key)
  :set #'approve-feedback--set-prefix-key
  :group 'approve)

;; A reload of this file makes the keymap again: the key goes back in.
(approve-feedback--set-prefix-key 'approve-feedback-prefix-key approve-feedback-prefix-key)

;;;###autoload
(define-minor-mode approve-feedback-mode
  "Show the review threads of the branch's pull request in this file's buffer.
The buffer's file is in a clone with a branch that heads a pull
request on GitHub.  The mode shows its open pull request updated
last, or the one picked with \\[approve-clone-pick-pull-request]; it asks before showing a
closed or merged one.  It changes no text.  Its commands are under
`approve-feedback-prefix-key': \\<approve-feedback-mode-map>\\[approve-feedback-toggle-threads] unfolds the threads of the
line at point, \\[approve-feedback-dispatch] lists them all.  In evil's normal state,
]t and [t go from thread to thread and the z keys fold them.
The mode line shows a speech bubble and the file's threads, and its
tooltip says the rest: the pull request, its state, and how the
clone stands to it.

\\{approve-feedback-command-map}"
  :lighter nil
  :keymap approve-feedback-mode-map
  :group 'approve
  (if approve-feedback-mode
      (condition-case err
          (if (approve-feedback--start)
              (progn (approve-feedback--add-mode-line)
                     (approve-feedback--setup-evil))
            (approve-feedback--stop)
            (setq approve-feedback-mode nil))
        ((error quit)
         (approve-feedback--stop)
         (setq approve-feedback-mode nil)
         (signal (car err) (cdr err))))
    (approve-feedback--stop)))

(defun approve-feedback--setup-evil ()
  "Bind evil's keys of the mode, when evil is loaded.
That is ]t and [t, and the fold keys (decision D50), bound in
approve-evil.el, which the mode requires then (decision D17)."
  (when (featurep 'evil)
    (require 'approve-evil)
    (approve-evil-setup-feedback)
    (when (and (bound-and-true-p evil-local-mode) (fboundp 'evil-normalize-keymaps))
      (evil-normalize-keymaps))))

;;; Turning it on and off

(defvar approve-feedback--found nil
  "What the mode turns on with, or nil: (BRANCH PR PRS).
BRANCH is as `approve-clone-branch-head' gives it, PR the pull
request to show and PRS the branch's.  The global mode, and the
commands that open a file for a pull request, bind it to what they
found, so the mode turns on with that, asking nothing.")

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
clone of a GitHub repository, or its branch no pull request.  The
branch's pull requests are asked of GitHub once a session, and again
when none of them is open: one may have been opened since."
  (unless buffer-file-name
    (user-error "This buffer visits no file"))
  (if approve-feedback--found
      (progn (apply #'approve-feedback--show approve-feedback--found)
             t)
    (let* ((branch (or (approve-clone-branch-head (file-name-directory buffer-file-name))
                       (user-error "%s is on no branch of a GitHub repository"
                                   (file-name-nondirectory buffer-file-name))))
           (head (plist-get branch :head))
           (prs (approve-clone-wait-pull-requests head))
           (prs (if (approve-clone-shown-pull-request head prs)
                    prs
                  (approve-clone-wait-pull-requests head t)))
           (pr (approve-feedback--choose head prs)))
      (when pr
        (approve-feedback--show branch pr prs)
        t))))

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
        approve-feedback--head nil
        approve-feedback--texts nil
        approve-feedback--fetching nil
        approve-feedback--error nil
        approve-feedback--shown nil
        approve-feedback--destination nil
        approve-feedback--head-ref (approve-clone-head (plist-get branch :clone))
        approve-model-store-buffer (approve-feedback--store-buffer pr)
        approve-context-function #'approve-feedback--context-at
        approve-context-threads-function #'approve-feedback--threads-at
        approve-diff-range-positions-function #'approve-feedback--range-positions)
  (approve-lines-unwatch)
  (approve-feedback--draw nil)
  (approve-changes-follow-selection)
  (add-hook 'window-selection-change-functions #'approve-feedback--on-window nil t)
  (add-hook 'window-buffer-change-functions #'approve-feedback--on-window nil t)
  (unless (advice-function-member-p #'approve-feedback--on-focus after-focus-change-function)
    (add-function :after after-focus-change-function #'approve-feedback--on-focus))
  (approve-feedback-redraw))

(defun approve-feedback--stop ()
  "Take what the mode added away from the current buffer."
  (approve-changes-follow-selection 'stop)
  (remove-hook 'window-selection-change-functions #'approve-feedback--on-window t)
  (remove-hook 'window-buffer-change-functions #'approve-feedback--on-window t)
  (approve-lines-unwatch)
  (approve-feedback--draw nil)
  (dolist (variable '(approve-feedback--branch approve-feedback--pull-request
                      approve-feedback--open approve-feedback--path
                      approve-feedback--clone-state approve-feedback--mode-line
                      approve-feedback--head approve-feedback--texts approve-feedback--fetching
                      approve-feedback--error approve-feedback--shown
                      approve-feedback--head-ref approve-feedback--destination
                      approve-model-store-buffer
                      approve-context-function approve-context-threads-function
                      approve-diff-range-positions-function))
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

;;; Following the clone and GitHub

(defun approve-feedback-follow-changes ()
  "Say in the clone's buffers of the current PR buffer what changed on GitHub.
A check for changes answered (decision D23): the mode line says ↻
while GitHub has changes.  approve.el puts it on `approve-changes-hook'."
  (dolist (buffer (approve-feedback--buffers (current-buffer)))
    (with-current-buffer buffer
      (approve-feedback--update-mode-line))))

(defun approve-feedback--follow-branch (branch prs)
  "Show the pull request of BRANCH among PRS, now that HEAD is on it.
The same pull request stays; another one takes its place; without an
open one, the mode goes off and says why."
  (let ((pr (approve-clone-shown-pull-request (plist-get branch :head) prs)))
    (cond
     ((null pr)
      (approve-feedback-mode -1)
      (message "Approve: no open pull request for %s" (plist-get branch :branch)))
     ((equal (alist-get 'id pr) (alist-get 'id approve-feedback--pull-request))
      (setq approve-feedback--branch branch
            approve-feedback--clone-state nil)
      (approve-feedback-redraw))
     (t (approve-feedback--show branch pr prs)))))

(defun approve-feedback--head-moved ()
  "Find the pull request of the buffer's branch again: HEAD moved.
That is a commit, a checkout or a pull (flow F21).  On the same
branch, the clone is compared with GitHub's head again; on another,
its pull request is asked for, once a session."
  (let ((branch (approve-clone-branch-head (file-name-directory buffer-file-name)))
        (buffer (current-buffer)))
    (cond
     ((null branch)
      (approve-feedback-mode -1)
      (message "Approve: %s is on no branch of a GitHub repository now"
               (file-name-nondirectory buffer-file-name)))
     ((approve-feedback--same-head-p (plist-get branch :head)
                                     (plist-get approve-feedback--branch :head))
      (setq approve-feedback--branch branch
            approve-feedback--clone-state nil)
      (approve-feedback-redraw))
     (t
      (approve-clone-branch-pull-requests
       (plist-get branch :head)
       :callback (lambda (prs)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when approve-feedback-mode
                         (approve-feedback--follow-branch branch prs)))))
       :errorback (lambda (err)
                    (approve-log 'warn "The pull requests of %s: %s"
                                 (plist-get branch :branch)
                                 (approve-api-error-message err))))))))

(defun approve-feedback--check-head (buffer)
  "Follow HEAD in the clone of BUFFER when it moved since it was last read."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'approve-feedback-mode buffer))
    (with-current-buffer buffer
      (let ((head (approve-clone-head (plist-get approve-feedback--branch :clone))))
        (unless (equal head approve-feedback--head-ref)
          (setq approve-feedback--head-ref head)
          (condition-case-unless-debug err
              (approve-feedback--head-moved)
            (error (approve-log-condition "Following HEAD in a clone's buffer" err))))))))

(defun approve-feedback--on-window (window)
  "Look whether HEAD moved soon, as WINDOW, which shows this buffer, is selected.
It is on the buffer's `window-selection-change-functions' and
`window-buffer-change-functions', which run during redisplay."
  (when (and (eq window (selected-window))
             (eq (window-buffer window) (current-buffer)))
    (run-at-time 0 nil #'approve-feedback--check-head (current-buffer))))

(defun approve-feedback--on-focus ()
  "Look whether HEAD moved soon, when Emacs gets the focus back.
It is on `after-focus-change-function': a commit in a terminal
comes back with the focus."
  (when (frame-focus-state)
    (run-at-time 0 nil #'approve-feedback--check-head (window-buffer (selected-window)))))

;;; GitHub's texts of the file

(defun approve-feedback--relevant-p ()
  "Return non-nil if the pull request changes the file, or has threads on it.
Otherwise the mode line is all the mode shows, and no text is compared."
  (cl-flet ((here-p (entry) (equal (alist-get 'path entry) approve-feedback--path)))
    (or (seq-some #'here-p (approve-pr-list-fields 'files '(path)))
        (seq-some #'here-p (approve-pr-list-fields 'reviewThreads '(path))))))

(defun approve-feedback--original-commit (thread)
  "Return the commit THREAD was written on, from its first comment, or nil."
  (alist-get 'oid (alist-get 'originalCommit
                             (car (approve-model-nodes (alist-get 'comments thread))))))

(defun approve-feedback--outdated-p (thread)
  "Return non-nil if THREAD is on a line of the head that changed since.
Its place is its original line at the commit it was written on."
  (let-alist thread
    (and (null .line) .originalLine (equal .diffSide "RIGHT")
         (not (equal .subjectType "FILE")))))

(defun approve-feedback--wanted (head)
  "Return the commits whose text of the file the threads need: HEAD first.
The others are those the outdated threads were written on."
  (delete-dups
   (cons head (delq nil (mapcar (lambda (thread)
                                  (and (approve-feedback--outdated-p thread)
                                       (approve-feedback--original-commit thread)))
                                (approve-pr-threads-for-file approve-feedback--path))))))

(defun approve-feedback--compare (oid text)
  "Compare TEXT, GitHub's file at OID, with the buffer, now and after edits.
TEXT is \"\" when GitHub has no such file.  Each answer draws the
threads again (`approve-feedback--decorate')."
  (setf (alist-get oid approve-feedback--texts nil nil #'equal) text)
  (setq approve-feedback--error nil)
  (approve-lines-watch oid text
                       :callback (lambda (_map) (approve-feedback--decorate))
                       :errorback (lambda (err)
                                    (setq approve-feedback--error (cadr err))
                                    (approve-feedback--update-mode-line))))

(defun approve-feedback--fetched (buffer oid text &optional err)
  "Compare TEXT, GitHub's file at OID, with BUFFER, or note ERR."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and approve-feedback-mode (member oid approve-feedback--fetching))
        (setq approve-feedback--fetching (delete oid approve-feedback--fetching))
        (if err
            (setq approve-feedback--error (approve-api-error-message err))
          (approve-feedback--compare oid text))
        (approve-feedback--update-mode-line)))))

(defun approve-feedback--fetch (oid)
  "Get GitHub's text of the buffer's file at OID, and compare it with the buffer.
It comes from the clone when it has the commit, else from GitHub,
kept for the session as the file view keeps it (decision D45)."
  (if-let* ((text (approve-clone-file-text (plist-get approve-feedback--branch :clone)
                                           oid approve-feedback--path)))
      (approve-feedback--compare oid text)
    (push oid approve-feedback--fetching)
    (let ((buffer (current-buffer)))
      (approve-file-fetch oid approve-feedback--path
                          :callback (lambda (text)
                                      (approve-feedback--fetched buffer oid text))
                          :errorback (lambda (err)
                                       (approve-feedback--fetched buffer oid nil err))))))

(defun approve-feedback--follow-head ()
  "Follow GitHub's head: compare the clone and the file with it when it moved.
The file is compared with the texts the threads need, the head's and
the outdated threads' commits', and no longer with the others."
  (let* ((oid (approve-pr-field 'headRefOid))
         (wanted (and (approve-feedback--relevant-p) (approve-feedback--wanted oid))))
    (unless (equal oid (plist-get approve-feedback--clone-state :oid))
      (setq approve-feedback--clone-state
            (append (list :oid oid)
                    (approve-clone-state (plist-get approve-feedback--branch :clone) oid))))
    (setq approve-feedback--head (and wanted oid))
    (dolist (text approve-feedback--texts)
      (unless (member (car text) wanted)
        (approve-lines-unwatch (car text))))
    (setq approve-feedback--texts (seq-filter (lambda (text) (member (car text) wanted))
                                              approve-feedback--texts))
    (dolist (each wanted)
      (unless (or (assoc each approve-feedback--texts)
                  (member each approve-feedback--fetching))
        (approve-feedback--fetch each)))))

(defun approve-feedback--github-line (oid line)
  "Return LINE of GitHub's text of the file at OID, or nil."
  (when-let* ((text (cdr (assoc oid approve-feedback--texts)))
              ((and line (> line 0))))
    (let ((start 0))
      (while (and start (> line 1))
        (setq start (string-search "\n" text start)
              start (and start (1+ start))
              line (1- line)))
      (when (and start (< start (length text)))
        (string-remove-suffix
         "\r" (substring text start (or (string-search "\n" text start) (length text))))))))

;;; Where the threads go

(defun approve-feedback--hunks ()
  "Return the parsed patch of the buffer's file in the pull request, or nil."
  (approve-diff-parse (alist-get 'patch (approve-diff-file-entry approve-feedback--path))))

(defun approve-feedback--from-base (hunks line)
  "Return where the base's LINE is at the head, by the patch HUNKS.
The value is (HEAD . REMOVED): HEAD is the line at the head, or when
the pull request REMOVED it, the head's line before where it was, 0
at the top."
  (let ((old 1) (new 1))
    (catch 'found
      (dolist (hunk hunks)
        (when (< line (approve-diff-hunk-old-start hunk))
          (throw 'found (cons (+ line (- new old)) nil)))
        (setq old (approve-diff-hunk-old-start hunk)
              new (approve-diff-hunk-new-start hunk))
        (pcase-dolist (`(,kind ,at ,_ ,_) (approve-diff-hunk-lines hunk))
          (pcase kind
            ('ctx (when (= at line)
                    (throw 'found (cons new nil)))
                  (cl-incf old)
                  (cl-incf new))
            ('del (when (= at line)
                    (throw 'found (cons (1- new) t)))
                  (cl-incf old))
            ('add (cl-incf new)))))
      (cons (+ line (- new old)) nil))))

(defun approve-feedback--removed-lines (hunks thread)
  "Return the text of the base's lines THREAD is on, from the patch HUNKS."
  (let-alist thread
    (let ((from (or .startLine .line)) (lines nil))
      (dolist (hunk hunks)
        (pcase-dolist (`(,kind ,old ,_ ,text) (approve-diff-hunk-lines hunk))
          (when (and (memq kind '(del ctx)) old (<= from old .line))
            (push text lines))))
      (nreverse lines))))

(defun approve-feedback--where (maps hunks thread)
  "Return where THREAD goes in the buffer: (LINE . WHERE), or nil.
MAPS are the maps of GitHub's texts by commit; HUNKS the file's
patch.  A thread on the whole file goes above the first line (LINE
0, WHERE `file'); one on a line of the head where the head's map
says, WHERE being `same', `edited' or `deleted'; one on lines the
pull request removed under the head's line before them (`removed');
an outdated one where its original line is now, by the map of its
commit (`outdated').  An outdated thread waits for that map."
  (let-alist thread
    (let ((head (cdr (assoc approve-feedback--head maps))))
      (cond
       ((equal .subjectType "FILE") (cons 0 'file))
       ((and .line (equal .diffSide "RIGHT")) (approve-lines-to-buffer head .line))
       ((and .line (equal .diffSide "LEFT"))
        (pcase-let ((`(,line . ,removed) (approve-feedback--from-base hunks .line)))
          (cond ((not removed) (approve-lines-to-buffer head line))
                ((zerop line) (cons 0 'removed))
                (t (cons (car (approve-lines-to-buffer head line)) 'removed)))))
       ((approve-feedback--outdated-p thread)
        (when-let* ((map (cdr (assoc (approve-feedback--original-commit thread) maps))))
          (cons (car (approve-lines-to-buffer map .originalLine)) 'outdated)))))))

(defun approve-feedback--place (maps)
  "Return the threads of the buffer's file by the lines they go under, by MAPS.
MAPS are the maps of GitHub's texts by commit, the head's among
them.  The value is ((LINE . ENTRIES) ...), in order of LINE, 0 above
the first line.  Each of ENTRIES is (THREAD . WHERE), as
`approve-feedback--where' says, in the store's order."
  (let ((hunks (approve-feedback--hunks))
        (lines nil))
    (dolist (thread (approve-pr-threads-for-file approve-feedback--path))
      (when-let* ((where (approve-feedback--where maps hunks thread)))
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
It is the speech bubble, or a triangle ◂ when they are all on lines
the pull request removed after the line, and the count of unresolved
threads, or of all when none is.  Its colour says what matters most
\(decision D46): red when one asks for changes, its id in ASKING
\(D34); gold when the line was edited here; dim when all are resolved
or outdated."
  (let* ((threads (mapcar #'car entries))
         (unresolved (seq-count (lambda (thread) (not (alist-get 'isResolved thread)))
                                threads)))
    (approve-face-add
     (concat (if (seq-every-p (lambda (entry) (eq (cdr entry) 'removed)) entries)
                 "◂"
               (approve-icon 'comment))
             (number-to-string (if (> unresolved 0) unresolved (length threads))))
     (cond ((seq-some (lambda (thread) (member (alist-get 'id thread) asking)) threads)
            'approve-thread-mark-changes-requested)
           ((seq-some #'approve-feedback--edited-p entries) 'approve-edited-here)
           ((seq-some (lambda (entry) (and (not (alist-get 'isResolved (car entry)))
                                           (not (eq (cdr entry) 'outdated))))
                      entries)
            'approve-diff-bubble)
           (t 'approve-dim)))))

(defun approve-feedback--context (entry hunks)
  "Return the rows that start ENTRY's thread when it is unfolded, or nil.
They say where it was: GitHub's text of a line edited or deleted
here, an outdated thread's line at its commit, the lines the pull
request removed; HUNKS are the file's patch."
  (let ((thread (car entry)))
    (cl-flet ((row (label text)
                (concat (approve-face (concat "┊ " label) 'approve-dim) " "
                        (approve-face (or text "") 'approve-edited-here) "\n")))
      (pcase (cdr entry)
        ('edited (row "on GitHub:"
                      (approve-feedback--github-line approve-feedback--head
                                                     (alist-get 'line thread))))
        ('deleted (row "deleted here:"
                       (approve-feedback--github-line approve-feedback--head
                                                      (alist-get 'line thread))))
        ('outdated
         (let ((oid (approve-feedback--original-commit thread)))
           (row (format "at %s:" (substring oid 0 (min 7 (length oid))))
                (approve-feedback--github-line oid (alist-get 'originalLine thread)))))
        ('removed
         (mapconcat (lambda (text) (row "removed:" text))
                    (approve-feedback--removed-lines hunks thread) ""))))))

(defun approve-feedback--bubble-help (entries unfolded)
  "Return the tooltip of the bubble of ENTRIES, UNFOLDED or not.
Such as \"1 thread · edited here: GitHub has  (retries 3))\"."
  (let ((edited (seq-find #'approve-feedback--edited-p entries))
        (kinds (delete-dups (mapcar #'cdr entries))))
    (concat (approve-feedback--count (length entries) "thread")
            (and edited
                 (format " · %s here: GitHub has %s" (cdr edited)
                         (or (approve-feedback--github-line approve-feedback--head
                                                           (alist-get 'line (car edited)))
                             "")))
            (and (memq 'outdated kinds) " · outdated: where its line was")
            (and (memq 'removed kinds) " · on lines the pull request removed after this one")
            (if unfolded "\nmouse-1: fold" "\nmouse-1: unfold"))))

(defun approve-feedback--threads-text (entries)
  "Return the threads of ENTRIES drawn as the file view draws them.
Each starts with where it was, when that isn't the line above it
\(`approve-feedback--context').  Their boxes name no keys: the
dispatch does (decision D46)."
  (let* ((owner approve-model-store-buffer)
         (hunks (approve-feedback--hunks))
         (contexts (mapcar (lambda (entry) (approve-feedback--context entry hunks))
                           entries)))
    (with-temp-buffer
      ;; New comments are marked as in the PR buffer (D44).
      (setq approve-model-store-buffer owner)
      (cl-loop for entry in entries
               for context in contexts
               do (let ((start (point)))
                    (when context
                      (insert context))
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
  "Draw the threads under their lines again, by the latest maps.
A map older than the buffer's text asks for another comparison,
and the threads stay where the text took them until it answers.
An outdated thread waits for the map of its commit.  Point goes
where `approve-feedback--destination' says, once that is known."
  (let* ((maps (mapcar (lambda (text) (cons (car text) (approve-lines-watched-map (car text))))
                       approve-feedback--texts))
         (head (cdr (assoc approve-feedback--head maps))))
    (cond
     ((null approve-feedback--head) (approve-feedback--draw nil))
     ((null head))
     ((seq-some (lambda (map) (and (cdr map) (not (approve-lines-current-p (cdr map)))))
                maps)
      (approve-lines-update))
     (t (approve-feedback--draw (approve-feedback--place (seq-filter #'cdr maps)))))
    (approve-feedback--update-mode-line)
    (when approve-feedback--destination
      (approve-feedback--reach))))

(defun approve-feedback-redraw ()
  "Draw what the current buffer shows of its pull request again, from the store.
Every redraw of the PR buffer calls it in the buffers of its clone."
  (when approve-feedback-mode
    (if (approve-feedback--loaded-p)
        (progn
          (approve-feedback--follow-head)
          (approve-feedback--decorate))
      (approve-feedback--update-mode-line))))

;;; The mode-line item

(defconst approve-feedback--mode-line-construct
  '(approve-feedback-mode ("" approve-feedback--mode-line))
  "What `global-mode-string' holds for the feedback mode (decision D50).
It shows `approve-feedback--mode-line' in the buffers where the mode
is on, wherever the mode line shows `global-mode-string': the
default one and doom-modeline's alike.")

(defun approve-feedback--add-mode-line ()
  "Put the feedback mode's item in `global-mode-string', once."
  (unless (and (listp global-mode-string)
               (member approve-feedback--mode-line-construct global-mode-string))
    (setq global-mode-string
          (append (if (listp global-mode-string) global-mode-string (list global-mode-string))
                  (list approve-feedback--mode-line-construct)))))

(defun approve-feedback--count (count noun)
  "Return \"COUNT NOUN\", NOUN in the plural unless COUNT is 1."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun approve-feedback--thread-counts ()
  "Return the threads counted in the mode line, as a plist.
:here, the threads of this file; :unresolved, those of them that are
unresolved; :asking, those that ask for changes (decision D34); :all,
the unresolved threads of the pull request."
  (let ((here 0) (unresolved 0) (all 0)
        (asking (seq-count (lambda (thread)
                             (equal (alist-get 'path thread) approve-feedback--path))
                           (approve-pr-threads-asking-for-changes))))
    (dolist (thread (approve-pr-list-fields 'reviewThreads '(path isResolved)))
      (let ((mine (equal (alist-get 'path thread) approve-feedback--path))
            (open (not (alist-get 'isResolved thread))))
        (when mine
          (cl-incf here))
        (when open
          (cl-incf all)
          (when mine
            (cl-incf unresolved)))))
    (list :here here :unresolved unresolved :asking asking :all all)))

(defun approve-feedback--clone-arrows (state)
  "Return the arrows of the clone's STATE, as the dispatch shows them, or nil.
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

(defun approve-feedback--threads-words (counts)
  "Return what COUNTS, of `approve-feedback--thread-counts', mean, in words."
  (let ((here (plist-get counts :here))
        (unresolved (plist-get counts :unresolved))
        (asking (plist-get counts :asking)))
    (concat (if (zerop here)
                "No thread in this file"
              (concat (approve-feedback--count here "thread") " in this file"
                      (cond ((= unresolved here)
                             (if (= here 1) ", unresolved" ", all unresolved"))
                            ((zerop unresolved)
                             (if (= here 1) ", resolved" ", all resolved"))
                            (t (format ", %d unresolved" unresolved)))
                      (and (> asking 0)
                           (format ", %d asking for changes" asking))))
            (format "; %d unresolved in the pull request" (plist-get counts :all)))))

(defun approve-feedback--tooltip (fields counts loading changes)
  "Return the mode-line item's tooltip: what Approve knows, in words.
FIELDS are the pull request's (`number', `title', `state', `isDraft',
`baseRefName'); COUNTS those of `approve-feedback--thread-counts', or
nil before the store is loaded; LOADING and CHANGES whether it loads,
and whether GitHub has changes."
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
                   (and counts (approve-feedback--threads-words counts))
                   (and loading "Loading the pull request…")
                   (and approve-feedback--error
                        (format "Could not compare the file with GitHub's: %s"
                                approve-feedback--error))
                   (and changes
                        (substitute-command-keys
                         "Updated on GitHub: \\<approve-feedback-mode-map>\\[approve-feedback-refresh] to see it"))
                   (substitute-command-keys
                    "mouse-1, \\<approve-feedback-mode-map>\\[approve-feedback-dispatch]: Approve\\='s menu")))
       "\n"))))

(defvar approve-feedback--mode-line-map
  (make-mode-line-mouse-map 'mouse-1 #'approve-feedback-mouse-dispatch)
  "Keymap of the mode-line item: a click opens the dispatch (decision D50).")

(defun approve-feedback--mode-line-text ()
  "Return the mode-line item of the current buffer (decision D50).
It is the speech bubble and the file's threads, resolved or not:
red when one asks for changes (decision D34), dim when none is
unresolved.  Then ↻ when GitHub has changes (decision D23), and …
while the pull request or GitHub's text of the file loads.  Its
tooltip says the rest: the pull request, the branch, the clone's
state and the counts, in words."
  (let* ((loaded (approve-feedback--loaded-p))
         (fields (if loaded
                     (approve-pr-fields '(number title state isDraft baseRefName))
                   approve-feedback--pull-request))
         (loading (or (not loaded) (approve-pr-loading-p) approve-feedback--fetching))
         (counts (and loaded (approve-feedback--thread-counts)))
         (changes (and loaded (approve-session-get :changes)))
         (bubble (concat (approve-icon 'comment)
                         (if counts (number-to-string (plist-get counts :here)) ""))))
    (propertize
     (concat (cond ((null counts) bubble)
                   ((> (plist-get counts :asking) 0)
                    (approve-face bubble 'approve-thread-mark-changes-requested))
                   ((zerop (plist-get counts :unresolved)) (approve-face bubble 'approve-dim))
                   (t bubble))
             (and changes (concat " " (approve-face "↻" 'approve-updated)))
             (and loading " …")
             " ")
     'help-echo (approve-feedback--tooltip fields counts loading changes)
     'mouse-face 'mode-line-highlight
     'local-map approve-feedback--mode-line-map)))

(defun approve-feedback--update-mode-line ()
  "Compute the mode-line item of the current buffer again."
  (setq approve-feedback--mode-line (approve-feedback--mode-line-text))
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
  (approve-feedback--toggle (approve-feedback--toggle-preflight)))

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
  (let* ((overlays (approve-feedback--toggle-all-preflight))
         (show (if (seq-some (lambda (overlay)
                               (not (approve-feedback--unfolded-p
                                     (overlay-get overlay 'approve-feedback-entries))))
                             overlays)
                   'show
                 'hide)))
    (dolist (overlay overlays)
      (approve-feedback--toggle overlay show))))

(defun approve-feedback-fold-threads (action)
  "Fold or unfold threads for a folding ACTION, as the fold keys do.
ACTION is `:toggle', `:open' or `:close' for the threads of the
line at point, `:open-all' or `:close-all' for every thread of the
file.  Return non-nil when there were threads to act on; nil when
the line, or the file, has none, or the mode is off."
  (when approve-feedback-mode
    (if (memq action '(:open-all :close-all))
        (when-let* ((overlays (approve-feedback--overlays)))
          (dolist (overlay overlays)
            (approve-feedback--toggle overlay (if (eq action :open-all) 'show 'hide)))
          t)
      (when-let* ((overlay (approve-feedback--overlay-at (point))))
        (approve-feedback--toggle overlay (pcase action
                                            (:toggle nil)
                                            (:close 'hide)
                                            (_ 'show)))
        t))))

(defun approve-feedback--key-elsewhere ()
  "Run what the keys of the current command run when the mode is off.
That is the buffer's own folding, under evil's z keys: hideshow's,
outline's, the user's own.  Return non-nil when there is such a
command."
  (let ((command (let ((approve-feedback-mode nil))
                   (key-binding (this-command-keys-vector) t))))
    (when (and command (commandp command)
               (not (eq command this-command)))
      (call-interactively command)
      t)))

(defun approve-feedback--fold (action what)
  "Do the folding ACTION on threads, else, or also, what the key does elsewhere.
ACTION is as `approve-feedback-fold-threads' takes it.  A line's
fold passes the key on when the line has no threads; a fold of
every thread passes it on too, as vim's zR and zM open and close
every fold.  WHAT names what is missing in the echo area when
nothing was done."
  (let* ((done (approve-feedback-fold-threads action))
         (passed (and (or (not done) (memq action '(:open-all :close-all)))
                      (approve-feedback--key-elsewhere))))
    (unless (or done passed)
      (message "No threads %s" what))))

(defun approve-feedback-toggle-fold ()
  "Fold or unfold the threads of the line at point, else what the key folds.
On a line without threads, the key does what it does without the
feedback mode: evil's za folds code as the buffer does (decision D50)."
  (interactive)
  (approve-feedback--fold :toggle "on this line"))

(defun approve-feedback-open-fold ()
  "Unfold the threads of the line at point, else what the key unfolds."
  (interactive)
  (approve-feedback--fold :open "on this line"))

(defun approve-feedback-close-fold ()
  "Fold the threads of the line at point, else what the key folds."
  (interactive)
  (approve-feedback--fold :close "on this line"))

(defun approve-feedback-open-all-folds ()
  "Unfold every thread of the file, and what the key unfolds without the mode.
As vim's zR unfolds every fold: the buffer's own too (decision D50)."
  (interactive)
  (approve-feedback--fold :open-all "in this file"))

(defun approve-feedback-close-all-folds ()
  "Fold every thread of the file, and what the key folds without the mode.
As vim's zM folds every fold: the buffer's own too (decision D50)."
  (interactive)
  (approve-feedback--fold :close-all "in this file"))

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
  "Move to the next line with an unresolved thread, and unfold its threads.
Past the last one of the file, the first of the next of the pull
request's files with one, in the diff's order, visited in the clone;
past the last file, the first again."
  (interactive)
  (approve-feedback--check)
  (let ((here (approve-feedback--here)))
    (if-let* ((overlay (seq-find (lambda (overlay) (> (approve-feedback--line-of overlay) here))
                                 (approve-feedback--unresolved-overlays))))
        (approve-feedback--goto overlay)
      (approve-feedback--other-file t))))

(defun approve-feedback-previous-thread ()
  "Move to the previous line with an unresolved thread, and unfold its threads.
From the first line, that can be the threads above it.  Before the
first one of the file, the last of the previous of the pull
request's files with one, visited in the clone; before the first
file, the last again."
  (interactive)
  (approve-feedback--check)
  (let ((here (approve-feedback--here)))
    (if-let* ((overlay (seq-find (lambda (overlay) (< (approve-feedback--line-of overlay) here))
                                 (reverse (approve-feedback--unresolved-overlays)))))
        (approve-feedback--goto overlay)
      (approve-feedback--other-file nil))))

(defun approve-feedback--thread-files ()
  "Return the pull request's files with an unresolved thread, in the diff's order."
  (let ((unresolved (make-hash-table :test #'equal)))
    (dolist (thread (approve-pr-list-fields 'reviewThreads '(path isResolved)))
      (unless (alist-get 'isResolved thread)
        (puthash (alist-get 'path thread) t unresolved)))
    (seq-filter (lambda (path) (gethash path unresolved))
                (mapcar (lambda (file) (alist-get 'path file)) (approve-pr-files)))))

(defun approve-feedback--other-file (forward)
  "Go to the next of the pull request's files with an unresolved thread.
That is its first unresolved thread, visited in the clone; the
previous file and its last thread unless FORWARD.  The files are in
the diff's order, and those the clone hasn't are passed by, saying
so.  Past the last file, it goes on from the first, which can be
this one again."
  (unless (approve-feedback--loaded-p)
    (user-error "No unresolved thread %s this" (if forward "after" "before")))
  (let* ((clone (plist-get approve-feedback--branch :clone))
         (files (approve-feedback--thread-files))
         (files (if forward files (reverse files)))
         (index (seq-position files approve-feedback--path))
         (order (if index
                    (append (nthcdr (1+ index) files) (seq-take files (1+ index)))
                  files))
         (missing nil)
         (next (seq-find (lambda (path)
                           (or (file-exists-p (expand-file-name path clone))
                               (progn (push path missing) nil)))
                         order))
         (wrapped (and next index (<= (seq-position files next) index)))
         (note (string-join
                (delq nil (list (and missing
                                     (format "%s not in the clone"
                                             (if (cdr missing)
                                                 (format "passed by %d files" (length missing))
                                               (concat "passed by " (car missing)))))
                                (and wrapped
                                     (if (equal next approve-feedback--path)
                                         (format "from the %s unresolved thread again"
                                                 (if forward "first" "last"))
                                       (format "from the pull request's %s file again"
                                               (if forward "first" "last"))))))
                "; ")))
    (cond
     ((null next)
      (user-error "No unresolved thread in the pull request%s"
                  (if (string-empty-p note) "" (concat "'s files here: " note))))
     ((equal next approve-feedback--path)
      (approve-feedback--goto
       (or (funcall (if forward #'car (lambda (list) (car (last list))))
                    (approve-feedback--unresolved-overlays))
           (user-error "No unresolved thread is placed here"))))
     (t
      (approve-feedback--visit (expand-file-name next clone) approve-feedback--pull-request
                               (list :unresolved (if forward 'first 'last)))))
    (unless (string-empty-p note)
      (message "Approve: %s" note))))

(add-hook 'approve-clone-picked-functions #'approve-feedback--picked)

;;; What is at point

(defun approve-feedback--threads-at (pos)
  "Return the ids of the threads drawn under the line at POS.
It is the buffer's `approve-context-threads-function'.  On the first
line, those above it when the line has none of its own, as
\\[approve-feedback-toggle-threads] folds them."
  (when-let* ((overlay (approve-feedback--overlay-at pos)))
    (mapcar (lambda (entry) (alist-get 'id (car entry)))
            (overlay-get overlay 'approve-feedback-entries))))

(defun approve-feedback--lines-at (pos)
  "Return the buffer's lines at POS, as (FIRST . LAST).
That is the region's lines when POS is point and the region is
active (a region ending at the start of a line leaves it out), else
POS's line."
  (save-restriction
    (widen)
    (if (and (= pos (point)) (use-region-p))
        (let ((start (region-beginning))
              (end (region-end)))
          (when (and (> end start) (save-excursion (goto-char end) (bolp)))
            (setq end (1- end)))
          (cons (line-number-at-pos start) (line-number-at-pos end)))
      (let ((line (line-number-at-pos pos)))
        (cons line line)))))

(defun approve-feedback--github-lines (first last)
  "Return GitHub's lines in the diff where the buffer's lines FIRST to LAST are.
The value is their `approve-line' plists, in order, when each is
GitHub's own line at the pull request's head, unchanged, with none
deleted between them, and in the pull request's diff: the only lines
GitHub takes a comment on (decisions D32 and D45).  Otherwise it is
why not, a string about the first line that isn't, such as \"Line 41
was edited here; GitHub takes comments only on its own lines\".  The
lines are compared as they are now (`approve-lines-current-map')."
  (let ((map (and approve-feedback--head (approve-lines-current-map approve-feedback--head))))
    (cond
     ((null approve-feedback--head)
      (format "Line %d isn't in the pull request's diff; GitHub takes no comment there" first))
     ((null map)
      (if approve-feedback--error
          (format "Could not compare the file with GitHub's: %s" approve-feedback--error)
        "Approve is still comparing the file with GitHub's: try again in a moment"))
     (t
      (let ((diff (make-hash-table))
            (lines nil)
            (previous nil))
        (pcase-dolist (`(,line . ,property)
                       (approve-file-diff-lines approve-feedback--path
                                                (approve-feedback--hunks) nil))
          (puthash line property diff))
        (catch 'why
          (dolist (line (number-sequence first last))
            (pcase-let ((`(,github . ,state) (approve-lines-to-github map line)))
              (when-let* ((why (cond
                                ((memq state '(edited added))
                                 (format "Line %d was %s here; GitHub takes comments only on its own lines"
                                         line state))
                                ((and previous (/= github (1+ previous)))
                                 (format "Lines were deleted here above line %d; GitHub takes comments only on its own lines"
                                         line))
                                ((not (gethash github diff))
                                 (format "Line %d isn't in the pull request's diff; GitHub takes no comment there"
                                         line)))))
                (throw 'why why))
              (push (gethash github diff) lines)
              (setq previous github)))
          (nreverse lines)))))))

(defun approve-feedback--code (line why)
  "Return the context of the buffer's LINE, which GitHub takes no comment on.
It is a `code' context, as on a line of the file view outside the
diff, with WHY, the reason, as :why.  Its :line is GitHub's line it
is, or replaced, when the head's map says."
  (let* ((map (and approve-feedback--head (approve-lines-watched-map approve-feedback--head)))
         (github (and (approve-lines-current-p map) (car (approve-lines-to-github map line))))
         (github (and github (> github 0) github)))
    (list :kind 'code :path approve-feedback--path :name approve-feedback--path
          :side "RIGHT" :line github :why why
          :url (approve-context-blob-url (approve-pr-field 'headRefOid)
                                         approve-feedback--path github))))

(defun approve-feedback--context-at (pos)
  "Return the context at POS: GitHub's line there, or why GitHub takes none.
It is the buffer's `approve-context-function'.  Where the buffer's
line is GitHub's, unchanged and in the pull request's diff, it is the
context the diff gives there (`line'), or with the region, from its
first line to its last (`lines'); else it is `code', whose :why says
why GitHub takes no comment there.  Nil until the pull request has
loaded."
  (when (approve-feedback--loaded-p)
    (pcase-let* ((`(,first . ,last) (approve-feedback--lines-at pos))
                 (lines (approve-feedback--github-lines first last)))
      (cond ((stringp lines) (approve-feedback--code first lines))
            ((and (= pos (point)) (use-region-p))
             (approve-context-of-lines (car lines) (car (last lines))))
            (t (approve-context-of-line (car lines)))))))

(defun approve-feedback--range-positions (range)
  "Return where the lines of RANGE start here, in order.
It is the buffer's `approve-diff-range-positions-function'.  RANGE's
lines of the head that the buffer has as GitHub has them are found
by the head's map; the base's lines, and those edited here, are not."
  (when-let* (((equal (plist-get range :path) approve-feedback--path))
              (map (and approve-feedback--head
                        (approve-lines-watched-map approve-feedback--head)))
              ((approve-lines-current-p map)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((positions nil))
          (dolist (line (approve-diff-range-lines range))
            (when-let* ((new (nth 2 line))
                        (here (approve-lines-to-buffer map new))
                        ((eq (cdr here) 'same)))
              (goto-char (point-min))
              (when (zerop (forward-line (1- (car here))))
                (push (point) positions))))
          (nreverse positions))))))

;;; Going to a place of GitHub's

(defun approve-feedback--head-line (side line)
  "Return where LINE on SIDE is at the pull request's head: (LINE . REMOVED).
SIDE \"LEFT\" is the base: by the file's patch, a line the pull request
removed is REMOVED, and goes to the head's line before where it was,
0 at the top."
  (if (equal side "LEFT")
      (approve-feedback--from-base (approve-feedback--hunks) line)
    (cons line nil)))

(defun approve-feedback--overlay-of-line (line)
  "Return the overlay of the threads under LINE, 0 above the first line, or nil."
  (seq-find (lambda (overlay) (= (approve-feedback--line-of overlay) line))
            (approve-feedback--overlays)))

(defconst approve-feedback--thread-notes
  '((edited . "The thread's line was edited here: this line is in its place")
    (deleted . "The thread's line was deleted here: it was after this line")
    (outdated . "The thread is outdated: its line was here")
    (removed . "The thread is on lines the pull request removed after this one"))
  "What the echo area says when point goes to a thread that isn't under its line.
By where the thread is drawn, as `approve-feedback--where' says.")

(defun approve-feedback--line-note (line head removed state)
  "Return what the echo area says when point goes to GitHub's LINE, or nil.
HEAD is LINE at the pull request's head, REMOVED non-nil when the
pull request removed it from the base, STATE where the head's line
is in the buffer, as `approve-lines-to-buffer' says."
  (cond
   (removed (format "The pull request removed line %d of the base; it was %s"
                    line (if (zerop head) "above the first line" "after this line")))
   ((eq state 'edited)
    (format "GitHub's line %d was edited here; this line is in its place" head))
   ((eq state 'deleted)
    (format "GitHub's line %d was deleted here; it was after this line" head))))

(defun approve-feedback--place-of (target)
  "Return where TARGET is in the buffer now, or nil when that isn't known yet.
TARGET is one of:
  (:side SIDE :line LINE)  GitHub's LINE on SIDE, \"RIGHT\" for the
                           pull request's head, \"LEFT\" for its base
  (:thread ID)             the thread ID, where it is drawn
  (:unresolved first)      the first line with an unresolved thread
  (:unresolved last)       the last one
The value is a plist: :line, the buffer's line, 0 above the first;
:overlay, the threads drawn under it, to unfold; :note, what the echo
area says when that isn't the line itself (edited or deleted here,
removed by the pull request), or nil.  GitHub's lines are placed by
the head's map of the buffer's text as it is now, threads where they
are drawn."
  (cond
   ((plist-get target :thread)
    (let ((id (plist-get target :thread)))
      (seq-some (lambda (overlay)
                  (when-let* ((entry (seq-find (lambda (entry)
                                                 (equal (alist-get 'id (car entry)) id))
                                               (overlay-get overlay 'approve-feedback-entries))))
                    (list :line (approve-feedback--line-of overlay) :overlay overlay
                          :note (alist-get (cdr entry) approve-feedback--thread-notes))))
                (approve-feedback--overlays))))
   ((plist-get target :unresolved)
    (when-let* ((overlays (approve-feedback--unresolved-overlays))
                (overlay (if (eq (plist-get target :unresolved) 'last)
                             (car (last overlays))
                           (car overlays))))
      (list :line (approve-feedback--line-of overlay) :overlay overlay)))
   (t
    (when-let* ((map (and approve-feedback--head
                          (approve-lines-watched-map approve-feedback--head)))
                ((approve-lines-current-p map)))
      (pcase-let* ((line (plist-get target :line))
                   (`(,head . ,removed) (approve-feedback--head-line (plist-get target :side)
                                                                    line))
                   (`(,here . ,state) (if (zerop head)
                                          (cons 0 'same)
                                        (approve-lines-to-buffer map head))))
        (list :line here :overlay (approve-feedback--overlay-of-line here)
              :note (approve-feedback--line-note line head removed state)))))))

(defun approve-feedback--settled-p ()
  "Return non-nil if the threads are placed as they will be, or can't be.
That is when the file isn't compared with GitHub's, when a text of
GitHub's could not be had, or when every one is in and compared with
the buffer's text as it is now."
  (or (null approve-feedback--head)
      approve-feedback--error
      (and (null approve-feedback--fetching)
           (seq-every-p (lambda (text)
                          (approve-lines-current-p (approve-lines-watched-map (car text))))
                        approve-feedback--texts))))

(defun approve-feedback--goto-line (line)
  "Move point to the start of LINE, the start of the buffer for 0."
  (goto-char (save-restriction
               (widen)
               (save-excursion
                 (goto-char (point-min))
                 (forward-line (max 0 (1- line)))
                 (point)))))

(defun approve-feedback--move (place)
  "Move point to PLACE, as `approve-feedback--place-of' gives it.
Its threads unfold, and the echo area says its note."
  (when-let* ((overlay (plist-get place :overlay)))
    (approve-feedback--toggle overlay 'show))
  (approve-feedback--goto-line (plist-get place :line))
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-point window (point)))
  (when-let* ((note (plist-get place :note)))
    (message "Approve: %s" note)))

(defun approve-feedback--reach ()
  "Move point where `approve-feedback--destination' says, once that is known.
When point has moved since it was asked, or the place can't be
known, point stays where it is."
  (pcase-let ((`(,target . ,position) approve-feedback--destination))
    (cond
     ((null target))
     ((/= (point) position)
      (setq approve-feedback--destination nil))
     ((not (approve-feedback--loaded-p)))
     ((when-let* ((place (approve-feedback--place-of target)))
        (setq approve-feedback--destination nil)
        (approve-feedback--move place)
        t))
     ((approve-feedback--settled-p)
      (setq approve-feedback--destination nil)
      (cond ((plist-get target :thread)
             (message "Approve: the thread isn't drawn in this buffer"))
            ((plist-get target :unresolved)
             (message "Approve: no unresolved thread is drawn in this buffer")))))))

(defun approve-feedback--rough-line (target)
  "Return the line point goes to until TARGET is placed, or nil to stay.
It is GitHub's line of a line or a thread at the pull request's head:
the buffer's line most of the time."
  (when (approve-feedback--loaded-p)
    (pcase-let ((`(,side ,line)
                 (if-let* ((id (plist-get target :thread)))
                     (and (approve-model-has-entity-p "PullRequestReviewThread" id)
                          (let-alist (approve-model-get "PullRequestReviewThread" id
                                                        '(line diffSide))
                            (list .diffSide .line)))
                   (list (plist-get target :side) (plist-get target :line)))))
      (and line (car (approve-feedback--head-line side line))))))

(defun approve-feedback--go (target)
  "Move point to TARGET, as `approve-feedback--place-of' takes it.
Its threads unfold.  Until the threads are placed, point is on
GitHub's line, and goes on once they are, unless it has moved.  The
clone's text is compared with the buffer's in milliseconds (decision
D45), so that is waited for, a second at most; a text fetched from
GitHub is not."
  (when-let* ((line (approve-feedback--rough-line target)))
    (approve-feedback--goto-line line))
  (setq approve-feedback--destination (cons target (point)))
  (dolist (text approve-feedback--texts)
    (when approve-feedback--destination
      (approve-lines-current-map (car text))))
  (approve-feedback--reach))

(defun approve-feedback--show-in (pr)
  "Show PR's threads in the current buffer, a file's in a clone with its branch.
PR is an alist with the pull request's `id' and `number' at least.
When the branch shows another pull request, PR becomes the one it
shows this session, in every buffer of the branch, as if picked
with \\[approve-clone-pick-pull-request] (decision D45, amended).  Signal a `user-error' when the
branch heads no such pull request."
  (let ((id (alist-get 'id pr)))
    (unless (and approve-feedback-mode
                 (equal id (alist-get 'id approve-feedback--pull-request)))
      (let* ((branch (or (approve-clone-branch-head (file-name-directory buffer-file-name))
                         (user-error "%s is on no branch of a GitHub repository"
                                     (file-name-nondirectory buffer-file-name))))
             (head (plist-get branch :head))
             (find (lambda (prs)
                     (seq-find (lambda (each) (equal (alist-get 'id each) id)) prs)))
             (prs (approve-clone-wait-pull-requests head))
             (prs (if (funcall find prs) prs (approve-clone-wait-pull-requests head t)))
             (row (or (funcall find prs)
                      (user-error "#%s is no pull request of %s"
                                  (alist-get 'number pr) (plist-get head :branch)))))
        (unless (equal id (alist-get 'id (approve-clone-shown-pull-request head prs)))
          (approve-clone-set-pull-request head row))
        (unless approve-feedback-mode
          (let ((approve-feedback--found (list branch row prs)))
            (approve-feedback-mode 1)))))))

(defun approve-feedback--visit (file pr target &optional action)
  "Visit FILE, in a clone, with PR's threads, and move point to TARGET.
PR is a pull request of the branch checked out there, an alist with
its `id' and `number' at least (`approve-feedback--show-in').  TARGET
is as `approve-feedback--place-of' takes it, or nil.  The buffer is
shown by the `display-buffer' ACTION, in the selected window when
nil."
  (let ((buffer (find-file-noselect file)))
    (if action
        (pop-to-buffer buffer action)
      (pop-to-buffer-same-window buffer))
    (with-current-buffer buffer
      (approve-feedback--show-in pr)
      (when target
        (approve-feedback--go target)))))

;;; Between the PR buffer and the clone

(defun approve-feedback--first-change (path)
  "Return the first change the pull request made in PATH, as a target.
That is the first line it added, or changed; where it removed lines,
when it added none in their place; nil when the patch says none."
  (let ((removed nil))
    (catch 'found
      (dolist (hunk (approve-diff-parse (alist-get 'patch (approve-diff-file-entry path))))
        (pcase-dolist (`(,kind ,old ,new ,_) (approve-diff-hunk-lines hunk))
          (pcase kind
            ('add (throw 'found (list :side "RIGHT" :line new)))
            ('del (unless removed
                    (setq removed (list :side "LEFT" :line old))))
            (_ (when removed
                 (throw 'found removed))))))
      removed)))

(defun approve-feedback--thread-target (id)
  "Return the path of the thread ID and the thread as a target, or nil."
  (when (and id (approve-model-has-entity-p "PullRequestReviewThread" id))
    (cons (approve-model-get "PullRequestReviewThread" id 'path) (list :thread id))))

(defun approve-feedback--target-of (context)
  "Return what CONTEXT opens in a clone, (PATH . TARGET), or nil.
A line of the diff or of a file view is that line, a thread or one
of its comments the thread, a file its first change."
  (pcase (plist-get context :kind)
    ((and (or 'line 'lines 'code) (guard (plist-get context :line)))
     (cons (plist-get context :path)
           (list :side (plist-get context :side) :line (plist-get context :line))))
    ('thread (approve-feedback--thread-target (plist-get context :thread-id)))
    ('review-comment
     (approve-feedback--thread-target
      (or (plist-get context :thread-id)
          (approve-nav-thread-of-comment (plist-get context :comment-id)))))
    ('file (cons (plist-get context :path)
                 (approve-feedback--first-change (plist-get context :path))))))

(defun approve-feedback--visit-preflight ()
  "Return what `approve-feedback-visit-file' opens: (PATH . TARGET).
TARGET is as `approve-feedback--place-of' takes it, or nil.  Signal a
`user-error' when point is on no line, thread or file, or before the
pull request has loaded."
  (unless (approve-model-root-ref)
    (user-error (if (or (derived-mode-p 'approve-pr-mode) approve-model-store-buffer)
                    "Wait until the pull request has loaded"
                  "Not in an Approve pull request buffer")))
  (or (seq-some #'approve-feedback--target-of
                (delq nil (list (approve-context-at-point) (approve-context-of-sections))))
      (user-error "Put point on a file, a line of the diff or a thread")))

(put 'approve-feedback-visit-file 'approve-preflight #'approve-feedback--visit-preflight)

(defun approve-feedback--pr-worktree ()
  "Return the worktree that has the pull request's branch checked out, or nil.
It is found among the clones of `approve-clone-directories', asking
which when several are as good (decision D45).  Signal a
`user-error' when the branch is gone."
  (let-alist (approve-pr-fields '(headRefName headRepository))
    (unless .headRepository.nameWithOwner
      (user-error "The pull request's branch is gone"))
    (let ((host (approve-model-meta :host))
          (head (split-string .headRepository.nameWithOwner "/")))
      (approve-clone-pull-request-worktree
       (list :host host :owner (car head) :repo (cadr head) :branch .headRefName)
       (list :host host :owner (approve-model-meta :owner) :repo (approve-model-meta :repo))))))

(defun approve-feedback--file-view-instead (path target why)
  "Say WHY PATH can't open in a clone, and offer the file view at TARGET.
In a file view, only say why."
  (if approve-context-file
      (user-error "%s" why)
    (when (y-or-n-p (format "%s; show the file view instead? " why))
      (pcase-let ((`(,side ,line)
                   (if-let* ((id (plist-get target :thread)))
                       (let-alist (approve-model-get "PullRequestReviewThread" id
                                                     '(line diffSide))
                         (list .diffSide .line))
                     (list (plist-get target :side) (plist-get target :line)))))
        (approve-file-show path (and line side) line)))))

;;;###autoload
(defun approve-feedback-visit-file ()
  "Open the line, thread or file at point in your clone (flow F22).
The file opens in the worktree that has the pull request's branch
checked out, among the clones of `approve-clone-directories',
shown by `approve-feedback-display-action', with the pull request's
threads (`approve-feedback-mode').  Point goes to GitHub's line where
the buffer has it now, its threads unfolded: a line edited there is
in its place, a line the pull request removed is where it was, and
the echo area says so.  A file goes to its first change.  When the
branch shows another of its pull requests, this one is shown from
then on, as \\<approve-feedback-command-map>\\[approve-clone-pick-pull-request] would pick it.  Without a clone, the
file view is offered instead (\\<approve-pr-mode-map>\\[approve-visit-file])."
  (interactive)
  (pcase-let* ((`(,path . ,target) (approve-feedback--visit-preflight))
               (worktree (approve-feedback--pr-worktree))
               (file (and worktree (expand-file-name path worktree))))
    (cond
     ((null worktree)
      (approve-feedback--file-view-instead
       path target (format "No clone has %s checked out" (approve-pr-field 'headRefName))))
     ((not (file-exists-p file))
      (approve-feedback--file-view-instead
       path target (format "%s isn't in %s (deleted or moved there)"
                           path (abbreviate-file-name worktree))))
     (t
      (approve-feedback--visit file (approve-pr-fields '(id number)) target
                               approve-feedback-display-action)))))

(defun approve-feedback--visit-pr-preflight ()
  "Return the context `approve-feedback-visit-pr' goes to in the PR buffer.
Signal a `user-error' when the mode is off here, or the pull request
hasn't loaded."
  (approve-feedback--check)
  (unless (approve-feedback--loaded-p)
    (user-error "Wait until the pull request has loaded"))
  (approve-context-at-point))

(put 'approve-feedback-visit-pr 'approve-preflight #'approve-feedback--visit-pr-preflight)

(defun approve-feedback-visit-pr ()
  "Show the pull request's buffer at the line at point (flow F22).
Where the line, or the region's lines, are GitHub's and in the pull
request's diff, at that line in the diff, its threads unfolded; else
at the first of the threads drawn under the line; else at the file's
diff.  The PR buffer is shown where it is already, else in this
window; \\<approve-pr-mode-map>\\[approve-feedback-visit-file] there comes back."
  (interactive)
  (let ((context (approve-feedback--visit-pr-preflight)))
    (if-let* (((not (memq (plist-get context :kind) '(line lines))))
              (thread (car (plist-get context :threads))))
        (progn
          (pop-to-buffer (approve-model-owner-buffer)
                         '(display-buffer-reuse-window display-buffer-same-window))
          (approve-nav-goto-thread thread))
      (approve-nav-goto-diff-line context))))

(keymap-set approve-pr-mode-map "f" #'approve-feedback-visit-file)
(keymap-set approve-file-mode-map "f" #'approve-feedback-visit-file)

;;; The global mode

(defvar approve-feedback-global-mode)

(defun approve-feedback--turn-on-with (branch prs)
  "Turn the feedback mode on here, with the pull request of BRANCH among PRS.
That is the one the branch shows, open or picked, if any; nothing
when the global mode was turned off meanwhile, or the mode on."
  (when-let* (((and approve-feedback-global-mode buffer-file-name
                    (not approve-feedback-mode)))
              (pr (approve-clone-shown-pull-request (plist-get branch :head) prs)))
    (condition-case-unless-debug err
        (let ((approve-feedback--found (list branch pr prs)))
          (approve-feedback-mode 1))
      (error (approve-log-condition "Turning on the feedback mode" err)))))

(defun approve-feedback--turn-on ()
  "Turn the feedback mode on soon, if the branch heads an open pull request.
It is how `approve-feedback-global-mode' turns it on, in each buffer
whose major mode is set.  A file outside a clone of GitHub costs
nothing, and one in a clone and branch seen this session no git
call.  The branch's pull requests are asked of GitHub once a session,
without waiting: the mode turns on when they are in (decision D45).
It never asks: a closed or merged pull request shows only once picked
with \\[approve-clone-pick-pull-request], and the files of git's own directory, such as a commit
message, never."
  (when (and buffer-file-name
             (not approve-feedback-mode)
             (not (file-remote-p buffer-file-name))
             (not (string-match-p "/\\.git/" buffer-file-name)))
    (condition-case-unless-debug err
        (when-let* ((branch (approve-clone-known-branch-head
                             (file-name-directory buffer-file-name))))
          (approve-clone-branch-pull-requests
           (plist-get branch :head)
           :callback (lambda (prs) (approve-feedback--turn-on-with branch prs))
           :errorback (lambda (err)
                        (approve-log 'warn "The pull requests of %s: %s"
                                     (plist-get branch :branch)
                                     (approve-api-error-message err)))))
      (error (approve-log-condition "Turning on the feedback mode" err)))))

;;;###autoload
(define-globalized-minor-mode approve-feedback-global-mode approve-feedback-mode
  approve-feedback--turn-on
  :group 'approve)

;;; The dispatch

(defun approve-feedback--dispatch-heading ()
  "Return the heading of the dispatch: the pull request and where the clone stands.
Such as \"#1234 Add retry logic to the GraphQL client · 1/2 · ↑1\"."
  (let* ((fields (if (approve-feedback--loaded-p)
                     (approve-pr-fields '(number title))
                   approve-feedback--pull-request))
         (open approve-feedback--open)
         (index (seq-position open approve-feedback--pull-request
                              (lambda (a b) (equal (alist-get 'id a) (alist-get 'id b))))))
    (let-alist fields
      (concat (propertize (format "#%s" .number) 'face 'transient-heading) " "
              (propertize (truncate-string-to-width (or .title "") 60 nil nil "…")
                          'face 'approve-title)
              (propertize (concat
                           (and index (cdr open) (format " · %d/%d" (1+ index) (length open)))
                           (when-let* ((arrows (approve-feedback--clone-arrows
                                                approve-feedback--clone-state)))
                             (concat " · " arrows)))
                          'face 'approve-dim)))))

(defun approve-feedback--toggle-preflight ()
  "Return the overlay of the threads `approve-feedback-toggle-threads' folds.
Signal a `user-error' when the line has none."
  (approve-feedback--check)
  (or (approve-feedback--overlay-at (point))
      (user-error "No threads on this line")))

(put 'approve-feedback-toggle-threads 'approve-preflight #'approve-feedback--toggle-preflight)

(defun approve-feedback--toggle-all-preflight ()
  "Return the overlays `approve-feedback-toggle-all-threads' folds.
Signal a `user-error' when the file has no threads."
  (approve-feedback--check)
  (or (approve-feedback--overlays)
      (user-error "No threads in this file")))

(put 'approve-feedback-toggle-all-threads 'approve-preflight
     #'approve-feedback--toggle-all-preflight)

;;;###autoload (autoload 'approve-feedback-dispatch "approve-feedback" nil t)
(transient-define-prefix approve-feedback-dispatch ()
  "Show the commands of the feedback mode (decisions D06 and D46).
Those that can't run at point are dimmed.  The commands of threads
act on those of the line at point, and ask which when there are
several.  Every one is in `approve-feedback-command-map', with the
same key."
  [:description approve-feedback--dispatch-heading
   ["Threads on this line"
    ("TAB" "Fold or unfold" approve-feedback-toggle-threads
     :inapt-if (lambda () (approve-action-why-not 'approve-feedback-toggle-threads)))
    ("T" "Fold or unfold all" approve-feedback-toggle-all-threads
     :inapt-if (lambda () (approve-action-why-not 'approve-feedback-toggle-all-threads)))
    ("n" "Next unresolved" approve-feedback-next-thread)
    ("p" "Previous unresolved" approve-feedback-previous-thread)
    ("r" "Reply" approve-reply
     :inapt-if (lambda () (approve-action-why-not 'approve-reply)))
    ("R" "Resolve / unresolve" approve-resolve
     :inapt-if (lambda () (approve-action-why-not 'approve-resolve)))
    ("e" "Edit" approve-edit
     :inapt-if (lambda () (approve-action-why-not 'approve-edit)))
    ("k" "Delete" approve-delete
     :inapt-if (lambda () (approve-action-why-not 'approve-delete)))
    ("+" "React" approve-react
     :inapt-if (lambda () (approve-action-why-not 'approve-react)))]
   ["Comment"
    ("c" "Comment" approve-comment
     :inapt-if (lambda () (approve-action-why-not 'approve-comment)))
    ("s" "Suggest a change" approve-suggest
     :inapt-if (lambda () (approve-action-why-not 'approve-suggest)))
    ("C" "On the pull request" approve-comment-on-pr
     :inapt-if (lambda () (approve-action-why-not 'approve-comment-on-pr)))]
   ["Pull request"
    ("RET" "The PR buffer, at this line" approve-feedback-visit-pr
     :inapt-if (lambda () (approve-action-why-not 'approve-feedback-visit-pr)))
    ("P" "Pick another pull request" approve-clone-pick-pull-request)
    ("g" "Refresh" approve-feedback-refresh)
    ("o" "Open in browser" approve-browse)
    ("S" "Submit review…" approve-submit-review
     :inapt-if (lambda () (approve-action-why-not 'approve-submit-review)))
    ("X" "Discard pending review" approve-discard-review
     :inapt-if (lambda () (approve-action-why-not 'approve-discard-review)))]]
  (interactive)
  (approve-feedback--check)
  (transient-setup 'approve-feedback-dispatch))

(defun approve-feedback-mouse-dispatch (event)
  "Open the dispatch of the buffer whose mode-line item was clicked, EVENT."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (call-interactively #'approve-feedback-dispatch))

(defun approve-feedback--visit-file-inapt-p ()
  "Return non-nil if `approve-feedback-visit-file' can't run at point."
  (approve-action-why-not 'approve-feedback-visit-file))

;; The dispatches of the PR buffer and the file view list f, as their
;; buffers bind it (decisions D06 and D46).
(transient-append-suffix 'approve-dispatch "O"
  '("f" "Your file (clone)" approve-feedback-visit-file
    :inapt-if approve-feedback--visit-file-inapt-p))
(transient-append-suffix 'approve-file-dispatch "y"
  '("f" "Your file (clone)" approve-feedback-visit-file
    :inapt-if approve-feedback--visit-file-inapt-p))

(defun approve-feedback-unload-function ()
  "Stop following the focus, as `unload-feature' unloads this file.
The mode-line item leaves `global-mode-string' too."
  (remove-function after-focus-change-function #'approve-feedback--on-focus)
  (when (listp global-mode-string)
    (setq global-mode-string (delete approve-feedback--mode-line-construct
                                     global-mode-string)))
  nil)

(provide 'approve-feedback)

;;; approve-feedback.el ends here
