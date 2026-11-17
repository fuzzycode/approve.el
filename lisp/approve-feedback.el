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
;; - The lighter says the pull request's number, the file's unresolved
;;   threads, which of the branch's open pull requests it is, how the
;;   clone stands to GitHub's head, and what is loading or changed; its
;;   tooltip says it in words (D46).
;; - `approve-feedback-command-map' holds the mode's commands, for the
;;   user to bind: the mode binds no key of its own (D46).
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
(require 'approve-model)
(require 'approve-session)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-pr)
(require 'approve-clone)

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

;;; Mode

(defvar-keymap approve-feedback-command-map
  :doc "The commands of `approve-feedback-mode', for a key of your choice.
The mode binds no key in your buffers (decision D46); bind this map
to one, under a leader key for example:

  (keymap-set approve-feedback-mode-map \"C-c a\" approve-feedback-command-map)"
  "g" #'approve-feedback-refresh
  "P" #'approve-clone-pick-pull-request)

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
        approve-model-store-buffer (approve-feedback--store-buffer pr))
  (approve-feedback-redraw))

(defun approve-feedback--stop ()
  "Take what the mode added away from the current buffer."
  (dolist (variable '(approve-feedback--branch approve-feedback--pull-request
                      approve-feedback--open approve-feedback--path
                      approve-feedback--clone-state approve-feedback--lighter
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

;;; Drawing

(defun approve-feedback--follow-head ()
  "Compare the clone with GitHub's head again when the head moved."
  (let ((oid (approve-pr-field 'headRefOid)))
    (unless (equal oid (plist-get approve-feedback--clone-state :oid))
      (setq approve-feedback--clone-state
            (append (list :oid oid)
                    (approve-clone-state (plist-get approve-feedback--branch :clone) oid))))))

(defun approve-feedback-redraw ()
  "Draw what the current buffer shows of its pull request again, from the store.
Every redraw of the PR buffer calls it in the buffers of its clone."
  (when approve-feedback-mode
    (when (approve-feedback--loaded-p)
      (approve-feedback--follow-head))
    (approve-feedback--update-lighter)))

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
         (loading (or (not loaded) (approve-pr-loading-p)))
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

(add-hook 'approve-clone-picked-functions #'approve-feedback--picked)

(provide 'approve-feedback)

;;; approve-feedback.el ends here
