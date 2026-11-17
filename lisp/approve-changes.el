;;; approve-changes.el --- Announce changes on GitHub, never apply them -*- lexical-binding: t; -*-

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

;; Change detection (decision D23; docs/flows.org, F17).  A PR buffer
;; never redraws by itself: content must not move under the reader's
;; cursor.  Instead a small query asks GitHub what tells whether the
;; pull request changed (`approve-query-changes'), and the header line
;; says what differs from the store:
;;
;;   #202 Rewrite the parser · ⟳ updated on GitHub — g to refresh · 2 new commits
;;
;; g loads the changes; the notice goes as the load starts.
;;
;; - `approve-changes-compare' compares an answer with the store: the
;;   head and the commit count; the title, state, draft state and the
;;   description's last edit; the newest conversation comments and
;;   reviews, by id and last edit.  The writes made from Approve are
;;   in the store already (decision D35), so they never count.
;; - `approve-changes-check' asks only when the buffer is ready: every
;;   query of the plan loaded without an error, nothing loading, no
;;   write in flight.  An answer that arrives after the store changed
;;   is dropped, as it may predate what the store holds.
;; - A PR buffer is checked when its window is selected or its frame
;;   gets the focus, at most once per `approve-focus-check-interval',
;;   and every `approve-poll-interval' seconds while a window shows it.
;;   A buffer that shares its store (a file view, a buffer of the clone)
;;   counts as showing it, and selecting such a buffer checks it too
;;   once the buffer called `approve-changes-follow-selection'.
;; - `approve-changes-hook' runs when a check answered, for the buffers
;;   that share the store to say it as the header line does.
;;
;; Layer rules: this is the buffers layer, beside approve-pr.el.  It
;; asks GitHub through approve-queries, reads the store through
;; selectors, and changes nothing but the header line.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'approve-api)
(require 'approve-log)
(require 'approve-model)
(require 'approve-session)
(require 'approve-queries)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-pr)

;;; Options

(defcustom approve-poll-interval 300
  "Seconds between checks for changes on GitHub, or nil for none.
The pull requests that a window shows are checked that often, and
the header line says when one changed; the buffer is never redrawn
by itself.  Selecting a PR buffer checks it too (see
`approve-focus-check-interval').  To check only then:

  (setq approve-poll-interval nil)"
  :type '(choice (const :tag "Only when a PR buffer is selected" nil)
                 (number :tag "Seconds"))
  :group 'approve)

(defcustom approve-focus-check-interval 60
  "Least seconds between two checks of a PR when its buffer is selected.
Selecting the window of a PR buffer, or giving its frame the focus,
checks it for changes on GitHub, unless it was checked or loaded
less than that long ago.  Nil turns these checks off:

  (setq approve-focus-check-interval nil)"
  :type '(choice (const :tag "Never on selection" nil)
                 (number :tag "Seconds"))
  :group 'approve)

;;; Comparing with the store

(defun approve-changes--newer-p (remote local)
  "Return non-nil if REMOTE is a later time than LOCAL.
Both are times as GitHub writes them (\"2026-11-15T08:03:16Z\") or
nil; nil is never later, and anything is later than nil."
  (and remote (or (null local) (string> remote local))))

(defun approve-changes--new-nodes (nodes)
  "Return how many of NODES the store doesn't hold.
NODES are objects with `__typename' and `id'."
  (seq-count (lambda (node)
               (not (approve-model-has-entity-p (alist-get '__typename node)
                                                (alist-get 'id node))))
             nodes))

(defun approve-changes--edited-p (nodes)
  "Return non-nil if one of NODES was edited after the store's copy.
NODES are objects with `__typename', `id' and `lastEditedAt'; those
the store doesn't hold don't count."
  (seq-some (lambda (node)
              (let ((typename (alist-get '__typename node))
                    (id (alist-get 'id node)))
                (and (approve-model-has-entity-p typename id)
                     (approve-changes--newer-p
                      (alist-get 'lastEditedAt node)
                      (approve-model-get typename id 'lastEditedAt)))))
            nodes))

(defun approve-changes-compare (remote)
  "Return how REMOTE, the PR as GitHub has it now, differs from the store.
REMOTE is the pullRequest of `approve-query-changes'.  The value is
nil when nothing differs, else a plist:
  :branch    non-nil when the head commit moved
  :commits   how many commits more GitHub has
  :comments  how many of the newest conversation comments the store lacks
  :reviews   how many of the newest reviews the store lacks
  :edited    non-nil when the title, state, draft state or description
             changed, or one of the newest comments or reviews was edited
The writes made from Approve are in the store (decision D35), so
they never count."
  (let-alist remote
    (let* ((local (approve-pr-fields '(headRefOid title state isDraft lastEditedAt
                                                  commitCount)))
           (branch (not (equal .headRefOid (alist-get 'headRefOid local))))
           (commits (max 0 (- (or .commitCount.totalCount 0)
                              (or (alist-get 'totalCount (alist-get 'commitCount local))
                                  0))))
           (comments (approve-changes--new-nodes .comments.nodes))
           (reviews (approve-changes--new-nodes .reviews.nodes))
           (edited (or (not (equal .title (alist-get 'title local)))
                       (not (equal .state (alist-get 'state local)))
                       (not (eq (and .isDraft t) (and (alist-get 'isDraft local) t)))
                       (approve-changes--newer-p .lastEditedAt
                                                 (alist-get 'lastEditedAt local))
                       (approve-changes--edited-p (append .comments.nodes
                                                          .reviews.nodes)))))
      (when (or branch (> commits 0) (> comments 0) (> reviews 0) edited)
        (list :branch branch :commits commits :comments comments
              :reviews reviews :edited (and edited t))))))

;;; The header line

(defun approve-changes--count (n one many)
  "Return N with ONE or MANY after it, as N says."
  (format "%d %s" n (if (= n 1) one many)))

(defun approve-changes-notice ()
  "Return the header line's notice of changes on GitHub, or nil.
Such as \"⟳ updated on GitHub — g to refresh · 2 new commits\", with
the key that refreshes here.  It is one of
`approve-pr-header-line-functions'."
  (when-let* ((changes (approve-session-get :changes)))
    (let* ((commits (plist-get changes :commits))
           (comments (plist-get changes :comments))
           (reviews (plist-get changes :reviews))
           (details (delq nil (list (cond ((> commits 0)
                                           (approve-changes--count commits "new commit"
                                                                   "new commits"))
                                          ((plist-get changes :branch) "the branch changed"))
                                    (and (> comments 0)
                                         (approve-changes--count comments "new comment"
                                                                 "new comments"))
                                    (and (> reviews 0)
                                         (approve-changes--count reviews "new review"
                                                                 "new reviews")))))
           (key (or (approve-render-key 'approve-pr-refresh)
                    (substitute-command-keys "\\[approve-pr-refresh]"))))
      (concat (approve-icon 'loading 'approve-updated) " "
              (approve-face "updated on GitHub — " 'approve-updated)
              (approve-face key 'approve-hint-key)
              (approve-face (mapconcat #'identity (cons " to refresh" details) " · ")
                            'approve-updated)))))

;;; Checking

(defun approve-changes--ready-p ()
  "Return non-nil if the current PR buffer can be checked now.
Every query of the plan has loaded without an error, nothing is
loading, no write is in flight and no check is out."
  (and (approve-model-root-ref)
       (not (approve-session-get :fatal))
       (not (approve-pr-loading-p))
       (not (approve-session-get :inflight))
       (not (approve-session-get :checking))
       (seq-every-p (lambda (entry)
                      (and (approve-pr-loaded-p (car entry))
                           (not (approve-pr-error (car entry)))))
                    approve-query-plan)))

(defvar approve-changes-hook nil
  "Hook run in a PR buffer when a check for changes on GitHub answered.
The session's :changes is then what differs from the store, or nil.
The header line says it already; buffers that share the store, such
as the clone's (approve-feedback.el), say it too from here.")

(defun approve-changes--take (remote version)
  "Say in the header line how REMOTE differs from the store.
VERSION is the store's version when the check was sent.  When the
store changed since, or a load or a write is under way, REMOTE may
predate it: then it is dropped, and a later check tells.  Then
`approve-changes-hook' runs."
  (if (and (= version (approve-model-version))
           (not (approve-pr-loading-p))
           (not (approve-session-get :inflight)))
      (let ((changes (approve-changes-compare remote)))
        (approve-log 'debug "Changes on GitHub of %s: %S" (buffer-name) changes)
        (approve-session-put :changes changes)
        (approve-pr-update-header-line)
        (run-hooks 'approve-changes-hook))
    (approve-log 'debug "Change check of %s dropped: the store changed meanwhile"
                 (buffer-name))))

(defun approve-changes-check ()
  "Check the PR of the current buffer for changes on GitHub (decision D23).
Nothing happens unless the buffer is a PR buffer that is ready
\(`approve-changes--ready-p'); return non-nil when the check was
sent.  The header line then says what differs from the store, or
nothing.  The buffer is not redrawn: \\<approve-pr-mode-map>\\[approve-pr-refresh] loads the changes."
  (when (and (derived-mode-p 'approve-pr-mode) (approve-changes--ready-p))
    (let ((version (approve-model-version)))
      (approve-session-put :checking t)
      (approve-session-put :checked (float-time))
      (condition-case-unless-debug err
          (approve-query-changes
           :callback (lambda (remote)
                       (approve-session-put :checking nil)
                       (approve-changes--take remote version))
           :errorback (lambda (err)
                        (approve-session-put :checking nil)
                        (approve-log 'debug "Change check of %s failed: %s" (buffer-name)
                                     (approve-api-error-message err))))
        (error
         (approve-session-put :checking nil)
         (approve-log-condition "Checking for changes on GitHub" err)))
      t)))

;;; When to check

(defun approve-changes--pr-buffer-p (buffer)
  "Return non-nil if BUFFER is a live PR buffer."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                'approve-pr-mode)))

(defun approve-changes--pr-buffer-of (buffer)
  "Return the PR buffer BUFFER shows, or nil.
That is BUFFER itself, or the PR buffer whose store it shares (a file
view, a buffer of the clone: `approve-model-store-buffer')."
  (cond ((approve-changes--pr-buffer-p buffer) buffer)
        ((buffer-live-p buffer)
         (let ((owner (buffer-local-value 'approve-model-store-buffer buffer)))
           (and (approve-changes--pr-buffer-p owner) owner)))))

(defun approve-changes--shown-p (buffer)
  "Return non-nil if a window shows the PR buffer BUFFER, or one sharing its store."
  (or (get-buffer-window buffer 'visible)
      (seq-some (lambda (other)
                  (and (eq (buffer-local-value 'approve-model-store-buffer other) buffer)
                       (get-buffer-window other 'visible)))
                (buffer-list))))

(defun approve-changes--due-p ()
  "Return non-nil if selecting the current PR buffer should check it."
  (and approve-focus-check-interval
       (let ((checked (approve-session-get :checked)))
         (or (null checked)
             (>= (- (float-time) checked) approve-focus-check-interval)))))

(defun approve-changes--selected (buffer)
  "Check the PR BUFFER shows if the selected window shows it and a check is due.
BUFFER is a PR buffer or one that shares its store."
  (when-let* ((pr (approve-changes--pr-buffer-of buffer))
              ((eq (window-buffer (selected-window)) buffer)))
    (with-current-buffer pr
      (when (approve-changes--due-p)
        (approve-changes-check)))))

(defun approve-changes--on-window (window)
  "Check the current buffer's PR soon if WINDOW, which shows it, is selected.
It is on the buffer's `window-selection-change-functions' and
`window-buffer-change-functions'.  Those run during redisplay, so
the check waits for a timer."
  (when (and (eq window (selected-window))
             (eq (window-buffer window) (current-buffer)))
    (run-at-time 0 nil #'approve-changes--selected (current-buffer))))

(defun approve-changes--on-focus ()
  "Check the selected window's PR soon, when Emacs has the focus.
It is on `after-focus-change-function'."
  (when (frame-focus-state)
    (run-at-time 0 nil #'approve-changes--selected (window-buffer (selected-window)))))

(defun approve-changes-follow-selection (&optional stop)
  "Check the current buffer's PR for changes when its window is selected.
The buffer shares a PR buffer's store, such as a buffer of the clone
\(approve-feedback.el): selecting it checks the PR as selecting the
PR buffer does, and a window showing it keeps the PR polled.  With
STOP, it no longer does."
  (if stop
      (progn
        (remove-hook 'window-selection-change-functions #'approve-changes--on-window t)
        (remove-hook 'window-buffer-change-functions #'approve-changes--on-window t))
    (add-hook 'window-selection-change-functions #'approve-changes--on-window nil t)
    (add-hook 'window-buffer-change-functions #'approve-changes--on-window nil t)))

(defvar approve-changes--timer nil
  "The timer of the next poll, or nil.")

(defun approve-changes--schedule-poll ()
  "Start the timer of the next poll, unless polling is off or it runs."
  (when (and approve-poll-interval (not (timerp approve-changes--timer)))
    (setq approve-changes--timer
          (run-at-time approve-poll-interval nil #'approve-changes--poll))))

(defun approve-changes--poll ()
  "Check the PR buffers a window shows, then wait for the next poll.
A PR buffer is shown too when a window shows a buffer sharing its
store.  Polling stops when no PR buffer is left; the next one starts
it."
  (setq approve-changes--timer nil)
  (let ((buffers (seq-filter #'approve-changes--pr-buffer-p (buffer-list))))
    (dolist (buffer buffers)
      (when (approve-changes--shown-p buffer)
        (with-current-buffer buffer
          (approve-changes-check))))
    (when buffers
      (approve-changes--schedule-poll))))

;;; Setup

(defun approve-changes--forget ()
  "Forget the changes found, as a load starts that brings them.
It is on `approve-pr-load-hook'.  Selecting the buffer doesn't check
it again for `approve-focus-check-interval' seconds."
  (approve-session-put :changes nil)
  (approve-session-put :checked (float-time)))

(defun approve-changes--setup ()
  "Check the current PR buffer for changes when it is selected, and poll.
It is on `approve-pr-mode-hook'."
  (add-hook 'window-selection-change-functions #'approve-changes--on-window nil t)
  (add-hook 'window-buffer-change-functions #'approve-changes--on-window nil t)
  (unless (advice-function-member-p #'approve-changes--on-focus
                                    after-focus-change-function)
    (add-function :after after-focus-change-function #'approve-changes--on-focus))
  (approve-changes--schedule-poll))

(add-hook 'approve-pr-mode-hook #'approve-changes--setup)
(add-hook 'approve-pr-load-hook #'approve-changes--forget)
(add-hook 'approve-pr-header-line-functions #'approve-changes-notice)

(defun approve-changes-unload-function ()
  "Stop checking for changes, as `unload-feature' unloads this file."
  (remove-function after-focus-change-function #'approve-changes--on-focus)
  (when (timerp approve-changes--timer)
    (cancel-timer approve-changes--timer))
  nil)

(provide 'approve-changes)

;;; approve-changes.el ends here
