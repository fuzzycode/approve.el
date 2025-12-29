;;; approve-ui.el --- UI rendering for Approve  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This module provides the UI rendering layer for Approve.
;;
;; Key responsibilities:
;; - Creating and managing the Approve buffer
;; - Rendering PR data using magit-section
;; - Coordinating between the data model and visual representation
;; - Handling buffer refresh and updates
;;
;; This is the entry point for all visual rendering of pull request
;; review data.

;;; Code:

(require 'magit-section)
(require 'cl-lib)

(require 'approve-model)
(require 'approve-api-queries)
(require 'approve-ui-headers)
(require 'approve-ui-faces)
(require 'approve-eldoc)

;;; Customization

(defcustom approve-review-sections-hook
  '(approve-insert-header-section)
  "Hook run to insert sections in the PR review buffer.
Each function is called with no arguments and should use the
PR data stored in buffer-local variables."
  :group 'approve
  :type 'hook)

;;; Major mode

(defvar approve-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'approve-ui-refresh)
    map)
  "Keymap for `approve-review-mode'.")

(define-derived-mode approve-review-mode magit-section-mode "Approve Review"
  "Major mode for GitHub PR reviews with Approve."
  :group 'approve
  :keymap approve-review-mode-map

  (setq-local revert-buffer-function #'approve-ui-refresh
              line-spacing 0.2       ; Give a bit more vertical space
              word-wrap t            ; Wrap long lines
              show-trailing-whitespace nil)

  (setq buffer-read-only t)

  ;; Enable eldoc for hover documentation
  (approve-eldoc-setup)

  (run-mode-hooks 'approve-review-mode-hook))

;;; Private functions

(defun approve-ui--handle-fetch-success (data)
  "Handle successful PR fetch with DATA.
Loads the data into the model and redraws the buffer."
  (when-let ((repository (alist-get 'repository data)))
    ;; Load repository-level data (it won't have __typename/id, so just store in metadata)
    (approve-model-set-metadata :repository-url (alist-get 'url repository))
    (approve-model-set-metadata :viewer-permission (alist-get 'viewerPermission repository))
    ;; Store available labels, users, milestones for later use in completion
    (approve-model-set-metadata :available-labels (alist-get 'labels repository))
    (approve-model-set-metadata :assignable-users (alist-get 'assignableUsers repository))
    (approve-model-set-metadata :mentionable-users (alist-get 'mentionableUsers repository))
    (approve-model-set-metadata :milestones (alist-get 'milestones repository))
    ;; Load the pull request as the root entity
    (when-let ((pr (alist-get 'pullRequest repository)))
      (approve-model-load pr t))

    (switch-to-buffer (current-buffer))
    (approve-ui-redraw)))

(defun approve-ui--buffer-name (owner repo number)
  "Generate a buffer name for PR NUMBER in OWNER/REPO."
  (format "*Approve: %s/%s#%d*" owner repo number))

(defun approve-ui--get-or-create-buffer (owner repo number)
  "Get or create the Approve buffer for PR NUMBER in OWNER/REPO.
Returns the buffer."
  (let ((buffer-name (approve-ui--buffer-name owner repo number)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (approve-review-mode)
            (approve-model-init `(:owner ,owner :repo ,repo :number ,number)))
          buffer))))

(defun approve-ui-redraw ()
  "Redraw the current Approve buffer using the data model.
This clears the buffer and runs `approve-review-sections-hook' to
insert all sections.  Does not fetch new data from GitHub."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (approve-root)
      (run-hooks 'approve-review-sections-hook))
    (goto-char (point-min))))

;;; Public API

(defun approve-ui-refresh (&rest _args)
  "Reload the current review buffer.
Fetches fresh data from GitHub using the owner, repo, and number
stored in the buffer's metadata, then redraws the buffer.

This function is suitable for use as `revert-buffer-function'."
  (interactive)
  (approve-with-pr-buffer
    (let ((owner (approve-model-metadata :owner))
          (repo (approve-model-metadata :repo))
          (number (approve-model-metadata :number)))
      (unless (and owner repo number)
        (user-error "Buffer missing PR metadata"))
      (approve-api-query-pull-request
       owner repo number
       :callback #'approve-ui--handle-fetch-success))))

(defun approve-ui-view-pr (owner repo number)
  "Display and fetch PR NUMBER from OWNER/REPO.
Creates or switches to the Approve buffer for this PR,
initializes the data model with the PR metadata, and
fetches the PR data from GitHub."
  (let ((buffer (approve-ui--get-or-create-buffer owner repo number)))
    (with-current-buffer buffer
      (approve-ui-refresh))))

(provide 'approve-ui)
;;; approve-ui.el ends here
