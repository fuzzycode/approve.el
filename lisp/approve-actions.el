;;; approve-actions.el --- User-interactive commands for Approve  -*- lexical-binding: t; -*-

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

;; This module provides user-interactive commands for Approve.
;;
;; These are the actions users invoke from sections, such as:
;; - Opening a PR in the browser
;; - Editing the PR title
;; - Adding comments
;; - Managing labels, assignees, etc.
;;
;; Each action checks permissions before attempting mutations and
;; provides user feedback via the echo area.

;;; Code:

(require 'browse-url)
(require 'cl-lib)

(require 'approve-model)
(require 'approve-input)
(require 'approve-api-mutations)

;; Forward declarations to avoid circular dependencies
(declare-function approve-ui-redraw "approve-ui")

;;; Helpers

(defun approve-actions--check-permission (permission-field action-name)
  "Check if the user has PERMISSION-FIELD for ACTION-NAME.
PERMISSION-FIELD is a symbol like `viewerCanUpdate'.
ACTION-NAME is a string describing the action for error messages.
Returns non-nil if permitted, otherwise displays a message and returns nil."
  (let ((can-do (approve-model-root permission-field)))
    (unless can-do
      (message "Cannot %s: you don't have permission" action-name))
    can-do))

(defun approve-actions--redraw-preserving-point ()
  "Redraw the buffer while preserving point position.
After redraw, recenters the window around point."
  (let ((line (line-number-at-pos))
        (col (current-column)))
    (approve-ui-redraw)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)
    (recenter)))

;;; Public Commands

(defun approve-action-browse-pr ()
  "Open the current pull request in the default web browser."
  (interactive)
  (approve-with-pr-buffer
    (let ((url (approve-model-root 'url)))
      (unless url
        (user-error "No URL available for this pull request"))
      (browse-url url)
      (message "Opened PR in browser"))))

(defun approve-action-edit-title ()
  "Edit the title of the current pull request."
  (interactive)
  (approve-with-pr-buffer
    (when (approve-actions--check-permission 'viewerCanUpdate "edit title")
      (let ((current-title (approve-model-root 'title))
            (pr-id (approve-model-root 'id))
            (buffer (current-buffer)))
        (approve-input-read
         :prompt "Edit PR Title"
         :initial current-title
         :multiline nil
         :on-commit
         (lambda (new-title)
           (if (string= new-title current-title)
               (message "Title unchanged")
             (with-current-buffer buffer
               (approve-api-mutation-update-pr-title
                pr-id new-title
                :on-success
                (lambda (data)
                  (with-current-buffer buffer
                    ;; Patch the model with the returned PR data
                    (when-let ((pr-data (alist-get 'pullRequest
                                                   (alist-get 'updatePullRequest data))))
                      (approve-model-patch pr-data))
                    (approve-actions--redraw-preserving-point))
                  (message "Title updated"))
                :on-error
                (lambda (err)
                  (message "Failed to update title: %s" err))))))
         :on-abort
         (lambda ()
           (message "Title edit cancelled")))))))

(provide 'approve-actions)
;;; approve-actions.el ends here
