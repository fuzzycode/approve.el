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
(require 'eieio)

(require 'approve-model)
(require 'approve-input)
(require 'approve-api-mutations)

;; Forward declarations to avoid circular dependencies
(declare-function approve-ui-redraw "approve-ui")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-value "magit-section")

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

(defun approve-action-browse-author ()
  "Open the PR author's GitHub profile in the default web browser."
  (interactive)
  (approve-with-pr-buffer
    (let ((url (magit-section-value (magit-current-section))))
      (unless url
        (user-error "No URL available for this author"))
      (browse-url url)
      (message "Opened author profile in browser"))))

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

;;; File Actions

(defun approve-actions--get-file-at-point ()
  "Return the file path at point, or nil if not on a file section."
  (when-let ((section (magit-current-section)))
    (and (eq (oref section type) 'file)
         (oref section value))))

(defun approve-actions--get-file-viewed-state (path)
  "Return the viewed state for file at PATH.
Returns one of \"VIEWED\", \"UNVIEWED\", or \"DISMISSED\", or nil if not found."
  (when-let ((files-data (approve-model-root 'files)))
    (let ((files (approve-model-get-nodes files-data)))
      (cl-loop for file in files
               when (string= (alist-get 'path file) path)
               return (alist-get 'viewerViewedState file)))))

(defun approve-action-toggle-file-viewed ()
  "Toggle the viewed state of the file at point.

The state transitions are:
  VIEWED    -> UNVIEWED
  UNVIEWED  -> VIEWED
  DISMISSED -> VIEWED"
  (interactive)
  (approve-with-pr-buffer
    (let ((path (approve-actions--get-file-at-point)))
      (unless path
        (user-error "No file at point"))
      (let* ((current-state (approve-actions--get-file-viewed-state path))
             (pr-id (approve-model-root 'id))
             (buffer (current-buffer))
             ;; Determine target state and mutation
             ;; VIEWED -> UNVIEWED (use unmark)
             ;; UNVIEWED/DISMISSED/nil -> VIEWED (use mark)
             (should-mark (not (string= current-state "VIEWED"))))
        (if should-mark
            (approve-api-mutation-mark-file-as-viewed
             pr-id path
             :on-success
             (lambda (data)
               (with-current-buffer buffer
                 (when-let ((pr-data (alist-get 'pullRequest
                                                (alist-get 'markFileAsViewed data))))
                   (approve-model-patch pr-data))
                 (approve-actions--redraw-preserving-point))
               (message "Marked %s as viewed" path))
             :on-error
             (lambda (err)
               (message "Failed to mark file as viewed: %s" err)))
          (approve-api-mutation-unmark-file-as-viewed
           pr-id path
           :on-success
           (lambda (data)
             (with-current-buffer buffer
               (when-let ((pr-data (alist-get 'pullRequest
                                              (alist-get 'unmarkFileAsViewed data))))
                 (approve-model-patch pr-data))
               (approve-actions--redraw-preserving-point))
             (message "Marked %s as unviewed" path))
           :on-error
           (lambda (err)
             (message "Failed to mark file as unviewed: %s" err))))))))

(provide 'approve-actions)
;;; approve-actions.el ends here
