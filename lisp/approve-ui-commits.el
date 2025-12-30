;;; approve-ui-commits.el --- Commit UI components for Approve  -*- lexical-binding: t; -*-

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

;; This file provides UI components for displaying commit information
;; in Approve pull request buffers.

;;; Code:

(require 'browse-url)
(require 'magit-section)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)

;;; Customization

(defcustom approve-review-commit-sections
  '(approve-review-commit-insert-sha-section
    approve-review-commit-insert-date-section
    approve-review-commit-insert-actor-section
    approve-review-commit-insert-title-section)
  "Defines what to show about each commit and in what order.
Each function receives a commit alist as its argument and should
insert its content at point (without trailing newline)."
  :group 'approve
  :type 'hook)

(defcustom approve-commit-date-format "%Y-%m-%d %H:%M"
  "Format string for displaying commit dates.
See `format-time-string' for available format specifiers."
  :group 'approve
  :type 'string)

;;; Keymaps

(defvar-keymap approve-commit-section-map
  :doc "Keymap for `approve-commit' sections.")

;;; Section Insert Functions

(defun approve-review-commit-insert-sha-section (commit)
  "Insert the abbreviated SHA for COMMIT."
  (let ((abbreviated-oid (alist-get 'abbreviatedOid commit)))
    (insert (approve-ui-propertize-face abbreviated-oid 'approve-commit-sha-face))))

(defun approve-review-commit-insert-date-section (commit)
  "Insert the commit date for COMMIT."
  (when-let ((date-str (approve-ui-format-date
                        (alist-get 'committedDate commit)
                        approve-commit-date-format)))
    (insert (approve-ui-propertize-face date-str 'approve-commit-date-face))))

(defun approve-review-commit-insert-actor-section (commit)
  "Insert author (and committer if different) for COMMIT."
  (let ((author (alist-get 'author commit))
        (committer (alist-get 'committer commit)))
    (insert (approve-ui-format-git-actor author 'approve-commit-author-face))
    ;; Show committer only if different from author
    (when (and committer
               (not (approve-ui-actors-same-p author committer)))
      (insert "/"
              (approve-ui-format-git-actor committer 'approve-commit-author-face)))))

(defun approve-review-commit-insert-title-section (commit)
  "Insert the commit title (message headline) for COMMIT."
  (let ((headline (alist-get 'messageHeadline commit)))
    (insert headline)))

;;; Commit Data Accessors

(defun approve--get-commit-by-oid (oid)
  "Return the commit data for commit with OID."
  (when-let ((commits-data (approve-model-root 'commits)))
    (let ((commits (approve-model-get-nodes commits-data)))
      (cl-loop for commit-wrapper in commits
               for commit = (alist-get 'commit commit-wrapper)
               when (and commit (equal (alist-get 'oid commit) oid))
               return commit))))

(defun approve--current-commit-section-p ()
  "Return non-nil if point is on an `approve-commit' section."
  (when-let ((section (magit-current-section)))
    (eq (oref section type) 'approve-commit)))

(defun approve--current-commit-oid ()
  "Return the OID of the commit at point, or nil if not on a commit section."
  (when (approve--current-commit-section-p)
    (oref (magit-current-section) value)))

(defun approve--current-commit ()
  "Return the commit data at point, or nil if not on a commit section."
  (when-let ((oid (approve--current-commit-oid)))
    (approve--get-commit-by-oid oid)))

;;; Commit Actions

(defun approve-commit-yank-abbreviated-sha ()
  "Copy the abbreviated SHA of the commit at point to the kill ring."
  (interactive)
  (approve-with-pr-buffer
    (if-let ((commit (approve--current-commit)))
        (let ((sha (alist-get 'abbreviatedOid commit)))
          (kill-new sha)
          (message "Copied: %s" sha))
      (user-error "No commit at point"))))

(defun approve-commit-yank-sha ()
  "Copy the full SHA of the commit at point to the kill ring."
  (interactive)
  (approve-with-pr-buffer
    (if-let ((commit (approve--current-commit)))
        (let ((sha (alist-get 'oid commit)))
          (kill-new sha)
          (message "Copied: %s" sha))
      (user-error "No commit at point"))))

(defun approve-commit-yank-message ()
  "Copy the commit message of the commit at point to the kill ring."
  (interactive)
  (approve-with-pr-buffer
    (if-let ((commit (approve--current-commit)))
        (let ((message (alist-get 'message commit)))
          (if (and message (not (string-empty-p message)))
              (progn
                (kill-new message)
                (message "Copied commit message"))
            (user-error "Commit has no message")))
      (user-error "No commit at point"))))

(defun approve-commit-browse-tree ()
  "Open the repository tree at the commit at point in the default web browser."
  (interactive)
  (approve-with-pr-buffer
    (if-let ((commit (approve--current-commit)))
        (let ((tree-url (alist-get 'treeUrl commit)))
          (if (and tree-url (not (string-empty-p tree-url)))
              (progn
                (browse-url tree-url)
                (message "Opened commit tree in browser"))
            (user-error "Commit has no tree URL")))
      (user-error "No commit at point"))))

;;; Main Section

(defun approve-insert-commits-section ()
  "Insert the commits section in the PR review buffer.
Loops through all commits and inserts them one per line using
`approve-review-commit-sections' for each commit.
The section heading shows the number of displayed commits and indicates
if more commits exist that weren't fetched.
Expanding a commit shows its full commit message.
Individual commits are collapsed by default."
  (when-let ((commits-data (approve-model-root 'commits)))
    (let* ((commits (approve-model-get-nodes commits-data))
           (commit-count (length commits))
           (total-count (approve-model-get-total-count commits-data))
           (truncated-p (approve-model-truncated-p commits-data)))
      ;; Add blank line before the section for visual separation
      (insert "\n")
      (magit-insert-section (commits)
        (magit-insert-heading
          commit-count
          (if (> commit-count 1) "Commits" "Commit"))
        (dolist (commit-wrapper commits)
          ;; Each commit in the list is wrapped in a `commit' key
          (let ((commit (alist-get 'commit commit-wrapper)))
            (when commit
              (let ((oid (alist-get 'oid commit))
                    (message (alist-get 'message commit)))
                (magit-insert-section (approve-commit oid t)
                  (magit-insert-heading
                    (approve--format-commit-line commit))
                  ;; Full commit message as hidden content
                  (when (and message (not (string-empty-p message)))
                    (insert message)
                    (unless (string-suffix-p "\n" message)
                      (insert "\n"))))))))
        ;; Show truncation info at the end of the section
        (when truncated-p
          (insert (propertize
                   (format "(showing %d of %d)" commit-count (or total-count "?"))
                   'face 'approve-pagination-truncated-face)
                  "\n"))
        (insert "\n")))))

(defun approve--format-commit-line (commit)
  "Format a single COMMIT for display as a heading line.
Uses `approve-review-commit-sections' to build the line."
  (let ((parts '()))
    (dolist (fn approve-review-commit-sections)
      (with-temp-buffer
        (funcall fn commit)
        (push (buffer-string) parts)))
    (string-join (nreverse parts) " ")))

(provide 'approve-ui-commits)
;;; approve-ui-commits.el ends here
