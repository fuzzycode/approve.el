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

;;; Main Section

(defun approve-insert-commits-section ()
  "Insert the commits section in the PR review buffer.
Loops through all commits and inserts them one per line using
`approve-review-commit-sections' for each commit.
The section heading shows \"Commits (N)\" where N is the total count.
Expanding a commit shows its full commit message.
Individual commits are collapsed by default."
  (when-let ((commits (approve-model-root 'commits)))
    (let ((commit-count (length commits)))
      ;; Add blank line before the section for visual separation
      (insert "\n")
      (magit-insert-section (commits)
        (magit-insert-heading commit-count (if (> commit-count 1) "Commits" "Commit"))
        (dolist (commit-wrapper commits)
          ;; Each commit in the list is wrapped in a `commit' key
          (let ((commit (alist-get 'commit commit-wrapper)))
            (when commit
              (let ((oid (alist-get 'oid commit))
                    (message (alist-get 'message commit))
                    (first t))
                (magit-insert-section (approve-commit oid t)
                  (magit-insert-heading
                    (dolist (fn approve-review-commit-sections)
                      (if first
                          (setq first nil)
                        (insert " "))
                      (funcall fn commit)))
                  ;; Full commit message as hidden content
                  (when (and message (not (string-empty-p message)))
                    (insert message)
                    (unless (string-suffix-p "\n" message)
                      (insert "\n"))))))))
        (insert "\n")))))

(provide 'approve-ui-commits)
;;; approve-ui-commits.el ends here
