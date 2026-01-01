;;; approve-ui-faces.el --- Faces for Approve -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (c) 2025 Björn Larsson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This file contains all face definitions for Approve and any related
;; utility functions.

;;; Code:

(defgroup approve-faces nil
  "Faces used by Approve."
  :group 'approve
  :group 'faces)

;;; Section Faces

(defface approve-section-heading-face
  '((t :inherit magit-section-heading))
  "Face for top-level section headings."
  :group 'approve-faces)

;;; Header Faces

(defface approve-title-face
  '((t :inherit approve-section-heading-face :height 1.3))
  "Face for the PR title."
  :group 'approve-faces)

(defface approve-pr-number-face
  '((t :inherit font-lock-comment-face))
  "Face for the PR number."
  :group 'approve-faces)

(defface approve-header-title-face
  '((t :inherit magit-section-secondary-heading))
  "Face for header titles like \"Title:\", \"Author:\", etc."
  :group 'approve-faces)

;;; Author Faces

(defface approve-author-face
  '((t :inherit font-lock-keyword-face))
  "Face for the PR author username."
  :group 'approve-faces)

;;; Commit Faces

(defface approve-commit-sha-face
  '((t :inherit magit-hash))
  "Face for commit SHA hashes."
  :group 'approve-faces)

(defface approve-commit-date-face
  '((t :inherit magit-log-date))
  "Face for commit dates."
  :group 'approve-faces)

(defface approve-commit-author-face
  '((t :inherit magit-log-author))
  "Face for commit author names."
  :group 'approve-faces)

;;; Pagination Faces

(defface approve-pagination-truncated-face
  '((t :inherit warning))
  "Face for indicating truncated/incomplete data."
  :group 'approve-faces)

;;; Diffstat Faces

(defface approve-diffstat-added-face
  '((t :inherit magit-diffstat-added))
  "Face for added lines indicator in diffstat."
  :group 'approve-faces)

(defface approve-diffstat-removed-face
  '((t :inherit magit-diffstat-removed))
  "Face for removed lines indicator in diffstat."
  :group 'approve-faces)

(defface approve-diffstat-file-face
  '((t :inherit magit-filename))
  "Face for filenames in diffstat."
  :group 'approve-faces)

(defface approve-diffstat-count-face
  '((t :inherit default))
  "Face for the change count in diffstat."
  :group 'approve-faces)

;;; File Viewed State Faces

(defface approve-file-viewed-face
  '((t :inherit success))
  "Face for the viewed indicator on files."
  :group 'approve-faces)

(defface approve-file-unviewed-face
  '((t :inherit warning))
  "Face for the unviewed indicator on files."
  :group 'approve-faces)

(defface approve-file-dismissed-face
  '((t :inherit error))
  "Face for the dismissed (new changes) indicator on files."
  :group 'approve-faces)

;;; Comment Faces

(defface approve-comment-author-face
  '((t :inherit font-lock-keyword-face))
  "Face for comment author names."
  :group 'approve-faces)

(defface approve-comment-timestamp-face
  '((t :inherit font-lock-comment-face))
  "Face for comment timestamps."
  :group 'approve-faces)

(defface approve-comment-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for placeholder text when no comments exist."
  :group 'approve-faces)

(defface approve-comment-code-block-face
  '((t :inherit (fixed-pitch font-lock-string-face) :extend t))
  "Face for code blocks in comments."
  :group 'approve-faces)

(defface approve-comment-code-inline-face
  '((t :inherit (fixed-pitch font-lock-constant-face)))
  "Face for inline code in comments."
  :group 'approve-faces)

(defface approve-comment-reaction-face
  '((t :inherit default))
  "Face for reaction counts on comments."
  :group 'approve-faces)

;;; Comment State Faces

(defface approve-comment-state-approved-face
  '((t :inherit success))
  "Face for APPROVED review state."
  :group 'approve-faces)

(defface approve-comment-state-changes-requested-face
  '((t :inherit error))
  "Face for CHANGES_REQUESTED review state."
  :group 'approve-faces)

(defface approve-comment-state-commented-face
  '((t :inherit font-lock-comment-face))
  "Face for COMMENTED review state."
  :group 'approve-faces)

(defface approve-comment-state-dismissed-face
  '((t :inherit warning))
  "Face for DISMISSED review state."
  :group 'approve-faces)

(defface approve-comment-state-pending-face
  '((t :inherit font-lock-warning-face))
  "Face for PENDING review state."
  :group 'approve-faces)

(defface approve-comment-state-default-face
  '((t :inherit default))
  "Default face for unknown review states."
  :group 'approve-faces)

;;; Utility Functions

;; Note: approve-ui-propertize-face and other helpers are in approve-ui-helpers.el

(provide 'approve-ui-faces)
;;; approve-ui-faces.el ends here
