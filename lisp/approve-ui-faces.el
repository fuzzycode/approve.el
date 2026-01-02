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

(defface approve-file-comment-indicator-face
  '((t :inherit font-lock-comment-face))
  "Face for the comment count indicator on files."
  :group 'approve-faces)

;;; Thread Visual Faces

(defface approve-thread-connector-face
  '((t :inherit shadow))
  "Face for the thread connector line between comments."
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

;;; Description Faces

(defface approve-description-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for placeholder text when no PR description exists."
  :group 'approve-faces)

(defface approve-comment-code-block-face
  '((t :inherit (fixed-pitch font-lock-string-face) :extend t))
  "Face for code blocks in comments."
  :group 'approve-faces)

(defface approve-comment-code-inline-face
  '((t :inherit (fixed-pitch font-lock-constant-face)))
  "Face for inline code in comments."
  :group 'approve-faces)

(defface approve-comment-blockquote-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for blockquotes in comments."
  :group 'approve-faces)

(defface approve-comment-blockquote-border-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for the border character of blockquotes in comments."
  :group 'approve-faces)

(defface approve-comment-reaction-face
  '((t :inherit default))
  "Face for reaction counts on comments."
  :group 'approve-faces)

(defface approve-comment-edited-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the edited indicator on comments."
  :group 'approve-faces)

(defface approve-comment-pending-face
  '((t :inherit font-lock-warning-face :slant italic))
  "Face for the pending indicator on comments."
  :group 'approve-faces)

(defface approve-comment-list-bullet-face
  '((t :inherit font-lock-builtin-face))
  "Face for list bullet points in comments."
  :group 'approve-faces)

(defface approve-comment-task-checked-face
  '((t :inherit success))
  "Face for checked task list items in comments."
  :group 'approve-faces)

(defface approve-comment-task-unchecked-face
  '((t :inherit shadow))
  "Face for unchecked task list items in comments."
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

;;; HTML Rendering Faces

(defface approve-html-blockquote-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for blockquotes in HTML content."
  :group 'approve-faces)

(defface approve-html-blockquote-border-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for the border character of blockquotes in HTML content."
  :group 'approve-faces)

(defface approve-html-inline-code-face
  '((t :inherit (fixed-pitch font-lock-constant-face)))
  "Face for inline code in HTML content."
  :group 'approve-faces)

(defface approve-html-code-block-face
  '((t :inherit (fixed-pitch font-lock-string-face) :extend t))
  "Face for code blocks in HTML content."
  :group 'approve-faces)

(defface approve-html-list-bullet-face
  '((t :inherit font-lock-builtin-face))
  "Face for list bullet points in HTML content."
  :group 'approve-faces)

(defface approve-html-task-checked-face
  '((t :inherit success))
  "Face for checked task list items in HTML content."
  :group 'approve-faces)

(defface approve-html-task-unchecked-face
  '((t :inherit shadow))
  "Face for unchecked task list items in HTML content."
  :group 'approve-faces)

(defface approve-html-strikethrough-face
  '((t :strike-through t))
  "Face for strikethrough text in HTML content."
  :group 'approve-faces)

(defface approve-html-link-face
  '((t :inherit link))
  "Face for links in HTML content."
  :group 'approve-faces)

(defface approve-html-details-summary-face
  '((t :inherit bold))
  "Face for the summary line of a <details> element."
  :group 'approve-faces)

;;; Dashboard Faces

(defface approve-dashboard-section-title-face
  '((t :inherit magit-section-heading))
  "Face for dashboard section titles."
  :group 'approve-faces)

(defface approve-dashboard-section-count-face
  '((t :inherit font-lock-comment-face))
  "Face for the count indicator in dashboard section titles."
  :group 'approve-faces)

(defface approve-dashboard-pr-number-face
  '((t :inherit magit-hash))
  "Face for PR numbers in the dashboard."
  :group 'approve-faces)

(defface approve-dashboard-pr-title-face
  '((t :inherit default))
  "Face for PR titles in the dashboard."
  :group 'approve-faces)

(defface approve-dashboard-repo-face
  '((t :inherit font-lock-comment-face))
  "Face for repository names in the dashboard."
  :group 'approve-faces)

(defface approve-dashboard-author-face
  '((t :inherit font-lock-keyword-face))
  "Face for PR author names in the dashboard."
  :group 'approve-faces)

(defface approve-dashboard-time-face
  '((t :inherit font-lock-comment-face))
  "Face for relative time display in the dashboard."
  :group 'approve-faces)

(defface approve-dashboard-draft-face
  '((t :inherit shadow :slant italic))
  "Face for draft PR indicator in the dashboard."
  :group 'approve-faces)

(defface approve-dashboard-state-open-face
  '((t :inherit success))
  "Face for open PR state indicator."
  :group 'approve-faces)

(defface approve-dashboard-state-merged-face
  '((t :inherit magit-branch-remote))
  "Face for merged PR state indicator."
  :group 'approve-faces)

(defface approve-dashboard-state-closed-face
  '((t :inherit error))
  "Face for closed PR state indicator."
  :group 'approve-faces)

(defface approve-dashboard-review-approved-face
  '((t :inherit success))
  "Face for approved review decision indicator."
  :group 'approve-faces)

(defface approve-dashboard-review-changes-requested-face
  '((t :inherit error))
  "Face for changes requested review decision indicator."
  :group 'approve-faces)

(defface approve-dashboard-review-required-face
  '((t :inherit warning))
  "Face for review required indicator."
  :group 'approve-faces)

(defface approve-dashboard-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for empty section placeholder text."
  :group 'approve-faces)

(defface approve-dashboard-loading-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for loading indicator text."
  :group 'approve-faces)

(defface approve-dashboard-error-face
  '((t :inherit error))
  "Face for error messages in the dashboard."
  :group 'approve-faces)

;;; Suggested Changes / Diff Faces

(defface approve-diff-context-face
  '((t :inherit default))
  "Face for context lines in diffs and suggested changes."
  :group 'approve-faces)

(defface approve-diff-addition-face
  '((t :inherit magit-diff-added))
  "Face for added lines in diffs and suggested changes."
  :group 'approve-faces)

(defface approve-diff-deletion-face
  '((t :inherit magit-diff-removed))
  "Face for removed lines in diffs and suggested changes."
  :group 'approve-faces)

;;; Utility Functions

;; Note: approve-ui-propertize-face and other helpers are in approve-ui-helpers.el

(provide 'approve-ui-faces)
;;; approve-ui-faces.el ends here
