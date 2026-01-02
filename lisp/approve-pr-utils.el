;;; approve-pr-utils.el --- Common PR utilities for Approve  -*- lexical-binding: t; -*-

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

;; This module provides common utilities for working with Pull Request
;; data across different Approve UI components (dashboard, search, etc.).
;;
;; It includes:
;; - Time formatting (relative times like "2h", "3d")
;; - PR field extraction and formatting
;; - Review status and state formatting
;; - String truncation utilities

;;; Code:

(require 'approve-ui-faces)
(require 'approve-ui-helpers)

;;; Time Formatting

(defun approve-pr-format-relative-time (date-string)
  "Format DATE-STRING as a relative time (e.g., \"2h\", \"3d\").
DATE-STRING should be in ISO 8601 format.
Returns nil if DATE-STRING is nil or cannot be parsed."
  (when-let ((time (approve-ui-parse-iso-date date-string)))
    (let* ((now (current-time))
           (diff (float-time (time-subtract now time)))
           (minutes (/ diff 60))
           (hours (/ diff 3600))
           (days (/ diff 86400))
           (weeks (/ diff 604800)))
      (cond
       ((< minutes 1) "now")
       ((< minutes 60) (format "%dm" (floor minutes)))
       ((< hours 24) (format "%dh" (floor hours)))
       ((< days 7) (format "%dd" (floor days)))
       ((< weeks 52) (format "%dw" (floor weeks)))
       (t (format-time-string "%Y-%m-%d" time))))))

;;; String Utilities

(defun approve-pr-truncate-string (str width)
  "Truncate STR to WIDTH characters, adding ellipsis if needed.
Returns STR unchanged if it's shorter than or equal to WIDTH."
  (if (> (length str) width)
      (concat (substring str 0 (- width 1)) "…")
    str))

(defun approve-pr-pad-string (str width)
  "Pad STR with spaces to WIDTH characters.
If STR is longer than WIDTH, it is truncated first."
  (format (format "%%-%ds" width)
          (approve-pr-truncate-string str width)))

;;; PR Field Extraction

(defun approve-pr-get-number (pr)
  "Extract the PR number from PR data."
  (alist-get 'number pr))

(defun approve-pr-get-title (pr)
  "Extract the title from PR data."
  (or (alist-get 'title pr) ""))

(defun approve-pr-get-url (pr)
  "Extract the URL from PR data."
  (alist-get 'url pr))

(defun approve-pr-get-state (pr)
  "Extract the state from PR data.
Returns \"OPEN\", \"MERGED\", or \"CLOSED\"."
  (alist-get 'state pr))

(defun approve-pr-get-draft-p (pr)
  "Return non-nil if PR is a draft."
  (alist-get 'isDraft pr))

(defun approve-pr-get-review-decision (pr)
  "Extract the review decision from PR data.
Returns \"APPROVED\", \"CHANGES_REQUESTED\", \"REVIEW_REQUIRED\", or nil."
  (alist-get 'reviewDecision pr))

(defun approve-pr-get-updated-at (pr)
  "Extract the updatedAt timestamp from PR data."
  (alist-get 'updatedAt pr))

(defun approve-pr-get-author-login (pr)
  "Extract the author's login from PR data."
  (alist-get 'login (alist-get 'author pr)))

(defun approve-pr-get-repo-full-name (pr)
  "Extract the full repository name (owner/repo) from PR data."
  (let* ((repo (alist-get 'repository pr))
         (owner (alist-get 'login (alist-get 'owner repo)))
         (name (alist-get 'name repo)))
    (format "%s/%s" owner name)))

;;; PR Formatting

(defun approve-pr-format-number (pr &optional width)
  "Format the PR number from PR data with face.
If WIDTH is provided, left-align to that width."
  (let* ((number (approve-pr-get-number pr))
         (format-str (if width (format "#%%-%dd" (1- width)) "#%d")))
    (approve-ui-propertize-face
     (format format-str number)
     'approve-dashboard-pr-number-face)))

(defun approve-pr-format-title (pr width &optional use-draft-face)
  "Format the PR title from PR data, truncating to WIDTH.
If USE-DRAFT-FACE is non-nil and PR is a draft, use draft face."
  (let* ((title (approve-pr-get-title pr))
         (is-draft (approve-pr-get-draft-p pr))
         (truncated (approve-pr-truncate-string title width))
         (face (if (and use-draft-face is-draft)
                   'approve-dashboard-draft-face
                 'approve-dashboard-pr-title-face)))
    (approve-ui-propertize-face truncated face)))

(defun approve-pr-format-repo (pr width)
  "Format the repository name from PR data, truncating to WIDTH."
  (let ((full-name (approve-pr-get-repo-full-name pr)))
    (approve-ui-propertize-face
     (approve-pr-truncate-string full-name width)
     'approve-dashboard-repo-face)))

(defun approve-pr-format-author (pr &optional width)
  "Format the author login from PR data.
If WIDTH is provided, truncate to that width."
  (let ((login (or (approve-pr-get-author-login pr) "")))
    (approve-ui-propertize-face
     (if width (approve-pr-truncate-string login width) login)
     'approve-dashboard-author-face)))

(defun approve-pr-format-time (pr &optional width)
  "Format the updated time from PR data as relative time.
If WIDTH is provided, right-align to that width."
  (let* ((updated-at (approve-pr-get-updated-at pr))
         (relative (or (approve-pr-format-relative-time updated-at) ""))
         (formatted (if width (format (format "%%%ds" width) relative) relative)))
    (approve-ui-propertize-face formatted 'approve-dashboard-time-face)))

(defun approve-pr-format-review-status (pr)
  "Format the review decision status from PR data.
Returns a single character indicator: ✓ (approved), ✗ (changes requested),
○ (review required), or space (unknown/none)."
  (pcase (approve-pr-get-review-decision pr)
    ("APPROVED"
     (approve-ui-propertize-face "✓" 'approve-dashboard-review-approved-face))
    ("CHANGES_REQUESTED"
     (approve-ui-propertize-face "✗" 'approve-dashboard-review-changes-requested-face))
    ("REVIEW_REQUIRED"
     (approve-ui-propertize-face "○" 'approve-dashboard-review-required-face))
    (_ " ")))

(defun approve-pr-format-state (pr)
  "Format the PR state (Open/Merged/Closed/Draft) with appropriate face."
  (let ((state (approve-pr-get-state pr))
        (is-draft (approve-pr-get-draft-p pr)))
    (cond
     (is-draft
      (approve-ui-propertize-face "Draft" 'approve-dashboard-draft-face))
     ((string= state "OPEN")
      (approve-ui-propertize-face "Open" 'approve-dashboard-state-open-face))
     ((string= state "MERGED")
      (approve-ui-propertize-face "Merged" 'approve-dashboard-state-merged-face))
     ((string= state "CLOSED")
      (approve-ui-propertize-face "Closed" 'approve-dashboard-state-closed-face))
     (t (or state "")))))

(provide 'approve-pr-utils)
;;; approve-pr-utils.el ends here
