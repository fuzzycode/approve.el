;;; approve-ui-helpers.el --- UI helper functions for Approve  -*- lexical-binding: t; -*-

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

;; This file provides common UI helper functions used across Approve's
;; UI components.  These include date parsing/formatting, actor formatting,
;; and other utilities.

;;; Code:

(require 'parse-time)

(require 'approve-ui-faces)
(require 'approve-eldoc)

;;; Date Helpers

(defun approve-ui-parse-iso-date (date-string)
  "Parse DATE-STRING in ISO 8601 format to Emacs time.
Returns nil if DATE-STRING is nil or empty."
  (when (and date-string (not (string-empty-p date-string)))
    (parse-iso8601-time-string date-string)))

(defun approve-ui-format-date (date-string format)
  "Format DATE-STRING according to FORMAT.
DATE-STRING should be in ISO 8601 format.
FORMAT is a format string for `format-time-string'.
Returns nil if DATE-STRING cannot be parsed."
  (when-let ((time (approve-ui-parse-iso-date date-string)))
    (format-time-string format time)))

;;; Actor Helpers

(defun approve-ui-format-git-actor (git-actor face)
  "Format GIT-ACTOR for display, returning propertized string with eldoc.
GIT-ACTOR is an alist with `name', `email', and optionally `user' fields.
FACE is the face to apply to the formatted string."
  (let* ((name (alist-get 'name git-actor))
         (email (alist-get 'email git-actor))
         (user (alist-get 'user git-actor))
         (login (when user (alist-get 'login user)))
         ;; Display preference: login > name > email
         (display-name (or login name email "unknown"))
         (hover-doc (approve-eldoc-format-user login name email)))
    (approve-eldoc-propertize
     (approve-ui-propertize-face display-name face)
     display-name
     hover-doc
     face)))

(defun approve-ui-actors-same-p (author committer)
  "Return non-nil if AUTHOR and COMMITTER represent the same person.
Compares by email first, then by name if emails are not available.
AUTHOR and COMMITTER are alists with `name' and `email' fields."
  (let ((author-email (alist-get 'email author))
        (committer-email (alist-get 'email committer))
        (author-name (alist-get 'name author))
        (committer-name (alist-get 'name committer)))
    (cond
     ;; If both have emails, compare emails
     ((and author-email committer-email)
      (string-equal author-email committer-email))
     ;; If both have names, compare names
     ((and author-name committer-name)
      (string-equal author-name committer-name))
     ;; Otherwise assume different
     (t nil))))

;;; Text Formatting

(defun approve-ui-propertize-face (string face)
  "Propertize STRING with FACE using both `face' and `font-lock-face'.
This ensures the face is applied correctly in buffers where
`font-lock-mode' is active."
  (propertize string 'face face 'font-lock-face face))

(provide 'approve-ui-helpers)
;;; approve-ui-helpers.el ends here
