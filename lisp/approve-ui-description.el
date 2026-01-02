;;; approve-ui-description.el --- PR description section for Approve  -*- lexical-binding: t; -*-

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

;; This module provides the PR description section rendering for Approve.
;;
;; The description section displays the PR body text, rendered from
;; GitHub's HTML representation to preserve formatting, code blocks,
;; task lists, and other markdown features.
;;
;; Features:
;; - Renders GitHub-flavored markdown via bodyHTML
;; - Supports code blocks with syntax highlighting
;; - Handles task lists, blockquotes, and tables
;; - Shows a placeholder message when no description is provided

;;; Code:

(require 'magit-section)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)
(require 'approve-ui-html)

;;; Customization

(defcustom approve-description-empty-message "No description provided."
  "Message to display when the PR has no description."
  :group 'approve
  :type 'string)

(defcustom approve-description-body-indent 2
  "Number of spaces to indent the description body content."
  :group 'approve
  :type 'integer)

;;; Sections

(defun approve-insert-description-section ()
  "Insert the description section showing the PR body."
  (with-approve-entity ((:root) (bodyHTML))
    (insert "\n")
    (magit-insert-section (description)
      (magit-insert-heading "Description")
      (approve-description--insert-body body-html))))

;;; Private Functions

(defun approve-description--insert-body (body-html)
  "Insert the description BODY-HTML content.
If BODY-HTML is nil or empty, inserts a placeholder message."
  (if (or (null body-html) (string-empty-p (string-trim body-html)))
      (progn
        (insert (make-string approve-description-body-indent ?\s)
                (propertize approve-description-empty-message
                            'face 'approve-description-empty-face)
                "\n"))
    ;; Render HTML directly into buffer, preserving text properties
    (approve-html-insert body-html approve-description-body-indent))
  (insert "\n"))

(provide 'approve-ui-description)
;;; approve-ui-description.el ends here
