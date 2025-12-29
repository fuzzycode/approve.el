;;; approve-ui-headers.el --- Header sections for Approve UI  -*- lexical-binding: t; -*-

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

;; This module provides header section rendering for Approve.
;;
;; Header sections display key PR information at the top of the review buffer:
;; - Title and PR number
;; - State (open, closed, merged, draft)
;; - Author information
;; - Labels
;; - Assignees
;; - Reviewers and review status
;; - Milestone
;; - Branch information
;;
;; Each section is designed to be independently insertable via hooks,
;; allowing users to customize which headers appear and in what order.

;;; Code:

(require 'magit-section)
(require 'cl-lib)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)
(require 'approve-actions)
(require 'approve-eldoc)

;; Forward declaration for approve-define-key
(declare-function approve-define-key "approve")

;;; Customization

(defcustom approve-review-header-sections-hook
  '(approve-insert-author-section)
  "Hook run to insert header sections in the PR review buffer.
These sections are rendered inside the collapsible headers section."
  :group 'approve
  :type 'hook)

;;; Section Keymaps

(defvar magit-headers-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'approve-action-browse-pr)
    map)
  "Keymap for the headers section.
Named with `magit-' prefix to be automatically used by magit-section.")

(defvar magit-author-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'approve-action-browse-author)
    map)
  "Keymap for the author section.
Named with `magit-' prefix to be automatically used by magit-section.")

;; Add the prefixed key binding at load time
(with-eval-after-load 'approve
  (approve-define-key magit-headers-section-map "e" #'approve-action-edit-title))

;;; Helpers

(defun approve-ui--format-title (title)
  "Return a formatted title string for TITLE."
  (propertize (format "%-17s" title) 'face 'approve-header-title-face))

;;; Sections

(defun approve-insert-author-section ()
  "Insert the author section showing the PR author."
  (with-approve-entity ((:root) (author))
    (when author
      (let* ((login (alist-get 'login author))
             (name (alist-get 'name author))
             (email (alist-get 'email author))
             (url (alist-get 'url author))
             (hover-doc (approve-eldoc-format-user login name email))
             (author-text (approve-eldoc-propertize
                           (propertize (concat "@" login) 'face 'approve-author-face)
                           (concat "@" login)
                           hover-doc
                           'approve-author-face)))
        (magit-insert-section (author url)
          (insert (approve-ui--format-title "Author:")
                  author-text
                  "\n"))))))

(defun approve-insert-header-section ()
  "Insert the header section in the PR review buffer."
  (with-approve-entity ((:root) (title number))
    (magit-insert-section (headers)
      (magit-insert-heading
        (approve-ui-propertize-face title 'approve-title-face)
        " "
        (approve-ui-propertize-face (format "#%d" number) 'approve-pr-number-face))
      (run-hooks 'approve-review-header-sections-hook))))

(provide 'approve-ui-headers)
;;; approve-ui-headers.el ends here
