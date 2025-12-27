;;; approve-develop.el --- Development utilities for Approve  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.
;; This file is NOT part of the Approve package distribution.

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

;; This file provides development and debugging utilities for Approve.
;;
;; It is intended for development use only and should NOT be included
;; in the final package distribution.
;;
;; To use, load this file during development:
;;
;;   (require 'approve-develop)
;;
;; Then use `M-x approve-dev-' to access development commands.

;;; Code:

(require 'approve)
(require 'approve-graphql)
(require 'pp)

;;; Custom Variables

(defgroup approve-develop nil
  "Development utilities for Approve."
  :group 'approve
  :prefix "approve-dev-")

(defcustom approve-dev-debug-buffer "*Approve Debug*"
  "Buffer name for debug output."
  :type 'string
  :group 'approve-develop)

;;; Internal Functions

(defun approve-dev--get-debug-buffer ()
  "Get or create the debug output buffer."
  (let ((buffer (get-buffer-create approve-dev-debug-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode)
        (setq-local buffer-read-only nil)))
    buffer))

(defun approve-dev--insert-header (title)
  "Insert a header with TITLE into the debug buffer."
  (insert (format "\n%s\n%s\n%s\n\n"
                  (make-string 60 ?=)
                  title
                  (make-string 60 ?=))))

(defun approve-dev--insert-result (data)
  "Pretty print DATA into the debug buffer."
  (let ((pp-max-width 80))
    (pp data (current-buffer)))
  (insert "\n"))

(defun approve-dev--display-result (title data)
  "Display DATA with TITLE in the debug buffer."
  (let ((buffer (approve-dev--get-debug-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (approve-dev--insert-header title)
      (approve-dev--insert-result data)
      (insert (format "--- %s ---\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (display-buffer buffer)))

;;; Interactive Commands - Test Running

(defun approve-dev-run-all-tests ()
  "Run all Buttercup tests for Approve."
  (interactive)
  (let ((default-directory (locate-dominating-file
                            (or buffer-file-name default-directory)
                            "Cask")))
    (if default-directory
        (compile "cask exec buttercup -L lisp test/")
      (user-error "Cannot find project root (no Cask file found)"))))

(defun approve-dev-run-tests-at-point ()
  "Run Buttercup test at point."
  (interactive)
  (if (fboundp 'buttercup-run-at-point)
      (buttercup-run-at-point)
    (user-error "Buttercup is not loaded.  Try: (require 'buttercup)")))

(defun approve-dev-run-tests-in-buffer ()
  "Run all Buttercup tests in current buffer."
  (interactive)
  (if (fboundp 'buttercup-run)
      (buttercup-run)
    (user-error "Buttercup is not loaded.  Try: (require 'buttercup)")))

;;; Interactive Commands - Reload Package

(defun approve-dev-reload ()
  "Reload all Approve package files."
  (interactive)
  (let ((files '("approve" "approve-graphql")))
    (dolist (file files)
      (when (featurep (intern file))
        (unload-feature (intern file) t)))
    (dolist (file files)
      (require (intern file)))
    (message "Reloaded: %s" (string-join files ", "))))


;;; Interactive Commands - Debug Buffer Management

(defun approve-dev-clear-debug-buffer ()
  "Clear the debug output buffer."
  (interactive)
  (when-let ((buffer (get-buffer approve-dev-debug-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Debug buffer cleared")))

(defun approve-dev-show-debug-buffer ()
  "Show the debug buffer."
  (interactive)
  (display-buffer (approve-dev--get-debug-buffer)))

;;; Interactive Commands - Evaluate and Inspect

(defun approve-dev-eval-and-inspect (expression)
  "Evaluate EXPRESSION and display result in debug buffer.
Useful for evaluating expressions and keeping a log of results."
  (interactive "xExpression: ")
  (let ((result (eval expression t)))
    (approve-dev--display-result
     (format "Eval: %S" expression)
     result)))

;;; Interactive Commands - GraphQL Inspection

(defun approve-dev--list-graphql-files ()
  "Return a list of all GraphQL files in the graphql directory."
  (let ((dir approve-graphql-directory))
    (when (file-directory-p dir)
      (directory-files-recursively dir "\\.graphql\\'" nil))))

(defun approve-dev--graphql-file-to-query-name (file-path)
  "Convert absolute FILE-PATH to a query name relative to graphql directory."
  (file-relative-name file-path approve-graphql-directory))

(defun approve-dev-inspect-graphql-query (query-name)
  "Load and display the resolved GraphQL query QUERY-NAME.
Lists all available queries and prompts for selection.
The resolved query (with all includes expanded) is displayed
in the debug buffer."
  (interactive
   (let* ((files (approve-dev--list-graphql-files))
          (query-names (mapcar #'approve-dev--graphql-file-to-query-name files)))
     (if (null query-names)
         (user-error "No GraphQL files found in %s" approve-graphql-directory)
       (list (completing-read "GraphQL query: " query-names nil t)))))
  (condition-case err
      (let ((resolved-query (approve-graphql-load query-name t))) ; bypass cache
        (approve-dev--display-result
         (format "GraphQL Query: %s" query-name)
         resolved-query)
        (message "Query loaded and displayed in debug buffer"))
    (approve-graphql-file-not-found
     (user-error "GraphQL file not found: %s" (cadr err)))
    (approve-graphql-circular-include
     (user-error "Circular include detected: %s -> %s"
                 (cadr err) (caddr err)))))

(provide 'approve-develop)
;;; approve-develop.el ends here
