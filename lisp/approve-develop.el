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
(require 'approve-model)
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

(defun approve-dev--insert-text (text)
  "Insert TEXT directly into the debug buffer.
Unlike `approve-dev--insert-result', this inserts the text as-is
without quoting, suitable for multi-line text content like GraphQL queries."
  (insert text)
  (unless (eq (char-before) ?\n)
    (insert "\n")))

(defun approve-dev--display-result (title data)
  "Display DATA with TITLE in the debug buffer."
  (let ((buffer (approve-dev--get-debug-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (approve-dev--insert-header title)
      (approve-dev--insert-result data)
      (insert (format "--- %s ---\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (display-buffer buffer)))

(defun approve-dev--display-text (title text)
  "Display TEXT with TITLE in the debug buffer.
Unlike `approve-dev--display-result', this displays TEXT as-is
without quoting, suitable for multi-line content like GraphQL queries."
  (let ((buffer (approve-dev--get-debug-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (approve-dev--insert-header title)
      (approve-dev--insert-text text)
      (insert (format "\n--- %s ---\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
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

;;; Doom Emacs Integration

(defun approve-dev-setup-doom-popups ()
  "Set up Doom Emacs popup rules for Approve buffers.
Call this from your Doom config to prevent Approve file view buffers
from being treated as popups.

Example usage in config.el:
  (after! approve
    (require \\='approve-develop)
    (approve-dev-setup-doom-popups))"
  (interactive)
  (when (fboundp 'set-popup-rule!)
    ;; Ignore file view buffers - they should open as regular buffers
    ;; Buffer name pattern: *Approve: owner/repo - filepath*
    (set-popup-rule! "^\\*Approve: .+/.+ - .+\\*$" :ignore t)
    (message "Doom popup rules configured for Approve")))

;;; Interactive Commands - Reload Package

(defun approve-dev--list-package-features ()
  "Return a list of all Approve package features.
Discovers all .el files in the lisp directory, excluding approve-develop.el."
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (locate-dominating-file default-directory "Cask")))
         (lisp-dir (expand-file-name "lisp" project-root))
         (files (directory-files lisp-dir nil "^approve.*\\.el$")))
    (mapcar (lambda (f)
              (intern (file-name-sans-extension f)))
            (seq-remove (lambda (f) (string= f "approve-develop.el")) files))))

(defun approve-dev-reload ()
  "Reload the Approve package.
Unloads all approve-* features and then reloads `approve' which
pulls in all its dependencies."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (locate-dominating-file default-directory "Cask")))
         (lisp-dir (expand-file-name "lisp" project-root))
         (features (approve-dev--list-package-features))
         ;; Copy list before sorting since sort is destructive
         ;; Sort alphabetically: approve < approve-api < approve-graphql
         ;; This puts approve first, which is correct for unloading since
         ;; approve depends on the others
         (sorted-features (sort (copy-sequence features)
                                (lambda (a b)
                                  (string< (symbol-name a) (symbol-name b))))))
    ;; Unload all features (approve first since it depends on the others)
    (dolist (feature sorted-features)
      (when (featurep feature)
        (unload-feature feature t)))
    ;; Temporarily put our lisp dir at front of load-path so require
    ;; finds the correct files (not files from other projects)
    (let ((load-path (cons lisp-dir load-path)))
      (load (expand-file-name "approve.el" lisp-dir) nil nil t))
    (message "Reloaded approve (with dependencies: %s)"
             (mapconcat #'symbol-name
                        (seq-filter #'featurep features)
                        ", "))))


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
      (let ((resolved-query (approve-graphql-load query-name)))
        (approve-dev--display-text
         (format "GraphQL Query: %s" query-name)
         resolved-query)
        (message "Query loaded and displayed in debug buffer"))
    (approve-graphql-file-not-found
     (user-error "GraphQL file not found: %s" (cadr err)))
    (approve-graphql-circular-include
     (user-error "Circular include detected: %s -> %s"
                 (cadr err) (caddr err)))))

;;; Interactive Commands - Model Inspection

(defun approve-dev-model-dump ()
  "Return a string representation of the current store for debugging."
  (interactive)
  (if (not approve-model--store)
      (message "Data store not initialized")
    (let ((output '()))
      (push (format "Root: %S" approve-model--root) output)
      (push (format "Metadata: %S" approve-model--metadata) output)
      (push "--- Entities by Type ---" output)
      (maphash
       (lambda (typename type-store)
         (push (format "\n[%s] (%d entities)"
                       typename (hash-table-count type-store))
               output)
         (maphash
          (lambda (id _entity)
            (push (format "  - %s" id) output))
          type-store))
       approve-model--store)
      (approve-dev--display-result "=== Approve Model Store ==="  output))))

(defun approve-dev-model-stats ()
  "Return statistics about the current store."
  (interactive)
  (if (not approve-model--store)
      '(:initialized nil)
    (let ((type-counts nil)
          (total 0))
      (maphash
       (lambda (typename type-store)
         (let ((count (hash-table-count type-store)))
           (push (cons typename count) type-counts)
           (cl-incf total count)))
       approve-model--store)
      (approve-dev--display-result "=== Approve Model Stats==="
                                   (list :initialized t
                                         :total-entities total
                                         :types-count (hash-table-count approve-model--store)
                                         :by-type type-counts
                                         :has-root (not (null approve-model--root)))))))

(provide 'approve-develop)
;;; approve-develop.el ends here
