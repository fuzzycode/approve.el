;;; approve-graphql.el --- GraphQL query handling for Approve  -*- lexical-binding: t; -*-

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

;; This module provides functionality for loading and processing GraphQL
;; query files with include directive support.
;;
;; Include directives allow GraphQL files to reference other files,
;; enabling reuse of fragments across multiple queries.
;;
;; Syntax:
;;   ${include:path/to/file.graphql}
;;
;; Paths are relative to the graphql directory.

;;; Code:

(require 'cl-lib)
(require 'dash)

;;; Custom Variables

(defgroup approve-graphql nil
  "GraphQL handling for Approve."
  :group 'approve
  :prefix "approve-graphql-")

(defcustom approve-graphql-directory
  (expand-file-name "graphql" (file-name-directory
                               (directory-file-name
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  "Directory containing GraphQL query files."
  :type 'directory
  :group 'approve-graphql)

;;; Constants

(defconst approve-graphql--include-regexp
  "\\${include:\\([^}]+\\)}"
  "Regexp matching include directives.
Group 1 captures the file path.")

;;; Error Handling

(define-error 'approve-graphql-error "Approve GraphQL error")
(define-error 'approve-graphql-file-not-found "GraphQL file not found" 'approve-graphql-error)
(define-error 'approve-graphql-circular-include "Circular include detected" 'approve-graphql-error)

;;; Internal Functions

(defun approve-graphql--resolve-path (path)
  "Resolve PATH relative to the GraphQL directory.
Returns the absolute path to the file."
  (expand-file-name path approve-graphql-directory))

(defun approve-graphql--read-file (file-path)
  "Read the contents of FILE-PATH.
Signals `approve-graphql-file-not-found' if file doesn't exist."
  (unless (file-exists-p file-path)
    (signal 'approve-graphql-file-not-found (list file-path)))
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun approve-graphql--process-includes (content current-file include-stack included-set)
  "Process include directives in CONTENT.
CURRENT-FILE is the path of the file being processed.
INCLUDE-STACK is a list of files currently being processed,
used for cycle detection.
INCLUDED-SET is a hash table of files already included globally,
used to prevent duplicate fragment definitions.
Returns the content with all includes expanded."
  (let ((result content)
        (start 0))
    (while (string-match approve-graphql--include-regexp result start)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (include-path (match-string 1 result))
             (resolved-path (approve-graphql--resolve-path include-path)))
        (when (-contains? include-stack resolved-path)
          (signal 'approve-graphql-circular-include
                  (list resolved-path include-stack)))
        (let ((included-content
               (if (gethash resolved-path included-set)
                   ;; Already included globally, skip content but still remove directive
                   ""
                 ;; Mark as included and process
                 (puthash resolved-path t included-set)
                 (approve-graphql--load-and-process resolved-path
                                                    (cons current-file include-stack)
                                                    included-set))))
          (setq result (concat (substring result 0 match-start)
                               included-content
                               (substring result match-end)))
          (setq start (+ match-start (length included-content))))))
    result))

(defun approve-graphql--load-and-process (file-path include-stack included-set)
  "Load and process FILE-PATH, expanding any includes.
INCLUDE-STACK is used for circular dependency detection.
INCLUDED-SET tracks globally included files to prevent duplicates.
Returns the processed content."
  (let ((content (approve-graphql--read-file file-path)))
    (approve-graphql--process-includes content file-path include-stack included-set)))

;;; Public API

(defun approve-graphql-load (query-name)
  "Load a GraphQL query by QUERY-NAME.
QUERY-NAME should be a path relative to the graphql directory,
e.g., \"queries/get-pull-request.graphql\".

Returns the processed query string with all includes expanded."
  (let* ((file-path (approve-graphql--resolve-path query-name))
         (included-set (make-hash-table :test 'equal)))
    (approve-graphql--load-and-process file-path nil included-set)))

(provide 'approve-graphql)
;;; approve-graphql.el ends here
