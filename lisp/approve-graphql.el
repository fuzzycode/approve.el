;;; approve-graphql.el --- GraphQL query loading with fragment includes -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson
;;
;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/bjornlarsson/approve
;; Package-Requires: ((emacs "29.1"))

;; This file is part of Approve.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Load GraphQL queries from files with support for recursive fragment
;; includes.  Include directives use the syntax:
;;
;;   ${include:fragments/actor-fragment.graphql}
;;
;; Paths are resolved relative to a configurable base directory
;; (defaults to the `graphql/' directory in the package).  Cyclic
;; dependencies are detected and reported as errors.

;;; Code:

(require 'cl-lib)

(defvar approve-graphql-base-directory
  (expand-file-name "graphql"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name
                           default-directory)))))
  "Base directory for resolving GraphQL include paths.
Include paths in `${include:...}' directives are resolved relative
to this directory.")

(defconst approve--graphql-include-regexp
  "\\${include:\\([^}]+\\)}"
  "Regexp matching include directives in GraphQL files.
Group 1 captures the relative file path.")

(cl-defun approve-graphql-load (file &key base-directory)
  "Load a GraphQL query from FILE, resolving all include directives.

FILE is a path relative to BASE-DIRECTORY, or an absolute path.
BASE-DIRECTORY defaults to `approve-graphql-base-directory'.

Include directives of the form ${include:path} are recursively
resolved.  Cyclic dependencies are detected and signaled as errors."
  (let* ((base (or base-directory approve-graphql-base-directory))
         (absolute (if (file-name-absolute-p file)
                       file
                     (expand-file-name file base))))
    (approve--graphql-resolve absolute base nil)))

(defun approve--graphql-resolve (file base-directory visited)
  "Resolve GraphQL FILE, expanding include directives.

BASE-DIRECTORY is the root for resolving relative include paths.
VISITED is a list of already-visited absolute file paths for
cycle detection."
  (let ((absolute (expand-file-name file)))
    (when (member absolute visited)
      (error "Cyclic GraphQL include detected: %s (visited: %s)"
             absolute (string-join visited " -> ")))
    (unless (file-exists-p absolute)
      (error "GraphQL file not found: %s" absolute))
    (let ((content (with-temp-buffer
                     (insert-file-contents absolute)
                     (buffer-string)))
          (new-visited (cons absolute visited)))
      (approve--graphql-expand-includes content base-directory new-visited))))

(defun approve--graphql-expand-includes (content base-directory visited)
  "Expand all include directives in CONTENT.

BASE-DIRECTORY is the root for resolving relative include paths.
VISITED is the list of already-visited files for cycle detection."
  (replace-regexp-in-string
   approve--graphql-include-regexp
   (lambda (match)
     (let* ((relative-path (match-string 1 match))
            (absolute-path (expand-file-name relative-path base-directory)))
       (approve--graphql-resolve absolute-path base-directory visited)))
   content
   t t))

(provide 'approve-graphql)

;;; approve-graphql.el ends here
