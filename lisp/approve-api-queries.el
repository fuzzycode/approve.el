;;; approve-api-queries.el --- GraphQL query functions for Approve  -*- lexical-binding: t; -*-

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

;; This module provides read-only GraphQL query functions for Approve.
;;
;; Key responsibilities:
;; - Fetching pull request data from GitHub
;; - Fetching comments, reviews, and review threads
;; - Fetching file diffs and commit information
;; - Pagination handling for large data sets
;;
;; All functions in this module are non-mutating - they only read data
;; from GitHub's GraphQL API.  For operations that modify data, see
;; `approve-api-mutations.el'.

;;; Code:

(require 'cl-lib)

(require 'approve-api)
(require 'approve-graphql)

;;; Public API

(cl-defun approve-api-query-pull-request (owner repo number
                                                 &key
                                                 callback
                                                 error-callback
                                                 (buffer (current-buffer)))
  "Fetch a pull request from GitHub.

OWNER is the repository owner (user or organization).
REPO is the repository name.
NUMBER is the pull request number.

Keyword arguments:
  :callback - Function called with the response data on success.
              Receives an alist with `repository' containing the PR data.
  :error-callback - Function called with error info on failure.
  :buffer - Buffer context for callbacks (defaults to current buffer).

Returns a request ID that can be used with `approve-api-cancel'.

Example:
  (approve-api-query-pull-request
   \"fuzzycode\" \"Approve.el\" 42
   :callback (lambda (data)
               (let ((pr (alist-get \\='pullRequest
                                    (alist-get \\='repository data))))
                 (message \"PR: %s\" (alist-get \\='title pr)))))"
  (approve-api-graphql
   (approve-graphql-load "queries/get-pull-request.graphql")
   `((repo_owner . ,owner)
     (repo_name . ,repo)
     (pr_id . ,number))
   :callback callback
   :error-callback error-callback
   :buffer buffer
   :progress-message (format "Fetching PR #%d from %s/%s..." number owner repo)))

(provide 'approve-api-queries)
;;; approve-api-queries.el ends here
