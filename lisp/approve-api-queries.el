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

;;; Internal Functions

(defun approve-api--fetch-diff (owner repo base head callback error-callback buffer)
  "Fetch the diff between BASE and HEAD for OWNER/REPO.
CALLBACK is called with the diff data on success.
ERROR-CALLBACK is called with error info on failure.
BUFFER is the buffer context for callbacks."
  (let ((resource (format "/repos/%s/%s/compare/%s...%s"
                          (url-hexify-string owner)
                          (url-hexify-string repo)
                          (url-hexify-string base)
                          (url-hexify-string head))))
    (approve-api-rest
     "GET" resource
     :callback callback
     :error-callback error-callback
     :buffer buffer
     :progress-message (format "Fetching diff for %s/%s..." owner repo))))

(defun approve-api--extract-pr-refs (data)
  "Extract base and head refs from PR DATA.
Returns a cons cell (BASE . HEAD) or nil if refs are not found."
  (when-let* ((repository (alist-get 'repository data))
              (pull-request (alist-get 'pullRequest repository))
              (base (alist-get 'baseRefOid pull-request))
              (head (alist-get 'headRefOid pull-request)))
    (cons base head)))

(defun approve-api--merge-diff-into-data (data diff-data)
  "Merge DIFF-DATA into the PR DATA structure.
Returns a new alist with the diff added under repository.pullRequest.diff."
  (let* ((repository (copy-alist (alist-get 'repository data)))
         (pull-request (copy-alist (alist-get 'pullRequest repository))))
    (setf (alist-get 'diff pull-request) diff-data)
    (setf (alist-get 'pullRequest repository) pull-request)
    (setf (alist-get 'repository data) repository)
    data))

;;; Public API

(cl-defun approve-api-query-pull-request (owner repo number
                                                 &key
                                                 callback
                                                 error-callback
                                                 (buffer (current-buffer)))
  "Fetch a pull request from GitHub including its diff.

OWNER is the repository owner (user or organization).
REPO is the repository name.
NUMBER is the pull request number.

Keyword arguments:
  :callback - Function called with the response data on success.
              Receives an alist with `repository' containing the PR data
              and the diff under `repository.pullRequest.diff'.
  :error-callback - Function called with error info on failure.
  :buffer - Buffer context for callbacks (defaults to current buffer).

Returns a request ID that can be used with `approve-api-cancel'.

The function first fetches PR metadata via GraphQL, then fetches the diff
via REST API, and finally calls the callback with the merged data.

Example:
  (approve-api-query-pull-request
   \"fuzzycode\" \"Approve.el\" 42
   :callback (lambda (data)
               (let* ((repo (alist-get \\='repository data))
                      (pr (alist-get \\='pullRequest repo))
                      (diff (alist-get \\='diff pr)))
                 (message \"PR: %s, Files changed: %d\"
                          (alist-get \\='title pr)
                          (length (alist-get \\='files diff))))))"
  (approve-api-graphql
   (approve-graphql-load "queries/get-pull-request.graphql")
   `((repo_owner . ,owner)
     (repo_name . ,repo)
     (pr_id . ,number))
   :callback (lambda (data)
               (if-let ((refs (approve-api--extract-pr-refs data)))
                   (approve-api--fetch-diff
                    owner repo (car refs) (cdr refs)
                    (lambda (diff-data)
                      (when callback
                        (funcall callback
                                 (approve-api--merge-diff-into-data data diff-data))))
                    error-callback
                    buffer)
                 ;; No refs found, return data without diff
                 (when callback
                   (funcall callback data))))
   :error-callback error-callback
   :buffer buffer
   :progress-message (format "Fetching PR #%d from %s/%s..." number owner repo)))

(cl-defun approve-api-search-pull-requests (query
                                                 &key
                                                 (limit 20)
                                                 callback
                                                 error-callback
                                                 (buffer (current-buffer)))
  "Search for pull requests on GitHub using QUERY.

QUERY is a GitHub search query string.  The function automatically
prepends \"is:pr\" if not already present.

Keyword arguments:
  :limit - Maximum number of results to return (default 20).
  :callback - Function called with the search results on success.
              Receives an alist with `search' containing the results.
  :error-callback - Function called with error info on failure.
  :buffer - Buffer context for callbacks (defaults to current buffer).

Returns a request ID that can be used with `approve-api-cancel'.

Example:
  (approve-api-search-pull-requests
   \"is:open review-requested:@me\"
   :limit 10
   :callback (lambda (data)
               (let* ((search (alist-get \\='search data))
                      (nodes (alist-get \\='nodes search)))
                 (message \"Found %d PRs\" (length nodes)))))"
  (let ((full-query (if (string-match-p "\\bis:pr\\b" query)
                        query
                      (concat "is:pr " query))))
    (approve-api-graphql
     (approve-graphql-load "queries/search-pull-requests.graphql")
     `((query . ,full-query)
       (first . ,limit))
     :callback callback
     :error-callback error-callback
     :buffer buffer
     :progress-message "Searching pull requests...")))

(provide 'approve-api-queries)
;;; approve-api-queries.el ends here

