;;; approve-api-mutations.el --- GraphQL mutation functions for Approve  -*- lexical-binding: t; -*-

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

;; This module provides mutating GraphQL operations for Approve.
;;
;; Key responsibilities:
;; - Submitting PR reviews (approve, request changes, comment)
;; - Adding and editing comments
;; - Resolving and unresolving review threads
;; - Merging and closing pull requests
;; - Managing PR labels and assignees
;;
;; All functions in this module modify state on GitHub.  For read-only
;; operations, see `approve-api-queries.el'.
;;
;; Mutation responses are designed to be compatible with `approve-model-patch'
;; for seamless store updates after successful mutations.

;;; Code:

(require 'cl-lib)

(require 'approve-api)
(require 'approve-graphql)

;;; Pull Request Mutations

(cl-defun approve-api-mutation-update-pr-title (pr-id title
                                                       &key
                                                       on-success
                                                       on-error
                                                       (buffer (current-buffer)))
  "Update the title of a pull request.

PR-ID is the GraphQL node ID of the pull request.
TITLE is the new title string.

Keyword arguments:
  :on-success - Function called with mutation response data on success.
                Receives the `updatePullRequest' response which includes
                the updated pull request fields.
  :on-error - Function called with error info on failure.
  :buffer - Buffer context for callbacks (defaults to current buffer).

Returns a request ID that can be used with `approve-api-cancel'."
  (approve-api-graphql
   (approve-graphql-load "mutations/update-pull-request-title.graphql")
   `((pullRequestId . ,pr-id)
     (title . ,title))
   :callback on-success
   :error-callback on-error
   :buffer buffer
   :progress-message "Updating PR title..."))

(provide 'approve-api-mutations)
;;; approve-api-mutations.el ends here
