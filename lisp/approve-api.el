;;; approve-api.el --- GitHub API communication for Approve  -*- lexical-binding: t; -*-

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

;; This module provides the API layer for communicating with GitHub.
;;
;; It handles all GitHub API interactions including:
;; - Fetching pull request data via GraphQL
;; - Submitting reviews and comments
;; - Updating PR state (approve, request changes, etc.)
;;
;; All API calls use the `ghub' library for authentication and request handling.

;;; Code:

(require 'ghub)
(require 'approve-graphql)

;;; Custom Variables

(defgroup approve-api nil
  "GitHub API settings for Approve."
  :group 'approve
  :prefix "approve-api-")

;;; Error Handling

(define-error 'approve-api-error "Approve API error")
(define-error 'approve-api-rate-limit "GitHub API rate limit exceeded" 'approve-api-error)
(define-error 'approve-api-not-found "Resource not found" 'approve-api-error)
(define-error 'approve-api-unauthorized "Unauthorized access" 'approve-api-error)

;;; Internal Functions

;;; Public API

(provide 'approve-api)
;;; approve-api.el ends here
