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

;; This module provides the low-level API layer for communicating with GitHub.
;;
;; Key features:
;; - Asynchronous API requests using ghub
;; - Timeout handling for API calls
;; - Progress reporting using Emacs progress reporters
;; - Callbacks executed in the originating buffer's context
;;
;; All API calls use the `ghub' library for authentication and request handling.

;;; Code:

(require 'ghub)
(require 'cl-lib)
(require 'dash)

;;; Custom Variables

(defgroup approve-api nil
  "GitHub API settings for Approve."
  :group 'approve
  :prefix "approve-api-")

(defcustom approve-api-username nil
  "Username used by `approve', see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
                 (string :tag "Username value"))
  :group 'approve-api)

(defcustom approve-api-host nil
  "Host used by `approve', see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
                 (string :tag "Host value"))
  :group 'approve-api)

(defcustom approve-api-auth-name 'approve
  "Auth name used by `approve', see `ghub-request' for details."
  :type 'symbol
  :group 'approve-api)

(defcustom approve-api-timeout 30
  "Default timeout in seconds for API requests.
Set to nil to disable timeouts."
  :type '(choice (const :tag "No timeout" nil)
                 (integer :tag "Seconds"))
  :group 'approve-api)

;;; Error Handling

(define-error 'approve-api-error "Approve API error")
(define-error 'approve-api-timeout "API request timed out" 'approve-api-error)
(define-error 'approve-api-rate-limit "GitHub API rate limit exceeded" 'approve-api-error)
(define-error 'approve-api-not-found "Resource not found" 'approve-api-error)
(define-error 'approve-api-unauthorized "Unauthorized access" 'approve-api-error)
(define-error 'approve-api-graphql-error "GraphQL query error" 'approve-api-error)

;;; Internal Variables

(defvar approve-api--pending-requests (make-hash-table :test 'eq)
  "Hash table tracking pending API requests.
Keys are request IDs, values are plists with request metadata.")

(defvar approve-api--request-counter 0
  "Counter for generating unique request IDs.")

;;; Internal Functions

(defun approve-api--generate-request-id ()
  "Generate a unique request ID."
  (cl-incf approve-api--request-counter))

(defun approve-api--make-ghub-params ()
  "Create the common parameters for ghub requests."
  (let ((params nil))
    (when approve-api-username
      (setq params (plist-put params :username approve-api-username)))
    (when approve-api-host
      (setq params (plist-put params :host approve-api-host)))
    (when approve-api-auth-name
      (setq params (plist-put params :auth approve-api-auth-name)))
    params))

(defun approve-api--cleanup-request (request-id)
  "Clean up request with REQUEST-ID, canceling its timer if present."
  (-when-let (request-data (gethash request-id approve-api--pending-requests))
    (-when-let (timer (plist-get request-data :timer))
      (cancel-timer timer))
    (remhash request-id approve-api--pending-requests)))

(defun approve-api--handle-timeout (request-id buffer error-callback)
  "Handle timeout for request REQUEST-ID.
BUFFER is the originating buffer.
ERROR-CALLBACK is called with the timeout error."
  (approve-api--cleanup-request request-id)
  (when error-callback
    (approve-api--call-in-buffer
     buffer
     error-callback
     (cons 'approve-api-timeout (list "Request timed out")))))

(defun approve-api--call-in-buffer (buffer func &rest args)
  "Call FUNC with ARGS in the context of BUFFER.
If BUFFER is no longer live, call in a temp buffer instead."
  (if (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (apply func args))
    (apply func args)))

(defun approve-api--create-success-callback (request-id buffer callback progress-reporter)
  "Create a success callback for ghub.
REQUEST-ID identifies the request for cleanup.
BUFFER is the originating buffer.
CALLBACK is the user's success callback.
PROGRESS-REPORTER is finished on completion."
  (lambda (response &rest _)
    (approve-api--cleanup-request request-id)
    (when progress-reporter
      (progress-reporter-done progress-reporter))
    (when callback
      (approve-api--call-in-buffer buffer callback response))))

(defun approve-api--create-error-callback (request-id buffer error-callback progress-reporter)
  "Create an error callback for ghub.
REQUEST-ID identifies the request for cleanup.
BUFFER is the originating buffer.
ERROR-CALLBACK is the user's error callback.
PROGRESS-REPORTER is finished on error."
  (lambda (error &rest _)
    (approve-api--cleanup-request request-id)
    (when progress-reporter
      (progress-reporter-done progress-reporter))
    (when error-callback
      (let ((processed-error (approve-api--process-error error)))
        (approve-api--call-in-buffer buffer error-callback processed-error)))))

(defun approve-api--process-error (error)
  "Process ERROR from ghub into an approve-api error.
Returns a cons cell (error-symbol . error-data)."
  (cond
   ((and (listp error)
         (eq (car error) 'ghub-http-error)
         (= (cadr error) 403)
         (string-match-p "rate limit" (or (caddr error) "")))
    (cons 'approve-api-rate-limit (cdr error)))
   ((and (listp error)
         (eq (car error) 'ghub-http-error)
         (= (cadr error) 404))
    (cons 'approve-api-not-found (cdr error)))
   ((and (listp error)
         (eq (car error) 'ghub-http-error)
         (= (cadr error) 401))
    (cons 'approve-api-unauthorized (cdr error)))
   (t
    (cons 'approve-api-error (if (listp error) error (list error))))))

(defun approve-api-default-error-handler (error)
  "Default handler for API errors.
ERROR is a cons cell (error-type . error-data).
Displays an appropriate message based on the error type."
  (let ((error-type (car error))
        (error-data (cdr error)))
    (pcase error-type
      ('approve-api-not-found
       (message "Pull request not found"))
      ('approve-api-unauthorized
       (message "Unauthorized: Check your GitHub token"))
      ('approve-api-rate-limit
       (message "GitHub API rate limit exceeded"))
      ('approve-api-timeout
       (message "Request timed out"))
      ('approve-api-graphql-error
       (message "GraphQL error: %S" error-data))
      (_
       (message "API error: %S" error)))))

(defun approve-api--start-timeout-timer (request-id buffer error-callback timeout)
  "Start a timeout timer for request REQUEST-ID.
BUFFER is the originating buffer.
ERROR-CALLBACK is called on timeout.
TIMEOUT is the timeout in seconds.
Returns the timer object."
  (when timeout
    (run-at-time timeout nil
                 #'approve-api--handle-timeout
                 request-id buffer error-callback)))

(defun approve-api--extract-graphql-errors (response)
  "Extract GraphQL errors from RESPONSE if present.
Returns nil if no errors, or the error list if present."
  (alist-get 'errors response))

;;; Public API

(cl-defun approve-api-graphql (query variables
                                      &key
                                      callback
                                      error-callback
                                      (buffer (current-buffer))
                                      (progress-message "Contacting GitHub...")
                                      (timeout approve-api-timeout))
  "Execute a GraphQL QUERY with VARIABLES asynchronously.

QUERY is the GraphQL query string.
VARIABLES is an alist of variables to pass to the query.

Keyword arguments:
  :callback - Function called with the response data on success.
  :error-callback - Function called with error info on failure.
  :buffer - Buffer context for callbacks (defaults to current buffer).
  :progress-message - Message for progress reporter (nil to disable).
  :timeout - Timeout in seconds (defaults to `approve-api-timeout').

Both callbacks are called in the context of BUFFER (if still live).

Returns a request ID that can be used with `approve-api-cancel'."
  (let* ((request-id (approve-api--generate-request-id))
         (progress-reporter (when progress-message
                              (make-progress-reporter progress-message)))
         (effective-error-callback (or error-callback #'approve-api-default-error-handler))
         (ghub-params (approve-api--make-ghub-params))
         (success-cb (approve-api--create-graphql-success-callback
                      request-id buffer callback effective-error-callback progress-reporter))
         (error-cb (approve-api--create-error-callback
                    request-id buffer effective-error-callback progress-reporter))
         (timer (approve-api--start-timeout-timer
                 request-id buffer effective-error-callback timeout)))
    (puthash request-id
             (list :timer timer
                   :buffer buffer
                   :start-time (current-time))
             approve-api--pending-requests)
    (apply #'ghub-graphql query variables
           (append (list :callback success-cb
                         :errorback error-cb)
                   ghub-params))
    request-id))

(defun approve-api--create-graphql-success-callback (request-id buffer callback error-callback progress-reporter)
  "Create a success callback for GraphQL requests.
REQUEST-ID identifies the request for cleanup.
BUFFER is the originating buffer.
CALLBACK is the user's success callback.
ERROR-CALLBACK is called if GraphQL errors are present.
PROGRESS-REPORTER is finished on completion."
  (lambda (response &rest _)
    (approve-api--cleanup-request request-id)
    (when progress-reporter
      (progress-reporter-done progress-reporter))
    (-if-let (errors (approve-api--extract-graphql-errors response))
        (when error-callback
          (approve-api--call-in-buffer
           buffer error-callback
           (cons 'approve-api-graphql-error errors)))
      (when callback
        (-when-let (data (alist-get 'data response))
          (approve-api--call-in-buffer buffer callback data))))))

(cl-defun approve-api-rest (method resource
                                    &key
                                    payload
                                    query
                                    callback
                                    error-callback
                                    (buffer (current-buffer))
                                    (progress-message "Contacting GitHub...")
                                    (timeout approve-api-timeout))
  "Make a REST API request to GitHub.

METHOD is the HTTP method (GET, POST, PUT, PATCH, DELETE).
RESOURCE is the API endpoint path (e.g., \"/repos/owner/repo/pulls\").

Keyword arguments:
  :payload - Alist of data to send in request body.
  :query - Alist of query parameters.
  :callback - Function called with the response data on success.
  :error-callback - Function called with error info on failure.
  :buffer - Buffer context for callbacks (defaults to current buffer).
  :progress-message - Message for progress reporter (nil to disable).
  :timeout - Timeout in seconds (defaults to `approve-api-timeout').

Both callbacks are called in the context of BUFFER (if still live).

Returns a request ID that can be used with `approve-api-cancel'."
  (let* ((request-id (approve-api--generate-request-id))
         (progress-reporter (when progress-message
                              (make-progress-reporter progress-message)))
         (effective-error-callback (or error-callback #'approve-api-default-error-handler))
         (ghub-params (approve-api--make-ghub-params))
         (success-cb (approve-api--create-success-callback
                      request-id buffer callback progress-reporter))
         (error-cb (approve-api--create-error-callback
                    request-id buffer effective-error-callback progress-reporter))
         (timer (approve-api--start-timeout-timer
                 request-id buffer effective-error-callback timeout)))
    (puthash request-id
             (list :timer timer
                   :buffer buffer
                   :start-time (current-time))
             approve-api--pending-requests)
    (apply #'ghub-request method resource nil
           (append (when payload (list :payload payload))
                   (when query (list :query query))
                   (list :callback success-cb
                         :errorback error-cb)
                   ghub-params))
    request-id))

(defun approve-api-cancel (request-id)
  "Cancel a pending API request identified by REQUEST-ID.
Returns t if the request was found and canceled, nil otherwise."
  (when (gethash request-id approve-api--pending-requests)
    (approve-api--cleanup-request request-id)
    t))

(defun approve-api-cancel-all ()
  "Cancel all pending API requests.
Returns the number of requests canceled."
  (let ((count 0))
    (maphash (lambda (id _)
               (approve-api--cleanup-request id)
               (cl-incf count))
             approve-api--pending-requests)
    count))

(defun approve-api-pending-count ()
  "Return the number of pending API requests."
  (hash-table-count approve-api--pending-requests))

(provide 'approve-api)
;;; approve-api.el ends here
