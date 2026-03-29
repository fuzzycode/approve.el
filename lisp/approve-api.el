;;; approve-api.el --- GitHub API communication layer for Approve -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson
;;
;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/bjornlarsson/approve
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghub "5.0"))

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

;; Unified API layer for GitHub communication.  Provides internal
;; GraphQL and REST dispatch functions that wrap `ghub' with a
;; simplified callback convention, progress reporting, and error
;; handling.
;;
;; Callers pass:
;;   :on-success (lambda (data) ...)
;;   :on-error   (lambda (error-message status-code) ...)
;;
;; If no :on-error is provided, a default handler logs the error
;; and shows a message to the user.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'ghub)
(require 'approve-log)

(defvar url-retrieve-timeout)

;;; Customization

(defcustom approve-ghub-username nil
  "Ghub username used by `approve', see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
          (string :tag "Username value"))
  :group 'approve)

(defcustom approve-ghub-host nil
  "Ghub host used by `approve', see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
          (string :tag "Host value"))
  :group 'approve)

(defcustom approve-ghub-auth-name 'approve
  "Ghub auth name used by `approve', see `ghub-request' for details."
  :type 'symbol
  :group 'approve)

(defcustom approve-api-timeout 30
  "Timeout in seconds for API requests.
Set to nil to disable timeout."
  :type '(choice integer (const nil))
  :group 'approve)

;;; Error handling

(defun approve-api--default-error-handler (error-message status-code)
  "Default error handler for API calls.
Shows ERROR-MESSAGE to the user and logs it with STATUS-CODE."
  (approve-log--message "API error (status %s): %s" status-code error-message)
  (message "[approve] API error: %s" error-message))

(defun approve-api--extract-error (err)
  "Extract a human-readable message and status code from ERR.
Returns a cons cell (MESSAGE . STATUS-CODE)."
  (cond
   ((stringp err)
    (cons err nil))
   ((and (listp err) (stringp (car err)))
    (cons (car err) (cadr err)))
   ((and (listp err) (keywordp (car err)))
    (let ((msg (or (plist-get err :message)
                   (format "%S" err)))
          (code (plist-get err :code)))
      (cons msg code)))
   (t
    (cons (format "%S" err) nil))))

;;; Callback wrapping

(defun approve-api--wrap-success (on-success reporter)
  "Wrap ON-SUCCESS callback, cleaning up REPORTER on completion.
Returns a ghub-compatible callback function."
  (lambda (value _headers _status _req)
    (when reporter
      (progress-reporter-done reporter))
    (when on-success
      (funcall on-success value))))

(defun approve-api--wrap-error (on-error reporter)
  "Wrap ON-ERROR callback, cleaning up REPORTER on failure.
Falls back to `approve-api--default-error-handler' if ON-ERROR is nil.
Returns a ghub-compatible errorback function."
  (lambda (err _headers _status _req)
    (when reporter
      (progress-reporter-done reporter))
    (let* ((extracted (approve-api--extract-error err))
           (message (car extracted))
           (code (cdr extracted))
           (handler (or on-error #'approve-api--default-error-handler)))
      (funcall handler message code))))

;;; Core dispatch

(cl-defun approve-api--graphql (query variables
                                &key on-success on-error
                                host
                                (message "Working..."))
  "Execute a GraphQL QUERY with VARIABLES against the GitHub API.

QUERY is a raw GraphQL query string.
VARIABLES is an alist of query variables.
HOST overrides `approve-ghub-host' for this request.
MESSAGE is shown in the minibuffer during the request.

Callbacks:
  ON-SUCCESS — called with the parsed response data.
  ON-ERROR — called with (error-message status-code).
              Falls back to default handler if nil."
  (let* ((reporter (make-progress-reporter message))
         (url-retrieve-timeout approve-api-timeout))
    (approve-log--message "GraphQL request to %s" (or host approve-ghub-host "default"))
    (condition-case err
        (ghub-request "POST" "/graphql"
                      nil
                      :payload `((query . ,query)
                                 ,@(and variables `((variables ,@variables))))
                      :noerror t
                      :silent t
                      :host (or host approve-ghub-host)
                      :auth approve-ghub-auth-name
                      :username approve-ghub-username
                      :callback (approve-api--wrap-success on-success reporter)
                      :errorback (approve-api--wrap-error on-error reporter))
      (error
       (when reporter
         (progress-reporter-done reporter))
       (let* ((extracted (approve-api--extract-error (cdr err)))
              (msg (car extracted))
              (code (cdr extracted))
              (handler (or on-error #'approve-api--default-error-handler)))
         (funcall handler msg code))))))

(cl-defun approve-api--rest (method resource
                             &key params payload
                             on-success on-error
                             host
                             (message "Working..."))
  "Execute a REST API call with METHOD on RESOURCE.

METHOD is an HTTP method string (\"GET\", \"POST\", \"PUT\", \"PATCH\", \"DELETE\").
RESOURCE is the API path (e.g., \"/repos/owner/repo/pulls\").
PARAMS is an alist of query parameters.
PAYLOAD is an alist for the request body.
HOST overrides `approve-ghub-host' for this request.
MESSAGE is shown in the minibuffer during the request.

Callbacks:
  ON-SUCCESS — called with the parsed response data.
  ON-ERROR — called with (error-message status-code).
              Falls back to default handler if nil."
  (let* ((reporter (make-progress-reporter message))
         (url-retrieve-timeout approve-api-timeout))
    (approve-log--message "REST %s %s on %s" method resource (or host approve-ghub-host "default"))
    (condition-case err
        (ghub-request method resource params
                      :payload payload
                      :noerror t
                      :silent t
                      :host (or host approve-ghub-host)
                      :auth approve-ghub-auth-name
                      :username approve-ghub-username
                      :callback (approve-api--wrap-success on-success reporter)
                      :errorback (approve-api--wrap-error on-error reporter))
      (error
       (when reporter
         (progress-reporter-done reporter))
       (let* ((extracted (approve-api--extract-error (cdr err)))
              (msg (car extracted))
              (code (cdr extracted))
              (handler (or on-error #'approve-api--default-error-handler)))
         (funcall handler msg code))))))

(provide 'approve-api)

;;; approve-api.el ends here
