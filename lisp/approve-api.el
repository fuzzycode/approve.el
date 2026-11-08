;;; approve-api.el --- GitHub requests for Approve -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/fuzzycode/approve.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Every request Approve makes to GitHub goes through this file
;; (docs/decisions.org, D07): `approve-api-graphql', `approve-api-rest'
;; and the paging helpers `approve-api-graphql-all' and
;; `approve-api-rest-all'.  Mutations are queued by `approve-api-mutate'
;; in approve-api-queue.el (D15).
;;
;; Requests are asynchronous.  Each one has a timeout, shows progress
;; in the echo area, records GitHub's rate limit, and ends in exactly
;; one of its two continuations:
;;
;; - the callback, with the response's data (GraphQL) or decoded body
;;   (REST);
;; - the errorback, with a condition (CLASS MESSAGE . PROPS), where
;;   CLASS is one of the `approve-api-error' classes below.  Without an
;;   errorback, `approve-api-default-errorback' shows it.
;;
;; Both run in the buffer that made the request and are skipped when
;; that buffer has been killed.  An error they signal is logged and
;; never stops the bookkeeping (progress, timers, the queue).
;;
;; One HTTP exchange is done by the function in `approve-api-transport',
;; by default `approve-api-ghub-transport'.  Tests and the development
;; tools replace it; everything else works the same whatever transport
;; is installed.
;;
;; This file is the transport layer (docs/architecture.org, "Layers").
;; It is the only file that calls ghub.  It knows nothing about the
;; store, the content of buffers or rendering: it hands data to
;; callbacks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'parse-time)
(require 'ghub)
(require 'approve-log)
(require 'approve-graphql)

;;; Options

(defgroup approve-api nil
  "How Approve talks to GitHub."
  :group 'approve
  :prefix "approve-api-")

(defcustom approve-api-host nil
  "GitHub API host, or nil for ghub's default (\"api.github.com\").
For GitHub Enterprise, use the API host of the instance, for example:

  (setq approve-api-host \"git.example.com/api/v3\")"
  :type '(choice (const :tag "Ghub's default" nil)
                 (string :tag "API host"))
  :group 'approve-api)

(defcustom approve-api-auth 'approve
  "Name of the token Approve asks ghub for (ghub's AUTH argument).
Each package uses its own token, so by default ghub looks up the
token of \"USER^approve\" in auth-source.  To use Forge's token:

  (setq approve-api-auth \\='forge)"
  :type 'symbol
  :group 'approve-api)

(defcustom approve-api-username nil
  "GitHub user to make requests as, or nil to let ghub find it.
Ghub reads the Git variable `github.user' by default.  For example:

  (setq approve-api-username \"octocat\")"
  :type '(choice (const :tag "From Git config" nil)
                 (string :tag "User name"))
  :group 'approve-api)

(defcustom approve-api-timeout 30
  "Seconds to wait for a response, or nil to wait forever.
A request that times out reports `approve-api-timeout' to its
errorback, and a later response is ignored.  For a slow connection:

  (setq approve-api-timeout 60)"
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'approve-api)

(defconst approve-api-default-progress "Contacting GitHub"
  "Progress text of requests that do not give their own.")

;;; Errors

(define-error 'approve-api-error "GitHub request failed" 'approve-error)
(define-error 'approve-api-network-error "Network error" 'approve-api-error)
(define-error 'approve-api-timeout "GitHub did not answer in time"
  'approve-api-network-error)
(define-error 'approve-api-unauthorized "GitHub refused the token"
  'approve-api-error)
(define-error 'approve-api-not-found "Not found or no access" 'approve-api-error)
(define-error 'approve-api-rate-limited "GitHub rate limit reached"
  'approve-api-error)
(define-error 'approve-api-graphql-error "GitHub GraphQL error"
  'approve-api-error)
(define-error 'approve-api-http-error "GitHub HTTP error" 'approve-api-error)
(define-error 'approve-api-cancelled "Request cancelled" 'approve-api-error)

(defun approve-api--condition (class message &rest props)
  "Return the condition (CLASS MESSAGE . PROPS).
PROPS is a plist with any of :status, :reset and :errors."
  (cons class (cons message props)))

(defun approve-api-error-message (err)
  "Return the message of ERR, an error given to an errorback."
  (if (stringp (cadr err)) (cadr err) (error-message-string err)))

(defun approve-api-error-status (err)
  "Return the HTTP status of ERR, an integer, or nil if there was none."
  (plist-get (cddr err) :status))

(defun approve-api-error-reset (err)
  "Return when the rate limit of ERR resets, in seconds since the epoch.
Return nil unless ERR is `approve-api-rate-limited'."
  (plist-get (cddr err) :reset))

(defun approve-api-default-errorback (err)
  "Show ERR, an error given to an errorback, in the echo area.
This is the errorback of requests that have none of their own."
  (let ((text (approve-api-error-message err)))
    (pcase (car err)
      ('approve-api-rate-limited
       (message "Approve: %s (resets at %s)" text
                (format-time-string "%H:%M" (approve-api-error-reset err))))
      ('approve-api-unauthorized
       (message "Approve: %s; check the token ghub finds for `%s'"
                text approve-api-auth))
      (_ (message "Approve: %s" text)))))

;;; Transport

(defvar approve-api-transport #'approve-api-ghub-transport
  "Function that performs one HTTP exchange with GitHub.
It is called with one plist describing the request:

  :kind       `graphql' or `rest'.
  :name       GraphQL: the query file name (\"queries/x.graphql\"), or
              nil for a query given as text.  REST: nil.
  :query      GraphQL: the expanded query text.  REST: nil.
  :variables  GraphQL: the variables alist.  REST: nil.
  :method     \"POST\" for GraphQL, else the REST method.
  :resource   \"/graphql\", or the REST resource.
  :params     REST parameters alist (the query of a GET), or nil.
  :payload    the body alist: ((query . Q) (variables . V)) for
              GraphQL, the REST payload, or nil.
  :host       API host, or nil for ghub's default.
  :auth       ghub's AUTH (`approve-api-auth').
  :username   user name, or nil (`approve-api-username').
  :on-success function of (VALUE HEADERS): VALUE is the decoded JSON
              body (objects as alists with symbol keys, arrays as
              lists, null and false as nil); HEADERS is an alist of
              (NAME . VALUE) strings.
  :on-failure function of one plist:
                :status   HTTP status, or nil for a network error
                :payload  decoded error body, or nil
                :headers  response headers alist, or nil
                :error    for a network error, what went wrong

The transport calls exactly one of :on-success and :on-failure,
once, after it has returned.  It may return a function of no
arguments that aborts the exchange; a timeout calls it.  If the
transport signals, the request fails with `approve-api-error'.")

(defun approve-api--ghub-abort (buffer)
  "Abort the retrieval whose process buffer is BUFFER, quietly."
  (when (buffer-live-p buffer)
    (let (process)
      (while (setq process (get-buffer-process buffer))
        (set-process-sentinel process #'ignore)
        (ignore-errors (delete-process process))))
    (kill-buffer buffer)))

(defun approve-api--ghub-failure (err headers)
  "Return the failure plist for ghub's error ERR and HEADERS.
ERR is url's error with ghub's decoded payload appended, such as
\(error http 404 PAYLOAD)."
  (if (eq (car-safe (cdr-safe err)) 'http)
      (list :status (nth 2 err) :payload (nth 3 err) :headers headers)
    (list :error err :headers headers)))

(defun approve-api-ghub-transport (request)
  "Perform REQUEST with `ghub-request'; the default `approve-api-transport'.
REQUEST is the plist described there.  Return a function that aborts
the retrieval."
  (let* ((on-success (plist-get request :on-success))
         (on-failure (plist-get request :on-failure))
         (buffer (ghub-request (plist-get request :method)
                               (plist-get request :resource)
                               (plist-get request :params)
                               :payload (plist-get request :payload)
                               :host (plist-get request :host)
                               :auth (plist-get request :auth)
                               :username (plist-get request :username)
                               :silent t
                               :noerror t
                               :callback (lambda (value headers _status _req)
                                           (funcall on-success value headers))
                               :errorback (lambda (err headers _status _req)
                                            (funcall on-failure
                                                     (approve-api--ghub-failure
                                                      err headers))))))
    (when (bufferp buffer)
      (lambda () (approve-api--ghub-abort buffer)))))

;;; Requests

(cl-defstruct (approve-api--request
               (:constructor approve-api--request-create)
               (:copier nil))
  "One request, from its start to its single outcome.
SPEC is the plist for the transport, without the continuations."
  spec label buffer callback errorback progress timeout transport
  on-settle start timer abort settled cost)

(defvar approve-api--in-flight nil
  "Requests started and not yet settled, newest first.")

(defvar approve-api--progress-message nil
  "The progress message last shown, or nil.")

(defun approve-api-in-flight-count ()
  "Return the number of requests waiting for GitHub."
  (length approve-api--in-flight))

(defun approve-api--spec (request prop)
  "Return PROP of the transport spec of REQUEST."
  (plist-get (approve-api--request-spec request) prop))

(defun approve-api--show-progress ()
  "Show one progress message for the requests in flight that want one.
Once none is left, clear the echo area unless it shows something else."
  (let ((showing (seq-filter #'approve-api--request-progress
                             approve-api--in-flight))
        (message-log-max nil))
    (cond
     (showing
      (let ((text (approve-api--request-progress (car showing))))
        (setq approve-api--progress-message
              (if (cdr showing)
                  (message "%s... (%d requests)" text (length showing))
                (message "%s..." text)))))
     (approve-api--progress-message
      (when (equal (current-message) approve-api--progress-message)
        (message nil))
      (setq approve-api--progress-message nil)))))

(cl-defun approve-api--make-request (spec &key callback errorback buffer
                                          timeout progress on-settle)
  "Return a request for the transport SPEC.
CALLBACK, ERRORBACK, BUFFER, TIMEOUT and PROGRESS are as for
`approve-api-graphql'.  ON-SETTLE, if non-nil, is called with the
request, OK and the result after the continuation, even if that
signalled.  The transport is the current `approve-api-transport'."
  (approve-api--request-create
   :spec spec
   :label (or (plist-get spec :name)
              (if (eq (plist-get spec :kind) 'graphql)
                  "query"
                (format "%s %s" (plist-get spec :method)
                        (plist-get spec :resource))))
   :buffer (or buffer (current-buffer))
   :callback callback :errorback errorback
   :timeout timeout :progress progress
   :transport approve-api-transport
   :on-settle on-settle))

(defun approve-api--start (request)
  "Send REQUEST through its transport.  Return nil."
  (setf (approve-api--request-start request) (current-time))
  (push request approve-api--in-flight)
  (approve-api--show-progress)
  (when-let* ((timeout (approve-api--request-timeout request)))
    (setf (approve-api--request-timer request)
          (run-at-time timeout nil #'approve-api--time-out request)))
  (approve-log 'debug "%s %s started %S" (approve-api--spec request :kind)
               (approve-api--request-label request)
               (approve-api--spec request :variables))
  (condition-case err
      (let ((abort (funcall (approve-api--request-transport request)
                            (append (approve-api--request-spec request)
                                    (list :on-success
                                          (lambda (value headers)
                                            (approve-api--succeed
                                             request value headers))
                                          :on-failure
                                          (lambda (failure)
                                            (approve-api--fail
                                             request failure)))))))
        (when (functionp abort)
          (setf (approve-api--request-abort request) abort)))
    (error
     (run-at-time 0 nil #'approve-api--settle request nil
                  (approve-api--condition 'approve-api-error
                                          (error-message-string err)))))
  nil)

(defun approve-api--time-out (request)
  "Fail REQUEST, which took longer than its timeout, and abort it."
  (unless (approve-api--request-settled request)
    (approve-api--settle
     request nil
     (approve-api--condition 'approve-api-timeout
                             (format "No answer from GitHub in %s seconds"
                                     (approve-api--request-timeout request))))
    (when-let* ((abort (approve-api--request-abort request)))
      (condition-case err
          (funcall abort)
        (error (approve-log-condition "Aborting a request" err))))))

(defun approve-api--succeed (request value headers)
  "Settle REQUEST with the decoded VALUE and HEADERS of a response."
  (unless (approve-api--request-settled request)
    (let ((outcome
           (condition-case err
               (progn
                 (approve-api--record-rate-limit request headers)
                 (if (eq (approve-api--spec request :kind) 'graphql)
                     (approve-api--graphql-outcome request value)
                   (cons t value)))
             (error
              (approve-log-condition "Reading a response" err)
              (cons nil (approve-api--condition
                         'approve-api-error (error-message-string err)))))))
      (approve-api--settle request (car outcome) (cdr outcome)))))

(defun approve-api--fail (request failure)
  "Settle REQUEST with FAILURE, a plist from the transport."
  (unless (approve-api--request-settled request)
    (approve-api--record-rate-limit request (plist-get failure :headers))
    (approve-api--settle request nil (approve-api--classify failure))))

(defun approve-api--settle (request ok result)
  "End REQUEST: if OK, RESULT is its value, else its error.
Stop the timer and the progress, log, then run the continuation in
the request's buffer, and its settle hook even if that failed.  Do
nothing if REQUEST has ended already."
  (unless (approve-api--request-settled request)
    (setf (approve-api--request-settled request) t)
    (when-let* ((timer (approve-api--request-timer request)))
      (cancel-timer timer))
    (setq approve-api--in-flight (delq request approve-api--in-flight))
    (unwind-protect
        (progn
          (approve-api--show-progress)
          (approve-api--log-outcome request ok result)
          (approve-api--deliver request
                                (if ok
                                    (approve-api--request-callback request)
                                  (or (approve-api--request-errorback request)
                                      #'approve-api-default-errorback))
                                result))
      (when-let* ((on-settle (approve-api--request-on-settle request)))
        (funcall on-settle request ok result)))))

(defun approve-api--log-outcome (request ok result)
  "Log how REQUEST ended; OK and RESULT are as for `approve-api--settle'."
  (let ((ms (if-let* ((start (approve-api--request-start request)))
                (round (* 1000 (float-time (time-since start))))
              0))
        (kind (approve-api--spec request :kind))
        (label (approve-api--request-label request))
        (cost (approve-api--request-cost request)))
    (if ok
        (approve-log 'debug "%s %s done in %d ms%s" kind label ms
                     (if cost (format ", cost %s" cost) ""))
      (approve-log 'warn "%s %s failed in %d ms: %s: %s" kind label ms
                   (car result) (approve-api-error-message result)))))

(defun approve-api--deliver (request function argument)
  "Call FUNCTION with ARGUMENT in the buffer of REQUEST.
Skip it when the buffer has been killed.  Log an error it signals
and say so briefly; never let it escape into the transport."
  (let ((buffer (approve-api--request-buffer request))
        (label (approve-api--request-label request)))
    (cond
     ((null function))
     ((not (buffer-live-p buffer))
      (approve-log 'debug "%s: buffer killed, continuation skipped" label))
     (t
      (with-current-buffer buffer
        (condition-case err
            (funcall function argument)
          (error
           (approve-log-condition (format "Continuation of %s" label) err)
           (message "Approve: error after %s: %s (see %s)" label
                    (error-message-string err) approve-log-buffer-name))))))))

;;; Classifying responses

(defun approve-api--header (headers name)
  "Return the value of header NAME in HEADERS, ignoring case."
  (cdr (assoc-string name headers t)))

(defun approve-api--payload-message (payload)
  "Return the error message in PAYLOAD, a decoded body, or nil."
  (when (consp payload)
    (let ((text (alist-get 'message payload))
          (errors (alist-get 'errors payload)))
      (cond ((stringp text) text)
            (errors (mapconcat (lambda (e)
                                 (format "%s" (or (alist-get 'message e) e)))
                               errors "; "))))))

(defun approve-api--rate-limit-reset (status headers payload)
  "Return the reset time if STATUS, HEADERS and PAYLOAD mean a rate limit.
The time is in seconds since the epoch.  A primary limit is a 403 or
429 with no requests remaining; a secondary one is a 403 or 429 with
a Retry-After header or a message about a rate limit."
  (when (memq status '(403 429))
    (let ((remaining (approve-api--header headers "x-ratelimit-remaining"))
          (reset (approve-api--header headers "x-ratelimit-reset"))
          (retry-after (approve-api--header headers "retry-after"))
          (now (time-convert nil 'integer)))
      (cond
       ((and (equal remaining "0") reset) (string-to-number reset))
       (retry-after (+ now (string-to-number retry-after)))
       ((or (= status 429)
            (string-match-p "rate limit"
                            (or (approve-api--payload-message payload) "")))
        ;; Without a time, GitHub asks to wait at least a minute.
        (+ now 60))))))

(defun approve-api--network-message (err)
  "Return a short text for ERR, a network error from the transport.
For url's (error connection-failed \"failed with code 6\" :host H),
that is \"connection-failed failed with code 6\"."
  (let ((parts (if (eq (car-safe err) 'error) (cdr err) (ensure-list err))))
    (mapconcat (lambda (part) (format "%s" part))
               (seq-take-while (lambda (part) (not (keywordp part)))
                               (delq nil (copy-sequence parts)))
               " ")))

(defun approve-api--classify (failure)
  "Return the error condition for FAILURE, a plist from the transport."
  (let* ((status (plist-get failure :status))
         (payload (plist-get failure :payload))
         (text (or (approve-api--payload-message payload)
                   (format "HTTP status %s" status)))
         (reset (and status
                     (approve-api--rate-limit-reset
                      status (plist-get failure :headers) payload))))
    (cond
     ((null status)
      (approve-api--condition
       'approve-api-network-error
       (let ((message (approve-api--network-message (plist-get failure :error))))
         (if (string-empty-p message) "Network error" message))))
     ((= status 401)
      (approve-api--condition 'approve-api-unauthorized text :status status))
     (reset
      (approve-api--condition 'approve-api-rate-limited text
                              :status status :reset reset))
     ((= status 404)
      (approve-api--condition 'approve-api-not-found text :status status))
     (t
      (approve-api--condition 'approve-api-http-error text :status status)))))

(defun approve-api--graphql-outcome (request response)
  "Record the rate limit of RESPONSE to REQUEST; return (OK . RESULT).
RESULT is the response's data, or the error when OK is nil.  The
response fails when an error is NOT_FOUND or RATE_LIMITED or when it
has no data besides rateLimit.  Data that comes with other errors (a
field the token may not read) is a success; the errors are logged."
  (let* ((data (alist-get 'data response))
         (errors (alist-get 'errors response))
         (types (mapcar (lambda (e) (alist-get 'type e)) errors))
         (text (approve-api--payload-message response)))
    (approve-api--record-graphql-rate-limit request (alist-get 'rateLimit data))
    (cond
     ((member "NOT_FOUND" types)
      (cons nil (approve-api--condition 'approve-api-not-found text
                                        :errors errors)))
     ((member "RATE_LIMITED" types)
      (cons nil (approve-api--condition
                 'approve-api-rate-limited text :errors errors
                 :reset (or (plist-get (approve-api-rate-limit
                                        'graphql (approve-api--spec request :host))
                                       :reset)
                            (+ (time-convert nil 'integer) 60)))))
     ((not (seq-some (lambda (field)
                       (and (not (eq (car-safe field) 'rateLimit))
                            (cdr-safe field)))
                     data))
      (cons nil (approve-api--condition 'approve-api-graphql-error
                                        (or text "GitHub returned no data")
                                        :errors errors)))
     (t
      (when errors
        (approve-log 'warn "graphql %s: data came with errors: %s"
                     (approve-api--request-label request) text))
      (cons t data)))))

;;; Rate limits

(defvar approve-api--rate-limits (make-hash-table :test 'equal)
  "Rate limits by (HOST . KIND); see `approve-api-rate-limit'.")

(defun approve-api-rate-limit (&optional kind host)
  "Return what GitHub last said about the rate limit of KIND on HOST.
KIND is `graphql' (the default) or `rest'; HOST defaults to
`approve-api-host'.  The value is a plist with :limit, :remaining,
:used, :reset (seconds since the epoch), :cost (of the last GraphQL
query) and :updated (a time), or nil before the first response.

Example:
  (plist-get (approve-api-rate-limit) :remaining) => 4990"
  (gethash (cons (or host approve-api-host) (or kind 'graphql))
           approve-api--rate-limits))

(defun approve-api--update-rate-limit (request values)
  "Merge VALUES, a plist, into the rate limit of REQUEST's host and kind.
Nil values are ignored; nothing changes when all are nil."
  (let* ((key (cons (approve-api--spec request :host)
                    (approve-api--spec request :kind)))
         (record (gethash key approve-api--rate-limits))
         (changed nil))
    (cl-loop for (prop value) on values by #'cddr
             when value do (setq record (plist-put record prop value)
                                 changed t))
    (when changed
      (puthash key (plist-put record :updated (current-time))
               approve-api--rate-limits))))

(defun approve-api--record-rate-limit (request headers)
  "Record the x-ratelimit-* HEADERS of the response to REQUEST."
  (approve-api--update-rate-limit
   request
   (cl-loop for (prop . name) in '((:limit . "x-ratelimit-limit")
                                   (:remaining . "x-ratelimit-remaining")
                                   (:used . "x-ratelimit-used")
                                   (:reset . "x-ratelimit-reset"))
            for value = (approve-api--header headers name)
            append (list prop (and value (string-to-number value))))))

(defun approve-api--record-graphql-rate-limit (request rate)
  "Record RATE, the rateLimit of a GraphQL response to REQUEST.
It overrides what the headers said, and gives the request's cost."
  (when rate
    (let-alist rate
      (setf (approve-api--request-cost request) .cost)
      (approve-api--update-rate-limit
       request
       (list :cost .cost :remaining .remaining
             :reset (and .resetAt
                         (time-convert (parse-iso8601-time-string .resetAt)
                                       'integer)))))))

;;; Public requests

(defun approve-api--graphql-spec (query variables host)
  "Return the transport spec for QUERY with VARIABLES on HOST.
QUERY is a file name ending in .graphql or a query text."
  (let* ((name (and (string-suffix-p ".graphql" query)
                    (not (string-match-p "[{}[:space:]]" query))
                    query))
         (text (if name (approve-graphql-load name) query)))
    (list :kind 'graphql :name name :query text :variables variables
          :method "POST" :resource "/graphql" :params nil
          :payload `((query . ,text)
                     ,@(and variables `((variables . ,variables))))
          :host (or host approve-api-host)
          :auth approve-api-auth :username approve-api-username)))

(cl-defun approve-api-graphql (query variables
                                     &key callback errorback
                                     (buffer (current-buffer))
                                     (timeout approve-api-timeout)
                                     (progress approve-api-default-progress)
                                     host)
  "Run the GraphQL QUERY with VARIABLES asynchronously.
QUERY is a file name under `approve-graphql-directory' ending in
\".graphql\" (loaded with `approve-graphql-load') or a query text.
VARIABLES is an alist; nil is sent as false, so use :null for null.

CALLBACK is called with the response's data, such as
\((repository (pullRequest ...)) (rateLimit ...)).  ERRORBACK is
called with the error, a condition (CLASS MESSAGE . PROPS) where
CLASS is a child of `approve-api-error': read it with
`approve-api-error-message', `approve-api-error-status' and
`approve-api-error-reset', or `signal' it.  Without ERRORBACK,
`approve-api-default-errorback' shows the error.

A response with an error of type NOT_FOUND fails with
`approve-api-not-found', one of type RATE_LIMITED with
`approve-api-rate-limited', and one with errors and no data with
`approve-api-graphql-error'.  When data comes with other errors,
CALLBACK gets the data and the errors are logged.

Both run in BUFFER (the current buffer by default) and are skipped
if it has been killed.  TIMEOUT is in seconds, nil for none.
PROGRESS is the echo-area text while the request runs, nil for none.
HOST is the API host, `approve-api-host' by default.

Example:
  (approve-api-graphql \"queries/pull-request.graphql\"
                       \\='((owner . \"magit\") (name . \"magit\") (number . 5000))
                       :callback (lambda (data) (message \"%S\" data)))"
  (approve-api--start
   (approve-api--make-request (approve-api--graphql-spec query variables host)
                              :callback callback :errorback errorback
                              :buffer buffer :timeout timeout
                              :progress progress)))

(cl-defun approve-api-rest (method resource
                                   &key params payload callback errorback
                                   (buffer (current-buffer))
                                   (timeout approve-api-timeout)
                                   (progress approve-api-default-progress)
                                   host)
  "Make the REST request METHOD on RESOURCE asynchronously.
METHOD is a string such as \"GET\"; RESOURCE starts with a slash.
PARAMS is an alist sent as the query of a GET or HEAD and as the body
otherwise; PAYLOAD is an alist sent as the body.  CALLBACK is called
with the decoded body.  ERRORBACK, BUFFER, TIMEOUT, PROGRESS and HOST
are as for `approve-api-graphql'.

Example:
  (approve-api-rest \"POST\" \"/markdown\"
                    :payload \\='((text . \"**hi**\") (mode . \"gfm\"))
                    :callback (lambda (html) (message \"%s\" html)))"
  (approve-api--start
   (approve-api--make-request
    (list :kind 'rest :name nil :query nil :variables nil
          :method method :resource resource :params params :payload payload
          :host (or host approve-api-host)
          :auth approve-api-auth :username approve-api-username)
    :callback callback :errorback errorback
    :buffer buffer :timeout timeout :progress progress)))

;;; Paging

(cl-defun approve-api-graphql-all (query variables
                                         &key path on-page callback errorback
                                         (buffer (current-buffer))
                                         (timeout approve-api-timeout)
                                         (progress approve-api-default-progress)
                                         host max-pages)
  "Run the paged GraphQL QUERY with VARIABLES, page after page.
PATH lists the keys from the data to the paged connection, such as
\(repository pullRequest files); the connection selects
pageInfo { hasNextPage endCursor }.  QUERY takes the cursor in its
$after variable, which is left out for the first page.

ON-PAGE is called with each page's data and its number, from 1, as
it arrives.  CALLBACK is called with no arguments after the last
page: when hasNextPage is false or after MAX-PAGES pages (nil for no
limit).  An error stops the paging and goes to ERRORBACK.  If ON-PAGE
signals, the error is logged and the paging stops.  BUFFER, TIMEOUT,
PROGRESS and HOST are as for `approve-api-graphql'.

Example:
  (approve-api-graphql-all \"queries/pull-request-files.graphql\"
                           \\='((owner . \"o\") (name . \"r\") (number . 1))
                           :path \\='(repository pullRequest files)
                           :on-page (lambda (data page) (message \"%d\" page)))"
  (let ((variables (assq-delete-all 'after (copy-sequence variables))))
    (letrec ((fetch
              (lambda (page cursor)
                (approve-api-graphql
                 query (if cursor `((after . ,cursor) ,@variables) variables)
                 :errorback errorback :buffer buffer :timeout timeout
                 :progress progress :host host
                 :callback
                 (lambda (data)
                   (when on-page
                     (funcall on-page data page))
                   (let-alist (let ((connection data))
                                (dolist (key path connection)
                                  (setq connection (alist-get key connection))))
                     (if (and .pageInfo.hasNextPage .pageInfo.endCursor
                              (not (and max-pages (>= page max-pages))))
                         (funcall fetch (1+ page) .pageInfo.endCursor)
                       (when callback
                         (funcall callback)))))))))
      (funcall fetch 1 nil))))

(cl-defun approve-api-rest-all (resource
                                &key params (per-page 100) (max-pages 30)
                                on-page callback errorback
                                (buffer (current-buffer))
                                (timeout approve-api-timeout)
                                (progress approve-api-default-progress)
                                host)
  "GET the paged REST list RESOURCE, page after page.
PARAMS is an alist of query parameters, to which per_page (PER-PAGE)
and page (from 1) are added.  ON-PAGE is called with each page's
list and its number as it arrives.  CALLBACK is called with no
arguments after the last page: one shorter than PER-PAGE, or page
MAX-PAGES.  The defaults fit the pull request files list: 100 per
page and at most 30 pages (3000 files).  ERRORBACK, BUFFER, TIMEOUT,
PROGRESS and HOST are as for `approve-api-graphql'.

Example:
  (approve-api-rest-all \"/repos/magit/magit/pulls/5000/files\"
                        :on-page (lambda (files page) (message \"%d\" page)))"
  (letrec ((fetch
            (lambda (page)
              (approve-api-rest
               "GET" resource
               :params `((per_page . ,per-page) (page . ,page) ,@params)
               :errorback errorback :buffer buffer :timeout timeout
               :progress progress :host host
               :callback
               (lambda (value)
                 (when on-page
                   (funcall on-page value page))
                 (if (and (>= (length value) per-page)
                          (not (and max-pages (>= page max-pages))))
                     (funcall fetch (1+ page))
                   (when callback
                     (funcall callback))))))))
    (funcall fetch 1)))

;;; Hosts

(defun approve-api-host-from-web-host (web-host)
  "Return the API host for WEB-HOST, the host of a pull request URL.

Example:
  (approve-api-host-from-web-host \"github.com\") => \"api.github.com\"
  (approve-api-host-from-web-host \"git.example.com\")
  => \"git.example.com/api/v3\""
  (let ((host (downcase web-host)))
    (if (member host '("github.com" "www.github.com"))
        "api.github.com"
      (concat host "/api/v3"))))

(provide 'approve-api)

;;; approve-api.el ends here
