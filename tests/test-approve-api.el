;;; test-approve-api.el --- Tests for approve-api -*- lexical-binding: t; -*-

;;; Commentary:

;; Requests through a fake transport installed in
;; `approve-api-transport': continuations in the origin buffer,
;; killed buffers, timeouts, error classes, GraphQL errors, rate
;; limits, progress, errors in callbacks and paging.  The default
;; transport is tested with a spy on `ghub-request'.

;;; Code:

(require 'buttercup)
(require 'approve-api)

;;; Fake transport

(defvar test-approve-api--sent nil
  "Request plists the fake transport received, newest first.")

(defvar test-approve-api--abort-calls 0
  "How often an abort function returned by the fake transport ran.")

(defun test-approve-api--transport (request)
  "Fake `approve-api-transport': remember REQUEST, answer nothing yet."
  (push request test-approve-api--sent)
  (lambda () (setq test-approve-api--abort-calls (1+ test-approve-api--abort-calls))))

(defun test-approve-api--succeed (value &optional headers request)
  "Answer REQUEST (default: the newest) with VALUE and HEADERS."
  (funcall (plist-get (or request (car test-approve-api--sent)) :on-success)
           value headers))

(defun test-approve-api--fail (failure &optional request)
  "Fail REQUEST (default: the newest) with the FAILURE plist."
  (funcall (plist-get (or request (car test-approve-api--sent)) :on-failure)
           failure))

(defun test-approve-api--error (failure)
  "Return the error an errorback receives for the transport FAILURE."
  (let (received)
    (approve-api-rest "GET" "/x" :errorback (lambda (err) (setq received err)))
    (test-approve-api--fail failure)
    received))

(defun test-approve-api--log ()
  "Return the text of the log buffer, or \"\"."
  (if-let* ((buffer (get-buffer approve-log-buffer-name)))
      (with-current-buffer buffer (buffer-string))
    ""))

(defmacro test-approve-api--with-fake (&rest body)
  "Run BODY with the fake transport and fresh request state."
  (declare (indent 0))
  `(let ((approve-api-transport #'test-approve-api--transport)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (approve-api--in-flight nil)
         (approve-api--progress-message nil)
         (approve-api--rate-limits (make-hash-table :test 'equal))
         (test-approve-api--sent nil)
         (test-approve-api--abort-calls 0))
     ,@body))

(defun test-approve-api--quiet ()
  "Keep progress and error messages out of the test output."
  (spy-on 'message :and-call-fake
          (lambda (format-string &rest args)
            (and format-string (apply #'format-message format-string args)))))

(describe "approve-api-graphql"
  (before-each (test-approve-api--quiet))

  (it "sends a query file expanded, with its name and variables"
    (let* ((directory (make-temp-file "approve-api-graphql-" t))
           (approve-graphql-directory directory))
      (unwind-protect
          (test-approve-api--with-fake
            (make-directory (expand-file-name "fragments" directory))
            (make-directory (expand-file-name "queries" directory))
            (with-temp-file (expand-file-name "fragments/user.graphql" directory)
              (insert "fragment UserFields on User { login }"))
            (with-temp-file (expand-file-name "queries/viewer.graphql" directory)
              (insert "${include:fragments/user.graphql}\nquery { viewer { ...UserFields } }"))
            (approve-api-graphql "queries/viewer.graphql" '((number . 1)))
            (let ((sent (car test-approve-api--sent))
                  (text "fragment UserFields on User { login }\nquery { viewer { ...UserFields } }"))
              (expect (plist-get sent :kind) :to-be 'graphql)
              (expect (plist-get sent :name) :to-equal "queries/viewer.graphql")
              (expect (plist-get sent :query) :to-equal text)
              (expect (plist-get sent :variables) :to-equal '((number . 1)))
              (expect (plist-get sent :method) :to-equal "POST")
              (expect (plist-get sent :resource) :to-equal "/graphql")
              (expect (plist-get sent :payload)
                      :to-equal `((query . ,text) (variables . ((number . 1)))))
              (expect (plist-get sent :auth) :to-be 'approve)))
        (delete-directory directory t))))

  (it "sends a query text as it is, without a name or empty variables"
    (test-approve-api--with-fake
      (approve-api-graphql "query { viewer { login } }" nil)
      (let ((sent (car test-approve-api--sent)))
        (expect (plist-get sent :name) :to-be nil)
        (expect (plist-get sent :payload)
                :to-equal '((query . "query { viewer { login } }"))))))

  (it "uses the host given, else approve-api-host"
    (test-approve-api--with-fake
      (let ((approve-api-host "git.example.com/api/v3"))
        (approve-api-graphql "query { a }" nil)
        (expect (plist-get (car test-approve-api--sent) :host)
                :to-equal "git.example.com/api/v3")
        (approve-api-graphql "query { a }" nil :host "other/api/v3")
        (expect (plist-get (car test-approve-api--sent) :host)
                :to-equal "other/api/v3"))))

  (it "calls back with the data, in the buffer that made the request"
    (test-approve-api--with-fake
      (let ((origin (generate-new-buffer " origin"))
            received in-buffer)
        (unwind-protect
            (progn
              (with-current-buffer origin
                (approve-api-graphql "query { a }" nil
                                     :callback (lambda (data)
                                                 (setq received data
                                                       in-buffer (current-buffer)))))
              (with-temp-buffer
                (test-approve-api--succeed '((data . ((viewer . ((login . "me"))))))))
              (expect received :to-equal '((viewer . ((login . "me")))))
              (expect in-buffer :to-be origin)
              (expect (approve-api-in-flight-count) :to-equal 0))
          (kill-buffer origin)))))

  (it "skips the callback and errorback when the buffer was killed, but still settles"
    (test-approve-api--with-fake
      (let ((called nil))
        (with-temp-buffer
          (approve-api-graphql "query { a }" nil
                               :callback (lambda (_) (setq called t))
                               :errorback (lambda (_) (setq called t)))
          (approve-api-graphql "query { b }" nil
                               :callback (lambda (_) (setq called t))
                               :errorback (lambda (_) (setq called t))))
        (expect (approve-api-in-flight-count) :to-equal 2)
        (test-approve-api--succeed '((data . ((a . 1)))) nil (cadr test-approve-api--sent))
        (test-approve-api--fail '(:status 500) (car test-approve-api--sent))
        (expect called :to-be nil)
        (expect (approve-api-in-flight-count) :to-equal 0))))

  (it "fails with approve-api-graphql-error and GitHub's messages when there is no data"
    (test-approve-api--with-fake
      (let (received)
        (approve-api-graphql "query { a }" nil :errorback (lambda (err) (setq received err)))
        (test-approve-api--succeed
         '((data . nil)
           (errors . (((message . "Field 'x' doesn't exist"))
                      ((message . "Variable $y is unused"))))))
        (expect (car received) :to-be 'approve-api-graphql-error)
        (expect (approve-api-error-message received)
                :to-equal "Field 'x' doesn't exist; Variable $y is unused"))))

  (it "fails when the data holds only the rate limit"
    (test-approve-api--with-fake
      (let (received)
        (approve-api-graphql "query { a }" nil :errorback (lambda (err) (setq received err)))
        (test-approve-api--succeed
         '((data . ((repository . nil) (rateLimit . ((cost . 1)))))
           (errors . (((type . "FORBIDDEN") (message . "No access"))))))
        (expect (car received) :to-be 'approve-api-graphql-error))))

  (it "maps a NOT_FOUND error to approve-api-not-found even with partial data"
    (test-approve-api--with-fake
      (let (received)
        (approve-api-graphql "query { a }" nil :errorback (lambda (err) (setq received err)))
        (test-approve-api--succeed
         '((data . ((repository . ((pullRequest . nil)))))
           (errors . (((type . "NOT_FOUND")
                       (message . "Could not resolve to a PullRequest with the number of 9."))))))
        (expect (car received) :to-be 'approve-api-not-found)
        (expect (approve-api-error-message received) :to-match "Could not resolve"))))

  (it "maps a RATE_LIMITED error to approve-api-rate-limited with the reset time"
    (test-approve-api--with-fake
      (let (received)
        (approve-api-graphql "query { a }" nil :errorback (lambda (err) (setq received err)))
        (test-approve-api--succeed
         '((data . nil) (errors . (((type . "RATE_LIMITED") (message . "API rate limit exceeded")))))
         '(("x-ratelimit-remaining" . "0") ("x-ratelimit-reset" . "1900000000")))
        (expect (car received) :to-be 'approve-api-rate-limited)
        (expect (approve-api-error-reset received) :to-equal 1900000000))))

  (it "delivers data that comes with other errors and logs the errors"
    (test-approve-api--with-fake
      (let ((approve-log-level 'warn)
            received errored)
        (approve-api-graphql "query { a }" nil
                             :callback (lambda (data) (setq received data))
                             :errorback (lambda (err) (setq errored err)))
        (test-approve-api--succeed
         '((data . ((repository . ((name . "r")))))
           (errors . (((type . "FORBIDDEN") (message . "Resource protected by SSO"))))))
        (expect received :to-equal '((repository . ((name . "r")))))
        (expect errored :to-be nil)
        (expect (test-approve-api--log) :to-match "Resource protected by SSO"))))

  (it "records the rate limit of the response, and logs the cost"
    (test-approve-api--with-fake
      (let ((approve-log-level 'debug))
        (approve-api-graphql "query { a }" nil)
        (test-approve-api--succeed
         '((data . ((a . 1)
                    (rateLimit . ((cost . 3) (remaining . 4990)
                                  (resetAt . "2030-01-01T00:00:00Z"))))))
         '(("X-RateLimit-Limit" . "5000") ("X-RateLimit-Remaining" . "4991")))
        (let ((rate (approve-api-rate-limit)))
          (expect (plist-get rate :cost) :to-equal 3)
          (expect (plist-get rate :remaining) :to-equal 4990)
          (expect (plist-get rate :limit) :to-equal 5000)
          (expect (plist-get rate :reset)
                  :to-equal (time-convert (encode-time '(0 0 0 1 1 2030 nil nil t)) 'integer)))
        (expect (approve-api-rate-limit 'rest) :to-be nil)
        (expect (test-approve-api--log) :to-match "graphql query done in [0-9]+ ms, cost 3"))))

  (it "ignores a second answer from a transport"
    (test-approve-api--with-fake
      (let ((calls 0))
        (approve-api-graphql "query { a }" nil
                             :callback (lambda (_) (setq calls (1+ calls)))
                             :errorback (lambda (_) (setq calls (1+ calls))))
        (test-approve-api--succeed '((data . ((a . 1)))))
        (test-approve-api--succeed '((data . ((a . 1)))))
        (test-approve-api--fail '(:status 500))
        (expect calls :to-equal 1)))))

(describe "approve-api error classes"
  (before-each (test-approve-api--quiet))

  (it "classifies a network failure"
    (test-approve-api--with-fake
      (let ((err (test-approve-api--error
                  '(:error (error connection-failed "failed with code 6\n"
                                  :host "api.github.com" :service 443)))))
        (expect (car err) :to-be 'approve-api-network-error)
        (expect (approve-api-error-message err)
                :to-equal "connection-failed: failed with code 6")
        (expect (approve-api-error-status err) :to-be nil))))

  (it "classifies 401 as unauthorized with GitHub's message"
    (test-approve-api--with-fake
      (let ((err (test-approve-api--error
                  '(:status 401 :payload ((message . "Bad credentials"))))))
        (expect (car err) :to-be 'approve-api-unauthorized)
        (expect (approve-api-error-message err) :to-equal "Bad credentials")
        (expect (approve-api-error-status err) :to-equal 401))))

  (it "classifies 404 as not found"
    (test-approve-api--with-fake
      (expect (car (test-approve-api--error '(:status 404 :payload ((message . "Not Found")))))
              :to-be 'approve-api-not-found)))

  (it "classifies a primary rate limit with its reset time"
    (test-approve-api--with-fake
      (let ((err (test-approve-api--error
                  '(:status 403 :payload ((message . "API rate limit exceeded"))
                    :headers (("x-ratelimit-remaining" . "0")
                              ("x-ratelimit-reset" . "1900000000"))))))
        (expect (car err) :to-be 'approve-api-rate-limited)
        (expect (approve-api-error-reset err) :to-equal 1900000000)
        (expect (plist-get (approve-api-rate-limit 'rest) :remaining) :to-equal 0))))

  (it "classifies a secondary rate limit, with Retry-After or by its message"
    (test-approve-api--with-fake
      (let ((now (time-convert nil 'integer))
            (retry (test-approve-api--error
                    '(:status 403 :headers (("Retry-After" . "30")))))
            (text (test-approve-api--error
                   '(:status 403 :payload ((message . "You have exceeded a secondary rate limit")))))
            (too-many (test-approve-api--error '(:status 429))))
        (dolist (err (list retry text too-many))
          (expect (car err) :to-be 'approve-api-rate-limited))
        (expect (approve-api-error-reset retry) :to-be-close-to (+ now 30) -1)
        (expect (approve-api-error-reset text) :to-be-close-to (+ now 60) -1))))

  (it "classifies other statuses as approve-api-http-error"
    (test-approve-api--with-fake
      (let ((forbidden (test-approve-api--error
                        '(:status 403 :payload ((message . "Must have admin rights"))
                          :headers (("x-ratelimit-remaining" . "4000")))))
            (server (test-approve-api--error '(:status 502))))
        (expect (car forbidden) :to-be 'approve-api-http-error)
        (expect (approve-api-error-message forbidden) :to-equal "Must have admin rights")
        (expect (car server) :to-be 'approve-api-http-error)
        (expect (approve-api-error-status server) :to-equal 502)
        (expect (approve-api-error-message server) :to-equal "HTTP status 502"))))

  (it "gives errors that can be signalled and caught as approve-error"
    (test-approve-api--with-fake
      (let ((err (test-approve-api--error '(:status 404))))
        (expect (signal (car err) (cdr err)) :to-throw 'approve-api-error)
        (expect (signal (car err) (cdr err)) :to-throw 'approve-error))))

  (it "fails with approve-api-error, later, when the transport signals"
    (let ((approve-api-transport (lambda (_) (error "Required Github token does not exist")))
          (approve-api-timeout nil)
          received)
      (approve-api-rest "GET" "/x" :errorback (lambda (err) (setq received err)))
      (expect received :to-be nil)
      (sleep-for 0.02)
      (expect (car received) :to-be 'approve-api-error)
      (expect (approve-api-error-message received) :to-match "token does not exist")))

  (it "fails with approve-api-network-error when the connection cannot be made at once"
    (let ((approve-api-timeout nil)
          (received nil))
      (dolist (condition '((file-error "make client process failed"
                                       "nodename nor servname provided, or not known"
                                       :name "api.github.com")
                           (error "Could not create connection to api.github.com:443")))
        (let ((approve-api-transport (lambda (_) (signal (car condition) (cdr condition)))))
          (approve-api-rest "GET" "/x" :errorback (lambda (err) (push err received)))))
      (sleep-for 0.02)
      (expect (mapcar #'car received)
              :to-equal '(approve-api-network-error approve-api-network-error))
      (expect (approve-api-error-message (cadr received))
              :to-equal "make client process failed: nodename nor servname provided, or not known")
      (expect (approve-api-error-message (car received))
              :to-equal "Could not create connection to api.github.com:443")))

  (it "cancels the request and lets a quit through when the user quits in the transport"
    (let ((approve-api-transport (lambda (_) (signal 'quit nil)))
          (approve-api-timeout 30)
          (approve-api--in-flight nil)
          received)
      (expect (condition-case nil
                  (approve-api-rest "GET" "/x" :errorback (lambda (err) (setq received err)))
                (quit 'quit))
              :to-be 'quit)
      (expect (car received) :to-be 'approve-api-cancelled)
      (expect (approve-api-in-flight-count) :to-equal 0)))

  (it "shows the error in the echo area when there is no errorback"
    (test-approve-api--with-fake
      (approve-api-rest "GET" "/x")
      (test-approve-api--fail '(:status 401 :payload ((message . "Bad credentials"))))
      (expect 'message :to-have-been-called-with
              "Approve: %s; check the token ghub finds for `%s'"
              "Bad credentials" 'approve))))

(describe "approve-api timeouts"
  (before-each (test-approve-api--quiet))

  (it "fails once with approve-api-timeout, aborts, and ignores a late answer"
    (test-approve-api--with-fake
      (let ((errors nil)
            (answers 0))
        (approve-api-rest "GET" "/slow" :timeout 0.01
                          :callback (lambda (_) (setq answers (1+ answers)))
                          :errorback (lambda (err) (push err errors)))
        (sleep-for 0.05)
        (expect (length errors) :to-equal 1)
        (expect (car (car errors)) :to-be 'approve-api-timeout)
        (expect test-approve-api--abort-calls :to-equal 1)
        (expect (approve-api-in-flight-count) :to-equal 0)
        (test-approve-api--succeed '((late . t)))
        (test-approve-api--fail '(:status 500))
        (expect answers :to-equal 0)
        (expect (length errors) :to-equal 1))))

  (it "cancels the timer when the answer comes first"
    (test-approve-api--with-fake
      (let ((errors nil))
        (approve-api-rest "GET" "/fast" :timeout 0.02
                          :errorback (lambda (err) (push err errors)))
        (test-approve-api--succeed '((ok . t)))
        (sleep-for 0.05)
        (expect errors :to-be nil)
        (expect test-approve-api--abort-calls :to-equal 0)))))

(describe "approve-api errors in continuations"
  (before-each (test-approve-api--quiet))

  (it "logs an error in a callback, says so, and still settles"
    (test-approve-api--with-fake
      (let ((approve-log-level 'error))
        (approve-api-rest "GET" "/x" :progress "Loading"
                          :callback (lambda (_) (error "Bug in callback")))
        (expect (approve-api-in-flight-count) :to-equal 1)
        (expect (test-approve-api--succeed '((ok . t))) :not :to-throw)
        (expect (approve-api-in-flight-count) :to-equal 0)
        (expect (test-approve-api--log) :to-match "Bug in callback")
        (expect 'message :to-have-been-called-with
                "Approve: error after %s: %s (see %s)"
                "GET /x" "Bug in callback" approve-log-buffer-name))))

  (it "logs an error in an errorback too"
    (test-approve-api--with-fake
      (let ((approve-log-level 'error))
        (approve-api-rest "GET" "/x" :errorback (lambda (_) (error "Bug in errorback")))
        (expect (test-approve-api--fail '(:status 500)) :not :to-throw)
        (expect (test-approve-api--log) :to-match "Bug in errorback")
        (expect (approve-api-in-flight-count) :to-equal 0)))))

(describe "approve-api progress"
  (before-each (test-approve-api--quiet))

  (it "shows one message counting the requests in flight, then clears it"
    (test-approve-api--with-fake
      (spy-on 'current-message :and-call-fake (lambda () approve-api--progress-message))
      (approve-api-rest "GET" "/a" :progress "Loading pull request")
      (expect 'message :to-have-been-called-with "%s..." "Loading pull request")
      (approve-api-rest "GET" "/b" :progress "Loading pull request")
      (expect 'message :to-have-been-called-with
              "%s... (%d requests)" "Loading pull request" 2)
      (test-approve-api--succeed nil nil (cadr test-approve-api--sent))
      (expect (spy-calls-args-for 'message (1- (spy-calls-count 'message)))
              :to-equal '("%s..." "Loading pull request"))
      (test-approve-api--succeed nil nil (car test-approve-api--sent))
      (expect 'message :to-have-been-called-with nil)
      (expect approve-api--progress-message :to-be nil)))

  (it "leaves the echo area alone when progress is nil"
    (test-approve-api--with-fake
      (approve-api-rest "GET" "/a" :progress nil)
      (test-approve-api--succeed nil)
      (expect 'message :not :to-have-been-called))))

(describe "approve-api-rest"
  (before-each (test-approve-api--quiet))

  (it "sends the method, resource, params and payload"
    (test-approve-api--with-fake
      (approve-api-rest "POST" "/markdown" :payload '((text . "hi")) :params nil)
      (let ((sent (car test-approve-api--sent)))
        (expect (plist-get sent :kind) :to-be 'rest)
        (expect (plist-get sent :method) :to-equal "POST")
        (expect (plist-get sent :resource) :to-equal "/markdown")
        (expect (plist-get sent :payload) :to-equal '((text . "hi")))
        (expect (plist-get sent :name) :to-be nil))))

  (it "calls back with the decoded body and records the REST rate limit"
    (test-approve-api--with-fake
      (let (received)
        (approve-api-rest "GET" "/repos/o/r/pulls/1/files" :params '((page . 1))
                          :callback (lambda (value) (setq received value)))
        (expect (plist-get (car test-approve-api--sent) :params) :to-equal '((page . 1)))
        (test-approve-api--succeed '(((filename . "a.el")))
                                   '(("x-ratelimit-remaining" . "4999")
                                     ("x-ratelimit-reset" . "1900000000")))
        (expect received :to-equal '(((filename . "a.el"))))
        (expect (plist-get (approve-api-rate-limit 'rest) :remaining) :to-equal 4999)
        (expect (plist-get (approve-api-rate-limit 'rest) :reset) :to-equal 1900000000)))))

(describe "approve-api-ghub-transport"
  (it "makes an asynchronous ghub-request with Approve's auth, host and payload"
    (let ((approve-api-transport #'approve-api-ghub-transport)
          (approve-api-timeout nil)
          (approve-api-auth 'approve)
          (approve-api-username "octocat")
          (approve-api--in-flight nil)
          (approve-api--rate-limits (make-hash-table :test 'equal))
          received)
      (spy-on 'ghub-request)
      (approve-api-graphql "query { a }" '((n . 1)) :progress nil
                           :host "api.github.com"
                           :callback (lambda (data) (setq received data)))
      (let* ((args (spy-calls-args-for 'ghub-request 0))
             (keys (nthcdr 3 args)))
        (expect (seq-take args 3) :to-equal '("POST" "/graphql" nil))
        (expect (plist-get keys :payload)
                :to-equal '((query . "query { a }") (variables . ((n . 1)))))
        (expect (plist-get keys :host) :to-equal "api.github.com")
        (expect (plist-get keys :auth) :to-be 'approve)
        (expect (plist-get keys :username) :to-equal "octocat")
        (expect (functionp (plist-get keys :callback)) :to-be-truthy)
        (expect (functionp (plist-get keys :errorback)) :to-be-truthy)
        (funcall (plist-get keys :callback) '((data . ((a . 1)))) nil nil nil)
        (expect received :to-equal '((a . 1))))))

  (it "turns ghub's HTTP errors into failures"
    (let ((failures nil))
      (spy-on 'ghub-request)
      (approve-api-ghub-transport
       (list :method "GET" :resource "/x"
             :on-success #'ignore
             :on-failure (lambda (failure) (push failure failures))))
      (let ((errorback (plist-get (nthcdr 3 (spy-calls-args-for 'ghub-request 0))
                                  :errorback)))
        (funcall errorback '(error http 404 ((message . "Not Found")))
                 '(("x-ratelimit-remaining" . "10")) nil nil))
      (expect failures
              :to-equal '((:status 404 :payload ((message . "Not Found"))
                           :headers (("x-ratelimit-remaining" . "10")))))))

  (it "reports a connection that fails while ghub handles it as a network error"
    ;; url-http calls ghub from the process sentinel when the connection
    ;; fails, and ghub signals there because no headers arrived.
    (test-approve-api--with-fake
      (let* ((approve-api-transport #'approve-api-ghub-transport)
             (buffer (generate-new-buffer " *http api.github.com:443*"))
             (process (make-pipe-process
                       :name "approve-test" :buffer buffer :noquery t
                       :sentinel (lambda (_process _event)
                                   (error "BUG: Missing headers in response buffer"))))
             (received nil))
        (unwind-protect
            (progn
              (spy-on 'ghub-request :and-return-value buffer)
              (approve-api-rest "GET" "/x" :progress nil
                                :errorback (lambda (err) (push err received)))
              (funcall (process-sentinel process) process "failed with code 6\n")
              (expect (length received) :to-equal 1)
              (expect (car (car received)) :to-be 'approve-api-network-error)
              (expect (approve-api-error-message (car received))
                      :to-equal "connection-failed: failed with code 6")
              (expect (approve-api-in-flight-count) :to-equal 0))
          (set-process-sentinel process #'ignore)
          (delete-process process)
          (kill-buffer buffer)))))

  (it "lets a sentinel error through once the request has ended"
    (let* ((buffer (generate-new-buffer " *http api.github.com:443*"))
           (process (make-pipe-process
                     :name "approve-test" :buffer buffer :noquery t
                     :sentinel (lambda (_process _event) (error "Late trouble"))))
           (failures 0))
      (unwind-protect
          (progn
            (spy-on 'ghub-request :and-return-value buffer)
            (approve-api-ghub-transport
             (list :method "GET" :resource "/x"
                   :on-success #'ignore
                   :on-failure (lambda (_) (setq failures (1+ failures)))))
            (funcall (plist-get (nthcdr 3 (spy-calls-args-for 'ghub-request 0))
                                :callback)
                     '((ok . t)) nil nil nil)
            (expect (funcall (process-sentinel process) process "deleted\n")
                    :to-throw 'error '("Late trouble"))
            (expect failures :to-equal 0))
        (set-process-sentinel process #'ignore)
        (delete-process process)
        (kill-buffer buffer))))

  (it "asks ghub from the home directory, so only the global Git config counts"
    (let ((directory nil))
      (spy-on 'ghub-request :and-call-fake
              (lambda (&rest _) (setq directory default-directory) nil))
      (let ((default-directory temporary-file-directory))
        (approve-api-ghub-transport
         (list :method "GET" :resource "/x" :on-success #'ignore :on-failure #'ignore)))
      (expect directory :to-equal (expand-file-name "~/"))))

  (it "returns a function that aborts the retrieval"
    (let ((buffer (generate-new-buffer " url")))
      (spy-on 'ghub-request :and-return-value buffer)
      (let ((abort (approve-api-ghub-transport
                    (list :method "GET" :resource "/x"
                          :on-success #'ignore :on-failure #'ignore))))
        (funcall abort)
        (expect (buffer-live-p buffer) :to-be nil)))))

(describe "approve-api-host-from-web-host"
  (it "maps github.com to api.github.com and other hosts to HOST/api/v3"
    (expect (approve-api-host-from-web-host "github.com") :to-equal "api.github.com")
    (expect (approve-api-host-from-web-host "GitHub.com") :to-equal "api.github.com")
    (expect (approve-api-host-from-web-host "git.example.com")
            :to-equal "git.example.com/api/v3")))

;;; test-approve-api.el ends here
