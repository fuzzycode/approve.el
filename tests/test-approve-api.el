;;; test-approve-api.el --- Tests for approve-api -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;;; Commentary:

;; Buttercup tests for the API communication layer.
;; All ghub calls are mocked to avoid real API requests.

;;; Code:

(require 'buttercup)
(require 'approve-api)

;;; Error extraction tests

(describe "approve-api--extract-error"

  (it "extracts message from a string"
    (let ((result (approve-api--extract-error "something went wrong")))
      (expect (car result) :to-equal "something went wrong")
      (expect (cdr result) :to-be nil)))

  (it "extracts message and code from a list"
    (let ((result (approve-api--extract-error '("not found" 404))))
      (expect (car result) :to-equal "not found")
      (expect (cdr result) :to-equal 404)))

  (it "extracts from a plist with :message and :code"
    (let ((result (approve-api--extract-error '(:message "rate limited" :code 429))))
      (expect (car result) :to-equal "rate limited")
      (expect (cdr result) :to-equal 429)))

  (it "formats unknown error types"
    (let ((result (approve-api--extract-error 42)))
      (expect (car result) :to-equal "42")
      (expect (cdr result) :to-be nil))))

;;; Callback wrapping tests

(describe "approve-api--wrap-success"

  (it "calls on-success with the value"
    (let* ((received nil)
           (callback (approve-api--wrap-success
                      (lambda (data) (setq received data))
                      nil)))
      (funcall callback '((name . "test")) nil nil nil)
      (expect received :to-equal '((name . "test")))))

  (it "completes the progress reporter"
    (let* ((reporter (make-progress-reporter "Testing..."))
           (callback (approve-api--wrap-success (lambda (_)) reporter)))
      (spy-on 'progress-reporter-done)
      (funcall callback nil nil nil nil)
      (expect 'progress-reporter-done :to-have-been-called)))

  (it "handles nil on-success gracefully"
    (let ((callback (approve-api--wrap-success nil nil)))
      (expect (funcall callback '((data . t)) nil nil nil)
              :not :to-throw))))

(describe "approve-api--wrap-error"

  (it "calls on-error with extracted message and code"
    (let* ((received-msg nil)
           (received-code nil)
           (callback (approve-api--wrap-error
                      (lambda (msg code)
                        (setq received-msg msg
                              received-code code))
                      nil)))
      (funcall callback '("forbidden" 403) nil nil nil)
      (expect received-msg :to-equal "forbidden")
      (expect received-code :to-equal 403)))

  (it "falls back to default error handler when on-error is nil"
    (let ((callback (approve-api--wrap-error nil nil)))
      (spy-on 'approve-api--default-error-handler)
      (funcall callback "something broke" nil nil nil)
      (expect 'approve-api--default-error-handler
              :to-have-been-called-with "something broke" nil)))

  (it "completes the progress reporter on error"
    (let* ((reporter (make-progress-reporter "Testing..."))
           (callback (approve-api--wrap-error (lambda (_msg _code)) reporter)))
      (spy-on 'progress-reporter-done)
      (funcall callback "error" nil nil nil)
      (expect 'progress-reporter-done :to-have-been-called))))

;;; Default error handler tests

(describe "approve-api--default-error-handler"

  (it "logs and shows a message"
    (spy-on 'approve-log--message)
    (spy-on 'message)
    (approve-api--default-error-handler "rate limit exceeded" 429)
    (expect 'approve-log--message :to-have-been-called)
    (expect 'message :to-have-been-called-with
            "[approve] API error: %s" "rate limit exceeded")))

;;; GraphQL dispatch tests

(describe "approve-api--graphql"

  (it "calls ghub-request with GraphQL payload"
    (spy-on 'ghub-request)
    (approve-api--graphql "query { viewer { login } }"
                          '((var . "value"))
                          :host "api.github.com"
                          :message "Testing...")
    (expect 'ghub-request :to-have-been-called)
    (let ((args (spy-calls-args-for 'ghub-request 0)))
      (expect (nth 0 args) :to-equal "POST")
      (expect (nth 1 args) :to-equal "/graphql")))

  (it "passes host and auth to ghub-request"
    (let ((approve-ghub-auth-name 'approve))
      (spy-on 'ghub-request)
      (approve-api--graphql "query {}" nil
                            :host "ghe.example.com")
      (let ((args (spy-calls-args-for 'ghub-request 0)))
        (expect (plist-get (cdddr args) :host) :to-equal "ghe.example.com")
        (expect (plist-get (cdddr args) :auth) :to-equal 'approve))))

  (it "handles synchronous errors with default handler"
    (spy-on 'ghub-request :and-call-fake
            (lambda (&rest _) (error "connection refused")))
    (spy-on 'approve-api--default-error-handler)
    (approve-api--graphql "query {}" nil)
    (expect 'approve-api--default-error-handler :to-have-been-called))

  (it "handles synchronous errors with custom handler"
    (let ((received nil))
      (spy-on 'ghub-request :and-call-fake
              (lambda (&rest _) (error "timeout")))
      (approve-api--graphql "query {}" nil
                            :on-error (lambda (msg _code) (setq received msg)))
      (expect received :to-equal "timeout"))))

;;; REST dispatch tests

(describe "approve-api--rest"

  (it "calls ghub-request with correct method and resource"
    (spy-on 'ghub-request)
    (approve-api--rest "GET" "/repos/owner/repo"
                       :message "Fetching...")
    (expect 'ghub-request :to-have-been-called)
    (let ((args (spy-calls-args-for 'ghub-request 0)))
      (expect (nth 0 args) :to-equal "GET")
      (expect (nth 1 args) :to-equal "/repos/owner/repo")))

  (it "passes payload for POST requests"
    (spy-on 'ghub-request)
    (approve-api--rest "POST" "/repos/owner/repo/issues"
                       :payload '((title . "test")))
    (let ((args (spy-calls-args-for 'ghub-request 0)))
      (expect (plist-get (cdddr args) :payload)
              :to-equal '((title . "test")))))

  (it "handles synchronous errors with default handler"
    (spy-on 'ghub-request :and-call-fake
            (lambda (&rest _) (error "network error")))
    (spy-on 'approve-api--default-error-handler)
    (approve-api--rest "GET" "/repos/owner/repo")
    (expect 'approve-api--default-error-handler :to-have-been-called))

  (it "handles synchronous errors with custom handler"
    (let ((received nil))
      (spy-on 'ghub-request :and-call-fake
              (lambda (&rest _) (error "forbidden")))
      (approve-api--rest "PUT" "/resource"
                         :on-error (lambda (msg _code) (setq received msg)))
      (expect received :to-equal "forbidden"))))

;;; Timeout configuration tests

(describe "approve-api-timeout"

  (it "binds url-retrieve-timeout during graphql calls"
    (let ((approve-api-timeout 60)
          (captured-timeout nil))
      (spy-on 'ghub-request :and-call-fake
              (lambda (&rest _) (setq captured-timeout url-retrieve-timeout)))
      (approve-api--graphql "query {}" nil)
      (expect captured-timeout :to-equal 60)))

  (it "binds url-retrieve-timeout during rest calls"
    (let ((approve-api-timeout 10)
          (captured-timeout nil))
      (spy-on 'ghub-request :and-call-fake
              (lambda (&rest _) (setq captured-timeout url-retrieve-timeout)))
      (approve-api--rest "GET" "/test")
      (expect captured-timeout :to-equal 10))))

;;; test-approve-api.el ends here
