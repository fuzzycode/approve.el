;;; test-approve-api.el --- Tests for approve-api  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the GitHub API communication layer.
;; These tests mock ghub to test the approve-api functionality in isolation.

;;; Code:

(require 'buttercup)
(require 'approve-api)

;;; Test Helpers

(defvar test-approve-api--ghub-graphql-calls nil
  "List of captured ghub-graphql calls.")

(defvar test-approve-api--ghub-request-calls nil
  "List of captured ghub-request calls.")

(defvar test-approve-api--ghub-graphql-callback nil
  "Stored callback from ghub-graphql.")

(defvar test-approve-api--ghub-graphql-errorback nil
  "Stored errorback from ghub-graphql.")

(defvar test-approve-api--ghub-request-callback nil
  "Stored callback from ghub-request.")

(defvar test-approve-api--ghub-request-errorback nil
  "Stored errorback from ghub-request.")

(defun test-approve-api--mock-ghub-graphql (query variables &rest args)
  "Mock implementation of `ghub-graphql'.
Captures the call and stores callbacks for later invocation."
  (push (list :query query :variables variables :args args)
        test-approve-api--ghub-graphql-calls)
  (setq test-approve-api--ghub-graphql-callback (plist-get args :callback))
  (setq test-approve-api--ghub-graphql-errorback (plist-get args :errorback))
  nil)

(defun test-approve-api--mock-ghub-request (method resource &rest args)
  "Mock implementation of `ghub-request'.
Captures the call and stores callbacks for later invocation."
  (push (list :method method :resource resource :args args)
        test-approve-api--ghub-request-calls)
  (setq test-approve-api--ghub-request-callback (plist-get args :callback))
  (setq test-approve-api--ghub-request-errorback (plist-get args :errorback))
  nil)

(defun test-approve-api--reset-mocks ()
  "Reset all mock state."
  (setq test-approve-api--ghub-graphql-calls nil)
  (setq test-approve-api--ghub-request-calls nil)
  (setq test-approve-api--ghub-graphql-callback nil)
  (setq test-approve-api--ghub-graphql-errorback nil)
  (setq test-approve-api--ghub-request-callback nil)
  (setq test-approve-api--ghub-request-errorback nil))

(defun test-approve-api--simulate-graphql-success (response)
  "Simulate a successful GraphQL response."
  (when test-approve-api--ghub-graphql-callback
    (funcall test-approve-api--ghub-graphql-callback response)))

(defun test-approve-api--simulate-graphql-error (error)
  "Simulate a GraphQL error."
  (when test-approve-api--ghub-graphql-errorback
    (funcall test-approve-api--ghub-graphql-errorback error)))

(defun test-approve-api--simulate-rest-success (response)
  "Simulate a successful REST response."
  (when test-approve-api--ghub-request-callback
    (funcall test-approve-api--ghub-request-callback response)))

(defun test-approve-api--simulate-rest-error (error)
  "Simulate a REST error."
  (when test-approve-api--ghub-request-errorback
    (funcall test-approve-api--ghub-request-errorback error)))

;;; Tests

(describe "approve-api"

  (before-each
    (test-approve-api--reset-mocks)
    ;; Mock ghub functions
    (spy-on 'ghub-graphql :and-call-fake #'test-approve-api--mock-ghub-graphql)
    (spy-on 'ghub-request :and-call-fake #'test-approve-api--mock-ghub-request)
    ;; Reset API state
    (approve-api-cancel-all)
    ;; Reset customizations
    (setq approve-api-username nil)
    (setq approve-api-host nil)
    (setq approve-api-auth-name 'approve)
    (setq approve-api-timeout 30))

  (after-each
    (approve-api-cancel-all))

  (describe "approve-api-graphql"

    (it "calls ghub-graphql with the query and variables"
      (approve-api-graphql "query { viewer { login } }"
                           '((foo . "bar"))
                           :callback #'ignore
                           :progress-message nil)
      (expect (length test-approve-api--ghub-graphql-calls) :to-equal 1)
      (let ((call (car test-approve-api--ghub-graphql-calls)))
        (expect (plist-get call :query) :to-equal "query { viewer { login } }")
        (expect (plist-get call :variables) :to-equal '((foo . "bar")))))

    (it "returns a request ID"
      (let ((id (approve-api-graphql "query { viewer { login } }" nil
                                     :progress-message nil)))
        (expect id :to-be-truthy)
        (expect (integerp id) :to-be-truthy)))

    (it "increments request IDs"
      (let ((id1 (approve-api-graphql "query1" nil :progress-message nil))
            (id2 (approve-api-graphql "query2" nil :progress-message nil)))
        (expect id2 :to-be-greater-than id1)))

    (it "tracks pending requests"
      (expect (approve-api-pending-count) :to-equal 0)
      (approve-api-graphql "query1" nil :progress-message nil)
      (expect (approve-api-pending-count) :to-equal 1)
      (approve-api-graphql "query2" nil :progress-message nil)
      (expect (approve-api-pending-count) :to-equal 2))

    (it "passes custom auth parameters to ghub"
      (setq approve-api-username "testuser")
      (setq approve-api-host "github.example.com")
      (setq approve-api-auth-name 'custom-auth)
      (approve-api-graphql "query { viewer { login } }" nil
                           :progress-message nil)
      (let* ((call (car test-approve-api--ghub-graphql-calls))
             (args (plist-get call :args)))
        (expect (plist-get args :username) :to-equal "testuser")
        (expect (plist-get args :host) :to-equal "github.example.com")
        (expect (plist-get args :auth) :to-equal 'custom-auth)))

    (it "calls success callback with data on successful response"
      (let ((result nil))
        (approve-api-graphql "query { viewer { login } }" nil
                             :callback (lambda (data) (setq result data))
                             :progress-message nil)
        (test-approve-api--simulate-graphql-success
         '((data . ((viewer . ((login . "testuser")))))))
        (expect result :to-equal '((viewer . ((login . "testuser")))))))

    (it "calls error callback on GraphQL errors in response"
      (let ((error-result nil))
        (approve-api-graphql "query { viewer { login } }" nil
                             :callback #'ignore
                             :error-callback (lambda (err) (setq error-result err))
                             :progress-message nil)
        (test-approve-api--simulate-graphql-success
         '((errors . (((message . "Some error"))))
           (data . nil)))
        (expect (car error-result) :to-equal 'approve-api-graphql-error)))

    (it "calls error callback on request error"
      (let ((error-result nil))
        (approve-api-graphql "query { viewer { login } }" nil
                             :callback #'ignore
                             :error-callback (lambda (err) (setq error-result err))
                             :progress-message nil)
        (test-approve-api--simulate-graphql-error
         '(ghub-http-error 500 "Internal Server Error"))
        (expect (car error-result) :to-equal 'approve-api-error)))

    (it "cleans up pending request on success"
      (approve-api-graphql "query { viewer { login } }" nil
                           :callback #'ignore
                           :progress-message nil)
      (expect (approve-api-pending-count) :to-equal 1)
      (test-approve-api--simulate-graphql-success '((data . nil)))
      (expect (approve-api-pending-count) :to-equal 0))

    (it "cleans up pending request on error"
      (approve-api-graphql "query { viewer { login } }" nil
                           :error-callback #'ignore
                           :progress-message nil)
      (expect (approve-api-pending-count) :to-equal 1)
      (test-approve-api--simulate-graphql-error '(error "test"))
      (expect (approve-api-pending-count) :to-equal 0)))

  (describe "approve-api-rest"

    (it "calls ghub-request with method and resource"
      (approve-api-rest "GET" "/repos/owner/repo/pulls"
                        :callback #'ignore
                        :progress-message nil)
      (expect (length test-approve-api--ghub-request-calls) :to-equal 1)
      (let ((call (car test-approve-api--ghub-request-calls)))
        (expect (plist-get call :method) :to-equal "GET")
        (expect (plist-get call :resource) :to-equal "/repos/owner/repo/pulls")))

    (it "passes payload to ghub-request"
      (approve-api-rest "POST" "/repos/owner/repo/pulls"
                        :payload '((title . "Test PR"))
                        :callback #'ignore
                        :progress-message nil)
      (let* ((call (car test-approve-api--ghub-request-calls))
             (args (plist-get call :args)))
        (expect (plist-get args :payload) :to-equal '((title . "Test PR")))))

    (it "passes query parameters to ghub-request"
      (approve-api-rest "GET" "/repos/owner/repo/pulls"
                        :query '((state . "open"))
                        :callback #'ignore
                        :progress-message nil)
      (let* ((call (car test-approve-api--ghub-request-calls))
             (args (plist-get call :args)))
        (expect (plist-get args :query) :to-equal '((state . "open")))))

    (it "returns a request ID"
      (let ((id (approve-api-rest "GET" "/user" :progress-message nil)))
        (expect id :to-be-truthy)
        (expect (integerp id) :to-be-truthy)))

    (it "calls success callback with response data"
      (let ((result nil))
        (approve-api-rest "GET" "/user"
                          :callback (lambda (data) (setq result data))
                          :progress-message nil)
        (test-approve-api--simulate-rest-success '((login . "testuser")))
        (expect result :to-equal '((login . "testuser")))))

    (it "calls error callback on failure"
      (let ((error-result nil))
        (approve-api-rest "GET" "/user"
                          :error-callback (lambda (err) (setq error-result err))
                          :progress-message nil)
        (test-approve-api--simulate-rest-error
         '(ghub-http-error 404 "Not Found"))
        (expect (car error-result) :to-equal 'approve-api-not-found))))

  (describe "approve-api-cancel"

    (it "cancels a pending request"
      (let ((id (approve-api-graphql "query" nil :progress-message nil)))
        (expect (approve-api-pending-count) :to-equal 1)
        (expect (approve-api-cancel id) :to-be-truthy)
        (expect (approve-api-pending-count) :to-equal 0)))

    (it "returns nil for non-existent request"
      (expect (approve-api-cancel 99999) :to-be nil)))

  (describe "approve-api-cancel-all"

    (it "cancels all pending requests"
      (approve-api-graphql "query1" nil :progress-message nil)
      (approve-api-graphql "query2" nil :progress-message nil)
      (approve-api-rest "GET" "/user" :progress-message nil)
      (expect (approve-api-pending-count) :to-equal 3)
      (expect (approve-api-cancel-all) :to-equal 3)
      (expect (approve-api-pending-count) :to-equal 0)))

  (describe "error processing"

    (it "converts 401 to unauthorized error"
      (let ((error-result nil))
        (approve-api-rest "GET" "/user"
                          :error-callback (lambda (err) (setq error-result err))
                          :progress-message nil)
        (test-approve-api--simulate-rest-error
         '(ghub-http-error 401 "Unauthorized"))
        (expect (car error-result) :to-equal 'approve-api-unauthorized)))

    (it "converts 404 to not-found error"
      (let ((error-result nil))
        (approve-api-rest "GET" "/user"
                          :error-callback (lambda (err) (setq error-result err))
                          :progress-message nil)
        (test-approve-api--simulate-rest-error
         '(ghub-http-error 404 "Not Found"))
        (expect (car error-result) :to-equal 'approve-api-not-found)))

    (it "converts 403 rate limit to rate-limit error"
      (let ((error-result nil))
        (approve-api-rest "GET" "/user"
                          :error-callback (lambda (err) (setq error-result err))
                          :progress-message nil)
        (test-approve-api--simulate-rest-error
         '(ghub-http-error 403 "API rate limit exceeded"))
        (expect (car error-result) :to-equal 'approve-api-rate-limit))))

  (describe "buffer context"

    (it "calls callback in the originating buffer"
      (let ((callback-buffer nil)
            (test-buffer (generate-new-buffer " *test-buffer*")))
        (unwind-protect
            (progn
              (with-current-buffer test-buffer
                (approve-api-graphql "query" nil
                                     :callback (lambda (_)
                                                 (setq callback-buffer (current-buffer)))
                                     :progress-message nil))
              (test-approve-api--simulate-graphql-success '((data . nil)))
              (expect callback-buffer :to-equal test-buffer))
          (kill-buffer test-buffer))))

    (it "still calls callback if originating buffer is killed"
      (let ((callback-called nil)
            (test-buffer (generate-new-buffer " *test-buffer*")))
        (with-current-buffer test-buffer
          (approve-api-graphql "query" nil
                               :callback (lambda (_) (setq callback-called t))
                               :progress-message nil))
        (kill-buffer test-buffer)
        (test-approve-api--simulate-graphql-success '((data . nil)))
        (expect callback-called :to-be-truthy)))

    (it "calls error callback in the originating buffer"
      (let ((callback-buffer nil)
            (test-buffer (generate-new-buffer " *test-buffer*")))
        (unwind-protect
            (progn
              (with-current-buffer test-buffer
                (approve-api-graphql "query" nil
                                     :error-callback (lambda (_)
                                                       (setq callback-buffer (current-buffer)))
                                     :progress-message nil))
              (test-approve-api--simulate-graphql-error '(error "test"))
              (expect callback-buffer :to-equal test-buffer))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer))))))

  (describe "timeout handling"

    (it "sets up a timeout timer when timeout is specified"
      (let ((mock-timer (run-at-time 1000 nil #'ignore)))  ; Create real timer far in the future
        (unwind-protect
            (progn
              (spy-on 'run-at-time :and-return-value mock-timer)
              (approve-api-graphql "query" nil
                                   :timeout 5
                                   :progress-message nil)
              (expect 'run-at-time :to-have-been-called)
              (let ((call-args (spy-calls-args-for 'run-at-time 0)))
                (expect (car call-args) :to-equal 5)))
          (cancel-timer mock-timer))))

    (it "does not set up timer when timeout is nil"
      (spy-on 'run-at-time)
      (approve-api-graphql "query" nil
                           :timeout nil
                           :progress-message nil)
      (expect 'run-at-time :not :to-have-been-called))

    (it "cancels timer on successful completion"
      (let ((mock-timer (run-at-time 1000 nil #'ignore)))  ; Create real timer far in the future
        (unwind-protect
            (progn
              (spy-on 'run-at-time :and-return-value mock-timer)
              (spy-on 'cancel-timer :and-call-through)
              (approve-api-graphql "query" nil
                                   :timeout 30
                                   :callback #'ignore
                                   :progress-message nil)
              (test-approve-api--simulate-graphql-success '((data . nil)))
              (expect 'cancel-timer :to-have-been-called-with mock-timer))
          ;; Timer may already be cancelled, ignore errors
          (ignore-errors (cancel-timer mock-timer)))))

    (it "cancels timer on error"
      (let ((mock-timer (run-at-time 1000 nil #'ignore)))  ; Create real timer far in the future
        (unwind-protect
            (progn
              (spy-on 'run-at-time :and-return-value mock-timer)
              (spy-on 'cancel-timer :and-call-through)
              (approve-api-graphql "query" nil
                                   :timeout 30
                                   :error-callback #'ignore
                                   :progress-message nil)
              (test-approve-api--simulate-graphql-error '(error "test"))
              (expect 'cancel-timer :to-have-been-called-with mock-timer))
          ;; Timer may already be cancelled, ignore errors
          (ignore-errors (cancel-timer mock-timer))))))

  (describe "progress reporting"

    (it "creates progress reporter when message is provided"
      (spy-on 'make-progress-reporter :and-return-value 'mock-reporter)
      (approve-api-graphql "query" nil
                           :progress-message "Loading...")
      (expect 'make-progress-reporter :to-have-been-called-with "Loading..."))

    (it "does not create progress reporter when message is nil"
      (spy-on 'make-progress-reporter)
      (approve-api-graphql "query" nil
                           :progress-message nil)
      (expect 'make-progress-reporter :not :to-have-been-called))

    (it "finishes progress reporter on success"
      (let ((mock-reporter (list 'mock-reporter)))
        (spy-on 'make-progress-reporter :and-return-value mock-reporter)
        (spy-on 'progress-reporter-done)
        (approve-api-graphql "query" nil
                             :progress-message "Loading..."
                             :callback #'ignore)
        (test-approve-api--simulate-graphql-success '((data . nil)))
        (expect 'progress-reporter-done :to-have-been-called-with mock-reporter)))

    (it "finishes progress reporter on error"
      (let ((mock-reporter (list 'mock-reporter)))
        (spy-on 'make-progress-reporter :and-return-value mock-reporter)
        (spy-on 'progress-reporter-done)
        (approve-api-graphql "query" nil
                             :progress-message "Loading..."
                             :error-callback #'ignore)
        (test-approve-api--simulate-graphql-error '(error "test"))
        (expect 'progress-reporter-done :to-have-been-called-with mock-reporter)))))

(provide 'test-approve-api)
;;; test-approve-api.el ends here
