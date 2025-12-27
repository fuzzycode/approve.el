;;; test-approve-api-queries.el --- Tests for approve-api-queries  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the GraphQL query functions.
;; These tests mock the underlying API layer to test query construction
;; and response handling in isolation.

;;; Code:

(require 'buttercup)
(require 'approve-api-queries)
(require 'approve-api)

;;; Test Helpers

(defvar test-approve-api-queries--graphql-calls nil
  "List of captured approve-api-graphql calls.")

(defvar test-approve-api-queries--last-callback nil
  "Stored callback from last API call.")

(defvar test-approve-api-queries--last-error-callback nil
  "Stored error callback from last API call.")

(defun test-approve-api-queries--mock-graphql (query variables &rest args)
  "Mock implementation of `approve-api-graphql'.
Captures the call and stores callbacks for later invocation."
  (push (list :query query :variables variables :args args)
        test-approve-api-queries--graphql-calls)
  (setq test-approve-api-queries--last-callback (plist-get args :callback))
  (setq test-approve-api-queries--last-error-callback (plist-get args :error-callback))
  42) ; Return mock request ID

(defun test-approve-api-queries--reset-mocks ()
  "Reset all mock state."
  (setq test-approve-api-queries--graphql-calls nil)
  (setq test-approve-api-queries--last-callback nil)
  (setq test-approve-api-queries--last-error-callback nil))

;;; Tests

(describe "approve-api-queries"

  (before-each
    (test-approve-api-queries--reset-mocks)
    (spy-on 'approve-api-graphql :and-call-fake #'test-approve-api-queries--mock-graphql)
    ;; Mock the graphql loader to return a simple query string
    (spy-on 'approve-graphql-load :and-return-value "query GetPullRequest { ... }"))

  (describe "approve-api-query-pull-request"

    (it "calls approve-api-graphql with correct parameters"
      (approve-api-query-pull-request "fuzzycode" "Approve.el" 42)
      (expect (length test-approve-api-queries--graphql-calls) :to-equal 1)
      (let ((call (car test-approve-api-queries--graphql-calls)))
        (expect (plist-get call :query) :to-equal "query GetPullRequest { ... }")
        (expect (plist-get call :variables)
                :to-equal '((repo_owner . "fuzzycode")
                            (repo_name . "Approve.el")
                            (pr_id . 42)))))

    (it "returns the request ID from approve-api-graphql"
      (let ((id (approve-api-query-pull-request "owner" "repo" 1)))
        (expect id :to-equal 42)))

    (it "passes callback to approve-api-graphql"
      (let ((my-callback (lambda (data) data)))
        (approve-api-query-pull-request "owner" "repo" 1 :callback my-callback)
        (let* ((call (car test-approve-api-queries--graphql-calls))
               (args (plist-get call :args)))
          (expect (plist-get args :callback) :to-equal my-callback))))

    (it "passes error-callback to approve-api-graphql"
      (let ((my-error-callback (lambda (err) err)))
        (approve-api-query-pull-request "owner" "repo" 1 :error-callback my-error-callback)
        (let* ((call (car test-approve-api-queries--graphql-calls))
               (args (plist-get call :args)))
          (expect (plist-get args :error-callback) :to-equal my-error-callback))))

    (it "passes buffer to approve-api-graphql"
      (let ((test-buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (progn
              (approve-api-query-pull-request "owner" "repo" 1 :buffer test-buffer)
              (let* ((call (car test-approve-api-queries--graphql-calls))
                     (args (plist-get call :args)))
                (expect (plist-get args :buffer) :to-equal test-buffer)))
          (kill-buffer test-buffer))))

    (it "uses current buffer as default"
      (approve-api-query-pull-request "owner" "repo" 1)
      (let* ((call (car test-approve-api-queries--graphql-calls))
             (args (plist-get call :args)))
        (expect (plist-get args :buffer) :to-equal (current-buffer))))

    (it "sets progress message with PR info"
      (approve-api-query-pull-request "fuzzycode" "Approve.el" 42)
      (let* ((call (car test-approve-api-queries--graphql-calls))
             (args (plist-get call :args)))
        (expect (plist-get args :progress-message)
                :to-equal "Fetching PR #42 from fuzzycode/Approve.el...")))

    (it "loads the GraphQL query from disk"
      (approve-api-query-pull-request "owner" "repo" 1)
      (expect 'approve-graphql-load :to-have-been-called-with
              "queries/get-pull-request.graphql"))))

(provide 'test-approve-api-queries)
;;; test-approve-api-queries.el ends here
