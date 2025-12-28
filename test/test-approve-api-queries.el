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

(defvar test-approve-api-queries--rest-calls nil
  "List of captured approve-api-rest calls.")

(defvar test-approve-api-queries--last-graphql-callback nil
  "Stored callback from last GraphQL API call.")

(defvar test-approve-api-queries--last-graphql-error-callback nil
  "Stored error callback from last GraphQL API call.")

(defvar test-approve-api-queries--last-rest-callback nil
  "Stored callback from last REST API call.")

(defvar test-approve-api-queries--last-rest-error-callback nil
  "Stored error callback from last REST API call.")

(defun test-approve-api-queries--mock-graphql (query variables &rest args)
  "Mock implementation of `approve-api-graphql'.
Captures the call and stores callbacks for later invocation."
  (push (list :query query :variables variables :args args)
        test-approve-api-queries--graphql-calls)
  (setq test-approve-api-queries--last-graphql-callback (plist-get args :callback))
  (setq test-approve-api-queries--last-graphql-error-callback (plist-get args :error-callback))
  42) ; Return mock request ID

(defun test-approve-api-queries--mock-rest (method resource &rest args)
  "Mock implementation of `approve-api-rest'.
Captures the call and stores callbacks for later invocation."
  (push (list :method method :resource resource :args args)
        test-approve-api-queries--rest-calls)
  (setq test-approve-api-queries--last-rest-callback (plist-get args :callback))
  (setq test-approve-api-queries--last-rest-error-callback (plist-get args :error-callback))
  43) ; Return mock request ID

(defun test-approve-api-queries--reset-mocks ()
  "Reset all mock state."
  (setq test-approve-api-queries--graphql-calls nil)
  (setq test-approve-api-queries--rest-calls nil)
  (setq test-approve-api-queries--last-graphql-callback nil)
  (setq test-approve-api-queries--last-graphql-error-callback nil)
  (setq test-approve-api-queries--last-rest-callback nil)
  (setq test-approve-api-queries--last-rest-error-callback nil))

(defun test-approve-api-queries--sample-pr-data ()
  "Return sample PR data for testing."
  '((repository
     (pullRequest
      (id . "PR_123")
      (title . "Test PR")
      (baseRefOid . "abc123")
      (headRefOid . "def456")))))

(defun test-approve-api-queries--sample-diff-data ()
  "Return sample diff data for testing."
  '((files . [((filename . "test.el")
               (status . "modified")
               (patch . "@@ -1,3 +1,4 @@"))])
    (commits . 1)))

;;; Tests

(describe "approve-api-queries"

  (before-each
    (test-approve-api-queries--reset-mocks)
    (spy-on 'approve-api-graphql :and-call-fake #'test-approve-api-queries--mock-graphql)
    (spy-on 'approve-api-rest :and-call-fake #'test-approve-api-queries--mock-rest)
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

    (it "fetches diff after GraphQL response"
      (approve-api-query-pull-request "owner" "repo" 1)
      ;; Initially no REST calls
      (expect (length test-approve-api-queries--rest-calls) :to-equal 0)
      ;; Simulate GraphQL response
      (funcall test-approve-api-queries--last-graphql-callback
               (test-approve-api-queries--sample-pr-data))
      ;; Now REST should be called
      (expect (length test-approve-api-queries--rest-calls) :to-equal 1)
      (let ((call (car test-approve-api-queries--rest-calls)))
        (expect (plist-get call :method) :to-equal "GET")
        (expect (plist-get call :resource)
                :to-match "/repos/owner/repo/compare/abc123\\.\\.\\.def456")))

    (it "calls user callback with merged data after diff fetch"
      (let* ((result nil)
             (my-callback (lambda (data) (setq result data))))
        (approve-api-query-pull-request "owner" "repo" 1 :callback my-callback)
        ;; Simulate GraphQL response
        (funcall test-approve-api-queries--last-graphql-callback
                 (test-approve-api-queries--sample-pr-data))
        ;; Simulate REST response
        (funcall test-approve-api-queries--last-rest-callback
                 (test-approve-api-queries--sample-diff-data))
        ;; Check result
        (expect result :not :to-be nil)
        (let* ((repo (alist-get 'repository result))
               (pr (alist-get 'pullRequest repo))
               (diff (alist-get 'diff pr)))
          (expect (alist-get 'title pr) :to-equal "Test PR")
          (expect (alist-get 'commits diff) :to-equal 1))))

    (it "calls user callback without diff if refs are missing"
      (let* ((result nil)
             (my-callback (lambda (data) (setq result data)))
             ;; PR data without refs
             (pr-data '((repository (pullRequest (id . "PR_123") (title . "No refs"))))))
        (approve-api-query-pull-request "owner" "repo" 1 :callback my-callback)
        ;; Simulate GraphQL response with missing refs
        (funcall test-approve-api-queries--last-graphql-callback pr-data)
        ;; No REST call should have been made
        (expect (length test-approve-api-queries--rest-calls) :to-equal 0)
        ;; Callback should still be called
        (expect result :not :to-be nil)
        (expect (alist-get 'title (alist-get 'pullRequest (alist-get 'repository result)))
                :to-equal "No refs")))

    (it "passes error-callback to approve-api-graphql"
      (let ((my-error-callback (lambda (err) err)))
        (approve-api-query-pull-request "owner" "repo" 1 :error-callback my-error-callback)
        (let* ((call (car test-approve-api-queries--graphql-calls))
               (args (plist-get call :args)))
          (expect (plist-get args :error-callback) :to-equal my-error-callback))))

    (it "passes error-callback to REST call"
      (let ((my-error-callback (lambda (err) err)))
        (approve-api-query-pull-request "owner" "repo" 1 :error-callback my-error-callback)
        ;; Simulate GraphQL response
        (funcall test-approve-api-queries--last-graphql-callback
                 (test-approve-api-queries--sample-pr-data))
        ;; Check that error callback was passed to REST call
        (expect test-approve-api-queries--last-rest-error-callback
                :to-equal my-error-callback)))

    (it "passes buffer to approve-api-graphql"
      (let ((test-buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (progn
              (approve-api-query-pull-request "owner" "repo" 1 :buffer test-buffer)
              (let* ((call (car test-approve-api-queries--graphql-calls))
                     (args (plist-get call :args)))
                (expect (plist-get args :buffer) :to-equal test-buffer)))
          (kill-buffer test-buffer))))

    (it "passes buffer to REST call"
      (let ((test-buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (progn
              (approve-api-query-pull-request "owner" "repo" 1 :buffer test-buffer)
              ;; Simulate GraphQL response
              (funcall test-approve-api-queries--last-graphql-callback
                       (test-approve-api-queries--sample-pr-data))
              (let* ((call (car test-approve-api-queries--rest-calls))
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
              "queries/get-pull-request.graphql")))

  (describe "approve-api--extract-pr-refs"

    (it "extracts base and head refs from valid data"
      (let ((refs (approve-api--extract-pr-refs (test-approve-api-queries--sample-pr-data))))
        (expect refs :not :to-be nil)
        (expect (car refs) :to-equal "abc123")
        (expect (cdr refs) :to-equal "def456")))

    (it "returns nil when repository is missing"
      (let ((refs (approve-api--extract-pr-refs '((other . "data")))))
        (expect refs :to-be nil)))

    (it "returns nil when pullRequest is missing"
      (let ((refs (approve-api--extract-pr-refs '((repository (other . "data"))))))
        (expect refs :to-be nil)))

    (it "returns nil when baseRefOid is missing"
      (let ((refs (approve-api--extract-pr-refs
                   '((repository (pullRequest (headRefOid . "def456")))))))
        (expect refs :to-be nil)))

    (it "returns nil when headRefOid is missing"
      (let ((refs (approve-api--extract-pr-refs
                   '((repository (pullRequest (baseRefOid . "abc123")))))))
        (expect refs :to-be nil))))

  (describe "approve-api--merge-diff-into-data"

    (it "adds diff to pullRequest in data"
      (let* ((pr-data (test-approve-api-queries--sample-pr-data))
             (diff-data (test-approve-api-queries--sample-diff-data))
             (result (approve-api--merge-diff-into-data pr-data diff-data))
             (pr (alist-get 'pullRequest (alist-get 'repository result))))
        (expect (alist-get 'diff pr) :to-equal diff-data)))

    (it "preserves existing PR fields"
      (let* ((pr-data (test-approve-api-queries--sample-pr-data))
             (diff-data (test-approve-api-queries--sample-diff-data))
             (result (approve-api--merge-diff-into-data pr-data diff-data))
             (pr (alist-get 'pullRequest (alist-get 'repository result))))
        (expect (alist-get 'id pr) :to-equal "PR_123")
        (expect (alist-get 'title pr) :to-equal "Test PR")))))

(provide 'test-approve-api-queries)
;;; test-approve-api-queries.el ends here
