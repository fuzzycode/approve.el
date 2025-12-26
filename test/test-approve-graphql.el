;;; test-approve-graphql.el --- Tests for approve-graphql  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the GraphQL loading and include processing functionality.

;;; Code:

(require 'buttercup)
(require 'approve-graphql)

;;; Test Fixtures

(defvar test-approve-graphql--temp-dir nil
  "Temporary directory for test fixtures.")

(defun test-approve-graphql--setup-fixtures ()
  "Create temporary GraphQL files for testing."
  (setq test-approve-graphql--temp-dir
        (make-temp-file "approve-graphql-test-" t))

  ;; Create subdirectories
  (make-directory (expand-file-name "fragments" test-approve-graphql--temp-dir) t)
  (make-directory (expand-file-name "queries" test-approve-graphql--temp-dir) t)

  ;; Create a simple fragment
  (with-temp-file (expand-file-name "fragments/user-fields.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "fragment UserFields on User {\n")
    (insert "  login\n")
    (insert "  avatarUrl\n")
    (insert "}"))

  ;; Create a PR fragment that includes user fragment
  (with-temp-file (expand-file-name "fragments/pr-fields.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "${include:fragments/user-fields.graphql}\n")
    (insert "\n")
    (insert "fragment PullRequestFields on PullRequest {\n")
    (insert "  title\n")
    (insert "  body\n")
    (insert "  author {\n")
    (insert "    ...UserFields\n")
    (insert "  }\n")
    (insert "}"))

  ;; Create a query that includes the PR fragment
  (with-temp-file (expand-file-name "queries/get-pr.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "${include:fragments/pr-fields.graphql}\n")
    (insert "\n")
    (insert "query GetPullRequest($owner: String!, $repo: String!, $number: Int!) {\n")
    (insert "  repository(owner: $owner, name: $repo) {\n")
    (insert "    pullRequest(number: $number) {\n")
    (insert "      ...PullRequestFields\n")
    (insert "    }\n")
    (insert "  }\n")
    (insert "}"))

  ;; Create a simple query without includes
  (with-temp-file (expand-file-name "queries/simple.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "query Simple {\n")
    (insert "  viewer {\n")
    (insert "    login\n")
    (insert "  }\n")
    (insert "}"))

  ;; Create circular include files for testing
  (with-temp-file (expand-file-name "circular-a.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "${include:circular-b.graphql}\n")
    (insert "# File A"))

  (with-temp-file (expand-file-name "circular-b.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "${include:circular-a.graphql}\n")
    (insert "# File B"))

  ;; Create a file with multiple includes
  (with-temp-file (expand-file-name "multi-include.graphql"
                                    test-approve-graphql--temp-dir)
    (insert "${include:fragments/user-fields.graphql}\n")
    (insert "\n")
    (insert "# Some content in between\n")
    (insert "\n")
    (insert "${include:queries/simple.graphql}\n")))

(defun test-approve-graphql--teardown-fixtures ()
  "Remove temporary test fixtures."
  (when (and test-approve-graphql--temp-dir
             (file-directory-p test-approve-graphql--temp-dir))
    (delete-directory test-approve-graphql--temp-dir t))
  (setq test-approve-graphql--temp-dir nil))

;;; Tests

(describe "approve-graphql"

  (before-each
    (test-approve-graphql--setup-fixtures)
    (setq approve-graphql-directory test-approve-graphql--temp-dir)
    (approve-graphql-clear-cache))

  (after-each
    (test-approve-graphql--teardown-fixtures))

  (describe "approve-graphql--resolve-path"

    (it "resolves relative paths to the graphql directory"
      (let ((approve-graphql-directory "/test/graphql"))
        (expect (approve-graphql--resolve-path "queries/test.graphql")
                :to-equal "/test/graphql/queries/test.graphql")))

    (it "handles nested paths"
      (let ((approve-graphql-directory "/test/graphql"))
        (expect (approve-graphql--resolve-path "deeply/nested/path/file.graphql")
                :to-equal "/test/graphql/deeply/nested/path/file.graphql"))))

  (describe "approve-graphql--read-file"

    (it "reads file contents successfully"
      (let ((content (approve-graphql--read-file
                      (expand-file-name "queries/simple.graphql"
                                        test-approve-graphql--temp-dir))))
        (expect content :to-match "query Simple")))

    (it "signals error for non-existent files"
      (expect (approve-graphql--read-file "/nonexistent/file.graphql")
              :to-throw 'approve-graphql-file-not-found)))

  (describe "approve-graphql-load"

    (it "loads a simple query without includes"
      (let ((query (approve-graphql-load "queries/simple.graphql")))
        (expect query :to-match "query Simple")
        (expect query :to-match "viewer")
        (expect query :not :to-match "\\${include:")))

    (it "expands single include directive"
      (let ((query (approve-graphql-load "fragments/pr-fields.graphql")))
        ;; Should contain the included UserFields fragment
        (expect query :to-match "fragment UserFields on User")
        (expect query :to-match "login")
        (expect query :to-match "avatarUrl")
        ;; Should contain the PR fields
        (expect query :to-match "fragment PullRequestFields on PullRequest")
        ;; Should not contain unexpanded include directives
        (expect query :not :to-match "\\${include:")))

    (it "expands nested includes"
      (let ((query (approve-graphql-load "queries/get-pr.graphql")))
        ;; Should contain UserFields (included by pr-fields)
        (expect query :to-match "fragment UserFields on User")
        ;; Should contain PullRequestFields
        (expect query :to-match "fragment PullRequestFields on PullRequest")
        ;; Should contain the query itself
        (expect query :to-match "query GetPullRequest")
        ;; No unexpanded includes
        (expect query :not :to-match "\\${include:")))

    (it "expands multiple includes in same file"
      (let ((query (approve-graphql-load "multi-include.graphql")))
        ;; Should contain UserFields
        (expect query :to-match "fragment UserFields on User")
        ;; Should contain Simple query
        (expect query :to-match "query Simple")
        ;; Should preserve content between includes
        (expect query :to-match "Some content in between")
        ;; No unexpanded includes
        (expect query :not :to-match "\\${include:")))

    (it "caches loaded queries"
      (let ((query1 (approve-graphql-load "queries/simple.graphql"))
            (query2 (approve-graphql-load "queries/simple.graphql")))
        (expect query1 :to-equal query2)
        ;; Verify it's actually cached
        (expect (hash-table-count approve-graphql--cache) :to-be-greater-than 0)))

    (it "bypasses cache when no-cache is t"
      (let ((query1 (approve-graphql-load "queries/simple.graphql")))
        ;; Modify the cached value to verify reload happens
        (puthash (approve-graphql--resolve-path "queries/simple.graphql")
                 "CACHED VALUE"
                 approve-graphql--cache)
        ;; Without no-cache, should return cached value
        (expect (approve-graphql-load "queries/simple.graphql")
                :to-equal "CACHED VALUE")
        ;; With no-cache, should reload
        (let ((reloaded (approve-graphql-load "queries/simple.graphql" t)))
          (expect reloaded :to-match "query Simple")
          (expect reloaded :not :to-equal "CACHED VALUE")))))

  (describe "circular include detection"

    (it "detects direct circular includes"
      (expect (approve-graphql-load "circular-a.graphql")
              :to-throw 'approve-graphql-circular-include))

    (it "provides useful error information"
      (condition-case err
          (approve-graphql-load "circular-a.graphql")
        (approve-graphql-circular-include
         (let ((data (cdr err)))
           ;; Error data should contain the file path that caused the cycle
           ;; circular-a includes circular-b, which tries to include circular-a again
           (expect (car data) :to-match "circular-a.graphql"))))))

  (describe "approve-graphql-clear-cache"

    (it "clears all cached queries"
      ;; Load something to populate cache
      (approve-graphql-load "queries/simple.graphql")
      (expect (hash-table-count approve-graphql--cache) :to-be-greater-than 0)
      ;; Clear cache
      (approve-graphql-clear-cache)
      (expect (hash-table-count approve-graphql--cache) :to-equal 0)))

  (describe "approve-graphql-reload"

    (it "forces reload from disk"
      ;; Load and cache
      (approve-graphql-load "queries/simple.graphql")
      ;; Corrupt cache
      (puthash (approve-graphql--resolve-path "queries/simple.graphql")
               "CORRUPTED"
               approve-graphql--cache)
      ;; Reload should get fresh content
      (let ((reloaded (approve-graphql-reload "queries/simple.graphql")))
        (expect reloaded :to-match "query Simple")
        (expect reloaded :not :to-equal "CORRUPTED"))))

  (describe "approve-graphql--include-regexp"

    (it "matches basic include directive"
      (let ((text "${include:file.graphql}"))
        (expect (string-match approve-graphql--include-regexp text) :not :to-be nil)
        (expect (match-string 1 text) :to-equal "file.graphql")))

    (it "matches include with path"
      (let ((text "${include:fragments/user.graphql}"))
        (expect (string-match approve-graphql--include-regexp text) :not :to-be nil)
        (expect (match-string 1 text) :to-equal "fragments/user.graphql")))

    (it "matches include in multiline content"
      (let ((text "# Header\n${include:test.graphql}\n# Footer"))
        (expect (string-match approve-graphql--include-regexp text) :not :to-be nil)
        (expect (match-string 1 text) :to-equal "test.graphql")))

    (it "does not match malformed directives"
      ;; Empty path - requires at least one character
      (expect (string-match approve-graphql--include-regexp "${include:}")
              :to-be nil)
      ;; Missing 'e' in 'include'
      (expect (string-match approve-graphql--include-regexp "${includ:file.graphql}")
              :to-be nil)
      ;; Missing opening brace
      (expect (string-match approve-graphql--include-regexp "$include:file.graphql}")
              :to-be nil))))

(provide 'test-approve-graphql)
;;; test-approve-graphql.el ends here
