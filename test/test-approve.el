;;; test-approve.el --- Tests for approve.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the main Approve entry point.

;;; Code:

(require 'buttercup)
(require 'approve)

;;; Tests

(describe "approve"

  (describe "approve--parse-pr-url"

    (describe "valid URLs"

      (it "parses standard github.com URL"
        (let ((result (approve--parse-pr-url
                       "https://github.com/fuzzycode/Approve.el/pull/42")))
          (expect result :to-equal '("fuzzycode" "Approve.el" 42))))

      (it "parses URL with trailing slash"
        (let ((result (approve--parse-pr-url
                       "https://github.com/owner/repo/pull/123/")))
          (expect result :to-equal '("owner" "repo" 123))))

      (it "parses URL with additional path segments"
        (let ((result (approve--parse-pr-url
                       "https://github.com/owner/repo/pull/1/files")))
          (expect result :to-equal '("owner" "repo" 1))))

      (it "parses URL with commits path"
        (let ((result (approve--parse-pr-url
                       "https://github.com/owner/repo/pull/99/commits")))
          (expect result :to-equal '("owner" "repo" 99))))

      (it "parses http URLs"
        (let ((result (approve--parse-pr-url
                       "http://github.com/owner/repo/pull/1")))
          (expect result :to-equal '("owner" "repo" 1))))

      (it "parses GitHub Enterprise URLs"
        (let ((result (approve--parse-pr-url
                       "https://github.mycompany.com/owner/repo/pull/42")))
          (expect result :to-equal '("owner" "repo" 42))))

      (it "parses URLs with complex owner names"
        (let ((result (approve--parse-pr-url
                       "https://github.com/my-org-name/my-repo/pull/1")))
          (expect result :to-equal '("my-org-name" "my-repo" 1))))

      (it "parses URLs with numeric repo names"
        (let ((result (approve--parse-pr-url
                       "https://github.com/owner/repo123/pull/456")))
          (expect result :to-equal '("owner" "repo123" 456))))

      (it "parses URLs with dots in repo name"
        (let ((result (approve--parse-pr-url
                       "https://github.com/owner/repo.el/pull/7")))
          (expect result :to-equal '("owner" "repo.el" 7)))))

    (describe "invalid URLs"

      (it "returns nil for non-GitHub URLs"
        (expect (approve--parse-pr-url "https://gitlab.com/owner/repo/pull/1")
                :to-be nil))

      (it "returns nil for non-PR URLs"
        (expect (approve--parse-pr-url "https://github.com/owner/repo")
                :to-be nil))

      (it "returns nil for issues URLs"
        (expect (approve--parse-pr-url "https://github.com/owner/repo/issues/1")
                :to-be nil))

      (it "returns nil for PR listing URLs"
        (expect (approve--parse-pr-url "https://github.com/owner/repo/pulls")
                :to-be nil))

      (it "returns nil for URLs without PR number"
        (expect (approve--parse-pr-url "https://github.com/owner/repo/pull/")
                :to-be nil))

      (it "returns nil for non-numeric PR numbers"
        (expect (approve--parse-pr-url "https://github.com/owner/repo/pull/abc")
                :to-be nil))

      (it "returns nil for random strings"
        (expect (approve--parse-pr-url "not a url at all")
                :to-be nil))

      (it "returns nil for empty string"
        (expect (approve--parse-pr-url "")
                :to-be nil))))

  (describe "approve-view-pr"

    (before-each
      (spy-on 'approve-ui-view-pr))

    (it "calls approve-ui-view-pr with parsed URL"
      (approve-view-pr "https://github.com/fuzzycode/Approve.el/pull/42")
      (expect 'approve-ui-view-pr :to-have-been-called-with
              "fuzzycode" "Approve.el" 42))

    (it "signals error for invalid URL"
      (expect (approve-view-pr "https://gitlab.com/owner/repo/pull/1")
              :to-throw 'user-error))

    (it "signals error for non-PR URL"
      (expect (approve-view-pr "https://github.com/owner/repo")
              :to-throw 'user-error))))

(provide 'test-approve)
;;; test-approve.el ends here
