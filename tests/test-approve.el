;;; test-approve.el --- Tests for approve.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The entry point: URL parsing and the section hook alias.

;;; Code:

(require 'buttercup)
(require 'approve)

(describe "approve-parse-url"
  (it "parses a github.com pull request URL"
    (expect (approve-parse-url "https://github.com/magit/magit/pull/5000")
            :to-equal '(:host "github.com" :owner "magit" :repo "magit"
                        :number 5000)))

  (it "ignores what follows the number"
    (dolist (url '("https://github.com/o/r/pull/7/files"
                   "https://github.com/o/r/pull/7/commits/abc123"
                   "https://github.com/o/r/pull/7#discussion_r1"
                   "https://github.com/o/r/pull/7?w=1"
                   "  https://github.com/o/r/pull/7/  "))
      (expect (plist-get (approve-parse-url url) :number) :to-equal 7)))

  (it "accepts GitHub Enterprise hosts"
    (expect (approve-parse-url "https://git.example.com/team/app/pull/12")
            :to-equal '(:host "git.example.com" :owner "team" :repo "app"
                        :number 12)))

  (it "returns nil for anything that is not a pull request URL"
    (dolist (url '("https://github.com/o/r/issues/7"
                   "https://github.com/o/r/pulls/7"
                   "https://github.com/o/r"
                   "github.com/o/r/pull/7"
                   "https://github.com/o/r/pull/x"
                   ""
                   nil))
      (expect (approve-parse-url url) :to-be nil))))

(describe "approve-add-section-hook"
  (it "is an alias of magit-add-section-hook"
    (expect (symbol-function 'approve-add-section-hook)
            :to-be 'magit-add-section-hook)))

;;; test-approve.el ends here
