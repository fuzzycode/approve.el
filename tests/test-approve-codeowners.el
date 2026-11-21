;;; test-approve-codeowners.el --- Tests for reading CODEOWNERS -*- lexical-binding: t; -*-

;;; Commentary:

;; Who owns a path, as GitHub reads a CODEOWNERS file (decision D54):
;; the patterns of GitHub's documentation, comments and escapes, the
;; last pattern that matches winning, and owners compared ignoring
;; case.

;;; Code:

(require 'buttercup)
(require 'approve-codeowners)

(defun test-codeowners--matches (pattern path)
  "Return non-nil if PATTERN covers PATH."
  (let ((case-fold-search nil))
    (string-match-p (approve-codeowners-pattern-regexp pattern) path)))

(describe "approve-codeowners-pattern-regexp"
  (it "matches a name alone at any depth, and what is under it"
    (dolist (path '("app.js" "src/app.js" "a/b/c/app.js"))
      (expect (test-codeowners--matches "*.js" path) :to-be-truthy))
    (expect (test-codeowners--matches "*.js" "app.jsx") :not :to-be-truthy)
    (expect (test-codeowners--matches "*" "deep/inside/any.file") :to-be-truthy)
    ;; apps/ is any directory named apps, wherever it is.
    (dolist (path '("apps/x.go" "web/apps/y/z.go"))
      (expect (test-codeowners--matches "apps/" path) :to-be-truthy))
    (expect (test-codeowners--matches "apps/" "apps") :not :to-be-truthy)
    (expect (test-codeowners--matches "apps/" "myapps/x.go") :not :to-be-truthy))

  (it "anchors a pattern with a leading or middle slash to the root"
    (expect (test-codeowners--matches "/build/logs/" "build/logs/a/b.log") :to-be-truthy)
    (expect (test-codeowners--matches "/build/logs/" "src/build/logs/b.log") :not :to-be-truthy)
    (expect (test-codeowners--matches "docs/getting-started.md" "docs/getting-started.md")
            :to-be-truthy)
    (expect (test-codeowners--matches "docs/getting-started.md" "x/docs/getting-started.md")
            :not :to-be-truthy)
    ;; A path names a file, or a directory and all under it.
    (expect (test-codeowners--matches "/scripts" "scripts/release.sh") :to-be-truthy))

  (it "takes the files right in a directory with dir/*, not deeper ones"
    (expect (test-codeowners--matches "docs/*" "docs/getting-started.md") :to-be-truthy)
    (expect (test-codeowners--matches "docs/*" "docs/build-app/troubleshooting.md")
            :not :to-be-truthy))

  (it "crosses directories with **"
    (dolist (path '("logs/a" "build/logs/a" "deeply/nested/logs/a.txt"))
      (expect (test-codeowners--matches "**/logs" path) :to-be-truthy))
    (expect (test-codeowners--matches "/src/**/test.go" "src/test.go") :to-be-truthy)
    (expect (test-codeowners--matches "/src/**/test.go" "src/a/b/test.go") :to-be-truthy)
    (expect (test-codeowners--matches "/src/**" "src/a/b/c.go") :to-be-truthy))

  (it "keeps ? and * within a directory, and backslashes literal"
    (expect (test-codeowners--matches "/src/?.go" "src/a.go") :to-be-truthy)
    (expect (test-codeowners--matches "/src/?.go" "src/ab.go") :not :to-be-truthy)
    (expect (test-codeowners--matches "/src/*.go" "src/sub/a.go") :not :to-be-truthy)
    (expect (test-codeowners--matches "/a+b(c).txt" "a+b(c).txt") :to-be-truthy)
    (expect (test-codeowners--matches "\\#notes" "#notes") :to-be-truthy))

  (it "tells upper from lower case, as GitHub does"
    (expect (test-codeowners--matches "/README.md" "readme.md") :not :to-be-truthy)))

(describe "approve-codeowners-parse and approve-codeowners-owners"
  (let ((text (concat "# The owners of acme/rocket\n"
                      "\n"
                      "*       @acme/core   # everything, at first\n"
                      "*.js    @alice @acme/web\n"
                      "/docs/  docs@example.com\n"
                      "/docs/generated/\n"
                      "my\\ file.txt @bob\n")))
    (it "reads a pattern and its owners on each line, leaving comments out"
      (let ((rules (approve-codeowners-parse text)))
        (expect (length rules) :to-equal 5)
        ;; The last line comes first: it wins.
        (expect (cdr (car rules)) :to-equal '("@bob"))
        (expect (cdr (car (last rules))) :to-equal '("@acme/core"))))

    (it "gives a path the owners of the last pattern that matches it"
      (let ((rules (approve-codeowners-parse text)))
        (expect (approve-codeowners-owners rules "Makefile") :to-equal '("@acme/core"))
        (expect (approve-codeowners-owners rules "web/app.js")
                :to-equal '("@alice" "@acme/web"))
        (expect (approve-codeowners-owners rules "docs/usage.md")
                :to-equal '("docs@example.com"))
        ;; A pattern without owners leaves its files without any.
        (expect (approve-codeowners-owners rules "docs/generated/api.md") :to-be nil)
        (expect (approve-codeowners-owners rules "a/my file.txt") :to-equal '("@bob"))))

    (it "says whether one of some owners owns a path, ignoring their case"
      (let ((owned (approve-codeowners-predicate (approve-codeowners-parse text)
                                                 '("@ACME/Web"))))
        (expect (funcall owned "src/app.js") :to-be t)
        (expect (funcall owned "src/app.go") :to-be nil)
        ;; Asked again, the answer is kept.
        (expect (funcall owned "src/app.go") :to-be nil)))

    (it "has no rules for no text"
      (expect (approve-codeowners-parse nil) :to-be nil)
      (expect (approve-codeowners-owners nil "a.go") :to-be nil))))

(provide 'test-approve-codeowners)

;;; test-approve-codeowners.el ends here
