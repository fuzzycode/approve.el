;;; test-approve-graphql.el --- Tests for approve-graphql -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;;; Commentary:

;; Buttercup tests for the GraphQL fragment loading and include
;; resolution system.

;;; Code:

(require 'buttercup)
(require 'approve-graphql)

(defvar test-approve-graphql--base-dir nil
  "Temporary directory used as the GraphQL base directory in tests.")

(defun test-approve-graphql--setup-file (relative-path content)
  "Create a file at RELATIVE-PATH under the test base directory with CONTENT."
  (let ((absolute (expand-file-name relative-path test-approve-graphql--base-dir)))
    (make-directory (file-name-directory absolute) t)
    (with-temp-file absolute
      (insert content))
    absolute))

(describe "approve-graphql-load"

  (before-each
    (setq test-approve-graphql--base-dir
          (make-temp-file "approve-graphql-test-" t)))

  (after-each
    (delete-directory test-approve-graphql--base-dir t))

  (it "loads a simple query without includes"
    (test-approve-graphql--setup-file
     "queries/simple.graphql"
     "query { viewer { login } }")
    (expect (approve-graphql-load "queries/simple.graphql"
                                  :base-directory test-approve-graphql--base-dir)
            :to-equal "query { viewer { login } }"))

  (it "resolves a single include directive"
    (test-approve-graphql--setup-file
     "fragments/actor.graphql"
     "fragment Actor on Actor {\n  login\n  avatarUrl\n}")
    (test-approve-graphql--setup-file
     "queries/pr.graphql"
     "query {\n  repository {\n    pullRequest {\n      author {\n        ${include:fragments/actor.graphql}\n      }\n    }\n  }\n}")
    (let ((result (approve-graphql-load "queries/pr.graphql"
                                        :base-directory test-approve-graphql--base-dir)))
      (expect result :to-match "fragment Actor on Actor")
      (expect result :not :to-match "\\${include:")))

  (it "resolves nested includes"
    (test-approve-graphql--setup-file
     "fragments/date.graphql"
     "createdAt\nupdatedAt")
    (test-approve-graphql--setup-file
     "fragments/comment.graphql"
     "fragment Comment on IssueComment {\n  body\n  ${include:fragments/date.graphql}\n}")
    (test-approve-graphql--setup-file
     "queries/comments.graphql"
     "query {\n  ${include:fragments/comment.graphql}\n}")
    (let ((result (approve-graphql-load "queries/comments.graphql"
                                        :base-directory test-approve-graphql--base-dir)))
      (expect result :to-match "fragment Comment on IssueComment")
      (expect result :to-match "createdAt")
      (expect result :not :to-match "\\${include:")))

  (it "handles multiple includes in the same file"
    (test-approve-graphql--setup-file
     "fragments/actor.graphql"
     "fragment Actor on Actor { login }")
    (test-approve-graphql--setup-file
     "fragments/date.graphql"
     "createdAt\nupdatedAt")
    (test-approve-graphql--setup-file
     "queries/multi.graphql"
     "${include:fragments/actor.graphql}\n${include:fragments/date.graphql}")
    (let ((result (approve-graphql-load "queries/multi.graphql"
                                        :base-directory test-approve-graphql--base-dir)))
      (expect result :to-match "fragment Actor")
      (expect result :to-match "createdAt")
      (expect result :not :to-match "\\${include:")))

  (it "detects direct cyclic includes"
    (test-approve-graphql--setup-file
     "a.graphql"
     "${include:b.graphql}")
    (test-approve-graphql--setup-file
     "b.graphql"
     "${include:a.graphql}")
    (expect (approve-graphql-load "a.graphql"
                                  :base-directory test-approve-graphql--base-dir)
            :to-throw 'error))

  (it "detects indirect cyclic includes"
    (test-approve-graphql--setup-file
     "a.graphql"
     "${include:b.graphql}")
    (test-approve-graphql--setup-file
     "b.graphql"
     "${include:c.graphql}")
    (test-approve-graphql--setup-file
     "c.graphql"
     "${include:a.graphql}")
    (expect (approve-graphql-load "a.graphql"
                                  :base-directory test-approve-graphql--base-dir)
            :to-throw 'error))

  (it "deduplicates fragments included from multiple files"
    (test-approve-graphql--setup-file
     "fragments/shared.graphql"
     "fragment Shared on Node { id }")
    (test-approve-graphql--setup-file
     "a.graphql"
     "${include:fragments/shared.graphql}\nquery A { }")
    (test-approve-graphql--setup-file
     "b.graphql"
     "${include:fragments/shared.graphql}\nquery B { }")
    (test-approve-graphql--setup-file
     "root.graphql"
     "${include:a.graphql}\n${include:b.graphql}")
    (let ((result (approve-graphql-load "root.graphql"
                                        :base-directory test-approve-graphql--base-dir)))
      (expect result :to-match "query A")
      (expect result :to-match "query B")
      (expect result :not :to-match "\\${include:")
      ;; Fragment should appear exactly once
      (let ((count 0)
            (start 0))
        (while (string-match "fragment Shared" result start)
          (setq count (1+ count)
                start (match-end 0)))
        (expect count :to-equal 1))))

  (it "signals an error for missing files"
    (expect (approve-graphql-load "nonexistent.graphql"
                                  :base-directory test-approve-graphql--base-dir)
            :to-throw 'error))

  (it "signals an error for missing included files"
    (test-approve-graphql--setup-file
     "query.graphql"
     "${include:missing.graphql}")
    (expect (approve-graphql-load "query.graphql"
                                  :base-directory test-approve-graphql--base-dir)
            :to-throw 'error))

  (it "loads a file with an absolute path"
    (let ((abs-path (test-approve-graphql--setup-file
                     "absolute.graphql"
                     "query { absolute }")))
      (expect (approve-graphql-load abs-path
                                    :base-directory test-approve-graphql--base-dir)
              :to-equal "query { absolute }")))

  (it "preserves content around include directives"
    (test-approve-graphql--setup-file
     "fragments/f.graphql"
     "INSERTED")
    (test-approve-graphql--setup-file
     "query.graphql"
     "before ${include:fragments/f.graphql} after")
    (expect (approve-graphql-load "query.graphql"
                                  :base-directory test-approve-graphql--base-dir)
            :to-equal "before INSERTED after")))

;;; test-approve-graphql.el ends here
