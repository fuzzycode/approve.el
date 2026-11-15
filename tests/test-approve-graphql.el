;;; test-approve-graphql.el --- Tests for approve-graphql -*- lexical-binding: t; -*-

;;; Commentary:

;; The GraphQL file loader on temporary files: includes, nested
;; includes, dedupe, cycles, missing files, absolute names and the
;; cache.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'approve-graphql)

(defvar test-approve-graphql--directory nil
  "Temporary graphql directory of the current test.")

(defun test-approve-graphql--write (name content)
  "Write CONTENT to NAME under the test directory; return its file name."
  (let ((file (expand-file-name name test-approve-graphql--directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert content))
    file))

(defun test-approve-graphql--count (regexp string)
  "Return how often REGEXP matches in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defmacro test-approve-graphql--caught (&rest body)
  "Run BODY and return the error it signals, or nil."
  `(condition-case err (progn ,@body nil) (error err)))

(defvar test-approve-graphql--saved-directory nil
  "The value of `approve-graphql-directory' before the current test.")

(describe "approve-graphql-load"
  ;; Each test points `approve-graphql-directory' at its own temporary
  ;; directory; after-each restores the real one for later tests.
  (before-each
    (setq test-approve-graphql--saved-directory approve-graphql-directory)
    (setq test-approve-graphql--directory
          (file-name-as-directory (make-temp-file "approve-graphql-" t)))
    (setq approve-graphql-directory test-approve-graphql--directory)
    (approve-graphql-clear-cache))

  (after-each
    (setq approve-graphql-directory test-approve-graphql--saved-directory)
    (delete-directory test-approve-graphql--directory t)
    (approve-graphql-clear-cache))

  (it "returns a file without includes unchanged"
    (test-approve-graphql--write "queries/viewer.graphql"
                                 "query { viewer { login } }\n")
    (expect (approve-graphql-load "queries/viewer.graphql")
            :to-equal "query { viewer { login } }\n"))

  (it "replaces an include with the included file, keeping the text around it"
    (test-approve-graphql--write "fragments/user.graphql"
                                 "fragment UserFields on User { login }")
    (test-approve-graphql--write "queries/q.graphql"
                                 "before\n${include:fragments/user.graphql}\nafter")
    (expect (approve-graphql-load "queries/q.graphql")
            :to-equal "before\nfragment UserFields on User { login }\nafter"))

  (it "resolves include paths from the graphql directory, not the including file"
    (test-approve-graphql--write "fragments/user.graphql" "USER")
    (test-approve-graphql--write "fragments/pr.graphql"
                                 "${include:fragments/user.graphql} PR")
    (test-approve-graphql--write "queries/q.graphql"
                                 "${include:fragments/pr.graphql} Q")
    (expect (approve-graphql-load "queries/q.graphql") :to-equal "USER PR Q"))

  (it "expands several includes in one file"
    (test-approve-graphql--write "a.graphql" "A")
    (test-approve-graphql--write "b.graphql" "B")
    (test-approve-graphql--write "q.graphql"
                                 "${include:a.graphql}-${include:b.graphql}")
    (expect (approve-graphql-load "q.graphql") :to-equal "A-B"))

  (it "includes a file shared by two fragments only once (diamond)"
    (test-approve-graphql--write "fragments/user.graphql"
                                 "fragment UserFields on User { login }\n")
    (test-approve-graphql--write "fragments/comment.graphql"
                                 "${include:fragments/user.graphql}fragment C on X { a }\n")
    (test-approve-graphql--write "fragments/review.graphql"
                                 "${include:fragments/user.graphql}fragment R on Y { b }\n")
    (test-approve-graphql--write
     "queries/q.graphql"
     "${include:fragments/comment.graphql}${include:fragments/review.graphql}query Q { x }\n")
    (let ((text (approve-graphql-load "queries/q.graphql")))
      (expect (test-approve-graphql--count "fragment UserFields" text) :to-equal 1)
      (expect text :to-match "fragment C on X")
      (expect text :to-match "fragment R on Y")
      (expect text :to-match "query Q")
      (expect text :not :to-match (regexp-quote "${include:"))))

  (it "drops a repeated include directive in the same file"
    (test-approve-graphql--write "a.graphql" "A")
    (test-approve-graphql--write "q.graphql" "${include:a.graphql}|${include:a.graphql}|")
    (expect (approve-graphql-load "q.graphql") :to-equal "A||"))

  (it "starts every load afresh: dedupe is per load"
    (test-approve-graphql--write "a.graphql" "A")
    (test-approve-graphql--write "q1.graphql" "${include:a.graphql}1")
    (test-approve-graphql--write "q2.graphql" "${include:a.graphql}2")
    (expect (approve-graphql-load "q1.graphql") :to-equal "A1")
    (expect (approve-graphql-load "q2.graphql") :to-equal "A2"))

  (it "signals a cycle naming the chain of files"
    (test-approve-graphql--write "a.graphql" "${include:b.graphql}")
    (test-approve-graphql--write "b.graphql" "${include:c.graphql}")
    (test-approve-graphql--write "c.graphql" "${include:a.graphql}")
    (let ((err (test-approve-graphql--caught (approve-graphql-load "a.graphql"))))
      (expect (car err) :to-be 'approve-graphql-include-cycle)
      (expect (cadr err) :to-equal "a.graphql -> b.graphql -> c.graphql -> a.graphql")))

  (it "signals a cycle when a file includes itself"
    (test-approve-graphql--write "a.graphql" "x ${include:a.graphql}")
    (expect (approve-graphql-load "a.graphql")
            :to-throw 'approve-graphql-include-cycle))

  (it "signals errors under approve-graphql-error and approve-error"
    (test-approve-graphql--write "a.graphql" "${include:a.graphql}")
    (expect (approve-graphql-load "a.graphql") :to-throw 'approve-graphql-error)
    (expect (approve-graphql-load "a.graphql") :to-throw 'approve-error))

  (it "signals a missing file naming its path"
    (let ((err (test-approve-graphql--caught (approve-graphql-load "queries/nope.graphql"))))
      (expect (car err) :to-be 'approve-graphql-missing-file)
      (expect (cadr err) :to-equal "queries/nope.graphql")))

  (it "signals a missing include naming it and the file that includes it"
    (test-approve-graphql--write "queries/q.graphql" "${include:fragments/gone.graphql}")
    (let ((err (test-approve-graphql--caught (approve-graphql-load "queries/q.graphql"))))
      (expect (car err) :to-be 'approve-graphql-missing-file)
      (expect (cdr err) :to-equal '("fragments/gone.graphql"
                                    :included-from "queries/q.graphql"))))

  (it "loads an absolute file name, resolving its includes from the directory"
    (test-approve-graphql--write "fragments/user.graphql" "USER")
    (let ((file (test-approve-graphql--write "queries/q.graphql"
                                             "${include:fragments/user.graphql}!")))
      (expect (file-name-absolute-p file) :to-be-truthy)
      (expect (approve-graphql-load file) :to-equal "USER!")))

  (it "returns the cached text while no file changes"
    (test-approve-graphql--write "q.graphql" "Q")
    (expect (approve-graphql-load "q.graphql") :to-equal "Q")
    (spy-on 'insert-file-contents :and-call-through)
    (expect (approve-graphql-load "q.graphql") :to-equal "Q")
    (expect 'insert-file-contents :not :to-have-been-called))

  (it "reloads when an included file changes"
    (let ((fragment (test-approve-graphql--write "f.graphql" "old")))
      (test-approve-graphql--write "q.graphql" "${include:f.graphql}")
      (set-file-times fragment (encode-time '(0 0 0 1 1 2020 nil nil t)))
      (expect (approve-graphql-load "q.graphql") :to-equal "old")
      (test-approve-graphql--write "f.graphql" "new")
      (set-file-times fragment (encode-time '(0 0 0 2 1 2020 nil nil t)))
      (expect (approve-graphql-load "q.graphql") :to-equal "new")))

  (it "signals when a cached file has been deleted"
    (let ((file (test-approve-graphql--write "q.graphql" "Q")))
      (expect (approve-graphql-load "q.graphql") :to-equal "Q")
      (delete-file file)
      (expect (approve-graphql-load "q.graphql")
              :to-throw 'approve-graphql-missing-file)))

  (it "reads the files again after approve-graphql-clear-cache"
    (test-approve-graphql--write "q.graphql" "Q")
    (approve-graphql-load "q.graphql")
    (approve-graphql-clear-cache)
    (spy-on 'insert-file-contents :and-call-through)
    (expect (approve-graphql-load "q.graphql") :to-equal "Q")
    (expect 'insert-file-contents :to-have-been-called)))

(describe "approve-graphql-directory"
  (it "still holds the real queries after the loader tests"
    (expect (file-exists-p (expand-file-name "queries/pull-request.graphql"
                                             approve-graphql-directory))
            :to-be-truthy))
  ;; The default is computed when the library loads, so test the rule
  ;; that computes it on both layouts.
  (it "defaults to graphql/ beside the library when it exists, else ../graphql"
    (let ((root (file-name-as-directory (make-temp-file "approve-pkg-" t))))
      (unwind-protect
          (let ((lisp (expand-file-name "lisp/" root)))
            (make-directory lisp)
            (expect (approve-graphql--default-directory lisp)
                    :to-equal (expand-file-name "graphql" root))
            (make-directory (expand-file-name "graphql" lisp))
            (expect (approve-graphql--default-directory lisp)
                    :to-equal (expand-file-name "graphql" lisp)))
        (delete-directory root t)))))

(defun test-approve-graphql--names (regexp text)
  "Return the first groups of REGEXP's matches in TEXT, sorted, each once."
  (let ((names nil)
        (start 0))
    (while (string-match regexp text start)
      (cl-pushnew (match-string 1 text) names :test #'equal)
      (setq start (match-end 0)))
    (sort names #'string<)))

(describe "The graphql files"
  ;; GitHub refuses a document that defines a fragment or a variable it
  ;; doesn't use, and one that uses one it doesn't define.  Tests never
  ;; send them, so check that here, for every query and mutation.
  (it "use each fragment and variable they define, and define each they use"
    (dolist (file (directory-files-recursively approve-graphql-directory
                                               "\\.graphql\\'"))
      (unless (string-match-p "/fragments/" file)
        (let ((text (replace-regexp-in-string "#.*" "" (approve-graphql-load file))))
          (expect (cons file (test-approve-graphql--names
                              "\\.\\.\\.\\([A-Za-z_]+\\)" (replace-regexp-in-string
                                                         "\\.\\.\\. *on " "" text)))
                  :to-equal (cons file (test-approve-graphql--names
                                        "fragment +\\([A-Za-z_]+\\)" text)))
          (expect (cons file (test-approve-graphql--names
                              "\\$\\([A-Za-z]+\\)[^:A-Za-z0-9_]" text))
                  :to-equal (cons file (test-approve-graphql--names
                                        "\\$\\([A-Za-z]+\\) *:" text)))))))

  (it "leave totalCount out of the timeline, where GitHub counts every item type"
    ;; With itemTypes, timelineItems.totalCount still counts the items it
    ;; filters out, so a complete conversation would say more is loading.
    (let ((text (replace-regexp-in-string
                 "#.*" "" (approve-graphql-load "queries/pull-request-timeline.graphql"))))
      (expect (string-match-p "timelineItems([^)]*) *{[ \t\n]*pageInfo" text)
              :to-be-truthy))))

;;; test-approve-graphql.el ends here
