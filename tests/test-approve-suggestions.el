;;; test-approve-suggestions.el --- Tests for committing suggestions -*- lexical-binding: t; -*-

;;; Commentary:

;; Finding suggested changes in a comment's markdown, changing a file's
;; text with them, GitHub's commit message, and the commands x and b on
;; the medium PR: what goes to GitHub (a fake answers the blob query
;; and the mutations) and what the buffer shows (decision D41, flow
;; F23).

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-suggestions)
(require 'approve-test-helpers "helpers")

(defconst test-suggestions--mine '("PRRC_kwDOekjTBMNbgPvJ0X" . 0)
  "The viewer's suggestion in the medium PR, on parser.go:129–131.")

(defconst test-suggestions--mine-thread "PRRT_kwDOkNvF9T3yvL1od7"
  "The thread of `test-suggestions--mine'.")

(defconst test-suggestions--carols '("PRRC_kwDOa9UFdMMGdeiIcG" . 0)
  "carol's suggestion in the medium PR, on engine.go:549; its thread is resolved.")

(defvar test-suggestions--requests nil
  "The blob queries and mutations sent to the fake GitHub, the last first.")

(defvar test-suggestions--answers nil
  "Alist (QUERY-FILE . FUNCTION): FUNCTION of the request returns the data.")

(defun test-suggestions--file (count)
  "Return a file of COUNT lines, \"line N\", ending with a newline."
  (mapconcat (lambda (n) (format "line %d\n" n)) (number-sequence 1 count) ""))

(defun test-suggestions--blob (text)
  "Return GitHub's answer to the blob query: the file TEXT."
  `((repository (__typename . "Repository") (id . "R_1")
                (object (__typename . "Blob") (id . "B_1") (text . ,text) (isBinary)
                        (isTruncated) (byteSize . ,(string-bytes text))))))

(defun test-suggestions--answer-all ()
  "Make the fake GitHub answer the blob query, the commit and the resolves."
  (setq test-suggestions--answers
        `(("queries/blob.graphql"
           . ,(lambda (_) (test-suggestions--blob (test-suggestions--file 600))))
          ("mutations/create-commit-on-branch.graphql"
           . ,(lambda (_) '((createCommitOnBranch (commit (oid . "c0ffee1234567")
                                                          (url . "https://x"))))))
          ("queries/pull-request-changes.graphql"
           . ,(lambda (_) '((repository (pullRequest (__typename . "PullRequest") (id . "PR")
                                                     (headRefOid . "c0ffee1234567"))))))
          ("mutations/resolve-review-thread.graphql"
           . ,(lambda (request)
                `((resolveReviewThread
                   (thread (__typename . "PullRequestReviewThread")
                           (id . ,(alist-get 'threadId (plist-get request :variables)))
                           (isResolved . t)))))))))

(defmacro test-suggestions--with-pr (&rest body)
  "Run BODY in the medium PR's buffer, GitHub faked for blobs and mutations."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((replay approve-api-transport)
            (approve-api-transport
             (lambda (request)
               (if-let* ((answer (cdr (assoc (plist-get request :name)
                                             test-suggestions--answers))))
                   (progn
                     (push request test-suggestions--requests)
                     (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                                               `((data . ,(funcall answer request)))
                                               nil)))
                       (lambda () (cancel-timer timer))))
                 (funcall replay request))))
            (test-suggestions--requests nil)
            (test-suggestions--answers nil)
            (pr (approve-test-open-pr "medium")))
       (unwind-protect
           (progn
             (switch-to-buffer pr)
             (delete-other-windows)
             (with-current-buffer pr
               ,@body))
         (dolist (buffer (buffer-list))
           (when (string-prefix-p "*approve-input" (buffer-name buffer))
             (with-current-buffer buffer
               (setq approve-input--done t))
             (kill-buffer buffer)))
         (kill-buffer pr)))))

(defun test-suggestions--sent (name)
  "Return the variables of the requests to NAME, the first first."
  (reverse (delq nil (mapcar (lambda (request)
                               (and (equal (plist-get request :name) name)
                                    (plist-get request :variables)))
                             test-suggestions--requests))))

(defun test-suggestions--goto (text)
  "Move point to the first TEXT, showing the diffs it may be in."
  (dolist (path '("internal/parser/parser.go" "internal/engine/engine.go"))
    (unless (save-excursion (goto-char (point-min)) (search-forward text nil t))
      (approve-nav-goto-file path)))
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (error "%S is not in the buffer" text))
  (goto-char (match-beginning 0)))

(defun test-suggestions--input ()
  "Return the open input buffer, or nil."
  (seq-find (lambda (buffer) (string-prefix-p "*approve-input" (buffer-name buffer)))
            (buffer-list)))

(defun test-suggestions--wait ()
  "Wait until no action waits for GitHub."
  (approve-test-wait-for (lambda () (null (approve-session-get :inflight)))))

(defun test-suggestions--commit-text (variables)
  "Return the new text of the first file the commit VARIABLES add."
  (let-alist variables
    (decode-coding-string (base64-decode-string
                           (alist-get 'contents (aref .fileChanges.additions 0)))
                          'utf-8-unix)))

(describe "approve-suggestions-parse"
  (it "finds each suggestion block, its lines, and none in other code blocks"
    (expect (approve-suggestions-parse
             (concat "Use this:\r\n```suggestion\r\n  a\r\n\tb\r\n```\r\nor\n"
                     "```go\nx\n```\n"
                     "~~~suggestion\n~~~\n"
                     "````suggestion\nwith ``` inside\n````\n"
                     "```suggestion foo\nc\n```"))
            :to-equal '(("  a" "\tb") nil ("with ``` inside") ("c"))))

  (it "skips a suggestion inside another code block, and keeps an open one to the end"
    (expect (approve-suggestions-parse "````md\n```suggestion\nx\n```\n````\n")
            :to-be nil)
    (expect (approve-suggestions-parse "```suggestion\nx\ny") :to-equal '(("x" "y"))))

  (it "takes the fence's indentation off its lines"
    (expect (approve-suggestions-parse "  ```suggestion\n    a\n b\n  ```")
            :to-equal '(("  a" "b"))))

  (it "is not fooled by a word that only starts like suggestion"
    (expect (approve-suggestions-parse "```suggestions\nx\n```") :to-be nil)))

(describe "approve-suggestions-splice"
  (it "replaces a line, a range and nothing, from the bottom up"
    (expect (approve-suggestions-splice "a\nb\nc\nd\n" '((1 1 ("A")) (3 4 ("C" "D" "E"))))
            :to-equal "A\nb\nC\nD\nE\n"))

  (it "deletes the lines of an empty suggestion"
    (expect (approve-suggestions-splice "a\nb\nc\n" '((2 2 nil))) :to-equal "a\nc\n"))

  (it "keeps CRLF line ends and a file without a last newline"
    (expect (approve-suggestions-splice "a\r\nb\r\nc" '((2 3 ("B" "C"))))
            :to-equal "a\r\nB\r\nC")
    (expect (approve-suggestions-splice "a\nb" '((2 2 nil))) :to-equal "a")
    (expect (approve-suggestions-splice "a\nb\n" '((1 1 ("x" "y")))) :to-equal "x\ny\nb\n"))

  (it "refuses lines that are not in the file"
    (expect (approve-suggestions-splice "a\n" '((2 2 ("b")))) :to-throw 'error))

  (it "gives lines without their ends"
    (expect (approve-suggestions-lines "a\r\nb\nc" 2 3) :to-equal '("b" "c"))
    (expect (approve-suggestions-lines "a\n" 1 2) :to-be nil)))

(describe "approve-suggestions-message"
  (let ((bob '((__typename . "User") (login . "bob") (name . "Bob B") (databaseId . 7)))
        (bot '((__typename . "Bot") (login . "github-actions") (databaseId . 41898282)))
        (me '((__typename . "User") (login . "me") (name . "Me") (databaseId . 1))))
    (cl-flet ((by (author &optional mine)
                (list :comment `((author . ,author) (viewerDidAuthor . ,mine)))))
      (it "names one suggester, with a trailer of their noreply address"
        (expect (approve-suggestions-message (list (by bob)))
                :to-equal "Apply suggestion from @bob\n\nCo-authored-by: Bob B <7+bob@users.noreply.github.com>\n")
        (expect (approve-suggestions-message (list (by bot)))
                :to-equal "Apply suggestion from @github-actions[bot]\n\nCo-authored-by: github-actions[bot] <41898282+github-actions[bot]@users.noreply.github.com>\n"))

      (it "has no trailer for the viewer, and one per suggester in a batch"
        (expect (approve-suggestions-message (list (by me t))) :to-equal "Apply suggestion from @me")
        (expect (approve-suggestions-message (list (by me t) (by bob) (by bot) (by bob)))
                :to-equal (concat "Apply batched suggestions from code review\n\n"
                                  "Co-authored-by: Bob B <7+bob@users.noreply.github.com>\n"
                                  "Co-authored-by: github-actions[bot] <41898282+github-actions[bot]@users.noreply.github.com>\n")))

      (it "leaves out a suggester without a database id"
        (expect (approve-suggestions-message (list (by '((login . "old")))))
                :to-equal "Apply suggestion from @old")))))

(describe "Suggestions in the medium PR"
  (before-each
    (spy-on 'message))

  (it "is the viewer's suggestion at point, which can be committed"
    (test-suggestions--with-pr
      (test-suggestions--goto "│ +\tresult = append")
      (let ((context (approve-context-at-point)))
        (expect (cons (plist-get context :comment-id) (plist-get context :index))
                :to-equal test-suggestions--mine))
      (let ((suggestion (approve-suggestions-get test-suggestions--mine)))
        (expect (plist-get suggestion :path) :to-equal "internal/parser/parser.go")
        (expect (list (plist-get suggestion :start) (plist-get suggestion :end))
                :to-equal '(129 131))
        (expect (plist-get suggestion :lines)
                :to-equal '("\tresult = append(result, parseAll(ctx, table)...)"))
        (expect (approve-suggestions-why-not suggestion) :to-be nil))
      (expect (approve-action-why-not 'approve-commit-suggestion) :to-be nil)
      (expect (approve-action-why-not 'approve-batch-suggestion) :to-be nil)))

  (it "says why a suggestion can't be committed, in GitHub's words"
    (test-suggestions--with-pr
      (let ((thread test-suggestions--mine-thread))
        (cl-flet ((why (&rest fields)
                    (approve-model-patch
                     `((__typename . "PullRequestReviewThread") (id . ,thread) ,@fields))
                    (prog1 (approve-suggestions-why-not
                            (approve-suggestions-get test-suggestions--mine))
                      (approve-model-patch
                       `((__typename . "PullRequestReviewThread") (id . ,thread)
                         (isOutdated) (line . 131) (diffSide . "RIGHT")
                         (startDiffSide . "RIGHT") (subjectType . "LINE"))))))
          (expect (why '(isOutdated . t)) :to-match "outdated")
          (expect (why '(startDiffSide . "LEFT"))
                  :to-equal "Unable to apply suggestions on deleted lines")
          (expect (why '(subjectType . "FILE")) :to-match "whole file")))
      (approve-model-patch `((__typename . "PullRequestReviewComment")
                             (id . ,(car test-suggestions--mine)) (state . "PENDING")))
      (expect (approve-suggestions-why-not (approve-suggestions-get test-suggestions--mine))
              :to-equal "This suggestion is part of a pending comment and cannot be applied")
      (approve-model-patch `((__typename . "PullRequest")
                             (id . ,(approve-model-ref-id (approve-model-root-ref)))
                             (state . "CLOSED")))
      (test-suggestions--goto "│ +\tresult = append")
      (expect (approve-action-why-not 'approve-commit-suggestion)
              :to-equal "Suggestions can't be applied: the pull request is closed")))

  (it "refuses a suggestion that changes nothing"
    (test-suggestions--with-pr
      (let ((suggestion (approve-suggestions-get test-suggestions--mine)))
        (expect (approve-suggestions-why-not
                 (plist-put (copy-sequence suggestion) :lines
                            (approve-suggestions--patch-lines suggestion)))
                :to-equal "This suggestion is invalid because no changes were made to the code"))))

  (it "says what to do on a comment without one, or with several"
    (test-suggestions--with-pr
      (test-suggestions--goto "Applied in the next commit.")
      (expect (approve-action-why-not 'approve-commit-suggestion)
              :to-equal "This comment has no suggested change")
      (test-suggestions--goto "These three lines can be one call")
      (expect (approve-action-why-not 'approve-commit-suggestion) :to-be nil)))

  (it "x commits the suggestion with the message written, resolves its thread, and loads the PR"
    (test-suggestions--with-pr
      (spy-on 'approve-pr-refresh)
      (test-suggestions--answer-all)
      (test-suggestions--goto "│ +\tresult = append")
      (call-interactively #'approve-commit-suggestion)
      (with-current-buffer (test-suggestions--input)
        (expect (buffer-string) :to-equal "Apply suggestion from @vera-example")
        (goto-char (point-max))
        (insert " to parse all")
        (approve-input-commit))
      (test-suggestions--wait)
      (expect (car (test-suggestions--sent "queries/blob.graphql"))
              :to-equal `((owner . "acme-example") (name . "rocket")
                          (expression . ,(concat (approve-pr-field 'headRefOid)
                                                 ":internal/parser/parser.go"))))
      (let ((commit (car (test-suggestions--sent "mutations/create-commit-on-branch.graphql"))))
        (expect (alist-get 'message commit)
                :to-equal '((headline . "Apply suggestion from @vera-example to parse all")))
        (expect (alist-get 'expectedHeadOid commit) :to-equal (approve-pr-field 'headRefOid))
        (expect (approve-suggestions-lines (test-suggestions--commit-text commit) 127 131)
                :to-equal '("line 127" "line 128"
                            "\tresult = append(result, parseAll(ctx, table)...)"
                            "line 132" "line 133")))
      (expect (test-suggestions--sent "mutations/resolve-review-thread.graphql")
              :to-equal `(((threadId . ,test-suggestions--mine-thread))))
      (expect 'approve-pr-refresh :to-have-been-called)))

  (it "C-u x commits at once with the default message"
    (test-suggestions--with-pr
      (spy-on 'approve-pr-refresh)
      (test-suggestions--answer-all)
      (test-suggestions--goto "│ +\tresult = append")
      (approve-commit-suggestion t)
      (test-suggestions--wait)
      (expect (test-suggestions--input) :to-be nil)
      (expect (alist-get 'message (car (test-suggestions--sent
                                        "mutations/create-commit-on-branch.graphql")))
              :to-equal '((headline . "Apply suggestion from @vera-example")))))

  (it "commits nothing when the file holds the suggestion already"
    (test-suggestions--with-pr
      (spy-on 'approve-pr-refresh)
      (test-suggestions--answer-all)
      (approve-model-patch
       `((__typename . "PullRequestReviewComment") (id . ,(car test-suggestions--mine))
         (body . "```suggestion\nline 129\nline 130\nline 131\n```")))
      (test-suggestions--goto "│ +\tresult = append")
      (approve-commit-suggestion t)
      (test-suggestions--wait)
      (expect (test-suggestions--sent "queries/blob.graphql") :not :to-be nil)
      (expect (test-suggestions--sent "mutations/create-commit-on-branch.graphql") :to-be nil)
      (expect 'approve-pr-refresh :not :to-have-been-called)))

  (it "b adds suggestions to the batch and takes them out; x commits the batch in one commit"
    (test-suggestions--with-pr
      (spy-on 'approve-pr-refresh)
      (test-suggestions--answer-all)
      (test-suggestions--goto "│ +\tresult = append")
      (call-interactively #'approve-batch-suggestion)
      (expect (approve-suggestions-batch) :to-equal (list test-suggestions--mine))
      (expect (substring-no-properties approve-pr--header-line)
              :to-match "1 suggestion in the batch")
      (test-suggestions--goto "│ +\tresult = append")
      (search-backward "Suggested change")
      (expect (seq-some (lambda (overlay)
                          (string-match-p "✓ in the batch"
                                          (or (overlay-get overlay 'display) "")))
                        (overlays-in (line-beginning-position) (line-end-position)))
              :to-be-truthy)
      (test-suggestions--goto "\t\treturn nil, fmt.Errorf(\"schedule: %w\", err)")
      (search-backward "Suggested change")
      (call-interactively #'approve-batch-suggestion)
      (expect (approve-suggestions-batch)
              :to-equal (list test-suggestions--mine test-suggestions--carols))
      (test-suggestions--goto "These three lines can be one call")
      (approve-commit-suggestion t)
      (test-suggestions--wait)
      (let ((commit (car (test-suggestions--sent "mutations/create-commit-on-branch.graphql"))))
        (expect (let-alist commit .message.headline)
                :to-equal "Apply batched suggestions from code review")
        (expect (let-alist commit .message.body)
                :to-equal "Co-authored-by: Carol Example <1023757+carol-example@users.noreply.github.com>")
        (expect (mapcar (lambda (file) (alist-get 'path file))
                        (let-alist commit .fileChanges.additions))
                :to-equal '("internal/parser/parser.go" "internal/engine/engine.go")))
      ;; carol's thread is resolved already.
      (expect (test-suggestions--sent "mutations/resolve-review-thread.graphql")
              :to-equal `(((threadId . ,test-suggestions--mine-thread))))
      (expect (approve-suggestions-batch) :to-be nil)))

  (it "b refuses a second suggestion on the same lines, and C-u b empties the batch"
    (test-suggestions--with-pr
      (approve-session-put :suggestions (list (cons "PRRC_other" 0)))
      (spy-on 'approve-suggestions-get :and-call-fake
              (lambda (id &optional _thread)
                (list :id id :path "internal/parser/parser.go" :start 131 :end 131
                      :lines '("x") :thread '((id . "T") (line . 131) (diffSide . "RIGHT")))))
      (test-suggestions--goto "│ +\tresult = append")
      (expect (approve-action-why-not 'approve-batch-suggestion)
              :to-equal "Unable to commit due to other pending changes affecting this line")
      (approve-batch-suggestion t)
      (expect (approve-suggestions-batch) :to-be nil)
      (expect (approve-batch-suggestion t) :to-throw 'user-error))))

;;; test-approve-suggestions.el ends here
