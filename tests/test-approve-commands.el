;;; test-approve-commands.el --- Tests for the commands that change a PR -*- lexical-binding: t; -*-

;;; Commentary:

;; The commands of a PR buffer, run as a user would in the medium PR:
;; point on a thing, the command, the text in its input buffer, then
;; what goes to GitHub (a fake answers the mutations as GitHub would)
;; and what the buffer shows afterwards.  Also the submit and discard
;; commands of approve-review.el, and the keys.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-commands)
(require 'approve-review)
(require 'approve-menus)
(require 'approve-test-helpers "helpers")

(defconst test-commands--path "internal/parser/parser.go"
  "The medium PR's file the tests comment on.")

(defconst test-commands--thread "PRRT_kwDO85-qIzFQYiTYBM"
  "A published, unresolved thread on line 15 of parser.go.")

(defconst test-commands--pending "PRR_kwDO5qcMmbrbgz7Ae3"
  "The viewer's pending review in the medium PR.")

(defvar test-commands--requests nil
  "The mutations sent to the fake GitHub, the last first.")

(defvar test-commands--answer nil
  "A function of a mutation request that returns GitHub's data.")

(defmacro test-commands--with-pr (&rest body)
  "Run BODY in the medium PR's buffer, GitHub faked for mutations.
Queries are answered from the fixture, mutations by
`test-commands--answer'.  The buffer and any input are killed after."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((replay approve-api-transport)
            (approve-api-transport
             (lambda (request)
               (if (string-prefix-p "mutations/" (or (plist-get request :name) ""))
                   (progn
                     (push request test-commands--requests)
                     (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                                               `((data . ,(funcall test-commands--answer
                                                                   request)))
                                               nil)))
                       (lambda () (cancel-timer timer))))
                 (funcall replay request))))
            (test-commands--requests nil)
            (test-commands--answer nil)
            (pr (approve-test-open-pr "medium")))
       (unwind-protect
           (progn
             (switch-to-buffer pr)
             (delete-other-windows)
             (with-current-buffer pr
               ,@body))
         (test-commands--kill-inputs)
         (kill-buffer pr)))))

(defun test-commands--kill-inputs ()
  "Kill every input buffer, keeping no draft."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*approve-input" (buffer-name buffer))
      (with-current-buffer buffer
        (setq approve-input--done t))
      (kill-buffer buffer))))

(defun test-commands--input ()
  "Return the open input buffer, or nil."
  (seq-find (lambda (buffer) (string-prefix-p "*approve-input" (buffer-name buffer)))
            (buffer-list)))

(defun test-commands--send (text)
  "Write TEXT at the end of the open input and send it; wait for GitHub."
  (let ((input (or (test-commands--input) (error "No input buffer is open"))))
    (with-current-buffer input
      (goto-char (point-max))
      (insert text)
      (approve-input-commit)))
  (test-commands--wait))

(defun test-commands--wait ()
  "Wait until no action waits for GitHub."
  (approve-test-wait-for (lambda () (null (approve-session-get :inflight)))))

(defun test-commands--sent ()
  "Return the query file and the sorted variables of the last mutation."
  (let ((request (car test-commands--requests)))
    (list (plist-get request :name)
          (sort (copy-sequence (plist-get request :variables))
                (lambda (a b) (string< (car a) (car b)))))))

(defun test-commands--header ()
  "Return the header line of the open input, without properties."
  (with-current-buffer (test-commands--input)
    (substring-no-properties (eval (cadr header-line-format) t))))

(defun test-commands--goto-line (side number &optional path)
  "Move point to the diff line NUMBER on SIDE of PATH, expanding its file."
  (let ((path (or path test-commands--path)))
    (approve-nav-goto-file path)
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (let ((line (get-text-property (point) 'approve-line)))
          (if (and line (equal (plist-get line :path) path) (equal (plist-get line :side) side)
                   (eql (plist-get line (if (equal side "LEFT") :old :new)) number))
              (setq found (point))
            (forward-line 1))))
      (or found (error "No line %s %s:%s" side path number)))))

(defun test-commands--goto-section (type value)
  "Move point to the heading of the section of TYPE with VALUE.
The threads of parser.go are drawn once its diff is expanded."
  (unless (approve-test-find-section type value)
    (approve-nav-goto-file test-commands--path))
  (let ((section (or (approve-test-find-section type value)
                     (error "No section %s %s" type value))))
    (approve-commands--show-at section)))

(defun test-commands--thread-answer (id comment review &rest fields)
  "Return a new thread ID with one COMMENT of REVIEW, and FIELDS."
  `((__typename . "PullRequestReviewThread") (id . ,id) (isResolved)
    (viewerCanReply . t) ,@fields
    (comments (totalCount . 1) (pageInfo (hasNextPage) (endCursor . "x"))
              (nodes ((__typename . "PullRequestReviewComment") (id . ,comment)
                      (state . "PENDING") (body . "New") (bodyHTML . "<p>New</p>")
                      (author (__typename . "User") (id . "U_vera") (login . "vera-example"))
                      (pullRequestReview (__typename . "PullRequestReview") (id . ,review)))))))

(describe "The commands"
  (before-each
    (spy-on 'message))

  (describe "v (approve-toggle-viewed)"
    (it "marks the file at point in the file list viewed, then not viewed"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (request)
                `((,(if (string-match-p "unmark" (plist-get request :name))
                        'unmarkFileAsViewed 'markFileAsViewed)
                   (pullRequest (__typename . "PullRequest") (id . "x"))))))
        (test-commands--goto-section 'approve-file-entry-section "README.md")
        (call-interactively #'approve-toggle-viewed)
        (test-commands--wait)
        (expect (car (test-commands--sent)) :to-equal "mutations/mark-file-as-viewed.graphql")
        (expect (alist-get 'path (cadr (test-commands--sent))) :to-equal "README.md")
        (expect (approve-commands--viewed-state "README.md") :to-equal "VIEWED")
        (expect (oref (magit-current-section) type) :to-be 'approve-file-entry-section)
        (expect (oref (magit-current-section) value) :to-equal "README.md")
        (test-commands--goto-section 'approve-file-entry-section "README.md")
        (call-interactively #'approve-toggle-viewed)
        (test-commands--wait)
        (expect (car (test-commands--sent)) :to-equal "mutations/unmark-file-as-viewed.graphql")
        (expect (approve-commands--viewed-state "README.md") :to-equal "UNVIEWED")))

    (it "folds a file marked viewed from its diff and moves to the next unviewed one"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_) '((markFileAsViewed (pullRequest (__typename . "PullRequest") (id . "x"))))))
        (test-commands--goto-line "RIGHT" 14)
        (call-interactively #'approve-toggle-viewed)
        (test-commands--wait)
        (expect (oref (approve-test-find-section 'approve-file-section test-commands--path) hidden)
                :to-be t)
        (let ((section (magit-current-section)))
          (expect (oref section type) :to-be 'approve-file-section)
          (expect (oref section value) :not :to-equal test-commands--path)
          (expect (approve-commands--viewed-state (oref section value))
                  :not :to-equal "VIEWED"))))

    (it "says what the command needs elsewhere"
      (test-commands--with-pr
        (goto-char (point-min))
        (expect (call-interactively #'approve-toggle-viewed) :to-throw 'user-error))))

  (describe "c (approve-comment)"
    (it "comments on a line, in the pending review, and shows the thread under it"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                `((addPullRequestReviewThread
                   (thread ,@(test-commands--thread-answer
                              "PRRT_new" "PRRC_new" test-commands--pending
                              `(path . ,test-commands--path) '(line . 14) '(diffSide . "RIGHT")
                              '(subjectType . "LINE")))))))
        (test-commands--goto-line "RIGHT" 14)
        (call-interactively #'approve-comment)
        (expect (test-commands--header) :to-match
                "Comment on internal/parser/parser.go:14 · pending review")
        (test-commands--send "Can this fail?")
        (expect (test-commands--sent)
                :to-equal `("mutations/add-pull-request-review-thread.graphql"
                            ((body . "Can this fail?") (line . 14) (path . ,test-commands--path)
                             (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp") (side . "RIGHT")
                             (subjectType . "LINE"))))
        (expect (approve-test-find-section 'approve-thread-section "PRRT_new") :not :to-be nil)))

    (it "comments on the lines of the region"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                `((addPullRequestReviewThread
                   (thread ,@(test-commands--thread-answer "PRRT_new" "PRRC_new"
                                                           test-commands--pending))))))
        (let ((transient-mark-mode t))
          (push-mark (test-commands--goto-line "LEFT" 13) t t)
          ;; A region ending at the start of a line leaves it out.
          (test-commands--goto-line "RIGHT" 14)
          (end-of-line)
          (call-interactively #'approve-comment))
        (expect (test-commands--header) :to-match "parser\\.go:-13–\\+14")
        (test-commands--send "These go together")
        (expect (cadr (test-commands--sent))
                :to-equal `((body . "These go together") (line . 14)
                            (path . ,test-commands--path)
                            (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp") (side . "RIGHT")
                            (startLine . 13) (startSide . "LEFT") (subjectType . "LINE")))))

    (it "refuses a region across hunks"
      (test-commands--with-pr
        (let ((transient-mark-mode t))
          (push-mark (test-commands--goto-line "RIGHT" 14) t t)
          (test-commands--goto-line "RIGHT" 54)
          (expect (call-interactively #'approve-comment)
                  :to-throw 'user-error '("A range must stay in one hunk of the diff"))
          (deactivate-mark))
        (expect test-commands--requests :to-be nil)))

    (it "comments on a whole file from its heading"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                `((addPullRequestReviewThread
                   (thread ,@(test-commands--thread-answer "PRRT_new" "PRRC_new"
                                                           test-commands--pending))))))
        (test-commands--goto-section 'approve-file-section test-commands--path)
        (call-interactively #'approve-comment)
        (test-commands--send "About the file")
        (expect (cadr (test-commands--sent))
                :to-equal `((body . "About the file") (path . ,test-commands--path)
                            (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp")
                            (subjectType . "FILE")))))

    (it "joins the pending review even when a single comment is asked for, and says so"
      (test-commands--with-pr
        (test-commands--goto-line "RIGHT" 14)
        (let ((current-prefix-arg '(4)))
          (call-interactively #'approve-comment))
        (expect (test-commands--header) :to-match "no single comment while one is pending")))

    (it "posts a single comment as a review with its thread when nothing is pending"
      (test-commands--with-pr
        (approve-model-remove-ref (approve-model-root-ref) 'pendingReviews
                                  (approve-model-make-ref "PullRequestReview"
                                                          test-commands--pending))
        (setq test-commands--answer
              (lambda (_)
                `((addPullRequestReview
                   (pullRequestReview
                    (__typename . "PullRequestReview") (id . "PRR_single") (state . "COMMENTED")
                    (pullRequest (__typename . "PullRequest") (id . "PR_kwDOPCDj7SnvM_RcFp")
                                 (pendingReviews (nodes))
                                 (addedThreads
                                  (nodes ,(test-commands--thread-answer
                                           "PRRT_single" "PRRC_single" "PRR_single"
                                           `(path . ,test-commands--path) '(line . 14)
                                           '(diffSide . "RIGHT") '(subjectType . "LINE"))))))))))
        (test-commands--goto-line "RIGHT" 14)
        (let ((current-prefix-arg '(4)))
          (call-interactively #'approve-comment))
        (expect (test-commands--header) :to-match "single comment, published at once")
        (test-commands--send "Quick note")
        (expect (car (test-commands--sent)) :to-equal "mutations/add-pull-request-review.graphql")
        (expect (alist-get 'event (cadr (test-commands--sent))) :to-equal "COMMENT")
        (expect (approve-test-find-section 'approve-thread-section "PRRT_single")
                :not :to-be nil)))

    (it "comments on the pull request where there is no code"
      (test-commands--with-pr
        (approve-nav-goto-section 'approve-description-section)
        (call-interactively #'approve-comment)
        (expect (test-commands--header) :to-match "Comment on #202 · conversation"))))

  (describe "s (approve-suggest)"
    (it "starts the comment with a suggestion of the lines"
      (test-commands--with-pr
        (test-commands--goto-line "RIGHT" 14)
        (call-interactively #'approve-suggest)
        (expect (with-current-buffer (test-commands--input) (buffer-string))
                :to-match "```suggestion\n.*lookup 14")
        (expect (test-commands--header) :to-match "Suggest a change on")))

    (it "refuses removed lines, which have no new version"
      (test-commands--with-pr
        (test-commands--goto-line "LEFT" 13)
        (expect (call-interactively #'approve-suggest) :to-throw 'user-error)
        (expect (test-commands--input) :to-be nil))))

  (describe "r (approve-reply)"
    (it "replies to the thread at point"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                `((addPullRequestReviewThreadReply
                   (comment (__typename . "PullRequestReviewComment") (id . "PRRC_reply")
                            (state . "PENDING") (body . "Done") (bodyHTML . "<p>Done</p>")
                            (pullRequestReview (__typename . "PullRequestReview")
                                               (id . ,test-commands--pending)))))))
        (test-commands--goto-section 'approve-thread-section test-commands--thread)
        (call-interactively #'approve-reply)
        (expect (test-commands--header)
                :to-match "Reply to @carol-example on internal/parser/parser.go:15 · pending review")
        (test-commands--send "Done")
        (expect (test-commands--sent)
                :to-equal `("mutations/add-pull-request-review-thread-reply.graphql"
                            ((body . "Done") (threadId . ,test-commands--thread))))
        (expect (approve-test-find-section 'approve-comment-section "PRRC_reply")
                :not :to-be nil)))

    (it "quotes the comment at point with a prefix argument"
      (test-commands--with-pr
        (test-commands--goto-section 'approve-comment-section "PRRC_kwDOjbJkqIaS7rYCer")
        (let ((current-prefix-arg '(4)))
          (call-interactively #'approve-reply))
        (expect (with-current-buffer (test-commands--input) (buffer-string)) :to-match "\\`> ")
        (expect (test-commands--header) :to-match "Reply to @alice-example")))

    (it "on a conversation comment, comments on the pull request quoting it"
      (test-commands--with-pr
        (test-commands--goto-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg")
        (call-interactively #'approve-reply)
        (expect (test-commands--header) :to-match "Comment on #202")
        (expect (with-current-buffer (test-commands--input) (buffer-string))
                :to-equal "> Thanks! I'll take a look today.\n\n"))))

  (describe "R (approve-resolve)"
    (it "resolves the thread at point, and refuses what the viewer can't"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                `((resolveReviewThread
                   (thread (__typename . "PullRequestReviewThread") (id . ,test-commands--thread)
                           (isResolved . t) (viewerCanResolve) (viewerCanUnresolve . t))))))
        (test-commands--goto-section 'approve-thread-section test-commands--thread)
        (call-interactively #'approve-resolve)
        (test-commands--wait)
        (expect (test-commands--sent)
                :to-equal `("mutations/resolve-review-thread.graphql"
                            ((threadId . ,test-commands--thread))))
        (expect (approve-model-get "PullRequestReviewThread" test-commands--thread 'isResolved)
                :to-be t)
        ;; Now resolved; this viewer may not unresolve it in the answer.
        (approve-model-patch `((__typename . "PullRequestReviewThread")
                               (id . ,test-commands--thread) (viewerCanUnresolve)))
        (test-commands--goto-section 'approve-thread-section test-commands--thread)
        (expect (call-interactively #'approve-resolve)
                :to-throw 'user-error '("You can't unresolve this thread")))))

  (describe "e (approve-edit)"
    (it "edits a comment from its markdown, and sends nothing when it is unchanged"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                '((updateIssueComment
                   (issueComment (__typename . "IssueComment") (id . "IC_kwDOVM-W6WgSu2tHHg")
                                 (body . "Thanks! Looking now.")
                                 (bodyHTML . "<p>Thanks! Looking now.</p>"))))))
        (test-commands--goto-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg")
        (call-interactively #'approve-edit)
        (expect (with-current-buffer (test-commands--input) (buffer-string))
                :to-equal "Thanks! I'll take a look today.")
        (test-commands--send "")
        (expect test-commands--requests :to-be nil)
        (test-commands--goto-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg")
        (call-interactively #'approve-edit)
        (with-current-buffer (test-commands--input)
          (erase-buffer))
        (test-commands--send "Thanks! Looking now.")
        (expect (test-commands--sent)
                :to-equal '("mutations/update-issue-comment.graphql"
                            ((body . "Thanks! Looking now.")
                             (commentId . "IC_kwDOVM-W6WgSu2tHHg"))))
        (expect (approve-test-section-text
                 (approve-test-find-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg"))
                :to-match "Looking now")))

    (it "refuses what the viewer may not edit, and the description for now"
      (test-commands--with-pr
        (test-commands--goto-section 'approve-comment-section "PRRC_kwDOk4pUe_h6k_jiHs")
        (expect (call-interactively #'approve-edit)
                :to-throw 'user-error '("You can't edit this comment"))
        (approve-nav-goto-section 'approve-description-section)
        (expect (call-interactively #'approve-edit) :to-throw 'user-error))))

  (describe "k (approve-delete)"
    (it "deletes the comment at point after asking"
      (test-commands--with-pr
        (setq test-commands--answer (lambda (_) '((deleteIssueComment (clientMutationId)))))
        (spy-on 'y-or-n-p :and-return-value nil)
        (test-commands--goto-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg")
        (call-interactively #'approve-delete)
        (expect 'y-or-n-p :to-have-been-called-with "Delete @bob-example's comment? ")
        (expect test-commands--requests :to-be nil)
        (spy-on 'y-or-n-p :and-return-value t)
        (call-interactively #'approve-delete)
        (test-commands--wait)
        (expect (test-commands--sent)
                :to-equal '("mutations/delete-issue-comment.graphql"
                            ((commentId . "IC_kwDOVM-W6WgSu2tHHg"))))
        (expect (approve-test-find-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg")
                :to-be nil))))

  (describe "C (approve-comment-on-pr)"
    (it "comments in the conversation, quoting the region, and goes to the comment"
      (test-commands--with-pr
        (setq test-commands--answer
              (lambda (_)
                '((addComment (commentEdge (node (__typename . "IssueComment") (id . "IC_new")
                                                 (body . "Agreed") (bodyHTML . "<p>Agreed</p>")
                                                 (author (__typename . "User") (id . "U_vera")
                                                         (login . "vera-example"))))))))
        (test-commands--goto-section 'approve-issue-comment-section "IC_kwDOVM-W6WgSu2tHHg")
        (let ((transient-mark-mode t))
          (search-forward "Thanks!")
          (push-mark (match-beginning 0) t t)
          (call-interactively #'approve-comment-on-pr))
        (expect (with-current-buffer (test-commands--input) (buffer-string))
                :to-equal "> Thanks!\n\n")
        (test-commands--send "Agreed")
        (expect (test-commands--sent)
                :to-equal '("mutations/add-comment.graphql"
                            ((body . "> Thanks!\n\nAgreed") (subjectId . "PR_kwDOPCDj7SnvM_RcFp"))))
        (expect (oref (magit-current-section) value) :to-equal "IC_new")))))

(describe "The pending review"
  (before-each
    (spy-on 'message))

  (it "is submitted with a verdict and a body written for it"
    (test-commands--with-pr
      (setq test-commands--answer
            (lambda (_)
              `((submitPullRequestReview
                 (pullRequestReview
                  (__typename . "PullRequestReview") (id . ,test-commands--pending)
                  (state . "CHANGES_REQUESTED") (body . "Two nits")
                  (pullRequest (__typename . "PullRequest") (id . "PR_kwDOPCDj7SnvM_RcFp")
                               (pendingReviews (nodes))))))))
      (call-interactively #'approve-submit-request-changes)
      (expect (test-commands--header) :to-match "Review body — Request changes · 3 comments")
      (test-commands--send "Two nits")
      (expect (test-commands--sent)
              :to-equal '("mutations/submit-pull-request-review.graphql"
                          ((body . "Two nits") (event . "REQUEST_CHANGES")
                           (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp"))))
      (expect (approve-test-find-section 'approve-pending-review-section) :to-be nil)))

  (it "approves at once with a prefix argument"
    (test-commands--with-pr
      (setq test-commands--answer
            (lambda (_)
              `((submitPullRequestReview
                 (pullRequestReview (__typename . "PullRequestReview") (id . ,test-commands--pending)
                                    (state . "APPROVED"))))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'approve-submit-approve))
      (test-commands--wait)
      (expect (test-commands--input) :to-be nil)
      (expect (cadr (test-commands--sent))
              :to-equal '((event . "APPROVE") (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp")))))

  (it "adds a review with the verdict when none is pending, and needs a body for a comment"
    (test-commands--with-pr
      (approve-model-remove-ref (approve-model-root-ref) 'pendingReviews
                                (approve-model-make-ref "PullRequestReview" test-commands--pending))
      (let ((current-prefix-arg '(4)))
        (expect (call-interactively #'approve-submit-comment)
                :to-throw 'user-error '("Comment needs a body when the review has no comments")))
      (setq test-commands--answer
            (lambda (_)
              '((addPullRequestReview
                 (pullRequestReview (__typename . "PullRequestReview") (id . "PRR_new")
                                    (state . "COMMENTED")
                                    (pullRequest (__typename . "PullRequest")
                                                 (id . "PR_kwDOPCDj7SnvM_RcFp")
                                                 (addedThreads (nodes))))))))
      (call-interactively #'approve-submit-comment)
      (test-commands--send "Looks fine overall")
      (expect (test-commands--sent)
              :to-equal '("mutations/add-pull-request-review.graphql"
                          ((body . "Looks fine overall") (event . "COMMENT")
                           (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp") (threadCount . 0))))))

  (it "can't be approved on the viewer's own pull request"
    (test-commands--with-pr
      (approve-model-patch '((__typename . "PullRequest") (id . "PR_kwDOPCDj7SnvM_RcFp")
                             (viewerDidAuthor . t)))
      (expect (call-interactively #'approve-submit-approve)
              :to-throw 'user-error '("GitHub doesn't let you approve your own pull request"))))

  (it "is discarded after asking"
    (test-commands--with-pr
      (setq test-commands--answer
            (lambda (_)
              `((deletePullRequestReview
                 (pullRequestReview (__typename . "PullRequestReview")
                                    (id . ,test-commands--pending))))))
      (spy-on 'yes-or-no-p :and-return-value t)
      (call-interactively #'approve-discard-review)
      (test-commands--wait)
      (expect 'yes-or-no-p :to-have-been-called-with
              "Discard your pending review (3 comments)? ")
      (expect (test-commands--sent)
              :to-equal `("mutations/delete-pull-request-review.graphql"
                          ((reviewId . ,test-commands--pending))))
      (expect (approve-test-find-section 'approve-pending-review-section) :to-be nil)
      (expect (call-interactively #'approve-discard-review)
              :to-throw 'user-error '("You have no pending review")))))

(describe "The keys"
  (it "bind the commands in the PR buffer"
    (dolist (binding '(("v" . approve-toggle-viewed) ("c" . approve-comment)
                       ("s" . approve-suggest) ("r" . approve-reply) ("R" . approve-resolve)
                       ("e" . approve-edit) ("k" . approve-delete)
                       ("C" . approve-comment-on-pr) ("S" . approve-submit-review)
                       ("X" . approve-discard-review)))
      (expect (keymap-lookup approve-pr-mode-map (car binding)) :to-be (cdr binding))))

  (it "bind evil's keys for them, in normal and visual state"
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state _map key def) (push (list state (key-description key) def) bound)))
                ((symbol-function 'evil-set-initial-state) #'ignore))
        (load "approve-evil" nil t))
      (dolist (binding '(("x" . approve-toggle-viewed) ("c" . approve-comment)
                         ("s" . approve-suggest) ("r" . approve-reply) ("R" . approve-resolve)
                         ("i" . approve-edit) ("D" . approve-delete)
                         ("C" . approve-comment-on-pr) ("S" . approve-submit-review)
                         ("X" . approve-discard-review)))
        (expect (member (list 'normal (car binding) (cdr binding)) bound) :to-be-truthy))
      (dolist (binding '(("c" . approve-comment) ("s" . approve-suggest)
                         ("r" . approve-reply) ("C" . approve-comment-on-pr)))
        (expect (member (list 'visual (car binding) (cdr binding)) bound) :to-be-truthy)))))

(provide 'test-approve-commands)

;;; test-approve-commands.el ends here
