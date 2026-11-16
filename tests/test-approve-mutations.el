;;; test-approve-mutations.el --- Tests for the mutations -*- lexical-binding: t; -*-

;;; Commentary:

;; Each mutation on the medium PR's store, through the mutation queue to
;; a fake transport that answers as GitHub would: what goes to GitHub,
;; and what the store holds once GitHub has answered (decision D35).

;;; Code:

(require 'buttercup)
(require 'approve-mutations)
(require 'approve-test-helpers "helpers")

(defconst test-mutations--pr "PR_kwDOPCDj7SnvM_RcFp"
  "The node id of the medium PR.")

(defconst test-mutations--pending "PRR_kwDO5qcMmbrbgz7Ae3"
  "The viewer's pending review in the medium PR.")

(defconst test-mutations--pending-comments
  '(("PRRC_kwDO9c1eOb8Z8RBSbC" . "PRRT_kwDOVCEPcVKg5CpjSd")
    ("PRRC_kwDO5ZpVeKUvCvYor_" . "PRRT_kwDO79Ci-laaE8gpgI")
    ("PRRC_kwDOl56Q4g39kaERSq" . "PRRT_kwDOdQlXJHIBEbMl4y"))
  "The pending review's comments, each with the thread it starts.")

(defconst test-mutations--thread "PRRT_kwDO85-qIzFQYiTYBM"
  "A published thread of the medium PR, with three comments.")

(defvar test-mutations--requests nil
  "The requests the fake transport got, the last first.")

(defvar test-mutations--answer nil
  "A function of a request that returns the body GitHub answers with.")

(defun test-mutations--transport (request)
  "Answer REQUEST with `test-mutations--answer', on a timer."
  (push request test-mutations--requests)
  (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                            (funcall test-mutations--answer request) nil)))
    (lambda () (cancel-timer timer))))

(defun test-mutations--answer-with (data)
  "Make GitHub answer the next requests with DATA."
  (setq test-mutations--answer (lambda (_request) `((data . ,(copy-tree data))))))

(defun test-mutations--load ()
  "Fill the current buffer's store with the medium PR."
  (approve-model-init (approve-test-fixture-meta "medium"))
  (dolist (query '("queries/pull-request.graphql" "queries/pull-request-files.graphql"
                   "queries/pull-request-threads.graphql"
                   "queries/pull-request-timeline.graphql"))
    (approve-model-load (copy-tree (let-alist (approve-test-fixture-data "medium" query)
                                     .repository.pullRequest))
                        t)))

(defmacro test-mutations--with-pr (&rest body)
  "Run BODY in a buffer holding the medium PR, GitHub being the fake."
  (declare (indent 0) (debug t))
  `(let ((approve-api-transport #'test-mutations--transport)
         (approve-graphql-directory approve-test-graphql-directory)
         (approve-api-timeout nil)
         (inhibit-message t)
         (test-mutations--requests nil)
         (test-mutations--answer nil))
     (with-temp-buffer
       (test-mutations--load)
       ,@body)))

(defun test-mutations--run (function &rest args)
  "Call the mutation FUNCTION with ARGS; return how it ended.
That is (t . RESULT) when it called its callback, (nil . ERROR) when
its errorback."
  (let ((outcome nil))
    (apply function (append args
                            (list :callback (lambda (result) (setq outcome (cons t result)))
                                  :errorback (lambda (err) (setq outcome (cons nil err))))))
    (approve-test-wait-for (lambda () outcome))
    outcome))

(defun test-mutations--sent ()
  "Return the query file and the variables of the last request, sorted."
  (let ((request (car test-mutations--requests)))
    (list (plist-get request :name)
          (sort (copy-sequence (plist-get request :variables))
                (lambda (a b) (string< (car a) (car b)))))))

(defun test-mutations--ref (typename id)
  "Return the ref to TYPENAME ID."
  (approve-model-make-ref typename id))

(defun test-mutations--ids (field &optional ref)
  "Return the ids of the nodes in FIELD of REF (default: the PR), in order."
  (mapcar #'approve-model-ref-id
          (approve-model-nodes (approve-model-raw-field (or ref (approve-model-root-ref))
                                                        field))))

(defun test-mutations--total (field &optional ref)
  "Return the totalCount of the list in FIELD of REF (default: the PR)."
  (approve-model-total-count
   (approve-model-raw-field (or ref (approve-model-root-ref)) field)))

(defun test-mutations--comment (id &rest fields)
  "Return a review comment ID of the pending review, with FIELDS, an alist."
  `((__typename . "PullRequestReviewComment") (id . ,id) ,@fields))

(defun test-mutations--review-state (&rest pending-comments)
  "Return the PR's review state as GitHub answers it.
With PENDING-COMMENTS, ids of comments, the pending review has them;
without, there is none."
  `((__typename . "PullRequest") (id . ,test-mutations--pr)
    (pendingReviews
     (nodes
      ,@(and pending-comments
             `(((__typename . "PullRequestReview") (id . ,test-mutations--pending)
                (state . "PENDING")
                (comments (totalCount . ,(length pending-comments))
                          (pageInfo (hasNextPage) (endCursor . "x"))
                          (nodes ,@(mapcar #'test-mutations--comment
                                           pending-comments))))))))))

(defun test-mutations--thread-answer (id comment review &rest fields)
  "Return a thread ID with one COMMENT of REVIEW, and FIELDS, an alist."
  `((__typename . "PullRequestReviewThread") (id . ,id) ,@fields
    (comments (totalCount . 1) (pageInfo (hasNextPage) (endCursor . "x"))
              (nodes ,(test-mutations--comment
                       comment `(pullRequestReview (__typename . "PullRequestReview")
                                                   (id . ,review)))))))

(describe "The mutations"
  (it "mark a file viewed, then not viewed, once GitHub has"
    (test-mutations--with-pr
      (test-mutations--answer-with
       `((markFileAsViewed (pullRequest (__typename . "PullRequest") (id . ,test-mutations--pr)))))
      (expect (test-mutations--run #'approve-mutation-set-file-viewed "README.md" t)
              :to-equal '(t . "VIEWED"))
      (expect (test-mutations--sent)
              :to-equal `("mutations/mark-file-as-viewed.graphql"
                          ((path . "README.md") (pullRequestId . ,test-mutations--pr))))
      (let ((states (mapcar (lambda (file) (alist-get 'viewerViewedState file))
                            (approve-pr-files))))
        (expect (seq-take states 4) :to-equal '("VIEWED" "DISMISSED" "VIEWED" "VIEWED")))
      (expect (test-mutations--run #'approve-mutation-set-file-viewed "README.md" nil)
              :to-equal '(t . "UNVIEWED"))
      (expect (car (test-mutations--sent)) :to-equal "mutations/unmark-file-as-viewed.graphql")
      (expect (alist-get 'viewerViewedState
                         (seq-find (lambda (file) (equal (alist-get 'path file) "README.md"))
                                   (approve-pr-files)))
              :to-equal "UNVIEWED")
      (expect (length (approve-pr-files)) :to-equal 30)))

  (it "refuse before sending when no pull request has loaded"
    (let ((approve-api-transport #'test-mutations--transport)
          (test-mutations--requests nil))
      (with-temp-buffer
        (expect (approve-mutation-set-file-viewed "a.el" t) :to-throw 'error)
        (expect test-mutations--requests :to-be nil))))

  (describe "approve-mutation-add-thread"
    (it "comments on a range; the thread joins the pending review GitHub answers with"
      (test-mutations--with-pr
        (test-mutations--answer-with
         `((addPullRequestReviewThread
            (thread ,@(test-mutations--thread-answer
                       "PRRT_new" "PRRC_new" test-mutations--pending
                       '(path . "internal/parser/parser.go") '(line . 14))
                    (pullRequest ,@(apply #'test-mutations--review-state
                                          (append (mapcar #'car test-mutations--pending-comments)
                                                  '("PRRC_new"))))))))
        (expect (test-mutations--run
                 #'approve-mutation-add-thread
                 '(:kind lines :path "internal/parser/parser.go" :side "RIGHT" :line 14
                         :start-side "RIGHT" :start-line 13 :url "https://x")
                 "Can this fail?")
                :to-equal `(t . ,(test-mutations--ref "PullRequestReviewThread" "PRRT_new")))
        (expect (test-mutations--sent)
                :to-equal `("mutations/add-pull-request-review-thread.graphql"
                            ((body . "Can this fail?") (line . 14)
                             (path . "internal/parser/parser.go")
                             (pullRequestId . ,test-mutations--pr) (side . "RIGHT")
                             (startLine . 13) (startSide . "RIGHT") (subjectType . "LINE"))))
        (expect (car (last (test-mutations--ids 'reviewThreads))) :to-equal "PRRT_new")
        (expect (test-mutations--total 'reviewThreads) :to-equal 21)
        (expect (alist-get 'id (car (last (approve-pr-threads-for-file
                                           "internal/parser/parser.go"))))
                :to-equal "PRRT_new")
        (expect (mapcar (lambda (comment) (alist-get 'id comment))
                        (approve-model-nodes (alist-get 'comments (approve-pr-pending-review))))
                :to-equal (append (mapcar #'car test-mutations--pending-comments) '("PRRC_new")))
        (expect (approve-model-raw-field (test-mutations--ref "PullRequestReviewThread" "PRRT_new")
                                         'pullRequest)
                :to-be nil)))

    (it "sends one line without a start, and a whole file as a FILE thread"
      (test-mutations--with-pr
        (test-mutations--answer-with
         `((addPullRequestReviewThread
            (thread ,@(test-mutations--thread-answer "PRRT_new" "PRRC_new" test-mutations--pending)
                    (pullRequest ,@(test-mutations--review-state "PRRC_new"))))))
        (dolist (target '((:kind line :path "a.go" :side "LEFT" :line 7 :old 7 :new nil)
                          (:kind lines :path "a.go" :side "LEFT" :line 7
                                 :start-side "LEFT" :start-line 7)))
          (test-mutations--run #'approve-mutation-add-thread target "One line")
          (expect (cadr (test-mutations--sent))
                  :to-equal `((body . "One line") (line . 7) (path . "a.go")
                              (pullRequestId . ,test-mutations--pr) (side . "LEFT")
                              (subjectType . "LINE"))))
        (test-mutations--run #'approve-mutation-add-thread
                             '(:kind file :path "a.go" :viewed "VIEWED") "The file")
        (expect (cadr (test-mutations--sent))
                :to-equal `((body . "The file") (path . "a.go")
                            (pullRequestId . ,test-mutations--pr) (subjectType . "FILE")))))

    (it "fails, changing nothing, when GitHub answers with no thread"
      (test-mutations--with-pr
        (test-mutations--answer-with '((addPullRequestReviewThread (thread))))
        (let ((outcome (test-mutations--run #'approve-mutation-add-thread
                                            '(:path "a.go" :side "RIGHT" :line 900) "Here")))
          (expect (car outcome) :to-be nil)
          (expect (cdr outcome) :to-equal '(approve-mutation-refused "that line takes no comment"))
          (expect (approve-api-error-message (cdr outcome)) :to-equal "that line takes no comment"))
        (expect (test-mutations--total 'reviewThreads) :to-equal 20))))

  (describe "approve-mutation-add-reply"
    (it "adds a published reply to its thread; its review stays out of the conversation"
      (test-mutations--with-pr
        (test-mutations--answer-with
         `((addPullRequestReviewThreadReply
            (comment (__typename . "PullRequestReviewComment") (id . "PRRC_reply")
                     (state . "SUBMITTED") (body . "Done")
                     (pullRequestReview (__typename . "PullRequestReview") (id . "PRR_reply")
                                        (state . "COMMENTED") (bodyHTML . ""))
                     (pullRequest ,@(test-mutations--review-state))))))
        (let ((thread (test-mutations--ref "PullRequestReviewThread" test-mutations--thread)))
          (expect (test-mutations--run #'approve-mutation-add-reply test-mutations--thread "Done")
                  :to-equal `(t . ,(test-mutations--ref "PullRequestReviewComment" "PRRC_reply")))
          (expect (test-mutations--sent)
                  :to-equal `("mutations/add-pull-request-review-thread-reply.graphql"
                              ((body . "Done") (threadId . ,test-mutations--thread))))
          (expect (car (last (test-mutations--ids 'comments thread))) :to-equal "PRRC_reply")
          (expect (test-mutations--total 'comments thread) :to-equal 4)
          ;; GitHub's timeline never lists a review that holds a reply alone.
          (expect (test-mutations--ids 'timelineItems) :not :to-contain "PRR_reply")
          (expect (length (test-mutations--ids 'timelineItems)) :to-equal 40)
          (expect (approve-pr-pending-review) :to-be nil))))

    (it "keeps a pending reply out of the conversation; it joins the pending review"
      (test-mutations--with-pr
        (test-mutations--answer-with
         `((addPullRequestReviewThreadReply
            (comment ,@(test-mutations--comment
                        "PRRC_reply" '(state . "PENDING")
                        `(pullRequestReview (__typename . "PullRequestReview")
                                            (id . ,test-mutations--pending)
                                            (state . "PENDING")))
                     (pullRequest ,@(apply #'test-mutations--review-state
                                           (append (mapcar #'car test-mutations--pending-comments)
                                                   '("PRRC_reply"))))))))
        (test-mutations--run #'approve-mutation-add-reply test-mutations--thread "Later")
        (expect (car (last (test-mutations--ids 'comments (test-mutations--ref
                                                           "PullRequestReviewThread"
                                                           test-mutations--thread))))
                :to-equal "PRRC_reply")
        (expect (length (test-mutations--ids 'timelineItems)) :to-equal 40)
        (expect (length (approve-model-nodes (alist-get 'comments (approve-pr-pending-review))))
                :to-equal 4))))

  (it "resolve and unresolve a thread"
    (test-mutations--with-pr
      (let ((thread (test-mutations--ref "PullRequestReviewThread" test-mutations--thread)))
        (dolist (resolved '(t nil))
          (test-mutations--answer-with
           `((,(if resolved 'resolveReviewThread 'unresolveReviewThread)
              (thread (__typename . "PullRequestReviewThread") (id . ,test-mutations--thread)
                      (isResolved . ,resolved)))))
          (expect (test-mutations--run (if resolved
                                           #'approve-mutation-resolve-thread
                                         #'approve-mutation-unresolve-thread)
                                       test-mutations--thread)
                  :to-equal (cons t thread))
          (expect (test-mutations--sent)
                  :to-equal `(,(if resolved
                                   "mutations/resolve-review-thread.graphql"
                                 "mutations/unresolve-review-thread.graphql")
                              ((threadId . ,test-mutations--thread))))
          (expect (approve-model-raw-field thread 'isResolved) :to-be resolved)
          (expect (test-mutations--total 'comments thread) :to-equal 3)))))

  (describe "approve-mutation-commit-files"
    (it "commits whole files to the head branch, from the head the store holds"
      (test-mutations--with-pr
        (test-mutations--answer-with
         '((createCommitOnBranch (commit (oid . "c0ffee") (url . "https://x/c0ffee")))))
        (let ((head (approve-pr-field 'headRefOid))
              (version (approve-model-version)))
          (expect (test-mutations--run #'approve-mutation-commit-files
                                       '(("a.txt" . "été\r\nlast"))
                                       "Apply suggestion from @bob\n\nCo-authored-by: Bob <1+bob@x>\n")
                  :to-equal '(t . "c0ffee"))
          (pcase-let ((`(,name ,variables) (test-mutations--sent)))
            (expect name :to-equal "mutations/create-commit-on-branch.graphql")
            (let-alist variables
              (expect .branch.branchName :to-equal (approve-pr-field 'headRefName))
              (expect .branch.repositoryNameWithOwner
                      :to-equal (let-alist (approve-pr-field 'headRepository) .nameWithOwner))
              (expect .expectedHeadOid :to-equal head)
              (expect .message :to-equal '((headline . "Apply suggestion from @bob")
                                           (body . "Co-authored-by: Bob <1+bob@x>")))
              (let ((file (aref .fileChanges.additions 0)))
                (expect (alist-get 'path file) :to-equal "a.txt")
                (expect (decode-coding-string (base64-decode-string (alist-get 'contents file))
                                              'utf-8-unix)
                        :to-equal "été\r\nlast"))))
          (expect (approve-model-version) :to-equal version))))

    (it "sends a one-line message without a body"
      (test-mutations--with-pr
        (test-mutations--answer-with '((createCommitOnBranch (commit (oid . "c0ffee")))))
        (test-mutations--run #'approve-mutation-commit-files '(("a.txt" . "a\n")) "Fix it")
        (expect (let-alist (cadr (test-mutations--sent)) .message)
                :to-equal '((headline . "Fix it")))))

    (it "says the branch moved when GitHub finds it elsewhere"
      (test-mutations--with-pr
        (setq test-mutations--answer
              (lambda (_request)
                '((data (createCommitOnBranch))
                  (errors ((type . "STALE_DATA")
                           (message . "Expected branch to point to \"abc\" but it did not."))))))
        (let ((outcome (test-mutations--run #'approve-mutation-commit-files
                                            '(("a.txt" . "a\n")) "Fix it")))
          (expect (car outcome) :to-be nil)
          (expect (approve-api-error-message (cdr outcome))
                  :to-equal (format "%s has commits this buffer hasn't loaded; \
refresh it, then try again" (approve-pr-field 'headRefName)))))))

  (describe "approve-mutation-add-review"
    (it "sends comments on lines, keeping the new threads of that review"
      (test-mutations--with-pr
        (test-mutations--answer-with
         `((addPullRequestReview
            (pullRequestReview
             (__typename . "PullRequestReview") (id . "PRR_single") (state . "COMMENTED")
             (pullRequest ,@(test-mutations--review-state)
                          (addedThreads
                           (nodes ,(test-mutations--thread-answer "PRRT_other" "PRRC_other"
                                                                  "PRR_other")
                                  ,(test-mutations--thread-answer "PRRT_single" "PRRC_single"
                                                                  "PRR_single"))))))))
        (expect (test-mutations--run #'approve-mutation-add-review
                                     :event "COMMENT"
                                     :threads '((:path "README.md" :side "RIGHT" :line 3
                                                       :body "Typo")))
                :to-equal `(t . ,(test-mutations--ref "PullRequestReview" "PRR_single")))
        (expect (test-mutations--sent)
                :to-equal `("mutations/add-pull-request-review.graphql"
                            ((event . "COMMENT") (pullRequestId . ,test-mutations--pr)
                             (threadCount . 1)
                             (threads . [((body . "Typo") (path . "README.md") (line . 3)
                                          (side . "RIGHT"))]))))
        (expect (last (test-mutations--ids 'reviewThreads)) :to-equal '("PRRT_single"))
        (expect (car (last (test-mutations--ids 'timelineItems))) :to-equal "PRR_single")
        (expect (approve-pr-field 'addedThreads) :to-be nil)
        (expect (approve-model-raw-field (test-mutations--ref "PullRequestReview" "PRR_single")
                                         'pullRequest)
                :to-be nil)))

    (it "sends a verdict alone, asking for no threads"
      (test-mutations--with-pr
        (test-mutations--answer-with
         `((addPullRequestReview
            (pullRequestReview (__typename . "PullRequestReview") (id . "PRR_ok")
                               (state . "APPROVED")
                               (pullRequest ,@(test-mutations--review-state)
                                            (addedThreads (nodes)))))))
        (test-mutations--run #'approve-mutation-add-review :event "APPROVE" :body "LGTM")
        (expect (test-mutations--sent)
                :to-equal `("mutations/add-pull-request-review.graphql"
                            ((body . "LGTM") (event . "APPROVE")
                             (pullRequestId . ,test-mutations--pr) (threadCount . 0))))
        (expect (test-mutations--total 'reviewThreads) :to-equal 20)
        (expect (car (last (test-mutations--ids 'timelineItems))) :to-equal "PRR_ok")))

    (it "refuses a comment on a whole file before sending"
      (test-mutations--with-pr
        (expect (approve-mutation-add-review :event "COMMENT"
                                             :threads '((:path "a.go" :body "File")))
                :to-throw 'error)
        (expect test-mutations--requests :to-be nil))))

  (it "submit the pending review: it is no longer pending and joins the conversation once"
    (test-mutations--with-pr
      (test-mutations--answer-with
       `((submitPullRequestReview
          (pullRequestReview
           (__typename . "PullRequestReview") (id . ,test-mutations--pending)
           (state . "APPROVED")
           (comments (totalCount . 3) (pageInfo (hasNextPage) (endCursor . "x"))
                     (nodes ,@(mapcar (lambda (comment)
                                        (test-mutations--comment (car comment)
                                                                 '(state . "SUBMITTED")))
                                      test-mutations--pending-comments)))
           (pullRequest (reviewDecision . "APPROVED")
                        ,@(test-mutations--review-state))))))
      (expect (approve-pr-pending-review) :not :to-be nil)
      (expect (approve-pr-field 'reviewDecision) :to-equal "CHANGES_REQUESTED")
      (dotimes (_ 2)
        (expect (test-mutations--run #'approve-mutation-submit-review "APPROVE")
                :to-equal `(t . ,(test-mutations--ref "PullRequestReview"
                                                      test-mutations--pending))))
      (expect (test-mutations--sent)
              :to-equal `("mutations/submit-pull-request-review.graphql"
                          ((event . "APPROVE") (pullRequestId . ,test-mutations--pr))))
      (expect (approve-pr-pending-review) :to-be nil)
      (expect (approve-pr-field 'reviewDecision) :to-equal "APPROVED")
      (expect (approve-model-get "PullRequestReviewComment" (caar test-mutations--pending-comments)
                                 'state)
              :to-equal "SUBMITTED")
      (expect (seq-count (lambda (id) (equal id test-mutations--pending))
                         (test-mutations--ids 'timelineItems))
              :to-equal 1)))

  (it "discard the pending review: its threads and replies leave the store's lists"
    (test-mutations--with-pr
      (let ((thread (test-mutations--ref "PullRequestReviewThread" test-mutations--thread)))
        ;; A pending reply on a published thread first.
        (test-mutations--answer-with
         `((addPullRequestReviewThreadReply
            (comment ,@(test-mutations--comment
                        "PRRC_reply" '(state . "PENDING")
                        `(pullRequestReview (__typename . "PullRequestReview")
                                            (id . ,test-mutations--pending)))
                     (pullRequest ,@(apply #'test-mutations--review-state
                                           (append (mapcar #'car test-mutations--pending-comments)
                                                   '("PRRC_reply"))))))))
        (test-mutations--run #'approve-mutation-add-reply test-mutations--thread "Later")
        (expect (test-mutations--total 'comments thread) :to-equal 4)
        (test-mutations--answer-with
         `((deletePullRequestReview (pullRequestReview (__typename . "PullRequestReview")
                                                       (id . ,test-mutations--pending)))))
        (expect (test-mutations--run #'approve-mutation-delete-review test-mutations--pending)
                :to-equal `(t . ,(test-mutations--ref "PullRequestReview"
                                                      test-mutations--pending)))
        (expect (test-mutations--sent)
                :to-equal `("mutations/delete-pull-request-review.graphql"
                            ((reviewId . ,test-mutations--pending))))
        (expect (approve-pr-pending-review) :to-be nil)
        (dolist (comment test-mutations--pending-comments)
          (expect (test-mutations--ids 'reviewThreads) :not :to-contain (cdr comment)))
        (expect (test-mutations--total 'reviewThreads) :to-equal 17)
        (expect (test-mutations--ids 'comments thread) :not :to-contain "PRRC_reply")
        (expect (test-mutations--total 'comments thread) :to-equal 3))))

  (it "delete a review comment; a review left with no comment and no body goes too"
    (test-mutations--with-pr
      (test-mutations--answer-with '((deletePullRequestReviewComment (clientMutationId))))
      ;; A published reply: its thread stays; its review, which has no
      ;; body and no comment left, is deleted by GitHub.
      (let ((thread (test-mutations--ref "PullRequestReviewThread" test-mutations--thread)))
        (expect (test-mutations--run #'approve-mutation-delete-review-comment
                                     "PRRC_kwDOjbJkqIaS7rYCer")
                :to-equal `(t . ,(test-mutations--ref "PullRequestReviewComment"
                                                      "PRRC_kwDOjbJkqIaS7rYCer")))
        (expect (test-mutations--sent)
                :to-equal '("mutations/delete-pull-request-review-comment.graphql"
                            ((commentId . "PRRC_kwDOjbJkqIaS7rYCer"))))
        (expect (test-mutations--ids 'comments thread)
                :to-equal '("PRRC_kwDOk4pUe_h6k_jiHs" "PRRC_kwDOW1CBXUSEPg3qZA"))
        (expect (test-mutations--ids 'reviewThreads) :to-contain test-mutations--thread)
        (expect (test-mutations--ids 'timelineItems) :not :to-contain "PRR_kwDOHuf84-IhbwNOnj")
        (expect (length (test-mutations--ids 'timelineItems)) :to-equal 39))
      ;; A comment of a review with a body: the review stays, counting one less.
      (let ((review (test-mutations--ref "PullRequestReview" "PRR_kwDOLL0E04LyFJdf5U")))
        (test-mutations--run #'approve-mutation-delete-review-comment "PRRC_kwDOvNUASM_gTiHG6o")
        (expect (test-mutations--ids 'timelineItems) :to-contain "PRR_kwDOLL0E04LyFJdf5U")
        (expect (approve-model-raw-field review 'commentCount) :to-equal '((totalCount . 4))))
      ;; The pending comments: each takes its thread, the last the review.
      (pcase-dolist (`(,comment . ,thread) test-mutations--pending-comments)
        (expect (approve-pr-pending-review) :not :to-be nil)
        (test-mutations--run #'approve-mutation-delete-review-comment comment)
        (expect (test-mutations--ids 'reviewThreads) :not :to-contain thread))
      (expect (approve-pr-pending-review) :to-be nil)
      (expect (test-mutations--total 'reviewThreads) :to-equal 17)))

  (it "edit a review comment, a review and a conversation comment"
    (test-mutations--with-pr
      (pcase-dolist (`(,function ,mutation ,field ,typename ,id ,query ,key)
                     '((approve-mutation-update-review-comment
                        updatePullRequestReviewComment pullRequestReviewComment
                        "PullRequestReviewComment" "PRRC_kwDOk4pUe_h6k_jiHs"
                        "mutations/update-pull-request-review-comment.graphql" commentId)
                       (approve-mutation-update-review
                        updatePullRequestReview pullRequestReview
                        "PullRequestReview" "PRR_kwDOSkZtjrqLxYfRz7"
                        "mutations/update-pull-request-review.graphql" reviewId)
                       (approve-mutation-update-issue-comment
                        updateIssueComment issueComment
                        "IssueComment" "IC_kwDOVM-W6WgSu2tHHg"
                        "mutations/update-issue-comment.graphql" commentId)))
        (test-mutations--answer-with
         `((,mutation (,field (__typename . ,typename) (id . ,id) (body . "New")
                              (bodyHTML . "<p>New</p>")))))
        (expect (test-mutations--run function id "New")
                :to-equal `(t . ,(test-mutations--ref typename id)))
        (expect (test-mutations--sent) :to-equal `(,query ((body . "New") (,key . ,id))))
        (expect (approve-model-get typename id 'bodyHTML) :to-equal "<p>New</p>"))))

  (it "add a conversation comment at the end of the conversation, and delete one"
    (test-mutations--with-pr
      (test-mutations--answer-with
       '((addComment (commentEdge (node (__typename . "IssueComment") (id . "IC_new")
                                        (body . "Hi"))))))
      (expect (test-mutations--run #'approve-mutation-add-comment "Hi")
              :to-equal `(t . ,(test-mutations--ref "IssueComment" "IC_new")))
      (expect (test-mutations--sent)
              :to-equal `("mutations/add-comment.graphql"
                          ((body . "Hi") (subjectId . ,test-mutations--pr))))
      (expect (car (last (test-mutations--ids 'timelineItems))) :to-equal "IC_new")
      (expect (length (test-mutations--ids 'timelineItems)) :to-equal 41)
      (test-mutations--answer-with '((deleteIssueComment (clientMutationId))))
      (test-mutations--run #'approve-mutation-delete-issue-comment "IC_kwDOVM-W6WgSu2tHHg")
      (expect (test-mutations--sent)
              :to-equal '("mutations/delete-issue-comment.graphql"
                          ((commentId . "IC_kwDOVM-W6WgSu2tHHg"))))
      (expect (test-mutations--ids 'timelineItems) :not :to-contain "IC_kwDOVM-W6WgSu2tHHg")
      (expect (length (test-mutations--ids 'timelineItems)) :to-equal 40)))

  (it "give GitHub's error to the errorback, changing nothing"
    (test-mutations--with-pr
      (setq test-mutations--answer
            (lambda (_request)
              '((data (resolveReviewThread))
                (errors ((type . "FORBIDDEN")
                         (message . "Resource not accessible by integration"))))))
      (let ((outcome (test-mutations--run #'approve-mutation-resolve-thread
                                          test-mutations--thread)))
        (expect (car outcome) :to-be nil)
        (expect (approve-api-error-message (cdr outcome))
                :to-equal "Resource not accessible by integration"))
      (expect (approve-model-raw-field (test-mutations--ref "PullRequestReviewThread"
                                                            test-mutations--thread)
                                       'isResolved)
              :to-be nil)))

  (it "give a failure to read GitHub's answer to the errorback, and log it"
    (test-mutations--with-pr
      (spy-on 'approve-log-condition)
      (test-mutations--answer-with
       `((resolveReviewThread (thread (id . ,test-mutations--thread) (isResolved . t)))))
      (let ((outcome (test-mutations--run #'approve-mutation-resolve-thread
                                          test-mutations--thread)))
        (expect (car outcome) :to-be nil)
        (expect (car (cdr outcome)) :to-be 'approve-api-error)
        (expect (approve-api-error-message (cdr outcome)) :to-match "see \\*approve-log\\*"))
      (expect 'approve-log-condition :to-have-been-called))))

(defun test-mutations--answer-by-name (answers)
  "Make GitHub answer each mutation with its data in ANSWERS.
ANSWERS is an alist (QUERY-FILE . DATA)."
  (setq test-mutations--answer
        (lambda (request)
          `((data . ,(copy-tree (cdr (assoc (plist-get request :name) answers))))))))

(defun test-mutations--names ()
  "Return the query files of the requests sent, the first first."
  (reverse (mapcar (lambda (request) (plist-get request :name)) test-mutations--requests)))

(defun test-mutations--pr-labels (&rest names)
  "Return the PR with labels NAMES, as GitHub answers a label mutation."
  `((__typename . "PullRequest") (id . ,test-mutations--pr)
    (labels (nodes ,@(mapcar (lambda (name)
                               `((__typename . "Label") (id . ,(concat "LA_" name))
                                 (name . ,name) (color . "ededed")))
                             names)))))

(describe "The metadata mutations"
  (it "add labels, then remove labels; the PR's labels are GitHub's"
    (test-mutations--with-pr
      (test-mutations--answer-by-name
       `(("mutations/add-labels-to-labelable.graphql"
          (addLabelsToLabelable
           (labelable ,@(test-mutations--pr-labels "enhancement" "area: parser" "bug"))))
         ("mutations/remove-labels-from-labelable.graphql"
          (removeLabelsFromLabelable
           (labelable ,@(test-mutations--pr-labels "enhancement" "bug"))))))
      (expect (test-mutations--run #'approve-mutation-set-labels
                                   '("LA_bug") '("LA_kwDObXGcJQS9m86GUU"))
              :to-equal `(t . ,(test-mutations--ref "PullRequest" test-mutations--pr)))
      (expect (test-mutations--names)
              :to-equal '("mutations/add-labels-to-labelable.graphql"
                          "mutations/remove-labels-from-labelable.graphql"))
      (expect (test-mutations--sent)
              :to-equal `("mutations/remove-labels-from-labelable.graphql"
                          ((labelIds . ["LA_kwDObXGcJQS9m86GUU"])
                           (labelableId . ,test-mutations--pr))))
      (expect (mapcar (lambda (label) (alist-get 'name label)) (approve-pr-field 'labels))
              :to-equal '("enhancement" "bug"))))

  (it "send only what changes, and stop at the first GitHub refuses"
    (test-mutations--with-pr
      (test-mutations--answer-by-name
       `(("mutations/remove-assignees-from-assignable.graphql"
          (removeAssigneesFromAssignable
           (assignable (__typename . "PullRequest") (id . ,test-mutations--pr)
                       (assignees (nodes)))))))
      (test-mutations--run #'approve-mutation-set-assignees nil '("U_kgDOUisnajVr3zkBPf"))
      (expect (test-mutations--sent)
              :to-equal `("mutations/remove-assignees-from-assignable.graphql"
                          ((assignableId . ,test-mutations--pr)
                           (assigneeIds . ["U_kgDOUisnajVr3zkBPf"]))))
      (expect (approve-pr-field 'assignees) :to-be nil)
      (setq test-mutations--requests nil
            test-mutations--answer
            (lambda (_request)
              '((data (addLabelsToLabelable))
                (errors ((type . "FORBIDDEN") (message . "Not allowed"))))))
      (expect (car (test-mutations--run #'approve-mutation-set-labels '("LA_bug") '("LA_x")))
              :to-be nil)
      (expect (test-mutations--names) :to-equal '("mutations/add-labels-to-labelable.graphql"))
      (expect (length (approve-pr-field 'labels)) :to-equal 2)))

  (it "ask users, teams and bots for a review, and no one else"
    (test-mutations--with-pr
      (test-mutations--answer-with
       `((requestReviews
          (pullRequest
           (__typename . "PullRequest") (id . ,test-mutations--pr)
           (reviewRequests
            (nodes ((__typename . "ReviewRequest") (id . "RR_new") (asCodeOwner)
                    (requestedReviewer (__typename . "User") (id . "U_erin")
                                       (login . "erin-example")))))))))
      (test-mutations--run #'approve-mutation-request-reviews
                           :users '("U_erin") :teams nil :bots nil)
      (expect (test-mutations--sent)
              :to-equal `("mutations/request-reviews.graphql"
                          ((botIds . []) (pullRequestId . ,test-mutations--pr)
                           (teamIds . []) (userIds . ["U_erin"]))))
      (expect (mapcar (lambda (request) (let-alist request .requestedReviewer.login))
                      (approve-pr-field 'reviewRequests))
              :to-equal '("erin-example"))))

  (it "set the milestone, and take it away with null"
    (test-mutations--with-pr
      (test-mutations--answer-with
       `((updatePullRequest
          (pullRequest (__typename . "PullRequest") (id . ,test-mutations--pr)
                       (milestone (__typename . "Milestone") (id . "MI_2")
                                  (title . "v2.0"))))))
      (test-mutations--run #'approve-mutation-set-milestone "MI_2")
      (expect (test-mutations--sent)
              :to-equal `("mutations/update-pull-request.graphql"
                          ((milestoneId . "MI_2") (pullRequestId . ,test-mutations--pr))))
      (expect (alist-get 'title (approve-pr-field 'milestone)) :to-equal "v2.0")
      (test-mutations--answer-with
       `((updatePullRequest
          (pullRequest (__typename . "PullRequest") (id . ,test-mutations--pr)
                       (milestone)))))
      (test-mutations--run #'approve-mutation-set-milestone nil)
      (expect (cadr (test-mutations--sent))
              :to-equal `((milestoneId . :null) (pullRequestId . ,test-mutations--pr)))
      (expect (approve-pr-field 'milestone) :to-be nil))))

(defun test-mutations--merge-state (&rest fields)
  "Return the PR with FIELDS, an alist, as a merge mutation answers."
  `((__typename . "PullRequest") (id . ,test-mutations--pr) ,@fields))

(describe "The merge mutations"
  (it "merge by a method from the head the store holds, with the message given"
    (test-mutations--with-pr
      (let ((head (approve-pr-field 'headRefOid)))
        (test-mutations--answer-with
         `((mergePullRequest
            (pullRequest ,@(test-mutations--merge-state
                            '(state . "MERGED") '(merged . t) '(closed . t)
                            '(viewerCanDeleteHeadRef . t))))))
        (expect (test-mutations--run #'approve-mutation-merge "SQUASH"
                                     :headline "Rewrite the parser (#202)" :body "")
                :to-equal `(t . ,(test-mutations--ref "PullRequest" test-mutations--pr)))
        (expect (test-mutations--sent)
                :to-equal `("mutations/merge-pull-request.graphql"
                            ((commitBody . "")
                             (commitHeadline . "Rewrite the parser (#202)")
                             (expectedHeadOid . ,head) (mergeMethod . "SQUASH")
                             (pullRequestId . ,test-mutations--pr))))
        (expect (approve-pr-field 'state) :to-equal "MERGED")
        (expect (approve-pr-viewer-can 'delete-head-ref) :to-be t)
        (test-mutations--run #'approve-mutation-merge "MERGE")
        (expect (cadr (test-mutations--sent))
                :to-equal `((expectedHeadOid . ,head) (mergeMethod . "MERGE")
                            (pullRequestId . ,test-mutations--pr))))))

  (it "enable auto-merge with a method, and disable it"
    (test-mutations--with-pr
      (test-mutations--answer-by-name
       `(("mutations/enable-pull-request-auto-merge.graphql"
          (enablePullRequestAutoMerge
           (pullRequest ,@(test-mutations--merge-state
                           '(autoMergeRequest (mergeMethod . "REBASE")
                                              (enabledAt . "2026-11-16T09:00:00Z"))
                           '(viewerCanDisableAutoMerge . t)))))
         ("mutations/disable-pull-request-auto-merge.graphql"
          (disablePullRequestAutoMerge
           (pullRequest ,@(test-mutations--merge-state '(autoMergeRequest)))))))
      (test-mutations--run #'approve-mutation-set-auto-merge t :method "REBASE")
      (expect (car (test-mutations--sent))
              :to-equal "mutations/enable-pull-request-auto-merge.graphql")
      (expect (alist-get 'mergeMethod (cadr (test-mutations--sent))) :to-equal "REBASE")
      (expect (let-alist (approve-pr-field 'autoMergeRequest) .mergeMethod) :to-equal "REBASE")
      (test-mutations--run #'approve-mutation-set-auto-merge nil)
      (expect (test-mutations--sent)
              :to-equal `("mutations/disable-pull-request-auto-merge.graphql"
                          ((pullRequestId . ,test-mutations--pr))))
      (expect (approve-pr-field 'autoMergeRequest) :to-be nil)))

  (it "add to the merge queue, and take out by the entry"
    (test-mutations--with-pr
      (let ((entry '(mergeQueueEntry (__typename . "MergeQueueEntry") (id . "MQE_1")
                                     (position . 2) (state . "QUEUED"))))
        (test-mutations--answer-by-name
         `(("mutations/enqueue-pull-request.graphql"
            (enqueuePullRequest
             (mergeQueueEntry (__typename . "MergeQueueEntry") (id . "MQE_1")
                              (pullRequest ,@(test-mutations--merge-state
                                              '(isInMergeQueue . t) entry)))))
           ("mutations/dequeue-pull-request.graphql"
            (dequeuePullRequest
             (mergeQueueEntry (__typename . "MergeQueueEntry") (id . "MQE_1")
                              (pullRequest ,@(test-mutations--merge-state
                                              '(isInMergeQueue) '(mergeQueueEntry))))))))
        (expect (approve-mutation-set-queued nil) :to-throw 'approve-mutation-refused)
        (expect test-mutations--requests :to-be nil)
        (expect (test-mutations--run #'approve-mutation-set-queued t)
                :to-equal `(t . ,(test-mutations--ref "PullRequest" test-mutations--pr)))
        (expect (test-mutations--sent)
                :to-equal `("mutations/enqueue-pull-request.graphql"
                            ((expectedHeadOid . ,(approve-pr-field 'headRefOid))
                             (pullRequestId . ,test-mutations--pr))))
        (expect (approve-pr-field 'isInMergeQueue) :to-be t)
        (expect (alist-get 'position (approve-pr-field 'mergeQueueEntry)) :to-equal 2)
        (test-mutations--run #'approve-mutation-set-queued nil)
        (expect (test-mutations--sent)
                :to-equal '("mutations/dequeue-pull-request.graphql" ((id . "MQE_1"))))
        (expect (approve-pr-field 'isInMergeQueue) :to-be nil))))

  (it "update the branch by a merge or a rebase, from the head the store holds"
    (test-mutations--with-pr
      (test-mutations--answer-with
       `((updatePullRequestBranch
          (pullRequest ,@(test-mutations--merge-state '(mergeable . "UNKNOWN"))))))
      (test-mutations--run #'approve-mutation-update-branch)
      (expect (test-mutations--sent)
              :to-equal `("mutations/update-pull-request-branch.graphql"
                          ((expectedHeadOid . ,(approve-pr-field 'headRefOid))
                           (pullRequestId . ,test-mutations--pr)
                           (updateMethod . "MERGE"))))
      (test-mutations--run #'approve-mutation-update-branch :rebase t)
      (expect (alist-get 'updateMethod (cadr (test-mutations--sent))) :to-equal "REBASE")
      (expect (approve-pr-field 'mergeable) :to-equal "UNKNOWN")))

  (it "close and reopen the pull request"
    (test-mutations--with-pr
      (test-mutations--answer-by-name
       `(("mutations/close-pull-request.graphql"
          (closePullRequest
           (pullRequest ,@(test-mutations--merge-state
                           '(state . "CLOSED") '(closed . t) '(viewerCanReopen . t)))))
         ("mutations/reopen-pull-request.graphql"
          (reopenPullRequest
           (pullRequest ,@(test-mutations--merge-state
                           '(state . "OPEN") '(closed) '(viewerCanReopen)))))))
      (test-mutations--run #'approve-mutation-set-closed t)
      (expect (test-mutations--sent)
              :to-equal `("mutations/close-pull-request.graphql"
                          ((pullRequestId . ,test-mutations--pr))))
      (expect (approve-pr-field 'state) :to-equal "CLOSED")
      (test-mutations--run #'approve-mutation-set-closed nil)
      (expect (car (test-mutations--sent)) :to-equal "mutations/reopen-pull-request.graphql")
      (expect (approve-pr-field 'state) :to-equal "OPEN")))

  (it "delete the head branch: the PR has no head ref then, and refuses another"
    (test-mutations--with-pr
      (let ((ref (alist-get 'id (approve-pr-field 'headRef))))
        (approve-model-patch (test-mutations--merge-state '(viewerCanDeleteHeadRef . t)))
        (test-mutations--answer-with '((deleteRef (clientMutationId))))
        (expect (test-mutations--run #'approve-mutation-delete-head-branch)
                :to-equal `(t . ,(test-mutations--ref "PullRequest" test-mutations--pr)))
        (expect (test-mutations--sent)
                :to-equal `("mutations/delete-ref.graphql" ((refId . ,ref))))
        (expect (approve-pr-field 'headRef) :to-be nil)
        (expect (approve-pr-viewer-can 'delete-head-ref) :to-be nil)
        (expect (approve-pr-field 'headRefName) :to-equal "parser-rewrite")
        (expect (approve-mutation-delete-head-branch) :to-throw 'approve-mutation-refused)))))

(provide 'test-approve-mutations)

;;; test-approve-mutations.el ends here
