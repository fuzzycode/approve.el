;;; test-approve-model.el --- Tests for approve-model -*- lexical-binding: t; -*-

;;; Commentary:

;; The store: normalization, refs, pages, list edits, selectors and
;; the shared store of the split layout.  The data is hand-made but
;; shaped as GitHub returns it (the store contract): objects are
;; alists, arrays are lists, null and false are nil.

;;; Code:

(require 'buttercup)
(require 'approve-model)

;;; Hand-made data

(defun test-approve-model--user (id login)
  "Return a User with ID and LOGIN."
  `((__typename . "User") (id . ,id) (login . ,login)))

(defun test-approve-model--response (pull-request)
  "Return a query response holding PULL-REQUEST."
  `((data . ((repository . ((pullRequest . ,pull-request)))
             (rateLimit . ((cost . 1) (remaining . 4999)
                           (resetAt . "2026-11-08T12:00:00Z")))))))

(defun test-approve-model--core ()
  "Return the PullRequest of the core query."
  `((__typename . "PullRequest")
    (id . "PR_1")
    (number . 7)
    (title . "Fix the frobnicator")
    (bodyHTML . "<p>Body</p>")
    (viewerCanUpdate . t)
    (viewerCanMergeAsAdmin . nil)
    (viewerCanReact . t)
    (author . ,(test-approve-model--user "U_alice" "alice"))
    (commitCount . ((totalCount . 3)))
    (labels . ((nodes . (((__typename . "Label") (id . "LA_1")
                          (name . "bug"))))))
    (pendingReviews
     . ((nodes . (((__typename . "PullRequestReview")
                   (id . "PRR_pending")
                   (state . "PENDING")
                   (author . ,(test-approve-model--user "U_bob" "bob")))))))
    (latestOpinionatedReviews
     . ((nodes . (((__typename . "PullRequestReview")
                   (id . "PRR_done")
                   (state . "APPROVED")
                   (author . ,(test-approve-model--user "U_carol"
                                                        "carol")))))))))

(defun test-approve-model--file (path additions)
  "Return a changed file at PATH with ADDITIONS."
  `((path . ,path) (additions . ,additions) (deletions . 1)
    (changeType . "MODIFIED") (viewerViewedState . "UNVIEWED")))

(defun test-approve-model--files-page (files total has-next cursor)
  "Return a files response with FILES of TOTAL, HAS-NEXT and CURSOR."
  (test-approve-model--response
   `((__typename . "PullRequest")
     (id . "PR_1")
     (files . ((totalCount . ,total)
               (pageInfo . ((hasNextPage . ,has-next) (endCursor . ,cursor)))
               (nodes . ,files))))))

(defun test-approve-model--comment (id review-id body &optional reply-to)
  "Return a review comment ID in review REVIEW-ID with BODY.
With REPLY-TO, the comment replies to the comment with that id."
  `((__typename . "PullRequestReviewComment")
    (id . ,id)
    (body . ,body)
    (author . ,(test-approve-model--user "U_bob" "bob"))
    (pullRequestReview . ((__typename . "PullRequestReview")
                          (id . ,review-id)))
    (replyTo . ,(and reply-to
                     `((__typename . "PullRequestReviewComment")
                       (id . ,reply-to))))))

(defun test-approve-model--thread (id path line resolved comments)
  "Return thread ID on PATH at LINE, RESOLVED or not, with COMMENTS."
  `((__typename . "PullRequestReviewThread")
    (id . ,id)
    (path . ,path)
    (line . ,line)
    (startLine . nil)
    (originalLine . ,line)
    (originalStartLine . nil)
    (diffSide . "RIGHT")
    (startDiffSide . nil)
    (isResolved . ,resolved)
    (isOutdated . nil)
    (subjectType . "LINE")
    (comments . ((totalCount . ,(length comments))
                 (pageInfo . ((hasNextPage . nil) (endCursor . "C")))
                 (nodes . ,comments)))))

(defun test-approve-model--threads-page (threads total has-next cursor)
  "Return a threads response with THREADS of TOTAL, HAS-NEXT and CURSOR."
  (test-approve-model--response
   `((__typename . "PullRequest")
     (id . "PR_1")
     (reviewThreads . ((totalCount . ,total)
                       (pageInfo . ((hasNextPage . ,has-next)
                                    (endCursor . ,cursor)))
                       (nodes . ,threads))))))

(defun test-approve-model--checks-page (ids has-next cursor)
  "Return a PR whose check runs IDS sit under a list without pageInfo.
The check list carries HAS-NEXT and CURSOR."
  `((__typename . "PullRequest")
    (id . "PR_1")
    (headCommit
     . ((nodes
         . (((__typename . "Commit")
             (id . "C_1")
             (statusCheckRollup
              . ((__typename . "StatusCheckRollup")
                 (id . "SCR_1")
                 (contexts
                  . ((totalCount . 3)
                     (pageInfo . ((hasNextPage . ,has-next)
                                  (endCursor . ,cursor)))
                     (nodes . ,(mapcar (lambda (id)
                                         `((__typename . "CheckRun")
                                           (id . ,id)))
                                       ids)))))))))))))

(defun test-approve-model--load-pr ()
  "Load the hand-made PR: core, two files and three threads."
  (approve-model-load (test-approve-model--core) t)
  (approve-model-load
   (test-approve-model--files-page
    (list (test-approve-model--file "lisp/a.el" 10)
          (test-approve-model--file "lisp/b.el" 4))
    2 nil "F2"))
  (approve-model-load
   (test-approve-model--threads-page
    (list (test-approve-model--thread
           "PRRT_1" "lisp/a.el" 5 nil
           (list (test-approve-model--comment "PRRC_1" "PRR_done" "Why?")
                 (test-approve-model--comment "PRRC_2" "PRR_pending"
                                              "Because." "PRRC_1")))
          (test-approve-model--thread
           "PRRT_2" "lisp/b.el" 3 nil
           (list (test-approve-model--comment "PRRC_3" "PRR_done" "Typo")))
          (test-approve-model--thread
           "PRRT_3" "lisp/a.el" 20 t
           (list (test-approve-model--comment "PRRC_4" "PRR_done" "Done"))))
    3 nil "T3")))

(defun test-approve-model--ids (entities)
  "Return the ids of ENTITIES."
  (mapcar (lambda (entity) (alist-get 'id entity)) entities))

;;; Tests

(describe "approve-model"
  (before-each
    (approve-model-init '(:host "github.com" :owner "o" :repo "r" :number 7)))

  (describe "state"
    (it "keeps the meta plist given to approve-model-init"
      (expect (approve-model-meta :owner) :to-equal "o")
      (expect (approve-model-meta :number) :to-equal 7)
      (approve-model-set-meta :viewer "bob")
      (expect (approve-model-meta :viewer) :to-equal "bob")
      (expect (plist-get (approve-model-meta) :repo) :to-equal "r"))

    (it "keeps extra data by key"
      (approve-model-set-extra :patches '(("a.el" . "patch")))
      (approve-model-set-extra :range '("abc" . "def"))
      (expect (approve-model-extra :patches) :to-equal '(("a.el" . "patch")))
      (expect (approve-model-extra :range) :to-equal '("abc" . "def")))

    (it "starts afresh on approve-model-init"
      (test-approve-model--load-pr)
      (approve-model-set-extra :range '("a" . "b"))
      (approve-model-init '(:owner "x"))
      (expect (approve-pr) :to-be nil)
      (expect (approve-model-has-entity-p "User" "U_alice") :to-be nil)
      (expect (approve-model-extra :range) :to-be nil)
      (expect (approve-model-meta :owner) :to-equal "x")))

  (describe "refs"
    (it "are made of a typename and an id"
      (let ((ref (approve-model-make-ref "User" "U_1")))
        (expect ref :to-equal '(:approve-ref "User" "U_1"))
        (expect (approve-model-ref-p ref) :to-be-truthy)
        (expect (approve-model-ref-type ref) :to-equal "User")
        (expect (approve-model-ref-id ref) :to-equal "U_1")))

    (it "are told apart from other values"
      (expect (approve-model-ref-p nil) :to-be nil)
      (expect (approve-model-ref-p "string") :to-be nil)
      (expect (approve-model-ref-p '(a b c)) :to-be nil)
      (expect (approve-model-ref-p '((:approve-ref "User" "U_1"))) :to-be nil)))

  (describe "approve-model-load"
    (it "returns a ref for an entity and stores its fields"
      (let ((ref (approve-model-load (test-approve-model--user "U_1" "joe"))))
        (expect ref :to-equal '(:approve-ref "User" "U_1"))
        (expect (approve-model-get "User" "U_1" 'login) :to-equal "joe")))

    (it "returns other data with refs in place of entities"
      (let ((result (approve-model-load
                     (test-approve-model--response
                      (test-approve-model--core)))))
        (expect (let-alist result .data.repository.pullRequest)
                :to-equal '(:approve-ref "PullRequest" "PR_1"))
        (expect (let-alist result .data.rateLimit.remaining) :to-equal 4999)))

    (it "stores nested entities on their own"
      (approve-model-load (test-approve-model--core) t)
      (expect (approve-model-has-entity-p "User" "U_alice") :to-be-truthy)
      (expect (approve-model-has-entity-p "Label" "LA_1") :to-be-truthy)
      (expect (approve-model-has-entity-p "User" "U_nobody") :to-be nil))

    (it "sets the root when asked"
      (approve-model-load (test-approve-model--core) t)
      (expect (approve-model-root-ref)
              :to-equal '(:approve-ref "PullRequest" "PR_1"))
      (expect (approve-model-root 'title) :to-equal "Fix the frobnicator"))

    (it "refuses to make a non-entity the root"
      (expect (approve-model-load '((foo . 1)) t)
              :to-throw 'approve-model-invalid-data))

    (it "unwraps connections, keeping totalCount and pageInfo"
      (test-approve-model--load-pr)
      (let ((files (approve-pr-field 'files)))
        (expect (approve-model-connection-p files) :to-be-truthy)
        (expect (approve-model-total-count files) :to-equal 2)
        (expect (approve-model-page-info files)
                :to-equal '((hasNextPage . nil) (endCursor . "F2")))
        (expect (length (approve-model-nodes files)) :to-equal 2)))

    (it "unwraps connections without paging data to a plain list"
      (approve-model-load (test-approve-model--core) t)
      (let ((labels (approve-pr-field 'labels)))
        (expect (approve-model-connection-p labels) :to-be nil)
        (expect (mapcar (lambda (l) (alist-get 'name l)) labels)
                :to-equal '("bug"))))

    (it "merges an entity loaded again; new fields win"
      (approve-model-load (test-approve-model--core) t)
      (approve-model-patch '((__typename . "PullRequest") (id . "PR_1")
                             (title . "New title") (state . "CLOSED")))
      (expect (approve-model-root 'title) :to-equal "New title")
      (expect (approve-model-root 'state) :to-equal "CLOSED")
      (expect (approve-model-root 'number) :to-equal 7))

    (it "updates every place an entity appears"
      (approve-model-load (test-approve-model--core) t)
      (approve-model-patch (test-approve-model--user "U_alice" "alice2"))
      (expect (alist-get 'login (approve-pr-field 'author))
              :to-equal "alice2"))

    (it "replaces a stored list with a first page"
      (test-approve-model--load-pr)
      (approve-model-load
       (test-approve-model--files-page
        (list (test-approve-model--file "lisp/c.el" 1)) 1 nil "F1"))
      (expect (mapcar (lambda (f) (alist-get 'path f)) (approve-pr-files))
              :to-equal '("lisp/c.el"))
      (expect (approve-model-total-count (approve-pr-field 'files))
              :to-equal 1))

    (it "keeps other lists when one is loaded"
      (test-approve-model--load-pr)
      (expect (length (approve-pr-threads)) :to-equal 3)
      (expect (length (approve-pr-files)) :to-equal 2)
      (expect (approve-pr-field 'title) :to-equal "Fix the frobnicator"))

    (it "accepts vectors as arrays"
      (approve-model-load `((__typename . "PullRequest") (id . "PR_9")
                            (labels . ((nodes . [((__typename . "Label")
                                                  (id . "LA_9")
                                                  (name . "x"))]))))
                          t)
      (expect (approve-model-root 'labels)
              :to-equal '(((__typename . "Label") (id . "LA_9") (name . "x")))))

    (describe "with an id but no __typename"
      (it "signals approve-model-invalid-data"
        (expect (approve-model-load '((id . "U_1") (login . "joe")))
                :to-throw 'approve-model-invalid-data))

      (it "signals for nested objects too"
        (expect (approve-model-load
                 `((__typename . "PullRequest") (id . "PR_1")
                   (author . ((id . "U_1") (login . "joe")))))
                :to-throw 'approve-model-invalid-data))

      (it "is an approve-error"
        (expect (approve-model-load '((id . "U_1")))
                :to-throw 'approve-error)))

    (it "keeps objects without an id inline"
      (let ((result (approve-model-load '((foo . "bar") (baz . 123)))))
        (expect (approve-model-ref-p result) :to-be nil)
        (expect (alist-get 'foo result) :to-equal "bar"))))

  (describe "approve-model-load-page"
    (it "appends the nodes of a later page and takes its pageInfo"
      (approve-model-load (test-approve-model--core) t)
      (approve-model-load
       (test-approve-model--threads-page
        (list (test-approve-model--thread "PRRT_1" "a.el" 1 nil nil))
        3 t "T1"))
      (approve-model-load-page
       (test-approve-model--threads-page
        (list (test-approve-model--thread "PRRT_2" "b.el" 2 nil nil))
        4 nil "T2"))
      (let ((threads (approve-pr-field 'reviewThreads)))
        (expect (test-approve-model--ids (approve-model-nodes threads))
                :to-equal '("PRRT_1" "PRRT_2"))
        (expect (approve-model-page-info threads)
                :to-equal '((hasNextPage . nil) (endCursor . "T2")))
        (expect (approve-model-total-count threads) :to-equal 4)))

    (it "appends a paged list nested under a list without pageInfo"
      (approve-model-load
       (test-approve-model--checks-page '("CR_1" "CR_2") t "c2") t)
      (approve-model-load-page
       (test-approve-model--checks-page '("CR_3") nil "c3"))
      (expect (test-approve-model--ids
               (approve-model-nodes
                (approve-model-get "StatusCheckRollup" "SCR_1" 'contexts)))
              :to-equal '("CR_1" "CR_2" "CR_3")))

    (it "skips refs that are already in the list"
      (approve-model-load (test-approve-model--core) t)
      (approve-model-load
       (test-approve-model--threads-page
        (list (test-approve-model--thread "PRRT_1" "a.el" 1 nil nil))
        3 t "T1"))
      (approve-model-load-page
       (test-approve-model--threads-page
        (list (test-approve-model--thread "PRRT_1" "a.el" 1 t nil)
              (test-approve-model--thread "PRRT_2" "b.el" 2 nil nil))
        3 nil "T2"))
      (expect (test-approve-model--ids (approve-pr-threads))
              :to-equal '("PRRT_1" "PRRT_2"))
      (expect (alist-get 'isResolved (car (approve-pr-threads))) :to-be t))

    (it "appends pages of inline nodes such as files"
      (approve-model-load (test-approve-model--core) t)
      (approve-model-load
       (test-approve-model--files-page
        (list (test-approve-model--file "a.el" 1)) 2 t "F1"))
      (approve-model-load-page
       (test-approve-model--files-page
        (list (test-approve-model--file "b.el" 2)) 2 nil "F2"))
      (expect (mapcar (lambda (f) (alist-get 'path f)) (approve-pr-files))
              :to-equal '("a.el" "b.el")))

    (it "replaces the nested lists of the appended nodes"
      (approve-model-load (test-approve-model--core) t)
      (approve-model-load
       (test-approve-model--threads-page
        (list (test-approve-model--thread
               "PRRT_1" "a.el" 1 nil
               (list (test-approve-model--comment "PRRC_1" "PRR_done" "a"))))
        3 t "T1"))
      (approve-model-load-page
       (test-approve-model--threads-page
        (list (test-approve-model--thread
               "PRRT_1" "a.el" 1 nil
               (list (test-approve-model--comment "PRRC_2" "PRR_done" "b"))))
        3 nil "T2"))
      (let ((thread (car (approve-pr-threads))))
        (expect (test-approve-model--ids
                 (approve-model-nodes (alist-get 'comments thread)))
                :to-equal '("PRRC_2"))))

    (it "loads like approve-model-load when the entity is new"
      (approve-model-load-page
       (test-approve-model--files-page
        (list (test-approve-model--file "a.el" 1)) 1 nil "F1"))
      (expect (approve-model-nodes
               (approve-model-get "PullRequest" "PR_1" 'files))
              :to-equal (list (test-approve-model--file "a.el" 1)))))

  (describe "reading"
    (before-each
      (test-approve-model--load-pr))

    (it "resolves refs in what it returns"
      (let ((author (alist-get 'author (approve-model-root))))
        (expect (approve-model-ref-p author) :to-be nil)
        (expect (alist-get 'login author) :to-equal "alice")))

    (it "returns one field or an alist of fields"
      (expect (approve-model-get "PullRequest" "PR_1" 'number) :to-equal 7)
      (expect (approve-model-get "PullRequest" "PR_1" '(number title))
              :to-equal '((number . 7) (title . "Fix the frobnicator"))))

    (it "reads through a ref"
      (expect (approve-model-get-ref '(:approve-ref "User" "U_bob") 'login)
              :to-equal "bob"))

    (it "stops at cycles: a ref on the current path stays a ref"
      (approve-model-load
       `((__typename . "PullRequestReview")
         (id . "PRR_done")
         (comments . ((totalCount . 1)
                      (nodes . (,(test-approve-model--comment
                                  "PRRC_1" "PRR_done" "Why?")))))))
      (let* ((review (approve-model-get "PullRequestReview" "PRR_done"))
             (comment (car (approve-model-nodes (alist-get 'comments review)))))
        (expect (alist-get 'body comment) :to-equal "Why?")
        (expect (alist-get 'pullRequestReview comment)
                :to-equal '(:approve-ref "PullRequestReview" "PRR_done")))
      (let* ((comment (approve-model-get "PullRequestReviewComment" "PRRC_1"))
             (review (alist-get 'pullRequestReview comment)))
        (expect (alist-get 'state review) :to-equal "APPROVED")
        (expect (car (approve-model-nodes (alist-get 'comments review)))
                :to-equal '(:approve-ref "PullRequestReviewComment" "PRRC_1"))))

    (it "resolves the whole PR graph despite cycles"
      (approve-model-load
       `((__typename . "PullRequestReview")
         (id . "PRR_done")
         (pullRequest . ((__typename . "PullRequest") (id . "PR_1")))))
      (expect (alist-get 'title (approve-pr)) :to-equal "Fix the frobnicator"))

    (it "leaves a ref to a missing entity as it is"
      (approve-model-delete '(:approve-ref "User" "U_alice"))
      (expect (approve-pr-field 'author)
              :to-equal '(:approve-ref "User" "U_alice")))

    (describe "errors"
      (it "signals approve-model-not-found for a missing entity"
        (expect (approve-model-get "User" "U_nobody")
                :to-throw 'approve-model-not-found))

      (it "signals approve-model-invalid-ref for a non-ref"
        (expect (approve-model-get-ref '(not a ref))
                :to-throw 'approve-model-invalid-ref))

      (it "signals approve-model-not-found when there is no root"
        (approve-model-init)
        (expect (approve-model-root) :to-throw 'approve-model-not-found))))

  (describe "list edits"
    (before-each
      (test-approve-model--load-pr))

    (it "append a ref to a connection and count it"
      (let ((thread '(:approve-ref "PullRequestReviewThread" "PRRT_1"))
            (reply (approve-model-patch
                    (test-approve-model--comment "PRRC_9" "PRR_pending" "Ok"))))
        (approve-model-append-ref thread 'comments reply)
        (let ((comments (approve-model-get-ref thread 'comments)))
          (expect (test-approve-model--ids (approve-model-nodes comments))
                  :to-equal '("PRRC_1" "PRRC_2" "PRRC_9"))
          (expect (approve-model-total-count comments) :to-equal 3)
          (expect (approve-model-page-info comments)
                  :to-equal '((hasNextPage . nil) (endCursor . "C"))))))

    (it "do not append a ref twice"
      (let ((thread '(:approve-ref "PullRequestReviewThread" "PRRT_1"))
            (ref '(:approve-ref "PullRequestReviewComment" "PRRC_1")))
        (approve-model-append-ref thread 'comments ref)
        (expect (approve-model-total-count
                 (approve-model-get-ref thread 'comments))
                :to-equal 2)))

    (it "remove a ref from a connection and count it"
      (let ((pr (approve-model-root-ref)))
        (approve-model-remove-ref
         pr 'reviewThreads '(:approve-ref "PullRequestReviewThread" "PRRT_2"))
        (expect (test-approve-model--ids (approve-pr-threads))
                :to-equal '("PRRT_1" "PRRT_3"))
        (expect (approve-model-total-count (approve-pr-field 'reviewThreads))
                :to-equal 2)))

    (it "edit plain lists"
      (let ((pr (approve-model-root-ref))
            (review '(:approve-ref "PullRequestReview" "PRR_new")))
        (approve-model-patch '((__typename . "PullRequestReview")
                               (id . "PRR_new") (state . "PENDING")))
        (approve-model-remove-ref
         pr 'pendingReviews '(:approve-ref "PullRequestReview" "PRR_pending"))
        (expect (approve-pr-pending-review) :to-be nil)
        (approve-model-append-ref pr 'pendingReviews review)
        (expect (alist-get 'id (approve-pr-pending-review))
                :to-equal "PRR_new")))

    (it "start a list in a field that has none"
      (let ((pr (approve-model-root-ref))
            (ref '(:approve-ref "User" "U_bob")))
        (approve-model-append-ref pr 'assignees ref)
        (expect (approve-model-root 'assignees)
                :to-equal (list (approve-model-get "User" "U_bob")))))

    (it "signal approve-model-not-found for a missing owner"
      (expect (approve-model-append-ref '(:approve-ref "User" "U_nobody")
                                        'followers
                                        '(:approve-ref "User" "U_bob"))
              :to-throw 'approve-model-not-found))

    (it "delete an entity from the store"
      (expect (approve-model-delete '(:approve-ref "Label" "LA_1"))
              :to-be-truthy)
      (expect (approve-model-has-entity-p "Label" "LA_1") :to-be nil)
      (expect (approve-model-delete '(:approve-ref "Label" "LA_1")) :to-be nil)))

  (describe "selectors"
    (it "return nil before the PR is loaded"
      (expect (approve-pr) :to-be nil)
      (expect (approve-pr-field 'title) :to-be nil)
      (expect (approve-pr-fields '(title)) :to-be nil)
      (expect (approve-pr-list-fields 'reviewThreads '(id)) :to-be nil)
      (expect (approve-pr-files) :to-be nil)
      (expect (approve-pr-threads) :to-be nil)
      (expect (approve-pr-threads-for-file "lisp/a.el") :to-be nil)
      (expect (approve-pr-pending-review) :to-be nil)
      (expect (approve-pr-viewer-can 'update) :to-be nil))

    (describe "on a loaded PR"
      (before-each
        (test-approve-model--load-pr))

      (it "approve-pr returns the resolved PullRequest"
        (let ((pr (approve-pr)))
          (expect (alist-get 'title pr) :to-equal "Fix the frobnicator")
          (expect (let-alist pr .author.login) :to-equal "alice")))

      (it "approve-pr-field reads one field"
        (expect (approve-pr-field 'number) :to-equal 7)
        (expect (approve-pr-field 'commitCount) :to-equal '((totalCount . 3))))

      (it "approve-pr-fields reads several fields"
        (expect (approve-pr-fields '(number title))
                :to-equal '((number . 7) (title . "Fix the frobnicator")))
        (expect (let-alist (approve-pr-fields '(author)) .author.login)
                :to-equal "alice"))

      (it "approve-pr-files lists the files"
        (expect (approve-pr-files)
                :to-equal (list (test-approve-model--file "lisp/a.el" 10)
                                (test-approve-model--file "lisp/b.el" 4))))

      (it "approve-pr-threads lists every thread, resolved"
        (let ((threads (approve-pr-threads)))
          (expect (test-approve-model--ids threads)
                  :to-equal '("PRRT_1" "PRRT_2" "PRRT_3"))
          (expect (let-alist (car (approve-model-nodes
                                   (alist-get 'comments (car threads))))
                    .author.login)
                  :to-equal "bob")))

      (it "approve-pr-list-fields reads some fields of every node"
        (expect (approve-pr-list-fields 'reviewThreads '(id path isResolved))
                :to-equal '(((id . "PRRT_1") (path . "lisp/a.el") (isResolved))
                            ((id . "PRRT_2") (path . "lisp/b.el") (isResolved))
                            ((id . "PRRT_3") (path . "lisp/a.el") (isResolved . t))))
        (expect (approve-pr-list-fields 'noSuchList '(id)) :to-be nil))

      (it "approve-pr-threads-for-file keeps the threads of one path in order"
        (let ((threads (approve-pr-threads-for-file "lisp/a.el")))
          (expect (test-approve-model--ids threads)
                  :to-equal '("PRRT_1" "PRRT_3"))
          (expect (mapcar (lambda (thread) (alist-get 'isResolved thread))
                          threads)
                  :to-equal '(nil t)))
        (expect (test-approve-model--ids
                 (approve-pr-threads-for-file "lisp/b.el"))
                :to-equal '("PRRT_2"))
        (expect (approve-pr-threads-for-file "lisp/none.el") :to-be nil))

      (it "a thread's comments know their review"
        (let* ((thread (car (approve-pr-threads-for-file "lisp/a.el")))
               (comments (approve-model-nodes (alist-get 'comments thread))))
          (expect (mapcar (lambda (c) (let-alist c .pullRequestReview.state))
                          comments)
                  :to-equal '("APPROVED" "PENDING"))))

      (it "resolves back-pointers one level: their own refs stay refs"
        (approve-model-patch
         `((__typename . "PullRequestReview")
           (id . "PRR_pending")
           (comments . ((totalCount . 1)
                        (nodes . (((__typename . "PullRequestReviewComment")
                                   (id . "PRRC_2"))))))))
        (let* ((thread (car (approve-pr-threads-for-file "lisp/a.el")))
               (reply (cadr (approve-model-nodes (alist-get 'comments thread))))
               (review (alist-get 'pullRequestReview reply))
               (reply-to (alist-get 'replyTo reply)))
          (expect (alist-get 'author reply) :to-equal
                  (approve-model-get "User" "U_bob"))
          (expect (alist-get 'state review) :to-equal "PENDING")
          (expect (alist-get 'author review)
                  :to-equal '(:approve-ref "User" "U_bob"))
          (expect (approve-model-nodes (alist-get 'comments review))
                  :to-equal '((:approve-ref "PullRequestReviewComment"
                                            "PRRC_2")))
          (expect (alist-get 'body reply-to) :to-equal "Why?")
          (expect (alist-get 'author reply-to)
                  :to-equal '(:approve-ref "User" "U_bob"))))

      (it "approve-pr-pending-review returns the viewer's pending review"
        (let ((review (approve-pr-pending-review)))
          (expect (alist-get 'id review) :to-equal "PRR_pending")
          (expect (let-alist review .author.login) :to-equal "bob")))

      (it "approve-pr-pending-review drops a review once it is submitted"
        (approve-model-patch '((__typename . "PullRequestReview")
                               (id . "PRR_pending") (state . "COMMENTED")))
        (expect (approve-pr-pending-review) :to-be nil))

      (it "approve-pr-viewer-can maps lisp names to viewerCan fields"
        (expect (approve-pr-viewer-can 'update) :to-be t)
        (expect (approve-pr-viewer-can 'react) :to-be t)
        (expect (approve-pr-viewer-can 'merge-as-admin) :to-be nil)
        (approve-model-patch '((__typename . "PullRequest") (id . "PR_1")
                               (viewerCanMergeAsAdmin . t)))
        (expect (approve-pr-viewer-can 'merge-as-admin) :to-be t)
        (expect (approve-pr-viewer-can 'not-loaded) :to-be nil))))

  (describe "connection helpers"
    (let ((connection '((nodes . ((a . 1) (b . 2)))
                        (totalCount . 5)
                        (pageInfo . ((hasNextPage . t) (endCursor . "x"))))))
      (it "recognise connections with paging data only"
        (expect (approve-model-connection-p connection) :to-be-truthy)
        (expect (approve-model-connection-p '((nodes . nil) (totalCount . 0)))
                :to-be-truthy)
        (expect (approve-model-connection-p '((nodes . ((a . 1)))))
                :to-be nil)
        (expect (approve-model-connection-p '(((a . 1)) ((b . 2))))
                :to-be nil))

      (it "read nodes, totalCount and pageInfo"
        (expect (approve-model-nodes connection) :to-equal '((a . 1) (b . 2)))
        (expect (approve-model-nodes '(x y)) :to-equal '(x y))
        (expect (approve-model-total-count connection) :to-equal 5)
        (expect (approve-model-total-count '(x)) :to-be nil)
        (expect (approve-model-page-info connection)
                :to-equal '((hasNextPage . t) (endCursor . "x")))
        (expect (approve-model-page-info '(x)) :to-be nil))

      (it "tell whether there is more"
        (expect (approve-model-has-next-page-p connection) :to-be t)
        (expect (approve-model-truncated-p connection) :to-be-truthy)
        (expect (approve-model-truncated-p '((nodes . (1 2)) (totalCount . 5)))
                :to-be-truthy)
        (expect (approve-model-truncated-p
                 '((nodes . (1 2)) (totalCount . 2)
                   (pageInfo . ((hasNextPage . nil)))))
                :to-be nil)
        (expect (approve-model-has-next-page-p '((nodes . (1)) (totalCount . 1)))
                :to-be nil)
        (expect (approve-model-truncated-p '(1 2)) :to-be nil))))

  (describe "approve-model-store-buffer"
    (let (pr-buffer diff-buffer)
      (before-each
        (setq pr-buffer (generate-new-buffer " *test-pr*")
              diff-buffer (generate-new-buffer " *test-diff*"))
        (with-current-buffer pr-buffer
          (approve-model-init '(:owner "o"))
          (test-approve-model--load-pr))
        (with-current-buffer diff-buffer
          (setq approve-model-store-buffer pr-buffer)))

      (after-each
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list pr-buffer diff-buffer)))

      (it "lets another buffer read the owner's store"
        (with-current-buffer diff-buffer
          (expect (approve-pr-field 'title) :to-equal "Fix the frobnicator")
          (expect (test-approve-model--ids
                   (approve-pr-threads-for-file "lisp/b.el"))
                  :to-equal '("PRRT_2"))
          (expect (approve-model-meta :owner) :to-equal "o")))

      (it "writes to the owner's store"
        (with-current-buffer diff-buffer
          (approve-model-patch '((__typename . "PullRequest") (id . "PR_1")
                                 (title . "From the diff buffer")))
          (approve-model-set-extra :range '("a" . "b")))
        (with-current-buffer pr-buffer
          (expect (approve-pr-field 'title) :to-equal "From the diff buffer")
          (expect (approve-model-extra :range) :to-equal '("a" . "b")))
        (with-current-buffer diff-buffer
          (expect approve-model--store :to-be nil)))

      (it "is named by approve-model-owner-buffer"
        (with-current-buffer diff-buffer
          (expect (approve-model-owner-buffer) :to-be pr-buffer))
        (with-current-buffer pr-buffer
          (expect (approve-model-owner-buffer) :to-be pr-buffer)))

      (it "signals approve-model-error when the owner was killed"
        (kill-buffer pr-buffer)
        (with-current-buffer diff-buffer
          (expect (approve-pr) :to-throw 'approve-model-error))))))

(provide 'test-approve-model)

;;; test-approve-model.el ends here
