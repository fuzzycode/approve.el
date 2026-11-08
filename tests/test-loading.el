;;; test-loading.el --- Loading a pull request through the fake API -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase 0's done-when: the medium fixture goes through the fake API
;; (`approve-test-with-fixture') into the store, and the selectors
;; return the threads of a file, the files with their viewed states and
;; the pending review.  The large fixture checks paging: 300 files over
;; three GraphQL pages and four REST pages.
;;
;; The loading here is the plan of docs/architecture.org ("Loading a
;; PR") written out by hand; Phase 1's approve-queries.el will own it.

;;; Code:

(require 'buttercup)
(require 'approve-api)
(require 'approve-api-paging)
(require 'approve-model)
(require 'approve-test-helpers "helpers")

(defvar test-loading--errors nil
  "Errors the requests of the current test reported.")

(defun test-loading--fail (err)
  "Remember ERR, an error from a request."
  (push err test-loading--errors))

(defun test-loading--pull-request (data)
  "Return the pullRequest in DATA, a GraphQL response's data."
  (let-alist data .repository.pullRequest))

(defun test-loading--load-core (fixture)
  "Load the core query of FIXTURE into the store; call back when done.
Return a function that says whether it is done."
  (let ((done nil))
    (approve-api-graphql "queries/pull-request.graphql"
                         (approve-test-fixture-variables fixture)
                         :errorback #'test-loading--fail
                         :callback (lambda (data)
                                     (approve-model-load
                                      (test-loading--pull-request data) t)
                                     (setq done t)))
    (lambda () done)))

(defun test-loading--load-list (fixture query field)
  "Load every page of the list FIELD from QUERY of FIXTURE into the store.
The first page replaces the list, later pages append to it.  Return a
function that returns the number of pages once the last one is in."
  (let ((pages 0) (done nil))
    (approve-api-graphql-all
     query (approve-test-fixture-variables fixture)
     :path (list 'repository 'pullRequest field)
     :errorback #'test-loading--fail
     :on-page (lambda (data page)
                (let ((pull-request (test-loading--pull-request data)))
                  (if (= page 1)
                      (approve-model-load pull-request t)
                    (approve-model-load-page pull-request)))
                (setq pages page))
     :callback (lambda () (setq done t)))
    (lambda () (and done pages))))

(defun test-loading--load-patches (fixture)
  "Load the REST files list of FIXTURE: patches by path in the extra data.
Return a function that returns (PAGES . ENTRIES) once the last page
is in.  The patches are kept as an alist of (PATH . PATCH) strings."
  (let* ((scenario (approve-test-fixture fixture))
         (pages 0) (entries 0) (done nil))
    (approve-model-set-extra :patches nil)
    (approve-api-rest-all
     (format "/repos/%s/%s/pulls/%d/files" (plist-get scenario :owner)
             (plist-get scenario :repo) (plist-get scenario :number))
     :errorback #'test-loading--fail
     :on-page (lambda (files page)
                (setq pages page entries (+ entries (length files)))
                (approve-model-set-extra
                 :patches
                 (append (approve-model-extra :patches)
                         (cl-loop for file in files
                                  when (alist-get 'patch file)
                                  collect (cons (alist-get 'filename file)
                                                (alist-get 'patch file))))))
     :callback (lambda () (setq done t)))
    (lambda () (and done (cons pages entries)))))

(defun test-loading--ids (things)
  "Return the ids of THINGS, alists."
  (mapcar (lambda (thing) (alist-get 'id thing)) things))

(defun test-loading--summary (thread)
  "Return what matters of THREAD for the medium fixture's parser.go."
  (let-alist thread
    (list .id .subjectType .diffSide .startLine .line .originalLine
          (and .isResolved t) (and .isOutdated t)
          (approve-model-total-count .comments)
          (length (approve-model-nodes .comments))
          (let-alist (car (approve-model-nodes .comments)) (list .author.login .state)))))

(describe "Loading the medium fixture through the fake API"
  :var (buffer)
  (before-all
    (setq test-loading--errors nil
          buffer (generate-new-buffer " *test-loading medium*"))
    (with-current-buffer buffer
      (approve-test-with-fixture "medium"
        (approve-model-init (approve-test-fixture-meta "medium"))
        (let ((core (test-loading--load-core "medium"))
              (files (test-loading--load-list
                      "medium" "queries/pull-request-files.graphql" 'files))
              (threads (test-loading--load-list
                        "medium" "queries/pull-request-threads.graphql" 'reviewThreads)))
          (approve-test-wait-for (lambda () (and (funcall core) (funcall files)
                                                 (funcall threads))))))))
  (after-all (kill-buffer buffer))

  (it "loads without errors and sets the pull request as the root"
    (expect test-loading--errors :to-equal nil)
    (with-current-buffer buffer
      (expect (approve-model-root-ref)
              :to-equal (approve-model-make-ref "PullRequest" "PR_kwDOPCDj7SnvM_RcFp"))
      (expect (approve-pr-field 'title) :to-equal "Rewrite the parser and engine scheduler")
      (expect (approve-model-meta :viewer) :to-equal "vera-example")))

  (it "returns the threads of a file, every kind of thread in order"
    (with-current-buffer buffer
      (expect (mapcar #'test-loading--summary
                      (approve-pr-threads-for-file "internal/parser/parser.go"))
              :to-equal
              '(("PRRT_kwDO85-qIzFQYiTYBM" "LINE" "RIGHT" nil 15 15 nil nil 3 3
                 ("carol-example" "SUBMITTED"))
                ("PRRT_kwDOkNvF9T3yvL1od7" "LINE" "RIGHT" 129 131 131 nil nil 2 2
                 ("vera-example" "SUBMITTED"))
                ("PRRT_kwDOjAPYGPfqXBuQNn" "LINE" "LEFT" nil 52 52 t nil 2 2
                 ("carol-example" "SUBMITTED"))
                ("PRRT_kwDOBjizZBfYz5abyp" "LINE" "RIGHT" nil nil 61 nil t 2 2
                 ("dave-example" "SUBMITTED"))
                ("PRRT_kwDOmVrZbzhFWoiCSv" "FILE" "RIGHT" nil nil nil nil nil 1 1
                 ("carol-example" "SUBMITTED"))
                ("PRRT_kwDOVCEPcVKg5CpjSd" "LINE" "RIGHT" nil 134 134 nil nil 1 1
                 ("vera-example" "PENDING"))))))

  (it "returns no threads for a file without any, and all threads of the PR"
    (with-current-buffer buffer
      (expect (approve-pr-threads-for-file "go.mod") :to-equal nil)
      (expect (length (approve-pr-threads)) :to-equal 20)
      (expect (let-alist (approve-pr-field 'threadCount) .totalCount) :to-equal 20)))

  (it "resolves a thread's comments with their authors and replies"
    (with-current-buffer buffer
      (let* ((thread (car (approve-pr-threads-for-file "internal/parser/parser.go")))
             (comments (approve-model-nodes (alist-get 'comments thread))))
        (expect (mapcar (lambda (comment) (let-alist comment .author.login)) comments)
                :to-equal '("carol-example" "alice-example" "carol-example"))
        (expect (let-alist (nth 1 comments) .replyTo.id)
                :to-equal (alist-get 'id (car comments)))
        (expect (let-alist (car comments) .pullRequestReview.__typename)
                :to-equal "PullRequestReview"))))

  (it "returns the files with their viewed states and change types"
    (with-current-buffer buffer
      (let ((files (approve-pr-files)))
        (expect (length files) :to-equal 30)
        (expect (seq-uniq (mapcar (lambda (file) (alist-get 'viewerViewedState file)) files))
                :to-have-same-items-as '("VIEWED" "UNVIEWED" "DISMISSED"))
        (expect (seq-uniq (mapcar (lambda (file) (alist-get 'changeType file)) files))
                :to-have-same-items-as
                '("ADDED" "DELETED" "MODIFIED" "RENAMED" "COPIED" "CHANGED"))
        (expect (seq-find (lambda (file)
                            (equal (alist-get 'path file) "internal/parser/parser.go"))
                          files)
                :to-equal '((path . "internal/parser/parser.go") (additions . 21)
                            (deletions . 11) (changeType . "MODIFIED")
                            (viewerViewedState . "UNVIEWED"))))))

  (it "returns the viewer's pending review with the comments of its threads"
    (with-current-buffer buffer
      (let* ((review (approve-pr-pending-review))
             (comments (approve-model-nodes (alist-get 'comments review)))
             (pending-threads
              (seq-filter (lambda (thread)
                            (let-alist (car (approve-model-nodes (alist-get 'comments thread)))
                              (equal .state "PENDING")))
                          (approve-pr-threads))))
        (expect (alist-get 'state review) :to-equal "PENDING")
        (expect (let-alist review .author.login) :to-equal "vera-example")
        (expect (let-alist review .commentCount.totalCount) :to-equal 3)
        (expect (length comments) :to-equal 3)
        (expect (test-loading--ids comments)
                :to-equal (mapcar (lambda (thread)
                                    (alist-get 'id (car (approve-model-nodes
                                                         (alist-get 'comments thread)))))
                                  pending-threads))))))

(describe "Loading the large fixture through the fake API"
  :var (buffer files patches)
  (before-all
    (setq test-loading--errors nil
          buffer (generate-new-buffer " *test-loading large*"))
    (with-current-buffer buffer
      (approve-test-with-fixture "large"
        (approve-model-init (approve-test-fixture-meta "large"))
        (let ((core (test-loading--load-core "large"))
              (file-pages (test-loading--load-list
                           "large" "queries/pull-request-files.graphql" 'files))
              (patch-pages (test-loading--load-patches "large")))
          (approve-test-wait-for (lambda () (and (funcall core) (funcall file-pages)
                                                 (funcall patch-pages))))
          (setq files (funcall file-pages)
                patches (funcall patch-pages))))))
  (after-all (kill-buffer buffer))

  (it "loads every page of the files"
    (expect test-loading--errors :to-equal nil)
    (expect files :to-equal 3)
    (with-current-buffer buffer
      (expect (length (approve-pr-files)) :to-equal 300)
      (expect (length (seq-uniq (mapcar (lambda (file) (alist-get 'path file))
                                        (approve-pr-files))))
              :to-equal 300)
      (expect (approve-model-total-count (approve-pr-field 'files)) :to-equal 300)
      (expect (approve-model-has-next-page-p (approve-pr-field 'files)) :to-be nil)
      (expect (approve-pr-field 'changedFiles) :to-equal 300)))

  (it "loads every page of the patches, up to the empty last one"
    (expect patches :to-equal '(4 . 300))
    (with-current-buffer buffer
      (let ((stored (approve-model-extra :patches)))
        (expect (length stored) :to-equal 298)
        (expect (assoc "pkg/hotel/icon_10.png" stored) :to-be nil)
        (expect (assoc "pkg/mike/schema_10.json" stored) :to-be nil)
        (expect (cdr (assoc "pkg/alpha/file_00.go" stored)) :to-match "\\`@@ -"))
      (expect (alist-get 'additions
                         (seq-find (lambda (file)
                                     (equal (alist-get 'path file) "pkg/mike/schema_10.json"))
                                   (approve-pr-files)))
              :to-equal 3500))))

;;; test-loading.el ends here
