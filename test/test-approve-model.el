;;; test-approve-model.el --- Tests for approve-model  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the Approve data model layer.

;;; Code:

(require 'buttercup)
(require 'approve-model)

;;; Test Fixtures

(defun test-approve-model--make-user (id login &optional name)
  "Create a test user entity."
  `((__typename . "User")
    (id . ,id)
    (login . ,login)
    (name . ,(or name login))
    (url . ,(format "https://github.com/%s" login))))

(defun test-approve-model--make-comment (id author-id body)
  "Create a test comment entity."
  `((__typename . "IssueComment")
    (id . ,id)
    (body . ,body)
    (author . ,(test-approve-model--make-user author-id "testuser"))))

(defun test-approve-model--make-pr (id number title &optional comments)
  "Create a test pull request entity."
  `((__typename . "PullRequest")
    (id . ,id)
    (number . ,number)
    (title . ,title)
    (state . "OPEN")
    (bodyHTML . "<p>Test body</p>")
    (author . ,(test-approve-model--make-user "U_author" "prauthor" "PR Author"))
    (comments . ,(or comments
                     `((totalCount . 0)
                       (nodes . []))))))

(defun test-approve-model--make-pr-with-comments (id number title)
  "Create a test PR with nested comments."
  `((__typename . "PullRequest")
    (id . ,id)
    (number . ,number)
    (title . ,title)
    (state . "OPEN")
    (author . ,(test-approve-model--make-user "U_author" "prauthor"))
    (comments . ((totalCount . 2)
                 (nodes . [,(test-approve-model--make-comment "IC_1" "U_commenter1" "First comment")
                           ,(test-approve-model--make-comment "IC_2" "U_commenter2" "Second comment")])))))

;;; Tests

(describe "approve-model"

  (before-each
    (approve-model-init))

  (after-each
    (approve-model-clear))

  (describe "store initialization"

    (it "initializes an empty store"
      (expect (approve-model-initialized-p) :to-be-truthy))

    (it "accepts metadata on initialization"
      (approve-model-init '(:owner "fuzzycode" :repo "Approve.el" :number 42))
      (expect (approve-model-metadata :owner) :to-equal "fuzzycode")
      (expect (approve-model-metadata :repo) :to-equal "Approve.el")
      (expect (approve-model-metadata :number) :to-equal 42)))

  (describe "reference handling"

    (it "creates valid references"
      (let ((ref (approve-model-make-ref "User" "U_123")))
        (expect (approve-model-ref-p ref) :to-be-truthy)
        (expect (approve-model-ref-type ref) :to-equal "User")
        (expect (approve-model-ref-id ref) :to-equal "U_123")))

    (it "identifies non-references"
      (expect (approve-model-ref-p nil) :to-be nil)
      (expect (approve-model-ref-p "string") :to-be nil)
      (expect (approve-model-ref-p '(a b c)) :to-be nil)
      (expect (approve-model-ref-p '((:approve-ref "User" "U_1"))) :to-be nil)))

  (describe "loading data"

    (it "loads and normalizes a simple entity"
      (let ((ref (approve-model-load (test-approve-model--make-user "U_1" "johndoe"))))
        (expect (approve-model-ref-p ref) :to-be-truthy)
        (expect (approve-model-ref-type ref) :to-equal "User")
        (expect (approve-model-ref-id ref) :to-equal "U_1")))

    (it "stores entities retrievably"
      (approve-model-load (test-approve-model--make-user "U_1" "johndoe" "John Doe"))
      (let ((user (approve-model-get "User" "U_1")))
        (expect (alist-get 'login user) :to-equal "johndoe")
        (expect (alist-get 'name user) :to-equal "John Doe")))

    (it "extracts single fields"
      (approve-model-load (test-approve-model--make-user "U_1" "johndoe"))
      (expect (approve-model-get "User" "U_1" 'login) :to-equal "johndoe"))

    (it "extracts multiple fields as alist"
      (approve-model-load (test-approve-model--make-user "U_1" "johndoe" "John Doe"))
      (let ((fields (approve-model-get "User" "U_1" '(login name))))
        (expect (alist-get 'login fields) :to-equal "johndoe")
        (expect (alist-get 'name fields) :to-equal "John Doe")))

    (it "normalizes nested entities"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      ;; PR should exist
      (expect (approve-model-has-entity-p "PullRequest" "PR_1") :to-be-truthy)
      ;; Author should be normalized separately
      (expect (approve-model-has-entity-p "User" "U_author") :to-be-truthy))

    (it "sets root entity when requested"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      (let ((root (approve-model-root)))
        (expect (alist-get 'title root) :to-equal "Test PR")))

    (it "unwraps GraphQL connection patterns"
      (let ((data (test-approve-model--make-pr-with-comments "PR_1" 42 "Test PR")))
        (approve-model-load data t)
        ;; Comments with totalCount should be wrapped with pagination metadata
        (let ((comments-data (approve-model-root 'comments)))
          ;; Should have nodes and totalCount
          (expect (alist-get 'totalCount comments-data) :to-equal 2)
          (let ((comments (alist-get 'nodes comments-data)))
            (expect (length comments) :to-equal 2)
            ;; Each comment should be resolved
            (expect (alist-get 'body (car comments)) :to-equal "First comment")))))

    (it "handles non-normalizable data"
      (let ((result (approve-model-load '((foo . "bar") (baz . 123)))))
        ;; Should return the data as-is (not a ref)
        (expect (approve-model-ref-p result) :to-be nil)
        (expect (alist-get 'foo result) :to-equal "bar"))))

  (describe "reference resolution"

    (it "resolves references when getting entities"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      (let* ((pr (approve-model-root))
             (author (alist-get 'author pr)))
        ;; Author should be resolved, not a ref
        (expect (approve-model-ref-p author) :to-be nil)
        (expect (alist-get 'login author) :to-equal "prauthor")))

    (it "resolves nested references in lists"
      (approve-model-load (test-approve-model--make-pr-with-comments "PR_1" 42 "Test PR") t)
      (let* ((pr (approve-model-root))
             (comments-data (alist-get 'comments pr))
             (comments (alist-get 'nodes comments-data))
             (first-comment (car comments))
             (author (alist-get 'author first-comment)))
        ;; Comment authors should be resolved
        (expect (alist-get 'login author) :to-equal "testuser")))

    (it "gets entity by reference"
      (let ((ref (approve-model-load (test-approve-model--make-user "U_1" "johndoe"))))
        (let ((user (approve-model-get-ref ref)))
          (expect (alist-get 'login user) :to-equal "johndoe"))))

    (it "gets field by reference"
      (let ((ref (approve-model-load (test-approve-model--make-user "U_1" "johndoe"))))
        (expect (approve-model-get-ref ref 'login) :to-equal "johndoe"))))

  (describe "entity queries"

    (it "lists all entities of a type"
      (approve-model-load (test-approve-model--make-user "U_1" "user1"))
      (approve-model-load (test-approve-model--make-user "U_2" "user2"))
      (approve-model-load (test-approve-model--make-user "U_3" "user3"))
      (let ((users (approve-model-entities "User")))
        (expect (length users) :to-equal 3)))

    (it "lists all IDs for a type"
      (approve-model-load (test-approve-model--make-user "U_1" "user1"))
      (approve-model-load (test-approve-model--make-user "U_2" "user2"))
      (let ((ids (approve-model-entity-ids "User")))
        (expect (length ids) :to-equal 2)
        (expect ids :to-contain "U_1")
        (expect ids :to-contain "U_2")))

    (it "checks entity existence"
      (approve-model-load (test-approve-model--make-user "U_1" "user1"))
      (expect (approve-model-has-entity-p "User" "U_1") :to-be-truthy)
      (expect (approve-model-has-entity-p "User" "U_999") :to-be nil)
      (expect (approve-model-has-entity-p "NonExistent" "X_1") :to-be nil)))

  (describe "update operations"

    (it "updates a single field"
      (approve-model-load (test-approve-model--make-user "U_1" "oldlogin"))
      (approve-model-update "User" "U_1" 'login "newlogin")
      (expect (approve-model-get "User" "U_1" 'login) :to-equal "newlogin"))

    (it "updates root entity"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Old Title") t)
      (approve-model-update-root 'title "New Title")
      (expect (approve-model-root 'title) :to-equal "New Title"))

    (it "adds new fields"
      (approve-model-load (test-approve-model--make-user "U_1" "user1"))
      (approve-model-update "User" "U_1" 'newField "new value")
      (expect (approve-model-get "User" "U_1" 'newField) :to-equal "new value"))

    (it "deletes entities"
      (approve-model-load (test-approve-model--make-user "U_1" "user1"))
      (expect (approve-model-has-entity-p "User" "U_1") :to-be-truthy)
      (approve-model-delete "User" "U_1")
      (expect (approve-model-has-entity-p "User" "U_1") :to-be nil)))

  (describe "patching"

    (it "patches existing entities with new data"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Original Title") t)
      ;; Simulate mutation response
      (approve-model-patch '((__typename . "PullRequest")
                             (id . "PR_1")
                             (title . "Updated Title")
                             (state . "CLOSED")))
      ;; Title should be updated
      (expect (approve-model-root 'title) :to-equal "Updated Title")
      ;; State should be updated
      (expect (approve-model-root 'state) :to-equal "CLOSED")))

  (describe "metadata"

    (it "stores and retrieves metadata"
      (approve-model-set-metadata :custom "value")
      (expect (approve-model-metadata :custom) :to-equal "value"))

    (it "returns full metadata plist"
      (approve-model-init '(:owner "test" :repo "repo"))
      (let ((meta (approve-model-metadata)))
        (expect (plist-get meta :owner) :to-equal "test"))))

  (describe "error handling"

    (it "signals error for non-existent entity"
      (expect (approve-model-get "User" "nonexistent")
              :to-throw 'approve-model-not-found))

    (it "signals error for invalid reference"
      (expect (approve-model-get-ref '(not a valid ref))
              :to-throw 'approve-model-invalid-ref))

    (it "signals error when no root is set"
      (expect (approve-model-root)
              :to-throw 'approve-model-not-found)))

  (describe "with-approve-entity macro"

    (it "binds fields from root entity"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      (with-approve-entity ((:root) (title state number))
        (expect title :to-equal "Test PR")
        (expect state :to-equal "OPEN")
        (expect number :to-equal 42)))

    (it "converts camelCase to lisp-case"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      (with-approve-entity ((:root) (bodyHTML))
        (expect body-html :to-equal "<p>Test body</p>")))

    (it "binds fields from specific entity"
      (approve-model-load (test-approve-model--make-user "U_1" "testuser" "Test User"))
      (with-approve-entity (("User" "U_1") (login name))
        (expect login :to-equal "testuser")
        (expect name :to-equal "Test User")))

    (it "binds fields from reference"
      (let ((ref (approve-model-load (test-approve-model--make-user "U_1" "testuser"))))
        (with-approve-entity ((:ref ref) (login))
          (expect login :to-equal "testuser")))))

  (describe "approve-model-let macro"

    (it "binds multiple entity fields"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      (approve-model-let ((pr-title (:root) title)
                          (pr-state (:root) state))
        (expect pr-title :to-equal "Test PR")
        (expect pr-state :to-equal "OPEN")))

    (it "binds from different entities"
      (approve-model-load (test-approve-model--make-pr "PR_1" 42 "Test PR") t)
      (approve-model-load (test-approve-model--make-user "U_extra" "extrauser"))
      (approve-model-let ((pr-title (:root) title)
                          (user-login ("User" "U_extra") login))
        (expect pr-title :to-equal "Test PR")
        (expect user-login :to-equal "extrauser"))))

  (describe "approve-model--field-to-var"

    (it "converts simple field names"
      (expect (approve-model--field-to-var 'title) :to-equal 'title)
      (expect (approve-model--field-to-var 'state) :to-equal 'state))

    (it "converts camelCase to lisp-case"
      (expect (approve-model--field-to-var 'bodyHTML) :to-equal 'body-html)
      (expect (approve-model--field-to-var 'createdAt) :to-equal 'created-at)
      (expect (approve-model--field-to-var 'viewerCanUpdate) :to-equal 'viewer-can-update)))

  (describe "pagination helpers"

    (describe "approve-model-paginated-p"

      (it "returns non-nil for data with nodes and totalCount"
        (let ((data '((nodes . ((item1) (item2))) (totalCount . 5))))
          (expect (approve-model-paginated-p data) :to-be-truthy)))

      (it "returns non-nil for data with nodes and pageInfo"
        (let ((data '((nodes . ((item1))) (pageInfo . ((hasNextPage . t))))))
          (expect (approve-model-paginated-p data) :to-be-truthy)))

      (it "returns nil for plain list"
        (expect (approve-model-paginated-p '((item1) (item2))) :to-be nil))

      (it "returns nil for data with only nodes (no metadata)"
        (let ((data '((nodes . ((item1))))))
          (expect (approve-model-paginated-p data) :to-be nil))))

    (describe "approve-model-get-nodes"

      (it "extracts nodes from paginated wrapper"
        (let ((data '((nodes . ((item1) (item2))) (totalCount . 5))))
          (expect (approve-model-get-nodes data) :to-equal '((item1) (item2)))))

      (it "returns plain list unchanged"
        (let ((data '((item1) (item2))))
          (expect (approve-model-get-nodes data) :to-equal data))))

    (describe "approve-model-get-total-count"

      (it "returns totalCount from paginated wrapper"
        (let ((data '((nodes . ((item1))) (totalCount . 42))))
          (expect (approve-model-get-total-count data) :to-equal 42)))

      (it "returns nil for non-paginated data"
        (expect (approve-model-get-total-count '((item1))) :to-be nil)))

    (describe "approve-model-get-page-info"

      (it "returns pageInfo from paginated wrapper"
        (let ((data '((nodes . ((item1))) (totalCount . 5) (pageInfo . ((hasNextPage . t) (endCursor . "abc"))))))
          (expect (approve-model-get-page-info data)
                  :to-equal '((hasNextPage . t) (endCursor . "abc")))))

      (it "returns nil for non-paginated data"
        (expect (approve-model-get-page-info '((item1))) :to-be nil)))

    (describe "approve-model-has-next-page-p"

      (it "returns t when hasNextPage is true"
        (let ((data '((nodes . ((item1))) (totalCount . 5) (pageInfo . ((hasNextPage . t))))))
          (expect (approve-model-has-next-page-p data) :to-be t)))

      (it "returns nil when hasNextPage is false"
        (let ((data '((nodes . ((item1))) (totalCount . 5) (pageInfo . ((hasNextPage . nil))))))
          (expect (approve-model-has-next-page-p data) :to-be nil)))

      (it "returns nil when no pageInfo"
        (let ((data '((nodes . ((item1))) (totalCount . 5))))
          (expect (approve-model-has-next-page-p data) :to-be nil))))

    (describe "approve-model-truncated-p"

      (it "returns t when hasNextPage is true"
        (let ((data '((nodes . ((item1))) (totalCount . 1) (pageInfo . ((hasNextPage . t))))))
          (expect (approve-model-truncated-p data) :to-be-truthy)))

      (it "returns t when totalCount exceeds nodes length"
        (let ((data '((nodes . ((item1) (item2))) (totalCount . 5))))
          (expect (approve-model-truncated-p data) :to-be-truthy)))

      (it "returns nil when all items are present"
        (let ((data '((nodes . ((item1) (item2))) (totalCount . 2) (pageInfo . ((hasNextPage . nil))))))
          (expect (approve-model-truncated-p data) :to-be nil)))

      (it "returns nil for non-paginated data"
        (expect (approve-model-truncated-p '((item1) (item2))) :to-be nil))))

  (describe "typename validation"

    (it "signals error when entity has id but no __typename"
      (let ((bad-entity '((id . "U_123") (login . "testuser"))))
        (expect (approve-model-load bad-entity)
                :to-throw 'error)))

    (it "allows data without id (not an entity)"
      (let ((plain-data '((foo . "bar") (baz . 123))))
        (expect (approve-model-load plain-data) :not :to-throw)))

    (it "signals error for nested entities missing __typename"
      (let ((data '((__typename . "PullRequest")
                    (id . "PR_1")
                    (author . ((id . "U_123") (login . "testuser"))))))
        (expect (approve-model-load data)
                :to-throw 'error)))))

(provide 'test-approve-model)
;;; test-approve-model.el ends here
