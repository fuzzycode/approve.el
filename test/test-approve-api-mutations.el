;;; test-approve-api-mutations.el --- Tests for approve-api-mutations  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the approve-api-mutations module.

;;; Code:

(require 'buttercup)
(require 'approve-api-mutations)
(require 'approve-api)
(require 'approve-graphql)

(describe "approve-api-mutations"

  (describe "approve-api-mutation-update-pr-title"

    (before-each
      (spy-on 'approve-api-graphql :and-return-value 123)
      (spy-on 'approve-graphql-load :and-return-value "mutation UpdatePR..."))

    (it "calls approve-api-graphql with the mutation"
      (approve-api-mutation-update-pr-title "PR_abc123" "New Title")
      (expect 'approve-api-graphql :to-have-been-called))

    (it "loads the correct GraphQL mutation file"
      (approve-api-mutation-update-pr-title "PR_abc123" "New Title")
      (expect 'approve-graphql-load :to-have-been-called-with
              "mutations/update-pull-request-title.graphql"))

    (it "passes PR ID and title as variables"
      (let ((captured-vars nil))
        (spy-on 'approve-api-graphql :and-call-fake
                (lambda (query vars &rest _)
                  (setq captured-vars vars)
                  123))
        (approve-api-mutation-update-pr-title "PR_abc123" "New Title")
        (expect (alist-get 'pullRequestId captured-vars) :to-equal "PR_abc123")
        (expect (alist-get 'title captured-vars) :to-equal "New Title")))

    (it "returns the request ID from approve-api-graphql"
      (let ((result (approve-api-mutation-update-pr-title "PR_abc123" "New Title")))
        (expect result :to-equal 123)))

    (it "passes on-success callback"
      (let ((captured-callback nil)
            (my-callback (lambda (data) data)))
        (spy-on 'approve-api-graphql :and-call-fake
                (lambda (query vars &rest args)
                  (setq captured-callback (plist-get args :callback))
                  123))
        (approve-api-mutation-update-pr-title
         "PR_abc123" "New Title"
         :on-success my-callback)
        (expect captured-callback :to-equal my-callback)))

    (it "passes on-error callback"
      (let ((captured-callback nil)
            (my-callback (lambda (err) err)))
        (spy-on 'approve-api-graphql :and-call-fake
                (lambda (query vars &rest args)
                  (setq captured-callback (plist-get args :error-callback))
                  123))
        (approve-api-mutation-update-pr-title
         "PR_abc123" "New Title"
         :on-error my-callback)
        (expect captured-callback :to-equal my-callback)))

    (it "passes buffer context"
      (let ((captured-buffer nil)
            (test-buffer (generate-new-buffer "test-buffer")))
        (unwind-protect
            (progn
              (spy-on 'approve-api-graphql :and-call-fake
                      (lambda (query vars &rest args)
                        (setq captured-buffer (plist-get args :buffer))
                        123))
              (approve-api-mutation-update-pr-title
               "PR_abc123" "New Title"
               :buffer test-buffer)
              (expect captured-buffer :to-equal test-buffer))
          (kill-buffer test-buffer))))

    (it "uses current buffer as default"
      (let ((captured-buffer nil))
        (spy-on 'approve-api-graphql :and-call-fake
                (lambda (query vars &rest args)
                  (setq captured-buffer (plist-get args :buffer))
                  123))
        (approve-api-mutation-update-pr-title "PR_abc123" "New Title")
        (expect captured-buffer :to-equal (current-buffer))))

    (it "sets appropriate progress message"
      (let ((captured-message nil))
        (spy-on 'approve-api-graphql :and-call-fake
                (lambda (query vars &rest args)
                  (setq captured-message (plist-get args :progress-message))
                  123))
        (approve-api-mutation-update-pr-title "PR_abc123" "New Title")
        (expect captured-message :to-equal "Updating PR title...")))))

(provide 'test-approve-api-mutations)
;;; test-approve-api-mutations.el ends here
