;;; test-approve-ui.el --- Tests for approve-ui  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the Approve UI layer.

;;; Code:

(require 'buttercup)
(require 'approve-ui)
(require 'approve-model)

;;; Test Helpers

(defvar test-approve-ui--query-calls nil
  "List of captured approve-api-query-pull-request calls.")

(defvar test-approve-ui--last-callback nil
  "Stored callback from last query call.")

(defun test-approve-ui--mock-query (owner repo number &rest args)
  "Mock implementation of `approve-api-query-pull-request'."
  (push (list :owner owner :repo repo :number number :args args)
        test-approve-ui--query-calls)
  (setq test-approve-ui--last-callback (plist-get args :callback))
  1) ; Return mock request ID

(defun test-approve-ui--reset-mocks ()
  "Reset all mock state."
  (setq test-approve-ui--query-calls nil)
  (setq test-approve-ui--last-callback nil))

(defun test-approve-ui--make-pr-response ()
  "Create a mock PR response from GitHub API."
  '((repository
     . ((url . "https://github.com/fuzzycode/Approve.el")
        (viewerPermission . "ADMIN")
        (labels . ((totalCount . 0) (nodes . [])))
        (assignableUsers . ((totalCount . 1) (nodes . [])))
        (mentionableUsers . ((totalCount . 1) (nodes . [])))
        (milestones . ((totalCount . 0) (nodes . [])))
        (pullRequest
         . ((__typename . "PullRequest")
            (id . "PR_123")
            (number . 42)
            (title . "Test PR")
            (state . "OPEN")
            (author . ((__typename . "User")
                       (id . "U_456")
                       (login . "testuser")))))))))

;;; Tests

(describe "approve-ui"

  (before-each
    (test-approve-ui--reset-mocks)
    (spy-on 'approve-api-query-pull-request
            :and-call-fake #'test-approve-ui--mock-query))

  (describe "approve-ui--buffer-name"

    (it "generates correct buffer name"
      (expect (approve-ui--buffer-name "fuzzycode" "Approve.el" 42)
              :to-equal "*Approve: fuzzycode/Approve.el#42*"))

    (it "handles different PR numbers"
      (expect (approve-ui--buffer-name "owner" "repo" 1)
              :to-equal "*Approve: owner/repo#1*")
      (expect (approve-ui--buffer-name "owner" "repo" 999)
              :to-equal "*Approve: owner/repo#999*")))

  (describe "approve-ui--get-or-create-buffer"

    (it "creates a new buffer if none exists"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 1)))
        (unwind-protect
            (progn
              (expect (buffer-live-p buffer) :to-be-truthy)
              (expect (buffer-name buffer) :to-equal "*Approve: owner/repo#1*"))
          (kill-buffer buffer))))

    (it "reuses existing buffer"
      (let* ((buffer1 (approve-ui--get-or-create-buffer "owner" "repo" 1))
             (buffer2 (approve-ui--get-or-create-buffer "owner" "repo" 1)))
        (unwind-protect
            (expect buffer1 :to-equal buffer2)
          (kill-buffer buffer1))))

    (it "sets major mode to approve-review-mode"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 1)))
        (unwind-protect
            (with-current-buffer buffer
              (expect major-mode :to-equal 'approve-review-mode))
          (kill-buffer buffer))))

    (it "initializes the model with metadata"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 42)))
        (unwind-protect
            (with-current-buffer buffer
              (expect (approve-model-initialized-p) :to-be-truthy)
              (expect (approve-model-metadata :owner) :to-equal "owner")
              (expect (approve-model-metadata :repo) :to-equal "repo")
              (expect (approve-model-metadata :number) :to-equal 42))
          (kill-buffer buffer)))))

  (describe "approve-ui-view-pr"

    (it "creates buffer and fetches PR data"
      (let ((buffer nil))
        (unwind-protect
            (progn
              (approve-ui-view-pr "fuzzycode" "Approve.el" 42)
              ;; Should have called the API
              (expect (length test-approve-ui--query-calls) :to-equal 1)
              (let ((call (car test-approve-ui--query-calls)))
                (expect (plist-get call :owner) :to-equal "fuzzycode")
                (expect (plist-get call :repo) :to-equal "Approve.el")
                (expect (plist-get call :number) :to-equal 42)))
          (when (and buffer (buffer-live-p buffer))
            (kill-buffer buffer))))))

  (describe "approve-ui-refresh"

    (it "fetches PR data using stored metadata"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 123)))
        (unwind-protect
            (with-current-buffer buffer
              (approve-ui-refresh)
              (expect (length test-approve-ui--query-calls) :to-equal 1)
              (let ((call (car test-approve-ui--query-calls)))
                (expect (plist-get call :owner) :to-equal "owner")
                (expect (plist-get call :repo) :to-equal "repo")
                (expect (plist-get call :number) :to-equal 123)))
          (kill-buffer buffer))))

    (it "signals error if not in an Approve buffer"
      (with-temp-buffer
        (expect (approve-ui-refresh) :to-throw 'user-error)))

    (it "signals error if metadata is missing"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-review-mode)
              (approve-model-init nil)
              (expect (approve-ui-refresh) :to-throw 'user-error))
          (kill-buffer buffer)))))

  (describe "approve-ui--handle-fetch-success"

    (it "loads PR data into the model as root"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 42)))
        (unwind-protect
            (with-current-buffer buffer
              (approve-ui--handle-fetch-success (test-approve-ui--make-pr-response))
              (expect (approve-model-root 'title) :to-equal "Test PR")
              (expect (approve-model-root 'state) :to-equal "OPEN")
              (expect (approve-model-root 'number) :to-equal 42))
          (kill-buffer buffer))))

    (it "stores repository metadata"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 42)))
        (unwind-protect
            (with-current-buffer buffer
              (approve-ui--handle-fetch-success (test-approve-ui--make-pr-response))
              (expect (approve-model-metadata :repository-url)
                      :to-equal "https://github.com/fuzzycode/Approve.el")
              (expect (approve-model-metadata :viewer-permission)
                      :to-equal "ADMIN"))
          (kill-buffer buffer))))

    (it "redraws the buffer"
      (let ((buffer (approve-ui--get-or-create-buffer "owner" "repo" 42)))
        (unwind-protect
            (with-current-buffer buffer
              (spy-on 'approve-ui-redraw)
              (approve-ui--handle-fetch-success (test-approve-ui--make-pr-response))
              (expect 'approve-ui-redraw :to-have-been-called))
          (kill-buffer buffer)))))



  (describe "approve-review-mode"

    (it "derives from magit-section-mode"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-review-mode)
              (expect (derived-mode-p 'magit-section-mode) :to-be-truthy))
          (kill-buffer buffer))))

    (it "sets revert-buffer-function"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-review-mode)
              (expect revert-buffer-function :to-equal #'approve-ui-refresh))
          (kill-buffer buffer))))

    (it "makes buffer read-only"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-review-mode)
              (expect buffer-read-only :to-be-truthy))
          (kill-buffer buffer))))

    (it "binds g to refresh"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-review-mode)
              (expect (lookup-key approve-review-mode-map "g")
                      :to-equal #'approve-ui-refresh))
          (kill-buffer buffer))))))

(provide 'test-approve-ui)
;;; test-approve-ui.el ends here
