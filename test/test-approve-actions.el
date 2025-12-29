;;; test-approve-actions.el --- Tests for approve-actions  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the approve-actions module.

;;; Code:

(require 'buttercup)
(require 'approve-actions)
(require 'approve-model)

(describe "approve-actions"

  (describe "approve-actions--check-permission"

    (before-each
      (approve-model-init))

    (after-each
      (approve-model-clear))

    (it "returns non-nil when permission is granted"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (viewerCanUpdate . t))
                          t)
      (expect (approve-actions--check-permission 'viewerCanUpdate "edit")
              :to-be-truthy))

    (it "returns nil when permission is denied"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (viewerCanUpdate . nil))
                          t)
      (expect (approve-actions--check-permission 'viewerCanUpdate "edit")
              :to-be nil))

    (it "displays message when permission is denied"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (viewerCanUpdate . nil))
                          t)
      (spy-on 'message)
      (approve-actions--check-permission 'viewerCanUpdate "edit title")
      (expect 'message :to-have-been-called-with
              "Cannot %s: you don't have permission" "edit title")))

  (describe "approve-action-browse-pr"

    (before-each
      (approve-model-init))

    (after-each
      (approve-model-clear))

    (it "calls browse-url with the PR URL"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (url . "https://github.com/owner/repo/pull/42"))
                          t)
      (spy-on 'browse-url)
      (approve-action-browse-pr)
      (expect 'browse-url :to-have-been-called-with
              "https://github.com/owner/repo/pull/42"))

    (it "signals error when model is not initialized"
      (approve-model-clear)
      (setq approve-model--store nil)
      (expect (approve-action-browse-pr)
              :to-throw 'user-error))

    (it "signals error when URL is not available"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123"))
                          t)
      (expect (approve-action-browse-pr)
              :to-throw 'user-error)))

  (describe "approve-action-edit-title"

    (before-each
      (approve-model-init))

    (after-each
      (approve-model-clear))

    (it "signals error when model is not initialized"
      (approve-model-clear)
      (setq approve-model--store nil)
      (expect (approve-action-edit-title)
              :to-throw 'user-error))

    (it "displays message and returns when user lacks permission"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (title . "Test PR")
                            (viewerCanUpdate . nil))
                          t)
      (spy-on 'message)
      (spy-on 'approve-input-read)
      (approve-action-edit-title)
      (expect 'message :to-have-been-called)
      (expect 'approve-input-read :not :to-have-been-called))

    (it "opens input buffer when user has permission"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (title . "Test PR")
                            (viewerCanUpdate . t))
                          t)
      (spy-on 'approve-input-read)
      (approve-action-edit-title)
      (expect 'approve-input-read :to-have-been-called))

    (it "passes current title as initial content"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (title . "My Current Title")
                            (viewerCanUpdate . t))
                          t)
      (let ((captured-args nil))
        (spy-on 'approve-input-read :and-call-fake
                (lambda (&rest args)
                  (setq captured-args args)))
        (approve-action-edit-title)
        (expect (plist-get captured-args :initial) :to-equal "My Current Title")))

    (it "sets multiline to nil for single-line input"
      (approve-model-load '((__typename . "PullRequest")
                            (id . "PR_123")
                            (title . "Test PR")
                            (viewerCanUpdate . t))
                          t)
      (let ((captured-args nil))
        (spy-on 'approve-input-read :and-call-fake
                (lambda (&rest args)
                  (setq captured-args args)))
        (approve-action-edit-title)
        (expect (plist-get captured-args :multiline) :to-be nil))))

  (describe "approve-actions--redraw-preserving-point"

    (it "calls approve-ui-redraw"
      (spy-on 'approve-ui-redraw)
      (spy-on 'recenter)
      (with-temp-buffer
        (insert "line 1\nline 2\n")
        (goto-char (point-min))
        (approve-actions--redraw-preserving-point)
        (expect 'approve-ui-redraw :to-have-been-called)))

    (it "restores point position after redraw"
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3\n")
        (goto-char (point-min))
        (forward-line 1)
        (forward-char 3)
        (let ((orig-line (line-number-at-pos))
              (orig-col (current-column)))
          (spy-on 'approve-ui-redraw :and-call-fake
                  (lambda ()
                    (erase-buffer)
                    (insert "new line 1\nnew line 2\nnew line 3\n")))
          (spy-on 'recenter)
          (approve-actions--redraw-preserving-point)
          (expect (line-number-at-pos) :to-equal orig-line)
          (expect (current-column) :to-equal orig-col))))))

(provide 'test-approve-actions)
;;; test-approve-actions.el ends here
