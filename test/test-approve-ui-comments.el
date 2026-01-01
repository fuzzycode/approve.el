;;; test-approve-ui-comments.el --- Tests for approve-ui-comments  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the comment UI functionality in Approve.

;;; Code:

(require 'buttercup)
(require 'approve-ui-comments)
(require 'approve-ui-html)
(require 'approve-eldoc)

(describe "approve-ui-comments"
  (describe "approve-comment--make-edited-info"
    (it "returns (t . timestamp) when comment was edited with timestamp"
      (let ((comment '((includesCreatedEdit . t)
                       (lastEditedAt . "2025-01-15T10:30:00Z"))))
        (expect (approve-comment--make-edited-info comment)
                :to-equal '(t . "2025-01-15T10:30:00Z"))))

    (it "returns (t . nil) when comment was edited but no timestamp"
      (let ((comment '((includesCreatedEdit . t)
                       (lastEditedAt))))
        (expect (approve-comment--make-edited-info comment)
                :to-equal '(t . nil))))

    (it "returns (nil . nil) when comment was not edited"
      (let ((comment '((includesCreatedEdit)
                       (lastEditedAt))))
        (expect (approve-comment--make-edited-info comment)
                :to-equal '(nil . nil))))

    (it "returns (nil . timestamp) when timestamp present but not marked as edited"
      ;; This case shouldn't really happen in practice, but we handle it
      (let ((comment '((includesCreatedEdit)
                       (lastEditedAt . "2025-01-15T10:30:00Z"))))
        (expect (approve-comment--make-edited-info comment)
                :to-equal '(nil . "2025-01-15T10:30:00Z")))))

  (describe "approve-comment--format-edited-indicator"
    (it "returns nil when edited-info is nil"
      (expect (approve-comment--format-edited-indicator nil)
              :to-be nil))

    (it "returns nil when comment was not edited"
      (expect (approve-comment--format-edited-indicator '(nil . nil))
              :to-be nil))

    (it "returns '(edited)' text when comment was edited"
      (let ((result (approve-comment--format-edited-indicator '(t . nil))))
        (expect result :not :to-be nil)
        (expect (substring-no-properties result) :to-equal "(edited)")))

    (it "applies the edited face"
      (let ((result (approve-comment--format-edited-indicator '(t . nil))))
        (expect (get-text-property 0 'face result)
                :to-equal 'approve-comment-edited-face)))

    (it "includes last edited date in eldoc when available"
      (let ((result (approve-comment--format-edited-indicator
                     '(t . "2025-01-15T10:30:00Z"))))
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-match "2025-01-15")))

    (it "shows 'Edited' in eldoc when no timestamp available"
      (let ((result (approve-comment--format-edited-indicator '(t . nil))))
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-equal "Edited")))

    (it "formats timestamp according to approve-timestamp-format"
      (let* ((approve-timestamp-format "%Y-%m-%d")
             (result (approve-comment--format-edited-indicator
                      '(t . "2025-01-15T10:30:00Z"))))
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-match "2025-01-15"))))

  (describe "approve-comment--format-header with edited info"
    (it "includes edited indicator when comment was edited"
      (let* ((author '((login . "testuser")))
             (created-at "2025-01-14T09:00:00Z")
             (edited-info '(t . "2025-01-15T10:30:00Z"))
             (result (approve-comment--format-header author created-at nil edited-info)))
        (expect result :to-match "(edited)")))

    (it "does not include edited indicator when comment was not edited"
      (let* ((author '((login . "testuser")))
             (created-at "2025-01-14T09:00:00Z")
             (edited-info '(nil . nil))
             (result (approve-comment--format-header author created-at nil edited-info)))
        (expect result :not :to-match "(edited)")))

    (it "does not include edited indicator when edited-info is nil"
      (let* ((author '((login . "testuser")))
             (created-at "2025-01-14T09:00:00Z")
             (result (approve-comment--format-header author created-at nil nil)))
        (expect result :not :to-match "(edited)")))))

;;; test-approve-ui-comments.el ends here
