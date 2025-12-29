;;; test-approve-eldoc.el --- Tests for approve-eldoc  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Unit tests for the Approve eldoc integration.

;;; Code:

(require 'buttercup)
(require 'approve-eldoc)

(describe "approve-eldoc"

  (describe "approve-eldoc-format-user"

    (it "returns nil when name and email are both nil"
      (expect (approve-eldoc-format-user "octocat" nil nil)
              :to-be nil))

    (it "returns nil when name and email are both empty"
      (expect (approve-eldoc-format-user "octocat" "" "")
              :to-be nil))

    (it "returns just name when email is nil"
      (expect (approve-eldoc-format-user "octocat" "The Octocat" nil)
              :to-equal "The Octocat"))

    (it "returns just name when email is empty"
      (expect (approve-eldoc-format-user "octocat" "The Octocat" "")
              :to-equal "The Octocat"))

    (it "returns just email when name is nil"
      (expect (approve-eldoc-format-user "octocat" nil "octocat@github.com")
              :to-equal "<octocat@github.com>"))

    (it "returns just email when name is empty"
      (expect (approve-eldoc-format-user "octocat" "" "octocat@github.com")
              :to-equal "<octocat@github.com>"))

    (it "returns name and email when both are present"
      (expect (approve-eldoc-format-user "octocat" "The Octocat" "octocat@github.com")
              :to-equal "The Octocat <octocat@github.com>")))

  (describe "approve-eldoc-propertize"

    (it "returns text unchanged when doc is nil"
      (let ((result (approve-eldoc-propertize "test" "thing" nil)))
        (expect result :to-equal "test")
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-be nil)))

    (it "returns text unchanged when doc is empty"
      (let ((result (approve-eldoc-propertize "test" "thing" "")))
        (expect result :to-equal "test")
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-be nil)))

    (it "adds doc property when doc is provided"
      (let ((result (approve-eldoc-propertize "test" "thing" "documentation")))
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-equal "documentation")))

    (it "adds thing property when doc is provided"
      (let ((result (approve-eldoc-propertize "test" "thing" "documentation")))
        (expect (get-text-property 0 'approve-eldoc-thing result)
                :to-equal "thing")))

    (it "adds face property when provided"
      (let ((result (approve-eldoc-propertize "test" "thing" "doc" 'bold)))
        (expect (get-text-property 0 'approve-eldoc-face result)
                :to-equal 'bold)))

    (it "preserves existing text properties"
      (let* ((input (propertize "test" 'face 'bold))
             (result (approve-eldoc-propertize input "thing" "doc")))
        (expect (get-text-property 0 'face result)
                :to-equal 'bold)
        (expect (get-text-property 0 'approve-eldoc-doc result)
                :to-equal "doc"))))

  (describe "approve-eldoc--documentation-function"

    (it "calls callback with doc when at position with doc"
      (with-temp-buffer
        (insert (approve-eldoc-propertize "test" "@user" "User Name"))
        (goto-char (point-min))
        (let ((called-with nil))
          (approve-eldoc--documentation-function
           (lambda (doc &rest args)
             (setq called-with (list doc args))))
          (expect (car called-with) :to-equal "User Name")
          (expect (plist-get (cadr called-with) :thing) :to-equal "@user"))))

    (it "does not call callback when at position without doc"
      (with-temp-buffer
        (insert "plain text")
        (goto-char (point-min))
        (let ((called nil))
          (approve-eldoc--documentation-function
           (lambda (&rest _) (setq called t)))
          (expect called :to-be nil))))

    (it "uses provided face in callback"
      (with-temp-buffer
        (insert (approve-eldoc-propertize "test" "@user" "doc" 'warning))
        (goto-char (point-min))
        (let ((face-used nil))
          (approve-eldoc--documentation-function
           (lambda (_doc &rest args)
             (setq face-used (plist-get args :face))))
          (expect face-used :to-equal 'warning))))

    (it "uses default face when no face specified"
      (with-temp-buffer
        (insert (approve-eldoc-propertize "test" "@user" "doc"))
        (goto-char (point-min))
        (let ((face-used nil))
          (approve-eldoc--documentation-function
           (lambda (_doc &rest args)
             (setq face-used (plist-get args :face))))
          (expect face-used :to-equal 'font-lock-variable-name-face))))))

(provide 'test-approve-eldoc)
;;; test-approve-eldoc.el ends here
