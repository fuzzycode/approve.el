;;; test-approve-input.el --- Tests for approve-input  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the approve-input module.

;;; Code:

(require 'buttercup)
(require 'approve-input)

(describe "approve-input-mode"

  (it "derives from gfm-mode"
    (expect (get 'approve-input-mode 'derived-mode-parent) :to-equal 'gfm-mode))

  (it "has C-c C-c bound to commit"
    (with-temp-buffer
      (approve-input-mode)
      (expect (key-binding (kbd "C-c C-c")) :to-equal #'approve-input-commit)))

  (it "has C-c C-k bound to abort"
    (with-temp-buffer
      (approve-input-mode)
      (expect (key-binding (kbd "C-c C-k")) :to-equal #'approve-input-abort))))

(describe "approve-input--get-content"

  (describe "with multiline enabled"

    (it "returns all content including newlines"
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--multiline t)
        (setq approve-input--content-start (point-min-marker))
        (insert "Line 1\nLine 2\nLine 3")
        (expect (approve-input--get-content) :to-equal "Line 1\nLine 2\nLine 3")))

    (it "preserves leading and trailing whitespace"
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--multiline t)
        (setq approve-input--content-start (point-min-marker))
        (insert "  spaced content  ")
        (expect (approve-input--get-content) :to-equal "  spaced content  "))))

  (describe "with multiline disabled"

    (it "returns only the first line"
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--multiline nil)
        (setq approve-input--content-start (point-min-marker))
        (insert "First line\nSecond line")
        (expect (approve-input--get-content) :to-equal "First line")))

    (it "trims whitespace from single line"
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--multiline nil)
        (setq approve-input--content-start (point-min-marker))
        (insert "  trimmed  ")
        (expect (approve-input--get-content) :to-equal "trimmed")))

    (it "handles empty input"
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--multiline nil)
        (setq approve-input--content-start (point-min-marker))
        (expect (approve-input--get-content) :to-equal "")))))

(describe "approve-input--create-buffer"

  (after-each
    (when-let ((buf (get-buffer "*Approve: Test*")))
      (kill-buffer buf)))

  (it "creates a buffer with the expected name"
    (let ((buffer (approve-input--create-buffer "Test" nil nil)))
      (expect (buffer-name buffer) :to-equal "*Approve: Test*")))

  (it "populates buffer with initial content"
    (let ((buffer (approve-input--create-buffer "Test" "Initial text" nil)))
      (with-current-buffer buffer
        (expect (buffer-string) :to-equal "Initial text"))))

  (it "sets multiline flag correctly"
    (let ((buffer (approve-input--create-buffer "Test" nil t)))
      (with-current-buffer buffer
        (expect approve-input--multiline :to-be-truthy))))

  (it "positions cursor at the beginning"
    (let ((buffer (approve-input--create-buffer "Test" "Some text" nil)))
      (with-current-buffer buffer
        (expect (point) :to-equal (point-min)))))

  (it "activates approve-input-mode"
    (let ((buffer (approve-input--create-buffer "Test" nil nil)))
      (with-current-buffer buffer
        (expect major-mode :to-equal 'approve-input-mode)))))

(describe "approve-input-read"

  (after-each
    (when-let ((buf (get-buffer "*Approve: Edit title*")))
      (kill-buffer buf)))

  (it "returns the created buffer"
    (cl-letf (((symbol-function 'approve-input--display-buffer) #'ignore))
      (let ((buffer (approve-input-read :prompt "Edit title")))
        (expect (bufferp buffer) :to-be-truthy)
        (expect (buffer-live-p buffer) :to-be-truthy))))

  (it "stores on-commit callback"
    (cl-letf (((symbol-function 'approve-input--display-buffer) #'ignore))
      (let* ((callback (lambda (text) text))
             (buffer (approve-input-read :prompt "Edit title" :on-commit callback)))
        (with-current-buffer buffer
          (expect approve-input--on-commit :to-equal callback)))))

  (it "stores on-abort callback"
    (cl-letf (((symbol-function 'approve-input--display-buffer) #'ignore))
      (let* ((callback (lambda () nil))
             (buffer (approve-input-read :prompt "Edit title" :on-abort callback)))
        (with-current-buffer buffer
          (expect approve-input--on-abort :to-equal callback)))))

  (it "stores source buffer"
    (cl-letf (((symbol-function 'approve-input--display-buffer) #'ignore))
      (let* ((source (current-buffer))
             (buffer (approve-input-read :prompt "Edit title")))
        (with-current-buffer buffer
          (expect approve-input--source-buffer :to-equal source))))))

(describe "approve-input-commit"

  (it "calls on-commit callback with content"
    (let ((received-text nil))
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--multiline nil)
        (setq approve-input--content-start (point-min-marker))
        (setq approve-input--on-commit (lambda (text) (setq received-text text)))
        (insert "Test content")
        ;; Mock cleanup to prevent window operations in test
        (cl-letf (((symbol-function 'approve-input--cleanup) #'ignore))
          (approve-input-commit)))
      (expect received-text :to-equal "Test content")))

  (it "handles nil on-commit callback"
    (with-temp-buffer
      (approve-input-mode)
      (setq approve-input--multiline nil)
      (setq approve-input--content-start (point-min-marker))
      (setq approve-input--on-commit nil)
      (cl-letf (((symbol-function 'approve-input--cleanup) #'ignore))
        (expect (approve-input-commit) :not :to-throw)))))

(describe "approve-input-abort"

  (it "calls on-abort callback"
    (let ((abort-called nil))
      (with-temp-buffer
        (approve-input-mode)
        (setq approve-input--on-abort (lambda () (setq abort-called t)))
        (cl-letf (((symbol-function 'approve-input--cleanup) #'ignore))
          (approve-input-abort)))
      (expect abort-called :to-be-truthy)))

  (it "handles nil on-abort callback"
    (with-temp-buffer
      (approve-input-mode)
      (setq approve-input--on-abort nil)
      (cl-letf (((symbol-function 'approve-input--cleanup) #'ignore))
        (expect (approve-input-abort) :not :to-throw)))))

(describe "approve-input--header-line"

  (it "includes the prompt"
    (with-temp-buffer
      (approve-input-mode)
      (setq approve-input--prompt "Edit description")
      (expect (approve-input--header-line) :to-match "Edit description")))

  (it "includes commit keybinding hint"
    (with-temp-buffer
      (approve-input-mode)
      (setq approve-input--prompt "Test")
      (expect (approve-input--header-line) :to-match "C-c C-c")))

  (it "includes abort keybinding hint"
    (with-temp-buffer
      (approve-input-mode)
      (setq approve-input--prompt "Test")
      (expect (approve-input--header-line) :to-match "C-c C-k")))

  (it "uses default prompt when none set"
    (with-temp-buffer
      (approve-input-mode)
      (setq approve-input--prompt nil)
      (expect (approve-input--header-line) :to-match "Input"))))

;;; test-approve-input.el ends here
