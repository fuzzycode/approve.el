;;; test-approve-context.el --- Tests for the context at point -*- lexical-binding: t; -*-

;;; Commentary:

;; Point on each kind of thing in the rendered medium PR, and the
;; context plist that comes back.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-context)
(require 'approve-test-helpers "helpers")

(defvar test-context--buffer nil
  "The loaded medium PR buffer, with parser.go expanded.")

(defun test-context--at (text &optional offset)
  "Return the context at the first TEXT in the buffer, OFFSET chars in."
  (with-current-buffer test-context--buffer
    (deactivate-mark)
    (goto-char (point-min))
    (unless (search-forward text nil t)
      (error "%S not in the buffer" text))
    (goto-char (+ (match-beginning 0) (or offset 0)))
    (approve-context-at-point)))

(describe "approve-context-at-point"
  (before-all
    (approve-test-with-fixture "medium"
      (setq test-context--buffer (approve-test-open-pr "medium"))
      (with-current-buffer test-context--buffer
        (magit-section-show (approve-test-find-section 'approve-file-section
                                                       "internal/parser/parser.go")))))
  (after-all (kill-buffer test-context--buffer))

  (it "is a user on a login, with the profile URL"
    (let ((context (test-context--at "bob-example")))
      (expect (plist-get context :kind) :to-be 'user)
      (expect (plist-get context :value) :to-equal "bob-example")
      (expect (plist-get context :url) :to-match "bob-example")))

  (it "is a label, a commit, an issue reference or a branch on those"
    (expect (plist-get (test-context--at "enhancement") :kind) :to-be 'label)
    (expect (plist-get (test-context--at "dd4ac8a") :kind) :to-be 'commit)
    (expect (plist-get (test-context--at "#12") :kind) :to-be 'ref)
    (expect (plist-get (test-context--at "parser-rewrite") :kind) :to-be 'branch))

  (it "is the description in the description"
    (let ((context (test-context--at "Rewrites the parser for speed")))
      (expect (plist-get context :kind) :to-be 'description)
      (expect (plist-get context :can-update) :to-be-truthy)))

  (it "is a file on a file entry and on a diff file heading"
    (let ((context (test-context--at "internal/parser/lexer.go")))
      (expect context :to-equal
              (list :kind 'file :path "internal/parser/lexer.go" :viewed "UNVIEWED"
                    :url (with-current-buffer test-context--buffer
                           (approve-context-file-url "internal/parser/lexer.go"))))))

  (it "is a diff line on a line, with the GitHub anchor of the line"
    (let ((context (test-context--at "if err != nil {\n" 2)))
      (expect (plist-get context :kind) :to-be 'line)
      (expect (plist-get context :path) :to-equal "internal/parser/parser.go")
      (expect (plist-get context :side) :to-equal "RIGHT")
      (expect (plist-get context :line) :to-equal 13)
      (expect (plist-get context :line-kind) :to-be 'add)
      (expect (plist-get context :url) :to-match "/files#diff-[0-9a-f]+R13\\'"))
    (let ((context (test-context--at "return nil, err\n" 2)))
      (expect (plist-get context :side) :to-equal "LEFT")
      (expect (plist-get context :line) :to-equal 14)))

  (it "is a range of lines over a region of diff lines"
    (with-current-buffer test-context--buffer
      (goto-char (point-min))
      (search-forward "if err := oldStep(ctx, 13)")
      (let ((transient-mark-mode t))
        (push-mark (point) t t)
        (search-forward "if err != nil {")
        (let ((context (approve-context-at-point)))
          (deactivate-mark)
          (expect (plist-get context :kind) :to-be 'lines)
          (expect (plist-get context :start-side) :to-equal "LEFT")
          (expect (plist-get context :start-line) :to-equal 13)
          (expect (plist-get context :side) :to-equal "RIGHT")
          (expect (plist-get context :line) :to-equal 13)))))

  (it "is a thread on a thread's heading, with what the viewer may do"
    (let ((context (test-context--at "╭─ " 0)))
      (expect (plist-get context :kind) :to-be 'thread)
      (expect (plist-get context :thread-id) :to-equal "PRRT_kwDOmVrZbzhFWoiCSv")
      (expect (plist-get context :resolved) :to-be nil)
      (expect (plist-get context :url) :to-match "\\`https://")))

  (it "is a review comment in a thread's comment, knowing its thread"
    (let ((context (test-context--at "Done, it is reused now.")))
      (expect (plist-get context :kind) :to-be 'review-comment)
      (expect (plist-get context :thread-id) :to-equal "PRRT_kwDO85-qIzFQYiTYBM")
      (expect (plist-get context :comment-id) :to-match "\\`PRRC_")))

  (it "is a suggestion in a suggested change, knowing its comment and thread"
    (let ((context (test-context--at "│ +\tresult = append" 2)))
      (expect (plist-get context :kind) :to-be 'suggestion)
      (expect (plist-get context :index) :to-equal 0)
      (expect (plist-get context :comment-id) :to-match "\\`PRRC_")
      (expect (plist-get context :thread-id) :to-match "\\`PRRT_"))
    (expect (plist-get (test-context--at "These three lines can be one call") :kind)
            :to-be 'review-comment))

  (it "is a conversation comment or a review in the conversation"
    (expect (plist-get (test-context--at "Thanks! I'll take a look today.") :kind)
            :to-be 'issue-comment)
    (let ((context (test-context--at "Looks good to me.")))
      (expect (plist-get context :kind) :to-be 'review)
      (expect (plist-get context :state) :to-equal "APPROVED")))

  (it "is the pull request anywhere else"
    (with-current-buffer test-context--buffer
      (goto-char (point-min))
      (search-forward "Status ")
      (forward-line 1)
      (expect (plist-get (approve-context-at-point) :kind) :to-be 'pr)))

  (it "says the threads drawn under a line, where they are overlays"
    (with-temp-buffer
      (insert "one\ntwo\n")
      (setq approve-model-store-buffer test-context--buffer
            approve-context-file '(:path "internal/parser/parser.go"
                                         :name "internal/parser/parser.go"
                                         :side "RIGHT" :oid "6ffac57")
            approve-context-threads-function (lambda (pos)
                                               (and (= (line-number-at-pos pos) 2)
                                                    '("PRRT_kwDO85-qIzFQYiTYBM"))))
      (goto-char (point-min))
      (expect (plist-member (approve-context-at-point) :threads) :to-equal '(:threads nil))
      (forward-line 1)
      (expect (plist-get (approve-context-at-point) :threads)
              :to-equal '("PRRT_kwDO85-qIzFQYiTYBM"))
      (expect (mapcar (lambda (context)
                        (list (plist-get context :kind) (plist-get context :label)))
                      (approve-context-of-threads '("PRRT_kwDO85-qIzFQYiTYBM")))
              :to-equal
              '((thread "@carol-example: This allocates on every call; hoist the buffer out of the l…")
                (review-comment
                 "@carol-example: This allocates on every call; hoist the buffer out of the l…")
                (review-comment "@alice-example: Done, it is reused now.")
                (review-comment "@carol-example: Thanks!"))))))

;;; test-approve-context.el ends here
