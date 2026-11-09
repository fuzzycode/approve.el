;;; test-approve-nav.el --- Tests for moving around a PR buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Jumps, the next unviewed file and unresolved thread, visiting,
;; opening and copying, in the medium PR.  Evil bindings are checked
;; without evil, through a stand-in for `evil-define-key*'.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-test-helpers "helpers")

(defmacro test-nav--with-medium (&rest body)
  "Run BODY in a freshly loaded medium PR buffer."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let ((buffer (approve-test-open-pr "medium")))
       (unwind-protect
           (with-current-buffer buffer
             (goto-char (point-min))
             ,@body)
         (kill-buffer buffer)))))

(defun test-nav--section-at-point ()
  "Return (TYPE . VALUE) of the section at point."
  (let ((section (magit-current-section)))
    (cons (oref section type) (oref section value))))

(defun test-nav--goto (text)
  "Move to the first TEXT in the buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(describe "Moving to the next work"
  (it "goes to the next and previous file not viewed, expanding it"
    (test-nav--with-medium
      (approve-next-unviewed-file)
      (expect (test-nav--section-at-point) :to-equal '(approve-file-section . "CHANGELOG.md"))
      (expect (oref (magit-current-section) hidden) :to-be nil)
      (approve-next-unviewed-file)
      (expect (test-nav--section-at-point) :to-equal '(approve-file-section . "README.md"))
      (approve-previous-unviewed-file)
      (expect (test-nav--section-at-point) :to-equal '(approve-file-section . "CHANGELOG.md"))
      (expect (approve-previous-unviewed-file) :to-throw 'user-error)))

  (it "skips viewed files"
    (test-nav--with-medium
      (approve-nav-goto-file "Makefile")
      (approve-next-unviewed-file)
      (expect (cdr (test-nav--section-at-point)) :to-equal "README.md")))

  (it "goes to the next and previous unresolved thread, in diff order"
    (test-nav--with-medium
      (approve-next-thread)
      (let ((first (magit-current-section)))
        (expect (oref first type) :to-be 'approve-thread-section)
        (expect (approve-model-get "PullRequestReviewThread" (oref first value) 'isResolved)
                :to-be nil)
        (approve-next-thread)
        (let ((second (magit-current-section)))
          (expect (oref second type) :to-be 'approve-thread-section)
          (expect (> (point) (oref first start)) :to-be t)
          (approve-previous-thread)
          (expect (oref (magit-current-section) value) :to-equal (oref first value))))))

  (it "skips resolved threads"
    (test-nav--with-medium
      (let ((seen nil))
        (condition-case nil
            (while t
              (approve-next-thread)
              (push (oref (magit-current-section) value) seen))
          (user-error nil))
        (expect (length seen) :to-equal 15)
        (dolist (id seen)
          (expect (approve-model-get "PullRequestReviewThread" id 'isResolved) :to-be nil))))))

(describe "Jumping"
  (it "goes to the sections, expanding collapsed ones"
    (test-nav--with-medium
      (approve-jump-commits)
      (expect (car (test-nav--section-at-point)) :to-be 'approve-commits-section)
      (expect (oref (magit-current-section) hidden) :to-be nil)
      (approve-jump-description)
      (expect (car (test-nav--section-at-point)) :to-be 'approve-description-section)
      (approve-jump-pending-review)
      (expect (car (test-nav--section-at-point)) :to-be 'approve-pending-review-section)))

  (it "goes to a file's diff by name"
    (test-nav--with-medium
      (approve-jump-file "pkg/api/types.go")
      (expect (test-nav--section-at-point) :to-equal '(approve-file-section . "pkg/api/types.go"))))

  (it "is a transient on j"
    (expect (keymap-lookup approve-pr-mode-map "j") :to-be 'approve-jump)
    (expect (get 'approve-jump 'transient--prefix) :to-be-truthy)))

(describe "approve-visit"
  (it "goes from a file in the list to its diff"
    (test-nav--with-medium
      (test-nav--goto "internal/parser/lexer.go")
      (approve-visit)
      (expect (test-nav--section-at-point)
              :to-equal '(approve-file-section . "internal/parser/lexer.go"))
      (expect (oref (magit-current-section) hidden) :to-be nil)))

  (it "goes from a pending comment to its thread"
    (test-nav--with-medium
      (test-nav--goto "Is this branch reachable")
      (approve-visit)
      (expect (test-nav--section-at-point)
              :to-equal '(approve-thread-section . "PRRT_kwDOVCEPcVKg5CpjSd"))))

  (it "goes from a review in the conversation to a thread it commented in"
    (test-nav--with-medium
      (test-nav--goto "Looks good to me.")
      (approve-visit)
      (let ((thread (magit-current-section)))
        (expect (oref thread type) :to-be 'approve-thread-section))))

  (it "opens links: pull requests in Approve, the rest in the browser"
    (test-nav--with-medium
      (spy-on 'approve-pr-open)
      (spy-on 'browse-url)
      (test-nav--goto "#12")
      (approve-visit)
      (expect 'browse-url :to-have-been-called-with
              "https://github.com/acme-example/rocket/issues/12")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize "link" 'approve-url "https://github.com/a/b/pull/3")))
      (backward-char 2)
      (approve-visit)
      (expect 'approve-pr-open :to-have-been-called-with
              '(:host "github.com" :owner "a" :repo "b" :number 3))))

  (it "toggles a section where there is nothing to follow"
    (test-nav--with-medium
      (approve-nav-goto-section 'approve-conversation-section)
      (approve-visit)
      (expect (oref (magit-current-section) hidden) :to-be t))))

(describe "approve-browse and approve-copy"
  (it "open the thing at point, else the pull request"
    (test-nav--with-medium
      (spy-on 'browse-url)
      (forward-line 2)
      (end-of-line)
      (approve-browse)
      (expect 'browse-url :to-have-been-called-with
              "https://github.com/acme-example/rocket/pull/202")
      (test-nav--goto "alice-example")
      (approve-browse)
      (expect 'browse-url :to-have-been-called-with "https://github.com/alice-example")))

  (it "copy a path:line, a sha, a login, or with a prefix the URL"
    (test-nav--with-medium
      (approve-nav-goto-file "internal/parser/parser.go")
      (search-forward "if err != nil {\n")
      (forward-line -1)
      (approve-copy)
      (expect (current-kill 0) :to-equal "internal/parser/parser.go:13")
      (approve-copy t)
      (expect (current-kill 0) :to-match "/pull/202/files#diff-[0-9a-f]+R13\\'")
      (test-nav--goto "dd4ac8a")
      (approve-copy)
      (expect (current-kill 0) :to-match "\\`dd4ac8a[0-9a-f]+\\'")
      (test-nav--goto "bob-example")
      (approve-copy)
      (expect (current-kill 0) :to-equal "bob-example"))))

(describe "approve-evil"
  (it "binds Approve's keys in normal state without shadowing motions"
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state map key def)
                   (push (list state map (key-description key) def) bound))))
        (load "approve-evil" nil t))
      (expect (seq-find (lambda (b) (equal (nth 2 b) "g r")) bound)
              :to-equal (list 'normal approve-pr-mode-map "g r" 'approve-pr-refresh))
      (dolist (motion '("h" "j" "k" "l" "w" "b" "e" "v" "V" "y" "n" "N" "/"))
        (expect (seq-find (lambda (b) (equal (nth 2 b) motion)) bound) :to-be nil)))))

;;; test-approve-nav.el ends here
