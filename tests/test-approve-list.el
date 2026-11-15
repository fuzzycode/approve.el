;;; test-approve-list.el --- Tests for approve-list -*- lexical-binding: t; -*-

;;; Commentary:

;; The rows of the lists of pull requests: their columns, the marks of
;; state, checks and review decision, widths that fit
;; `approve-page-width', `approve-list-columns', and the pull request
;; as the context anywhere on its row.

;;; Code:

(require 'buttercup)
(require 'approve-list)
(require 'approve-context)
(require 'approve-test-helpers "helpers")

(defun test-list--pr (&rest fields)
  "Return a pull request in the shape of a search result, with FIELDS.
FIELDS is a plist of field names and values that replace the
defaults: #1234 in fuzzycode/approve by @alice, open, updated two
hours before `approve-test-now', checks running, changes requested."
  (let ((pr (copy-tree '((__typename . "PullRequest")
                         (id . "PR_1234")
                         (number . 1234)
                         (url . "https://github.com/fuzzycode/approve/pull/1234")
                         (title . "Add retry logic to the GraphQL client")
                         (state . "OPEN")
                         (isDraft)
                         (createdAt . "2026-10-01T12:00:00Z")
                         (updatedAt . "2026-10-04T10:00:00Z")
                         (author (__typename . "User") (id . "U_1") (login . "alice")
                                 (url . "https://github.com/alice") (name . "Alice"))
                         (repository (__typename . "Repository") (id . "R_1")
                                     (nameWithOwner . "fuzzycode/approve")
                                     (url . "https://github.com/fuzzycode/approve"))
                         (baseRefName . "main")
                         (headRefName . "feature/retry")
                         (additions . 340)
                         (deletions . 56)
                         (changedFiles . 12)
                         (reviewDecision . "CHANGES_REQUESTED")
                         (statusCheckRollup (__typename . "StatusCheckRollup") (id . "SCR_1")
                                            (state . "PENDING"))))))
    (cl-loop for (field value) on fields by #'cddr
             do (setf (alist-get field pr) value))
    pr))

(defmacro test-list--with-buffer (&rest body)
  "Run BODY in a temporary section buffer that copies like Approve's."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (magit-section-mode)
     (setq-local filter-buffer-substring-function
                 (lambda (beg end delete)
                   (approve-render-strip-decorations
                    (buffer-substring--filter beg end delete))))
     (let ((inhibit-read-only t)
           (approve-render-now approve-test-now))
       ,@body)))

(defun test-list--insert (prs &optional empty)
  "Insert the rows of PRS in a root section; EMPTY as for `approve-list-insert'."
  (magit-insert-section (approve-test-root)
    (approve-list-insert prs empty)))

(defun test-list--lines ()
  "Return the lines of the buffer, without properties or the last newline."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))

(describe "approve-list"
  :var (saved-icons)
  (before-all
    (setq saved-icons approve-use-nerd-icons
          approve-use-nerd-icons nil)
    (approve-icon-clear-cache))
  (after-all
    (setq approve-use-nerd-icons saved-icons)
    (approve-icon-clear-cache))

  (it "draws a pull request as a row: state, number, title, repository, author, update, checks, review"
    (test-list--with-buffer
      (test-list--insert (list (test-list--pr)))
      (let ((line (car (test-list--lines))))
        (expect line :to-match
                (concat "\\`  ● #1234   Add retry logic to the GraphQL client +"
                        "fuzzycode/approve +@alice +2h  ◐  ✗\\'")))))

  (it "ends each row at approve-page-width, the title taking what is left"
    (test-list--with-buffer
      (let ((approve-page-width 90))
        (test-list--insert (list (test-list--pr)
                                 (test-list--pr 'number 7 'title (make-string 80 ?x)
                                                'url "https://github.com/a/b/pull/7"))))
      (dolist (line (test-list--lines))
        (expect (string-width line) :to-be 90))
      (let ((title (string-trim (substring (cadr (test-list--lines)) 12 38))))
        (expect title :to-equal (concat (make-string 25 ?x) "…")))))

  (it "marks the state: open, draft, merged, closed; a draft's title is dim"
    (test-list--with-buffer
      (test-list--insert (list (test-list--pr 'number 1 'url "u1")
                               (test-list--pr 'number 2 'url "u2" 'isDraft t)
                               (test-list--pr 'number 3 'url "u3" 'state "MERGED")
                               (test-list--pr 'number 4 'url "u4" 'state "CLOSED")))
      (expect (mapcar (lambda (line) (substring line 2 3)) (test-list--lines))
              :to-equal '("●" "◌" "⮌" "⊘"))
      (goto-char (point-min))
      (search-forward "Add retry")
      (expect (get-text-property (match-beginning 0) 'face) :to-be nil)
      (search-forward "Add retry")
      (expect (get-text-property (match-beginning 0) 'face) :to-equal 'approve-draft-title)))

  (it "marks the checks and GitHub's review decision"
    (test-list--with-buffer
      (test-list--insert
       (cl-loop for (checks review) in '(("SUCCESS" "APPROVED") ("FAILURE" "CHANGES_REQUESTED")
                                         ("PENDING" "REVIEW_REQUIRED") (nil nil))
                for number from 1
                collect (test-list--pr 'number number 'url (format "u%d" number)
                                       'statusCheckRollup (and checks `((state . ,checks)))
                                       'reviewDecision review)))
      (expect (mapcar (lambda (line) (substring line -4)) (test-list--lines))
              :to-equal '("✓  ✓" "✗  ✗" "◐  ○" "·  ·"))))

  (it "writes the author as @login, a deleted one as @ghost, and how long ago it changed"
    (test-list--with-buffer
      (test-list--insert (list (test-list--pr 'author nil 'updatedAt "2026-09-01T12:00:00Z")))
      (expect (car (test-list--lines)) :to-match "@ghost +1mo  ")))

  (it "follows approve-list-columns: their order, widths, and functions"
    (test-list--with-buffer
      (let ((approve-list-columns
             `((number 5) (author 8) (,(lambda (pr) (alist-get 'headRefName pr)) 6)
               (title 12))))
        (test-list--insert (list (test-list--pr))))
      (expect (car (test-list--lines))
              :to-equal "  #1234  @alice    featu…  Add retry l…")))

  (it "makes each row a section of the pull request, its context anywhere on the row"
    (test-list--with-buffer
      (test-list--insert (list (test-list--pr)))
      (let ((section (approve-test-find-section 'approve-list-pr-section)))
        (expect (oref section value)
                :to-equal "https://github.com/fuzzycode/approve/pull/1234"))
      (dolist (pos (list (point-min)
                         (progn (goto-char (point-min)) (search-forward "@alice") (point))
                         (progn (goto-char (point-min)) (line-end-position))))
        (let ((context (approve-context-at-point pos)))
          (expect (plist-get context :kind) :to-be 'pull-request)
          (expect (plist-get context :value) :to-equal "fuzzycode/approve#1234")
          (expect (plist-get context :url)
                  :to-equal "https://github.com/fuzzycode/approve/pull/1234")))))

  (it "copies a row without its indentation"
    (test-list--with-buffer
      (test-list--insert (list (test-list--pr)))
      (expect (filter-buffer-substring (point-min) (point-max)) :to-match "\\`●")))

  (it "says when the list is empty"
    (test-list--with-buffer
      (test-list--insert nil)
      (test-list--insert nil "Nobody asked for your review.")
      (expect (test-list--lines)
              :to-equal '("  No pull requests." "  Nobody asked for your review.")))))

;;; test-approve-list.el ends here
