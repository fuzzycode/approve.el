;;; test-approve-pr.el --- Tests for the pull request buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Opening a PR (F01), loading it (F02), redrawing it with point and
;; collapsed sections kept (D13), refreshing it (F17), error states,
;; lazily loaded checks (F16) and the header line.  Everything goes
;; through the fake API with the fixtures.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-test-helpers "helpers")

(defmacro test-pr--with-medium (&rest body)
  "Run BODY in the loaded medium PR buffer, killed afterwards."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let ((buffer (approve-test-open-pr "medium")))
       (unwind-protect
           (with-current-buffer buffer ,@body)
         (kill-buffer buffer)))))

(defun test-pr--goto-text (text)
  "Move point to the start of the first TEXT in the buffer."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (error "%S is not in the buffer" text))
  (goto-char (match-beginning 0)))

(defun test-pr--shown-but-collapsed ()
  "Return (TYPE VALUE) of each collapsed section whose body still shows."
  (let (shown)
    (magit-map-sections
     (lambda (section)
       (when (and (oref section hidden)
                  (oref section content)
                  (< (oref section content) (oref section end))
                  (not (invisible-p (oref section content))))
         (push (list (oref section type) (oref section value)) shown))))
    (nreverse shown)))

(describe "approve-pr-open"
  (it "names the buffer after the PR and puts it in approve-pr-mode"
    (approve-test-with-fixture "small"
      (let ((buffer (approve-test-open-pr "small")))
        (unwind-protect
            (progn
              (expect (buffer-name buffer) :to-equal "*approve: acme-example/greeter#7*")
              (expect (buffer-local-value 'major-mode buffer) :to-be 'approve-pr-mode))
          (kill-buffer buffer)))))

  (it "shows an open buffer of the PR as it is"
    (approve-test-with-fixture "small"
      (let ((buffer (approve-test-open-pr "small")))
        (unwind-protect
            (progn
              (spy-on 'approve-pr-load)
              (expect (approve-pr-open (approve-test-pr-plist "small")) :to-be buffer)
              (expect 'approve-pr-load :not :to-have-been-called))
          (kill-buffer buffer)))))

  (it "is what `approve' opens, and `approve' refuses other URLs"
    (spy-on 'approve-pr-open)
    (approve "https://github.com/acme-example/greeter/pull/7/files")
    (expect 'approve-pr-open :to-have-been-called-with
            '(:host "github.com" :owner "acme-example" :repo "greeter" :number 7))
    (expect (approve "https://github.com/acme-example/greeter/issues/7")
            :to-throw 'user-error))

  (it "remembers the PRs opened in the session, newest first, each once"
    (let ((approve-pr--recent nil)
          (magit '(:host "github.com" :owner "magit" :repo "magit" :number 5000)))
      (approve-test-with-fixture "small"
        (let ((buffer (approve-test-open-pr "small")))
          (unwind-protect
              (progn
                (approve-pr-remember magit)
                ;; Showing an open buffer counts as opening it.
                (approve-pr-open (approve-test-pr-plist "small"))
                (expect (approve-pr-recent)
                        :to-equal (list (approve-test-pr-plist "small") magit))
                (approve-pr-remember '(:host "github.com" :owner "Magit" :repo "MAGIT"
                                       :number 5000))
                (expect (length (approve-pr-recent)) :to-be 2)
                (expect (plist-get (car (approve-pr-recent)) :owner) :to-equal "Magit"))
            (kill-buffer buffer))))))

  (it "remembers the last 50 PRs"
    (let ((approve-pr--recent nil))
      (dotimes (number 60)
        (approve-pr-remember (list :host "github.com" :owner "o" :repo "r" :number number)))
      (expect (length (approve-pr-recent)) :to-be 50)
      (expect (plist-get (car (approve-pr-recent)) :number) :to-be 59))))

(describe "Loading a PR"
  (it "draws a skeleton that says what is loading"
    (approve-test-with-fixture "medium"
      (let ((buffer (approve-test-open-pr "medium" t)))
        (unwind-protect
            (with-current-buffer buffer
              (expect (buffer-string) :to-match "^acme-example/rocket#202")
              (expect (buffer-string) :to-match "Loading…")
              (expect (approve-pr-loading-p 'core) :to-be-truthy)
              (expect approve-pr--header-line :to-match "#202 · loading pull request, files")
              (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
              (approve-test-wait-for (lambda () (null approve-pr--redraw-timer)))
              (expect (buffer-string) :not :to-match "Loading…\nCommits"))
          (kill-buffer buffer)))))

  (it "fills every section from the store"
    (test-pr--with-medium
      (expect (approve-session-get :errors) :to-be nil)
      (expect (approve-model-meta :viewer) :to-equal "vera-example")
      (dolist (type '(approve-header-section approve-pending-review-section
                      approve-description-section approve-checks-section
                      approve-commits-section approve-conversation-section
                      approve-diff-section))
        (expect (approve-test-find-section type) :to-be-truthy))
      (expect (approve-test-section-text (approve-test-find-section 'approve-header-section))
              :to-match "\\`Author      @alice-example")
      (expect (length (oref (approve-test-find-section 'approve-diff-section) children))
              :to-equal 30)))

  (it "shows the file list only when the sections hook has it (D05, amended)"
    (test-pr--with-medium
      (expect (approve-test-find-section 'approve-files-section) :to-be nil)
      (approve-test-show-file-list)
      (expect (length (oref (approve-test-find-section 'approve-files-section) children))
              :to-equal 30)
      (expect (oref (approve-test-find-section 'approve-files-section) end)
              :to-equal (oref (approve-test-find-section 'approve-diff-section) start))))

  (it "puts the number, title and state in the header line"
    (test-pr--with-medium
      (let ((line (substring-no-properties approve-pr--header-line)))
        (expect line :to-match "\\` #202 Rewrite the parser and engine scheduler +.? ?Open\\'")
        (expect (approve-test-section-text (approve-test-find-section 'approve-header-section))
                :not :to-match "Rewrite the parser"))))

  (it "cuts a long title so the state still shows"
    (test-pr--with-medium
      (let ((approve-page-width 40))
        (approve-pr-update-header-line)
        (let ((line (substring-no-properties approve-pr--header-line)))
          (expect line :to-match "… +.? ?Open\\'")
          (expect (string-width (car (split-string line "…"))) :to-be-less-than 40)))))

  (it "shows a query's error in its section only"
    (approve-test-with-fixture "medium"
      (let* ((approve-api-transport (approve-test-fail-queries
                                     approve-api-transport
                                     '("queries/pull-request-commits.graphql")))
             (buffer (approve-test-open-pr "medium")))
        (unwind-protect
            (with-current-buffer buffer
              (expect (approve-pr-error 'commits) :to-be-truthy)
              (expect (approve-test-section-text (approve-test-find-section
                                                  'approve-commits-section))
                      :to-match "Could not load the commits: Bad Gateway · g to retry")
              (expect (approve-test-section-text (approve-test-find-section
                                                  'approve-description-section))
                      :to-match "Rewrites the parser"))
          (kill-buffer buffer)))))

  (it "says so in the whole buffer when the PR is not found"
    (approve-test-with-fixture "medium"
      (let ((buffer (approve-pr-setup-buffer
                     '(:host "github.com" :owner "acme-example" :repo "nope" :number 1))))
        (unwind-protect
            (with-current-buffer buffer
              (approve-pr-load)
              (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
              (approve-pr-redraw)
              (expect (buffer-string)
                      :to-match "Pull request not found, or the token has no access to it."))
          (kill-buffer buffer)))))

  (it "says once, for the whole buffer, that the pull request could not load"
    (approve-test-with-fixture "medium"
      (spy-on 'message)
      (let* ((approve-api-transport (approve-test-fail-queries
                                     approve-api-transport
                                     '("queries/pull-request.graphql")))
             (buffer (approve-test-open-pr "medium")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((text (buffer-string)))
                (expect text :to-match "\\`acme-example/rocket#202\n\n")
                (expect text :to-match "Could not load the pull request\\.\n  Bad Gateway\n")
                (expect text :to-match "g to retry")
                ;; The sections that loaded are not drawn.
                (expect text :not :to-match "^Diff")
                (expect (approve-test-find-section 'approve-diff-section) :to-be nil))
              (expect (substring-no-properties approve-pr--header-line) :to-equal " #202")
              (expect 'message :to-have-been-called-with
                      "Approve: could not load %s: %s" "acme-example/rocket#202" "Bad Gateway"))
          (kill-buffer buffer)))))

  (it "makes a token error of any query the error of the whole buffer"
    (approve-test-with-fixture "medium"
      (spy-on 'message)
      (let* ((fixtures approve-api-transport)
             (approve-api-transport
              (lambda (request)
                (if (equal (plist-get request :name) "queries/pull-request-files.graphql")
                    (error "Required Github token (\"vera^approve\" for either \"api.github.com\" or \"github.com\") does not exist.\nSee https://docs.magit.vc/ghub/Getting-Started.html")
                  (funcall fixtures request))))
             (buffer (approve-test-open-pr "medium")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((text (buffer-string)))
                (expect text :to-match "Could not use the GitHub token\\.\n  Required Github token")
                (expect text :to-match "Approve asks ghub for the token of `approve'")
                ;; Not the description, which loaded.
                (expect text :not :to-match "Rewrites the parser"))
              (expect 'message :to-have-been-called-with
                      "Approve: could not load %s: %s" "acme-example/rocket#202"
                      "Required Github token (\"vera^approve\" for either \"api.github.com\" or \"github.com\") does not exist."))
          (kill-buffer buffer)))))

  (it "lets a token error replace another error of the whole buffer"
    (with-temp-buffer
      (approve-model-init '(:host "github.com" :owner "o" :repo "r" :number 1))
      (spy-on 'message)
      (approve-pr--note-fatal 'files '(approve-api-timeout "No answer"))
      (expect (approve-session-get :fatal) :to-be nil)
      (approve-pr--note-fatal 'core '(approve-api-timeout "No answer"))
      (expect (approve-session-get :fatal) :to-equal '(approve-api-timeout "No answer"))
      (approve-pr--note-fatal 'files '(approve-api-unauthorized "No token"))
      (expect (approve-session-get :fatal) :to-equal '(approve-api-unauthorized "No token"))
      (approve-pr--note-fatal 'core '(approve-api-timeout "Later"))
      (expect (approve-session-get :fatal) :to-equal '(approve-api-unauthorized "No token"))
      (expect 'message :to-have-been-called-times 2))))

(describe "Redrawing"
  (it "coalesces redraws scheduled close together"
    (test-pr--with-medium
      (spy-on 'approve-pr-redraw :and-call-through)
      (approve-pr-schedule-redraw)
      (approve-pr-schedule-redraw)
      (approve-pr-schedule-redraw)
      (approve-test-wait-for (lambda () (null approve-pr--redraw-timer)))
      (expect 'approve-pr-redraw :to-have-been-called-times 1)))

  (it "keeps point on the same section, line and column"
    (test-pr--with-medium
      (approve-test-show-file-list)
      (test-pr--goto-text "internal/parser/lexer.go")
      (forward-char 5)
      (let ((column (current-column)))
        (approve-pr-redraw)
        (expect (magit-section-ident (magit-current-section))
                :to-equal (magit-section-ident
                           (approve-test-find-section 'approve-file-entry-section
                                                      "internal/parser/lexer.go")))
        (expect (current-column) :to-equal column))))

  (it "moves point to the same line when its section is gone"
    (test-pr--with-medium
      (test-pr--goto-text "Fix lexer edge cases")
      (let ((line (line-number-at-pos))
            (approve-pr-sections-hook (remove 'approve-insert-commits-section
                                              approve-pr-sections-hook)))
        (approve-pr-redraw)
        (expect (line-number-at-pos) :to-equal line))))

  (it "moves point to the nearest section left when its own is gone"
    (test-pr--with-medium
      (magit-section-show (approve-test-find-section 'approve-checks-section))
      (approve-pr--run-lazy-loaders)
      (approve-test-wait-for (lambda () (approve-pr-loaded-p 'checks)))
      (approve-pr-redraw)
      (test-pr--goto-text "CI / test")
      (expect (oref (magit-current-section) type) :to-be 'approve-check-section)
      (cl-letf (((symbol-function 'approve-sections--insert-checks) #'ignore))
        (approve-pr-redraw))
      (expect (oref (magit-current-section) type) :to-be 'approve-checks-section)))

  (it "keeps collapsed sections collapsed and expanded ones expanded"
    (test-pr--with-medium
      (magit-section-hide (approve-test-find-section 'approve-description-section))
      (magit-section-show (approve-test-find-section 'approve-commits-section))
      (approve-pr-redraw)
      (expect (oref (approve-test-find-section 'approve-description-section) hidden)
              :to-be t)
      (expect (oref (approve-test-find-section 'approve-commits-section) hidden)
              :to-be nil)))

  (it "hides the body of every collapsed section, when opened and after a redraw"
    (test-pr--with-medium
      (expect (test-pr--shown-but-collapsed) :to-be nil)
      (magit-section-hide (approve-test-find-section 'approve-description-section))
      (approve-pr-redraw)
      (expect (test-pr--shown-but-collapsed) :to-be nil)
      (expect (invisible-p (oref (approve-test-find-section 'approve-description-section)
                                 content))
              :to-be-truthy)))

  (it "detaches the markers of the sections it erases"
    (test-pr--with-medium
      (let ((old (approve-test-find-section 'approve-diff-section)))
        (approve-pr-redraw)
        (expect (marker-buffer (oref old start)) :to-be nil)
        (expect (marker-buffer (oref old end)) :to-be nil)
        (expect (marker-buffer (oref (approve-test-find-section 'approve-diff-section) start))
                :to-be (current-buffer)))))

  (it "starts with checks and commits collapsed and the rest expanded"
    (test-pr--with-medium
      (expect (oref (approve-test-find-section 'approve-checks-section) hidden) :to-be t)
      (expect (oref (approve-test-find-section 'approve-commits-section) hidden) :to-be t)
      (expect (oref (approve-test-find-section 'approve-description-section) hidden)
              :to-be nil)
      (expect (oref (approve-test-find-section 'approve-diff-section) hidden) :to-be nil))))

(describe "Refreshing"
  (it "loads again, keeping the text until the data arrives"
    (test-pr--with-medium
      (let ((before (buffer-string)))
        (approve-pr-refresh)
        (expect (buffer-string) :to-equal before)
        (expect (approve-pr-loading-p) :to-be-truthy)
        (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
        (approve-pr-redraw)
        (expect (substring-no-properties (buffer-string))
                :to-equal (substring-no-properties before)))))

  (it "replaces paged lists whole"
    (approve-test-with-fixture "large"
      (let ((buffer (approve-test-open-pr "large")))
        (unwind-protect
            (with-current-buffer buffer
              (expect (length (approve-pr-files)) :to-equal 300)
              (approve-pr-refresh)
              (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
              (expect (length (approve-pr-files)) :to-equal 300)
              (expect (hash-table-count (approve-model-extra :patches)) :to-equal 300))
          (kill-buffer buffer))))))

(describe "The checks"
  (it "are counted in the heading before they load"
    (test-pr--with-medium
      (expect (approve-pr-loaded-p 'checks) :to-be nil)
      (expect (car (split-string (approve-test-section-text
                                  (approve-test-find-section 'approve-checks-section))
                                 "\n"))
              :to-match "\\`Checks (6) .*2 passed · 1 failed · 3 running\\'")))

  (it "load when the Checks section is expanded, and show failing ones first"
    (test-pr--with-medium
      (expect (approve-pr-loaded-p 'checks) :to-be nil)
      (approve-pr--run-lazy-loaders)
      (expect (approve-pr-loading-p 'checks) :to-be nil)
      (magit-section-show (approve-test-find-section 'approve-checks-section))
      (approve-pr--run-lazy-loaders)
      (expect (approve-pr-loading-p 'checks) :to-be-truthy)
      (approve-test-wait-for (lambda () (approve-pr-loaded-p 'checks)))
      (approve-pr-redraw)
      (let* ((section (approve-test-find-section 'approve-checks-section))
             (lines (cdr (split-string (approve-test-section-text section) "\n" t))))
        (expect (oref section hidden) :to-be nil)
        (expect (car lines) :to-match "CI / test")
        (expect (car lines) :to-match "required")
        (expect (car lines) :to-match "failure")))))

(describe "Copying"
  (it "leaves out text marked as decoration"
    (with-temp-buffer
      (approve-pr-mode)
      (let ((inhibit-read-only t))
        (insert (approve-render-decoration "12 ") "code" (approve-render-decoration " x")))
      (expect (filter-buffer-substring (point-min) (point-max)) :to-equal "code"))))

;;; test-approve-pr.el ends here
