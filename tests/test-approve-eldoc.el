;;; test-approve-eldoc.el --- Tests for extended information -*- lexical-binding: t; -*-

;;; Commentary:

;; Echo lines and eldoc buffer text for each kind of thing in the
;; medium PR, and the lazy details: fetched only when wanted, cached,
;; errors remembered for a minute.  Detail queries are faked.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-test-helpers "helpers")

(defvar test-eldoc--buffer nil
  "The loaded medium PR buffer, with parser.go and the checks expanded.")

(defun test-eldoc--describe (text &optional offset fetch)
  "Return the description of the thing at the first TEXT, OFFSET chars in.
FETCH as for `approve-eldoc-describe'."
  (with-current-buffer test-eldoc--buffer
    (goto-char (point-min))
    (unless (search-forward text nil t)
      (error "%S not in the buffer" text))
    (goto-char (+ (match-beginning 0) (or offset 0)))
    (approve-eldoc-describe (approve-context-at-point) fetch)))

(defun test-eldoc--echo (text &optional offset)
  "Return the echo line of the thing at TEXT, without properties."
  (substring-no-properties (plist-get (test-eldoc--describe text offset) :echo)))

(describe "approve-eldoc"
  (before-all
    (setq approve-use-nerd-icons nil)
    (approve-icon-clear-cache)
    (approve-test-with-fixture "medium"
      (setq test-eldoc--buffer (approve-test-open-pr "medium"))
      (with-current-buffer test-eldoc--buffer
        (magit-section-show (approve-test-find-section 'approve-checks-section))
        (approve-pr--run-lazy-loaders)
        (approve-test-wait-for (lambda () (approve-pr-loaded-p 'checks)))
        (approve-pr-redraw)
        (magit-section-show (approve-test-find-section 'approve-file-section
                                                       "internal/parser/parser.go"))
        (magit-section-show (approve-test-find-section 'approve-file-threads-section
                                                       'outdated)))))
  (after-all
    (kill-buffer test-eldoc--buffer)
    (setq approve-use-nerd-icons t
          approve-render-now nil)
    (approve-icon-clear-cache))
  (before-each
    (setq approve-render-now approve-test-now)
    (approve-eldoc-clear-cache))

  (describe "echo lines"
    (it "name a person from the store"
      (expect (test-eldoc--echo "bob-example") :to-equal "@bob-example — Bob Example"))

    (it "describe a label, a commit, an issue and a milestone"
      (expect (test-eldoc--echo "enhancement")
              :to-equal " enhancement  — New feature or request")
      (expect (test-eldoc--echo "dd4ac8a")
              :to-match "\\`dd4ac8a — Add the new parser — @alice-example · 2026-10-01 ")
      (expect (test-eldoc--echo "#12")
              :to-equal "#12 — The parser is slow on large inputs — open issue")
      (expect (test-eldoc--echo "v1.2") :to-match "\\`v1.2 — due "))

    (it "describe a check, a file and a branch"
      (expect (test-eldoc--echo "CI / test") :to-match "\\`CI / test — failure — “.*” · required")
      (expect (test-eldoc--echo "internal/parser/lexer.go")
              :to-equal "internal/parser/lexer.go — modified · +10 −5 — not viewed · 2 threads, 1 unresolved")
      (expect (test-eldoc--echo "parser-rewrite")
              :to-equal "acme-example:parser-rewrite — in this repository"))

    (it "describe a check with no title in its state's words, with its duration"
      (let* ((check '((__typename . "CheckRun") (name . "deploy") (status . "COMPLETED")
                      (conclusion . "TIMED_OUT") (startedAt . "2026-10-01T10:00:00Z")
                      (completedAt . "2026-10-01T10:06:00Z")
                      (checkSuite (workflowRun (workflow (name . "CI"))))))
             (description (approve-eldoc-describe-check (list :thing (list :check check)) nil)))
        (expect (substring-no-properties (plist-get description :echo))
                :to-equal "CI / deploy — timed out")
        (expect (plist-get description :doc) :to-match "^Duration +6m 00s$")))

    (it "describe a thread, saying when K shows its original code"
      (with-current-buffer test-eldoc--buffer
        (goto-char (point-min))
        (search-forward "· outdated ─")
        (goto-char (line-beginning-position))
        (let ((echo (substring-no-properties
                     (plist-get (approve-eldoc-describe (approve-context-at-point)) :echo))))
          (expect echo :to-equal
                  "thread — outdated (was line 61) — 2 comments · K shows the original code"))))

    (it "count reactions, saying when the viewer is among them"
      (expect (test-eldoc--echo "👍 3") :to-match "\\`👍 3 — "))

    (it "give a date in full"
      (expect (test-eldoc--echo "3 days ago") :to-match "\\`3 days ago — 2026-10-01 ")))

  (describe "a commit's author GitHub links to no account"
    (it "says so, with the name and e-mail Git wrote"
      (let* ((context (with-temp-buffer
                        (insert (approve-render-git-actor
                                 '((name . "Mona Lisa") (email . "mona@example.com"))))
                        (approve-context-at-point (point-min))))
             (description (approve-eldoc-describe context)))
        (expect (plist-get context :kind) :to-be 'git-actor)
        (expect (plist-get context :value) :to-equal "Mona Lisa <mona@example.com>")
        (expect (substring-no-properties (plist-get description :echo))
                :to-equal "Mona Lisa — <mona@example.com> — no GitHub account")
        (expect (plist-get description :doc) :to-match "no account has this one"))))

  (describe "the eldoc function"
    (it "calls back with the doc and the echo line, and says nothing elsewhere"
      (with-current-buffer test-eldoc--buffer
        (goto-char (point-min))
        (search-forward "bob-example") (goto-char (match-beginning 0))
        (let (args)
          (expect (approve-eldoc-function (lambda (&rest a) (setq args a))) :to-be t)
          (expect (substring-no-properties (car args)) :to-match "\\`@bob-example — Bob Example")
          (expect (substring-no-properties (plist-get (cdr args) :echo))
                  :to-equal "@bob-example — Bob Example"))
        (goto-char (point-min))
        (search-forward "Rewrites the parser")
        (expect (approve-eldoc-function #'ignore) :to-be nil))))

  (describe "details"
    (it "are not fetched when point only rests on a thing"
      (spy-on 'approve-query-details)
      (with-current-buffer test-eldoc--buffer
        (setq approve-eldoc--wanted nil)
        (goto-char (point-min))
        (search-forward "bob-example") (goto-char (match-beginning 0))
        (approve-eldoc-function #'ignore))
      (expect 'approve-query-details :not :to-have-been-called))

    (it "are fetched once when wanted, then shown and cached"
      (let ((pending nil))
        (spy-on 'approve-query-details
                :and-call-fake (lambda (kind key &rest args)
                                 (push (list kind key (plist-get args :callback)) pending)))
        (spy-on 'eldoc-print-current-symbol-info)
        (let ((doc (plist-get (test-eldoc--describe "bob-example" 0 t) :doc)))
          (expect doc :to-match "loading details…"))
        (expect (length pending) :to-equal 1)
        (expect (car pending) :to-equal (list 'user "bob-example" (nth 2 (car pending))))
        (test-eldoc--describe "bob-example" 0 t)
        (expect (length pending) :to-equal 1)
        (with-current-buffer test-eldoc--buffer
          (funcall (nth 2 (car pending))
                   '((login . "bob-example") (name . "Bob Example") (pronouns . "he/him")
                     (email . "bob@example.com") (bio . "Tools and coffee.")
                     (company . "@acme") (location . "Gothenburg"))))
        (let ((doc (substring-no-properties
                    (plist-get (test-eldoc--describe "bob-example" 0 t) :doc))))
          (expect doc :to-match "Bob Example (he/him)")
          (expect doc :to-match "bob@example.com")
          (expect doc :to-match "Tools and coffee.")
          (expect doc :to-match "Location   Gothenburg")
          (expect doc :to-match "In this pull request\n  approved")
          (expect doc :not :to-match "loading details"))))

    (it "remember a failure for a minute and say so"
      (spy-on 'approve-query-details
              :and-call-fake (lambda (_kind _key &rest args)
                               (funcall (plist-get args :errorback)
                                        '(approve-api-not-found "Not found"))))
      (spy-on 'eldoc-print-current-symbol-info)
      (test-eldoc--describe "carol-example" 0 t)
      (expect (plist-get (test-eldoc--describe "carol-example" 0 t) :doc)
              :to-match "details unavailable (Not found)")
      (expect 'approve-query-details :to-have-been-called-times 1))

    (it "of an outdated thread show the code it was written on"
      (spy-on 'approve-query-details
              :and-call-fake (lambda (kind _key &rest args)
                               (expect kind :to-be 'comment-code)
                               (funcall (plist-get args :callback)
                                        '((diffHunk . "@@ -58,3 +58,3 @@\n a\n-b\n+c")
                                          (originalLine . 61)
                                          (originalCommit (abbreviatedOid . "e99c61a"))))))
      (spy-on 'eldoc-print-current-symbol-info)
      (with-current-buffer test-eldoc--buffer
        (goto-char (point-min))
        (search-forward "· outdated ─")
        (goto-char (line-beginning-position))
        (approve-eldoc-describe (approve-context-at-point) t)
        (let ((doc (substring-no-properties
                    (plist-get (approve-eldoc-describe (approve-context-at-point)) :doc))))
          (expect doc :to-match "Written on line 61 at e99c61a")
          (expect doc :to-match "@@ -58,3 \\+58,3 @@\n a\n-b\n\\+c"))))))

;;; test-approve-eldoc.el ends here
