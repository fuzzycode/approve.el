;;; test-approve-sections.el --- Tests for the sections of the PR buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; What the header, metadata rows, pending review, description, commits,
;; file list and conversation show for the fixtures, and the things
;; (people, labels, commits) they carry for the context and eldoc.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-test-helpers "helpers")

(defvar test-sections--buffer nil
  "The loaded medium PR buffer the tests read.")

(defun test-sections--text (type &optional value)
  "Return the text of the first section of TYPE (and VALUE)."
  (with-current-buffer test-sections--buffer
    (let ((section (approve-test-find-section type value)))
      (unless section (error "No %s section" type))
      (approve-test-section-text section))))

(defun test-sections--thing-at (text)
  "Return the `approve-thing' at the first TEXT in the buffer."
  (with-current-buffer test-sections--buffer
    (goto-char (point-min))
    (search-forward text)
    (get-text-property (match-beginning 0) 'approve-thing)))

(describe "The sections of the medium PR"
  (before-all
    (let ((approve-use-nerd-icons nil))
      (approve-icon-clear-cache)
      (approve-test-with-fixture "medium"
        (setq test-sections--buffer (approve-test-open-pr "medium")))))
  (after-all
    (kill-buffer test-sections--buffer)
    (approve-icon-clear-cache))

  (describe "header"
    (it "shows the author, repository, branches and dates as rows; the title is in the header line"
      (let ((lines (split-string (test-sections--text 'approve-header-section) "\n")))
        (expect (nth 0 lines) :to-equal "Author      @alice-example")
        (expect (nth 1 lines) :to-equal "Repository  acme-example/rocket")
        (expect (nth 2 lines) :to-equal "Branch      parser-rewrite → main")
        (expect (nth 3 lines) :to-equal "Opened      3 days ago · updated 23 hours ago")
        (expect (nth 4 lines) :to-match "\\`Reviewers ")))

    (it "says when and by whom a merged PR was merged"
      (cl-letf (((symbol-function 'approve-pr-fields)
                 (lambda (_fields)
                   '((createdAt . "2026-10-01T12:00:00Z") (updatedAt . "2026-10-04T10:00:00Z")
                     (merged . t) (mergedAt . "2026-10-04T11:00:00Z")
                     (mergedBy (login . "bob-example"))))))
        (let ((approve-render-now approve-test-now))
          (let ((dates (approve-sections--dates)))
            (expect (substring-no-properties dates)
                    :to-equal "3 days ago · updated 2 hours ago · merged 1 hour ago by @bob-example")
            ;; Dates look like the other rows' values; only the words are dim.
            (expect (get-text-property 0 'face dates) :to-be 'default)
            (expect (get-text-property (string-search "updated" dates) 'face dates)
                    :to-be 'approve-dim)))))

    (it "links the repository and carries the branches"
      (with-current-buffer test-sections--buffer
        (goto-char (point-min))
        (search-forward "acme-example/rocket")
        (expect (get-text-property (match-beginning 0) 'approve-url)
                :to-match "/acme-example/rocket\\'"))
      (expect (plist-get (test-sections--thing-at "parser-rewrite →") :kind) :to-be 'branch))

    (it "shows reviewers with their verdicts, then who is asked"
      (expect (test-sections--text 'approve-meta-section 'reviewers)
              :to-equal "Reviewers   ✓ @bob-example   ✗ @carol-example   ○ @dave-example   ○ @acme-example/core\n"))

    (it "shows assignees, labels, milestone, closing issues and status"
      (expect (test-sections--text 'approve-meta-section 'assignees)
              :to-equal "Assignees   @alice-example\n")
      (expect (test-sections--text 'approve-meta-section 'labels)
              :to-equal "Labels       enhancement   area: parser \n")
      (expect (test-sections--text 'approve-meta-section 'milestone)
              :to-equal "Milestone   v1.2\n")
      (expect (test-sections--text 'approve-meta-section 'issues)
              :to-equal "Issues      #12 The parser is slow on large inputs\n")
      (expect (test-sections--text 'approve-meta-section 'status)
              :to-equal "Status      ✗ Changes requested · ✓ No conflicts · ✗ 2 of 6 checks passed\n"))

    (it "carries what people, labels and issues are"
      (expect (plist-get (test-sections--thing-at "bob-example") :login) :to-equal "bob-example")
      (expect (plist-get (test-sections--thing-at "@acme-example/core") :kind) :to-be 'team)
      (expect (plist-get (test-sections--thing-at " enhancement ") :kind) :to-be 'label)
      (expect (plist-get (test-sections--thing-at "#12") :number) :to-equal 12)
      (expect (plist-get (test-sections--thing-at "v1.2") :kind) :to-be 'milestone)
      (expect (plist-get (test-sections--thing-at "parser-rewrite") :kind) :to-be 'branch)))

  (describe "pending review"
    (it "lists the viewer's pending comments with where they are"
      (let ((lines (split-string (test-sections--text 'approve-pending-review-section)
                                 "\n" t)))
        (expect (car lines) :to-match "\\`Your pending review (3)")
        (expect (nth 1 lines) :to-match "\\`  internal/parser/parser.go:134 +Is this branch reachable")
        (expect (nth 2 lines) :to-match "\\`  internal/engine/engine.go:365–367 ")
        (expect (length lines) :to-equal 4)))

    (it "lists its comments without the pending face"
      (with-current-buffer test-sections--buffer
        (let ((section (approve-test-find-section 'approve-pending-review-section)))
          (expect (seq-some (lambda (pos)
                              (memq 'approve-pending
                                    (ensure-list (get-text-property pos 'face))))
                            (number-sequence (oref section start) (1- (oref section end))))
                  :to-be nil)))))

  (describe "description"
    (it "shows the rendered body and its reactions"
      (expect (test-sections--text 'approve-description-section)
              :to-equal (concat "Description  e edit\n"
                                "  Rewrites the parser for speed and renames the queue to a scheduler.\n\n"
                                "  The lexer fixes come in a follow-up commit.\n\n"
                                "  Fixes #12.\n\n  👍 3  🚀 1\n\n")))

    (it "says it was edited right after its heading"
      (cl-letf (((symbol-function 'approve-pr-fields)
                 (lambda (_fields)
                   '((id . "PR_1") (bodyHTML . "<p>Hello</p>")
                     (lastEditedAt . "2026-10-04T10:00:00Z"))))
                ((symbol-function 'approve-model-root-ref) (lambda () t)))
        (with-temp-buffer
          (magit-section-mode)
          (let ((inhibit-read-only t))
            (magit-insert-section (root)
              (approve-insert-description-section)))
          (goto-char (point-min))
          (expect (buffer-substring-no-properties (point-min) (line-end-position))
                  :to-equal "Description · edited")
          (search-forward "edited")
          (expect (plist-get (get-text-property (match-beginning 0) 'approve-thing) :kind)
                  :to-be 'date)))))

  (describe "commits"
    (it "counts commits and marks those after the viewer's review"
      (let ((lines (split-string (test-sections--text 'approve-commits-section) "\n" t)))
        (expect (car lines) :to-match "\\`Commits (4) +2 new since your review")
        (expect (nth 2 lines) :to-match "\\`    6fa7df8  Rename the queue to scheduler .*← your review")
        (expect (nth 3 lines) :to-match "\\`  ┃ 71cc73d  Address review comments .* new\\'")))

    (it "carries each commit's oid"
      (expect (plist-get (test-sections--thing-at "dd4ac8a") :kind) :to-be 'commit)))

  (describe "files"
    (it "sums the files up in the heading"
      (expect (car (split-string (test-sections--text 'approve-files-section) "\n"))
              :to-match "\\`Files changed (30)  \\+987 −184 +2 unresolved file threads\\'"))

    (it "lists each file with its counts, bar and threads on the whole file"
      ;; parser.go has one thread on the file and four on lines: the
      ;; diff's heading counts those.
      (expect (test-sections--text 'approve-file-entry-section "internal/parser/parser.go")
              :to-match "\\`  internal/parser/parser.go +\\+21  −11  ██▒ +💬 1\n")
      (expect (test-sections--text 'approve-file-entry-section "internal/engine/engine.go")
              :not :to-match "💬")
      (expect (test-sections--text 'approve-file-entry-section "internal/engine/legacy.go")
              :to-match "\\`  internal/engine/legacy.go +\\+0  −38  ▒▒▒▒ "))

    (it "marks red the threads that ask for changes, and only those (D34)"
      ;; carol's latest verdict requests changes; her thread on parser.go
      ;; is on the whole file, so the file list's mark is red and the
      ;; diff heading's, which counts threads on lines, is not.
      (with-current-buffer test-sections--buffer
        (let ((red (lambda (type path)
                     (let ((section (approve-test-find-section type path)))
                       (seq-some (lambda (pos)
                                   (memq 'approve-thread-mark-changes-requested
                                         (ensure-list (get-text-property pos 'face))))
                                 (number-sequence (oref section start)
                                                  (1- (or (oref section content)
                                                          (oref section end)))))))))
          (expect (funcall red 'approve-file-entry-section "internal/parser/parser.go")
                  :to-be-truthy)
          (expect (funcall red 'approve-file-section "internal/parser/parser.go")
                  :not :to-be-truthy)
          (expect (seq-filter (lambda (file)
                                (funcall red 'approve-file-entry-section
                                         (alist-get 'path file)))
                              (approve-pr-files))
                  :to-equal (list (seq-find (lambda (file)
                                              (equal (alist-get 'path file)
                                                     "internal/parser/parser.go"))
                                            (approve-pr-files)))))))

    (it "leaves out which files are viewed, which the diff's headings say"
      (with-current-buffer test-sections--buffer
        (let ((section (approve-test-find-section 'approve-file-entry-section
                                                  "internal/engine/legacy.go")))
          ;; legacy.go is viewed: it is neither marked nor dimmed.
          (expect (seq-some (lambda (pos)
                              (memq 'approve-viewed
                                    (ensure-list (get-text-property pos 'face))))
                            (number-sequence (oref section start) (1- (oref section end))))
                  :to-be nil)))))

  (describe "conversation"
    (it "starts with who opened the PR and ends the line"
      (let ((lines (split-string (test-sections--text 'approve-conversation-section) "\n" t)))
        (expect (car lines) :to-equal "Conversation (30)")
        (expect (nth 1 lines) :to-equal "  @alice-example opened this 3 days ago")
        (expect (car (last lines)) :to-equal "  ╰ C comment on this pull request")))

    (it "says no more is loading once the last page is in"
      (expect (test-sections--text 'approve-conversation-section)
              :not :to-match "more is loading"))

    (it "folds runs of commits and of labels"
      (let ((text (test-sections--text 'approve-conversation-section)))
        (expect text :to-match "  ├ ↑ @alice-example pushed 2 commits · 3 days ago\n  │     dd4ac8a  Add the new parser\n  │     6fa7df8  Rename")
        (expect text :to-match "  ├ 🏷 @alice-example added  enhancement   area: parser  · 3 days ago")))

    (it "shows comments and reviews with their verdict, body and code comments"
      (let ((text (test-sections--text 'approve-conversation-section)))
        (expect text :to-match "  ├ 💬 @bob-example commented · 3 days ago +r quote · \\+ react\n  │   Thanks! I'll take a look today.\n  │   👍 1\n")
        (expect text :to-match "  ├ ✓ @bob-example approved · 1 day ago · 2 comments on code\n  │   Looks good to me.")
        (expect text :to-match "  ├ ✗ @carol-example requested changes · 1 day ago · 1 comment on code")))

    (it "shows events in words"
      (let ((text (test-sections--text 'approve-conversation-section)))
        (expect text :to-match "changed the title from “Parser rewrite” to “Rewrite the parser and engine scheduler”")
        (expect text :to-match "asked @acme-example/core for a review")
        (expect text :to-match "mentioned this in #12 The parser is slow")
        (expect text :not :to-match "acme-example/rocket#12")
        (expect text :to-match "force-pushed the branch from e99c61a to 6fa7df8")))

    (it "names the repository of a mention only when it is another one"
      (let ((approve-render-now approve-test-now)
            (approve-use-nerd-icons nil))
        (expect (substring-no-properties
                 (approve-conversation--event-text
                  '((__typename . "CrossReferencedEvent")
                    (actor (login . "carol-example"))
                    (createdAt . "2026-10-04T10:00:00Z")
                    (isCrossRepository . t)
                    (source (__typename . "Issue") (number . 7) (title . "Slow on ARM")
                            (url . "https://github.com/other-example/tool/issues/7")
                            (repository (nameWithOwner . "other-example/tool"))))))
                :to-match "mentioned this in other-example/tool#7 Slow on ARM")))

    (it "draws the timeline's line left of comment bodies, left out on copy"
      (with-current-buffer test-sections--buffer
        (goto-char (point-min))
        (search-forward "Thanks! I'll take a look")
        (goto-char (match-beginning 0))
        (expect (buffer-substring-no-properties (line-beginning-position) (point))
                :to-equal "  │   ")
        (expect (filter-buffer-substring (line-beginning-position) (line-end-position))
                :to-equal "Thanks! I'll take a look today.")))

    (it "keeps empty lines free of trailing spaces"
      (with-current-buffer test-sections--buffer
        (goto-char (point-min))
        (expect (re-search-forward "^[ \t]+$" nil t) :to-be nil)))))

(describe "Indentation in the medium PR"
  (it "is text, never a line-prefix, which a collapsed body lends the next heading"
    (approve-test-with-fixture "medium"
      (let ((buffer (approve-test-open-pr "medium")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-show-level-4-all)
              (expect (text-property-not-all (point-min) (point-max) 'line-prefix nil)
                      :to-be nil)
              ;; Each section starts at the start of its line, indentation
              ;; included, so hiding a body never hides part of a line.
              (magit-map-sections
               (lambda (section)
                 (save-excursion
                   (goto-char (oref section start))
                   (expect (bolp) :to-be t)
                   (when (oref section content)
                     (goto-char (oref section content))
                     (expect (bolp) :to-be t))))))
          (kill-buffer buffer))))))

(defun test-sections--check (name status conclusion &rest more)
  "Return a check run NAME with STATUS, CONCLUSION and MORE fields.
MORE alternates field names and values, as in `list'."
  (append `((__typename . "CheckRun") (id . ,(concat "CR_" name)) (name . ,name)
            (status . ,status) (conclusion . ,conclusion))
          (cl-loop for (key value) on more by #'cddr collect (cons key value))))

(describe "The checks (F16)"
  (describe "each"
    (it "went one of four ways: skipped and neutral runs are apart, as GitHub has them"
      (expect (mapcar #'approve-sections-check-kind
                      (list (test-sections--check "a" "COMPLETED" "SUCCESS")
                            (test-sections--check "b" "COMPLETED" "SKIPPED")
                            (test-sections--check "c" "COMPLETED" "NEUTRAL")
                            (test-sections--check "d" "COMPLETED" "CANCELLED")
                            (test-sections--check "e" "QUEUED" nil)
                            '((__typename . "StatusContext") (state . "EXPECTED"))))
              :to-equal '(success skipped skipped failure running running)))

    (it "says how long it ran, never less than a second"
      (expect (approve-sections-check-duration
               (test-sections--check "a" "COMPLETED" "SUCCESS"
                                     'startedAt "2026-10-01T10:00:00Z"
                                     'completedAt "2026-10-01T10:02:10Z"))
              :to-equal "2m 10s")
      ;; GitHub ends a skipped job before it starts; apps' checks take no time.
      (expect (approve-sections-check-duration
               (test-sections--check "b" "COMPLETED" "SKIPPED"
                                     'startedAt "2026-10-01T10:00:00Z"
                                     'completedAt "2026-10-01T09:59:59Z"))
              :to-be nil)
      (expect (approve-sections-check-duration
               (test-sections--check "c" "COMPLETED" "SUCCESS"
                                     'startedAt "2026-10-01T10:00:00Z"
                                     'completedAt "2026-10-01T10:00:00Z"))
              :to-be nil))

    (it "says its state in words where a time would say less"
      (expect (mapcar (lambda (check)
                        (substring-no-properties (approve-sections--check-when check)))
                      (list (test-sections--check "a" "IN_PROGRESS" nil)
                            (test-sections--check "b" "QUEUED" nil)
                            (test-sections--check "c" "COMPLETED" "SKIPPED")
                            (test-sections--check "d" "COMPLETED" "TIMED_OUT")
                            (test-sections--check "e" "COMPLETED" "FAILURE")
                            '((__typename . "StatusContext") (state . "PENDING"))))
              :to-equal '("running" "queued" "skipped" "timed out" "" "pending")))

    (it "keeps the end of its name when the name is too long"
      (let ((name "Test All / prebuild_apple_dependencies / Build Apple Slice (Debug, ios)"))
        (expect (approve-sections--shorten-check-name name 80) :to-equal name)
        (expect (approve-sections--shorten-check-name name 66)
                :to-equal "… / prebuild_apple_dependencies / Build Apple Slice (Debug, ios)")
        (expect (approve-sections--shorten-check-name name 50)
                :to-equal "… / Build Apple Slice (Debug, ios)")
        (expect (approve-sections--shorten-check-name name 20)
                :to-equal "…Slice (Debug, ios)"))))

  (it "come failing first, then running, passed and skipped; required first in each"
    (expect (mapcar (lambda (check) (alist-get 'name check))
                    (approve-sections-sort-checks
                     (list (test-sections--check "skipped" "COMPLETED" "SKIPPED")
                           (test-sections--check "passed" "COMPLETED" "SUCCESS")
                           (test-sections--check "required" "COMPLETED" "SUCCESS"
                                                 'isRequired t)
                           (test-sections--check "running" "IN_PROGRESS" nil)
                           (test-sections--check "failed" "COMPLETED" "FAILURE"))))
            :to-equal '("failed" "running" "required" "passed" "skipped")))

  (it "fold the passed and skipped ones beyond a few, and say which"
    (expect (substring-no-properties
             (approve-sections--more-checks
              (list (test-sections--check "a" "COMPLETED" "SUCCESS")
                    (test-sections--check "b" "COMPLETED" "SKIPPED"))))
            :to-equal "… 1 more passed, 1 skipped")
    (expect (substring-no-properties
             (approve-sections--more-checks
              (list (test-sections--check "b" "COMPLETED" "NEUTRAL"))))
            :to-equal "… 1 more skipped"))

  (describe "of the small PR"
    (it "count the skipped and neutral ones apart, and list them last, in grey"
      (let ((approve-use-nerd-icons nil))
        (approve-icon-clear-cache)
        (approve-test-with-fixture "small"
          (let ((buffer (approve-test-open-pr "small")))
            (unwind-protect
                (with-current-buffer buffer
                  (expect (substring-no-properties (approve-sections--status))
                          :to-match "✓ 2 of 2 checks passed\\'")
                  (magit-section-show (approve-test-find-section 'approve-checks-section))
                  (approve-pr--run-lazy-loaders)
                  (approve-test-wait-for (lambda () (approve-pr-loaded-p 'checks)))
                  (approve-pr-redraw)
                  (let ((lines (split-string (approve-test-section-text
                                              (approve-test-find-section
                                               'approve-checks-section))
                                             "\n" t)))
                    (expect (car lines)
                            :to-match "\\`Checks (4) .*✓ 2 passed · 2 skipped\\'")
                    (expect (cdr lines)
                            :to-equal
                            '("  ✓ Tests / test (3.12)  4m 00s   required  test (3.12): success"
                              "  ✓ Lint / lint          4m 00s             lint: success"
                              "  ⊘ Tests / deploy       skipped            deploy: skipped"
                              "  ⊘ Docs / docs          neutral            docs: neutral")))
                  (goto-char (point-min))
                  (search-forward "Tests / deploy       skipped")
                  (expect (get-text-property (1- (point)) 'face) :to-be 'approve-check-skipped)
                  (expect (get-text-property (+ (line-beginning-position) 2) 'face)
                          :to-be 'approve-check-skipped)
                  (expect (plist-get (approve-context-at-point) :url)
                          :to-match "/actions/runs/.*/job/"))
              (kill-buffer buffer)
              (approve-icon-clear-cache))))))))

;;; test-approve-sections.el ends here
