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
    (it "shows the title, state, branches and dates"
      (let ((lines (split-string (test-sections--text 'approve-header-section) "\n")))
        (expect (nth 0 lines) :to-match "\\`#202 Rewrite the parser and engine scheduler +● Open")
        (expect (nth 1 lines)
                :to-equal "acme-example/rocket · alice-example wants to merge 4 commits into main from parser-rewrite")
        (expect (nth 2 lines) :to-equal "Opened 3 days ago · updated 23 hours ago")))

    (it "shows reviewers with their verdicts, then who is asked"
      (expect (test-sections--text 'approve-meta-section 'reviewers)
              :to-equal "Reviewers   ✓ bob-example   ✗ carol-example   ○ dave-example   ○ @acme-example/core\n"))

    (it "shows assignees, labels, milestone, closing issues and status"
      (expect (test-sections--text 'approve-meta-section 'assignees)
              :to-equal "Assignees   alice-example\n")
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
        (expect (car lines) :to-match "\\`Your pending review +3 comments")
        (expect (nth 1 lines) :to-match "\\`internal/parser/parser.go:134 +Is this branch reachable")
        (expect (nth 2 lines) :to-match "\\`internal/engine/engine.go:365–367 ")
        (expect (length lines) :to-equal 4))))

  (describe "description"
    (it "shows the rendered body and its reactions"
      (expect (test-sections--text 'approve-description-section)
              :to-equal (concat "Description\n"
                                "Rewrites the parser for speed and renames the queue to a scheduler.\n\n"
                                "The lexer fixes come in a follow-up commit.\n\n"
                                "Fixes #12.\n\n👍 3  🚀 1\n\n"))))

  (describe "commits"
    (it "counts commits and marks those after the viewer's review"
      (let ((lines (split-string (test-sections--text 'approve-commits-section) "\n" t)))
        (expect (car lines) :to-match "\\`Commits (4) +2 new since your review")
        (expect (nth 2 lines) :to-match "\\`  6fa7df8  Rename the queue to scheduler .*← your review")
        (expect (nth 3 lines) :to-match "\\`┃ 71cc73d  Address review comments .* new\\'")))

    (it "carries each commit's oid"
      (expect (plist-get (test-sections--thing-at "dd4ac8a") :kind) :to-be 'commit)))

  (describe "files"
    (it "sums the files up in the heading"
      (expect (car (split-string (test-sections--text 'approve-files-section) "\n"))
              :to-match "\\`Files changed   30 files  \\+987 −184 +8/30 viewed · 15 unresolved threads"))

    (it "lists each file with its viewed mark, counts, bar and threads"
      (expect (test-sections--text 'approve-file-entry-section "internal/parser/parser.go")
              :to-match "\\`· internal/parser/parser.go +\\+21  −11  ██▒ +💬 5\n")
      (expect (test-sections--text 'approve-file-entry-section "internal/engine/legacy.go")
              :to-match "\\`✓ internal/engine/legacy.go +\\+0  −38  ▒▒▒▒ ")
      (expect (test-sections--text 'approve-file-entry-section "CHANGELOG.md")
              :to-match "\\`↻ CHANGELOG.md ")))

  (describe "conversation"
    (it "starts with who opened the PR and ends the line"
      (let ((lines (split-string (test-sections--text 'approve-conversation-section) "\n" t)))
        (expect (car lines) :to-equal "Conversation (30)")
        (expect (nth 1 lines) :to-equal "alice-example opened this 3 days ago")
        (expect (car (last lines)) :to-equal "╰")))

    (it "folds runs of commits and of labels"
      (let ((text (test-sections--text 'approve-conversation-section)))
        (expect text :to-match "├ ↑ alice-example pushed 2 commits · 3 days ago\n  dd4ac8a  Add the new parser\n  6fa7df8  Rename")
        (expect text :to-match "├ 🏷 alice-example added  enhancement   area: parser  · 3 days ago")))

    (it "shows comments and reviews with their verdict, body and code comments"
      (let ((text (test-sections--text 'approve-conversation-section)))
        (expect text :to-match "├ 💬 bob-example commented · 3 days ago\nThanks! I'll take a look today.\n👍 1\n")
        (expect text :to-match "├ ✓ bob-example approved · 1 day ago · 2 comments on code\nLooks good to me.")
        (expect text :to-match "├ ✗ carol-example requested changes · 1 day ago · 1 comment on code")))

    (it "shows events in words"
      (let ((text (test-sections--text 'approve-conversation-section)))
        (expect text :to-match "changed the title from “Parser rewrite” to “Rewrite the parser and engine scheduler”")
        (expect text :to-match "asked @acme-example/core for a review")
        (expect text :to-match "mentioned this in acme-example/rocket#12 The parser is slow")
        (expect text :to-match "force-pushed the branch from e99c61a to 6fa7df8")))

    (it "draws the timeline's line left of comment bodies, display-only"
      (with-current-buffer test-sections--buffer
        (goto-char (point-min))
        (search-forward "Thanks! I'll take a look")
        (expect (substring-no-properties (get-text-property (point) 'line-prefix))
                :to-equal "  │   ")))))

;;; test-approve-sections.el ends here
