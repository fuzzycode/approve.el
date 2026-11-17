;;; test-approve-feedback.el --- Tests for the feedback mode -*- lexical-binding: t; -*-

;;; Commentary:

;; The feedback mode (flow F21, decisions D45 and D46) in a buffer of
;; the medium PR's internal/parser/parser.go, in a temporary directory
;; that stands for the clone: the branch, its pull requests and the
;; clone's state are the clone module's answers, faked here; the pull
;; request loads through the fake API.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-feedback)
(require 'approve-test-helpers "helpers")

(defconst test-feedback--path "internal/parser/parser.go"
  "The medium PR's file with threads of every kind.")

(defconst test-feedback--head
  '(:host "github.com" :owner "acme-example" :repo "rocket" :branch "parser-rewrite")
  "The branch of the clone on GitHub.")

(defun test-feedback--row (number &optional state)
  "Return a pull request of the branch: NUMBER in STATE, open by default."
  `((__typename . "PullRequest") (id . ,(format "PR_%d" number)) (number . ,number)
    (url . ,(format "https://github.com/acme-example/rocket/pull/%d" number))
    (title . ,(format "Pull request %d" number)) (state . ,(or state "OPEN")) (isDraft)
    (updatedAt . "2026-10-03T10:00:00Z") (baseRefName . "main")
    (headRefName . "parser-rewrite")
    (repository (__typename . "Repository") (id . "R_1")
                (nameWithOwner . "acme-example/rocket"))))

(defvar test-feedback--prs nil
  "The pull requests of the clone's branch, as GitHub would list them.")

(defvar test-feedback--root nil
  "The temporary directory that stands for the clone.")

(defvar test-feedback--blobs nil
  "The blob queries sent to the fake GitHub, the last first.")

(defconst test-feedback--text
  (mapconcat (lambda (n) (format "line %d\n" n)) (number-sequence 1 600) "")
  "GitHub's text of the file at the head, and the clone's at first.")

(defconst test-feedback--head-oid "6ffac5752e1be53bad90e2beeb6d4b0e1f998cc7"
  "The medium PR's head.")

(defvar test-feedback--older nil
  "GitHub's text of the file at commits other than the head, or nil.
Nil means the head's, `test-feedback--text'.")

(defun test-feedback--blob-answer (request)
  "Return GitHub's answer to the blob query REQUEST.
The file's text is `test-feedback--text' at the head, else
`test-feedback--older'."
  (push request test-feedback--blobs)
  (let* ((expression (alist-get 'expression (plist-get request :variables)))
         (text (if (and test-feedback--older
                        (not (string-prefix-p test-feedback--head-oid expression)))
                   test-feedback--older
                 test-feedback--text))
         (timer (run-at-time
                 0 nil (plist-get request :on-success)
                 `((data (repository
                          (__typename . "Repository") (id . "R_1")
                          (object (__typename . "Blob") (id . "B_1")
                                  (text . ,text) (isBinary) (isTruncated)
                                  (byteSize . ,(string-bytes text))))))
                 nil)))
    (lambda () (cancel-timer timer))))

(defun test-feedback--pr-buffers ()
  "Return the live PR buffers."
  (seq-filter (lambda (buffer) (eq (buffer-local-value 'major-mode buffer) 'approve-pr-mode))
              (buffer-list)))

(defmacro test-feedback--with-clone (&rest body)
  "Run BODY in a buffer of the medium PR's file in a fake clone.
The branch heads `test-feedback--prs' (the medium PR #202 and #203,
open, unless BODY binds it otherwise), and is a commit ahead of
GitHub.  The clone hasn't GitHub's head: the file's text comes from
the fake GitHub (`test-feedback--text').  The buffer, the PR buffers
and the clone are gone after."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((test-feedback--root (file-name-as-directory
                                  (make-temp-file "approve-feedback-" t)))
            (file (expand-file-name test-feedback--path test-feedback--root))
            (replay approve-api-transport)
            (approve-api-transport
             (lambda (request)
               (if (equal (plist-get request :name) "queries/blob.graphql")
                   (test-feedback--blob-answer request)
                 (funcall replay request))))
            (test-feedback--blobs nil))
       (make-directory (file-name-directory file) t)
       (with-temp-file file
         (insert test-feedback--text))
       (approve-file-clear-cache)
       (spy-on 'approve-clone-branch-head
               :and-return-value (list :clone test-feedback--root :branch "parser-rewrite"
                                       :remote "origin" :head test-feedback--head))
       (spy-on 'approve-clone-wait-pull-requests
               :and-call-fake (lambda (_head) test-feedback--prs))
       (spy-on 'approve-clone-state :and-return-value '(:state ahead :ahead 1 :behind 0))
       (spy-on 'approve-clone-file-text :and-return-value nil)
       (let ((buffer (find-file-noselect file)))
         (unwind-protect
             (with-current-buffer buffer
               ,@body)
           (with-current-buffer buffer
             (set-buffer-modified-p nil))
           (kill-buffer buffer)
           (mapc #'kill-buffer (test-feedback--pr-buffers))
           (approve-file-clear-cache)
           (delete-directory test-feedback--root t))))))

(defun test-feedback--wait ()
  "Wait until the pull request of the current buffer and GitHub's text are in."
  (approve-test-wait-for (lambda () (and (approve-feedback--loaded-p)
                                         (not (approve-pr-loading-p)))))
  (with-current-buffer approve-model-store-buffer
    (approve-pr-redraw))
  (approve-test-wait-for (lambda () (not approve-feedback--fetching))))

(defun test-feedback--lighter ()
  "Return the lighter of the current buffer, without its properties."
  (substring-no-properties approve-feedback--lighter))

(defun test-feedback--drawn ()
  "Return the threads drawn here: ((LINE (GITHUB-LINE . WHERE) ...) ...).
LINE 0 is above the first line."
  (mapcar (lambda (overlay)
            (cons (approve-feedback--line-of overlay)
                  (mapcar (lambda (entry) (cons (alist-get 'line (car entry)) (cdr entry)))
                          (overlay-get overlay 'approve-feedback-entries))))
          (approve-feedback--overlays)))

(defun test-feedback--bubble (line)
  "Return the bubble in the margin of LINE, 0 above the first line."
  (let ((overlay (seq-find (lambda (overlay) (= (approve-feedback--line-of overlay) line))
                           (approve-feedback--overlays))))
    (cadr (get-text-property 0 'display (overlay-get overlay 'before-string)))))

(defun test-feedback--threads-shown (line)
  "Return the text of the threads unfolded under LINE, or nil when folded."
  (let* ((overlay (seq-find (lambda (overlay) (= (approve-feedback--line-of overlay) line))
                            (approve-feedback--overlays)))
         (text (overlay-get overlay (if (zerop line) 'before-string 'after-string))))
    (and text (string-match-p "╭" text) (substring-no-properties text))))

(defun test-feedback--start ()
  "Turn the mode on, and wait until the threads are drawn."
  (approve-feedback-mode 1)
  (test-feedback--wait)
  (approve-test-wait-for #'approve-feedback--overlays))

(defun test-feedback--idle ()
  "Compare the lines again, as Emacs would once idle after an edit."
  (cancel-timer approve-lines--timer)
  (approve-lines--idle (current-buffer)))

(describe "approve-feedback-mode"
  (it "shows the branch's pull request, loaded in a PR buffer it doesn't show"
    (let ((test-feedback--prs (list (test-feedback--row 202) (test-feedback--row 203))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (expect approve-feedback-mode :to-be t)
        (expect (buffer-name approve-model-store-buffer)
                :to-equal "*approve: acme-example/rocket#202*")
        (expect (get-buffer-window approve-model-store-buffer t) :to-be nil)
        (expect (test-feedback--lighter) :to-match "#202 …")
        (test-feedback--wait)
        (expect approve-feedback--path :to-equal test-feedback--path)
        (expect (test-feedback--lighter) :not :to-match "…")
        ;; Five unresolved threads in the file, the first of two open
        ;; pull requests, a commit not pushed.
        (expect (test-feedback--lighter) :to-match "#202 .*5 1/2 ↑1\\'")
        (let ((tooltip (get-text-property 1 'help-echo approve-feedback--lighter)))
          (expect tooltip :to-match "acme-example/rocket#202")
          (expect tooltip :to-match "parser-rewrite into main · 1 of 2 open pull requests")
          (expect tooltip :to-match "1 commit not pushed")
          (expect tooltip :to-match "5 unresolved threads in this file")))))

  (it "shares the store of the PR buffer that is open"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (let ((pr (approve-test-open-pr "medium")))
          (approve-feedback-mode 1)
          (expect approve-model-store-buffer :to-be pr)
          (expect (test-feedback--pr-buffers) :to-equal (list pr))
          (expect (approve-pr-field 'number) :to-equal 202)
          (expect (test-feedback--lighter) :not :to-match "1/")))))

  (it "asks before a merged pull request, and stays off when declined"
    (let ((test-feedback--prs (list (test-feedback--row 202 "MERGED"))))
      (test-feedback--with-clone
        (spy-on 'y-or-n-p :and-return-value nil)
        (approve-feedback-mode 1)
        (expect (spy-calls-args-for 'y-or-n-p 0)
                :to-equal '("#202 is merged: show its threads anyway? "))
        (expect approve-feedback-mode :to-be nil)
        (expect (test-feedback--pr-buffers) :to-equal nil)
        (spy-on 'y-or-n-p :and-return-value t)
        (approve-feedback-mode 1)
        (expect approve-feedback-mode :to-be t)
        ;; The row says it is merged until the store says otherwise.
        (expect (test-feedback--lighter) :to-match "#202 … merged\\'"))))

  (it "says why it can't turn on, and stays off"
    (let ((test-feedback--prs nil))
      (test-feedback--with-clone
        (expect (approve-feedback-mode 1) :to-throw 'user-error '("No pull request for parser-rewrite"))
        (expect approve-feedback-mode :to-be nil)
        (spy-on 'approve-clone-branch-head :and-return-value nil)
        (expect (approve-feedback-mode 1) :to-throw 'user-error)
        (expect approve-feedback-mode :to-be nil))))

  (it "takes away what it added when turned off"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (approve-feedback-mode -1)
        (expect approve-model-store-buffer :to-be nil)
        (expect approve-feedback--lighter :to-be nil)
        (expect approve-feedback--pull-request :to-be nil)))))

(describe "The feedback mode's PR buffer"
  (it "redraws the clone's buffers when it redraws"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (spy-on 'approve-feedback-redraw)
        (with-current-buffer approve-model-store-buffer
          (approve-pr-redraw))
        (expect 'approve-feedback-redraw :to-have-been-called-times 1))))

  (it "is opened and loaded again when it is killed"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (let ((old approve-model-store-buffer))
          (kill-buffer old)
          (approve-test-wait-for (lambda () (buffer-live-p approve-model-store-buffer)))
          (expect approve-model-store-buffer :not :to-be old)
          (test-feedback--wait)
          (expect (approve-pr-field 'number) :to-equal 202)))))

  (it "is the one picked for the branch"
    (let ((test-feedback--prs (list (test-feedback--row 202) (test-feedback--row 203))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (let ((pr (test-feedback--row 203)))
          (run-hook-with-args 'approve-clone-picked-functions test-feedback--head pr)
          (expect approve-feedback--pull-request :to-be pr)
          (expect (buffer-name approve-model-store-buffer)
                  :to-equal "*approve: acme-example/rocket#203*")
          (expect (test-feedback--lighter) :to-match "#203 … 2/2"))))))

(describe "approve-feedback-refresh"
  (it "loads the pull request again in its PR buffer"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (let ((owner approve-model-store-buffer))
          (spy-on 'approve-pr-refresh)
          (approve-feedback-refresh)
          (expect 'approve-pr-refresh :to-have-been-called-with nil)
          (expect (spy-context-current-buffer (spy-calls-most-recent 'approve-pr-refresh))
                  :to-be owner)))))

  (it "says the mode is off"
    (with-temp-buffer
      (expect (approve-feedback-refresh) :to-throw 'user-error))))

(defmacro test-feedback--with-one (&rest body)
  "Run BODY as `test-feedback--with-clone' does, the branch heading #202 alone."
  (declare (indent 0) (debug t))
  `(let ((test-feedback--prs (list (test-feedback--row 202))))
     (test-feedback--with-clone
       ,@body)))

(describe "The feedback mode's threads"
  (it "go under their lines, by a diff of GitHub's file and the buffer"
    (test-feedback--with-one
      ;; A line added at the top, and line 131 edited.
      (goto-char (point-min))
      (insert "new first line\n")
      (forward-line 130)
      (end-of-line)
      (insert " edited")
      (test-feedback--start)
      (expect (test-feedback--drawn)
              :to-equal '((0 (nil . file)) (16 (15 . same)) (55 (52 . removed))
                          (62 (nil . outdated)) (132 (131 . edited)) (135 (134 . same))))
      ;; GitHub's texts came from GitHub, the head's and the outdated
      ;; thread's commit's: the clone hasn't them.
      (expect (length test-feedback--blobs) :to-equal 2)
      (expect right-margin-width :to-be-greater-than 2)
      (let ((bubble (test-feedback--bubble 132)))
        (expect (substring-no-properties bubble) :to-match "1\\'")
        (expect (get-text-property 0 'help-echo bubble)
                :to-equal "1 thread · edited here: GitHub has line 131\nmouse-1: unfold")
        (expect (get-text-property (1- (length bubble)) 'face bubble)
                :to-equal 'approve-edited-here))
      (expect (get-text-property (1- (length (test-feedback--bubble 16))) 'face
                                 (test-feedback--bubble 16))
              :to-equal 'approve-diff-bubble)))

  (it "take GitHub's file from the clone when it has the commit"
    (test-feedback--with-one
      (spy-on 'approve-clone-file-text :and-return-value test-feedback--text)
      (test-feedback--start)
      (expect test-feedback--blobs :to-equal nil)
      (expect (spy-calls-args-for 'approve-clone-file-text 0)
              :to-equal (list test-feedback--root "6ffac5752e1be53bad90e2beeb6d4b0e1f998cc7"
                              test-feedback--path))
      (expect (mapcar #'car (test-feedback--drawn)) :to-equal '(0 15 54 61 131 134))))

  (it "start folded, and TAB, T, n and p unfold them"
    (test-feedback--with-one
      (goto-char (point-min))
      (forward-line 130)
      (end-of-line)
      (insert " edited")
      (test-feedback--start)
      (expect (seq-filter #'test-feedback--threads-shown '(0 15 54 61 131 134)) :to-equal nil)
      (goto-char (point-min))
      (forward-line 14)
      (approve-feedback-toggle-threads)
      (expect (test-feedback--threads-shown 15) :to-be-truthy)
      (approve-feedback-toggle-threads)
      (expect (test-feedback--threads-shown 15) :to-be nil)
      ;; The resolved thread on removed lines is passed by; the edited
      ;; line's threads start with GitHub's text of it.
      (approve-feedback-next-thread)
      (expect (line-number-at-pos) :to-equal 61)
      (approve-feedback-next-thread)
      (expect (line-number-at-pos) :to-equal 131)
      (expect (test-feedback--threads-shown 131) :to-match "┊ on GitHub: line 131\n")
      (approve-feedback-previous-thread)
      (approve-feedback-previous-thread)
      (expect (line-number-at-pos) :to-equal 15)
      (approve-feedback-previous-thread)
      (expect (point) :to-equal (point-min))
      (expect (test-feedback--threads-shown 0) :to-match "On this file")
      (expect (approve-feedback-previous-thread) :to-throw 'user-error)
      (forward-line 1)
      (approve-feedback-toggle-all-threads)
      (expect (length (seq-filter #'test-feedback--threads-shown '(0 15 54 61 131 134)))
              :to-equal 6)
      (approve-feedback-toggle-all-threads)
      (expect (seq-filter #'test-feedback--threads-shown '(0 15 54 61 131 134)) :to-equal nil)
      (forward-line 2)
      (expect (approve-feedback-toggle-threads) :to-throw 'user-error)))

  (it "move with the text, and go to their lines anew once Emacs is idle"
    (test-feedback--with-one
      (test-feedback--start)
      (goto-char (point-min))
      (insert "one\ntwo\n")
      (expect (mapcar #'car (test-feedback--drawn)) :to-equal '(0 17 56 63 133 136))
      (test-feedback--idle)
      (approve-test-wait-for (lambda () (approve-lines-current-p
                                         (approve-lines-watched-map test-feedback--head-oid))))
      (expect (test-feedback--drawn)
              :to-equal '((0 (nil . file)) (17 (15 . same)) (56 (52 . removed))
                          (63 (nil . outdated)) (133 (131 . same)) (136 (134 . same))))))

  (it "put an outdated thread where its line is now, by its commit's text"
    ;; At the outdated thread's commit, the file lacked the head's first
    ;; ten lines: its line 61 is the head's line 71.
    (let ((test-feedback--older (mapconcat (lambda (n) (format "line %d\n" n))
                                           (number-sequence 11 600) "")))
      (test-feedback--with-one
        (test-feedback--start)
        (expect (assq 71 (test-feedback--drawn)) :to-equal '(71 (nil . outdated)))
        (let ((bubble (test-feedback--bubble 71)))
          (expect (get-text-property (1- (length bubble)) 'face bubble) :to-equal 'approve-dim)
          (expect (get-text-property 0 'help-echo bubble) :to-match "outdated"))
        (goto-char (point-min))
        (forward-line 70)
        (approve-feedback-toggle-threads)
        (expect (test-feedback--threads-shown 71)
                :to-match "┊ at [0-9a-f]\\{7\\}: line 71\n"))))

  (it "put a thread on removed lines under the line before them, with their text"
    (test-feedback--with-one
      (test-feedback--start)
      (let ((bubble (test-feedback--bubble 54)))
        (expect (substring-no-properties bubble) :to-equal "◂1")
        (expect (get-text-property (1- (length bubble)) 'face bubble) :to-equal 'approve-dim)
        (expect (get-text-property 0 'help-echo bubble) :to-match "removed after this one"))
      (goto-char (point-min))
      (forward-line 53)
      (approve-feedback-toggle-threads)
      (expect (test-feedback--threads-shown 54)
              :to-match "┊ removed: \t// TODO(52): drop this fallback\n")))

  (it "leave nothing behind when the mode is turned off"
    (test-feedback--with-one
      (let ((margin right-margin-width))
        (test-feedback--start)
        (approve-feedback-mode -1)
        (expect (approve-feedback--overlays) :to-equal nil)
        (expect right-margin-width :to-equal margin)
        (expect (local-variable-p 'right-margin-width) :to-be nil)
        (expect approve-lines--watches :to-equal nil))))

  (it "are not looked for in a file the pull request doesn't touch"
    (test-feedback--with-one
      (spy-on 'file-relative-name :and-return-value "docs/untouched.txt")
      (approve-feedback-mode 1)
      (test-feedback--wait)
      (expect test-feedback--blobs :to-equal nil)
      (expect approve-lines--watches :to-equal nil)
      (expect (approve-feedback--overlays) :to-equal nil)
      (expect (local-variable-p 'right-margin-width) :to-be nil))))

;;; test-approve-feedback.el ends here
