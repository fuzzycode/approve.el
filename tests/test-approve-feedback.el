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
  "Return a pull request of the branch: NUMBER in STATE, open by default.
The medium PR, #202, has its id in the store."
  `((__typename . "PullRequest")
    (id . ,(if (= number 202) "PR_kwDOPCDj7SnvM_RcFp" (format "PR_%d" number)))
    (number . ,number)
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

(defvar test-feedback--requests nil
  "The mutations sent to the fake GitHub, the last first.")

(defvar test-feedback--answer nil
  "A function of a mutation request that returns GitHub's data.")

(defun test-feedback--mutation-answer (request)
  "Answer the mutation REQUEST by `test-feedback--answer', as GitHub would."
  (push request test-feedback--requests)
  (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                            `((data . ,(funcall test-feedback--answer request)))
                            nil)))
    (lambda () (cancel-timer timer))))

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
               (cond ((equal (plist-get request :name) "queries/blob.graphql")
                      (test-feedback--blob-answer request))
                     ((string-prefix-p "mutations/" (or (plist-get request :name) ""))
                      (test-feedback--mutation-answer request))
                     (t (funcall replay request)))))
            (test-feedback--blobs nil)
            (test-feedback--requests nil)
            (test-feedback--answer nil))
       (make-directory (file-name-directory file) t)
       (with-temp-file file
         (insert test-feedback--text))
       (approve-file-clear-cache)
       (clrhash approve-clone--picked)
       (spy-on 'approve-clone-branch-head
               :and-return-value (list :clone test-feedback--root :branch "parser-rewrite"
                                       :remote "origin" :head test-feedback--head))
       (spy-on 'approve-clone-wait-pull-requests
               :and-call-fake (lambda (_head &optional _refresh) test-feedback--prs))
       (spy-on 'approve-clone-state :and-return-value '(:state ahead :ahead 1 :behind 0))
       (spy-on 'approve-clone-file-text :and-return-value nil)
       (let ((buffer (find-file-noselect file)))
         (unwind-protect
             (with-current-buffer buffer
               ,@body)
           (with-current-buffer buffer
             (set-buffer-modified-p nil))
           (when approve-feedback-global-mode
             (approve-feedback-global-mode -1))
           (dolist (input (buffer-list))
             (when (string-prefix-p "*approve-input" (buffer-name input))
               (with-current-buffer input
                 (setq approve-input--done t))
               (kill-buffer input)))
           (dolist (other (buffer-list))
             (when (and (buffer-file-name other)
                        (string-prefix-p test-feedback--root (buffer-file-name other)))
               (with-current-buffer other
                 (set-buffer-modified-p nil))
               (kill-buffer other)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (mapc #'kill-buffer (test-feedback--pr-buffers))
           (approve-file-clear-cache)
           (clrhash approve-clone--picked)
           (delete-directory test-feedback--root t))))))

(defun test-feedback--wait ()
  "Wait until the pull request of the current buffer and GitHub's text are in."
  (approve-test-wait-for (lambda () (and (approve-feedback--loaded-p)
                                         (not (approve-pr-loading-p)))))
  (with-current-buffer approve-model-store-buffer
    (approve-pr-redraw))
  (approve-test-wait-for (lambda () (not approve-feedback--fetching))))

(defun test-feedback--mode-line ()
  "Return the mode-line item of the current buffer, without its properties."
  (substring-no-properties approve-feedback--mode-line))

(defun test-feedback--tooltip ()
  "Return the tooltip of the mode-line item of the current buffer."
  (get-text-property 0 'help-echo approve-feedback--mode-line))

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
  "Turn the mode on, and wait until the threads are drawn.
That is once every text of GitHub's is compared with the buffer, an
outdated thread's commit's too."
  (approve-feedback-mode 1)
  (test-feedback--wait)
  (approve-test-wait-for (lambda () (and (approve-feedback--overlays)
                                         (approve-feedback--settled-p)))))

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
        (expect (test-feedback--mode-line) :to-match "… \\'")
        (test-feedback--wait)
        (expect approve-feedback--path :to-equal test-feedback--path)
        (expect (test-feedback--mode-line) :not :to-match "…")
        ;; The bubble and the file's six threads; the rest in the tooltip.
        (expect (test-feedback--mode-line) :to-match "[^0-9]6 \\'")
        ;; Half a space between the bubble and the count.
        (let ((count (string-search "6" approve-feedback--mode-line)))
          (expect (get-text-property (1- count) 'display approve-feedback--mode-line)
                  :to-equal '(space :width 0.5)))
        (let ((tooltip (test-feedback--tooltip)))
          (expect tooltip :to-match "acme-example/rocket#202")
          (expect tooltip :to-match "parser-rewrite into main · 1 of 2 open pull requests")
          (expect tooltip :to-match "1 commit not pushed")
          (expect tooltip :to-match (concat "6 threads in this file, 5 unresolved, "
                                            "1 asking for changes; "
                                            "15 unresolved in the pull request"))
          (expect tooltip :to-match "mouse-1, C-c ' \\?: Approve's menu\\'")
          (expect (get-text-property 0 'local-map approve-feedback--mode-line)
                  :to-be approve-feedback--mode-line-map))
        ;; In global-mode-string, where doom-modeline shows it too, in
        ;; the buffers where the mode is on; no lighter.
        (expect (member approve-feedback--mode-line-construct global-mode-string)
                :to-be-truthy)
        (expect approve-feedback--mode-line-construct
                :to-equal '(approve-feedback-mode ("" approve-feedback--mode-line)))
        (expect (get 'approve-feedback--mode-line 'risky-local-variable) :to-be t)
        (expect (cadr (assq 'approve-feedback-mode minor-mode-alist)) :to-be nil))))

  (it "shares the store of the PR buffer that is open"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (let ((pr (approve-test-open-pr "medium")))
          (approve-feedback-mode 1)
          (expect approve-model-store-buffer :to-be pr)
          (expect (test-feedback--pr-buffers) :to-equal (list pr))
          (expect (approve-pr-field 'number) :to-equal 202)
          (expect (test-feedback--tooltip) :not :to-match "open pull requests of this branch")))))

  (it "waits for the pull request's core, which can answer after its other queries"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (let* ((held nil)
               (answer approve-api-transport)
               (approve-api-transport
                (lambda (request)
                  (if (equal (plist-get request :name) "queries/pull-request.graphql")
                      (progn (push request held) #'ignore)
                    (funcall answer request)))))
          (approve-feedback-mode 1)
          (approve-test-wait-for (lambda () (approve-pr-loaded-p 'threads)))
          ;; The threads are in the store, the pull request's fields not.
          (expect (approve-model-root-ref) :to-be-truthy)
          (expect (approve-feedback--loaded-p) :to-be nil)
          (approve-feedback-redraw)
          (expect test-feedback--blobs :to-equal nil)
          (expect (test-feedback--mode-line) :to-match "… \\'")
          (expect (test-feedback--tooltip) :to-match "#202  Pull request 202 · open")
          (expect (approve-feedback--overlays) :to-equal nil)
          ;; Once it answers, the threads are drawn.
          (funcall answer (car held))
          (test-feedback--wait)
          (approve-test-wait-for (lambda () (approve-feedback--overlays)))
          (expect (mapcar #'car (test-feedback--drawn)) :to-equal '(0 15 54 61 131 134))))))

  (it "says a pull request without a state in its tooltip"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (expect (approve-feedback--tooltip '((number . 202) (title . "Parser")) nil t nil)
                :to-match "#202  Parser · \n"))))

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
        (expect (test-feedback--tooltip) :to-match "#202  Pull request 202 · merged"))))

  (it "says why it can't turn on, and stays off"
    (let ((test-feedback--prs nil))
      (test-feedback--with-clone
        (expect (approve-feedback-mode 1) :to-throw 'user-error '("No pull request for parser-rewrite"))
        ;; None was open: GitHub was asked again, as one may be now.
        (expect (spy-calls-all-args 'approve-clone-wait-pull-requests)
                :to-equal (list (list test-feedback--head) (list test-feedback--head t)))
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
        (expect approve-feedback--mode-line :to-be nil)
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
          (expect (test-feedback--tooltip) :to-match "#203 .*\n.*2 of 2 open pull requests"))))))

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
      ;; Before the first, the last again: the pull request's other
      ;; files with threads aren't in this clone.
      (approve-feedback-previous-thread)
      (expect (line-number-at-pos) :to-equal 134)
      (expect (test-feedback--threads-shown 134) :to-be-truthy)
      (goto-char (point-min))
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

(describe "The feedback mode, following GitHub and the clone"
  (it "says ↻ when a check finds changes on GitHub"
    (test-feedback--with-one
      (approve-feedback-mode 1)
      (test-feedback--wait)
      (expect (memq #'approve-changes--on-window window-selection-change-functions)
              :to-be-truthy)
      (with-current-buffer approve-model-store-buffer
        (approve-session-put :changes '(:branch t :commits 1 :comments 0 :reviews 0))
        (run-hooks 'approve-changes-hook))
      (expect (test-feedback--mode-line) :to-match "↻")
      (expect (test-feedback--tooltip)
              :to-match "Updated on GitHub: C-c ' g to see it")))

  (it "follows HEAD: on the same branch, to another, and off when it has none"
    (let ((test-feedback--prs (list (test-feedback--row 202) (test-feedback--row 203)))
          (head '("parser-rewrite" . "aaaa")))
      (test-feedback--with-clone
        (spy-on 'approve-clone-head :and-call-fake (lambda (_clone) head))
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (expect (memq #'approve-feedback--on-window window-selection-change-functions)
                :to-be-truthy)
        (let ((compared (spy-calls-count 'approve-clone-state)))
          ;; Nothing moved: nothing is asked.
          (approve-feedback--check-head (current-buffer))
          (expect (spy-calls-count 'approve-clone-state) :to-equal compared)
          ;; A commit: the clone is compared with GitHub's head again.
          (setq head '("parser-rewrite" . "bbbb"))
          (approve-feedback--check-head (current-buffer))
          (expect (spy-calls-count 'approve-clone-state) :to-equal (1+ compared)))
        ;; A checkout of a branch with another pull request.
        (let ((other (plist-put (copy-sequence test-feedback--head) :branch "other")))
          (spy-on 'approve-clone-branch-head
                  :and-return-value (list :clone test-feedback--root :branch "other"
                                          :remote "origin" :head other))
          (spy-on 'approve-clone-branch-pull-requests
                  :and-call-fake (lambda (_head &rest args)
                                   (funcall (plist-get args :callback)
                                            (list (test-feedback--row 203)))))
          (setq head '("other" . "cccc"))
          (approve-feedback--check-head (current-buffer))
          (expect (alist-get 'number approve-feedback--pull-request) :to-equal 203)
          ;; And one without.
          (spy-on 'approve-clone-branch-pull-requests
                  :and-call-fake (lambda (_head &rest args)
                                   (funcall (plist-get args :callback) nil)))
          (spy-on 'approve-clone-branch-head
                  :and-return-value (list :clone test-feedback--root :branch "third"
                                          :remote "origin"
                                          :head (plist-put (copy-sequence other)
                                                           :branch "third")))
          (setq head '("third" . "dddd"))
          (approve-feedback--check-head (current-buffer))
          (expect approve-feedback-mode :to-be nil)
          (expect (memq #'approve-feedback--on-window window-selection-change-functions)
                  :to-be nil))))))

(defun test-feedback--goto (line)
  "Move point to the start of the buffer's LINE, the region off."
  (deactivate-mark)
  (goto-char (point-min))
  (forward-line (1- line)))

(defconst test-feedback--fold-keys
  '(("z a" . approve-feedback-toggle-fold)
    ("z o" . approve-feedback-open-fold)
    ("z O" . approve-feedback-open-fold-recursively)
    ("z c" . approve-feedback-close-fold)
    ("z C" . approve-feedback-close-fold-recursively)
    ("z R" . approve-feedback-open-all-folds)
    ("z M" . approve-feedback-close-all-folds))
  "The mode's fold keys, as evil's normal state has them.")

(defmacro test-feedback--with-fold-keys (&rest body)
  "Run BODY with the mode's fold keys, and a folding of the buffer's own.
That folding is a local map, `map' in BODY, binding the mode's z
keys, zr and zm to a command that pushes its keys to `passed'."
  (declare (indent 0) (debug t))
  `(let ((map (make-sparse-keymap))
         (old (current-local-map))
         (passed nil))
     (dolist (key (append (mapcar #'car test-feedback--fold-keys) '("z r" "z m")))
       (keymap-set map key (lambda () (interactive) (push key passed))))
     (dolist (binding test-feedback--fold-keys)
       (keymap-set approve-feedback-mode-map (car binding) (cdr binding)))
     (unwind-protect
         (progn
           (use-local-map map)
           ,@body)
       (use-local-map old)
       (keymap-unset approve-feedback-mode-map "z" t))))

(defun test-feedback--press (keys)
  "Run what KEYS run in the current buffer, as the command loop would."
  (let* ((vector (kbd keys))
         (command (key-binding vector t)))
    (cl-letf (((symbol-function 'this-command-keys-vector) (lambda () vector)))
      (let ((this-command command))
        (call-interactively command)))))

(defun test-feedback--fold (from to &optional start-of-line)
  "Fold the lines after FROM to TO, as hideshow folds a block; return the fold.
The fold is an overlay that makes the text invisible from the end of
line FROM to the end of line TO, which isearch opens by deleting
it.  With START-OF-LINE, from the start of the line after FROM."
  (save-excursion
    (let* ((start (progn (test-feedback--goto (if start-of-line (1+ from) from))
                         (if start-of-line (point) (pos-eol))))
           (end (progn (test-feedback--goto to) (pos-eol)))
           (overlay (make-overlay start end)))
      (add-to-invisibility-spec '(test-fold . t))
      (overlay-put overlay 'invisible 'test-fold)
      (overlay-put overlay 'isearch-open-invisible #'delete-overlay)
      overlay)))

(defun test-feedback--bubble-on (line)
  "Return the bubble in the right margin of LINE, its own or a fold's, or nil."
  (save-excursion
    (test-feedback--goto line)
    (when-let* ((overlay (or (seq-find (lambda (overlay)
                                         (= (approve-feedback--line-of overlay) line))
                                       (approve-feedback--overlays))
                             (approve-feedback--carrier-at (point))))
                (before (overlay-get overlay 'before-string)))
      (cadr (get-text-property 0 'display before)))))

(defun test-feedback--after-folds ()
  "Return the threads shown after folds: ((LINE . TEXT) ...).
LINE is the line where the fold ends, which the screen line shows."
  (save-restriction
    (widen)
    (mapcar (lambda (overlay)
              (cons (line-number-at-pos (overlay-start overlay))
                    (substring-no-properties (or (overlay-get overlay 'before-string)
                                                 (overlay-get overlay 'after-string)))))
            (seq-filter (lambda (overlay) (and (overlay-get overlay 'approve-feedback-extra)
                                               (or (overlay-get overlay 'before-string)
                                                   (overlay-get overlay 'after-string))
                                               (not (overlay-get overlay
                                                                 'approve-feedback-carried))))
                        (overlays-in (point-min) (point-max))))))

(defun test-feedback--select (first last)
  "Select the buffer's lines FIRST to LAST, as a region over them.
The mark is active from then on in the buffer, as `transient-mark-mode' says."
  (setq-local transient-mark-mode t)
  (test-feedback--goto first)
  (push-mark (point) t t)
  (forward-line (- last first))
  (end-of-line))

(defun test-feedback--input ()
  "Return the open input buffer, or nil."
  (seq-find (lambda (buffer) (string-prefix-p "*approve-input" (buffer-name buffer)))
            (buffer-list)))

(defun test-feedback--send (text)
  "Write TEXT in the open input and send it; wait for GitHub.
The buffer the input was for is current after."
  (let ((source nil))
    (with-current-buffer (or (test-feedback--input) (error "No input buffer is open"))
      (setq source approve-input--source)
      (goto-char (point-max))
      (insert text)
      (approve-input-commit))
    (set-buffer source))
  (approve-test-wait-for (lambda () (null (approve-session-get :inflight)))))

(defun test-feedback--sent ()
  "Return the query file and the variables of the last mutation, sorted."
  (let ((request (car test-feedback--requests)))
    (list (plist-get request :name)
          (sort (copy-sequence (plist-get request :variables))
                (lambda (a b) (string< (car a) (car b)))))))

(defun test-feedback--dimmed ()
  "Return the keys of the dispatch dimmed at point, in order."
  (let ((suffixes nil))
    (cl-labels ((walk (x)
                  (cond ((and (consp x) (keywordp (car x)) (plist-member x :command))
                         (push x suffixes))
                        ((consp x) (walk (car x)) (walk (cdr x)))
                        ((vectorp x) (mapc #'walk x)))))
      (walk (get 'approve-feedback-dispatch 'transient--layout)))
    (delq nil (mapcar (lambda (suffix)
                        (when-let* ((inapt (plist-get suffix :inapt-if)))
                          (and (funcall inapt) (plist-get suffix :key))))
                      (nreverse suffixes)))))

(describe "What is at point in a clone's buffer"
  (it "is GitHub's line in the diff, or why GitHub takes no comment"
    (test-feedback--with-one
      (goto-char (point-min))
      (forward-line 130)
      (end-of-line)
      (insert " edited")
      (test-feedback--start)
      (test-feedback--goto 15)
      (expect (approve-context-at-point)
              :to-equal (list :kind 'line :path test-feedback--path :side "RIGHT" :line 15
                              :old nil :new 15 :line-kind 'add
                              :url (approve-context-file-url test-feedback--path "RIGHT" 15)
                              :threads '("PRRT_kwDO85-qIzFQYiTYBM")))
      (test-feedback--goto 5)
      (expect (approve-context-at-point)
              :to-equal
              (list :kind 'code :path test-feedback--path :name test-feedback--path
                    :side "RIGHT" :line 5
                    :why "Line 5 isn't in the pull request's diff; GitHub takes no comment there"
                    :url (concat "https://github.com/acme-example/rocket/blob/"
                                 test-feedback--head-oid "/internal/parser/parser.go#L5")
                    :threads nil))
      (expect (approve-action-why-not 'approve-comment)
              :to-equal "Line 5 isn't in the pull request's diff; GitHub takes no comment there")
      (test-feedback--goto 131)
      (expect (approve-action-why-not 'approve-suggest)
              :to-equal "Line 131 was edited here; GitHub takes comments only on its own lines")
      ;; A region over lines of one hunk, and one that takes in the edited line.
      (test-feedback--select 13 14)
      (let ((context (approve-context-at-point)))
        (expect (list (plist-get context :kind) (plist-get context :start-line)
                      (plist-get context :line))
                :to-equal '(lines 13 14)))
      (test-feedback--select 129 131)
      (expect (plist-get (approve-context-at-point) :why)
              :to-equal "Line 131 was edited here; GitHub takes comments only on its own lines")
      ;; A line added just now: the lines are compared again at once.
      (test-feedback--goto 1)
      (insert "new first line\n")
      (test-feedback--goto 1)
      (expect (plist-get (approve-context-at-point) :why)
              :to-equal "Line 1 was added here; GitHub takes comments only on its own lines")
      (test-feedback--goto 16)
      (expect (plist-get (approve-context-at-point) :line) :to-equal 15)))

  (it "names the threads of the line, those above the file on the first line"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--goto 1)
      (expect (plist-get (approve-context-at-point) :threads)
              :to-equal '("PRRT_kwDOmVrZbzhFWoiCSv"))
      (test-feedback--goto 134)
      (expect (plist-get (approve-context-at-point) :threads)
              :to-equal '("PRRT_kwDOVCEPcVKg5CpjSd")))))

(describe "The feedback mode's actions"
  (it "comment on GitHub's line, highlighted while the comment is written"
    (test-feedback--with-one
      (setq test-feedback--answer
            (lambda (_)
              `((addPullRequestReviewThread
                 (thread (__typename . "PullRequestReviewThread") (id . "PRRT_new")
                         (isResolved) (viewerCanReply . t) (path . ,test-feedback--path)
                         (line . 14) (diffSide . "RIGHT") (subjectType . "LINE")
                         (comments (totalCount . 1) (pageInfo (hasNextPage) (endCursor . "x"))
                                   (nodes ((__typename . "PullRequestReviewComment")
                                           (id . "PRRC_new") (state . "PENDING")
                                           (body . "New") (bodyHTML . "<p>New</p>")
                                           (author (__typename . "User") (id . "U_vera")
                                                   (login . "vera-example"))))))))))
      (test-feedback--start)
      (test-feedback--goto 14)
      (let ((clone (current-buffer)))
        (call-interactively #'approve-comment)
        (expect (with-current-buffer (test-feedback--input)
                  (substring-no-properties (eval (cadr header-line-format) t)))
                :to-match "Comment on internal/parser/parser.go:14 · pending review")
        (with-current-buffer clone
          (expect (seq-find (lambda (overlay) (eq (overlay-get overlay 'face) 'approve-target))
                            (overlays-at (point)))
                  :to-be-truthy)))
      (test-feedback--send "Can this fail?")
      (expect (test-feedback--sent)
              :to-equal `("mutations/add-pull-request-review-thread.graphql"
                          ((body . "Can this fail?") (line . 14) (path . ,test-feedback--path)
                           (pullRequestId . "PR_kwDOPCDj7SnvM_RcFp") (side . "RIGHT")
                           (subjectType . "LINE"))))
      ;; The PR buffer redrew, and this buffer with it: the new thread
      ;; is under its line, and the line is no longer highlighted.
      (expect (assq 14 (test-feedback--drawn)) :to-equal '(14 (14 . same)))
      (expect (seq-find (lambda (overlay) (eq (overlay-get overlay 'face) 'approve-target))
                        (overlays-at (point)))
              :to-be nil)))

  (it "reply to, resolve and edit the threads of the line at point"
    (test-feedback--with-one
      (setq test-feedback--answer
            (lambda (request)
              (pcase (plist-get request :name)
                ("mutations/add-pull-request-review-thread-reply.graphql"
                 '((addPullRequestReviewThreadReply
                    (comment (__typename . "PullRequestReviewComment") (id . "PRRC_reply")
                             (state . "PENDING") (body . "Done") (bodyHTML . "<p>Done</p>")))))
                ("mutations/resolve-review-thread.graphql"
                 '((resolveReviewThread
                    (thread (__typename . "PullRequestReviewThread")
                            (id . "PRRT_kwDOmVrZbzhFWoiCSv")
                            (isResolved . t) (viewerCanResolve) (viewerCanUnresolve . t))))))))
      (test-feedback--start)
      (test-feedback--goto 5)
      (expect (approve-action-why-not 'approve-reply) :to-equal "No threads on this line")
      (test-feedback--goto 15)
      (call-interactively #'approve-reply)
      (expect (with-current-buffer (test-feedback--input)
                (substring-no-properties (eval (cadr header-line-format) t)))
              :to-match "Reply to @carol-example on internal/parser/parser.go:15")
      (test-feedback--send "Done")
      (expect (test-feedback--sent)
              :to-equal '("mutations/add-pull-request-review-thread-reply.graphql"
                          ((body . "Done") (threadId . "PRRT_kwDO85-qIzFQYiTYBM"))))
      ;; The file's thread, from the first line.
      (test-feedback--goto 1)
      (call-interactively #'approve-resolve)
      (approve-test-wait-for (lambda () (null (approve-session-get :inflight))))
      (expect (test-feedback--sent)
              :to-equal '("mutations/resolve-review-thread.graphql"
                          ((threadId . "PRRT_kwDOmVrZbzhFWoiCSv"))))
      (expect (test-feedback--tooltip) :to-match "6 threads in this file, 4 unresolved")
      ;; The viewer's pending comment, the only one to edit there.
      (test-feedback--goto 134)
      (let ((clone (current-buffer)))
        (call-interactively #'approve-edit)
        (set-buffer clone))
      (expect (with-current-buffer (test-feedback--input) (buffer-string))
              :to-match "\\`Is this branch reachable after the lexer change\\?"))))

(describe "The feedback mode's keys"
  (it "reach every command of its map from its dispatch, with the same keys"
    (let ((suffixes nil)
          (bound nil))
      (cl-labels ((walk (x)
                    (cond ((and (consp x) (keywordp (car x)) (plist-member x :command))
                           (push x suffixes))
                          ((consp x) (walk (car x)) (walk (cdr x)))
                          ((vectorp x) (mapc #'walk x)))))
        (walk (get 'approve-feedback-dispatch 'transient--layout)))
      (map-keymap (lambda (_event binding) (push binding bound))
                  approve-feedback-command-map)
      (dolist (suffix suffixes)
        ;; U, the fetch into the clones, is in the dispatch alone (D53).
        (expect (keymap-lookup approve-feedback-command-map (plist-get suffix :key))
                :to-be (unless (eq (plist-get suffix :command) 'approve-fetch-pull-request)
                         (plist-get suffix :command))))
      (expect (seq-difference (delete-dups bound)
                              (cons 'approve-feedback-dispatch
                                    (mapcar (lambda (suffix) (plist-get suffix :command))
                                            suffixes)))
              :to-equal nil)
      (expect (keymap-lookup approve-feedback-command-map "?")
              :to-be 'approve-feedback-dispatch)))

  (it "are under the prefix key, C-c ' by default, which can be changed"
    (expect (keymap-lookup approve-feedback-mode-map "C-c ' TAB")
            :to-be 'approve-feedback-toggle-threads)
    (expect (keymap-lookup approve-feedback-mode-map "C-c ' ?")
            :to-be 'approve-feedback-dispatch)
    (unwind-protect
        (progn
          (customize-set-variable 'approve-feedback-prefix-key "C-c a")
          (expect (keymap-lookup approve-feedback-mode-map "C-c '") :to-be nil)
          (expect (keymap-lookup approve-feedback-mode-map "C-c a r") :to-be 'approve-reply)
          (customize-set-variable 'approve-feedback-prefix-key nil)
          (expect (keymap-lookup approve-feedback-mode-map "C-c a") :to-be nil))
      (customize-set-variable 'approve-feedback-prefix-key "C-c '"))
    (expect (keymap-lookup approve-feedback-mode-map "C-c ' r") :to-be 'approve-reply))

  (it "bind evil's ]t, [t and fold keys as the mode's own, in normal state"
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-minor-mode-key)
                 (lambda (state mode key def)
                   (push (list state mode (key-description key) def) bound)))
                ((symbol-function 'evil-define-key*) #'ignore)
                ((symbol-function 'evil-set-initial-state) #'ignore)
                ((symbol-function 'evil-add-command-properties) #'ignore))
        (require 'approve-evil)
        (approve-evil-setup-feedback))
      (expect (reverse bound)
              :to-equal '((normal approve-feedback-mode "] t" approve-feedback-next-thread)
                          (normal approve-feedback-mode "[ t" approve-feedback-previous-thread)
                          (normal approve-feedback-mode "z a" approve-feedback-toggle-fold)
                          (normal approve-feedback-mode "z o" approve-feedback-open-fold)
                          (normal approve-feedback-mode "z O"
                                  approve-feedback-open-fold-recursively)
                          (normal approve-feedback-mode "z c" approve-feedback-close-fold)
                          (normal approve-feedback-mode "z C"
                                  approve-feedback-close-fold-recursively)
                          (normal approve-feedback-mode "z R" approve-feedback-open-all-folds)
                          (normal approve-feedback-mode "z M" approve-feedback-close-all-folds)))))

  (it "fold the line's threads as its innermost fold, else pass the key on"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--with-fold-keys
        ;; za: the line's threads, and only them.
        (test-feedback--goto 15)
        (test-feedback--press "z a")
        (expect (test-feedback--threads-shown 15) :to-be-truthy)
        (expect passed :to-equal nil)
        ;; zc folds them, then the code: they are folded already.
        (test-feedback--press "z c")
        (expect (test-feedback--threads-shown 15) :to-be nil)
        (expect passed :to-equal nil)
        (test-feedback--press "z c")
        (expect passed :to-equal '("z c"))
        ;; zo unfolds them, then the code.
        (test-feedback--press "z o")
        (expect (test-feedback--threads-shown 15) :to-be-truthy)
        (expect passed :to-equal '("z c"))
        (test-feedback--press "z o")
        (expect passed :to-equal '("z o" "z c"))
        ;; zC and zO do both at once.
        (test-feedback--press "z C")
        (expect (test-feedback--threads-shown 15) :to-be nil)
        (expect (car passed) :to-equal "z C")
        (test-feedback--press "z O")
        (expect (test-feedback--threads-shown 15) :to-be-truthy)
        (expect (car passed) :to-equal "z O")
        ;; A line without threads: the key does what it does without the mode.
        (setq passed nil)
        (test-feedback--goto 5)
        (test-feedback--press "z a")
        (expect passed :to-equal '("z a"))
        ;; zR and zM: every thread, and nothing else.
        (test-feedback--press "z R")
        (expect (length (seq-filter #'test-feedback--threads-shown
                                    '(0 15 54 61 131 134)))
                :to-equal 6)
        (test-feedback--press "z M")
        (expect (seq-filter #'test-feedback--threads-shown '(0 15 54 61 131 134))
                :to-equal nil)
        (expect passed :to-equal '("z a"))
        ;; In a file without threads, they pass on.
        (let ((approve-feedback--drawn nil))
          (test-feedback--press "z R"))
        (expect passed :to-equal '("z R" "z a")))))

  (it "drop the error of the key passed on when the threads were folded"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--with-fold-keys
        (keymap-set map "z O" (lambda () (interactive) (user-error "No fold here")))
        (keymap-set map "z o" (lambda () (interactive) (user-error "No fold here")))
        (test-feedback--goto 15)
        (test-feedback--press "z O")
        (expect (test-feedback--threads-shown 15) :to-be-truthy)
        ;; With nothing done to the threads, the error is the key's.
        (expect (test-feedback--press "z o") :to-throw 'user-error '("No fold here")))))

  (it "pass on only keys of the mode's, never M-x"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--with-fold-keys
        (spy-on 'message)
        (spy-on 'this-command-keys-vector :and-return-value (kbd "M-x"))
        (spy-on 'execute-extended-command)
        (test-feedback--goto 5)
        (let ((this-command 'approve-feedback-toggle-fold))
          (call-interactively #'approve-feedback-toggle-fold))
        (expect 'execute-extended-command :not :to-have-been-called)
        (expect 'message :to-have-been-called-with "%s" "No threads on this line"))))

  (it "say so where there are no threads and nothing else folds"
    (test-feedback--with-one
      (test-feedback--start)
      (spy-on 'this-command-keys-vector :and-return-value [?z ?a])
      (spy-on 'message)
      (test-feedback--goto 5)
      (approve-feedback-toggle-fold)
      (expect 'message :to-have-been-called-with "%s" "No threads on this line")
      ;; Threads that are as the key wants them say so.
      (test-feedback--goto 15)
      (approve-feedback-close-fold)
      (expect 'message :to-have-been-called-with
              "%s" "Threads on this line already folded")))

  (it "fold threads for the actions the fold keys name"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--goto 15)
      (expect (approve-feedback-fold-threads :open) :to-be t)
      (expect (test-feedback--threads-shown 15) :to-be-truthy)
      ;; Unfolded already: nothing to do.
      (expect (approve-feedback-fold-threads :open) :to-be nil)
      (expect (approve-feedback-fold-threads :close) :to-be t)
      (expect (test-feedback--threads-shown 15) :to-be nil)
      (expect (approve-feedback-fold-threads :close) :to-be nil)
      (test-feedback--goto 5)
      (expect (approve-feedback-fold-threads :toggle) :to-be nil)
      (expect (approve-feedback-fold-threads :open-all) :to-be t)
      (expect (length (seq-filter #'test-feedback--threads-shown '(0 15 54 61 131 134)))
              :to-equal 6)))

  (it "dim in the dispatch what can't run on the line at point"
    (test-feedback--with-one
      (test-feedback--start)
      (expect (substring-no-properties (approve-feedback--dispatch-heading))
              :to-equal "#202 Rewrite the parser and engine scheduler · ↑1")
      (test-feedback--goto 5)
      (expect (test-feedback--dimmed) :to-equal '("TAB" "r" "R" "e" "k" "+" "c" "s"))
      (test-feedback--goto 15)
      (expect (test-feedback--dimmed) :to-equal '("e"))))

  (it "open the dispatch only where the mode is on"
    (with-temp-buffer
      (expect (approve-feedback-dispatch) :to-throw 'user-error))))

(defun test-feedback--pr-goto-line (number)
  "Move point to the added or context line NUMBER of the file in the PR buffer."
  (approve-nav-goto-file test-feedback--path)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((line (get-text-property (point) 'approve-line)))
        (if (and line (equal (plist-get line :path) test-feedback--path)
                 (equal (plist-get line :side) "RIGHT") (eql (plist-get line :new) number))
            (setq found (point))
          (forward-line 1))))
    (or found (error "No line %s of %s" number test-feedback--path))))

(defun test-feedback--edit ()
  "Edit the buffer without saving: a line added at the top, and line 131."
  (goto-char (point-min))
  (insert "new first line\n")
  (forward-line 130)
  (end-of-line)
  (insert " edited"))

(defun test-feedback--add-file (path)
  "Write PATH in the clone, with GitHub's text of the medium PR's file."
  (let ((file (expand-file-name path test-feedback--root)))
    (make-directory (file-name-directory file) t)
    (write-region test-feedback--text nil file nil 'silent)
    file))

(describe "The feedback mode and the buffer's own folds"
  (it "count the threads of the lines a fold hides on the line that shows it"
    (test-feedback--with-one
      (test-feedback--start)
      (approve-feedback-fold-threads :open-all)
      (let ((fold (test-feedback--fold 50 62)))
        (approve-feedback--follow-folds)
        ;; Lines 54 and 61 show neither bubble nor threads...
        (dolist (line '(54 61))
          (let ((overlay (approve-feedback--overlay-of-line line)))
            (expect (overlay-get overlay 'before-string) :to-be nil)
            (expect (overlay-get overlay 'after-string) :to-be nil)))
        (expect (test-feedback--after-folds) :to-equal nil)
        ;; ... line 50, which shows the fold, has their bubble: one is
        ;; unresolved.
        (let ((bubble (test-feedback--bubble-on 50)))
          (expect (substring-no-properties bubble) :to-match "[^0-9]1\\'")
          (expect (get-text-property 0 'help-echo bubble)
                  :to-equal (concat "2 threads in the folded code, on lines 54, 61\n"
                                    "mouse-1: unfold the code")))
        (expect right-margin-width :to-equal 3)
        ;; Once the fold opens, they are as they were.
        (delete-overlay fold)
        (approve-feedback--follow-folds)
        (expect (test-feedback--bubble-on 50) :to-be nil)
        (expect (test-feedback--threads-shown 54) :to-be-truthy)
        (expect (test-feedback--threads-shown 61) :to-be-truthy))))

  (it "count a fold's first line's threads with those it hides"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--fold 131 140)
      (approve-feedback--follow-folds)
      (let ((bubble (test-feedback--bubble-on 131)))
        (expect (substring-no-properties bubble) :to-match "[^0-9]2\\'")
        (expect (get-text-property 0 'help-echo bubble)
                :to-equal (concat "1 thread · 1 more thread in the folded code, on line 134\n"
                                  "mouse-1: unfold")))
      (expect (approve-feedback--carrier-at (point)) :to-be nil)))

  (it "show a fold's first line's threads after the fold, as its screen line ends"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--goto 15)
      (approve-feedback-fold-threads :open)
      (let ((fold (test-feedback--fold 15 20)))
        (approve-feedback--follow-folds)
        (expect (overlay-get (approve-feedback--overlay-of-line 15) 'after-string) :to-be nil)
        (expect (test-feedback--bubble-on 15) :to-be-truthy)
        (expect (mapcar #'car (test-feedback--after-folds)) :to-equal '(20))
        (expect (cdr (car (test-feedback--after-folds))) :to-match "\\` \n.*╭")
        ;; Its screen lines take no line number: they start on line 20.
        (test-feedback--goto 20)
        (expect (get-char-property (pos-eol) 'display-line-numbers-disable) :to-be t)
        ;; Folded with the line before, it shows nothing.
        (delete-overlay fold)
        (test-feedback--fold 14 20)
        (approve-feedback--follow-folds)
        (expect (test-feedback--after-folds) :to-equal nil)
        (expect (overlay-get (approve-feedback--overlay-of-line 15) 'after-string) :to-be nil)
        (expect (test-feedback--bubble-on 14) :to-be-truthy))))

  (it "put the bubble of a fold that starts a line on the line before it"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--fold 50 62 t)
      (approve-feedback--follow-folds)
      (expect (test-feedback--bubble-on 50) :to-be-truthy)
      (expect (get-text-property 0 'help-echo (test-feedback--bubble-on 50))
              :to-match "\\`2 threads in the folded code")))

  (it "keep it when the threads are drawn again"
    (test-feedback--with-one
      (test-feedback--start)
      (test-feedback--fold 50 62)
      (approve-feedback-redraw)
      (expect (test-feedback--bubble-on 50) :to-be-truthy)
      (expect (overlay-get (approve-feedback--overlay-of-line 54) 'before-string) :to-be nil)))

  (it "paint the threads again only when the folds change"
    (test-feedback--with-one
      (test-feedback--start)
      (expect (memq #'approve-feedback--follow-folds post-command-hook) :to-be-truthy)
      (spy-on 'approve-feedback--paint-all :and-call-through)
      (approve-feedback--follow-folds)
      (expect 'approve-feedback--paint-all :not :to-have-been-called)
      (let ((fold (test-feedback--fold 50 62)))
        (approve-feedback--follow-folds)
        (expect 'approve-feedback--paint-all :to-have-been-called-times 1)
        ;; Text that moves the folds and the threads alike changes nothing.
        (goto-char (point-min))
        (insert "one\ntwo\n")
        (approve-feedback--follow-folds)
        (expect 'approve-feedback--paint-all :to-have-been-called-times 1)
        (expect (test-feedback--bubble-on 52) :to-be-truthy)
        (delete-overlay fold)
        (approve-feedback--follow-folds)
        (expect 'approve-feedback--paint-all :to-have-been-called-times 2))
      (approve-feedback-mode -1)
      (expect (memq #'approve-feedback--follow-folds post-command-hook) :to-be nil)))

  (it "open the folds that hide a thread it goes to, as isearch does"
    (test-feedback--with-one
      (test-feedback--start)
      ;; Outline's folds: the lines after each tenth are its body.
      (setq-local outline-regexp "line [0-9]*0$")
      (outline-minor-mode 1)
      (outline-hide-body)
      (approve-feedback--follow-folds)
      (expect (get-text-property 0 'help-echo (test-feedback--bubble-on 130))
              :to-match "\\`2 threads in the folded code, on lines 131, 134")
      (test-feedback--goto 100)
      (approve-feedback-next-thread)
      (expect (line-number-at-pos) :to-equal 131)
      (expect (invisible-p (point)) :to-be nil)
      (expect (test-feedback--threads-shown 131) :to-be-truthy)
      ;; The line 130 shows its lines again: no bubble of hidden threads.
      (expect (test-feedback--bubble-on 130) :to-be nil)
      ;; Line 61's fold is still closed.
      (expect (overlay-get (approve-feedback--overlay-of-line 61) 'before-string) :to-be nil)
      (outline-minor-mode -1)))

  (it "open the folds of a fold's bubble with TAB"
    (test-feedback--with-one
      (test-feedback--start)
      (let ((fold (test-feedback--fold 50 62)))
        (approve-feedback--follow-folds)
        (test-feedback--goto 50)
        (expect (approve-action-why-not 'approve-feedback-toggle-threads) :to-be nil)
        (approve-feedback-toggle-threads)
        (expect (overlay-buffer fold) :to-be nil)
        (expect (test-feedback--bubble-on 50) :to-be nil)
        (expect (test-feedback--bubble-on 54) :to-be-truthy)))))

(describe "f in a PR buffer"
  (it "opens the line at point in the clone, where the buffer has it now"
    (test-feedback--with-one
      (spy-on 'approve-clone-file-text :and-return-value test-feedback--text)
      (spy-on 'approve-clone-pull-request-worktree :and-return-value test-feedback--root)
      (spy-on 'message :and-call-through)
      (test-feedback--edit)
      (let ((clone (current-buffer))
            (pr (approve-test-open-pr "medium")))
        (with-current-buffer pr
          (test-feedback--pr-goto-line 15)
          (approve-feedback-visit-file))
        (expect (spy-calls-args-for 'approve-clone-pull-request-worktree 0)
                :to-equal '((:host "github.com" :owner "acme-example" :repo "rocket"
                                   :branch "parser-rewrite")
                            (:host "github.com" :owner "acme-example" :repo "rocket")))
        (expect (window-buffer (selected-window)) :to-be clone)
        (expect approve-feedback-mode :to-be t)
        (expect approve-model-store-buffer :to-be pr)
        (expect approve-feedback--destination :to-be nil)
        (expect (line-number-at-pos) :to-equal 16)
        (expect (test-feedback--threads-shown 16) :to-be-truthy)
        ;; A line edited here: the line in its place, and the echo area
        ;; says so.
        (with-current-buffer pr
          (test-feedback--pr-goto-line 131)
          (approve-feedback-visit-file))
        (expect (line-number-at-pos) :to-equal 132)
        (expect (test-feedback--threads-shown 132) :to-match "┊ on GitHub: line 131")
        (expect (spy-calls-args-for 'message (1- (spy-calls-count 'message)))
                :to-equal '("Approve: %s" "GitHub's line 131 was edited here; this line is in its place")))))

  (it "goes on to the line once GitHub's text is in, unless point moved"
    (test-feedback--with-one
      (spy-on 'approve-clone-pull-request-worktree :and-return-value test-feedback--root)
      (test-feedback--edit)
      (let ((pr (approve-test-open-pr "medium")))
        (with-current-buffer pr
          (test-feedback--pr-goto-line 15)
          (approve-feedback-visit-file))
        ;; GitHub's line meanwhile, as the clone hasn't GitHub's head.
        (expect (line-number-at-pos) :to-equal 15)
        (expect approve-feedback--destination :to-be-truthy)
        (approve-test-wait-for (lambda () (null approve-feedback--destination)))
        (expect (line-number-at-pos) :to-equal 16)
        (expect (test-feedback--threads-shown 16) :to-be-truthy))))

  (it "opens a thread where it is drawn, saying why it isn't on its line"
    (test-feedback--with-one
      (spy-on 'approve-clone-file-text :and-return-value test-feedback--text)
      (spy-on 'approve-clone-pull-request-worktree :and-return-value test-feedback--root)
      (spy-on 'message :and-call-through)
      (let ((pr (approve-test-open-pr "medium")))
        (with-current-buffer pr
          (approve-nav-goto-file test-feedback--path)
          (goto-char (oref (approve-test-find-section 'approve-thread-section
                                                      "PRRT_kwDOBjizZBfYz5abyp")
                           start))
          (approve-feedback-visit-file))
        (expect (line-number-at-pos) :to-equal 61)
        (expect (test-feedback--threads-shown 61) :to-match "┊ at [0-9a-f]\\{7\\}: line 61")
        (expect (spy-calls-args-for 'message (1- (spy-calls-count 'message)))
                :to-equal '("Approve: %s" "The thread is outdated: its line was here")))))

  (it "shows the pull request it comes from, as if picked for the branch"
    (let ((test-feedback--prs (list (test-feedback--row 203) (test-feedback--row 202))))
      (test-feedback--with-clone
        (spy-on 'approve-clone-pull-request-worktree :and-return-value test-feedback--root)
        (approve-feedback-mode 1)
        (expect (alist-get 'number approve-feedback--pull-request) :to-equal 203)
        (let ((pr (approve-test-open-pr "medium")))
          (with-current-buffer pr
            (test-feedback--pr-goto-line 15)
            (approve-feedback-visit-file))
          (expect (alist-get 'number approve-feedback--pull-request) :to-equal 202)
          (expect approve-model-store-buffer :to-be pr)
          (expect (alist-get 'number (approve-clone-shown-pull-request test-feedback--head
                                                                       test-feedback--prs))
                  :to-equal 202)))))

  (it "offers the file view when no clone has the branch, or the file"
    (test-feedback--with-one
      (spy-on 'approve-clone-pull-request-worktree :and-return-value nil)
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'approve-file-show)
      (let ((pr (approve-test-open-pr "medium"))
            (empty (file-name-as-directory (make-temp-file "approve-empty-" t))))
        (unwind-protect
            (with-current-buffer pr
              (test-feedback--pr-goto-line 15)
              (approve-feedback-visit-file)
              (expect (spy-calls-args-for 'y-or-n-p 0)
                      :to-equal '("No clone has parser-rewrite checked out; show the file view instead? "))
              (expect 'approve-file-show
                      :to-have-been-called-with test-feedback--path "RIGHT" 15)
              (spy-on 'approve-clone-pull-request-worktree :and-return-value empty)
              (spy-on 'y-or-n-p :and-return-value nil)
              (approve-feedback-visit-file)
              (expect (car (spy-calls-args-for 'y-or-n-p 0))
                      :to-equal (format "%s isn't in %s (deleted or moved there); show the file view instead? "
                                        test-feedback--path (abbreviate-file-name empty)))
              (expect 'approve-file-show :to-have-been-called-times 1))
          (delete-directory empty t)))
      ;; A file view only says why.
      (with-temp-buffer
        (setq-local approve-context-file (list :path test-feedback--path))
        (expect (approve-feedback--file-view-instead test-feedback--path nil "No clone")
                :to-throw 'user-error '("No clone")))))

  (it "is in the dispatches of the PR buffer and the file view, dimmed off a file"
    (test-feedback--with-one
      (let ((pr (approve-test-open-pr "medium")))
        (expect (keymap-lookup approve-pr-mode-map "f") :to-be 'approve-feedback-visit-file)
        (expect (keymap-lookup approve-file-mode-map "f") :to-be 'approve-feedback-visit-file)
        (dolist (prefix '(approve-dispatch approve-file-dispatch))
          (expect (plist-get (cdr (transient-get-suffix prefix "f")) :command)
                  :to-be 'approve-feedback-visit-file))
        (with-current-buffer pr
          (goto-char (oref (approve-test-find-section 'approve-description-section) start))
          (expect (approve-action-why-not 'approve-feedback-visit-file)
                  :to-equal "Put point on a file, a line of the diff or a thread")
          (approve-nav-goto-file test-feedback--path)
          (expect (approve-feedback--visit-preflight)
                  :to-equal (list test-feedback--path :side "RIGHT" :line 13))))))

  (it "is gF in evil's normal state"
    (cl-letf (((symbol-function 'evil-define-key*) #'ignore)
              ((symbol-function 'evil-set-initial-state) #'ignore)
              ((symbol-function 'evil-add-command-properties) #'ignore))
      (require 'approve-evil))
    (expect (assoc "gF" approve-evil-pr-bindings)
            :to-equal '("gF" . approve-feedback-visit-file))
    (expect (assoc "gF" approve-evil-file-bindings)
            :to-equal '("gF" . approve-feedback-visit-file))))

(defun test-feedback--messages ()
  "Return what `message', a spy, said, in order."
  (mapcar (lambda (args) (apply #'format-message args)) (spy-calls-all-args 'message)))

(describe "approve-fetch-pull-request"
  (it "fetches into the clones of the base repository, then compares them again"
    (test-feedback--with-one
      (approve-feedback-mode 1)
      (test-feedback--wait)
      (let ((compared (spy-calls-count 'approve-clone-state))
            (callback nil))
        (spy-on 'approve-clone-fetch-pull-request
                :and-call-fake (lambda (_repository _number done)
                                 (setq callback done)
                                 '("/src/rocket/" "/work/rocket/")))
        (spy-on 'message)
        ;; From the clone's buffer, which shares the PR buffer's store.
        (expect (approve-fetch-pull-request) :to-equal '("/src/rocket/" "/work/rocket/"))
        (expect (butlast (spy-calls-args-for 'approve-clone-fetch-pull-request 0))
                :to-equal '((:host "github.com" :owner "acme-example" :repo "rocket") 202))
        (expect (test-feedback--messages)
                :to-equal '("Fetching #202 into /src/rocket and /work/rocket..."))
        (funcall callback '(("/src/rocket/")
                            ("/work/rocket/" . "couldn't find remote ref refs/pull/202/head")))
        (expect (car (last (test-feedback--messages)))
                :to-equal (concat "Fetched #202 into /src/rocket, as refs/pullreqs/202; "
                                  "not into /work/rocket: couldn't find remote ref refs/pull/202/head"))
        ;; The clone may have GitHub's head now.
        (expect (spy-calls-count 'approve-clone-state) :to-equal (1+ compared))
        (funcall callback '(("/src/rocket/" . "Could not resolve host: github.com")))
        (expect (car (last (test-feedback--messages)))
                :to-equal "Could not fetch #202 into /src/rocket: Could not resolve host: github.com")
        (expect (spy-calls-count 'approve-clone-state) :to-equal (1+ compared)))))

  (it "says so where there is no clone, and fetches nothing"
    (approve-test-with-fixture "medium"
      (let ((pr (approve-test-open-pr "medium")))
        (unwind-protect
            (progn
              (spy-on 'approve-clone-fetch-pull-request :and-return-value nil)
              (spy-on 'message)
              (with-current-buffer pr
                (expect (approve-fetch-pull-request) :to-be nil))
              (expect (test-feedback--messages)
                      :to-equal '("No clone of acme-example/rocket in approve-clone-directories: nothing fetched")))
          (kill-buffer pr))))
    (with-temp-buffer
      (expect (approve-fetch-pull-request) :to-throw 'user-error)))

  (it "is U in the dispatches, with no key of its own"
    (dolist (prefix '(approve-dispatch approve-file-dispatch approve-feedback-dispatch))
      (expect (plist-get (cdr (transient-get-suffix prefix "U")) :command)
              :to-be 'approve-fetch-pull-request))
    (dolist (map (list approve-pr-mode-map approve-file-mode-map approve-feedback-command-map))
      (expect (where-is-internal 'approve-fetch-pull-request map) :to-be nil))))

(describe "RET in the feedback mode's map"
  (it "shows the PR buffer at the line, else its thread, else the file"
    (test-feedback--with-one
      (test-feedback--start)
      (let ((owner approve-model-store-buffer)
            (clone (current-buffer)))
        (expect (keymap-lookup approve-feedback-command-map "RET")
                :to-be 'approve-feedback-visit-pr)
        (test-feedback--goto 15)
        (approve-feedback-visit-pr)
        (expect (window-buffer (selected-window)) :to-be owner)
        (with-current-buffer owner
          (expect (plist-get (get-text-property (point) 'approve-line) :new) :to-equal 15))
        ;; The outdated thread drawn under line 61, which isn't in the diff.
        (set-buffer clone)
        (test-feedback--goto 61)
        (approve-feedback-visit-pr)
        (with-current-buffer owner
          (let ((section (magit-current-section)))
            (expect (oref section type) :to-be 'approve-thread-section)
            (expect (oref section value) :to-equal "PRRT_kwDOBjizZBfYz5abyp")))
        ;; A line with neither.
        (set-buffer clone)
        (test-feedback--goto 5)
        (approve-feedback-visit-pr)
        (with-current-buffer owner
          (let ((section (magit-current-section)))
            (expect (oref section type) :to-be 'approve-file-section)
            (expect (oref section value) :to-equal test-feedback--path)))))))

(describe "n and p in the feedback mode's map"
  (it "go on to the pull request's next and previous files with threads"
    (test-feedback--with-one
      (let ((migrations (test-feedback--add-file "internal/store/migrations.go"))
            (lexer (test-feedback--add-file "internal/parser/lexer.go")))
        (test-feedback--start)
        (test-feedback--goto 134)
        ;; Past the last thread of parser.go: migrations.go, its thread on
        ;; the whole file.
        (approve-feedback-next-thread)
        (expect (buffer-file-name (window-buffer (selected-window))) :to-equal migrations)
        (with-current-buffer (window-buffer (selected-window))
          (expect approve-feedback-mode :to-be t)
          (approve-test-wait-for (lambda () (null approve-feedback--destination)))
          (expect (point) :to-equal (point-min))
          (expect (test-feedback--threads-shown 0) :to-match "On this file")
          ;; Back: parser.go's last thread.
          (approve-feedback-previous-thread))
        (with-current-buffer (window-buffer (selected-window))
          (expect buffer-file-name :to-equal (expand-file-name test-feedback--path
                                                               test-feedback--root))
          (expect (line-number-at-pos) :to-equal 134)
          ;; Before its first: lexer.go, its thread on lines the pull
          ;; request removed.
          (goto-char (point-min))
          (approve-feedback-previous-thread))
        (with-current-buffer (window-buffer (selected-window))
          (expect buffer-file-name :to-equal lexer)
          (approve-test-wait-for (lambda () (null approve-feedback--destination)))
          (expect (line-number-at-pos) :to-equal 7)
          (expect (test-feedback--threads-shown 7) :to-match "┊ removed:"))))))

(describe "Picking a thread in the feedback mode"
  (it "goes to the thread's line in the clone, in this file or another"
    (test-feedback--with-one
      (let ((migrations (test-feedback--add-file "internal/store/migrations.go")))
        (test-feedback--start)
        (expect approve-goto-thread-function :to-be #'approve-feedback--goto-thread)
        ;; The file at point comes first.
        (expect (approve-nav--jump-thread-preflight) :to-equal test-feedback--path)
        (expect (plist-get (cdar (approve-nav--thread-candidates test-feedback--path)) :path)
                :to-equal test-feedback--path)
        ;; A thread of this file, in a fold: the fold opens.
        (let ((id (alist-get 'id (caar (overlay-get (approve-feedback--overlay-of-line 131)
                                                    'approve-feedback-entries))))
              (fold (test-feedback--fold 125 140)))
          (test-feedback--goto 5)
          (approve-jump-thread id)
          (expect (line-number-at-pos) :to-equal 131)
          (expect (overlay-buffer fold) :to-be nil)
          (expect (test-feedback--threads-shown 131) :to-be-truthy)
          (expect (line-number-at-pos (mark t)) :to-equal 5))
        ;; Another file's: it opens in the clone.
        (approve-jump-thread (alist-get 'id (car (approve-pr-threads-for-file
                                                  "internal/store/migrations.go"))))
        (expect (buffer-file-name (window-buffer (selected-window))) :to-equal migrations)
        (with-current-buffer (window-buffer (selected-window))
          (expect approve-feedback-mode :to-be t)
          (approve-test-wait-for (lambda () (null approve-feedback--destination)))
          (expect (test-feedback--threads-shown 0) :to-match "On this file")))))

  (it "shows the thread in the PR buffer when the clone hasn't its file"
    (test-feedback--with-one
      (test-feedback--start)
      (spy-on 'message)
      (let* ((thread (seq-find (lambda (thread)
                                 (not (file-exists-p (expand-file-name (alist-get 'path thread)
                                                                       test-feedback--root))))
                               (approve-pr-threads)))
             (pr approve-model-store-buffer))
        (approve-jump-thread (alist-get 'id thread))
        (expect (window-buffer (selected-window)) :to-be pr)
        (with-current-buffer pr
          (expect (oref (magit-current-section) value) :to-equal (alist-get 'id thread)))
        (expect 'message :to-have-been-called-with
                "Approve: %s isn't in the clone; the thread is in the pull request"
                (alist-get 'path thread))))))

(describe "approve-feedback-global-mode"
  (it "turns the mode on in the files of a branch with an open pull request"
    (test-feedback--with-one
      (spy-on 'approve-clone-known-branch-head
              :and-return-value (list :clone test-feedback--root :branch "parser-rewrite"
                                      :remote "origin" :head test-feedback--head))
      (spy-on 'approve-clone-branch-pull-requests
              :and-call-fake (lambda (_head &rest args)
                               (funcall (plist-get args :callback) test-feedback--prs)))
      (spy-on 'y-or-n-p)
      (approve-feedback-global-mode 1)
      (expect approve-feedback-mode :to-be t)
      (expect (alist-get 'number approve-feedback--pull-request) :to-equal 202)
      ;; A file opened since.
      (with-current-buffer (find-file-noselect
                            (test-feedback--add-file "internal/store/migrations.go"))
        (expect approve-feedback-mode :to-be t))
      ;; Not git's own files, and nothing is asked of a file elsewhere.
      (let ((asked (spy-calls-count 'approve-clone-known-branch-head)))
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name ".git/COMMIT_EDITMSG" test-feedback--root))
          (approve-feedback--turn-on)
          (expect approve-feedback-mode :to-be nil)
          (setq buffer-file-name nil))
        (expect (spy-calls-count 'approve-clone-known-branch-head) :to-equal asked))
      (expect 'y-or-n-p :not :to-have-been-called)
      (approve-feedback-global-mode -1)
      (expect approve-feedback-mode :to-be nil)))

  (it "leaves a branch alone whose pull requests are closed or merged"
    (let ((test-feedback--prs (list (test-feedback--row 202 "MERGED"))))
      (test-feedback--with-clone
        (spy-on 'approve-clone-known-branch-head
                :and-return-value (list :clone test-feedback--root :branch "parser-rewrite"
                                        :remote "origin" :head test-feedback--head))
        (spy-on 'approve-clone-branch-pull-requests
                :and-call-fake (lambda (_head &rest args)
                                 (funcall (plist-get args :callback) test-feedback--prs)))
        (spy-on 'y-or-n-p)
        (approve-feedback-global-mode 1)
        (expect approve-feedback-mode :to-be nil)
        (expect 'y-or-n-p :not :to-have-been-called)
        ;; Once picked, it shows.
        (puthash (approve-clone--head-key test-feedback--head) "PR_kwDOPCDj7SnvM_RcFp"
                 approve-clone--picked)
        (approve-feedback--turn-on)
        (expect approve-feedback-mode :to-be t)))))

;;; test-approve-feedback.el ends here
