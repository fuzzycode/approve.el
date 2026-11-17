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
           (dolist (input (buffer-list))
             (when (string-prefix-p "*approve-input" (buffer-name input))
               (with-current-buffer input
                 (setq approve-input--done t))
               (kill-buffer input)))
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
          (expect tooltip :to-match "5 unresolved threads in this file")
          (expect tooltip :to-match "mouse-1: Approve's menu\\'")
          (expect (get-text-property 1 'local-map approve-feedback--lighter)
                  :to-be approve-feedback--lighter-map)))))

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
      (expect (test-feedback--lighter) :to-match "↻")
      (expect (get-text-property 1 'help-echo approve-feedback--lighter)
              :to-match "Updated on GitHub: .*approve-feedback-refresh to see it")))

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
      (expect (test-feedback--lighter) :to-match "#202 .*4 ↑1\\'")
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
        (expect (keymap-lookup approve-feedback-command-map (plist-get suffix :key))
                :to-be (plist-get suffix :command)))
      (expect (seq-difference (delete-dups bound)
                              (cons 'approve-feedback-dispatch
                                    (mapcar (lambda (suffix) (plist-get suffix :command))
                                            suffixes)))
              :to-equal nil)
      (expect (keymap-lookup approve-feedback-command-map "?")
              :to-be 'approve-feedback-dispatch)))

  (it "bind evil's ]t and [t alone in the user's buffers"
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state map key def)
                   (when (eq map approve-feedback-mode-map)
                     (push (list state (key-description key) def) bound))))
                ((symbol-function 'evil-set-initial-state) #'ignore))
        (require 'approve-evil)
        (approve-evil-setup-feedback approve-feedback-mode-map))
      (expect (reverse bound)
              :to-equal '((normal "] t" approve-feedback-next-thread)
                          (normal "[ t" approve-feedback-previous-thread)))))

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

;;; test-approve-feedback.el ends here
