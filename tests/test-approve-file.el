;;; test-approve-file.el --- Tests for the file view -*- lexical-binding: t; -*-

;;; Commentary:

;; The whole-file view (flow F15, decision D43) on the medium PR: the
;; file's versions as GitHub would send them (a fake answers the blob
;; query and the mutations), what the view shows (marks, the diff's
;; lines, threads, the header line), its keys, and how it follows the
;; PR buffer.  The parts that need no buffer are tested on a small
;; patch first.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-file)
(require 'approve-menus)
(require 'approve-test-helpers "helpers")

(defconst test-file--path "internal/parser/parser.go"
  "The medium PR's file with threads: on lines 15, 131 and 134 of the
head, on line 52 of the base (resolved), outdated, and on the file.")

(defconst test-file--head "6ffac5752e1be53bad90e2beeb6d4b0e1f998cc7"
  "The medium PR's head commit.")

(defconst test-file--base "9c70676340c0533e1b6042826bb5f9cfb801a5ab"
  "The medium PR's base commit.")

(defconst test-file--pending "PRR_kwDO5qcMmbrbgz7Ae3"
  "The viewer's pending review in the medium PR.")

(defvar test-file--requests nil
  "The blob queries and mutations sent to the fake GitHub, the last first.")

(defvar test-file--blob nil
  "A function of OID and PATH that returns GitHub's object, or nil.
Nil means the default: a text file of 600 lines, \"<oid> N\".")

(defvar test-file--answer nil
  "A function of a mutation request that returns GitHub's data.")

(defvar evil-mode)

(defun test-file--text (oid)
  "Return the text of a file of 600 lines at commit OID."
  (mapconcat (lambda (n) (format "%s %d\n" (substring oid 0 4) n))
             (number-sequence 1 600) ""))

(defun test-file--object (oid path)
  "Return GitHub's object of PATH at OID, from `test-file--blob'."
  (if test-file--blob
      (funcall test-file--blob oid path)
    (let ((text (test-file--text oid)))
      `((__typename . "Blob") (id . "B_1") (text . ,text) (isBinary) (isTruncated)
        (byteSize . ,(string-bytes text))))))

(defun test-file--answer-request (request)
  "Return the data the fake GitHub answers REQUEST with."
  (if (equal (plist-get request :name) "queries/blob.graphql")
      (let ((expression (alist-get 'expression (plist-get request :variables))))
        (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" expression)
        `((repository (__typename . "Repository") (id . "R_1")
                      (object ,@(test-file--object (match-string 1 expression)
                                                   (match-string 2 expression))))))
    (funcall test-file--answer request)))

(defmacro test-file--with-pr (&rest body)
  "Run BODY in the medium PR's buffer, GitHub faked for blobs and mutations.
The buffer is in the selected window.  It, its file views and any
input are killed after, and the session's texts forgotten."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((replay approve-api-transport)
            (approve-api-transport
             (lambda (request)
               (if (or (equal (plist-get request :name) "queries/blob.graphql")
                       (string-prefix-p "mutations/" (or (plist-get request :name) "")))
                   (progn
                     (push request test-file--requests)
                     (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                                               `((data . ,(test-file--answer-request request)))
                                               nil)))
                       (lambda () (cancel-timer timer))))
                 (funcall replay request))))
            (test-file--requests nil)
            (test-file--blob nil)
            (test-file--answer nil)
            (pr (approve-test-open-pr "medium")))
       (approve-file-clear-cache)
       (unwind-protect
           (progn
             (switch-to-buffer pr)
             (delete-other-windows)
             (with-current-buffer pr
               ,@body))
         (dolist (buffer (buffer-list))
           (when (string-prefix-p "*approve-input" (buffer-name buffer))
             (with-current-buffer buffer
               (setq approve-input--done t))
             (kill-buffer buffer)))
         (kill-buffer pr)
         (approve-file-clear-cache)))))

(defun test-file--blobs ()
  "Return the expressions of the blob queries sent, the first first."
  (reverse (delq nil (mapcar (lambda (request)
                               (and (equal (plist-get request :name) "queries/blob.graphql")
                                    (alist-get 'expression (plist-get request :variables))))
                             test-file--requests))))

(defun test-file--view ()
  "Return the file view of the medium PR, once it is shown."
  (approve-test-wait-for
   (lambda () (seq-find (lambda (buffer) (buffer-local-value 'approve-file-mode buffer))
                        (buffer-list)))))

(defun test-file--goto-diff-line (side number &optional path)
  "Move point to the diff line NUMBER on SIDE of PATH in the PR buffer."
  (let ((path (or path test-file--path)))
    (approve-nav-goto-file path)
    (goto-char (car (or (approve-diff-range-positions
                         (list :path path :side side :line number))
                        (error "No line %s %s:%s" side path number))))))

(defun test-file--open (side line &optional path)
  "Show PATH (the file with threads by default) at SIDE and LINE; return its view.
It is asked from the PR buffer, in the selected window, and shown there."
  (let ((path (or path test-file--path))
        (owner (approve-model-owner-buffer)))
    (switch-to-buffer owner)
    (with-current-buffer owner
      (approve-file-show path side line))
    (approve-test-wait-for
     (lambda ()
       (let ((view (buffer-local-value 'approve-context-file (window-buffer))))
         (and (equal (plist-get view :path) path) (equal (plist-get view :side) side)))))
    (window-buffer)))

(defun test-file--marks ()
  "Return the fringe marks of the current view: (LINE KIND SHAPE), in order.
KIND is what the pull request did, `added', `removed' or `changed';
SHAPE is `bar' on a line, `above' or `below' at its edge."
  (let ((marks nil))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when-let* ((mark (overlay-get overlay 'approve-file-mark)))
        (push (cons (line-number-at-pos (overlay-start overlay)) mark) marks)))
    (sort marks (lambda (a b) (< (car a) (car b))))))

(defun test-file--bars (kind from to)
  "Return the marks of bars of KIND on the lines FROM to TO."
  (mapcar (lambda (line) (list line kind 'bar)) (number-sequence from to)))

(defun test-file--threads ()
  "Return the lines of the current view with threads: (LINE . SHOWN), in order.
SHOWN is non-nil when the threads are drawn, not only the bubble."
  (let ((lines nil))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'approve-file-threads)
        (push (cons (line-number-at-pos (overlay-start overlay))
                    (and (string-match-p "╭─" (overlay-get overlay 'before-string)) t))
              lines)))
    (sort lines (lambda (a b) (< (car a) (car b))))))

(defun test-file--header ()
  "Return the header line of the current view, without properties."
  (substring-no-properties approve-file--header-line))

;;; Without a buffer

(defconst test-file--patch "@@ -1,5 +1,5 @@\n a\n-b\n+B\n+C\n c\n-d\n e"
  "A patch: base a b c d e, head a B C c e.")

(describe "What a version of a file shows"
  (it "lists the changes each version has, and where the other one has lines"
    (let ((hunks (approve-diff-parse test-file--patch)))
      (expect (approve-file--blocks hunks nil)
              :to-equal '((2 2 changed) (5 0 removed)))
      (expect (approve-file--blocks hunks t)
              :to-equal '((2 1 changed) (4 1 removed)))
      ;; A file that only gains lines, at its end.
      (expect (approve-file--blocks (approve-diff-parse "@@ -1,2 +1,3 @@\n a\n b\n+c") t)
              :to-equal '((3 0 added)))))

  (it "marks lines with a bar, and where the other version has lines with a triangle"
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (approve-file--put-marks '((1 0 removed) (2 1 changed) (4 0 added)))
      ;; Lines removed before the first line go above it; lines added
      ;; after the last one, below it.
      (expect (test-file--marks)
              :to-equal '((1 removed above) (2 changed bar) (3 added below)))))

  (it "finds a line's place in the other version"
    (let ((hunks (approve-diff-parse test-file--patch)))
      (expect (mapcar (lambda (line) (approve-file--map-line hunks line nil)) '(1 2 3 4 5 9))
              :to-equal '(1 2 2 3 5 9))
      (expect (mapcar (lambda (line) (approve-file--map-line hunks line t)) '(1 2 3 4 5))
              :to-equal '(1 2 4 5 5))))

  (it "puts the threads under the lines of each version, and counts the others"
    (let ((hunks (approve-diff-parse test-file--patch))
          (threads (list '((id . "T1") (diffSide . "RIGHT") (line . 3) (subjectType . "LINE"))
                         '((id . "T2") (diffSide . "LEFT") (line . 4) (subjectType . "LINE"))
                         '((id . "T3") (diffSide . "LEFT") (line . 3) (subjectType . "LINE"))
                         '((id . "T4") (diffSide . "RIGHT") (line) (subjectType . "LINE"))
                         '((id . "T5") (diffSide . "RIGHT") (line) (subjectType . "FILE")))))
      (cl-flet ((sorted (index)
                  (let ((lines nil))
                    (maphash (lambda (line threads)
                               (push (cons line (mapcar (lambda (thread) (alist-get 'id thread))
                                                        threads))
                                     lines))
                             (plist-get index :lines))
                    (list (sort lines (lambda (a b) (< (car a) (car b))))
                          (plist-get index :elsewhere)))))
        (expect (sorted (approve-file--thread-index threads hunks nil))
                :to-equal '(((3 "T1") (4 "T3")) 3))
        (expect (sorted (approve-file--thread-index threads hunks t))
                :to-equal '(((3 "T3") (4 "T2")) 3)))))

  (it "counts the columns its line numbers take, for a bubble's room"
    (with-temp-buffer
      (insert (make-string 150 ?\n))
      (expect (approve-file--line-numbers-width) :to-be 0)
      (display-line-numbers-mode 1)
      ;; The digits of the last line, 151, and two of padding.
      (expect (approve-file--line-numbers-width) :to-be 5)
      (setq-local display-line-numbers-width 5)
      (expect (approve-file--line-numbers-width) :to-be 7))))

;;; The view

(describe "The file view"
  (before-each
    (spy-on 'message))

  (it "opens on RET on a diff line, at the head, on that line"
    (test-file--with-pr
      (test-file--goto-diff-line "RIGHT" 15)
      (call-interactively #'approve-visit)
      (let ((view (test-file--view)))
        (expect (window-buffer (selected-window)) :to-be view)
        (with-current-buffer view
          (expect (buffer-name) :to-equal
                  "*approve-file: acme-example/rocket internal/parser/parser.go @ 6ffac57*")
          (expect (buffer-string) :to-equal (test-file--text test-file--head))
          (expect buffer-read-only :to-be t)
          (expect approve-model-store-buffer :to-be pr)
          (expect (line-number-at-pos (window-point)) :to-be 15)))
      (expect (test-file--blobs) :to-equal (list (concat test-file--head ":" test-file--path)))))

  (it "opens a removed line in the base, and a file at its first change"
    (test-file--with-pr
      (test-file--goto-diff-line "LEFT" 13)
      (call-interactively #'approve-visit)
      (with-current-buffer (test-file--view)
        (expect approve-context-file :to-equal
                (list :path test-file--path :name test-file--path :side "LEFT"
                      :oid test-file--base))
        (expect (line-number-at-pos) :to-be 13)
        (quit-window))
      (approve-nav-goto-file "internal/engine/scheduler.go")
      (call-interactively #'approve-visit-file)
      (test-file--view)
      (approve-test-wait-for (lambda () (string-match-p "scheduler" (buffer-name (window-buffer)))))
      (with-current-buffer (window-buffer)
        (expect (plist-get approve-context-file :side) :to-equal "RIGHT")
        (expect (line-number-at-pos (window-point)) :to-be 1))))

  (it "is in the file's major mode, without running the mode's hooks"
    (require 'make-mode)
    (test-file--with-pr
      (let ((ran nil)
            ;; Evil is on everywhere: it turns on in the view too.
            (evil-mode t))
        (cl-letf (((default-value 'makefile-mode-hook) (list (lambda () (push 'makefile ran))))
                  ((default-value 'prog-mode-hook) (list (lambda () (push 'prog ran))))
                  ((default-value 'approve-file-mode-hook) (list (lambda () (push 'file ran))))
                  ((symbol-function 'evil-initialize) (lambda () (push 'evil ran))))
          (with-current-buffer (test-file--open "RIGHT" 1 "Makefile")
            (expect (derived-mode-p 'makefile-mode) :to-be-truthy)
            ;; Font-lock is not turned on in batch Emacs; its keywords are set.
            (expect font-lock-defaults :to-be-truthy)
            (expect approve-file-mode :to-be t)
            (expect ran :to-equal '(file evil)))))))

  (it "marks the changes, and gives diff lines and other lines their context"
    (test-file--with-pr
      (with-current-buffer (test-file--open "RIGHT" 15)
        (expect (test-file--marks)
                :to-equal (append (test-file--bars 'changed 13 18)
                                  (test-file--bars 'changed 55 57)
                                  (test-file--bars 'added 126 137)
                                  '((218 removed above))))
        (expect (approve-context-at-point)
                :to-equal (list :kind 'line :path test-file--path :side "RIGHT" :line 15
                                :old nil :new 15 :line-kind 'add
                                :url (approve-context-file-url test-file--path "RIGHT" 15)
                                :threads '("PRRT_kwDO85-qIzFQYiTYBM")))
        (approve-file--goto-line 3)
        (expect (approve-context-at-point)
                :to-equal (list :kind 'code :path test-file--path :name test-file--path
                                :side "RIGHT" :line 3
                                :url (concat "https://github.com/acme-example/rocket/blob/"
                                             test-file--head "/internal/parser/parser.go#L3")
                                :threads nil))
        (approve-copy)
        (expect (current-kill 0) :to-equal "internal/parser/parser.go:3")
        (approve-file-toggle-version)
        (test-file--view)
        (approve-test-wait-for (lambda () (equal (plist-get approve-context-file :side) "LEFT")))
        (expect (test-file--marks)
                :to-equal (append (test-file--bars 'changed 13 14)
                                  (test-file--bars 'changed 51 54)
                                  '((123 added above))
                                  (test-file--bars 'removed 203 207))))))

  (it "shows threads under their lines, the resolved ones folded"
    (test-file--with-pr
      (with-current-buffer (test-file--open "RIGHT" 1)
        (expect (test-file--threads) :to-equal '((15 . t) (131 . t) (134 . t)))
        (expect (test-file--header) :to-match
                "\\`  *head 6ffac57 · 4 changes · 3 threads · 3 more in the diff")
        (approve-file-toggle-version)
        (approve-test-wait-for (lambda () (equal (plist-get approve-context-file :side) "LEFT")))
        (expect (test-file--threads) :to-equal '((52 . nil)))
        (approve-file--goto-line 52)
        (approve-file-toggle-threads)
        (expect (test-file--threads) :to-equal '((52 . t)))
        (approve-file-toggle-threads)
        (expect (test-file--threads) :to-equal '((52 . nil)))
        (approve-file--goto-line 51)
        (expect (approve-file-toggle-threads) :to-throw 'user-error))))

  (it "moves between changes and unresolved threads"
    (test-file--with-pr
      (with-current-buffer (test-file--open "RIGHT" 1)
        (let ((lines nil))
          (dotimes (_ 4)
            (approve-file-next-change)
            (push (line-number-at-pos) lines))
          (expect (nreverse lines) :to-equal '(13 55 126 218)))
        (expect (approve-file-next-change) :to-throw 'user-error)
        (approve-file-previous-change)
        (expect (line-number-at-pos) :to-be 126)
        (approve-file--goto-line 1)
        (approve-file-next-thread)
        (expect (line-number-at-pos) :to-be 15)
        (approve-file-next-thread)
        (expect (line-number-at-pos) :to-be 131)
        (approve-file-previous-thread)
        (expect (line-number-at-pos) :to-be 15)
        ;; A folded line unfolds when ]t moves to it.
        (approve-file--goto-line 131)
        (approve-file-toggle-threads)
        (approve-file--goto-line 15)
        (approve-file-next-thread)
        (expect (assq 131 (test-file--threads)) :to-equal '(131 . t)))))

  (it "shows the other version on the matching line, and says when there is none"
    (test-file--with-pr
      (with-current-buffer (test-file--open "RIGHT" 15)
        (approve-file-toggle-version)
        (approve-test-wait-for (lambda () (equal (plist-get approve-context-file :side) "LEFT")))
        (expect (buffer-name) :to-equal
                "*approve-file: acme-example/rocket internal/parser/parser.go @ 9c70676*")
        (expect (buffer-string) :to-equal (test-file--text test-file--base))
        (expect (line-number-at-pos) :to-be 14)
        (expect (test-file--header) :to-match "\\`  *base 9c70676"))
      ;; A renamed file's base is at its old path.
      (with-current-buffer (test-file--open "RIGHT" 1 "internal/engine/scheduler.go")
        (approve-file-toggle-version)
        (approve-test-wait-for (lambda () (equal (plist-get approve-context-file :side) "LEFT")))
        (expect (plist-get approve-context-file :name) :to-equal "internal/engine/queue.go")
        (expect (car (last (test-file--blobs)))
                :to-equal (concat test-file--base ":internal/engine/queue.go")))
      ;; An added file has no base, a deleted one no head.
      (with-current-buffer (test-file--open "RIGHT" 1 "docs/usage.md")
        (expect (approve-action-why-not 'approve-file-toggle-version)
                :to-equal "docs/usage.md is new in this pull request: it has no base version"))
      (approve-nav-goto-file "internal/engine/legacy.go")
      (expect (approve-file--visit-preflight)
              :to-equal '("internal/engine/legacy.go" "LEFT" nil))
      (expect (approve-file-show "internal/engine/legacy.go" "RIGHT") :to-throw 'user-error)))

  (it "fetches each version once a session, until C-u g"
    (test-file--with-pr
      (test-file--open "RIGHT" 15)
      (let ((view (test-file--open "RIGHT" 20)))
        (expect (with-current-buffer view (line-number-at-pos)) :to-be 20))
      (expect (length (test-file--blobs)) :to-be 1)
      (run-hooks 'approve-pr-clear-cache-functions)
      (test-file--open "RIGHT" 30)
      (expect (length (test-file--blobs)) :to-be 2)))

  (it "says why it can't show a file"
    (test-file--with-pr
      (setq test-file--blob (lambda (_oid _path)
                              '((__typename . "Blob") (id . "B_1") (text) (isBinary . t)
                                (isTruncated) (byteSize . 10))))
      (approve-file-show "assets/logo.png" "RIGHT")
      (approve-test-wait-for
       (lambda ()
         (seq-some (lambda (args)
                     (and (stringp (car args))
                          (equal (apply #'format-message args)
                                 "Approve: could not show assets/logo.png: The file is binary")))
                   (spy-calls-all-args 'message))))
      (expect (window-buffer (selected-window)) :to-be pr)))

  (it "comments on a line of the diff, and says why not on others"
    (test-file--with-pr
      (setq test-file--answer
            (lambda (_)
              `((addPullRequestReviewThread
                 (thread (__typename . "PullRequestReviewThread") (id . "PRRT_new")
                         (isResolved) (viewerCanReply . t) (path . ,test-file--path)
                         (line . 14) (diffSide . "RIGHT") (subjectType . "LINE")
                         (comments (totalCount . 1) (pageInfo (hasNextPage) (endCursor . "x"))
                                   (nodes ((__typename . "PullRequestReviewComment")
                                           (id . "PRRC_new") (state . "PENDING")
                                           (body . "New") (bodyHTML . "<p>New</p>")
                                           (author (__typename . "User") (id . "U_vera")
                                                   (login . "vera-example"))
                                           (pullRequestReview
                                            (__typename . "PullRequestReview")
                                            (id . ,test-file--pending))))))))))
      (let ((view (test-file--open "RIGHT" 3)))
        (set-buffer view)
        (expect (approve-action-why-not 'approve-comment)
                :to-equal
                "Line 3 is not in the diff: GitHub takes comments only on lines of the diff")
        (approve-file--goto-line 14)
        (call-interactively #'approve-comment)
        (let ((input (seq-find (lambda (buffer)
                                 (string-prefix-p "*approve-input" (buffer-name buffer)))
                               (buffer-list))))
          (expect (with-current-buffer input
                    (substring-no-properties (eval (cadr header-line-format) t)))
                  :to-match "Comment on internal/parser/parser.go:14 · pending review")
          ;; The line is highlighted in the view, as in the diff.
          (with-current-buffer view
            (expect (seq-find (lambda (overlay) (eq (overlay-get overlay 'face) 'approve-target))
                              (overlays-at (save-excursion (approve-file--goto-line 14) (point))))
                    :to-be-truthy))
          (with-current-buffer input
            (insert "Can this fail?")
            (approve-input-commit)))
        (with-current-buffer view
          (approve-test-wait-for (lambda () (null (approve-session-get :inflight))))
          (let ((variables (plist-get (car test-file--requests) :variables)))
            (expect (plist-get (car test-file--requests) :name)
                    :to-equal "mutations/add-pull-request-review-thread.graphql")
            (expect (list (alist-get 'path variables) (alist-get 'line variables)
                          (alist-get 'side variables))
                    :to-equal (list test-file--path 14 "RIGHT")))
          ;; The PR buffer redrew, and the view with it.
          (expect (assq 14 (test-file--threads)) :to-equal '(14 . t))))))

  (it "acts on the threads of the line at point, asking which when several take it"
    (test-file--with-pr
      (setq test-file--answer
            (lambda (request)
              (pcase (plist-get request :name)
                ("mutations/resolve-review-thread.graphql"
                 '((resolveReviewThread
                    (thread (__typename . "PullRequestReviewThread")
                            (id . "PRRT_kwDO85-qIzFQYiTYBM")
                            (isResolved . t) (viewerCanResolve) (viewerCanUnresolve . t)))))
                (_ '((deletePullRequestReviewComment (clientMutationId)))))))
      (let ((view (test-file--open "RIGHT" 3)))
        (set-buffer view)
        ;; A line without threads.
        (expect (approve-action-why-not 'approve-reply) :to-equal "No threads on this line")
        (expect (approve-action-why-not 'approve-delete) :to-equal "No threads on this line")
        (expect (approve-action-why-not 'approve-suggest)
                :to-equal
                "Line 3 is not in the diff: GitHub takes comments only on lines of the diff")
        ;; Line 15 has one thread, of three comments by others.
        (approve-file--goto-line 15)
        (expect (approve-action-why-not 'approve-reply) :to-be nil)
        (expect (approve-action-why-not 'approve-edit) :to-equal "You can't edit this comment")
        (expect (approve-action-why-not 'approve-comment) :to-be nil)
        (spy-on 'completing-read :and-call-fake (lambda (_prompt labels &rest _)
                                                  (nth 1 labels)))
        (call-interactively #'approve-resolve)
        (expect 'completing-read :not :to-have-been-called)
        (approve-test-wait-for (lambda () (null (approve-session-get :inflight))))
        (expect (plist-get (car test-file--requests) :name)
                :to-equal "mutations/resolve-review-thread.graphql")
        (expect (alist-get 'threadId (plist-get (car test-file--requests) :variables))
                :to-equal "PRRT_kwDO85-qIzFQYiTYBM")
        ;; Deleting asks which of its comments: the second is alice's.
        (spy-on 'y-or-n-p :and-return-value t)
        (call-interactively #'approve-delete)
        (expect (spy-calls-args-for 'completing-read 0)
                :to-equal '("Which comment: "
                            ("@carol-example: This allocates on every call; hoist the buffer out of the l…"
                             "@alice-example: Done, it is reused now."
                             "@carol-example: Thanks!")
                            nil t))
        (expect 'y-or-n-p :to-have-been-called-with "Delete @alice-example's comment? ")
        (approve-test-wait-for (lambda () (null (approve-session-get :inflight))))
        (expect (alist-get 'commentId (plist-get (car test-file--requests) :variables))
                :to-equal "PRRC_kwDOjbJkqIaS7rYCer")
        ;; Line 131 has the viewer's comment: editing takes it without asking.
        (approve-file--goto-line 131)
        (call-interactively #'approve-edit)
        (expect (spy-calls-count 'completing-read) :to-equal 1)
        (let ((input (seq-find (lambda (buffer)
                                 (string-prefix-p "*approve-input" (buffer-name buffer)))
                               (buffer-list))))
          (expect (with-current-buffer input (buffer-string))
                  :to-match "\\`These three lines can be one call:")))))

  (it "goes to its line in the diff on RET, unfolding its threads"
    (test-file--with-pr
      (with-current-buffer (test-file--open "RIGHT" 15)
        (call-interactively #'approve-visit))
      (expect (window-buffer (selected-window)) :to-be pr)
      (expect (current-buffer) :to-be pr)
      (let ((line (get-text-property (point) 'approve-line)))
        (expect (list (plist-get line :path) (plist-get line :new))
                :to-equal (list test-file--path 15)))
      (expect (oref (magit-current-section) type) :to-be 'approve-line-section)
      (expect (oref (magit-current-section) hidden) :to-be nil)
      ;; A line that is not in the diff goes to the file.
      (with-current-buffer (test-file--open "RIGHT" 3)
        (call-interactively #'approve-visit))
      (expect (oref (magit-current-section) type) :to-be 'approve-file-section)
      (expect (oref (magit-current-section) value) :to-equal test-file--path)))

  (it "follows the pull request's head, and dies with its PR buffer"
    (test-file--with-pr
      (let ((view (test-file--open "RIGHT" 15)))
        (with-current-buffer pr
          (approve-model-patch `((__typename . "PullRequest")
                                 (id . ,(approve-model-ref-id (approve-model-root-ref)))
                                 (headRefOid . "abcdef0123456789")))
          (approve-pr-redraw))
        (with-current-buffer view
          (approve-test-wait-for
           (lambda () (equal (plist-get approve-context-file :oid) "abcdef0123456789")))
          (expect (buffer-string) :to-equal (test-file--text "abcdef0123456789"))
          (expect (line-number-at-pos) :to-be 15)
          (expect (buffer-name) :to-match "@ abcdef0\\*\\'"))
        (kill-buffer pr)
        (expect (buffer-live-p view) :to-be nil)))))

(describe "The file view's keys"
  (it "reach every command it binds from its dispatch, with the same keys"
    (let ((suffixes nil)
          (bound nil))
      (cl-labels ((walk (x)
                    (cond ((and (consp x) (keywordp (car x)) (plist-member x :command))
                           (push x suffixes))
                          ((consp x) (walk (car x)) (walk (cdr x)))
                          ((vectorp x) (mapc #'walk x))))
                  (collect (map)
                    (map-keymap (lambda (_event binding)
                                  (cond ((keymapp binding) (collect binding))
                                        ((symbolp binding) (push binding bound))))
                                map)))
        (walk (get 'approve-file-dispatch 'transient--layout))
        (collect approve-file-mode-map))
      (dolist (suffix suffixes)
        ;; U, the fetch into the clones, is in the dispatch alone (D53).
        (expect (lookup-key approve-file-mode-map (kbd (plist-get suffix :key)))
                :to-be (unless (eq (plist-get suffix :command) 'approve-fetch-pull-request)
                         (plist-get suffix :command))))
      (expect (seq-difference (delete-dups bound)
                              (cons 'approve-file-dispatch
                                    (mapcar (lambda (suffix) (plist-get suffix :command))
                                            suffixes)))
              :to-equal nil)))

  (it "bind evil's keys without shadowing motions"
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state map key def)
                   (when (eq map approve-file-mode-map)
                     (push (list state (key-description key) def) bound))))
                ((symbol-function 'evil-set-initial-state) #'ignore)
                ((symbol-function 'evil-add-command-properties) #'ignore))
        (load "approve-evil" nil t))
      (dolist (binding '((normal "] c" approve-file-next-change)
                         (normal "[ c" approve-file-previous-change)
                         (normal "] t" approve-file-next-thread)
                         (normal "g b" approve-file-toggle-version)
                         (normal "o" approve-browse)
                         (normal "g o" approve-browse)
                         (normal "x" approve-toggle-viewed)
                         (normal "r" approve-reply)
                         (normal "i" approve-edit)
                         (normal "D" approve-delete)
                         (normal "?" approve-file-dispatch)
                         (visual "c" approve-comment)))
        (expect (member binding bound) :to-be-truthy))
      (dolist (key '("h" "l" "w" "b" "e" "v" "V" "y" "n" "N" "p" "/"))
        (expect (seq-find (lambda (b) (equal (car (split-string (nth 1 b))) key)) bound)
                :to-be nil)))))

;;; test-approve-file.el ends here
