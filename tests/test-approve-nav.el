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

(describe "What is new since the last review"
  (it "goes to every new comment once: the conversation's, then the diff's"
    (test-nav--with-medium
      ;; vera reviewed at 15:40 on the first day: 14 entries of the
      ;; conversation and 20 comments in threads came after.
      (expect (approve-nav-new-count) :to-equal 34)
      (expect (substring-no-properties (approve-menus--new-description))
              :to-equal "Next new comment (34 since your review)")
      (expect (approve-menus--nothing-new-p) :to-be nil)
      (let ((stops nil))
        (condition-case nil
            (while t
              (approve-next-new-comment)
              (push (cons (point) (oref (magit-current-section) type)) stops))
          (user-error nil))
        (setq stops (nreverse stops))
        (expect (length stops) :to-equal 34)
        (expect (sort (mapcar #'car stops) #'<) :to-equal (mapcar #'car stops))
        (expect (seq-uniq (mapcar #'cdr (seq-take stops 14)))
                :to-have-same-items-as '(approve-review-section approve-issue-comment-section))
        (expect (seq-uniq (mapcar #'cdr (nthcdr 14 stops)))
                :to-have-same-items-as '(approve-thread-section approve-comment-section))
        ;; Each stop's line says new, and is shown.
        (dolist (stop stops)
          (goto-char (car stop))
          (expect (text-property-not-all (point) (line-end-position) 'approve-new nil)
                  :to-be-truthy)
          (expect (invisible-p (point)) :to-be nil))
        ;; And back again.
        (goto-char (point-max))
        (let ((back nil))
          (condition-case nil
              (while t
                (approve-previous-new-comment)
                (push (point) back))
            (user-error nil))
          (expect back :to-equal (mapcar #'car stops))))))

  (it "expands a file only to show a new comment in it"
    (test-nav--with-medium
      (approve-jump-files)
      (approve-next-new-comment)
      (let ((file (approve-nav--current-file)))
        (expect (oref file hidden) :to-be nil)
        ;; The files before it with no new comment stay collapsed.
        (dolist (section (approve-nav--file-sections))
          (when (< (oref section start) (oref file start))
            (expect (oref section hidden) :to-be t))))))

  (it "says that nothing is new before the viewer's first review"
    (test-nav--with-medium
      (approve-model-patch `((__typename . "PullRequest") (id . ,(approve-pr-field 'id))
                             (viewerLatestReview)))
      (expect (approve-nav-new-count) :to-equal 0)
      (expect (approve-menus--nothing-new-p) :to-be t)
      (expect (approve-next-new-comment) :to-throw 'user-error))))

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

(defun test-nav--threads ()
  "Return the medium PR's threads: alists of id, path, line and state."
  (approve-pr-list-fields 'reviewThreads '(id path line isResolved isOutdated subjectType)))

(describe "Picking a thread"
  (it "names a thread's lines as GitHub does"
    (cl-flet ((lines (&rest fields) (approve-nav--thread-lines fields)))
      (expect (lines '(line . 55) '(diffSide . "RIGHT")) :to-equal "55")
      (expect (lines '(line . 14) '(startLine . 13) '(diffSide . "RIGHT")) :to-equal "13–14")
      (expect (lines '(line . 14) '(startLine . 14) '(diffSide . "RIGHT")) :to-equal "14")
      (expect (lines '(line . 32) '(diffSide . "LEFT")) :to-equal "-32")
      (expect (lines '(line . 14) '(startLine . 13) '(diffSide . "RIGHT")
                     '(startDiffSide . "LEFT"))
              :to-equal "-13–+14")
      (expect (lines '(originalLine . 61) '(diffSide . "RIGHT")) :to-equal "61")
      (expect (lines '(line . 1) '(subjectType . "FILE")) :to-be nil)))

  (it "offers every thread, by file: the one at point first, then the diff's order"
    (test-nav--with-medium
      (let* ((threads (test-nav--threads))
             (paths (mapcar (lambda (file) (alist-get 'path file)) (approve-pr-files)))
             (with-threads (seq-filter (lambda (path)
                                         (seq-find (lambda (thread)
                                                     (equal (alist-get 'path thread) path))
                                                   threads))
                                       paths))
             (first (car (last with-threads)))
             (candidates (approve-nav--thread-candidates first))
             (order (delete-dups (mapcar (lambda (each) (plist-get (cdr each) :path))
                                         candidates))))
        (expect (length candidates) :to-equal (length threads))
        (expect order :to-equal (cons first (remove first with-threads)))
        (dolist (path order)
          (let ((states (mapcar (lambda (each)
                                  (let ((thread (seq-find (lambda (thread)
                                                            (equal (alist-get 'id thread)
                                                                   (plist-get (cdr each) :id)))
                                                          threads)))
                                    (if (or (alist-get 'isResolved thread)
                                            (alist-get 'isOutdated thread))
                                        1 0)))
                                (seq-filter (lambda (each)
                                              (equal (plist-get (cdr each) :path) path))
                                            candidates))))
            ;; The open ones first in each file.
            (expect states :to-equal (sort (copy-sequence states) #'<))))
        ;; A resolved thread is dimmed, and says so.
        (let* ((resolved (seq-find (lambda (thread) (alist-get 'isResolved thread)) threads))
               (candidate (seq-find (lambda (each)
                                      (equal (plist-get (cdr each) :id)
                                             (alist-get 'id resolved)))
                                    candidates)))
          (expect (car candidate) :to-match (concat "\\`" (regexp-quote
                                                           (alist-get 'path resolved))
                                                    ":"))
          (expect (member 'approve-dim (ensure-list (get-text-property
                                                     0 'face (car candidate))))
                  :to-be-truthy)
          (expect (plist-get (cdr candidate) :notes) :to-match "resolved")))))

  (it "groups the threads by file, shows their notes and returns the one picked"
    (test-nav--with-medium
      (let* ((candidates (approve-nav--thread-candidates nil))
             (picked (nth 2 candidates))
             (seen nil))
        (spy-on 'completing-read
                :and-call-fake
                (lambda (prompt collection &rest _)
                  (let* ((metadata (cdr (funcall collection "" nil 'metadata)))
                         (group (alist-get 'group-function metadata))
                         (label (car picked))
                         (path (plist-get (cdr picked) :path)))
                    (setq seen
                          (list prompt (alist-get 'category metadata)
                                (funcall group label nil)
                                (equal (concat path ":" (funcall group label t)) label)
                                (funcall (alist-get 'annotation-function metadata) label)
                                (alist-get 'display-sort-function metadata)
                                (length (all-completions "" collection))))
                    (substring-no-properties label))))
        (expect (approve-nav-read-thread "Thread: ") :to-equal (plist-get (cdr picked) :id))
        (expect (nth 0 seen) :to-equal "Thread: ")
        (expect (nth 1 seen) :to-be 'approve-thread)
        (expect (nth 2 seen) :to-equal (plist-get (cdr picked) :path))
        (expect (nth 3 seen) :to-be t)
        (expect (nth 4 seen) :to-equal (and (plist-get (cdr picked) :notes)
                                            (concat "  " (plist-get (cdr picked) :notes))))
        (expect (nth 5 seen) :to-be #'identity)
        (expect (nth 6 seen) :to-equal (length candidates)))))

  (it "goes to the thread picked, in the diff, the mark where point was"
    (test-nav--with-medium
      (let ((id (alist-get 'id (car (last (test-nav--threads))))))
        (approve-jump-description)
        (let ((from (point)))
          (approve-jump-thread id)
          (expect (test-nav--section-at-point) :to-equal (cons 'approve-thread-section id))
          (expect (invisible-p (point)) :to-be nil)
          (expect (mark t) :to-equal from)))))

  (it "goes to the PR buffer from a buffer that shares its store"
    (test-nav--with-medium
      (let ((pr (current-buffer))
            (id (alist-get 'id (car (test-nav--threads)))))
        (with-temp-buffer
          (setq approve-model-store-buffer pr)
          (approve-jump-thread id))
        (expect (window-buffer (selected-window)) :to-be pr)
        (expect (test-nav--section-at-point) :to-equal (cons 'approve-thread-section id)))))

  (it "says why where it can't pick"
    (with-temp-buffer
      (expect (approve-nav--jump-thread-preflight) :to-throw 'user-error
              '("Not in a buffer of a pull request")))
    (test-nav--with-medium
      (approve-nav-goto-file "pkg/api/types.go")
      (expect (approve-nav--jump-thread-preflight) :to-equal "pkg/api/types.go")
      (approve-model-patch `((__typename . "PullRequest") (id . ,(approve-pr-field 'id))
                             (reviewThreads (nodes))))
      (expect (approve-nav--jump-thread-preflight) :to-throw 'user-error
              '("No threads in this pull request"))))

  (it "is j in the jump menu"
    (expect (plist-get (cdr (transient-get-suffix 'approve-jump "j")) :command)
            :to-be 'approve-jump-thread)))

(describe "approve-fold-others"
  (it "folds every section but the one at point, and in the diff every file but its own"
    (test-nav--with-medium
      (approve-nav-goto-file "README.md")
      (approve-nav-goto-file "pkg/api/types.go")
      (forward-line 2)
      (approve-fold-others)
      (let ((folded (lambda ()
                      (mapcar (lambda (section) (oref section type))
                              (seq-filter (lambda (section)
                                            (and (oref section content) (oref section hidden)))
                                          (oref magit-root-section children))))))
        (expect (funcall folded)
                :to-equal '(approve-pending-review-section approve-description-section
                            approve-checks-section approve-commits-section
                            approve-conversation-section approve-files-section))
        (expect (oref (approve-test-find-section 'approve-file-section "pkg/api/types.go") hidden)
                :to-be nil)
        (expect (oref (approve-test-find-section 'approve-file-section "README.md") hidden)
                :to-be-truthy)
        (expect (plist-get (approve-context-at-point) :path) :to-equal "pkg/api/types.go")
        ;; It holds when the buffer is drawn again.
        (approve-pr-redraw)
        (expect (oref (approve-test-find-section 'approve-description-section) hidden)
                :to-be-truthy)
        ;; From the description's heading, the diff folds and it opens.
        (approve-jump-description)
        (approve-fold-others)
        (expect (oref (approve-test-find-section 'approve-description-section) hidden)
                :to-be nil)
        (expect (oref (approve-test-find-section 'approve-diff-section) hidden)
                :to-be-truthy)
        (expect (keymap-lookup approve-pr-mode-map "O") :to-be 'approve-fold-others)))))

(describe "approve-visit"
  (it "opens a file whole, from the file list and from its heading in the diff"
    (test-nav--with-medium
      (spy-on 'approve-file-show)
      (test-nav--goto "internal/parser/lexer.go")
      (approve-visit)
      (expect 'approve-file-show :to-have-been-called-with
              "internal/parser/lexer.go" "RIGHT" nil)
      (approve-nav-goto-file "internal/engine/legacy.go")
      (approve-visit)
      ;; A deleted file has only its base.
      (expect 'approve-file-show :to-have-been-called-with
              "internal/engine/legacy.go" "LEFT" nil)))

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
      (expect (oref (magit-current-section) hidden) :to-be t)))

  (it "shows a commit by the first of approve-visit-commit-functions that can"
    (test-nav--with-medium
      (spy-on 'browse-url)
      (let* ((asked nil)
             (approve-visit-commit-functions
              (list (lambda (oid url) (push (list 'no oid url) asked) nil)
                    (lambda (oid _url) (push (list 'yes oid) asked) t)))
             (commit (car (oref (approve-nav-goto-section 'approve-commits-section t)
                                children)))
             (oid (oref commit value)))
        (goto-char (oref commit start))
        (approve-visit)
        (expect (nreverse asked)
                :to-equal `((no ,oid ,(format "https://github.com/acme-example/rocket/pull/202/commits/%s"
                                             oid))
                            (yes ,oid)))
        (expect 'browse-url :not :to-have-been-called))))

  (it "opens a commit on GitHub when no function shows it"
    (test-nav--with-medium
      (spy-on 'browse-url)
      (let* ((approve-visit-commit-functions (list #'ignore))
             (commit (car (oref (approve-nav-goto-section 'approve-commits-section t)
                                children))))
        (goto-char (oref commit start))
        (approve-visit)
        (expect 'browse-url :to-have-been-called-with
                (format "https://github.com/acme-example/rocket/pull/202/commits/%s"
                        (oref commit value)))))))

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
  (it "starts input buffers in insert state, and makes picking a thread a jump"
    (let ((initial nil)
          (properties nil))
      (cl-letf (((symbol-function 'evil-define-key*) #'ignore)
                ((symbol-function 'evil-set-initial-state)
                 (lambda (mode state) (push (cons mode state) initial)))
                ((symbol-function 'evil-add-command-properties)
                 (lambda (command &rest props) (push (cons command props) properties))))
        (load "approve-evil" nil t))
      (expect initial :to-equal '((approve-input-mode . insert)))
      (expect properties :to-equal '((approve-jump-thread :jump t)))))

  (it "binds Approve's keys in normal state without shadowing motions"
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state map key def)
                   (push (list state map (key-description key) def) bound)))
                ((symbol-function 'evil-set-initial-state) #'ignore)
                ((symbol-function 'evil-add-command-properties) #'ignore))
        (load "approve-evil" nil t))
      (expect (seq-find (lambda (b) (and (eq (nth 1 b) approve-pr-mode-map)
                                         (equal (nth 2 b) "g r")))
                        bound)
              :to-equal (list 'normal approve-pr-mode-map "g r" 'approve-pr-refresh))
      (expect (seq-find (lambda (b) (and (eq (nth 1 b) approve-pr-mode-map)
                                         (equal (nth 2 b) "g y")))
                        bound)
              :to-equal (list 'normal approve-pr-mode-map "g y" 'approve-copy))
      ;; The dashboard's G, refreshing the list at point, is gR: G is
      ;; evil's motion to the end.
      (expect (seq-find (lambda (b) (and (eq (nth 1 b) approve-dashboard-mode-map)
                                         (equal (nth 2 b) "g R")))
                        bound)
              :to-equal (list 'normal approve-dashboard-mode-map "g R"
                              'approve-dashboard-refresh-section))
      (expect (seq-find (lambda (b) (equal (nth 2 b) "G")) bound) :to-be nil)
      ;; z folds as in vim: zo and zc in both buffers, zp all but the
      ;; section at point, and in the dashboard zM and zR fold and
      ;; unfold every list.
      (dolist (map (list approve-pr-mode-map approve-dashboard-mode-map))
        (expect (mapcar (lambda (key)
                          (nth 3 (seq-find (lambda (b) (and (eq (nth 1 b) map)
                                                            (equal (nth 2 b) key)))
                                           bound)))
                        '("z a" "z o" "z c" "z p"))
                :to-equal '(approve-section-toggle approve-section-show
                                                   approve-section-hide approve-fold-others)))
      (expect (mapcar (lambda (key)
                        (nth 3 (seq-find (lambda (b)
                                           (and (eq (nth 1 b) approve-dashboard-mode-map)
                                                (equal (nth 2 b) key)))
                                         bound)))
                      '("z M" "z m" "z R" "z r"))
              :to-equal '(approve-dashboard-fold-lists approve-dashboard-fold-lists
                          approve-dashboard-unfold-lists approve-dashboard-unfold-lists))
      ;; No key may be an evil motion, search, visual selection or yank,
      ;; nor start with one.  An operator (c d) may be a key of its own,
      ;; as change and delete have nothing to do in a read-only buffer
      ;; (docs/ui.org, "Keys"), but never start a sequence; j and k
      ;; stay motions, by screen line.
      (dolist (key '("h" "l" "w" "b" "e" "v" "V" "y" "n" "N" "/"))
        (expect (seq-find (lambda (b) (equal (car (split-string (nth 2 b))) key)) bound)
                :to-be nil))
      (dolist (key '("c" "d"))
        (expect (seq-find (lambda (b) (string-prefix-p (concat key " ") (nth 2 b))) bound)
                :to-be nil))
      (expect (mapcar (lambda (key) (nth 3 (seq-find (lambda (b) (equal (nth 2 b) key)) bound)))
                      '("j" "k" "<down>" "<up>"))
              :to-equal '(evil-next-visual-line evil-previous-visual-line
                          evil-next-visual-line evil-previous-visual-line)))))

;;; test-approve-nav.el ends here
