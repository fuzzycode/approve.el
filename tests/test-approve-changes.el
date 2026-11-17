;;; test-approve-changes.el --- Tests for change detection -*- lexical-binding: t; -*-

;;; Commentary:

;; Change detection (decision D23, flow F17) in the medium PR: what
;; counts as a change on GitHub, what the header line says, when a
;; check is sent or dropped, and what makes one run.  The check's
;; answers are made from the store, then changed the way GitHub would.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-changes)
(require 'approve-test-helpers "helpers")

(defconst test-changes--query "queries/pull-request-changes.graphql"
  "The query of the check.")

(defvar test-changes--remote nil
  "The pullRequest the fake check answers with, or nil to fail.")

(defvar test-changes--asked 0
  "How many checks the fake transport answered.")

(defun test-changes--store-nodes (typename)
  "Return the entities of TYPENAME in the store as the check selects them.
At most the last 20 by id, each with `__typename', `id' and
`lastEditedAt'."
  (let (nodes)
    (when-let* ((table (gethash typename approve-model--store)))
      (maphash (lambda (id entity)
                 (push `((__typename . ,typename) (id . ,id)
                         (lastEditedAt . ,(alist-get 'lastEditedAt entity)))
                       nodes))
               table))
    (last (sort nodes (lambda (a b) (string< (alist-get 'id a) (alist-get 'id b))))
          20)))

(defun test-changes--remote-from-store ()
  "Return the check's answer for a PR that is as the store holds it."
  (let-alist (approve-pr-fields '(id headRefOid title state isDraft lastEditedAt
                                     commitCount))
    `((__typename . "PullRequest") (id . ,.id) (headRefOid . ,.headRefOid)
      (title . ,.title) (state . ,.state) (isDraft . ,.isDraft)
      (lastEditedAt . ,.lastEditedAt)
      (commitCount (totalCount . ,.commitCount.totalCount))
      (comments (nodes . ,(test-changes--store-nodes "IssueComment")))
      (reviews (nodes . ,(test-changes--store-nodes "PullRequestReview"))))))

(defun test-changes--with (remote field value)
  "Return REMOTE with FIELD set to VALUE."
  (cons (cons field value) (assq-delete-all field (copy-tree remote))))

(defun test-changes--with-node (remote field node)
  "Return REMOTE with NODE added to the nodes of FIELD."
  (let ((nodes (alist-get 'nodes (alist-get field remote))))
    (test-changes--with remote field `((nodes . ,(append nodes (list node)))))))

(defun test-changes--transport (fixtures)
  "Return a transport answering the check from `test-changes--remote'.
Other requests go to FIXTURES, the fixtures' transport."
  (lambda (request)
    (if (equal (plist-get request :name) test-changes--query)
        (let ((timer (run-at-time
                      0 nil
                      (lambda ()
                        (setq test-changes--asked (1+ test-changes--asked))
                        (if test-changes--remote
                            (funcall (plist-get request :on-success)
                                     `((data (repository (pullRequest . ,test-changes--remote))))
                                     nil)
                          (funcall (plist-get request :on-failure)
                                   (list :status 502
                                         :payload '((message . "Bad Gateway")))))))))
          (lambda () (cancel-timer timer)))
      (funcall fixtures request))))

(defmacro test-changes--with-medium (&rest body)
  "Run BODY in the loaded medium PR buffer, killed afterwards.
The check is answered from `test-changes--remote'."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((approve-api-transport (test-changes--transport approve-api-transport))
            (test-changes--remote nil)
            (test-changes--asked 0)
            (buffer (approve-test-open-pr "medium")))
       (unwind-protect
           (with-current-buffer buffer ,@body)
         (kill-buffer buffer)))))

(defun test-changes--check (remote)
  "Check the current PR buffer, GitHub answering REMOTE; wait for it.
Return what the check found."
  (setq test-changes--remote remote)
  (expect (approve-changes-check) :to-be-truthy)
  (approve-test-wait-for (lambda () (not (approve-session-get :checking))))
  (approve-session-get :changes))

(defun test-changes--notice ()
  "Return the notice in the header line, without properties, or nil."
  (let ((line (substring-no-properties approve-pr--header-line)))
    (and (string-match "updated on GitHub.*" line)
         (string-trim-right (match-string 0 line) "[ \t]+.? ?Open"))))

(describe "What counts as a change on GitHub"
  (it "is nothing when GitHub has what the store holds"
    (test-changes--with-medium
      (expect (approve-changes-compare (test-changes--remote-from-store)) :to-be nil)))

  (it "is new commits when the head moved"
    (test-changes--with-medium
      (let* ((remote (test-changes--remote-from-store))
             (count (let-alist remote .commitCount.totalCount))
             (remote (test-changes--with remote 'headRefOid "f00df00d"))
             (remote (test-changes--with remote 'commitCount `((totalCount . ,(+ count 2)))))
             (changes (approve-changes-compare remote)))
        (expect (plist-get changes :branch) :to-be-truthy)
        (expect (plist-get changes :commits) :to-equal 2))))

  (it "is a changed branch when the head moved without more commits"
    (test-changes--with-medium
      (let ((changes (approve-changes-compare
                      (test-changes--with (test-changes--remote-from-store)
                                          'headRefOid "f00df00d"))))
        (expect (plist-get changes :branch) :to-be-truthy)
        (expect (plist-get changes :commits) :to-equal 0))))

  (it "is the comments and reviews the store doesn't hold"
    (test-changes--with-medium
      (let* ((remote (test-changes--remote-from-store))
             (remote (test-changes--with-node remote 'comments
                                              '((__typename . "IssueComment") (id . "IC_new"))))
             (remote (test-changes--with-node remote 'reviews
                                              '((__typename . "PullRequestReview") (id . "PRR_a"))))
             (remote (test-changes--with-node remote 'reviews
                                              '((__typename . "PullRequestReview") (id . "PRR_b"))))
             (changes (approve-changes-compare remote)))
        (expect (plist-get changes :comments) :to-equal 1)
        (expect (plist-get changes :reviews) :to-equal 2)
        (expect (plist-get changes :branch) :to-be nil)
        (expect (plist-get changes :edited) :to-be nil))))

  (it "is an edit of the title, the state, the description or a comment"
    (test-changes--with-medium
      (let* ((remote (test-changes--remote-from-store))
             (comment (car (alist-get 'nodes (alist-get 'comments remote))))
             (edited-comment
              (test-changes--with
               remote 'comments
               `((nodes ,(cons '(lastEditedAt . "2099-01-01T00:00:00Z")
                               (assq-delete-all 'lastEditedAt (copy-tree comment))))))))
        (expect comment :to-be-truthy)
        (dolist (changed (list (test-changes--with remote 'title "Another title")
                               (test-changes--with remote 'state "MERGED")
                               (test-changes--with remote 'isDraft t)
                               (test-changes--with remote 'lastEditedAt "2099-01-01T00:00:00Z")
                               edited-comment))
          (let ((changes (approve-changes-compare changed)))
            (expect (plist-get changes :edited) :to-be-truthy)
            (expect (plist-get changes :comments) :to-equal 0))))))

  (it "leaves out what a write from Approve put in the store"
    (test-changes--with-medium
      ;; A conversation comment, as approve-mutation-add-comment leaves it.
      (let* ((comment '((__typename . "IssueComment") (id . "IC_mine")
                        (body . "Thanks!") (lastEditedAt)))
             (remote (test-changes--with-node (test-changes--remote-from-store)
                                              'comments comment)))
        (expect (plist-get (approve-changes-compare remote) :comments) :to-equal 1)
        (approve-model-patch comment)
        (expect (approve-changes-compare remote) :to-be nil)))))

(describe "The header line"
  (it "says the PR changed on GitHub, how to refresh and what is new"
    (test-changes--with-medium
      (let* ((remote (test-changes--remote-from-store))
             (count (let-alist remote .commitCount.totalCount))
             (remote (test-changes--with remote 'headRefOid "f00df00d"))
             (remote (test-changes--with remote 'commitCount `((totalCount . ,(+ count 2)))))
             (remote (test-changes--with-node remote 'comments
                                              '((__typename . "IssueComment") (id . "IC_new")))))
        (test-changes--check remote)
        (expect (test-changes--notice)
                :to-equal "updated on GitHub — g to refresh · 2 new commits · 1 new comment")
        (expect approve-pr--header-line :to-match "\\` #202 Rewrite the parser")
        (expect (get-text-property (string-match "updated" approve-pr--header-line)
                                   'face approve-pr--header-line)
                :to-be 'approve-updated))))

  (it "names the key that refreshes, the user's own"
    (test-changes--with-medium
      (use-local-map (copy-keymap approve-pr-mode-map))
      (keymap-set (current-local-map) "g" nil)
      (keymap-set (current-local-map) "<f5>" #'approve-pr-refresh)
      (test-changes--check (test-changes--with (test-changes--remote-from-store)
                                               'title "Another title"))
      (expect (test-changes--notice) :to-equal "updated on GitHub — <f5> to refresh")))

  (it "says nothing when nothing changed, nor while the buffer is drawn again"
    (test-changes--with-medium
      (let ((before approve-pr--header-line))
        (expect (test-changes--check (test-changes--remote-from-store)) :to-be nil)
        (expect approve-pr--header-line :to-equal before)
        (expect (test-changes--notice) :to-be nil))))

  (it "keeps the notice through redraws, and drops it when g loads the changes"
    (test-changes--with-medium
      (test-changes--check (test-changes--with (test-changes--remote-from-store)
                                               'headRefOid "f00df00d"))
      (expect (test-changes--notice) :to-equal "updated on GitHub — g to refresh · the branch changed")
      (approve-pr-redraw)
      (expect (test-changes--notice) :to-match "the branch changed")
      (approve-pr-refresh)
      (expect (approve-session-get :changes) :to-be nil)
      (expect (test-changes--notice) :to-be nil)
      (approve-test-wait-for (lambda () (not (approve-pr-loading-p)))))))

(describe "approve-changes-check"
  (it "never redraws the buffer"
    (test-changes--with-medium
      (spy-on 'approve-pr-redraw)
      (spy-on 'approve-pr-schedule-redraw)
      (test-changes--check (test-changes--with (test-changes--remote-from-store)
                                               'headRefOid "f00df00d"))
      (expect (approve-session-get :changes) :to-be-truthy)
      (expect 'approve-pr-redraw :not :to-have-been-called)
      (expect 'approve-pr-schedule-redraw :not :to-have-been-called)))

  (it "waits until the PR has loaded, and while a query runs or failed"
    (approve-test-with-fixture "medium"
      (let* ((approve-api-transport
              (test-changes--transport (approve-test-fail-queries
                                        approve-api-transport
                                        '("queries/pull-request-commits.graphql"))))
             (test-changes--asked 0)
             (buffer (approve-test-open-pr "medium" t)))
        (unwind-protect
            (with-current-buffer buffer
              (expect (approve-changes-check) :to-be nil)
              (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
              (expect (approve-pr-error 'commits) :to-be-truthy)
              (expect (approve-changes-check) :to-be nil)
              (expect test-changes--asked :to-equal 0))
          (kill-buffer buffer)))))

  (it "waits while a write is in flight, or a check is out"
    (test-changes--with-medium
      (approve-session-add :inflight '(1 (section approve-description-section nil)))
      (expect (approve-changes-check) :to-be nil)
      (approve-session-put :inflight nil)
      (setq test-changes--remote (test-changes--remote-from-store))
      (expect (approve-changes-check) :to-be-truthy)
      (expect (approve-changes-check) :to-be nil)
      (approve-test-wait-for (lambda () (not (approve-session-get :checking))))
      (expect test-changes--asked :to-equal 1)))

  (it "drops an answer that arrives after the store changed"
    (test-changes--with-medium
      (setq test-changes--remote (test-changes--with-node
                                  (test-changes--remote-from-store) 'comments
                                  '((__typename . "IssueComment") (id . "IC_gone"))))
      (approve-changes-check)
      ;; A write deletes a comment while the check is out: GitHub's
      ;; answer may still list it.
      (approve-model-patch '((__typename . "IssueComment") (id . "IC_other")))
      (approve-test-wait-for (lambda () (not (approve-session-get :checking))))
      (expect test-changes--asked :to-equal 1)
      (expect (approve-session-get :changes) :to-be nil)))

  (it "fails quietly, and keeps what the last check found"
    (test-changes--with-medium
      (spy-on 'message)
      (test-changes--check (test-changes--with (test-changes--remote-from-store)
                                               'title "Another title"))
      (test-changes--check nil)
      (expect 'message :not :to-have-been-called)
      (expect (approve-session-get :checking) :to-be nil)
      (expect (test-changes--notice) :to-equal "updated on GitHub — g to refresh"))))

(describe "When a PR is checked"
  (it "is when its window is selected, at most once per approve-focus-check-interval"
    (test-changes--with-medium
      (setq test-changes--remote (test-changes--remote-from-store))
      (set-window-buffer (selected-window) (current-buffer))
      (spy-on 'approve-changes-check :and-call-through)
      ;; The load just brought everything: not due yet.
      (approve-changes--selected (current-buffer))
      (expect 'approve-changes-check :not :to-have-been-called)
      (approve-session-put :checked (- (float-time) 61))
      (approve-changes--selected (current-buffer))
      (expect 'approve-changes-check :to-have-been-called-times 1)
      (approve-test-wait-for (lambda () (not (approve-session-get :checking))))
      (approve-changes--selected (current-buffer))
      (expect 'approve-changes-check :to-have-been-called-times 1)
      (let ((approve-focus-check-interval nil))
        (approve-session-put :checked nil)
        (approve-changes--selected (current-buffer))
        (expect 'approve-changes-check :to-have-been-called-times 1))))

  (it "is not when another buffer is selected"
    (test-changes--with-medium
      (approve-session-put :checked nil)
      (spy-on 'approve-changes-check)
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (approve-changes--selected buffer))
      (expect 'approve-changes-check :not :to-have-been-called)))

  (it "is on selection, through the buffer's window hooks"
    (test-changes--with-medium
      (expect (memq #'approve-changes--on-window window-selection-change-functions)
              :to-be-truthy)
      (expect (memq #'approve-changes--on-window window-buffer-change-functions)
              :to-be-truthy)
      (expect (advice-function-member-p #'approve-changes--on-focus
                                        after-focus-change-function)
              :to-be-truthy)))

  (it "is every approve-poll-interval seconds while a window shows it"
    (test-changes--with-medium
      (let ((approve-changes--timer nil)
            (approve-poll-interval 1000)
            (checked nil))
        (spy-on 'approve-changes-check :and-call-fake
                (lambda () (push (current-buffer) checked)))
        (unwind-protect
            (progn
              (with-temp-buffer
                (set-window-buffer (selected-window) (current-buffer))
                (approve-changes--poll)
                (expect checked :to-be nil)
                (cancel-timer approve-changes--timer))
              (set-window-buffer (selected-window) (current-buffer))
              (approve-changes--poll)
              (expect checked :to-equal (list (current-buffer)))
              (expect (timerp approve-changes--timer) :to-be-truthy)
              (expect (timer--function approve-changes--timer) :to-be #'approve-changes--poll))
          (when (timerp approve-changes--timer)
            (cancel-timer approve-changes--timer)))))))

(describe "A buffer that shares a PR buffer's store"
  (it "checks the PR when it is selected, once it follows the selection"
    (test-changes--with-medium
      (let ((pr (current-buffer)))
        (approve-session-put :checked nil)
        (spy-on 'approve-changes-check)
        (with-temp-buffer
          (setq approve-model-store-buffer pr)
          (approve-changes-follow-selection)
          (expect (memq #'approve-changes--on-window window-selection-change-functions)
                  :to-be-truthy)
          (set-window-buffer (selected-window) (current-buffer))
          (approve-changes--selected (current-buffer))
          (expect (spy-context-current-buffer (spy-calls-most-recent 'approve-changes-check))
                  :to-be pr)
          (approve-changes-follow-selection 'stop)
          (expect (memq #'approve-changes--on-window window-selection-change-functions)
                  :to-be nil)))))

  (it "keeps its PR polled while a window shows it"
    (test-changes--with-medium
      (let ((pr (current-buffer))
            (approve-changes--timer nil)
            (approve-poll-interval 1000)
            (checked nil))
        (spy-on 'approve-changes-check :and-call-fake
                (lambda () (push (current-buffer) checked)))
        (unwind-protect
            (with-temp-buffer
              (setq approve-model-store-buffer pr)
              (set-window-buffer (selected-window) (current-buffer))
              (approve-changes--poll)
              (expect checked :to-equal (list pr)))
          (when (timerp approve-changes--timer)
            (cancel-timer approve-changes--timer))))))

  (it "hears of each check's answer through approve-changes-hook"
    (test-changes--with-medium
      (let* ((heard nil)
             (approve-changes-hook
              (list (lambda () (push (approve-session-get :changes) heard)))))
        (test-changes--check (test-changes--with (test-changes--remote-from-store)
                                                 'title "Renamed"))
        (expect (length heard) :to-equal 1)
        (expect (plist-get (car heard) :edited) :to-be t)))))

;;; test-approve-changes.el ends here
