;;; test-approve-merge.el --- Tests for the merge menu's commands -*- lexical-binding: t; -*-

;;; Commentary:

;; The merge menu (m, flow F13, decision D42) in the medium PR, run as
;; a user would: what each command checks before anything is sent, the
;; question or the commit message it asks for, what goes to GitHub (a
;; fake answers as GitHub would), and what the buffer shows after.  The
;; menu dims each command where it would refuse.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-merge)
(require 'approve-menus)
(require 'approve-test-helpers "helpers")

(defconst test-merge--pr "PR_kwDOPCDj7SnvM_RcFp"
  "The node id of the medium PR.")

(defvar test-merge--requests nil
  "The mutations sent to the fake GitHub, the last first.")

(defvar test-merge--answer nil
  "A function of a mutation request that returns GitHub's data.")

(defconst test-merge--message
  '((repository (pullRequest (viewerMergeHeadlineText . "Rewrite the parser (#202)")
                             (viewerMergeBodyText . "* Split the lexer\n\n* Schedule"))))
  "GitHub's commit message for a merge, as the query answers it.")

(defun test-merge--transport (replay)
  "Return a transport: mutations, the merge message and the head faked.
The rest is REPLAY's."
  (lambda (request)
    (let* ((name (or (plist-get request :name) ""))
           (data (cond ((string-prefix-p "mutations/" name)
                        (push request test-merge--requests)
                        (funcall test-merge--answer request))
                       ((equal name "queries/pull-request-merge-message.graphql")
                        (copy-tree test-merge--message))
                       ((equal name "queries/pull-request-changes.graphql")
                        `((repository (pullRequest (__typename . "PullRequest")
                                                   (id . ,test-merge--pr)
                                                   (headRefOid . "new-head"))))))))
      (if data
          (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                                    `((data . ,data)) nil)))
            (lambda () (cancel-timer timer)))
        (funcall replay request)))))

(defun test-merge--kill-inputs ()
  "Kill every input buffer, keeping no draft."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*approve-input" (buffer-name buffer))
      (with-current-buffer buffer
        (setq approve-input--done t))
      (kill-buffer buffer))))

(defmacro test-merge--with-pr (&rest body)
  "Run BODY in the medium PR's buffer, GitHub faked for what changes.
The PR can be merged: no conflicts, nothing blocks it."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((approve-api-transport (test-merge--transport approve-api-transport))
            (test-merge--requests nil)
            (test-merge--answer nil)
            (pr (approve-test-open-pr "medium")))
       (unwind-protect
           (progn
             (switch-to-buffer pr)
             (delete-other-windows)
             (with-current-buffer pr
               (test-merge--patch '(mergeStateStatus . "CLEAN"))
               ,@body))
         (test-merge--kill-inputs)
         (kill-buffer pr)))))

(defun test-merge--patch (&rest fields)
  "Patch the PR in the store with FIELDS, an alist."
  (approve-model-patch `((__typename . "PullRequest") (id . ,test-merge--pr) ,@fields)))

(defun test-merge--patch-repository (&rest fields)
  "Patch the PR's base repository in the store with FIELDS, an alist."
  (approve-model-patch `((__typename . "Repository")
                         (id . ,(alist-get 'id (approve-pr-field 'baseRepository)))
                         ,@fields)))

(defun test-merge--answer-with (mutation &rest fields)
  "Make GitHub answer MUTATION with the PR and FIELDS, an alist."
  (setq test-merge--answer
        (lambda (_request)
          `((,mutation (pullRequest (__typename . "PullRequest") (id . ,test-merge--pr)
                                    ,@fields))))))

(defun test-merge--wait ()
  "Wait until no action waits for GitHub."
  (approve-test-wait-for (lambda () (null (approve-session-get :inflight)))))

(defun test-merge--sent ()
  "Return the query files and the sorted variables of the mutations sent, in order."
  (reverse (mapcar (lambda (request)
                     (list (plist-get request :name)
                           (sort (copy-sequence (plist-get request :variables))
                                 (lambda (a b) (string< (car a) (car b))))))
                   test-merge--requests)))

(defun test-merge--input ()
  "Return the open input buffer, or nil."
  (seq-find (lambda (buffer) (string-prefix-p "*approve-input" (buffer-name buffer)))
            (buffer-list)))

(defun test-merge--dimmed ()
  "Return the keys of the merge menu dimmed now, in order."
  (let ((found nil))
    (cl-labels ((walk (x)
                  (cond ((and (consp x) (keywordp (car x)) (plist-member x :key))
                         (when-let* ((inapt (plist-get x :inapt-if)))
                           (when (funcall inapt)
                             (push (plist-get x :key) found))))
                        ((consp x) (walk (car x)) (walk (cdr x)))
                        ((vectorp x) (mapc #'walk x)))))
      (walk (get 'approve-merge 'transient--layout)))
    (nreverse found)))

(describe "The merge menu"
  (before-each
    (spy-on 'message)
    (spy-on 'approve-pr-refresh))

  (it "opens on m, and on gm in evil's normal state, from the dispatch too"
    (expect (keymap-lookup approve-pr-mode-map "m") :to-be 'approve-merge)
    (expect (get 'approve-merge 'transient--prefix) :to-be-truthy)
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state _map key def) (push (list state (key-description key) def) bound)))
                ((symbol-function 'evil-set-initial-state) #'ignore)
                ((symbol-function 'evil-add-command-properties) #'ignore))
        (load "approve-evil" nil t))
      (expect (member '(normal "g m" approve-merge) bound) :to-be-truthy)))

  (describe "its title line"
    (it "says what stands in the way, as GitHub's merge box does"
      (test-merge--with-pr
        (test-merge--patch '(mergeStateStatus . "BLOCKED"))
        (let ((title (substring-no-properties (approve-merge-summary))))
          (expect title :to-match "\\`Merge #202 into main   ")
          (expect title :to-match "no conflicts")
          (expect title :to-match "check")
          (expect title :to-match "1 change requested"))
        (test-merge--patch '(mergeable . "CONFLICTING") '(mergeStateStatus . "BEHIND")
                           '(autoMergeRequest (mergeMethod . "SQUASH")
                                              (enabledBy (__typename . "User") (id . "U_v")
                                                         (login . "vera-example"))))
        (let ((title (substring-no-properties (approve-merge-summary))))
          (expect title :to-match "conflicts")
          (expect title :to-match "behind main")
          (expect title :to-match "auto-merge (squash) by @vera-example"))))

    (it "says who merged it, or that it is closed"
      (test-merge--with-pr
        (test-merge--patch '(state . "MERGED") '(headRef)
                           '(mergedBy (__typename . "User") (id . "U_b") (login . "bob-example")))
        (expect (substring-no-properties (approve-merge-summary))
                :to-match "#202 is merged into main by @bob-example · parser-rewrite is deleted")
        (test-merge--patch '(state . "CLOSED"))
        (expect (substring-no-properties (approve-merge-summary)) :to-match "#202 is closed"))))

  (describe "approve-merge-why-not"
    (it "lets a pull request merge by the methods its repository allows"
      (test-merge--with-pr
        (expect (approve-merge-why-not "MERGE") :to-be nil)
        (expect (approve-merge-why-not "SQUASH") :to-be nil)
        (expect (approve-merge-why-not "REBASE")
                :to-equal "acme-example/rocket doesn't allow rebase merging")
        (expect (test-merge--dimmed) :to-equal '("r" "k" "x" "Q"))))

    (it "says what stands in the way, and which key helps"
      (test-merge--with-pr
        (cl-flet ((why (&rest fields)
                    (apply #'test-merge--patch fields)
                    (prog1 (approve-merge-why-not "SQUASH")
                      (test-merge--patch '(state . "OPEN") '(isDraft) '(mergeable . "MERGEABLE")
                                         '(mergeStateStatus . "CLEAN") '(isMergeQueueEnabled)
                                         '(viewerCanMergeAsAdmin)))))
          (expect (why '(state . "CLOSED")) :to-equal "#202 is closed; c reopens it")
          (expect (why '(state . "MERGED")) :to-equal "#202 is merged already")
          (expect (why '(isDraft . t)) :to-equal "#202 is a draft; D marks it ready for review")
          (expect (why '(mergeable . "CONFLICTING"))
                  :to-equal "#202 has conflicts with main; resolve them first")
          (expect (why '(mergeStateStatus . "UNKNOWN"))
                  :to-equal "GitHub is still working out whether #202 can be merged; g refreshes")
          (expect (why '(isMergeQueueEnabled . t))
                  :to-equal "main merges through a merge queue; Q adds #202 to it")
          (expect (why '(mergeStateStatus . "BLOCKED"))
                  :to-equal "The rules of main don't allow merging #202 yet; a merges it once they do")
          (expect (why '(mergeStateStatus . "BEHIND"))
                  :to-equal "#202 is behind main, whose rules want it up to date; u updates it")
          (expect (why '(mergeStateStatus . "BLOCKED") '(viewerCanMergeAsAdmin . t)) :to-be nil)
          (test-merge--patch-repository '(viewerPermission . "TRIAGE"))
          (expect (why) :to-equal "You can't merge into acme-example/rocket")))))

  (describe "m and s (approve-merge-commit, approve-merge-squash)"
    (it "write the commit message from GitHub's, then merge the head the store holds"
      (test-merge--with-pr
        (test-merge--answer-with 'mergePullRequest '(state . "MERGED") '(merged . t)
                                 '(closed . t))
        (let ((head (approve-pr-field 'headRefOid)))
          (call-interactively #'approve-merge-squash)
          (let ((input (test-merge--input)))
            (expect (with-current-buffer input (buffer-string))
                    :to-equal "Rewrite the parser (#202)\n\n* Split the lexer\n\n* Schedule")
            (with-current-buffer input
              (erase-buffer)
              (insert "Rewrite the parser\n\n\n")
              (approve-input-commit)))
          (test-merge--wait)
          (expect (test-merge--sent)
                  :to-equal `(("mutations/merge-pull-request.graphql"
                               ((commitBody . "") (commitHeadline . "Rewrite the parser")
                                (expectedHeadOid . ,head) (mergeMethod . "SQUASH")
                                (pullRequestId . ,test-merge--pr)))))
          (expect 'approve-pr-refresh :to-have-been-called)
          (expect (approve-pr-field 'state) :to-equal "MERGED"))))

    (it "merge with GitHub's message after a question with C-u"
      (test-merge--with-pr
        (test-merge--answer-with 'mergePullRequest '(state . "MERGED"))
        (spy-on 'y-or-n-p :and-return-value nil)
        (approve-merge-commit '(4))
        (expect 'y-or-n-p :to-have-been-called-with "Create a merge commit #202 into main? ")
        (expect test-merge--requests :to-be nil)
        (spy-on 'y-or-n-p :and-return-value t)
        (approve-merge-commit '(4))
        (test-merge--wait)
        (expect (cadr (car (test-merge--sent)))
                :to-equal `((expectedHeadOid . ,(approve-pr-field 'headRefOid))
                            (mergeMethod . "MERGE") (pullRequestId . ,test-merge--pr)))))

    (it "refuse what GitHub's merge box would, sending nothing"
      (test-merge--with-pr
        (test-merge--patch '(state . "CLOSED"))
        (expect (call-interactively #'approve-merge-squash)
                :to-throw 'user-error '("#202 is closed; c reopens it"))
        (expect (call-interactively #'approve-merge-rebase)
                :to-throw 'user-error '("#202 is closed; c reopens it"))
        (expect test-merge--requests :to-be nil)))

    (it "delete the branch after, with -d, when GitHub says the viewer may"
      (test-merge--with-pr
        (test-merge--patch-repository '(rebaseMergeAllowed . t))
        (let ((ref (alist-get 'id (approve-pr-field 'headRef))))
          (setq test-merge--answer
                (lambda (request)
                  (if (string-match-p "delete-ref" (plist-get request :name))
                      '((deleteRef (clientMutationId)))
                    `((mergePullRequest
                       (pullRequest (__typename . "PullRequest") (id . ,test-merge--pr)
                                    (state . "MERGED") (viewerCanDeleteHeadRef . t)))))))
          (spy-on 'y-or-n-p :and-return-value t)
          (spy-on 'transient-args :and-return-value '("--delete-branch"))
          (let ((transient-current-command 'approve-merge))
            (call-interactively #'approve-merge-rebase))
          (expect 'y-or-n-p :to-have-been-called-with
                  "Rebase and merge #202 into main and delete parser-rewrite? ")
          (test-merge--wait)
          (expect (mapcar #'car (test-merge--sent))
                  :to-equal '("mutations/merge-pull-request.graphql"
                              "mutations/delete-ref.graphql"))
          (expect (cadr (cadr (test-merge--sent))) :to-equal `((refId . ,ref)))
          (expect (approve-pr-field 'headRef) :to-be nil)))))

  (describe "a and x (auto-merge)"
    (it "ask for the method, the viewer's default first, then the message"
      (test-merge--with-pr
        (test-merge--patch '(mergeStateStatus . "BLOCKED"))
        (test-merge--answer-with 'enablePullRequestAutoMerge
                                 '(autoMergeRequest (mergeMethod . "MERGE"))
                                 '(viewerCanDisableAutoMerge . t))
        (let ((offered nil))
          (spy-on 'completing-read
                  :and-call-fake (lambda (_prompt table &rest more)
                                   (setq offered (list (all-completions "" table)
                                                       (nth 4 more)))
                                   "Create a merge commit"))
          (call-interactively #'approve-enable-auto-merge)
          (expect offered :to-equal '(("Squash and merge" "Create a merge commit")
                                      "Squash and merge")))
        (with-current-buffer (test-merge--input)
          (approve-input-commit))
        (test-merge--wait)
        (let ((variables (cadr (car (test-merge--sent)))))
          (expect (car (car (test-merge--sent)))
                  :to-equal "mutations/enable-pull-request-auto-merge.graphql")
          (expect (alist-get 'mergeMethod variables) :to-equal "MERGE")
          (expect (alist-get 'commitHeadline variables) :to-equal "Rewrite the parser (#202)"))
        (expect (test-merge--dimmed) :to-contain "a")
        (expect (test-merge--dimmed) :not :to-contain "x")
        (test-merge--answer-with 'disablePullRequestAutoMerge '(autoMergeRequest))
        (call-interactively #'approve-disable-auto-merge)
        (test-merge--wait)
        (expect (car (car (last (test-merge--sent))))
                :to-equal "mutations/disable-pull-request-auto-merge.graphql")
        (expect (approve-pr-field 'autoMergeRequest) :to-be nil)))

    (it "are dimmed where GitHub doesn't offer them"
      (test-merge--with-pr
        (test-merge--patch '(viewerCanEnableAutoMerge))
        (expect (approve-action-why-not 'approve-enable-auto-merge)
                :to-equal "#202 can be merged now, or you may not enable auto-merge")
        (test-merge--patch-repository '(autoMergeAllowed))
        (expect (approve-action-why-not 'approve-enable-auto-merge)
                :to-equal "acme-example/rocket doesn't allow auto-merge")
        (expect (approve-action-why-not 'approve-disable-auto-merge)
                :to-equal "Auto-merge is off"))))

  (describe "Q (approve-toggle-merge-queue)"
    (it "adds to the merge queue after asking, where the base has one"
      (test-merge--with-pr
        (expect (approve-action-why-not 'approve-toggle-merge-queue)
                :to-equal "main has no merge queue")
        (test-merge--patch '(isMergeQueueEnabled . t))
        (expect (approve-merge-why-not "SQUASH")
                :to-equal "main merges through a merge queue; Q adds #202 to it")
        (setq test-merge--answer
              (lambda (_request)
                `((enqueuePullRequest
                   (mergeQueueEntry
                    (__typename . "MergeQueueEntry") (id . "MQE_1")
                    (pullRequest (__typename . "PullRequest") (id . ,test-merge--pr)
                                 (isInMergeQueue . t)
                                 (mergeQueueEntry (__typename . "MergeQueueEntry")
                                                  (id . "MQE_1") (position . 3))))))))
        (spy-on 'y-or-n-p :and-return-value t)
        (call-interactively #'approve-toggle-merge-queue)
        (expect 'y-or-n-p :to-have-been-called-with "Add #202 to the merge queue of main? ")
        (test-merge--wait)
        (expect (car (car (test-merge--sent))) :to-equal "mutations/enqueue-pull-request.graphql")
        (expect (approve-menus--queue-description) :to-equal "Remove from merge queue")
        (expect (substring-no-properties (approve-merge-summary))
                :to-match "in the merge queue (#3)"))))

  (describe "u and U (update the branch)"
    (it "merge the base in, or rebase, after asking; the PR loads once the head moved"
      (test-merge--with-pr
        (test-merge--answer-with 'updatePullRequestBranch '(mergeable . "UNKNOWN"))
        (spy-on 'y-or-n-p :and-return-value t)
        (call-interactively #'approve-update-branch)
        (expect 'y-or-n-p :to-have-been-called-with "Merge main into parser-rewrite? ")
        (test-merge--wait)
        (expect (test-merge--sent)
                :to-equal `(("mutations/update-pull-request-branch.graphql"
                             ((expectedHeadOid . ,(approve-pr-field 'headRefOid))
                              (pullRequestId . ,test-merge--pr) (updateMethod . "MERGE")))))
        (expect 'approve-pr-refresh :to-have-been-called)
        (call-interactively #'approve-rebase-branch)
        (test-merge--wait)
        (expect (alist-get 'updateMethod (cadr (car (last (test-merge--sent)))))
                :to-equal "REBASE")))

    (it "need the right to push to the branch"
      (test-merge--with-pr
        (test-merge--patch '(viewerCanUpdateBranch) '(viewerCanEditFiles))
        (expect (approve-action-why-not 'approve-update-branch)
                :to-equal "You can't push to parser-rewrite"))))

  (describe "c and k (close, reopen, delete the branch)"
    (it "close after asking, reopen, and delete the branch of a closed one"
      (test-merge--with-pr
        (expect (approve-action-why-not 'approve-delete-branch)
                :to-equal "#202 is open: merge or close it first")
        (setq test-merge--answer
              (lambda (request)
                (pcase (plist-get request :name)
                  ("mutations/close-pull-request.graphql"
                   `((closePullRequest
                      (pullRequest (__typename . "PullRequest") (id . ,test-merge--pr)
                                   (state . "CLOSED") (closed . t) (viewerCanReopen . t)
                                   (viewerCanDeleteHeadRef . t)))))
                  ("mutations/delete-ref.graphql" '((deleteRef (clientMutationId)))))))
        (spy-on 'y-or-n-p :and-return-value t)
        (call-interactively #'approve-toggle-closed)
        (expect 'y-or-n-p :to-have-been-called-with "Close #202 without merging it? ")
        (test-merge--wait)
        (expect (substring-no-properties approve-pr--header-line) :to-match "Closed")
        (expect (approve-menus--close-description) :to-equal "Reopen pull request")
        (expect (test-merge--dimmed) :to-contain "m")
        (expect (test-merge--dimmed) :not :to-contain "k")
        (call-interactively #'approve-delete-branch)
        (expect 'y-or-n-p :to-have-been-called-with "Delete the branch parser-rewrite? ")
        (test-merge--wait)
        (expect (car (car (last (test-merge--sent)))) :to-equal "mutations/delete-ref.graphql")
        (expect (approve-action-why-not 'approve-delete-branch)
                :to-equal "parser-rewrite is deleted already")))

    (it "never reopen a merged one"
      (test-merge--with-pr
        (test-merge--patch '(state . "MERGED"))
        (expect (approve-action-why-not 'approve-toggle-closed)
                :to-equal "#202 is merged; it can't be reopened")))))

;;; test-approve-merge.el ends here
