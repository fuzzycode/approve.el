;;; approve-merge.el --- Merge, close and update a pull request -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/fuzzycode/approve.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; The commands of the merge menu, m (docs/flows.org, F13; docs/ui.org,
;; "Merge / state"; decision D42), each a run of the action pipeline
;; (F00):
;;
;;   m   `approve-merge-commit'        merge with a merge commit
;;   s   `approve-merge-squash'        squash and merge
;;   r   `approve-merge-rebase'        rebase and merge
;;   -d                                delete the branch after the merge
;;   a   `approve-enable-auto-merge'   merge once the base's rules allow it
;;   x   `approve-disable-auto-merge'  no more
;;   Q   `approve-toggle-merge-queue'  add to the merge queue, or take out
;;   u   `approve-update-branch'       merge the base into the branch
;;   U   `approve-rebase-branch'       rebase the branch on the base
;;   c   `approve-toggle-closed'       close the pull request, or reopen it
;;   k   `approve-delete-branch'       delete the branch of a merged or
;;                                     closed one
;;
;; m and s write the commit message first, in an input buffer that
;; starts from GitHub's own (asked for when the command runs: it
;; changes with each push); C-c C-c merges.  With C-u they merge with
;; GitHub's message after a question, as r does.  Each command checks
;; first what GitHub's merge box would say (`approve-merge-why-not'):
;; GitHub's API merges a closed pull request, and says little more than
;; "Pull Request is not mergeable".  The menu dims each command where
;; it would refuse (decision D06), and its title line is the merge box
;; in one line (`approve-merge-summary').
;;
;; Layer rules: this is the actions layer.  The commands read the
;; store, ask, and leave the rest to the pipeline and the mutations,
;; which keep the store true (decision D35).  A merge or an update of
;; the branch loads the PR again: they change its commits, diff and
;; conversation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'approve-model)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-sections)
(require 'approve-input)
(require 'approve-queries)
(require 'approve-mutations)
(require 'approve-actions)
(require 'approve-pr)
(require 'approve-manage)

;;; What the store says

(defconst approve-merge-methods
  '(("MERGE" mergeCommitAllowed "Create a merge commit" "merge commits")
    ("SQUASH" squashMergeAllowed "Squash and merge" "squash merging")
    ("REBASE" rebaseMergeAllowed "Rebase and merge" "rebase merging"))
  "The ways to merge a pull request: (METHOD SETTING NAME WHAT).
SETTING is the field of the base repository that allows METHOD; WHAT
names it in \"REPOSITORY doesn't allow WHAT\".")

(defun approve-merge--repository (field)
  "Return FIELD of the PR's base repository."
  (alist-get field (approve-pr-field 'baseRepository)))

(defun approve-merge--repository-name ()
  "Return the PR's base repository as OWNER/NAME."
  (or (approve-merge--repository 'nameWithOwner)
      (format "%s/%s" (approve-model-meta :owner) (approve-model-meta :repo))))

(defun approve-merge-allowed-p (method)
  "Return non-nil if the PR's repository allows merging by METHOD."
  (approve-merge--repository (nth 1 (assoc method approve-merge-methods))))

(defun approve-merge--name (method)
  "Return the name of METHOD, such as \"Squash and merge\"."
  (nth 2 (assoc method approve-merge-methods)))

(defun approve-merge--number ()
  "Return the PR as #NUMBER."
  (format "#%s" (approve-pr-field 'number)))

(defun approve-merge--base ()
  "Return the name of the PR's base branch."
  (approve-pr-field 'baseRefName))

(defun approve-merge--can-write-p ()
  "Return non-nil if the viewer may write to the PR's repository.
That is what merging needs; GitHub refuses those the base's rules
keep out."
  (member (approve-merge--repository 'viewerPermission) '("WRITE" "MAINTAIN" "ADMIN")))

(defun approve-merge--bypass-p ()
  "Return non-nil if merging bypasses the base's rules.
That is when they block the merge (BLOCKED, BEHIND) and the viewer
may bypass them (`viewerCanMergeAsAdmin')."
  (and (member (approve-pr-field 'mergeStateStatus) '("BLOCKED" "BEHIND"))
       (approve-pr-viewer-can 'merge-as-admin)))

(defun approve-merge--state-why-not ()
  "Return why the PR can't change now, being merged or closed, or nil."
  (pcase (approve-pr-field 'state)
    ("MERGED" (format "%s is merged already" (approve-merge--number)))
    ("CLOSED" (format "%s is closed; c reopens it" (approve-merge--number)))))

(defun approve-merge-why-not (method)
  "Return why the PR can't be merged by METHOD now, or nil when it can.
METHOD is \"MERGE\", \"SQUASH\" or \"REBASE\".  The reasons are GitHub's
merge box's, checked before anything is sent (decision D42), and say
which key helps when one does."
  (let ((number (approve-merge--number))
        (base (approve-merge--base)))
    (or (approve-merge--state-why-not)
        (and (not (approve-merge-allowed-p method))
             (format "%s doesn't allow %s" (approve-merge--repository-name)
                     (nth 3 (assoc method approve-merge-methods))))
        (and (not (approve-merge--can-write-p))
             (format "You can't merge into %s" (approve-merge--repository-name)))
        (and (approve-pr-field 'isDraft)
             (format "%s is a draft; D marks it ready for review" number))
        (and (approve-pr-field 'isMergeQueueEnabled)
             (format "%s merges through a merge queue; Q adds %s to it" base number))
        (and (equal (approve-pr-field 'mergeable) "CONFLICTING")
             (format "%s has conflicts with %s; resolve them first" number base))
        (and (equal method "REBASE") (not (approve-pr-field 'canBeRebased))
             (format "%s can't be rebased onto %s" number base))
        (and (or (equal (approve-pr-field 'mergeable) "UNKNOWN")
                 (equal (approve-pr-field 'mergeStateStatus) "UNKNOWN"))
             (format "GitHub is still working out whether %s can be merged; g refreshes"
                     number))
        (and (equal (approve-pr-field 'mergeStateStatus) "BLOCKED")
             (not (approve-merge--bypass-p))
             (concat (format "The rules of %s don't allow merging %s yet" base number)
                     (and (approve-pr-viewer-can 'enable-auto-merge)
                          "; a merges it once they do")))
        (and (equal (approve-pr-field 'mergeStateStatus) "BEHIND")
             (not (approve-merge--bypass-p))
             (format "%s is behind %s, whose rules want it up to date; u updates it"
                     number base)))))

;;; The title line

(defun approve-merge--item (icon face text)
  "Return TEXT after ICON, both in FACE."
  (concat (approve-icon icon face) " " (approve-face text face)))

(defun approve-merge--plural (count word &optional plural)
  "Return COUNT and WORD, or PLURAL (WORD with an s) when COUNT isn't 1."
  (format "%d %s" count (if (= count 1) word (or plural (concat word "s")))))

(defun approve-merge--verdicts (state)
  "Return how many reviewers' latest verdict is STATE."
  (seq-count (lambda (review) (equal (alist-get 'state review) state))
             (approve-pr-field 'latestOpinionatedReviews)))

(defun approve-merge--checks-state ()
  "Return `failing', `running' or `passed' for the PR's checks, or nil."
  (when-let* ((counts (approve-sections-check-counts)))
    (pcase-let ((`(,_passed ,failed ,running) counts))
      (cond ((> failed 0) 'failing)
            ((> running 0) 'running)
            (t 'passed)))))

(defun approve-merge--checks-item ()
  "Return what the checks say, for the title line, or nil."
  (when-let* ((counts (approve-sections-check-counts)))
    (pcase-let ((`(,_passed ,failed ,running) counts))
      (pcase (approve-merge--checks-state)
        ('failing
         (approve-merge--item 'check-failure 'approve-check-failure
                              (concat (approve-merge--plural failed "check") " failing")))
        ('running
         (approve-merge--item 'check-pending 'approve-check-pending
                              (concat (approve-merge--plural running "check") " running")))
        (_ (approve-merge--item 'check-success 'approve-check-success "checks"))))))

(defun approve-merge--reviews-item ()
  "Return what the reviews say, for the title line, or nil."
  (let ((approvals (approve-merge--verdicts "APPROVED"))
        (changes (approve-merge--verdicts "CHANGES_REQUESTED")))
    (pcase (approve-pr-field 'reviewDecision)
      ("CHANGES_REQUESTED"
       (approve-merge--item 'changes-requested 'approve-changes-requested
                            (concat (approve-merge--plural (max changes 1) "change")
                                    " requested")))
      ("REVIEW_REQUIRED"
       (approve-merge--item 'requested 'approve-requested "review required"))
      (_ (and (> approvals 0)
              (approve-merge--item 'approved 'approve-approved
                                   (approve-merge--plural approvals "approval")))))))

(defun approve-merge--open-items ()
  "Return the title line's items for an open PR, in order."
  (let* ((status (approve-pr-field 'mergeStateStatus))
         (auto (approve-pr-field 'autoMergeRequest))
         (entry (approve-pr-field 'mergeQueueEntry)))
    (list
     (and (approve-pr-field 'isDraft)
          (approve-merge--item 'draft 'approve-state-draft "draft"))
     (pcase (approve-pr-field 'mergeable)
       ("MERGEABLE" (approve-merge--item 'check-success 'approve-check-success "no conflicts"))
       ("CONFLICTING" (approve-merge--item 'check-failure 'approve-check-failure "conflicts"))
       (_ (approve-merge--item 'check-pending 'approve-check-pending
                               "checking for conflicts")))
     (approve-merge--checks-item)
     (approve-merge--reviews-item)
     (and (equal status "BEHIND")
          (approve-face (format "behind %s" (approve-merge--base)) 'approve-check-pending))
     ;; Blocked by a rule nothing above shows (a code owner's review).
     (and (equal status "BLOCKED")
          (not (member (approve-pr-field 'reviewDecision)
                       '("CHANGES_REQUESTED" "REVIEW_REQUIRED")))
          (not (memq (approve-merge--checks-state) '(failing running)))
          (approve-merge--item 'check-failure 'approve-check-failure
                               (format "blocked by the rules of %s" (approve-merge--base))))
     (and (approve-merge--bypass-p)
          (approve-face "you may bypass the rules" 'approve-dim))
     (and auto
          (let-alist auto
            (concat (approve-face (format "auto-merge (%s)" (downcase .mergeMethod))
                                  'approve-state-open)
                    (and .enabledBy
                         (concat " " (approve-face "by" 'approve-dim) " "
                                 (approve-render-user .enabledBy))))))
     (and entry
          (approve-face (format "in the merge queue (%s)"
                                (if (alist-get 'position entry)
                                    (format "#%s" (alist-get 'position entry))
                                  (downcase (or (alist-get 'state entry) "queued"))))
                        'approve-state-open)))))

(defun approve-merge-summary ()
  "Return the merge menu's title line: the PR's merge box in one line.
Such as \"Merge #12 into main   ✓ no conflicts · ✓ checks · ✓ 1
approval\", or who merged it, or that it is closed."
  (let ((number (approve-merge--number))
        (base (approve-merge--base)))
    (pcase (approve-pr-field 'state)
      ("MERGED"
       (concat (approve-icon 'merged 'approve-state-merged) " "
               (approve-face (format "%s is merged into %s" number base) 'approve-state-merged)
               (when-let* ((by (approve-pr-field 'mergedBy)))
                 (concat " " (approve-face "by" 'approve-dim) " " (approve-render-user by)))
               (unless (approve-pr-field 'headRef)
                 (approve-face (format " · %s is deleted" (approve-pr-field 'headRefName))
                               'approve-dim))))
      ("CLOSED"
       (concat (approve-icon 'closed 'approve-state-closed) " "
               (approve-face (format "%s is closed" number) 'approve-state-closed)))
      (_
       (concat (propertize (format "Merge %s into %s" number base) 'face 'transient-heading)
               "   "
               (approve-render-join (approve-merge--open-items)))))))

;;; Merging (m, s, r)

(defun approve-merge--preflight (method)
  "Return METHOD if the PR can be merged by it now; else signal why not."
  (approve-action-context)
  (when-let* ((why (approve-merge-why-not method)))
    (user-error "%s" why))
  method)

(put 'approve-merge-commit 'approve-preflight (lambda () (approve-merge--preflight "MERGE")))
(put 'approve-merge-squash 'approve-preflight (lambda () (approve-merge--preflight "SQUASH")))
(put 'approve-merge-rebase 'approve-preflight (lambda () (approve-merge--preflight "REBASE")))

(defun approve-merge-deletes-branches-p ()
  "Return non-nil if the PR's repository deletes a merged branch itself."
  (and (approve-model-root-ref) (approve-merge--repository 'deleteBranchOnMerge)))

(defun approve-merge--delete-branch-p ()
  "Return non-nil if the merge menu's -d asks to delete the branch.
Not where the repository deletes merged branches itself."
  (and (eq transient-current-command 'approve-merge)
       (member "--delete-branch" (transient-args 'approve-merge))
       (not (approve-merge-deletes-branches-p))))

(defun approve-merge--wait-message (method)
  "Return GitHub's commit message for merging by METHOD, as one text.
Its headline, a blank line, then its body.  Wait for it, saying so in
the echo area; \\[keyboard-quit] stops waiting.  Signal a
`user-error' when GitHub can't give it."
  (let ((answer nil)
        (failure nil))
    (approve-query-merge-message method
                                 :callback (lambda (message) (setq answer message))
                                 :errorback (lambda (err) (setq failure err)))
    (with-temp-message "Asking GitHub for the commit message..."
      (while (not (or answer failure))
        (accept-process-output nil 0.05)))
    (when failure
      (user-error "%s" (format "Could not get GitHub's commit message: %s"
                               (approve-api-error-message failure))))
    (string-trim (concat (car answer) "\n\n" (cdr answer)))))

(defun approve-merge-split-message (text)
  "Return TEXT, a commit message, as (HEADLINE . BODY).
The headline is the first line; the body the rest, without the blank
lines around it, \"\" when there is none."
  (let ((lines (split-string (string-trim text) "\n")))
    (cons (string-trim (car lines))
          (string-trim (string-join (cdr lines) "\n")))))

(defun approve-merge--question (what)
  "Return the question before WHAT, such as \"Squash and merge #12 into main\"."
  (concat what (and (approve-merge--bypass-p) ", bypassing its rules") "? "))

(defun approve-merge--send-merge (method delete)
  "Return the :send of a merge by METHOD.
Its text is the commit message; without one, GitHub writes its own.
With DELETE, the branch goes too once GitHub says the viewer may
delete it.  The result is (PR DELETED)."
  (lambda (text callback errorback)
    (let ((parts (and text (approve-merge-split-message text))))
      (approve-mutation-merge
       method :headline (car parts) :body (cdr parts)
       :errorback errorback
       :callback
       (lambda (pr)
         (if (not (and delete (approve-pr-viewer-can 'delete-head-ref)))
             (funcall callback (list pr nil))
           (approve-mutation-delete-head-branch
            :callback (lambda (_) (funcall callback (list pr t)))
            :errorback
            (lambda (err)
              (funcall errorback
                       (list 'approve-api-error
                             (format "merged, but %s was not deleted: %s"
                                     (approve-pr-field 'headRefName)
                                     (approve-api-error-message err))))))))))))

(defun approve-merge--merge (method default)
  "Merge the PR by METHOD after the message, or GitHub's with DEFAULT.
The command of m, s and r."
  (approve-merge--preflight method)
  (let* ((number (approve-merge--number))
         (base (approve-merge--base))
         (branch (approve-pr-field 'headRefName))
         (delete (approve-merge--delete-branch-p))
         (what (format "%s %s into %s" (approve-merge--name method) number base))
         (target (list 'merge-message method))
         (write (and (not default) (not (equal method "REBASE")))))
    (when (or write
              (y-or-n-p (approve-merge--question
                         (concat what (and delete (format " and delete %s" branch))))))
      (approve-action-run
       :what (format "merge %s" number)
       :input (and write
                   (list :prompt what
                         :context (if (approve-merge--bypass-p)
                                      "bypassing its rules"
                                    (concat "from " branch
                                            (and delete ", then deleting it")))
                         :target target :name (format "merge %s" number)
                         :initial (and (not (approve-input-draft target))
                                       (approve-merge--wait-message method))
                         :commit-label "merge"))
       :marks '((section approve-meta-section status))
       :send (approve-merge--send-merge method delete)
       :done (pcase-lambda (`(,_pr ,deleted))
               (message "Approve: %s is merged into %s%s" number base
                        (if deleted (format "; %s is deleted" branch) ""))
               (approve-pr-refresh))))))

(defun approve-merge-commit (&optional default)
  "Merge the pull request with a merge commit (flow F13).
Write the commit message first; it starts as GitHub's.  With a DEFAULT
prefix argument, merge with GitHub's message after a question.  With
-d in the merge menu, the branch is deleted after."
  (interactive "P")
  (approve-merge--merge "MERGE" default))

(defun approve-merge-squash (&optional default)
  "Squash the pull request's commits into one, on its base (flow F13).
Write the commit message first; it starts as GitHub's.  With a DEFAULT
prefix argument, squash with GitHub's message after a question.  With
-d in the merge menu, the branch is deleted after."
  (interactive "P")
  (approve-merge--merge "SQUASH" default))

(defun approve-merge-rebase ()
  "Rebase the pull request's commits onto its base, after a question (F13).
With -d in the merge menu, the branch is deleted after."
  (interactive)
  (approve-merge--merge "REBASE" t))

;;; Auto-merge (a, x)

(defun approve-merge--auto-merge-preflight ()
  "Return the methods auto-merge can use; signal why it can't be enabled."
  (approve-action-context)
  (when-let* ((why (approve-merge--state-why-not)))
    (user-error "%s" why))
  (when (approve-pr-field 'autoMergeRequest)
    (user-error "%s" "Auto-merge is on already; x disables it"))
  (unless (approve-pr-viewer-can 'enable-auto-merge)
    (user-error "%s" (if (approve-merge--repository 'autoMergeAllowed)
                         (format "%s can be merged now, or you may not enable auto-merge"
                                 (approve-merge--number))
                       (format "%s doesn't allow auto-merge"
                               (approve-merge--repository-name)))))
  (or (seq-filter #'approve-merge-allowed-p (mapcar #'car approve-merge-methods))
      (user-error "%s" "The repository allows no way to merge")))

(put 'approve-enable-auto-merge 'approve-preflight #'approve-merge--auto-merge-preflight)

(defun approve-merge--read-method (methods)
  "Return one of METHODS, asking which when there are more than one.
The viewer's default method is offered first."
  (if (null (cdr methods))
      (car methods)
    (let* ((default (approve-merge--repository 'viewerDefaultMergeMethod))
           (methods (if (member default methods)
                        (cons default (remove default methods))
                      methods))
           (choice (approve-manage-read-one
                    "Merge it, once it may, by: " (mapcar #'approve-merge--name methods)
                    :category 'approve-merge-method
                    :default (approve-merge--name (car methods)))))
      (car (seq-find (lambda (entry) (equal (nth 2 entry) choice)) approve-merge-methods)))))

(defun approve-enable-auto-merge (&optional default)
  "Merge the pull request once the base's rules allow it (flow F13).
Choose the method, when the repository allows more than one, then
write the commit message, starting as GitHub's.  With a DEFAULT
prefix argument, GitHub's message is used.  \\[approve-disable-auto-merge] in the merge
menu disables it."
  (interactive "P")
  (let* ((method (approve-merge--read-method (approve-merge--auto-merge-preflight)))
         (number (approve-merge--number))
         (target (list 'auto-merge-message method))
         (write (and (not default) (not (equal method "REBASE")))))
    (approve-action-run
     :what "enable auto-merge"
     :input (and write
                 (list :prompt (format "Auto-merge %s: %s" number
                                       (downcase (approve-merge--name method)))
                       :context (format "once %s's rules allow it" (approve-merge--base))
                       :target target :name (format "auto-merge %s" number)
                       :initial (and (not (approve-input-draft target))
                                     (approve-merge--wait-message method))
                       :commit-label "enable"))
     :marks '((section approve-meta-section status))
     :send (lambda (text callback errorback)
             (let ((parts (and text (approve-merge-split-message text))))
               (approve-mutation-set-auto-merge t :method method
                                                :headline (car parts) :body (cdr parts)
                                                :callback callback :errorback errorback)))
     :done (lambda (_pr)
             (message "Approve: %s will be merged (%s) once %s's rules allow it" number
                      (downcase (approve-merge--name method)) (approve-merge--base))))))

(defun approve-merge--disable-auto-merge-preflight ()
  "Signal why auto-merge can't be disabled, or return the context."
  (prog1 (approve-action-context)
    (unless (approve-pr-field 'autoMergeRequest)
      (user-error "%s" "Auto-merge is off"))
    (unless (approve-pr-viewer-can 'disable-auto-merge)
      (user-error "%s" "You can't disable auto-merge"))))

(put 'approve-disable-auto-merge 'approve-preflight
     #'approve-merge--disable-auto-merge-preflight)

(defun approve-disable-auto-merge ()
  "Merge the pull request no more once the base's rules allow it (flow F13)."
  (interactive)
  (approve-merge--disable-auto-merge-preflight)
  (let ((number (approve-merge--number)))
    (approve-action-run
     :what "disable auto-merge"
     :marks '((section approve-meta-section status))
     :send (lambda (_text callback errorback)
             (approve-mutation-set-auto-merge nil :callback callback :errorback errorback))
     :done (lambda (_pr) (message "Approve: auto-merge of %s is off" number)))))

;;; The merge queue (Q)

(defun approve-merge--queue-preflight ()
  "Return the context where `approve-toggle-merge-queue' runs, or signal why not."
  (prog1 (approve-action-context)
    (when-let* ((why (approve-merge--state-why-not)))
      (user-error "%s" why))
    (unless (approve-pr-field 'isMergeQueueEnabled)
      (user-error "%s has no merge queue" (approve-merge--base)))
    (unless (approve-merge--can-write-p)
      (user-error "%s" (format "You can't merge into %s"
                               (approve-merge--repository-name))))
    (when (and (approve-pr-field 'isDraft) (not (approve-pr-field 'isInMergeQueue)))
      (user-error "%s is a draft; D marks it ready for review" (approve-merge--number)))))

(put 'approve-toggle-merge-queue 'approve-preflight #'approve-merge--queue-preflight)

(defun approve-toggle-merge-queue ()
  "Add the pull request to its base's merge queue, or take it out (flow F13).
It asks first: the queue merges it once its checks pass."
  (interactive)
  (approve-merge--queue-preflight)
  (let ((queued (approve-pr-field 'isInMergeQueue))
        (number (approve-merge--number))
        (base (approve-merge--base)))
    (when (y-or-n-p (if queued
                        (format "Take %s out of the merge queue? " number)
                      (format "Add %s to the merge queue of %s? " number base)))
      (approve-action-run
       :what (if queued "take it out of the merge queue" "add it to the merge queue")
       :marks '((section approve-meta-section status))
       :send (lambda (_text callback errorback)
               (approve-mutation-set-queued (not queued)
                                            :callback callback :errorback errorback))
       :done (lambda (_pr)
               (message "Approve: %s is %s the merge queue" number
                        (if queued "out of" "in")))))))

;;; Updating the branch (u, U)

(defun approve-merge--update-preflight ()
  "Return the context where the branch can be updated, or signal why not.
GitHub's `viewerCanUpdateBranch' follows a setting of the repository,
so the right to push to the branch is enough (decision D42)."
  (prog1 (approve-action-context)
    (when-let* ((why (approve-merge--state-why-not)))
      (user-error "%s" why))
    (unless (approve-pr-field 'headRef)
      (user-error "%s" "The pull request's branch is gone"))
    (unless (or (approve-pr-viewer-can 'update-branch) (approve-pr-viewer-can 'edit-files))
      (user-error "%s" (format "You can't push to %s" (approve-pr-field 'headRefName))))))

(put 'approve-update-branch 'approve-preflight #'approve-merge--update-preflight)
(put 'approve-rebase-branch 'approve-preflight #'approve-merge--update-preflight)

(defun approve-merge--update (rebase)
  "Bring the branch up to date with the base, by a rebase with REBASE.
The command of u and U."
  (approve-merge--update-preflight)
  (let ((branch (approve-pr-field 'headRefName))
        (base (approve-merge--base))
        (head (approve-pr-field 'headRefOid)))
    (when (y-or-n-p (if rebase
                        (format "Rebase %s on %s?  It rewrites the branch's commits " branch base)
                      (format "Merge %s into %s? " base branch)))
      (approve-action-run
       :what (if rebase (format "rebase %s" branch) (format "update %s" branch))
       :marks '((section approve-meta-section branch))
       :send (lambda (_text callback errorback)
               (approve-mutation-update-branch
                :rebase rebase :errorback errorback
                :callback (lambda (pr)
                            ;; GitHub makes the commits after it answers.
                            (approve-query-await-head
                             head :away t :callback (lambda () (funcall callback pr))))))
       :done (lambda (_pr)
               (message "Approve: %s is %s %s" branch (if rebase "rebased on" "up to date with")
                        base)
               (approve-pr-refresh))))))

(defun approve-update-branch ()
  "Merge the base branch into the pull request's branch (flow F13).
It asks first.  The pull request loads again once GitHub has the
merge commit."
  (interactive)
  (approve-merge--update nil))

(defun approve-rebase-branch ()
  "Rebase the pull request's branch on its base (flow F13).
It asks first: the branch's commits are written anew.  The pull
request loads again once GitHub has them."
  (interactive)
  (approve-merge--update t))

;;; Closing and reopening (c)

(defun approve-merge--close-preflight ()
  "Return the context where the PR can be closed or reopened, or signal why not."
  (prog1 (approve-action-context)
    (pcase (approve-pr-field 'state)
      ("MERGED" (user-error "%s" (format "%s is merged; it can't be reopened"
                                         (approve-merge--number))))
      ("CLOSED" (unless (approve-pr-viewer-can 'reopen)
                  (user-error "%s" "You can't reopen this pull request")))
      (_ (unless (approve-pr-viewer-can 'close)
           (user-error "%s" "You can't close this pull request"))))))

(put 'approve-toggle-closed 'approve-preflight #'approve-merge--close-preflight)

(defun approve-toggle-closed ()
  "Close the pull request without merging it, or reopen it (flow F13).
It asks first."
  (interactive)
  (approve-merge--close-preflight)
  (let ((closed (equal (approve-pr-field 'state) "CLOSED"))
        (number (approve-merge--number)))
    (when (y-or-n-p (if closed
                        (format "Reopen %s? " number)
                      (format "Close %s without merging it? " number)))
      (approve-action-run
       :what (if closed "reopen it" "close it")
       :marks '((section approve-meta-section status))
       :send (lambda (_text callback errorback)
               (approve-mutation-set-closed (not closed)
                                            :callback callback :errorback errorback))
       :done (lambda (_pr)
               (message "Approve: %s is %s" number (if closed "open again" "closed")))))))

;;; Deleting the branch (k)

(defun approve-merge--delete-preflight ()
  "Return the branch `approve-delete-branch' would delete, or signal why not."
  (approve-action-context)
  (unless (approve-pr-field 'headRef)
    (user-error "%s is deleted already" (approve-pr-field 'headRefName)))
  (when (equal (approve-pr-field 'state) "OPEN")
    (user-error "%s is open: merge or close it first" (approve-merge--number)))
  (unless (approve-pr-viewer-can 'delete-head-ref)
    (user-error "%s" (format "You can't delete %s" (approve-pr-field 'headRefName))))
  (approve-pr-field 'headRefName))

(put 'approve-delete-branch 'approve-preflight #'approve-merge--delete-preflight)

(defun approve-delete-branch ()
  "Delete the branch of the merged or closed pull request (flow F13).
It asks first."
  (interactive)
  (let ((branch (approve-merge--delete-preflight)))
    (when (y-or-n-p (format "Delete the branch %s? " branch))
      (approve-action-run
       :what (format "delete %s" branch)
       :marks '((section approve-meta-section branch))
       :send (lambda (_text callback errorback)
               (approve-mutation-delete-head-branch :callback callback :errorback errorback))
       :done (lambda (_pr) (message "Approve: %s is deleted" branch))))))

(provide 'approve-merge)

;;; approve-merge.el ends here
