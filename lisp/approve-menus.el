;;; approve-menus.el --- Transient menus of Approve -*- lexical-binding: t; -*-

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

;; The transients of Approve (decision D06; docs/ui.org,
;; "Transients"):
;;
;;   ?  `approve-dispatch'       every command of a PR buffer, by what
;;                               it works on
;;   ?  `approve-dashboard-dispatch'  every command of the dashboard
;;   ?  `approve-file-dispatch'  every command of a file view
;;   j  `approve-jump'           the sections of the PR, the next work
;;                               (unviewed files, unresolved threads)
;;                               and any changed file by name
;;   S  `approve-submit-review'  a verdict for the pending review, its
;;                               body, or discarding it (flow F09)
;;   m  `approve-merge'          merge, auto-merge, the merge queue, the
;;                               branch, close or reopen (flow F13)
;;
;; The dispatch lists the commands that exist, no more: a new command
;; joins it, and any other menu it fits, in the change that adds it.
;; Every command bound in a PR buffer, or in the dashboard, is
;; reachable from its dispatch, with the key it has there; a few that
;; are seldom used have a key in the dispatch alone (# locks the
;; conversation).
;;
;; Suffixes that do not apply are shown dimmed, so a menu never
;; changes shape.  A command that changes the PR says itself whether
;; it can run here: its preflight (`approve-action-why-not').
;;
;; Layer rules: this is the entry points and menus layer.  It only
;; arranges commands defined below it.

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'approve-model)
(require 'approve-pr)
(require 'approve-nav)
(require 'approve-actions)
(require 'approve-review)
(require 'approve-commands)
(require 'approve-manage)
(require 'approve-merge)
(require 'approve-suggestions)
(require 'approve-eldoc)
(require 'approve-dashboard)
(require 'approve-file)
(require 'approve-faces)

(defun approve-menus--no-pending-review-p ()
  "Return non-nil if the viewer has no pending review."
  (not (approve-pr-pending-review)))

;;;###autoload (autoload 'approve-jump "approve-menus" nil t)
(transient-define-prefix approve-jump ()
  "Jump to a part of the pull request, or to the next thing to review."
  [["Sections"
    ("d" "Description" approve-jump-description)
    ("c" "Conversation" approve-jump-conversation)
    ("k" "Checks" approve-jump-checks)
    ("m" "Commits" approve-jump-commits)
    ("f" "Files" approve-jump-files)
    ("p" "Pending review" approve-jump-pending-review
     :inapt-if approve-menus--no-pending-review-p)]
   ["Next to review"
    ("u" "Next unviewed file" approve-next-unviewed-file)
    ("U" "Previous unviewed file" approve-previous-unviewed-file)
    ("t" "Next unresolved thread" approve-next-thread)
    ("T" "Previous unresolved thread" approve-previous-thread)]
   ["Find"
    ("/" "File…" approve-jump-file)]])

(keymap-set approve-pr-mode-map "j" #'approve-jump)

;;; Submit review (S)

(defun approve-menus--submit-heading ()
  "Return the heading of the submit menu: the PR and the pending review."
  (format "Submit review  #%s  %s" (approve-pr-field 'number)
          (propertize (if (approve-pr-pending-review)
                          (concat "pending: " (approve-review-summary))
                        (approve-review-summary))
                      'face 'approve-dim)))

(defun approve-menus--body-description ()
  "Return the body suffix's description, with the start of the body."
  (if-let* ((body (approve-review-body)))
      (format "Edit body…  %s"
              (propertize (format "\"%s\"" (truncate-string-to-width
                                             (car (split-string body "\n")) 40 nil nil "…"))
                          'face 'approve-dim))
    "Edit body…"))

(defun approve-menus--approve-inapt-p ()
  "Return non-nil if the viewer may not approve."
  (approve-review-why-not-verdict "APPROVE"))

(defun approve-menus--request-changes-inapt-p ()
  "Return non-nil if the viewer may not request changes."
  (approve-review-why-not-verdict "REQUEST_CHANGES"))

;;;###autoload (autoload 'approve-submit-review "approve-menus" nil t)
(transient-define-prefix approve-submit-review ()
  "Submit your review of the pull request, or discard it (flow F09).
A verdict asks for the review's body, starting from its draft; with
\\[universal-argument] before it, the review is submitted without a body.  With no pending
review, a verdict adds a review and submits it at once."
  [:description approve-menus--submit-heading
   ["Verdict"
    ("a" "Approve" approve-submit-approve :inapt-if approve-menus--approve-inapt-p)
    ("r" "Request changes" approve-submit-request-changes
     :inapt-if approve-menus--request-changes-inapt-p)
    ("c" "Comment" approve-submit-comment)]
   ["Body"
    ("e" approve-edit-review-body :description approve-menus--body-description)]
   ["Other"
    ("X" "Discard pending review" approve-discard-review
     :inapt-if approve-menus--no-pending-review-p)]])

(keymap-set approve-pr-mode-map "S" #'approve-submit-review)
(put 'approve-submit-review 'approve-preflight #'approve-action-context)

;;; Merge (m)

(defun approve-menus--merge-heading ()
  "Return the heading of the merge menu: the PR's merge box in one line."
  (if (approve-model-root-ref)
      (approve-merge-summary)
    (propertize "Merge" 'face 'transient-heading)))

(defun approve-menus--delete-branch-description ()
  "Return the description of -d, saying when the repository deletes itself."
  (if (approve-merge-deletes-branches-p)
      (concat "Delete branch after merge "
              (propertize "(the repository does)" 'face 'approve-dim))
    "Delete branch after merge"))

(defun approve-menus--draft-description ()
  "Return the description of `approve-toggle-draft': what it will do."
  (if (and (approve-model-root-ref) (approve-pr-field 'isDraft))
      "Mark ready for review"
    "Convert to draft"))

(defun approve-menus--close-description ()
  "Return the description of `approve-toggle-closed': what it will do."
  (if (and (approve-model-root-ref) (equal (approve-pr-field 'state) "CLOSED"))
      "Reopen pull request"
    "Close pull request"))

(defun approve-menus--queue-description ()
  "Return the description of `approve-toggle-merge-queue': what it will do."
  (if (and (approve-model-root-ref) (approve-pr-field 'isInMergeQueue))
      "Remove from merge queue"
    "Add to merge queue"))

;;;###autoload (autoload 'approve-merge "approve-menus" nil t)
(transient-define-prefix approve-merge ()
  "Merge the pull request, close it, or bring its branch up to date (flow F13).
The title line says what stands in the way, as GitHub's merge box
does; what can't run now is dimmed.  Merging with a merge commit or a
squash asks for the commit message first, starting as GitHub's; with
\\[universal-argument] before the key, GitHub's is used after a question (decision D42)."
  [:description approve-menus--merge-heading
   ["Method"
    ("m" "Create a merge commit" approve-merge-commit
     :inapt-if (lambda () (approve-action-why-not 'approve-merge-commit)))
    ("s" "Squash and merge" approve-merge-squash
     :inapt-if (lambda () (approve-action-why-not 'approve-merge-squash)))
    ("r" "Rebase and merge" approve-merge-rebase
     :inapt-if (lambda () (approve-action-why-not 'approve-merge-rebase)))]
   ["Options"
    ("-d" "Delete branch after merge" "--delete-branch"
     :description approve-menus--delete-branch-description
     :inapt-if approve-merge-deletes-branches-p)]
   ["State"
    ("D" approve-toggle-draft :description approve-menus--draft-description
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-draft)))
    ("c" approve-toggle-closed :description approve-menus--close-description
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-closed)))
    ("u" "Update branch" approve-update-branch
     :inapt-if (lambda () (approve-action-why-not 'approve-update-branch)))
    ("U" "Rebase branch" approve-rebase-branch
     :inapt-if (lambda () (approve-action-why-not 'approve-rebase-branch)))
    ("k" "Delete branch" approve-delete-branch
     :inapt-if (lambda () (approve-action-why-not 'approve-delete-branch)))]]
  [["When blocked"
    ("a" "Enable auto-merge" approve-enable-auto-merge
     :inapt-if (lambda () (approve-action-why-not 'approve-enable-auto-merge)))]
   [""
    ("x" "Disable auto-merge" approve-disable-auto-merge
     :inapt-if (lambda () (approve-action-why-not 'approve-disable-auto-merge)))]
   [""
    ("Q" approve-toggle-merge-queue :description approve-menus--queue-description
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-merge-queue)))]])

(keymap-set approve-pr-mode-map "m" #'approve-merge)
(put 'approve-merge 'approve-preflight #'approve-action-context)

;;; Dispatch (?)

(defun approve-menus--dispatch-heading ()
  "Return the heading of the dispatch menu: the number and title of the PR."
  (concat (propertize "Approve" 'face 'transient-heading) "  "
          (propertize (string-trim-right
                       (format "#%s %s" (approve-model-meta :number)
                               (truncate-string-to-width
                                (or (and (approve-model-root-ref) (approve-pr-field 'title)) "")
                                60 nil nil "…")))
                      'face 'approve-title)))

(defun approve-menus--threads-description ()
  "Return the description of `approve-toggle-threads': what it will do."
  (if (eq (approve-session-get :threads) 'folded)
      "Unfold all threads"
    "Fold all threads"))

(defun approve-menus--lock-description ()
  "Return the description of `approve-toggle-lock': what it will do."
  (if (and (approve-model-root-ref) (approve-pr-field 'locked))
      "Unlock conversation"
    "Lock conversation"))

(defun approve-menus--commit-suggestion-description ()
  "Return the description of `approve-commit-suggestion': what it commits."
  (let ((count (length (and (approve-model-root-ref) (approve-suggestions-batch)))))
    (if (> count 0)
        (format "Commit the batch (%d)" count)
      "Commit suggestion")))

(defun approve-menus--batch-suggestion-description ()
  "Return the description of `approve-batch-suggestion': what it will do."
  (if (and (approve-model-root-ref) (approve-suggestions-batched-at-point-p))
      "Take out of batch"
    "Add to batch"))

;;;###autoload (autoload 'approve-dispatch "approve-menus" nil t)
(transient-define-prefix approve-dispatch ()
  "Show the commands of the pull request buffer (decision D06).
Those that can't run at point, or that you may not use, are dimmed."
  [:description approve-menus--dispatch-heading
   ["Review"
    ("c" "Comment at point" approve-comment
     :inapt-if (lambda () (approve-action-why-not 'approve-comment)))
    ("s" "Suggest change" approve-suggest
     :inapt-if (lambda () (approve-action-why-not 'approve-suggest)))
    ("x" approve-commit-suggestion
     :description approve-menus--commit-suggestion-description
     :inapt-if (lambda () (approve-action-why-not 'approve-commit-suggestion)))
    ("b" approve-batch-suggestion
     :description approve-menus--batch-suggestion-description
     :inapt-if (lambda () (approve-action-why-not 'approve-batch-suggestion)))
    ("v" "Toggle viewed" approve-toggle-viewed
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-viewed)))
    ("S" "Submit review…" approve-submit-review
     :inapt-if (lambda () (approve-action-why-not 'approve-submit-review)))
    ("X" "Discard pending review" approve-discard-review
     :inapt-if (lambda () (approve-action-why-not 'approve-discard-review)))]
   ["Comments"
    ("r" "Reply" approve-reply
     :inapt-if (lambda () (approve-action-why-not 'approve-reply)))
    ("R" "Resolve / unresolve" approve-resolve
     :inapt-if (lambda () (approve-action-why-not 'approve-resolve)))
    ("e" "Edit at point" approve-edit
     :inapt-if (lambda () (approve-action-why-not 'approve-edit)))
    ("k" "Delete at point" approve-delete
     :inapt-if (lambda () (approve-action-why-not 'approve-delete)))
    ("+" "React" approve-react
     :inapt-if (lambda () (approve-action-why-not 'approve-react)))
    ("H" "Hide / unhide" approve-hide
     :inapt-if (lambda () (approve-action-why-not 'approve-hide)))
    ("C" "Conversation comment" approve-comment-on-pr
     :inapt-if (lambda () (approve-action-why-not 'approve-comment-on-pr)))]
   ["Pull request"
    ("t" "Title" approve-edit-title
     :inapt-if (lambda () (approve-action-why-not 'approve-edit-title)))
    ("d" "Description" approve-edit-description
     :inapt-if (lambda () (approve-action-why-not 'approve-edit-description)))
    ("l" "Labels" approve-set-labels
     :inapt-if (lambda () (approve-action-why-not 'approve-set-labels)))
    ("a" "Assignees" approve-set-assignees
     :inapt-if (lambda () (approve-action-why-not 'approve-set-assignees)))
    ("A" "Request reviewers" approve-request-reviewers
     :inapt-if (lambda () (approve-action-why-not 'approve-request-reviewers)))
    ("M" "Milestone" approve-set-milestone
     :inapt-if (lambda () (approve-action-why-not 'approve-set-milestone)))
    ("D" "Draft / ready" approve-toggle-draft
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-draft)))
    ("w" "Subscription" approve-set-subscription
     :inapt-if (lambda () (approve-action-why-not 'approve-set-subscription)))
    ("#" approve-toggle-lock :description approve-menus--lock-description
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-lock)))
    ("m" "Merge / close…" approve-merge
     :inapt-if (lambda () (approve-action-why-not 'approve-merge)))]
   ["Buffer"
    ("g" "Refresh" approve-pr-refresh)
    ("RET" "Visit at point" approve-visit)
    ("F" "Whole file" approve-visit-file
     :inapt-if (lambda () (approve-action-why-not 'approve-visit-file)))
    ("o" "Open in browser" approve-browse)
    ("y" "Copy URL / sha" approve-copy)
    ("j" "Jump…" approve-jump)
    ("K" "Details (eldoc)" approve-show-details)
    ("T" approve-toggle-threads :description approve-menus--threads-description)
    ("O" "Fold all but this" approve-fold-others)]])

(keymap-set approve-pr-mode-map "?" #'approve-dispatch)

;;; File view dispatch (?)

(defun approve-menus--file-heading ()
  "Return the heading of the file view's dispatch: the file and its version."
  (concat (propertize "Approve" 'face 'transient-heading) "  "
          (propertize (or (plist-get approve-context-file :name) "") 'face 'approve-title)
          "  " (propertize (approve-file-version-label) 'face 'approve-dim)))

(defun approve-menus--version-description ()
  "Return the description of `approve-file-toggle-version': what it will do."
  (if (equal (plist-get approve-context-file :side) "LEFT")
      "Show the head"
    "Show the base"))

;;;###autoload (autoload 'approve-file-dispatch "approve-menus" nil t)
(transient-define-prefix approve-file-dispatch ()
  "Show the commands of the file view (decision D06).
Those that can't run at point are dimmed.  The threads are acted on
in the diff, which RET goes to (decision D43)."
  [:description approve-menus--file-heading
   ["Move"
    ("n" "Next change" approve-file-next-change)
    ("p" "Previous change" approve-file-previous-change)
    ("]t" "Next unresolved thread" approve-file-next-thread)
    ("[t" "Previous unresolved thread" approve-file-previous-thread)
    ("RET" "Go to the diff" approve-visit)]
   ["Review"
    ("c" "Comment at point" approve-comment
     :inapt-if (lambda () (approve-action-why-not 'approve-comment)))
    ("s" "Suggest change" approve-suggest
     :inapt-if (lambda () (approve-action-why-not 'approve-suggest)))
    ("v" "Toggle viewed" approve-toggle-viewed
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-viewed)))]
   ["Buffer"
    ("TAB" "Fold the line's threads" approve-file-toggle-threads)
    ("b" approve-file-toggle-version :description approve-menus--version-description
     :inapt-if (lambda () (approve-action-why-not 'approve-file-toggle-version)))
    ("g" "Refresh" approve-file-refresh)
    ("o" "Open in browser" approve-browse)
    ("y" "Copy path:line / URL" approve-copy)
    ("q" "Back" quit-window)]])

(keymap-set approve-file-mode-map "?" #'approve-file-dispatch)

;;; Dashboard dispatch (?)

;;;###autoload (autoload 'approve-dashboard-dispatch "approve-menus" nil t)
(transient-define-prefix approve-dashboard-dispatch ()
  "Show the commands of the dashboard (decision D06).
Those that can't run at point are dimmed."
  [:description (lambda () (concat (propertize "Approve" 'face 'transient-heading) "  "
                                   (propertize "Dashboard" 'face 'approve-title)))
   ["Pull request"
    ("RET" "Open" approve-visit)
    ("o" "Open in browser" approve-browse)
    ("y" "Copy URL / search" approve-copy)
    ("K" "Details (eldoc)" approve-show-details)]
   ["Lists"
    ("g" "Refresh all" approve-dashboard-refresh)
    ("G" "Refresh list at point" approve-dashboard-refresh-section
     :inapt-if (lambda () (approve-action-why-not 'approve-dashboard-refresh-section)))
    ("T" approve-dashboard-toggle-lists
     :description (lambda () (if (approve-dashboard-lists-folded-p)
                                 "Unfold all lists"
                               "Fold all lists")))
    ("O" "Fold other lists" approve-fold-others)]])

(keymap-set approve-dashboard-mode-map "?" #'approve-dashboard-dispatch)

(provide 'approve-menus)

;;; approve-menus.el ends here
