;;; approve-conversation.el --- The conversation of a pull request -*- lexical-binding: t; -*-

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

;; The Conversation section: the PR's timeline as GitHub's conversation
;; tab shows it (docs/ui.org, "Middle: checks, commits, conversation"):
;; conversation comments, reviews, pushes and events, on a line that
;; runs from "opened this" to the end.
;;
;; Each item is a section of its own: `approve-issue-comment-section',
;; `approve-review-section' or `approve-event-section', valued by the
;; node id of its (first) timeline item.  Runs of small things are
;; folded into one item: consecutive commits into "pushed N commits",
;; consecutive replies of one person into one "reviewed".  The event
;; types shown are `approve-timeline-event-types'.
;;
;; Layer rules: this is the renderer layer.  It reads the store
;; through selectors and inserts text; it never changes the store or
;; calls the network.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-model)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-html)
(require 'approve-render)
(require 'approve-thread)
(require 'approve-pr)
(require 'approve-sections)

(defcustom approve-timeline-event-types
  '("IssueComment" "PullRequestReview" "PullRequestCommit"
    "HeadRefForcePushedEvent" "BaseRefForcePushedEvent"
    "LabeledEvent" "UnlabeledEvent" "AssignedEvent" "UnassignedEvent"
    "RenamedTitleEvent" "MilestonedEvent" "DemilestonedEvent"
    "ReadyForReviewEvent" "ConvertToDraftEvent"
    "ReviewRequestedEvent" "ReviewRequestRemovedEvent" "ReviewDismissedEvent"
    "BaseRefChangedEvent" "HeadRefDeletedEvent" "HeadRefRestoredEvent"
    "MergedEvent" "ClosedEvent" "ReopenedEvent" "CrossReferencedEvent")
  "Timeline items the Conversation section shows, by GraphQL type name.
To hide label and assignment events:

  (setq approve-timeline-event-types
        (seq-difference approve-timeline-event-types
                        \\='(\"LabeledEvent\" \"UnlabeledEvent\"
                          \"AssignedEvent\" \"UnassignedEvent\")))"
  :type '(repeat string)
  :group 'approve)

(defclass approve-conversation-section (magit-section) ()
  "The conversation of the pull request.")
(defclass approve-issue-comment-section (magit-section) ()
  "A conversation comment; its value is the comment id.")
(defclass approve-review-section (magit-section) ()
  "A submitted review; its value is the review id.")
(defclass approve-event-section (magit-section) ()
  "Something that happened to the PR; its value is the event id.")

;;; Folding the timeline

(defun approve-conversation--reply-review-p (item)
  "Return non-nil if ITEM is a review with only code comments.
GitHub makes such a review for every reply to a thread."
  (let-alist item
    (and (equal .__typename "PullRequestReview")
         (equal .state "COMMENTED")
         (or (null .bodyHTML) (string-blank-p .bodyHTML)))))

(defun approve-conversation--same-run-p (run item)
  "Return non-nil if ITEM continues RUN, a list of folded items."
  (let ((last (car run)))
    (cond
     ((equal (alist-get '__typename item) "PullRequestCommit")
      (equal (alist-get '__typename last) "PullRequestCommit"))
     ((approve-conversation--reply-review-p item)
      (and (approve-conversation--reply-review-p last)
           (equal (let-alist item .author.login) (let-alist last .author.login))))
     ((member (alist-get '__typename item) '("LabeledEvent" "UnlabeledEvent"))
      (and (equal (alist-get '__typename item) (alist-get '__typename last))
           (equal (let-alist item .actor.login) (let-alist last .actor.login)))))))

(defun approve-conversation-items ()
  "Return the timeline as a list of runs, each a list of items in order.
Items are filtered by `approve-timeline-event-types'; pending reviews,
which only the viewer sees, are left out."
  (let ((runs nil))
    (dolist (item (approve-model-nodes (approve-pr-field 'timelineItems)))
      (when (and (member (alist-get '__typename item) approve-timeline-event-types)
                 (not (equal (alist-get 'state item) "PENDING")))
        (if (and runs (approve-conversation--same-run-p (car runs) item))
            (push item (car runs))
          (push (list item) runs))))
    (nreverse (mapcar #'reverse runs))))

;;; Items

(defun approve-conversation--who (item)
  "Return who did ITEM: its author or actor."
  (approve-render-user (or (alist-get 'author item) (alist-get 'actor item))))

(defun approve-conversation--line (icon item &rest words)
  "Return the heading of ITEM: ICON, who, WORDS and when."
  (concat icon " " (approve-conversation--who item) " "
          (apply #'concat words)
          (approve-face " · " 'approve-dim)
          (approve-render-date (alist-get 'createdAt item))))

(defun approve-conversation--label (label)
  "Return LABEL as a coloured box."
  (let-alist label
    (approve-thing (approve-face (format " %s " .name) (approve-label-face .color))
                   (list :kind 'label :name .name :color .color
                         :description .description :url .url))))

(defun approve-conversation--commit (commit)
  "Return the abbreviated oid of COMMIT, as a thing."
  (let-alist commit
    (approve-thing (approve-face (or .abbreviatedOid "") 'approve-hash)
                   (list :kind 'commit :oid .oid :url .url))))

(defun approve-conversation--reviewer (reviewer)
  "Return REVIEWER, a user or a team, as text."
  (let-alist reviewer
    (if (member .__typename '("Team" "EnterpriseTeam"))
        (approve-thing (approve-face (concat "@" (or .combinedSlug .slug)) 'approve-user)
                       (list :kind 'team :slug (or .combinedSlug .slug) :url .url))
      (approve-render-user reviewer))))

(defun approve-conversation--review-verb (state)
  "Return the icon and verb of a review in STATE."
  (pcase state
    ("APPROVED" (cons (approve-icon 'approved 'approve-approved)
                      (approve-face "approved" 'approve-approved)))
    ("CHANGES_REQUESTED" (cons (approve-icon 'changes-requested 'approve-changes-requested)
                               (approve-face "requested changes"
                                             'approve-changes-requested)))
    ("DISMISSED" (cons (approve-icon 'dismissed 'approve-dim)
                       (approve-face "reviewed (dismissed)" 'approve-dim)))
    (_ (cons (approve-icon 'commented 'approve-commented) "reviewed"))))

(defun approve-conversation--event-text (item)
  "Return the heading of the event ITEM, or nil for an unknown one."
  (let-alist item
    (let ((dim (lambda (text) (approve-face text 'approve-dim))))
      (pcase .__typename
        ((or "HeadRefForcePushedEvent" "BaseRefForcePushedEvent")
         (approve-conversation--line
          (approve-icon 'push) item
          (funcall dim (if (equal .__typename "BaseRefForcePushedEvent")
                           "force-pushed the base branch from "
                         "force-pushed the branch from "))
          (approve-conversation--commit .beforeCommit) (funcall dim " to ")
          (approve-conversation--commit .afterCommit)))
        ("AssignedEvent"
         (approve-conversation--line (approve-icon 'person) item (funcall dim "assigned ")
                                     (approve-render-user .assignee)))
        ("UnassignedEvent"
         (approve-conversation--line (approve-icon 'person) item (funcall dim "unassigned ")
                                     (approve-render-user .assignee)))
        ("RenamedTitleEvent"
         (approve-conversation--line (approve-icon 'pending-comment) item
                                     (funcall dim "changed the title from ")
                                     (format "“%s”" .previousTitle) (funcall dim " to ")
                                     (format "“%s”" .currentTitle)))
        ("MilestonedEvent"
         (approve-conversation--line (approve-icon 'milestone) item
                                     (funcall dim "added this to ") .milestoneTitle))
        ("DemilestonedEvent"
         (approve-conversation--line (approve-icon 'milestone) item
                                     (funcall dim "removed this from ") .milestoneTitle))
        ("ReadyForReviewEvent"
         (approve-conversation--line (approve-icon 'open 'approve-state-open) item
                                     (funcall dim "marked this ready for review")))
        ("ConvertToDraftEvent"
         (approve-conversation--line (approve-icon 'draft 'approve-state-draft) item
                                     (funcall dim "marked this as a draft")))
        ("ReviewRequestedEvent"
         (approve-conversation--line (approve-icon 'requested) item
                                     (funcall dim "asked ")
                                     (approve-conversation--reviewer .requestedReviewer)
                                     (funcall dim " for a review")))
        ("ReviewRequestRemovedEvent"
         (approve-conversation--line (approve-icon 'requested) item
                                     (funcall dim "no longer asks ")
                                     (approve-conversation--reviewer .requestedReviewer)
                                     (funcall dim " for a review")))
        ("ReviewDismissedEvent"
         (approve-conversation--line (approve-icon 'dismissed) item
                                     (funcall dim "dismissed a review")
                                     (if (and .dismissalMessage
                                              (not (string-empty-p .dismissalMessage)))
                                         (concat (funcall dim ": ") .dismissalMessage)
                                       "")))
        ("BaseRefChangedEvent"
         (approve-conversation--line (approve-icon 'push) item
                                     (funcall dim "changed the base branch from ")
                                     (approve-face (or .previousRefName "") 'approve-branch)
                                     (funcall dim " to ")
                                     (approve-face (or .currentRefName "") 'approve-branch)))
        ("HeadRefDeletedEvent"
         (approve-conversation--line (approve-icon 'closed 'approve-dim) item
                                     (funcall dim "deleted the branch")))
        ("HeadRefRestoredEvent"
         (approve-conversation--line (approve-icon 'open) item
                                     (funcall dim "restored the branch")))
        ("MergedEvent"
         (approve-conversation--line (approve-icon 'merged 'approve-state-merged) item
                                     (approve-face "merged" 'approve-state-merged)
                                     (funcall dim " commit ")
                                     (approve-conversation--commit .mergeCommit)
                                     (funcall dim " into ")
                                     (approve-face (or .mergeRefName "") 'approve-branch)))
        ("ClosedEvent"
         (approve-conversation--line (approve-icon 'closed 'approve-state-closed) item
                                     (approve-face "closed this" 'approve-state-closed)))
        ("ReopenedEvent"
         (approve-conversation--line (approve-icon 'open 'approve-state-open) item
                                     (approve-face "reopened this" 'approve-state-open)))
        ("CrossReferencedEvent"
         ;; Like GitHub: "#12" from this repository, "owner/repo#12" from another.
         (approve-conversation--line
          (approve-icon 'issue) item (funcall dim "mentioned this in ")
          (let ((other .isCrossRepository))
            (let-alist .source
              (concat (propertize (approve-face (format "%s#%s"
                                                        (if other
                                                            (or .repository.nameWithOwner "")
                                                          "")
                                                        .number)
                                                'approve-link)
                                  'approve-url .url
                                  'approve-thing (list :kind 'ref :number .number
                                                       :url .url :title .title))
                      " " (or .title ""))))))))))

(defun approve-conversation--gutter-body (start)
  "Draw the timeline's line left of the text inserted since START."
  (approve-render-prefix start (point) (approve-face "│   " 'approve-thread-border)))

(defun approve-conversation--insert-run (run)
  "Insert RUN, a list of timeline items folded into one entry."
  (let* ((item (car run))
         (type (alist-get '__typename item))
         (id (alist-get 'id item))
         (branch (approve-face "├ " 'approve-thread-border)))
    (cond
     ((equal type "IssueComment")
      (magit-insert-section (approve-issue-comment-section id)
        (magit-insert-heading
          (approve-render-justify
           (concat branch
                   (approve-conversation--line
                    (approve-icon 'commented) item
                    (approve-face "commented" 'approve-dim)
                    (if (alist-get 'lastEditedAt item)
                        (concat (approve-face " · " 'approve-dim) (approve-render-edited item))
                      "")))
           (approve-render-hints '("r" approve-reply "quote") '("+" approve-react "react"))))
        (let ((start (point)))
          (approve-thread-insert-comment-body item)
          (approve-conversation--gutter-body start))))
     ((equal type "PullRequestReview")
      (let* ((verb (approve-conversation--review-verb (alist-get 'state item)))
             (comments (apply #'+ (mapcar (lambda (review)
                                            (or (let-alist review .commentCount.totalCount) 0))
                                          run))))
        (magit-insert-section (approve-review-section id)
          (magit-insert-heading
            (concat branch
                    (approve-conversation--line (car verb) item (cdr verb))
                    (if (> comments 0)
                        (approve-face (format " · %d comment%s on code" comments
                                              (if (= comments 1) "" "s"))
                                      'approve-dim)
                      "")))
          (unless (approve-conversation--reply-review-p item)
            (let ((start (point)))
              (approve-thread-insert-comment-body item)
              (approve-conversation--gutter-body start))))))
     ((equal type "PullRequestCommit")
      (let ((commits (mapcar (lambda (node) (alist-get 'commit node)) run)))
        (magit-insert-section (approve-event-section id)
          (magit-insert-heading
            (concat branch
                    (approve-icon 'push) " "
                    (approve-render-git-actor (let-alist (car commits) .author))
                    (approve-face (format " pushed %d commit%s" (length commits)
                                          (if (cdr commits) "s" ""))
                                  'approve-dim)
                    (approve-face " · " 'approve-dim)
                    (approve-render-date (let-alist (car (last commits)) .committedDate))))
          (let ((start (point)))
            (dolist (commit commits)
              (insert "  " (approve-conversation--commit commit) "  "
                      (or (alist-get 'messageHeadline commit) "") "\n"))
            (approve-conversation--gutter-body start)))))
     ((member type '("LabeledEvent" "UnlabeledEvent"))
      (magit-insert-section (approve-event-section id)
        (insert branch
                (approve-conversation--line
                 (approve-icon 'label) item
                 (approve-face (if (equal type "LabeledEvent") "added " "removed ")
                               'approve-dim)
                 (mapconcat (lambda (event)
                              (approve-conversation--label (alist-get 'label event)))
                            run " "))
                "\n")))
     (t
      (when-let* ((text (approve-conversation--event-text item)))
        (magit-insert-section (approve-event-section id)
          (insert branch text "\n")))))))

(defun approve-insert-conversation-section ()
  "Insert the conversation: comments, reviews, pushes and events."
  (let* ((connection (approve-pr-field 'timelineItems))
         (runs (approve-conversation-items))
         (bar (approve-face "│" 'approve-thread-border)))
    (magit-insert-section (approve-conversation-section)
      (magit-insert-heading
        (approve-sections-heading "Conversation" (and connection (length runs))))
      (let ((start (point)))
        (if (not connection)
            (insert (or (approve-sections-state 'timeline "the conversation") ""))
          (let-alist (approve-pr-fields '(author createdAt))
            (insert (approve-render-user .author)
                    (approve-face " opened this " 'approve-dim)
                    (approve-render-date .createdAt) "\n"))
          (dolist (run runs)
            (insert bar "\n")
            (approve-conversation--insert-run run))
          (when (approve-model-truncated-p connection)
            (insert bar "\n" (approve-face "┆ more is loading…" 'approve-loading) "\n"))
          (insert bar "\n"
                  (approve-face "╰" 'approve-thread-border)
                  (if-let* ((hints (approve-render-hints
                                    '("C" approve-comment-on-pr "comment on this pull request"))))
                      (concat " " hints)
                    "")
                  "\n"))
        (approve-sections-insert-body start))
      (insert "\n"))))

(provide 'approve-conversation)

;;; approve-conversation.el ends here
