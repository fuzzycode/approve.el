;;; approve-sections.el --- Sections of the pull request buffer -*- lexical-binding: t; -*-

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

;; The section inserters of the top of a pull request buffer
;; (docs/ui.org, "PR buffer — single layout"): the header with its
;; metadata rows, the viewer's pending review, the description, the
;; checks, the commits and the list of changed files.  The
;; conversation is in approve-conversation.el, the diff in
;; approve-diff.el.
;;
;; Each inserter is named `approve-insert-<name>-section' and goes in
;; `approve-pr-sections-hook' (decision D04).  It reads the store
;; through selectors and shows "Loading…" or the error of the query
;; its data comes from (`approve-sections-state').
;;
;; Layer rules: this is the renderer layer.  Inserters read the store
;; and the session and insert text; they never change them or call
;; the network.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-model)
(require 'approve-session)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-html)
(require 'approve-render)
(require 'approve-pr)

;;; Options

(defcustom approve-metadata-rows
  '(author repository branch opened reviewers assignees labels milestone issues
           status)
  "Metadata rows of the header, under the title, in order.
Each is one of `author', `repository', `branch' (head → base and the
number of commits), `opened' (when it was opened, updated, merged or
closed), `reviewers', `assignees', `labels', `milestone', `issues'
and `status'.  To show only reviewers and labels:

  (setq approve-metadata-rows \\='(reviewers labels))"
  :type '(repeat (choice (const author) (const repository) (const branch)
                         (const opened) (const reviewers) (const assignees)
                         (const labels) (const milestone) (const issues)
                         (const status)))
  :group 'approve)

(defcustom approve-checks-passed-shown 5
  "Number of passed checks listed before the rest are folded.
The others are under \"… N more passed\".  To list them all:

  (setq approve-checks-passed-shown most-positive-fixnum)"
  :type 'natnum
  :group 'approve)

(defcustom approve-diffstat-width 15
  "Width of the bar that shows a file's size of change in the file list.
The largest file fills it."
  :type 'natnum
  :group 'approve)

;;; Section classes

(defclass approve-header-section (magit-section) ()
  "The header: title, state, branches and the metadata rows.")
(defclass approve-meta-section (magit-section) ()
  "One metadata row; its value names the row, such as `labels'.")
(defclass approve-pending-review-section (magit-section) ()
  "The viewer's pending review.")
(defclass approve-pending-comment-section (magit-section) ()
  "One comment of the pending review; its value is the comment id.")
(defclass approve-description-section (magit-section) ()
  "The description of the pull request.")
(defclass approve-checks-section (magit-section) ()
  "The checks of the head commit.")
(defclass approve-check-section (magit-section) ()
  "One check run or status; its value is its id.")
(defclass approve-checks-more-section (magit-section) ()
  "The passed checks that are folded away.")
(defclass approve-commits-section (magit-section) ()
  "The commits of the pull request.")
(defclass approve-commit-section (magit-section) ()
  "One commit; its value is the oid.")
(defclass approve-files-section (magit-section) ()
  "The list of changed files.")
(defclass approve-file-entry-section (magit-section) ()
  "One changed file in the list; its value is the path.")

;;; Helpers

(defun approve-sections-state (key what)
  "Return a line saying the data of query KEY is loading or failed, or nil.
WHAT names the data in the line, such as \"files\"."
  (cond ((approve-pr-error key) (approve-render-failure what (approve-pr-error key)))
        ((approve-pr-loading-p key) (approve-render-loading))))

(defun approve-sections-heading (title &optional count right)
  "Return a section heading: TITLE, COUNT in parentheses, RIGHT aligned.
COUNT is a number or nil; RIGHT a string or nil."
  (approve-render-justify
   (concat (approve-face title 'approve-heading)
           (and count (approve-face (format " (%s)" count) 'approve-count)))
   right))

(defconst approve-sections-indentation (approve-render-decoration "  ")
  "The indentation of a body line, for lines that insert it themselves.")

(defun approve-sections-insert-body (start)
  "Indent the text inserted since START by two columns."
  (approve-render-prefix start (point) "  "))

(defun approve-sections--ref (thing)
  "Return \"#N title\" of THING, an issue or pull request, as a link."
  (let-alist thing
    (concat (propertize (approve-face (format "#%s" .number) 'approve-link)
                        'approve-thing (list :kind 'ref :number .number :url .url
                                             :title .title :state .state)
                        'approve-url .url)
            " " (or .title ""))))

;;; Header

(defun approve-sections--branch (name ref owner)
  "Return the branch NAME as text, with REF (a Ref) and OWNER for eldoc."
  (approve-thing (approve-face (or name "?") 'approve-branch)
                 (list :kind 'branch :name name :ref ref :owner owner)))

(defun approve-sections--branches ()
  "Return the branch row's value: head → base, and the number of commits."
  (let-alist (approve-pr-fields '(commitCount baseRefName headRefName baseRef headRef
                                              headRepositoryOwner isCrossRepository))
    (let ((count .commitCount.totalCount)
          (head (if (and .isCrossRepository .headRepositoryOwner)
                    (format "%s:%s" (alist-get 'login .headRepositoryOwner) .headRefName)
                  .headRefName)))
      (concat (approve-sections--branch head .headRef .headRepositoryOwner)
              (approve-face " → " 'approve-dim)
              (approve-sections--branch .baseRefName .baseRef nil)
              (when count
                (approve-face (format " · %d commit%s" count (if (= count 1) "" "s"))
                              'approve-dim))))))

(defun approve-sections--repository ()
  "Return the repository's owner/name, which opens its page on GitHub."
  (let ((name (format "%s/%s" (approve-model-meta :owner) (approve-model-meta :repo)))
        (url (when-let* ((pr (approve-pr-field 'url)))
               (replace-regexp-in-string "/pull/[0-9]+\\'" "" pr))))
    (if url (propertize name 'approve-url url) name)))

(defun approve-sections--dates ()
  "Return the opened row's value: when it was opened, updated, merged or closed.
The dates look like the other rows' values; the words between them
are dim."
  (let-alist (approve-pr-fields '(createdAt updatedAt mergedAt closedAt merged closed
                                            mergedBy))
    (let ((date (lambda (iso) (approve-render-date iso 'default)))
          (dim (lambda (text) (approve-face text 'approve-dim))))
      (approve-render-join
       (list (funcall date .createdAt)
             (concat (funcall dim "updated ") (funcall date .updatedAt))
             (cond (.merged (concat (funcall dim "merged ") (funcall date .mergedAt)
                                    (when .mergedBy
                                      (concat (funcall dim " by ")
                                              (approve-render-user .mergedBy)))))
                   (.closed (concat (funcall dim "closed ") (funcall date .closedAt)))))))))

(defun approve-insert-header-section ()
  "Insert the header: the metadata rows.
The number, title and state are in the header line, which stays in
view (`approve-pr--update-header-line')."
  (magit-insert-section (approve-header-section)
    (cond
     ((approve-model-root-ref)
      (dolist (row approve-metadata-rows)
        (approve-sections--insert-meta-row row)))
     (t
      (insert (approve-face (format "%s/%s#%s" (approve-model-meta :owner)
                                    (approve-model-meta :repo)
                                    (approve-model-meta :number))
                            'approve-dim)
              "\n")
      (insert (or (approve-sections-state 'core "the pull request") ""))))
    (insert "\n")))

;;; Metadata rows

(defconst approve-sections--meta-width 12
  "Width of the names of metadata rows.")

(defun approve-sections--meta-row (name value)
  "Return a metadata row: NAME, then VALUE or a dim \"None\"."
  (concat (approve-face (approve-render-pad name approve-sections--meta-width)
                        'approve-secondary-heading)
          (if (or (null value) (string-empty-p value))
              (approve-face "None" 'approve-dim)
            value)))

(defun approve-sections--review-mark (state)
  "Return the icon of a reviewer whose latest review is in STATE."
  (pcase state
    ("APPROVED" (approve-icon 'approved 'approve-approved))
    ("CHANGES_REQUESTED" (approve-icon 'changes-requested 'approve-changes-requested))
    ("COMMENTED" (approve-icon 'commented 'approve-commented))
    ("DISMISSED" (approve-icon 'dismissed 'approve-dim))
    (_ (approve-icon 'requested 'approve-requested))))

(defun approve-sections--reviewer (reviewer)
  "Return REVIEWER, a user or team that was asked for a review, as text."
  (let-alist reviewer
    (if (member .__typename '("Team" "EnterpriseTeam"))
        (approve-thing (approve-face (concat "@" (or .combinedSlug .slug)) 'approve-user)
                       (list :kind 'team :slug (or .combinedSlug .slug) :url .url
                             :id .id))
      (approve-render-user reviewer))))

(defun approve-sections--reviewers ()
  "Return the reviewers row's value: who reviewed, then who is asked."
  (let-alist (approve-pr-fields '(latestOpinionatedReviews reviewRequests))
    (let* ((reviewed (mapcar (lambda (review)
                               (let-alist review
                                 (concat (approve-sections--review-mark .state) " "
                                         (approve-render-user .author))))
                             .latestOpinionatedReviews))
           (logins (mapcar (lambda (review) (let-alist review .author.login))
                           .latestOpinionatedReviews))
           (asked (delq nil
                        (mapcar (lambda (request)
                                  (let-alist request
                                    (unless (member .requestedReviewer.login logins)
                                      (concat (approve-sections--review-mark nil) " "
                                              (approve-sections--reviewer
                                               .requestedReviewer)))))
                                .reviewRequests))))
      (string-join (append reviewed asked) "   "))))

(defun approve-sections--label (label)
  "Return LABEL as a coloured box."
  (let-alist label
    (approve-thing (approve-face (format " %s " .name) (approve-label-face .color))
                   (list :kind 'label :name .name :color .color
                         :description .description :url .url))))

(defun approve-sections--check-counts ()
  "Return the PR's check counts as (PASSED FAILED RUNNING), or nil."
  (let-alist (approve-pr-field 'statusCheckRollup)
    (when .contextCounts
      (let ((passed 0) (failed 0) (running 0))
        (dolist (entry (append .contextCounts.checkRunCountsByState
                               .contextCounts.statusContextCountsByState))
          (let-alist entry
            (pcase .state
              ((or "SUCCESS" "NEUTRAL" "SKIPPED") (cl-incf passed .count))
              ((or "FAILURE" "ERROR" "TIMED_OUT" "CANCELLED" "STARTUP_FAILURE"
                   "ACTION_REQUIRED" "STALE")
               (cl-incf failed .count))
              (_ (cl-incf running .count)))))
        (list passed failed running)))))

(defun approve-sections--rollup-icon (state)
  "Return the icon of the check rollup STATE."
  (pcase state
    ("SUCCESS" (approve-icon 'check-success 'approve-check-success))
    ((or "FAILURE" "ERROR") (approve-icon 'check-failure 'approve-check-failure))
    ((or "PENDING" "EXPECTED") (approve-icon 'check-pending 'approve-check-pending))
    (_ (approve-icon 'check-pending 'approve-dim))))

(defun approve-sections-checks-summary ()
  "Return the checks summary, such as \"◐ 7 passed · 1 failed\", or nil."
  (when-let* ((rollup (approve-pr-field 'statusCheckRollup))
              (counts (approve-sections--check-counts)))
    (pcase-let ((`(,passed ,failed ,running) counts))
      (concat (approve-sections--rollup-icon (alist-get 'state rollup)) " "
              (approve-render-join
               (list (and (> passed 0) (approve-face (format "%d passed" passed)
                                                     'approve-check-success))
                     (and (> failed 0) (approve-face (format "%d failed" failed)
                                                     'approve-check-failure))
                     (and (> running 0) (approve-face (format "%d running" running)
                                                      'approve-check-pending))))))))

(defun approve-sections--status ()
  "Return the status row's value: review decision, conflicts, checks."
  (let-alist (approve-pr-fields '(reviewDecision mergeable merged closed))
    (approve-render-join
     (list (pcase .reviewDecision
             ("APPROVED" (concat (approve-icon 'approved 'approve-approved) " "
                                 (approve-face "Approved" 'approve-approved)))
             ("CHANGES_REQUESTED"
              (concat (approve-icon 'changes-requested 'approve-changes-requested) " "
                      (approve-face "Changes requested" 'approve-changes-requested)))
             ("REVIEW_REQUIRED"
              (concat (approve-icon 'requested 'approve-requested) " "
                      (approve-face "Review required" 'approve-requested))))
           (unless (or .merged .closed)
             (pcase .mergeable
               ("MERGEABLE" (concat (approve-icon 'check-success 'approve-check-success) " "
                                    (approve-face "No conflicts" 'approve-check-success)))
               ("CONFLICTING" (concat (approve-icon 'check-failure 'approve-check-failure) " "
                                      (approve-face "Conflicts" 'approve-check-failure)))))
           (when-let* ((counts (approve-sections--check-counts)))
             (pcase-let ((`(,passed ,failed ,running) counts))
               (let ((total (+ passed failed running)))
                 (concat (approve-sections--rollup-icon
                          (let-alist (approve-pr-field 'statusCheckRollup) .state))
                         " "
                         (format "%d of %d checks passed" passed total)))))))))

(defun approve-sections--meta-value (row)
  "Return the value of the metadata ROW as text, or nil when empty."
  (pcase row
    ('author (when-let* ((author (approve-pr-field 'author)))
               (approve-render-user author)))
    ('repository (approve-sections--repository))
    ('branch (approve-sections--branches))
    ('opened (approve-sections--dates))
    ('reviewers (approve-sections--reviewers))
    ('assignees (mapconcat #'approve-render-user (approve-pr-field 'assignees) "  "))
    ('labels (mapconcat #'approve-sections--label (approve-pr-field 'labels) " "))
    ('milestone (when-let* ((milestone (approve-pr-field 'milestone)))
                  (let-alist milestone
                    (approve-thing (approve-face .title 'default)
                                   (list :kind 'milestone :title .title :due .dueOn
                                         :progress .progressPercentage :state .state
                                         :description .description :url .url)))))
    ('issues (mapconcat #'approve-sections--ref
                        (approve-pr-field 'closingIssuesReferences) "   "))
    ('status (approve-sections--status))))

(defun approve-sections--insert-meta-row (row)
  "Insert the metadata ROW as a section of its own."
  (magit-insert-section (approve-meta-section row)
    (insert (approve-sections--meta-row
             (capitalize (symbol-name row))
             (approve-sections--meta-value row))
            "\n")))

;;; Pending review

(defun approve-sections--location (comment)
  "Return \"path:line\" or \"path:start–end\" of the review COMMENT."
  (let-alist comment
    (let ((line (or .line .originalLine))
          (start (or .startLine .originalStartLine)))
      (approve-face (cond ((equal .subjectType "FILE") .path)
                          ((and start line (/= start line)) (format "%s:%s–%s" .path start line))
                          (line (format "%s:%s" .path line))
                          (t .path))
                    'approve-path))))

(defun approve-sections--suggestion-p (comment)
  "Return non-nil if COMMENT holds a suggested change."
  (string-match-p "js-suggested-changes-blob" (or (alist-get 'bodyHTML comment) "")))

(defun approve-insert-pending-review-section ()
  "Insert the viewer's pending review, when there is one."
  (when-let* ((review (approve-pr-pending-review)))
    (let* ((comments (approve-model-nodes (alist-get 'comments review)))
           (replies (seq-count (lambda (c) (alist-get 'replyTo c)) comments))
           (new (- (length comments) replies))
           (start (point)))
      (magit-insert-section (approve-pending-review-section (alist-get 'id review))
        (magit-insert-heading
          (approve-sections-heading
           "Your pending review" nil
           (approve-render-join
            (list (approve-render-join
                   (list (and (> new 0) (format "%d comment%s" new (if (= new 1) "" "s")))
                         (and (> replies 0) (format "%d repl%s" replies
                                                    (if (= replies 1) "y" "ies")))))
                  (approve-render-hints '("S" approve-submit-review "submit")
                                        '("X" approve-discard-review "discard"))))))
        (let ((body (point))
              (width (min 40 (apply #'max 10 (mapcar (lambda (comment)
                                                       (string-width
                                                        (approve-sections--location comment)))
                                                     comments)))))
          (dolist (comment comments)
            (magit-insert-section (approve-pending-comment-section (alist-get 'id comment))
              (insert (if (alist-get 'replyTo comment)
                          (concat (approve-face "↳ reply on " 'approve-dim)
                                  (approve-sections--location comment))
                        (concat (approve-render-pad (approve-sections--location comment) width)
                                "  "
                                (if (approve-sections--suggestion-p comment)
                                    (concat (approve-icon 'pending-comment 'approve-pending)
                                            " suggestion")
                                  (approve-render-truncate
                                   (approve-render-first-line (alist-get 'bodyHTML comment))
                                   50))))
                      "\n")))
          (approve-sections-insert-body body))
        (insert "\n"))
      (approve-face-region start (1- (point)) 'approve-pending))))

;;; Description

(defun approve-insert-description-section ()
  "Insert the description of the pull request, with its reactions."
  (magit-insert-section (approve-description-section)
    (let-alist (approve-pr-fields '(id bodyHTML reactionGroups lastEditedAt))
      (magit-insert-heading
        (approve-sections-heading
         "Description" nil
         (approve-render-join
          (list (and .lastEditedAt (approve-render-edited
                                    `((lastEditedAt . ,.lastEditedAt))))
                (approve-render-hints '("e" approve-edit "edit"))))))
      (let ((start (point)))
        (cond
         ((not (approve-model-root-ref))
          (insert (or (approve-sections-state 'core "the description") "")))
         ((not (approve-html-insert .bodyHTML))
          (insert (approve-render-empty "No description provided."))))
        (when-let* ((reactions (approve-render-reactions-text .reactionGroups .id)))
          (insert "\n" reactions "\n"))
        (approve-sections-insert-body start)))
    (insert "\n")))

;;; Checks

(defun approve-sections--check-kind (check)
  "Return how CHECK went: `failure', `running' or `success'."
  (let-alist check
    (if (equal .__typename "StatusContext")
        (pcase .state
          ("SUCCESS" 'success)
          ((or "FAILURE" "ERROR") 'failure)
          (_ 'running))
      (if (not (equal .status "COMPLETED"))
          'running
        (pcase .conclusion
          ((or "SUCCESS" "NEUTRAL" "SKIPPED") 'success)
          (_ 'failure))))))

(defun approve-sections--check-name (check)
  "Return the name of CHECK, with its workflow when it has one."
  (let-alist check
    (if (equal .__typename "StatusContext")
        .context
      (if-let* ((workflow .checkSuite.workflowRun.workflow.name))
          (format "%s / %s" workflow .name)
        .name))))

(defun approve-sections--duration (start end)
  "Return the time from START to END, ISO 8601 times, as \"2m 10s\"."
  (when-let* ((from (approve-render-parse-time start))
              (to (approve-render-parse-time end)))
    (let ((seconds (round (float-time (time-subtract to from)))))
      (cond ((>= seconds 3600) (format "%dh %02dm" (/ seconds 3600) (/ (% seconds 3600) 60)))
            ((>= seconds 60) (format "%dm %02ds" (/ seconds 60) (% seconds 60)))
            (t (format "%ds" seconds))))))

(defun approve-sections--check-line (check)
  "Return the line of CHECK in the expanded checks."
  (let-alist check
    (let* ((kind (approve-sections--check-kind check))
           (icon (pcase kind
                   ('success (approve-icon 'check-success 'approve-check-success))
                   ('failure (approve-icon 'check-failure 'approve-check-failure))
                   (_ (approve-icon 'check-pending 'approve-check-pending))))
           (when (if (eq kind 'running)
                     (approve-face (if (member .status '("QUEUED" "WAITING" "PENDING"))
                                       "queued" "running")
                                   'approve-check-pending)
                   (or (approve-sections--duration .startedAt .completedAt) "")))
           (note (or .description .title)))
      (approve-thing
       (concat icon " "
               (approve-render-pad (approve-render-truncate (approve-sections--check-name check) 38)
                           40)
               (approve-render-pad when 9)
               (if .isRequired (approve-face "required" 'approve-secondary-heading)
                 (make-string 8 ?\s))
               (if note (concat "  " (approve-face (approve-render-truncate note 40)
                                                   'approve-dim))
                 ""))
       (list :kind 'check :check check :url (or .detailsUrl .targetUrl .url))))))

(defun approve-sections--sort-checks (checks)
  "Return CHECKS failing first, then running, then passed; required first."
  (let ((rank (lambda (check)
                (+ (pcase (approve-sections--check-kind check)
                     ('failure 0) ('running 2) (_ 4))
                   (if (alist-get 'isRequired check) 0 1)))))
    (seq-sort-by rank #'< checks)))

(defun approve-sections--insert-checks (checks)
  "Insert the lines of CHECKS, folding passed ones beyond a few."
  (let* ((sorted (approve-sections--sort-checks checks))
         (shown 0)
         (more nil))
    (dolist (check sorted)
      (if (and (eq (approve-sections--check-kind check) 'success)
               (>= shown approve-checks-passed-shown))
          (push check more)
        (when (eq (approve-sections--check-kind check) 'success)
          (cl-incf shown))
        (magit-insert-section (approve-check-section (alist-get 'id check))
          (insert (approve-sections--check-line check) "\n"))))
    (when more
      (magit-insert-section (approve-checks-more-section nil t)
        (magit-insert-heading
          (approve-face (format "… %d more passed" (length more)) 'approve-dim))
        (dolist (check (nreverse more))
          (magit-insert-section (approve-check-section (alist-get 'id check))
            (insert (approve-sections--check-line check) "\n")))))))

(defun approve-insert-checks-section ()
  "Insert the checks: the summary in the heading, each check when expanded.
The checks themselves load when the section is first expanded."
  (when (or (not (approve-model-root-ref)) (approve-pr-field 'statusCheckRollup))
    (magit-insert-section (approve-checks-section nil t)
      (magit-insert-heading
        (approve-render-justify (approve-face "Checks" 'approve-heading)
                                (approve-sections-checks-summary)
                                nil))
      (let ((start (point))
            (contexts (let-alist (approve-pr-field 'statusCheckRollup) .contexts)))
        (cond
         ((approve-model-nodes contexts)
          (approve-sections--insert-checks (approve-model-nodes contexts)))
         (t (insert (or (approve-sections-state 'checks "the checks")
                        (approve-render-loading)))))
        (approve-sections-insert-body start))
      (insert "\n"))))

(defun approve-sections--checks-wanted-p ()
  "Return non-nil if the Checks section is expanded."
  (when-let* ((root magit-root-section)
              (section (seq-find (lambda (s) (eq (oref s type) 'approve-checks-section))
                                 (oref root children))))
    (not (oref section hidden))))

(add-to-list 'approve-pr-lazy-loaders
             (cons 'checks #'approve-sections--checks-wanted-p))

;;; Commits

(defun approve-sections--reviewed-oid ()
  "Return the oid of the commit the viewer last reviewed, or nil."
  (let-alist (approve-pr-field 'viewerLatestReview) .commit.oid))

(defun approve-sections--new-commits (commits)
  "Return how many of COMMITS came after the viewer's last review, or nil."
  (when-let* ((oid (approve-sections--reviewed-oid))
              (position (seq-position (mapcar (lambda (commit) (alist-get 'oid commit))
                                              commits)
                                      oid)))
    (- (length commits) position 1)))

(defun approve-sections--commit-author (commit)
  "Return the author of COMMIT: the GitHub user, else the Git name."
  (let-alist commit
    (if .author.user
        (approve-render-user .author.user)
      (approve-face (or .author.name "?") 'approve-user))))

(defun approve-sections--commit-line (commit reviewed new)
  "Return the line of COMMIT; REVIEWED if the viewer reviewed it, NEW if after."
  (let-alist commit
    (concat (if new (approve-face "┃ " 'approve-new) "  ")
            (approve-thing (approve-face .abbreviatedOid 'approve-hash)
                           (list :kind 'commit :oid .oid :url .url))
            "  "
            (approve-render-pad (approve-render-truncate (or .messageHeadline "") 44) 46)
            (approve-render-pad (approve-sections--commit-author commit) 14)
            (approve-render-pad (approve-render-date .committedDate) 14)
            (if .statusCheckRollup
                (approve-sections--rollup-icon .statusCheckRollup.state)
              " ")
            (cond (reviewed (approve-face "   ← your review" 'approve-dim))
                  (new (approve-face "   new" 'approve-new))
                  (t "")))))

(defun approve-insert-commits-section ()
  "Insert the commits, marking those after the viewer's last review."
  (let* ((connection (approve-pr-field 'commits))
         (commits (mapcar (lambda (node) (alist-get 'commit node))
                          (approve-model-nodes connection)))
         (total (or (approve-model-total-count connection)
                    (let-alist (approve-pr-field 'commitCount) .totalCount)))
         (new (approve-sections--new-commits commits))
         (reviewed (approve-sections--reviewed-oid)))
    (magit-insert-section (approve-commits-section nil t)
      (magit-insert-heading
        (approve-sections-heading
         "Commits" total
         (and new (> new 0)
              (approve-face (format "%d new since your review" new) 'approve-new))))
      (let ((start (point))
            (after nil))
        (if commits
            (dolist (commit commits)
              (let ((oid (alist-get 'oid commit)))
                (magit-insert-section (approve-commit-section oid)
                  (insert (approve-sections--commit-line commit (equal oid reviewed) after)
                          "\n"))
                (when (and reviewed (equal oid reviewed))
                  (setq after t))))
          (insert (or (approve-sections-state 'commits "the commits") "")))
        (approve-sections-insert-body start))
      (insert "\n"))))

;;; Files

(defun approve-sections-thread-counts (kind)
  "Return a hash table from path to (UNRESOLVED . TOTAL) threads of KIND.
KIND is `file' for threads on a whole file, which the file list
counts, or `line' for threads on lines of the diff, outdated ones
included, which the diff's headings count."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (thread (approve-pr-list-fields 'reviewThreads '(path isResolved subjectType)))
      (let-alist thread
        (when (eq (equal .subjectType "FILE") (eq kind 'file))
          (let ((counts (gethash .path table (cons 0 0))))
            (puthash .path (cons (+ (car counts) (if .isResolved 0 1))
                                 (1+ (cdr counts)))
                     table)))))
    table))

(defun approve-sections-unresolved (counts &optional what)
  "Return \"N unresolved WHAT threads\" from COUNTS, or nil when N is 0.
COUNTS is a table from `approve-sections-thread-counts'; WHAT, such
as \"file\", goes before \"thread\"."
  (let ((unresolved (apply #'+ (mapcar #'car (hash-table-values counts)))))
    (and (> unresolved 0)
         (approve-face (format "%d unresolved %sthread%s" unresolved
                               (if what (concat what " ") "")
                               (if (= unresolved 1) "" "s"))
                       'approve-outdated))))

(defun approve-sections-viewed-mark (state)
  "Return the mark of a file whose viewed STATE is GitHub's."
  (pcase state
    ("VIEWED" (approve-icon 'viewed 'approve-viewed))
    ("DISMISSED" (approve-icon 'dismissed 'approve-dismissed))
    (_ (approve-icon 'unviewed 'approve-unviewed))))

(defun approve-sections--bar (additions deletions largest)
  "Return a bar of ADDITIONS and DELETIONS scaled to the LARGEST file."
  (let* ((total (+ additions deletions))
         (width (if (> largest 0)
                    (max (if (> total 0) 1 0)
                         (round (* approve-diffstat-width (/ (float total) largest))))
                  0))
         (added (if (> total 0) (round (* width (/ (float additions) total))) 0))
         (removed (- width added)))
    (concat (approve-face (make-string added ?█) 'approve-diffstat-added)
            (approve-face (make-string removed ?▒) 'approve-diffstat-removed))))

(defun approve-sections-counts (additions deletions &optional width)
  "Return ADDITIONS and DELETIONS as \"+A −D\", each padded to WIDTH if given."
  (let ((pad (lambda (text) (if width (approve-render-pad text width t) text))))
    (concat (approve-face (funcall pad (format "+%d" additions)) 'approve-diffstat-added)
            " "
            (approve-face (funcall pad (format "−%d" deletions)) 'approve-diffstat-removed))))

(defun approve-sections-viewed-count (files)
  "Return how many of FILES the viewer marked as viewed."
  (seq-count (lambda (file) (equal (alist-get 'viewerViewedState file) "VIEWED")) files))

(defun approve-sections-thread-count (counts)
  "Return the thread mark of a file from COUNTS, (UNRESOLVED . TOTAL).
It is the unresolved count, or the total, dim, when all are resolved;
empty without threads."
  (cond ((null counts) "")
        ((> (car counts) 0)
         (concat (approve-icon 'comment) " " (format "%d" (car counts))))
        (t (approve-face (concat (approve-icon 'comment) " " (format "%d" (cdr counts)))
                         'approve-dim))))

(defun approve-insert-files-section ()
  "Insert the list of changed files with their size and threads.
The threads are those on a whole file; the diff's headings count the
threads on lines, and say which files the viewer viewed."
  (let* ((connection (approve-pr-field 'files))
         (files (approve-pr-files))
         (counts (approve-sections-thread-counts 'file))
         (largest (apply #'max 0 (mapcar (lambda (file)
                                           (+ (alist-get 'additions file)
                                              (alist-get 'deletions file)))
                                         files)))
         (path-width (min 50 (apply #'max 10 (mapcar (lambda (file)
                                                       (string-width (alist-get 'path file)))
                                                     files))))
         (number-width (1+ (length (number-to-string largest)))))
    (magit-insert-section (approve-files-section)
      (let-alist (approve-pr-fields '(changedFiles additions deletions))
        (magit-insert-heading
          (approve-render-justify
           (concat (approve-face "Files changed" 'approve-heading)
                   (if .changedFiles
                       (concat "   " (format "%d file%s  " .changedFiles
                                             (if (= .changedFiles 1) "" "s"))
                               (approve-sections-counts .additions .deletions))
                     ""))
           (and files (approve-sections-unresolved counts "file")))))
      ;; Each row carries its indentation: one insertion per row, where
      ;; indenting afterwards would insert again into a buffer of markers.
      (dolist (file files)
        (let-alist file
          (magit-insert-section (approve-file-entry-section .path)
            (insert approve-sections-indentation
                    (approve-face (approve-render-pad (approve-render-shorten-path
                                                       .path path-width)
                                                      path-width)
                                  'approve-path)
                    "  "
                    (approve-sections-counts .additions .deletions number-width)
                    "  "
                    (approve-render-pad (approve-sections--bar .additions .deletions largest)
                                        approve-diffstat-width)
                    "  "
                    (approve-sections-thread-count (gethash .path counts))
                    "\n"))))
      (let ((start (point)))
        (cond
         ((approve-model-truncated-p connection)
          (insert (approve-face (format "… %d more loading"
                                        (- (approve-model-total-count connection)
                                           (length files)))
                                'approve-loading)
                  "\n"))
         ((null files)
          (insert (or (approve-sections-state 'files "the files")
                      (approve-render-empty "No files changed.")))))
        (approve-sections-insert-body start))
      (insert "\n"))))

(provide 'approve-sections)

;;; approve-sections.el ends here
