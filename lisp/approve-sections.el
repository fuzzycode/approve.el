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
(require 'approve-log)
(require 'approve-pr)

;;; Options

(defcustom approve-metadata-rows
  '(author repository branch opened reviewers assignees labels milestone issues
           status)
  "Metadata rows of the header, under the title, in order.
Each is one of `author', `repository', `branch' (head → base),
`opened' (when it was opened, updated, merged or closed),
`reviewers', `assignees', `labels', `milestone', `issues' and
`status'.  To show only reviewers and labels:

  (setq approve-metadata-rows \\='(reviewers labels))"
  :type '(repeat (choice (const author) (const repository) (const branch)
                         (const opened) (const reviewers) (const assignees)
                         (const labels) (const milestone) (const issues)
                         (const status)))
  :group 'approve)

(defcustom approve-checks-passed-shown 5
  "Number of passed checks listed before the rest are folded.
The others are under \"… N more passed\".  Once that line is
expanded, it goes and they join the list, for as long as the
buffer lives.  To list them all from the start:

  (setq approve-checks-passed-shown most-positive-fixnum)

Checks in groups (`approve-checks-group-by') are all listed."
  :type 'natnum
  :group 'approve)

(defconst approve-sections--check-groupings
  '(choice (const :tag "In one list" nil)
           (const :tag "By workflow" workflow)
           (const :tag "By app" app)
           (const :tag "By how they went" state)
           (function :tag "Function that returns a check's group"))
  "The `:type' of a way of grouping the checks.")

(defcustom approve-checks-group-by nil
  "How the checks are grouped under the Checks heading.
nil lists them in one list.  The others put each group under a
heading of its own, which starts expanded when one of its checks
failed or still runs, collapsed otherwise:

  `workflow'  by the GitHub Actions workflow that ran them, or the
              app for other check runs; a commit status by its
              context up to its last \"/\", \"codecov\" for
              \"codecov/patch\"
  `app'       by the app that ran them, such as \"GitHub Actions\";
              a commit status as for `workflow'
  `state'     failed, running, passed and skipped
  FUNCTION    called with a check, an alist as GitHub sends it (a
              CheckRun or a StatusContext); it returns the title of
              the check's group, or nil for \"Other\"

A check's name drops its group's title from its start: \"CI /
lint\" is \"lint\" under \"CI\".  `approve-checks-group-by-repository'
says otherwise for some repositories.  To group by workflow:

  (setq approve-checks-group-by \\='workflow)"
  :type approve-sections--check-groupings
  :group 'approve)

(defcustom approve-checks-group-by-repository nil
  "How the checks are grouped in some repositories.
An alist of (REGEXP . GROUPING).  The first REGEXP that matches a
repository's \"OWNER/NAME\", ignoring case, says how its checks are
grouped, as `approve-checks-group-by' does, which is for the
others.  To group by workflow in one repository only:

  (setq approve-checks-group-by-repository
        \\='((\"^big-org/monorepo$\" . workflow)))"
  :type `(alist :key-type (regexp :tag "Repository")
                :value-type ,approve-sections--check-groupings)
  :group 'approve)

(defcustom approve-diffstat-width 15
  "Width of the bar that shows a file's size of change in the file list.
The largest file fills it."
  :type 'natnum
  :group 'approve)

(defcustom approve-diff-orders
  '((github "GitHub's order" nil)
    (headers "Headers before their sources" approve-diff-order-headers-first))
  "Orders of the diff's files: (NAME DESCRIPTION KEY).
NAME, a symbol, names the order in `approve-diff-order', in the
filter menu (\\<approve-pr-mode-map>\\[approve-filter-menu]), whose s puts the diff in the next order,
and in the diff's heading.  DESCRIPTION says what it does.  KEY is
nil for GitHub's order, by path, or a function of a changed file,
an alist with `path', `changeType', `additions', `deletions' and
`viewerViewedState', that returns its sort key: a number, a
string, or a list of them compared element by element, numbers by
`<' and strings by `string<'.  Files with equal keys keep GitHub's
order.  KEY is called in the PR buffer for each file each time the
diff draws, so it must be quick and must not call the network; one
that signals leaves the files in GitHub's order, and its error is
logged once.

The diff, the file list, the thread picker and the next file of
the feedback mode go in the order (decision D58).  To put the
largest files first:

  (defun my-approve-largest-first (file)
    (- (+ (alist-get \\='additions file) (alist-get \\='deletions file))))
  (approve-diff-order-register \\='largest \"Largest first\"
                               #\\='my-approve-largest-first)"
  :type '(repeat (list (symbol :tag "Name")
                       (string :tag "Description")
                       (choice (const :tag "GitHub's order" nil)
                               (function :tag "Function of a file that returns its key"))))
  :group 'approve)

(defcustom approve-diff-order 'github
  "The order of the diff's files a PR buffer starts with.
The NAME of an order of `approve-diff-orders': `github' (by path,
as GitHub lists them) or `headers' (a header before the sources
of its name, `approve-diff-order-extensions').  \\<approve-pr-mode-map>\\[approve-filter-menu] s puts a
buffer in the next order; the diff's heading names an order that
isn't this one.  To review headers before their sources:

  (setq approve-diff-order \\='headers)"
  :type '(symbol :tag "Name of an order of `approve-diff-orders'")
  :group 'approve)

(defcustom approve-diff-order-extensions
  '("h" "hh" "hpp" "hxx" "h++" "c" "cc" "cpp" "cxx" "c++" "m" "mm")
  "Extensions in the order the files of one name come, for `headers'.
In the order `headers' of `approve-diff-orders', files in one
directory whose names differ only by these extensions come in
their order: src/widget.h before src/widget.cpp.  Case is
ignored; an extension not listed comes after them.  The other
files keep their order, by path.  To have C++'s alone, with the
template definitions after their header:

  (setq approve-diff-order-extensions \\='(\"h\" \"hpp\" \"tpp\" \"cpp\"))"
  :type '(repeat (string :tag "Extension, without its dot"))
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
  "The passed checks that are folded away, under \"… N more\".
Once expanded it loses that line: its checks join the list.")
(defclass approve-checks-group-section (magit-section) ()
  "A group of checks (`approve-checks-group-by').
Its value is (TITLE . OPEN), TITLE nil for \"Other\", OPEN non-nil
when one of its checks failed or still runs: such a group starts
expanded, and one that comes to fail opens again.")
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

(defun approve-sections-title (title &optional count)
  "Return TITLE as a section's title, with COUNT in parentheses if given.
COUNT is a number or nil: \"Commits (4)\"."
  (concat (approve-face title 'approve-heading)
          (and count (approve-face (format " (%s)" count) 'approve-count))))

(defun approve-sections-heading (title &optional count right width)
  "Return a section heading: TITLE, COUNT in parentheses, RIGHT aligned.
COUNT is a number or nil; RIGHT a string or nil, which ends at
column WIDTH, or at the page's right edge by default
\(`approve-render-justify')."
  (approve-render-justify (approve-sections-title title count) right width))

(defconst approve-sections-indentation (approve-render-decoration "  ")
  "The indentation of a body line, for lines that insert it themselves.")

(defun approve-sections-insert-body (start)
  "Indent the text inserted since START by two columns."
  (approve-render-prefix start (point) "  "))

(defun approve-sections--make-markers (section)
  "Give SECTION, and the sections in it, markers where they have positions.
They are made as Magit makes them: the start's advances over text
inserted there, the others don't."
  (magit-map-sections
   (lambda (section)
     (pcase-dolist (`(,slot . ,advance) '((start . t) (content) (end)))
       (let ((value (slot-value section slot)))
         (when (integerp value)
           (setf (slot-value section slot) (copy-marker value advance))))))
   section))

(defun approve-sections-insert-each (items insert)
  "Call INSERT with each of ITEMS to insert its section, however many.
INSERT is called at point, inside the current section, and inserts
sections there: one for each item, usually.  Thousands of sections
inserted the usual way take a time that grows with their square:
each insertion moves every marker in the buffer, a section has three,
and each section is added at the end of its parent's list of
children.  Here they have positions instead while they are inserted,
and their markers and their place among their parent's children
once all are.  So INSERT must only insert at point.  A section's
body inserts with markers if it is drawn at once, as it may insert
anywhere in itself: insert it with `approve-sections-lazy-body'."
  (let* ((parent magit-insert-section--current)
         (siblings (oref parent children))
         (sections nil))
    ;; A buffer-local binding, so other buffers keep their markers.
    (unless (local-variable-p 'magit-section-inhibit-markers)
      (setq-local magit-section-inhibit-markers nil))
    (unwind-protect
        (let ((magit-section-inhibit-markers t))
          (dolist (item items)
            (oset parent children nil)
            (funcall insert item)
            (dolist (section (oref parent children))
              (push section sections))))
      (setq sections (nreverse sections))
      (oset parent children (append siblings sections))
      (mapc #'approve-sections--make-markers sections))))

(defmacro approve-sections-lazy-body (&rest body)
  "Insert BODY as the current section's body, once it is expanded.
Like `magit-insert-section-body', for the sections that
`approve-sections-insert-each' inserts: a body drawn at once gets
markers, as one drawn later does."
  (declare (indent 0) (debug t))
  `(magit-insert-section-body
     (let ((magit-section-inhibit-markers nil))
       ,@body)))

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
  "Return the branch row's value: head → base.
The Commits heading counts the commits."
  (let-alist (approve-pr-fields '(baseRefName headRefName baseRef headRef
                                  headRepositoryOwner isCrossRepository))
    (let ((head (if (and .isCrossRepository .headRepositoryOwner)
                    (format "%s:%s" (alist-get 'login .headRepositoryOwner) .headRefName)
                  .headRefName)))
      (concat (approve-sections--branch head .headRef .headRepositoryOwner)
              (approve-face " → " 'approve-dim)
              (approve-sections--branch .baseRefName .baseRef nil)))))

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
view (`approve-pr-update-header-line')."
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

(defun approve-sections--meta-list (items separator)
  "Return ITEMS, strings, joined by SEPARATOR as a metadata row's value.
The value wraps between items so that the row ends by
`approve-render-width'; each next line starts under the first item."
  (approve-render-wrap items (- (approve-render-width) approve-sections--meta-width)
                       separator
                       (approve-render-decoration
                        (make-string approve-sections--meta-width ?\s))))

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
  "Return the reviewers row's value: who reviewed, then who is asked.
It wraps when they don't fit on one line (`approve-sections--meta-list')."
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
      (approve-sections--meta-list (append reviewed asked) "   "))))

(defun approve-sections--label (label)
  "Return LABEL as a coloured box."
  (let-alist label
    (approve-thing (approve-face (format " %s " .name) (approve-label-face .color))
                   (list :kind 'label :name .name :color .color
                         :description .description :url .url))))

(defun approve-sections-check-counts ()
  "Return the PR's check counts as (PASSED FAILED RUNNING SKIPPED), or nil.
SKIPPED counts the check runs that were skipped or ended neutral:
they don't fail the pull request, and GitHub counts them apart."
  (let-alist (approve-pr-field 'statusCheckRollup)
    (when .contextCounts
      (let ((passed 0) (failed 0) (running 0) (skipped 0))
        (dolist (entry (append .contextCounts.checkRunCountsByState
                               .contextCounts.statusContextCountsByState))
          (let-alist entry
            (pcase .state
              ("SUCCESS" (cl-incf passed .count))
              ((or "NEUTRAL" "SKIPPED") (cl-incf skipped .count))
              ((or "FAILURE" "ERROR" "TIMED_OUT" "CANCELLED" "STARTUP_FAILURE"
                   "ACTION_REQUIRED" "STALE")
               (cl-incf failed .count))
              (_ (cl-incf running .count)))))
        (list passed failed running skipped)))))

(defun approve-sections--rollup-icon (state)
  "Return the icon of the check rollup STATE."
  (pcase state
    ("SUCCESS" (approve-icon 'check-success 'approve-check-success))
    ((or "FAILURE" "ERROR") (approve-icon 'check-failure 'approve-check-failure))
    ((or "PENDING" "EXPECTED") (approve-icon 'check-pending 'approve-check-pending))
    (_ (approve-icon 'check-pending 'approve-dim))))

(defun approve-sections--check-counts-text (icon counts)
  "Return ICON and COUNTS in words, such as \"◐ 7 passed · 1 failed\".
COUNTS is (PASSED FAILED RUNNING SKIPPED); a count of 0 is left out."
  (pcase-let ((`(,passed ,failed ,running ,skipped) counts))
    (concat icon " "
            (approve-render-join
             (list (and (> passed 0) (approve-face (format "%d passed" passed)
                                                   'approve-check-success))
                   (and (> failed 0) (approve-face (format "%d failed" failed)
                                                   'approve-check-failure))
                   (and (> running 0) (approve-face (format "%d running" running)
                                                    'approve-check-pending))
                   (and (> skipped 0) (approve-face (format "%d skipped" skipped)
                                                    'approve-check-skipped)))))))

(defun approve-sections-checks-summary ()
  "Return the checks summary, such as \"◐ 7 passed · 1 failed\", or nil."
  (when-let* ((rollup (approve-pr-field 'statusCheckRollup))
              (counts (approve-sections-check-counts)))
    (approve-sections--check-counts-text
     (approve-sections--rollup-icon (alist-get 'state rollup)) counts)))

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
           ;; Skipped checks are left out, as they neither pass nor fail.
           (when-let* ((counts (approve-sections-check-counts)))
             (pcase-let ((`(,passed ,failed ,running ,skipped) counts))
               (let ((total (+ passed failed running)))
                 (concat (approve-sections--rollup-icon
                          (let-alist (approve-pr-field 'statusCheckRollup) .state))
                         " "
                         (if (and (= total 0) (> skipped 0))
                             (format "%d %s skipped" skipped
                                     (if (= skipped 1) "check" "checks"))
                           (format "%d of %d checks passed" passed total))))))))))

(defun approve-sections--meta-value (row)
  "Return the value of the metadata ROW as text, or nil when empty."
  (pcase row
    ('author (when-let* ((author (approve-pr-field 'author)))
               (approve-render-user author)))
    ('repository (approve-sections--repository))
    ('branch (approve-sections--branches))
    ('opened (approve-sections--dates))
    ('reviewers (approve-sections--reviewers))
    ('assignees (approve-sections--meta-list
                 (mapcar #'approve-render-user (approve-pr-field 'assignees)) "  "))
    ('labels (approve-sections--meta-list
              (mapcar #'approve-sections--label (approve-pr-field 'labels)) " "))
    ('milestone (when-let* ((milestone (approve-pr-field 'milestone)))
                  (let-alist milestone
                    (approve-thing (approve-face .title 'default)
                                   (list :kind 'milestone :title .title :due .dueOn
                                         :progress .progressPercentage :state .state
                                         :description .description :url .url)))))
    ('issues (approve-sections--meta-list
              (mapcar #'approve-sections--ref (approve-pr-field 'closingIssuesReferences))
              "   "))
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
  "Insert the viewer's pending review, when there is one.
Its heading counts its comments, replies included."
  (when-let* ((review (approve-pr-pending-review)))
    (let ((comments (approve-model-nodes (alist-get 'comments review))))
      (magit-insert-section (approve-pending-review-section (alist-get 'id review))
        (magit-insert-heading
          (approve-sections-heading
           "Your pending review" (length comments)
           (approve-render-hints '(approve-submit-review "submit")
                                 '(approve-discard-review "discard"))))
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
                                    (concat (approve-icon 'pending-comment) " suggestion")
                                  (approve-render-truncate
                                   (approve-render-first-line (alist-get 'bodyHTML comment))
                                   50))))
                      "\n")))
          (approve-sections-insert-body body))
        (insert "\n")))))

;;; Description

(defun approve-insert-description-section ()
  "Insert the description of the pull request, with its reactions.
An edited description says so right after its heading."
  (magit-insert-section (approve-description-section)
    (let-alist (approve-pr-fields '(id bodyHTML reactionGroups lastEditedAt))
      (magit-insert-heading
        (approve-render-justify
         (approve-render-join
          (list (approve-sections-title "Description")
                (and .lastEditedAt (approve-render-edited
                                    `((lastEditedAt . ,.lastEditedAt))))))
         (approve-render-hints '(approve-edit "edit"))))
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

(defun approve-sections-check-kind (check)
  "Return how CHECK went: `failure', `running', `success' or `skipped'.
A check run that was skipped or ended neutral is `skipped': it
doesn't fail the pull request, and GitHub lists it apart."
  (let-alist check
    (if (equal .__typename "StatusContext")
        (pcase .state
          ("SUCCESS" 'success)
          ((or "FAILURE" "ERROR") 'failure)
          (_ 'running))
      (if (not (equal .status "COMPLETED"))
          'running
        (pcase .conclusion
          ("SUCCESS" 'success)
          ((or "NEUTRAL" "SKIPPED") 'skipped)
          (_ 'failure))))))

(defun approve-sections-check-name (check)
  "Return the name of CHECK, with its workflow when it has one."
  (let-alist check
    (if (equal .__typename "StatusContext")
        .context
      (if-let* ((workflow .checkSuite.workflowRun.workflow.name))
          (format "%s / %s" workflow .name)
        .name))))

(defun approve-sections-check-state (check)
  "Return the state of CHECK in words, such as \"running\" or \"timed out\"."
  (let-alist check
    (let ((state (cond ((equal .__typename "StatusContext") .state)
                       ((equal .status "IN_PROGRESS") "running")
                       ((equal .status "COMPLETED") (or .conclusion "completed"))
                       (t .status))))
      (string-replace "_" " " (downcase (or state "unknown"))))))

(defun approve-sections-check-duration (check)
  "Return how long CHECK ran, such as \"2m 10s\", or nil.
Nil when it is still running or took no time: apps' checks start and
end at once, and GitHub ends skipped jobs before they start."
  (let-alist check
    (when-let* ((from (approve-render-parse-time .startedAt))
                (to (approve-render-parse-time .completedAt))
                (seconds (round (float-time (time-subtract to from))))
                ((> seconds 0)))
      (cond ((>= seconds 3600) (format "%dh %02dm" (/ seconds 3600) (/ (% seconds 3600) 60)))
            ((>= seconds 60) (format "%dm %02ds" (/ seconds 60) (% seconds 60)))
            (t (format "%ds" seconds))))))

(defun approve-sections-check-icon (check)
  "Return the icon of CHECK, by how it went."
  (pcase (approve-sections-check-kind check)
    ('success (approve-icon 'check-success 'approve-check-success))
    ('failure (approve-icon 'check-failure 'approve-check-failure))
    ('skipped (approve-icon 'check-skipped 'approve-check-skipped))
    (_ (approve-icon 'check-pending 'approve-check-pending))))

(defun approve-sections--check-when (check)
  "Return the second column of CHECK's line: its time, or its state.
A check that runs, was skipped or ended other than by passing or
failing, such as cancelled, says so; the others say how long they
took, or nothing."
  (let ((kind (approve-sections-check-kind check)))
    (if (or (memq kind '(running skipped))
            (and (eq kind 'failure)
                 (not (member (alist-get 'conclusion check) '(nil "FAILURE")))))
        (approve-face (approve-sections-check-state check)
                      (pcase kind
                        ('running 'approve-check-pending)
                        ('skipped 'approve-check-skipped)
                        (_ 'approve-check-failure)))
      (or (approve-sections-check-duration check) ""))))

(defun approve-sections--shorten-check-name (name width)
  "Return NAME cut to WIDTH columns, keeping its end.
A check's name joins its workflow's and its job's with \" / \"; the
end tells it from its siblings, so whole parts go from the start
first, then characters.

Example:
  (approve-sections--shorten-check-name \"Test All / build / Slice (ios)\" 24)
  => \"… / build / Slice (ios)\""
  (let ((parts (split-string name " / ")))
    (or (seq-find (lambda (candidate) (<= (string-width candidate) width))
                  (cons name (cl-loop for rest on (cdr parts)
                                      collect (concat "… / " (string-join rest " / ")))))
        (let ((start (length name)))
          (while (and (> start 0) (< (string-width name (1- start)) width))
            (cl-decf start))
          (concat "…" (string-trim-left (substring name start)))))))

(defun approve-sections--check-columns (rows &optional indent)
  "Return the widths of the columns of ROWS: (NAME WHEN REQUIRED INDENT).
ROWS are (CHECK . NAME), NAME the name the check's line shows.  The
lines are indented INDENT columns, 2 by default.  NAME fits the
longest name, up to the page's width less 58 and INDENT, which
leaves the notes room.  REQUIRED is 0 when no check is required."
  (let ((indent (or indent 2))
        (checks (mapcar #'car rows)))
    (list (min (apply #'max 1 (mapcar (lambda (row) (string-width (cdr row))) rows))
               (max 30 (- (approve-render-width) 58 indent)))
          (apply #'max 0 (mapcar (lambda (check)
                                   (string-width (approve-sections--check-when check)))
                                 checks))
          (if (seq-some (lambda (check) (alist-get 'isRequired check)) checks)
              (string-width "required")
            0)
          indent)))

(defun approve-sections--check-line (check name columns)
  "Return the line of CHECK, which shows NAME, in the expanded checks.
COLUMNS are the widths of its columns and its indentation, from
`approve-sections--check-columns'.  Its note (the check's title or
the status's description) fills the line up to the page's width
\(`approve-render-width')."
  (pcase-let ((`(,name-width ,when-width ,required-width ,indent) columns))
    (let-alist check
      (let* ((left (concat
                    (approve-sections-check-icon check) " "
                    (approve-render-pad (approve-sections--shorten-check-name name name-width)
                                        name-width)
                    (if (> when-width 0)
                        (concat "  " (approve-render-pad (approve-sections--check-when check)
                                                         when-width))
                      "")
                    (if (> required-width 0)
                        (concat "  " (if .isRequired
                                         (approve-face "required" 'approve-secondary-heading)
                                       (make-string required-width ?\s)))
                      "")))
             (note (car (split-string (or .description .title "") "\n")))
             ;; The indentation, and two columns between left and note.
             (room (- (approve-render-width) (string-width left) indent 2)))
        (approve-thing
         (string-trim-right
          (concat left
                  (if (string-empty-p note)
                      ""
                    (concat "  " (approve-face (approve-render-truncate note (max 20 room))
                                               'approve-dim)))))
         (list :kind 'check :check check :url (or .detailsUrl .targetUrl .url)))))))

(defun approve-sections--check-rank (check)
  "Return where CHECK comes as GitHub lists checks, the lowest first.
Failing, running, passed, then skipped; required ones first in each."
  (+ (pcase (approve-sections-check-kind check)
       ('failure 0) ('running 2) ('success 4) (_ 6))
     (if (alist-get 'isRequired check) 0 1)))

(defun approve-sections-sort-checks (checks)
  "Return CHECKS as GitHub lists them: failing, running, passed, skipped.
Required checks come first in each; the others keep GitHub's order."
  (seq-sort-by #'approve-sections--check-rank #'< checks))

(defun approve-sections--more-checks (more)
  "Return the heading of the folded checks MORE, such as \"… 4 more passed\"."
  (let ((passed (seq-count (lambda (check) (eq (approve-sections-check-kind check) 'success))
                           more)))
    (approve-face
     (concat "… " (approve-render-join
                   (list (and (> passed 0) (format "%d more passed" passed))
                         (and (< passed (length more))
                              (format (if (> passed 0) "%d skipped" "%d more skipped")
                                      (- (length more) passed))))
                   ", "))
     'approve-dim)))

(defun approve-sections--insert-check-rows (rows columns)
  "Insert the line of each of ROWS, (CHECK . NAME), in COLUMNS.
COLUMNS are from `approve-sections--check-columns'."
  (pcase-dolist (`(,check . ,name) rows)
    (magit-insert-section (approve-check-section (alist-get 'id check))
      (insert (approve-sections--check-line check name columns) "\n"))))

(defun approve-sections--insert-check-list (checks)
  "Insert CHECKS in one list, folding passed and skipped ones beyond a few.
The first `approve-checks-passed-shown' of them show, passed ones
first; the rest go under \"… N more passed\".  That line goes once it
is expanded: its checks show at once, and the buffer is drawn again
without it, as it stays (`approve-pr--visibility')."
  (let* ((rows (mapcar (lambda (check) (cons check (approve-sections-check-name check)))
                       (approve-sections-sort-checks checks)))
         (columns (approve-sections--check-columns rows))
         (shown 0)
         (listed nil)
         (more nil))
    (dolist (row rows)
      (cond ((not (memq (approve-sections-check-kind (car row)) '(success skipped)))
             (push row listed))
            ((< shown approve-checks-passed-shown)
             (cl-incf shown)
             (push row listed))
            (t (push row more))))
    (approve-sections--insert-check-rows (nreverse listed) columns)
    (when more
      (setq more (nreverse more))
      (magit-insert-section section (approve-checks-more-section nil t)
        (if (not (oref section hidden))
            (approve-sections--insert-check-rows more columns)
          (magit-insert-heading (approve-sections--more-checks (mapcar #'car more)))
          (approve-sections-lazy-body
            (let ((start (point)))
              (approve-sections--insert-check-rows more columns)
              (approve-sections-insert-body start))
            (approve-pr-schedule-redraw)))))))

;;;; Groups

(defvar approve-sections--failed-groupings (make-hash-table :test #'eq :weakness 'key)
  "The functions grouping the checks that signalled, logged once each.")

(defun approve-sections--status-prefix (check)
  "Return the context of the commit status CHECK up to its last \"/\", or nil."
  (let ((context (or (alist-get 'context check) "")))
    (when-let* ((slash (string-match-p "/[^/]*\\'" context))
                ((> slash 0)))
      (substring context 0 slash))))

(defun approve-sections-check-workflow (check)
  "Return the workflow of CHECK, the title of its group by workflow, or nil.
That is the GitHub Actions workflow that ran it, else the app that
did; for a commit status, its context up to the last \"/\": \"codecov\"
for \"codecov/patch\"."
  (let-alist check
    (if (equal .__typename "StatusContext")
        (approve-sections--status-prefix check)
      (or .checkSuite.workflowRun.workflow.name .checkSuite.app.name))))

(defun approve-sections-check-app (check)
  "Return the app that ran CHECK, the title of its group by app, or nil.
A commit status names no app: its context up to the last \"/\" stands
in, as for `approve-sections-check-workflow'."
  (let-alist check
    (if (equal .__typename "StatusContext")
        (approve-sections--status-prefix check)
      .checkSuite.app.name)))

(defun approve-sections-check-outcome (check)
  "Return how CHECK went, the title of its group by state.
That is \"Failed\", \"Running\", \"Passed\" or \"Skipped\", as
`approve-sections-check-kind' says."
  (pcase (approve-sections-check-kind check)
    ('failure "Failed")
    ('running "Running")
    ('success "Passed")
    (_ "Skipped")))

(defun approve-sections--check-grouping ()
  "Return the function that names a check's group in this buffer, or nil.
Nil lists the checks in one list.  `approve-checks-group-by-repository'
says, for the pull request's repository, else `approve-checks-group-by'."
  (let* ((repository (format "%s/%s" (approve-model-meta :owner) (approve-model-meta :repo)))
         (entry (let ((case-fold-search t))
                  (seq-find (lambda (entry) (string-match-p (car entry) repository))
                            approve-checks-group-by-repository)))
         (grouping (if entry (cdr entry) approve-checks-group-by)))
    (pcase grouping
      ('workflow #'approve-sections-check-workflow)
      ('app #'approve-sections-check-app)
      ('state #'approve-sections-check-outcome)
      ((pred functionp) grouping))))

(defun approve-sections--check-group (function check)
  "Return the title of CHECK's group by FUNCTION, or nil for \"Other\".
A FUNCTION that signals puts CHECK under \"Other\", so the checks
still draw; its error is logged once."
  (let ((title (condition-case-unless-debug err
                   (funcall function check)
                 (error
                  (unless (gethash function approve-sections--failed-groupings)
                    (puthash function t approve-sections--failed-groupings)
                    (approve-log-condition "The grouping of the checks" err))
                  nil))))
    (and (stringp title) (not (string-empty-p title)) title)))

(defun approve-sections-group-checks (checks function)
  "Return CHECKS in groups by FUNCTION, as a list of (TITLE . CHECKS).
FUNCTION returns the title of a check's group, a string, or nil: the
group of TITLE nil, \"Other\".  Each group's checks are sorted as in
one list (`approve-sections-sort-checks').  The groups come as their
first checks would, failing ones first, then by title, \"Other\" last."
  (let ((groups nil))
    (dolist (check (approve-sections-sort-checks checks))
      (let* ((title (approve-sections--check-group function check))
             (group (assoc title groups)))
        (if group
            (push check (cdr group))
          (push (list title check) groups))))
    (sort (mapcar (lambda (group) (cons (car group) (reverse (cdr group)))) groups)
          (lambda (a b)
            (let ((rank-a (approve-sections--check-rank (cadr a)))
                  (rank-b (approve-sections--check-rank (cadr b))))
              (cond ((/= rank-a rank-b) (< rank-a rank-b))
                    ((null (car a)) nil)
                    ((null (car b)) t)
                    (t (string< (downcase (car a)) (downcase (car b))))))))))

(defun approve-sections--check-name-in (check title)
  "Return the name of CHECK in the group TITLE, without TITLE at its start.
\"CI / lint\" is \"lint\" in \"CI\", and the status \"codecov/patch\"
\"patch\" in \"codecov\"."
  (let ((name (approve-sections-check-name check)))
    (or (and title
             (seq-some (lambda (separator)
                         (let ((prefix (concat title separator)))
                           (and (string-prefix-p prefix name)
                                (< (length prefix) (length name))
                                (substring name (length prefix)))))
                       '(" / " "/")))
        name)))

(defun approve-sections--check-group-heading (title checks)
  "Return the heading of the group TITLE of CHECKS, sorted.
Its title, \"Other\" for nil, how many, and how they went on the
right, after the icon of the first."
  (let ((kinds (mapcar #'approve-sections-check-kind checks)))
    (approve-render-justify
     (concat (approve-face (or title "Other") 'approve-secondary-heading)
             (approve-face (format " (%d)" (length checks)) 'approve-count))
     (approve-sections--check-counts-text
      (approve-sections-check-icon (car checks))
      (mapcar (lambda (kind) (seq-count (apply-partially #'eq kind) kinds))
              '(success failure running skipped))))))

(defun approve-sections--insert-check-groups (checks function)
  "Insert CHECKS in groups by FUNCTION, each under a heading of its own.
A group starts expanded when one of its checks failed or still runs,
collapsed otherwise, and lists all its checks.  Their columns line
up across the groups."
  (let* ((groups (mapcar (lambda (group)
                           (let ((title (car group)))
                             (cons title
                                   (mapcar (lambda (check)
                                             (cons check (approve-sections--check-name-in
                                                          check title)))
                                           (cdr group)))))
                         (approve-sections-group-checks checks function)))
         (columns (approve-sections--check-columns (apply #'append (mapcar #'cdr groups))
                                                   4)))
    (pcase-dolist (`(,title . ,rows) groups)
      (let ((open (and (seq-some (lambda (row)
                                   (memq (approve-sections-check-kind (car row))
                                         '(failure running)))
                                 rows)
                       t)))
        (magit-insert-section (approve-checks-group-section (cons title open) (not open))
          (magit-insert-heading
            (approve-sections--check-group-heading title (mapcar #'car rows)))
          (let ((start (point)))
            (approve-sections--insert-check-rows rows columns)
            (approve-sections-insert-body start)))))))

(defun approve-sections--insert-checks (checks)
  "Insert the lines of CHECKS, in one list or in groups.
`approve-checks-group-by-repository' and `approve-checks-group-by'
say which."
  (if-let* ((function (approve-sections--check-grouping)))
      (approve-sections--insert-check-groups checks function)
    (approve-sections--insert-check-list checks)))

(defun approve-insert-checks-section ()
  "Insert the checks: the summary in the heading, each check when expanded.
The checks themselves load when the section is first expanded."
  (when (or (not (approve-model-root-ref)) (approve-pr-field 'statusCheckRollup))
    (magit-insert-section (approve-checks-section nil t)
      (magit-insert-heading
        (approve-sections-heading "Checks"
                                  (when-let* ((counts (approve-sections-check-counts)))
                                    (apply #'+ counts))
                                  (approve-sections-checks-summary)))
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
  "Return the author of COMMIT: the GitHub account, else the Git name."
  (approve-render-git-actor (alist-get 'author commit)))

(defconst approve-sections--commit-author-width 20
  "The most columns the author of a commit takes in the commits.")

(defun approve-sections--commit-columns (commits)
  "Return the widths of the columns of COMMITS: (HEADLINE AUTHOR DATE).
Each fits the longest in its column, AUTHOR up to
`approve-sections--commit-author-width'.  HEADLINE has no limit of
its own, so no headline is cut: GitHub sends one of up to 72
columns, the most Git's conventions allow, whole, and cuts a
longer one itself, to 69 columns and an ellipsis."
  (cl-flet ((widest (lambda (column)
                      (apply #'max 0 (mapcar (lambda (commit)
                                               (string-width (funcall column commit)))
                                             commits)))))
    (list (widest (lambda (commit) (or (alist-get 'messageHeadline commit) "")))
          (min approve-sections--commit-author-width
               (widest #'approve-sections--commit-author))
          (widest (lambda (commit)
                    (approve-render-relative-time (alist-get 'committedDate commit)))))))

(defun approve-sections--commit-line (commit columns reviewed new)
  "Return the line of COMMIT; REVIEWED if the viewer reviewed it, NEW if after.
COLUMNS are the widths of its columns, from
`approve-sections--commit-columns'."
  (pcase-let ((`(,headline-width ,author-width ,date-width) columns))
    (let-alist commit
      (string-trim-right
       (concat (if new (approve-face "┃ " 'approve-new) "  ")
               (approve-thing (approve-face .abbreviatedOid 'approve-hash)
                              (list :kind 'commit :oid .oid :url .url))
               "  "
               (approve-render-pad (or .messageHeadline "") headline-width)
               "  "
               (approve-render-pad (approve-render-truncate
                                    (approve-sections--commit-author commit) author-width)
                                   author-width)
               "  "
               (approve-render-pad (approve-render-date .committedDate) date-width)
               "  "
               (if .statusCheckRollup
                   (approve-sections--rollup-icon .statusCheckRollup.state)
                 " ")
               (cond (reviewed (approve-face "   ← your review" 'approve-dim))
                     (new (approve-face "   new" 'approve-new))
                     (t "")))))))

(defun approve-insert-commits-section ()
  "Insert the commits, marking those after the viewer's last review."
  (let* ((connection (approve-pr-field 'commits))
         (commits (mapcar (lambda (node) (alist-get 'commit node))
                          (approve-model-nodes connection)))
         (total (or (approve-model-total-count connection)
                    (let-alist (approve-pr-field 'commitCount) .totalCount)))
         (new (approve-sections--new-commits commits))
         (reviewed (approve-sections--reviewed-oid))
         (columns (approve-sections--commit-columns commits)))
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
                  (insert (approve-sections--commit-line commit columns
                                                         (equal oid reviewed) after)
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

(defun approve-sections-asking-paths (kind)
  "Return the paths with a thread of KIND that asks for changes.
KIND is as for `approve-sections-thread-counts'.  A thread asks for
changes while it is unresolved and the review it started in still
requests them (`approve-pr-threads-asking-for-changes', D34)."
  (delete-dups
   (delq nil (mapcar (lambda (thread)
                       (let-alist thread
                         (and (eq (equal .subjectType "FILE") (eq kind 'file)) .path)))
                     (approve-pr-threads-asking-for-changes)))))

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

;;; The diff's filters

;; The filters that are on are the session's :filters, which
;; approve-filter.el turns on and off (decision D54); this file only
;; reads them, for the diff and the file list.

(defvar approve-sections--failed-filters (make-hash-table :test #'eq :weakness 'key)
  "The predicates of filters that signalled, logged once each.")

(defun approve-sections--passes-p (filter file)
  "Return non-nil if FILE passes FILTER, a plist of the session's :filters.
A predicate that signals lets the file pass, so the diff still
draws; its error is logged once."
  (let ((predicate (plist-get filter :predicate)))
    (condition-case-unless-debug err
        (funcall predicate file)
      (error
       (unless (gethash predicate approve-sections--failed-filters)
         (puthash predicate t approve-sections--failed-filters)
         (approve-log-condition (format "The diff's filter %s" (plist-get filter :key)) err))
       t))))

(defun approve-sections-filtered-p ()
  "Return non-nil if a filter of the diff is on."
  (and (approve-session-get :filters) t))

(defun approve-sections-filter-files (files)
  "Return the FILES that pass the diff's filters that are on, in order.
Each is an alist of `approve-pr-files'.  With no filter on, FILES
themselves.  A file passes every filter, or one of them when the
session's :filter-combine is `or'."
  (if-let* ((filters (approve-session-get :filters)))
      (let ((any (eq (approve-session-get :filter-combine) 'or)))
        (seq-filter (lambda (file)
                      (if any
                          (seq-some (lambda (filter) (approve-sections--passes-p filter file))
                                    filters)
                        (seq-every-p (lambda (filter) (approve-sections--passes-p filter file))
                                     filters)))
                    files))
    files))

(defun approve-sections-path-table (files)
  "Return a hash table of the paths of FILES, such as those that pass a filter."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file files)
      (puthash (alist-get 'path file) t table))
    table))

(defun approve-sections-filter-name (filter)
  "Return what FILTER keeps, in a few words: its label, else its description.
The description's trailing ellipsis goes."
  (or (plist-get filter :label)
      (string-remove-suffix "…" (string-remove-suffix "..." (plist-get filter :description)))))

(defun approve-sections-filter-note ()
  "Return the note of a heading that the diff's filters are on, or nil.
Such as \"filter: path ~ _test and owned by @alice\"."
  (when-let* ((filters (approve-session-get :filters)))
    (concat (approve-face "filter: " 'approve-dim)
            (mapconcat (lambda (filter)
                         (approve-face (approve-sections-filter-name filter) 'approve-filter))
                       filters
                       (approve-face (if (eq (approve-session-get :filter-combine) 'or)
                                         " or "
                                       " and ")
                                     'approve-dim)))))

(defun approve-sections-filter-count (shown files total)
  "Return the count of a heading of FILES, SHOWN of them passing the filters.
TOTAL is how many files the pull request changes, or nil for the
length of FILES.  With no filter on, the total alone; else \"5 of
12\"."
  (let ((total (or total (length files))))
    (if (approve-sections-filtered-p)
        (format "%d of %d" (length shown) total)
      total)))

(defun approve-sections--shown-counts (counts paths)
  "Return COUNTS, from `approve-sections-thread-counts', of PATHS alone.
PATHS is a table from `approve-sections-path-table', or nil for
every path."
  (if (null paths)
      counts
    (let ((table (make-hash-table :test #'equal)))
      (maphash (lambda (path count)
                 (when (gethash path paths)
                   (puthash path count table)))
               counts)
      table)))

(defun approve-sections-no-file-passes ()
  "Return the line that says no file passes the diff's filters, and what to do."
  (concat (approve-face "No file passes the filter" 'approve-empty)
          (if-let* ((hints (approve-render-hints '(approve-filter-menu "change it"))))
              (concat (approve-face " · " 'approve-dim) hints)
            "")
          "\n"))

;;; The diff's order

;; The order of a PR buffer's diff is the session's :diff-order, which
;; approve-filter.el sets, else `approve-diff-order' (decision D58);
;; this file only reads it.  The diff and the file list draw their
;; files in it, and the thread picker and the feedback mode go
;; through them in it.

(defvar approve-sections--failed-orders (make-hash-table :test #'eq :weakness 'key)
  "The key functions of orders that signalled, logged once each.")

(defun approve-sections-diff-order ()
  "Return the entry of `approve-diff-orders' the diff is in, or nil.
Its name is the session's :diff-order, else `approve-diff-order'.
Nil when no order has that name: the diff is then in GitHub's
order."
  (assq (or (approve-session-get :diff-order) approve-diff-order) approve-diff-orders))

(defun approve-diff-order-headers-first (file)
  "Return the sort key of FILE that puts a header before its sources.
It is FILE's path without its extension, then the rank of its
extension in `approve-diff-order-extensions', ignoring case; an
extension not there comes after those that are.  FILE is an alist
of `approve-pr-files'."
  ;; A regexp rather than `file-name-extension', which is three times
  ;; slower: thousands of files are keyed at each redraw.
  (let* ((path (alist-get 'path file))
         (start (save-match-data
                  (and (string-match "[^./]\\(\\.[^./]+\\)\\'" path)
                       (match-beginning 1))))
         (count (length approve-diff-order-extensions)))
    (if start
        (list (substring path 0 start)
              (- count (length (member-ignore-case (substring path (1+ start))
                                                   approve-diff-order-extensions))))
      (list path count))))

(defun approve-sections--key< (a b)
  "Return non-nil if the sort key A comes before the sort key B.
A key is a number, a string, or a list of them, compared element
by element: numbers by `<', strings by `string<'.  A key that is
the start of the other comes first."
  (let ((a (if (listp a) a (list a)))
        (b (if (listp b) b (list b)))
        (result 'same))
    (while (and (eq result 'same) a b)
      (let ((x (pop a)) (y (pop b)))
        (cond ((if (stringp x) (string< x y) (< x y)) (setq result t))
              ((if (stringp x) (string< y x) (< y x)) (setq result nil)))))
    (if (eq result 'same) (and b t) result)))

(defun approve-sections-order-files (files)
  "Return FILES in the diff's order (decision D58).
Each is an alist of `approve-pr-files'.  In GitHub's order, FILES
themselves; else a new list, sorted by the keys of the order of
`approve-sections-diff-order', files with equal keys as they are
in FILES.  A key function that signals, or keys that can't be
compared, leave FILES as they are; the error is logged once."
  (if-let* ((order (approve-sections-diff-order))
            (key (nth 2 order)))
      (condition-case-unless-debug err
          (mapcar #'cdr
                  (sort (mapcar (lambda (file) (cons (funcall key file) file)) files)
                        (lambda (a b) (approve-sections--key< (car a) (car b)))))
        (error
         (unless (gethash key approve-sections--failed-orders)
           (puthash key t approve-sections--failed-orders)
           (approve-log-condition (format "The diff's order %s" (car order)) err))
         files))
    files))

(defun approve-sections-diff-files ()
  "Return the changed files the diff draws, in its order.
Those of `approve-pr-files' that pass its filters (decision D54),
in the buffer's order (decision D58)."
  (approve-sections-order-files (approve-sections-filter-files (approve-pr-files))))

(defun approve-sections-order-note ()
  "Return the note of a heading that the diff is in an order of its own, or nil.
Such as \"order: headers\": the buffer's order, when it isn't
`approve-diff-order'."
  (when-let* ((name (approve-session-get :diff-order))
              ((not (eq name approve-diff-order))))
    (concat (approve-face "order: " 'approve-dim)
            (approve-face (symbol-name name) 'approve-order))))

(defun approve-sections-thread-count (counts &optional asking)
  "Return the thread mark of a file from COUNTS, (UNRESOLVED . TOTAL).
It is the unresolved count, or the total, dim, when all are resolved;
empty without threads.  With ASKING, one of them asks for changes
that still stand, and the mark is red
\(`approve-thread-mark-changes-requested', decision D34)."
  (cond ((null counts) "")
        ((> (car counts) 0)
         (let ((mark (concat (approve-icon 'comment) " " (format "%d" (car counts)))))
           (if asking (approve-face-add mark 'approve-thread-mark-changes-requested) mark)))
        (t (approve-face (concat (approve-icon 'comment) " " (format "%d" (cdr counts)))
                         'approve-dim))))

(defun approve-insert-files-section ()
  "Insert the list of changed files with their size and threads.
The threads are those on a whole file; the diff's headings count the
threads on lines, and say which files the viewer viewed.  With a
filter of the diff on, the list has only the files that pass it, and
its heading says how many of all they are (decision D54).  The files
are in the diff's order (decision D58)."
  (let* ((connection (approve-pr-field 'files))
         (all (approve-pr-files))
         (files (approve-sections-order-files (approve-sections-filter-files all)))
         (paths (and (approve-sections-filtered-p) (approve-sections-path-table files)))
         (counts (approve-sections--shown-counts (approve-sections-thread-counts 'file) paths))
         (asking (approve-sections-asking-paths 'file))
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
           (concat (approve-sections-title
                    "Files changed"
                    (and .changedFiles
                         (approve-sections-filter-count files all .changedFiles)))
                   (and .changedFiles
                        (concat "  " (if paths
                                         (approve-sections-counts
                                          (apply #'+ (mapcar (lambda (file)
                                                               (alist-get 'additions file))
                                                             files))
                                          (apply #'+ (mapcar (lambda (file)
                                                               (alist-get 'deletions file))
                                                             files)))
                                       (approve-sections-counts .additions .deletions)))))
           (and all (approve-render-join
                     (list (approve-sections-filter-note)
                           (approve-sections-order-note)
                           (approve-sections-unresolved counts "file")))))))
      ;; Each row carries its indentation: one insertion per row, where
      ;; indenting afterwards would insert again.
      (approve-sections-insert-each
       files
       (lambda (file)
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
                     (approve-sections-thread-count (gethash .path counts)
                                                    (member .path asking))
                     "\n")))))
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
                      (if all
                          (approve-sections-no-file-passes)
                        (approve-render-empty "No files changed."))))))
        (approve-sections-insert-body start))
      (insert "\n"))))

(provide 'approve-sections)

;;; approve-sections.el ends here
