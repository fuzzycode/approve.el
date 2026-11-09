;;; approve-eldoc.el --- Extended information through eldoc -*- lexical-binding: t; -*-

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

;; Extended information about the thing at point (decisions D27 and
;; D29; docs/flows.org, F20; docs/ui.org, "Eldoc"), in two tiers:
;;
;; - the echo area: one line built from the store (and from details
;;   cached before), as soon as point rests on the thing;
;; - the eldoc buffer (K, `approve-show-details'): everything worth
;;   knowing, with details the store lacks fetched on demand.
;;
;; `approve-eldoc-function' is on `eldoc-documentation-functions' in
;; every PR buffer.  It finds the context at point and its describer in
;; `approve-eldoc-describers'; a describer returns the echo line, the
;; full text, and which details it would like.  Details are fetched
;; only when K asked for this thing or the eldoc buffer is showing, are
;; cached for the session (an error for a minute), and eldoc runs again
;; once they arrive.  Moving point around never touches the network.
;;
;; Layer rules: this sits with the renderers (it draws text from the
;; store) but, as the architecture's eldoc flow says, it also starts
;; the lazy detail queries of approve-queries.  It never changes the
;; store.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'eldoc)
(require 'approve-model)
(require 'approve-context)
(require 'approve-queries)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-pr)

(defvar eldoc--doc-buffer)

;;; Cache

(defvar approve-eldoc--details (make-hash-table :test #'equal)
  "Extended details for the session, by (HOST KIND KEY).
A value is (:data DATA), (:error MESSAGE :time TIME) or (:loading t).")

(defconst approve-eldoc--error-seconds 60
  "Seconds a failed details query is remembered before it is tried again.")

(defun approve-eldoc-clear-cache ()
  "Forget every extended detail fetched this session."
  (interactive)
  (clrhash approve-eldoc--details))

(defun approve-eldoc--key (spec)
  "Return the cache key of the details SPEC, (KIND . KEY)."
  (list (approve-model-meta :host) (car spec) (cdr spec)))

(defun approve-eldoc--cached (spec)
  "Return the cache entry of the details SPEC, or nil.
An error older than `approve-eldoc--error-seconds' counts as absent."
  (let ((entry (gethash (approve-eldoc--key spec) approve-eldoc--details)))
    (if (and (plist-get entry :error)
             (> (- (float-time) (plist-get entry :time)) approve-eldoc--error-seconds))
        nil
      entry)))

;;; Text helpers

(defun approve-eldoc--row (name value)
  "Return a row of the eldoc buffer: NAME, then VALUE, or nil without VALUE."
  (when (and value (not (and (stringp value) (string-empty-p value))))
    (concat (approve-face (approve-render-pad name 10) 'approve-secondary-heading)
            (format "%s" value) "\n")))

(defun approve-eldoc--link (url)
  "Return URL as a button that opens it."
  (when url
    (buttonize (approve-face url 'approve-link) #'browse-url url)))

(defun approve-eldoc--time (iso)
  "Return the local date and time of ISO, an ISO 8601 time."
  (if-let* ((time (approve-render-parse-time iso)))
      (format-time-string "%Y-%m-%d %H:%M" time)
    ""))

(defun approve-eldoc--when (iso)
  "Return \"2 days ago (2026-11-06 09:12)\" for ISO."
  (format "%s (%s)" (approve-render-relative-time iso) (approve-eldoc--time iso)))

(defun approve-eldoc--title (&rest parts)
  "Return the first line of a doc: PARTS joined by \" — \"."
  (concat (string-join (delq nil parts) (approve-face " — " 'approve-dim)) "\n"))

;;; Describers

(defun approve-eldoc--pr-role (login)
  "Return what LOGIN does in this PR, as lines, or nil."
  (let ((pr (approve-pr-fields '(author assignees latestOpinionatedReviews reviewRequests
                                        authorAssociation))))
    (let-alist pr
      (let ((review (seq-find (lambda (review) (equal (let-alist review .author.login) login))
                              .latestOpinionatedReviews)))
        (delq nil
              (list (and (equal .author.login login)
                         (format "opened this pull request (%s)"
                                 (downcase (or .authorAssociation "contributor"))))
                    (and review
                         (let-alist review
                           (format "%s %s" (pcase .state
                                             ("APPROVED" "approved")
                                             ("CHANGES_REQUESTED" "requested changes")
                                             (_ "reviewed"))
                                   (approve-render-relative-time .submittedAt))))
                    (and (seq-find (lambda (request)
                                     (equal (let-alist request .requestedReviewer.login) login))
                                   .reviewRequests)
                         "is asked for a review")
                    (and (seq-find (lambda (user) (equal (alist-get 'login user) login))
                                   .assignees)
                         "is assigned")))))))

(defun approve-eldoc-describe-user (context details)
  "Describe the person of CONTEXT, with DETAILS from GitHub if fetched."
  (let* ((thing (plist-get context :thing))
         (login (plist-get context :value))
         (name (or (alist-get 'name details) (plist-get thing :name)))
         (handle (approve-face (concat "@" login) 'approve-user))
         (roles (approve-eldoc--pr-role login)))
    (list
     :echo (string-trim-right (approve-eldoc--title handle name))
     :details (and (not (equal (plist-get thing :type) "Bot")) (cons 'user login))
     :doc
     (concat
      (approve-eldoc--title handle
                            (and name (concat (approve-face name 'bold)
                                              (if-let* ((pronouns (alist-get 'pronouns details)))
                                                  (approve-face (format " (%s)" pronouns)
                                                                'approve-dim)
                                                ""))))
      (when-let* ((email (alist-get 'email details)))
        (unless (string-empty-p email) (concat email "\n")))
      (when-let* ((bio (alist-get 'bio details)))
        (concat "\n" (approve-face bio 'italic) "\n"))
      (when details "\n")
      (approve-eldoc--row "Company" (alist-get 'company details))
      (approve-eldoc--row "Location" (alist-get 'location details))
      (approve-eldoc--row "Website" (alist-get 'websiteUrl details))
      (when-let* ((status (alist-get 'status details)))
        (let-alist status
          (approve-eldoc--row "Status" (string-join (delq nil (list .emoji .message
                                                                     (and .indicatesLimitedAvailability
                                                                          "(busy)")))
                                                    " "))))
      (when roles
        (concat "\n" (approve-face "In this pull request" 'approve-heading) "\n"
                (mapconcat (lambda (role) (concat "  " role "\n")) roles "")))
      "\n" (approve-eldoc--link (or (plist-get context :url)
                                    (format "https://github.com/%s" login)))
      "\n"))))

(defun approve-eldoc-describe-team (context details)
  "Describe the team of CONTEXT, with DETAILS from GitHub if fetched."
  (let* ((slug (plist-get context :value))
         (handle (approve-face (concat "@" slug) 'approve-user))
         (members (let-alist details .members.totalCount)))
    (list
     :echo (string-trim-right
            (approve-eldoc--title handle
                                  (or (alist-get 'name details) "team")
                                  (and members (format "%d members" members))))
     :details (cons 'team slug)
     :doc (concat (approve-eldoc--title handle (alist-get 'name details))
                  (when-let* ((description (alist-get 'description details)))
                    (concat "\n" description "\n"))
                  (approve-eldoc--row "Members" members)
                  "\n" (approve-eldoc--link (or (alist-get 'url details)
                                                (plist-get context :url)))
                  "\n"))))

(defun approve-eldoc-describe-label (context _details)
  "Describe the label of CONTEXT."
  (let* ((thing (plist-get context :thing))
         (name (approve-face (format " %s " (plist-get thing :name))
                             (approve-label-face (plist-get thing :color))))
         (description (plist-get thing :description)))
    (list :echo (string-trim-right (approve-eldoc--title name description))
          :doc (concat (approve-eldoc--title name description)
                       (approve-eldoc--row "Colour" (and (plist-get thing :color)
                                                         (concat "#" (plist-get thing :color))))
                       "\n" (approve-eldoc--link (plist-get thing :url)) "\n"))))

(defun approve-eldoc--commit-in-pr (oid)
  "Return the PR's commit whose oid starts with OID, or nil."
  (seq-find (lambda (commit) (string-prefix-p oid (or (alist-get 'oid commit) "")))
            (mapcar (lambda (node) (alist-get 'commit node))
                    (approve-model-nodes (approve-pr-field 'commits)))))

(defun approve-eldoc--check-mark (rollup)
  "Return the checks part of a commit's line for its ROLLUP, or nil."
  (when rollup
    (let ((state (alist-get 'state rollup)))
      (concat (pcase state
                ("SUCCESS" (approve-icon 'check-success 'approve-check-success))
                ((or "FAILURE" "ERROR") (approve-icon 'check-failure 'approve-check-failure))
                (_ (approve-icon 'check-pending 'approve-check-pending)))
              " checks"))))

(defun approve-eldoc--git-actor (actor)
  "Return the Git ACTOR (author or committer) as text."
  (let-alist actor
    (if .user.login
        (approve-face .user.login 'approve-user)
      (approve-face (or .name "?") 'approve-user))))

(defun approve-eldoc-describe-commit (context details)
  "Describe the commit of CONTEXT, with DETAILS from GitHub if fetched."
  (let* ((oid (plist-get context :value))
         (commit (or (approve-eldoc--commit-in-pr oid) details))
         (sha (approve-face (or (alist-get 'abbreviatedOid commit) (substring oid 0 (min 7 (length oid))))
                            'approve-hash)))
    (let-alist commit
      (let ((headline (or .messageHeadline
                          (and (alist-get 'message details)
                               (car (split-string (alist-get 'message details) "\n"))))))
        (list
         :echo (string-trim-right
                (approve-eldoc--title
                 sha headline
                 (and .author
                      (approve-render-join
                       (list (approve-eldoc--git-actor .author)
                             (approve-eldoc--time (or .committedDate .author.date))
                             (approve-eldoc--check-mark .statusCheckRollup))))))
         :details (cons 'commit (or .oid oid))
         :doc
         (concat
          (approve-eldoc--title sha headline)
          (when-let* ((message (alist-get 'message details)))
            (let ((body (string-trim (string-join (cdr (split-string message "\n")) "\n"))))
              (unless (string-empty-p body) (concat "\n" body "\n"))))
          "\n"
          (approve-eldoc--row "Author" (and .author (concat (approve-eldoc--git-actor .author) " "
                                                            (approve-eldoc--when .author.date))))
          (let-alist details
            (concat
             (and .committer (not (equal .committer.email .author.email))
                  (approve-eldoc--row "Committer" (approve-eldoc--git-actor .committer)))
             (approve-eldoc--row "Signature"
                                 (and .signature
                                      (if .signature.isValid
                                          (format "valid (%s)" (or .signature.signer.login
                                                                   (downcase .signature.state)))
                                        (downcase (or .signature.state "invalid")))))
             (approve-eldoc--row "Changes" (and .additions
                                                (format "%s files, +%d −%d"
                                                        (or .changedFilesIfAvailable "?")
                                                        .additions .deletions)))))
          (approve-eldoc--row "Checks" (approve-eldoc--check-mark .statusCheckRollup))
          "\n" (approve-eldoc--link (or (plist-get context :url) .url)) "\n"))))))

(defun approve-eldoc--ref-key (context)
  "Return the (OWNER REPO NUMBER) of the #123 reference of CONTEXT."
  (let ((thing (plist-get context :thing)))
    (list (or (plist-get thing :owner) (approve-model-meta :owner))
          (or (plist-get thing :repo) (approve-model-meta :repo))
          (plist-get context :value))))

(defun approve-eldoc--state (state type)
  "Return the STATE of an issue or pull request of TYPE in words."
  (format "%s %s" (downcase (or state "")) (if (equal type "PullRequest") "pull request" "issue")))

(defun approve-eldoc-describe-ref (context details)
  "Describe the issue or pull request of CONTEXT, with DETAILS if fetched."
  (let* ((thing (plist-get context :thing))
         (number (approve-face (format "#%s" (plist-get context :value)) 'approve-link))
         (title (or (alist-get 'title details) (plist-get thing :title)))
         (state (or (alist-get 'state details) (plist-get thing :state)))
         (type (or (alist-get '__typename details) "Issue")))
    (list
     :echo (string-trim-right (approve-eldoc--title number title
                                                    (and state (approve-eldoc--state state type))))
     :details (cons 'ref (approve-eldoc--ref-key context))
     :doc
     (concat
      (approve-eldoc--title number (and title (approve-face title 'bold)))
      (approve-eldoc--row "State" (and state (approve-eldoc--state state type)))
      (approve-eldoc--row "Author" (let-alist details .author.login))
      (approve-eldoc--row "Labels" (mapconcat (lambda (label)
                                                (let-alist label
                                                  (approve-face (format " %s " .name)
                                                                (approve-label-face .color))))
                                              (let-alist details .labels.nodes) " "))
      (when-let* ((body (alist-get 'bodyText details)))
        (unless (string-empty-p body)
          (concat "\n" (approve-render-truncate (string-trim body) 400) "\n")))
      "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))

(defun approve-eldoc-describe-check (context _details)
  "Describe the check of CONTEXT."
  (let* ((check (plist-get (plist-get context :thing) :check)))
    (let-alist check
      (let* ((name (approve-face (if .context .context
                                   (if .checkSuite.workflowRun.workflow.name
                                       (format "%s / %s" .checkSuite.workflowRun.workflow.name .name)
                                     .name))
                                 'bold))
             (result (or (and .conclusion (downcase .conclusion))
                         (and .state (downcase .state))
                         (and .status (downcase .status))))
             (summary (or .title .description)))
        (list
         :echo (string-trim-right
                (approve-eldoc--title name result
                                      (approve-render-join
                                       (list (and summary (format "“%s”" summary))
                                             (and .isRequired "required")))))
         :doc (concat
               (approve-eldoc--title name result)
               (and summary (concat "\n" summary "\n\n"))
               (approve-eldoc--row "Required" (if .isRequired "yes" "no"))
               (approve-eldoc--row "App" .checkSuite.app.name)
               (approve-eldoc--row "Started" (and .startedAt (approve-eldoc--when .startedAt)))
               (approve-eldoc--row "Finished" (and .completedAt (approve-eldoc--when .completedAt)))
               "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))))

(defun approve-eldoc-describe-file (context _details)
  "Describe the changed file of CONTEXT."
  (let* ((path (plist-get context :path))
         (file (seq-find (lambda (file) (equal (alist-get 'path file) path)) (approve-pr-files)))
         (entry (when-let* ((table (approve-model-extra :patches))) (gethash path table)))
         (threads (approve-pr-list-fields 'reviewThreads '(path isResolved)))
         (mine (seq-filter (lambda (thread) (equal (alist-get 'path thread) path)) threads))
         (open (seq-count (lambda (thread) (not (alist-get 'isResolved thread))) mine))
         (viewed (pcase (plist-get context :viewed)
                   ("VIEWED" "viewed") ("DISMISSED" "changed since viewed") (_ "not viewed"))))
    (let-alist file
      (let ((summary (approve-render-join
                      (list (downcase (or .changeType ""))
                            (and .additions (format "+%d −%d" .additions .deletions))))))
        (list
         :echo (string-trim-right
                (approve-eldoc--title (approve-face path 'approve-path) summary
                                      (approve-render-join
                                       (list viewed
                                             (and mine (format "%d thread%s%s" (length mine)
                                                               (if (cdr mine) "s" "")
                                                               (if (> open 0)
                                                                   (format ", %d unresolved" open)
                                                                 "")))))))
         :doc (concat (approve-eldoc--title (approve-face path 'approve-path) summary)
                      (approve-eldoc--row "Renamed" (alist-get 'previous_filename entry))
                      (approve-eldoc--row "Viewed" viewed)
                      (approve-eldoc--row "Threads" (and mine (format "%d, %d unresolved"
                                                                      (length mine) open)))
                      "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))))

(defun approve-eldoc--comment-line (comment)
  "Return a one-line summary of the review COMMENT."
  (let-alist comment
    (concat (approve-render-user .author) (approve-face " · " 'approve-dim)
            (approve-render-relative-time .createdAt) ": "
            (approve-render-truncate (approve-render-first-line .bodyHTML) 60))))

(defun approve-eldoc-describe-thread (context details)
  "Describe the thread of CONTEXT, with DETAILS (its original code) if fetched."
  (let* ((id (plist-get context :thread-id))
         (thread (and (approve-model-has-entity-p "PullRequestReviewThread" id)
                      (approve-model-get "PullRequestReviewThread" id))))
    (let-alist thread
      (let* ((comments (approve-model-nodes .comments))
             (count (or (approve-model-total-count .comments) (length comments)))
             (state (cond (.isResolved (if .resolvedBy
                                           (format "resolved by %s" .resolvedBy.login)
                                         "resolved"))
                          ((or .isOutdated (null .line))
                           (format "outdated (was line %s)" (or .originalLine "?")))
                          ((equal .subjectType "FILE") "on the file")
                          (t (format "on line %s" .line))))
             (outdated (or .isOutdated (null .line))))
        (list
         :echo (string-trim-right
                (approve-eldoc--title "thread"
                                      (approve-face state (if outdated 'approve-outdated 'default))
                                      (concat (format "%d comment%s" count (if (= count 1) "" "s"))
                                              (if outdated " · K shows the original code" ""))))
         :details (and outdated (car comments) (cons 'comment-code (alist-get 'id (car comments))))
         :doc
         (concat
          (approve-eldoc--title (format "Thread on %s" .path) state)
          (when-let* ((hunk (alist-get 'diffHunk details)))
            (concat (approve-face (format "Written on line %s at %s; that line is no longer in the diff."
                                          (or (alist-get 'originalLine details) "?")
                                          (let-alist details .originalCommit.abbreviatedOid))
                                  'approve-dim)
                    "\n\n"
                    (mapconcat (lambda (line)
                                 (approve-face line (cond ((string-prefix-p "@@" line)
                                                           'approve-diff-hunk-heading)
                                                          ((string-prefix-p "+" line)
                                                           'approve-diff-added)
                                                          ((string-prefix-p "-" line)
                                                           'approve-diff-removed)
                                                          (t 'approve-diff-context))))
                               (split-string hunk "\n") "\n")
                    "\n"))
          "\n"
          (mapconcat (lambda (comment) (concat (approve-eldoc--comment-line comment) "\n"))
                     comments "")
          "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))))

(defun approve-eldoc--reactors (content details)
  "Return who gave the reaction CONTENT, from DETAILS, as text, or nil."
  (when-let* ((group (seq-find (lambda (group) (equal (alist-get 'content group) content))
                               (alist-get 'reactionGroups details))))
    (let-alist group
      (let ((names (mapcar (lambda (node) (alist-get 'login node)) .reactors.nodes)))
        (concat (string-join names ", ")
                (if (> .reactors.totalCount (length names))
                    (format " and %d more" (- .reactors.totalCount (length names)))
                  ""))))))

(defun approve-eldoc--reaction-count (group)
  "Return \"you and 2 others\" or \"3\" for the reaction GROUP."
  (let-alist group
    (if .viewerHasReacted
        (if (> .reactors.totalCount 1)
            (format "you and %d other%s" (1- .reactors.totalCount)
                    (if (= .reactors.totalCount 2) "" "s"))
          "you")
      (format "%d" .reactors.totalCount))))

(defun approve-eldoc-describe-reactions (context details)
  "Describe the reactions of CONTEXT, with DETAILS (who reacted) if fetched."
  (let* ((thing (plist-get context :thing))
         (groups (plist-get thing :groups))
         (line (lambda (group)
                 (let-alist group
                   (format "%s %d — %s" (approve-render-reaction-emoji .content)
                           .reactors.totalCount (approve-eldoc--reaction-count group))))))
    (list
     :echo (mapconcat line groups (approve-face " · " 'approve-dim))
     :details (and (plist-get thing :subject) (cons 'reactors (plist-get thing :subject)))
     :doc (concat
           (mapconcat (lambda (group)
                        (concat (funcall line group) "\n"
                                (when-let* ((who (approve-eldoc--reactors
                                                  (alist-get 'content group) details)))
                                  (concat "  " who "\n"))))
                      groups "")))))

(defun approve-eldoc-describe-date (context _details)
  "Describe the date of CONTEXT."
  (let* ((iso (plist-get context :value))
         (label (plist-get (plist-get context :thing) :label))
         (text (approve-eldoc--title (if label
                                         (format "%s %s" label (approve-render-relative-time iso))
                                       (approve-render-relative-time iso))
                                     (approve-eldoc--time iso))))
    (list :echo (string-trim-right text) :doc text)))

(defun approve-eldoc-describe-milestone (context _details)
  "Describe the milestone of CONTEXT."
  (let ((thing (plist-get context :thing)))
    (let ((title (approve-face (or (plist-get thing :title) "") 'bold))
          (due (and (plist-get thing :due)
                    (format "due %s" (format-time-string
                                      "%Y-%m-%d" (approve-render-parse-time (plist-get thing :due))))))
          (progress (and (plist-get thing :progress)
                         (format "%d%% done" (round (plist-get thing :progress))))))
      (list :echo (string-trim-right (approve-eldoc--title title due progress))
            :doc (concat (approve-eldoc--title title due progress)
                         (when-let* ((description (plist-get thing :description)))
                           (unless (string-empty-p description)
                             (concat "\n" description "\n")))
                         "\n" (approve-eldoc--link (plist-get thing :url)) "\n")))))

(defun approve-eldoc-describe-branch (context _details)
  "Describe the branch of CONTEXT."
  (let* ((thing (plist-get context :thing))
         (ref (plist-get thing :ref))
         (repository (alist-get 'repository ref))
         (owner (or (alist-get 'login (plist-get thing :owner))
                    (let-alist repository .owner.login)))
         (name (approve-face (if owner (format "%s:%s" owner (plist-get thing :name))
                               (plist-get thing :name))
                             'approve-branch))
         (where (cond ((null ref) "deleted")
                      ((alist-get 'isFork repository) "in a fork")
                      (t "in this repository"))))
    (list :echo (string-trim-right (approve-eldoc--title name where))
          :doc (concat (approve-eldoc--title name where)
                       (approve-eldoc--row "Repository" (alist-get 'nameWithOwner repository))
                       "\n" (approve-eldoc--link (alist-get 'url repository)) "\n"))))

(defvar approve-eldoc-describers
  '((user . approve-eldoc-describe-user)
    (team . approve-eldoc-describe-team)
    (label . approve-eldoc-describe-label)
    (commit . approve-eldoc-describe-commit)
    (ref . approve-eldoc-describe-ref)
    (check . approve-eldoc-describe-check)
    (file . approve-eldoc-describe-file)
    (thread . approve-eldoc-describe-thread)
    (reactions . approve-eldoc-describe-reactions)
    (date . approve-eldoc-describe-date)
    (milestone . approve-eldoc-describe-milestone)
    (branch . approve-eldoc-describe-branch))
  "Describers of things at point, by context kind.
Each is called with the context and the details fetched for it (or
nil) and returns a plist: :echo, the line for the echo area; :doc,
the text for the eldoc buffer; :details, (KIND . KEY) of the details
query that would add to them (see `approve-query-details'), or nil.")

;;; The eldoc function

(defvar-local approve-eldoc--wanted nil
  "The context whose details \\[approve-show-details] asked for.")

(defun approve-eldoc--identity (context)
  "Return what identifies the thing of CONTEXT, to compare contexts."
  (list (plist-get context :kind) (plist-get context :value)
        (plist-get context :thread-id) (plist-get context :path)))

(defun approve-eldoc--buffer-visible-p ()
  "Return non-nil if the eldoc buffer is in a window."
  (and (boundp 'eldoc--doc-buffer)
       (buffer-live-p eldoc--doc-buffer)
       (get-buffer-window eldoc--doc-buffer t)))

(defun approve-eldoc--fetch (spec)
  "Fetch the details SPEC, then document point again in this buffer."
  (let ((key (approve-eldoc--key spec))
        (buffer (current-buffer)))
    (puthash key '(:loading t) approve-eldoc--details)
    (approve-query-details
     (car spec) (cdr spec)
     :callback (lambda (data)
                 (puthash key (list :data data) approve-eldoc--details)
                 (approve-eldoc--refresh buffer))
     :errorback (lambda (err)
                  (puthash key (list :error (if (stringp (cadr err)) (cadr err)
                                              (format "%s" err))
                                     :time (float-time))
                           approve-eldoc--details)
                  (approve-eldoc--refresh buffer)))))

(defun approve-eldoc--refresh (buffer)
  "Run eldoc again in BUFFER when it is the selected one."
  (when (and (buffer-live-p buffer) (eq buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (eldoc-print-current-symbol-info t))))

(defun approve-eldoc-describe (context &optional fetch)
  "Return the description of CONTEXT: a plist with :echo and :doc.
Details already cached are used.  With FETCH, missing details are
fetched (and eldoc runs again when they arrive); the doc then says
they are loading.  Return nil when nothing describes CONTEXT."
  (when-let* ((describer (alist-get (plist-get context :kind) approve-eldoc-describers)))
    (let* ((first (funcall describer context nil))
           (spec (plist-get first :details))
           (entry (and spec (approve-eldoc--cached spec)))
           (result (if (plist-get entry :data)
                       (funcall describer context (plist-get entry :data))
                     first))
           (note nil))
      (cond
       ((plist-get entry :loading)
        (setq note (concat (approve-icon 'loading 'approve-loading) " "
                           (approve-face "loading details…" 'approve-loading))))
       ((plist-get entry :error)
        (setq note (approve-face (format "details unavailable (%s)" (plist-get entry :error))
                                 'approve-dim)))
       ((and spec (null entry) fetch)
        (approve-eldoc--fetch spec)
        (setq note (concat (approve-icon 'loading 'approve-loading) " "
                           (approve-face "loading details…" 'approve-loading)))))
      (list :echo (plist-get result :echo)
            :doc (concat (plist-get result :doc) (and note (concat "\n" note "\n")))))))

(defun approve-eldoc-function (callback &rest _)
  "Document the thing at point for eldoc, calling CALLBACK.
The echo line comes from the store; details are fetched only when
\\[approve-show-details] asked for this thing or the eldoc buffer
is showing.  Return nil when there is nothing to say."
  (when (approve-model-root-ref)
    (let* ((context (approve-context-at-point))
           (fetch (or (approve-eldoc--buffer-visible-p)
                      (equal approve-eldoc--wanted (approve-eldoc--identity context))))
           (description (approve-eldoc-describe context fetch)))
      (when description
        (funcall callback (plist-get description :doc)
                 :echo (plist-get description :echo))
        t))))

(defun approve-show-details ()
  "Show everything about the thing at point in the eldoc buffer (K).
Details GitHub has but the buffer lacks are fetched once per session."
  (interactive)
  (setq approve-eldoc--wanted (approve-eldoc--identity (approve-context-at-point)))
  (eldoc-print-current-symbol-info t)
  (when (and (boundp 'eldoc--doc-buffer) (buffer-live-p eldoc--doc-buffer))
    (eldoc-doc-buffer t)))

(defun approve-eldoc-setup ()
  "Turn on Approve's eldoc in the current buffer."
  (add-hook 'eldoc-documentation-functions #'approve-eldoc-function nil t)
  (eldoc-mode 1))

(add-hook 'approve-pr-mode-hook #'approve-eldoc-setup)
(add-hook 'approve-pr-clear-cache-functions #'approve-eldoc-clear-cache)
(keymap-set approve-pr-mode-map "K" #'approve-show-details)

(provide 'approve-eldoc)

;;; approve-eldoc.el ends here
