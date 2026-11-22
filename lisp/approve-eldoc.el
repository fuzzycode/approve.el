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
;; every PR buffer and in the dashboard.  It finds the context at point
;; and its describer in `approve-eldoc-describers'; a describer returns
;; the echo line, the full text, and which details it would like.
;; Details are fetched only when K asked for this thing or the eldoc
;; buffer is showing, are cached for the session (an error for a
;; minute), and eldoc runs again once they arrive.  Moving point around
;; never touches the network.
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
(require 'approve-log)
(require 'approve-context)
(require 'approve-queries)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-pr)
(require 'approve-sections)

(defvar eldoc--doc-buffer)

;;; Cache

(defvar approve-eldoc--details (make-hash-table :test #'equal)
  "Extended details for the session, by (HOST KIND KEY).
A value is (:data DATA), (:error MESSAGE :time TIME) or (:loading t
:buffer BUFFER), BUFFER being the one that asked for them.")

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
An error older than `approve-eldoc--error-seconds' counts as absent,
and so do details still loading for a buffer killed since: their
answer is dropped with it (`approve-api-graphql')."
  (let ((entry (gethash (approve-eldoc--key spec) approve-eldoc--details)))
    (cond ((and (plist-get entry :error)
                (> (- (float-time) (plist-get entry :time)) approve-eldoc--error-seconds))
           nil)
          ((and (plist-get entry :loading)
                (not (buffer-live-p (plist-get entry :buffer))))
           nil)
          (t entry))))

;;; Text helpers

(defun approve-eldoc--row (name value)
  "Return a row of the eldoc buffer: NAME, then VALUE, or nil without VALUE."
  (when (and value (not (and (stringp value) (string-empty-p value))))
    (concat (approve-face (approve-render-pad name 11) 'approve-secondary-heading)
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
  "Return the first line of a doc: PARTS joined by \" — \".
Parts that are nil or empty are left out."
  (concat (string-join (seq-remove (lambda (part) (or (null part) (string-empty-p part)))
                                   parts)
                       (approve-face " — " 'approve-dim))
          "\n"))

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
  (approve-render-git-actor actor))

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
                                          (format "valid (%s)" (if .signature.signer.login
                                                                   (concat "@" .signature.signer.login)
                                                                 (downcase .signature.state)))
                                        (downcase (or .signature.state "invalid")))))
             (approve-eldoc--row "Changes" (and .additions
                                                (format "%s files, +%d −%d"
                                                        (or .changedFilesIfAvailable "?")
                                                        .additions .deletions)))))
          (approve-eldoc--row "Checks" (approve-eldoc--check-mark .statusCheckRollup))
          "\n" (approve-eldoc--link (or (plist-get context :url) .url)) "\n"))))))

(defun approve-eldoc-describe-git-actor (context _details)
  "Describe the Git author of CONTEXT, a commit's author with no account.
GitHub links a commit to an account by its author's e-mail; when no
account has that e-mail (verified), GitHub only knows the name Git
wrote, and so does Approve."
  (let* ((thing (plist-get context :thing))
         (name (approve-face (or (plist-get thing :name) "?") 'approve-user))
         (email (plist-get thing :email))
         (note (approve-face "no GitHub account" 'approve-dim)))
    (list
     :echo (string-trim-right
            (approve-eldoc--title name (and email (format "<%s>" email)) note))
     :doc
     (concat
      (approve-eldoc--title name (and email (format "<%s>" email)))
      "\n"
      (approve-face
       (concat "The commit's author as Git wrote it.  GitHub links a commit to an\n"
               "account by its author's e-mail, and no account has this one\n"
               "verified, so there is no @login to show.\n")
       'approve-dim)))))

(defun approve-eldoc--ref-key (context)
  "Return the (OWNER REPO NUMBER) of the #123 reference of CONTEXT."
  (let ((thing (plist-get context :thing)))
    (list (or (plist-get thing :owner) (approve-model-meta :owner))
          (or (plist-get thing :repo) (approve-model-meta :repo))
          (plist-get context :value))))

(defun approve-eldoc--state (state type)
  "Return the STATE of an issue or pull request of TYPE in words."
  (format "%s %s" (downcase (or state "")) (if (equal type "PullRequest") "pull request" "issue")))

(defun approve-eldoc--labels (details)
  "Return the labels of DETAILS, a #123 reference's, as boxes."
  (mapconcat (lambda (label)
               (let-alist label
                 (approve-face (format " %s " .name) (approve-label-face .color))))
             (let-alist details .labels.nodes) " "))

(defun approve-eldoc--body (details)
  "Return the start of the text of DETAILS, a #123 reference's, or nil."
  (when-let* ((body (alist-get 'bodyText details)))
    (unless (string-empty-p body)
      (concat "\n" (approve-render-truncate (string-trim body) 400) "\n"))))

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
      (approve-eldoc--row "Author" (let-alist details
                                     (and .author.login (concat "@" .author.login))))
      (approve-eldoc--row "Labels" (approve-eldoc--labels details))
      (approve-eldoc--body details)
      "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))

(defun approve-eldoc--pr-state (pr)
  "Return the state of PR, a row of a list, in words: open, draft, merged, closed."
  (let-alist pr
    (pcase .state
      ("MERGED" "merged")
      ("CLOSED" "closed")
      (_ (if .isDraft "draft" "open")))))

(defun approve-eldoc--pr-checks (pr &optional icon)
  "Return the checks of PR's last commit in words, or nil without checks.
With ICON, the words follow the checks' mark."
  (pcase-let ((`(,name ,face ,words)
               (pcase (let-alist pr .statusCheckRollup.state)
                 ("SUCCESS" '(check-success approve-check-success "passed"))
                 ((or "FAILURE" "ERROR") '(check-failure approve-check-failure "failing"))
                 ((or "PENDING" "EXPECTED") '(check-pending approve-check-pending "running")))))
    (when words
      (if icon
          (concat (approve-icon name face) " " words)
        (concat "checks " words)))))

(defun approve-eldoc--pr-review (pr)
  "Return GitHub's review decision on PR in words, or nil without one."
  (pcase (alist-get 'reviewDecision pr)
    ("APPROVED" "approved")
    ("CHANGES_REQUESTED" "changes requested")
    ("REVIEW_REQUIRED" "review required")))

(defconst approve-eldoc--roles
  '(("OWNER" . "owner") ("MEMBER" . "member") ("COLLABORATOR" . "collaborator")
    ("CONTRIBUTOR" . "contributor") ("FIRST_TIME_CONTRIBUTOR" . "first-time contributor")
    ("FIRST_TIMER" . "new to GitHub") ("MANNEQUIN" . "mannequin"))
  "GitHub's author associations in words; NONE has none.")

(defun approve-eldoc--pr-author (pr)
  "Return PR's author as @login, with their role in the repository."
  (let-alist pr
    (concat (approve-render-user .author)
            (when-let* ((role (cdr (assoc .authorAssociation approve-eldoc--roles))))
              (approve-face (format " (%s)" role) 'approve-dim)))))

(defun approve-eldoc--count (count noun)
  "Return COUNT and NOUN, plural unless COUNT is 1: \"3 files\"."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun approve-eldoc--pr-discussion (pr)
  "Return PR's discussion, such as \"5 comments, 15 threads\", or nil.
Comments are in the conversation, threads on the code; GitHub's lists
and gh-dash count the two together."
  (let-alist pr
    (let ((parts (delq nil (list (and .commentCount.totalCount (> .commentCount.totalCount 0)
                                      (approve-eldoc--count .commentCount.totalCount "comment"))
                                 (and .threadCount.totalCount (> .threadCount.totalCount 0)
                                      (approve-eldoc--count .threadCount.totalCount "thread"))))))
      (and parts (string-join parts ", ")))))

(defun approve-eldoc--pr-size (pr)
  "Return PR's additions and deletions, \"+7 −2\", or nil."
  (let-alist pr
    (and .additions
         (concat (approve-face (format "+%d" .additions) 'approve-diffstat-added)
                 " " (approve-face (format "−%d" .deletions) 'approve-diffstat-removed)))))

(defun approve-eldoc--pr-branch (pr)
  "Return PR's head and base branches, \"feature → main\", or nil."
  (let-alist pr
    (and .headRefName
         (concat (approve-face .headRefName 'approve-branch) " → "
                 (approve-face .baseRefName 'approve-branch)))))

(defun approve-eldoc--login (login)
  "Return LOGIN, a user's or a team's, as @login."
  (approve-face (concat "@" (or login "ghost")) 'approve-user))

(defun approve-eldoc--pr-reviewers (details)
  "Return who reviews the pull request of DETAILS, or nil.
Each reviewer's latest verdict comes first, then who is asked."
  (let-alist details
    (let ((verdicts
           (mapcar (lambda (review)
                     (let-alist review
                       (concat (pcase .state
                                 ("APPROVED" (approve-icon 'approved 'approve-approved))
                                 ("CHANGES_REQUESTED"
                                  (approve-icon 'changes-requested 'approve-changes-requested))
                                 ("DISMISSED" (approve-icon 'dismissed 'approve-dismissed))
                                 (_ (approve-icon 'commented 'approve-commented)))
                               " " (approve-eldoc--login .author.login))))
                   .latestOpinionatedReviews.nodes))
          (asked
           (mapcar (lambda (request)
                     (let-alist request
                       (concat (approve-icon 'requested 'approve-requested) " "
                               (approve-eldoc--login (or .requestedReviewer.login
                                                         .requestedReviewer.combinedSlug)))))
                   .reviewRequests.nodes)))
      (and (or verdicts asked) (string-join (append verdicts asked) "  ")))))

(defun approve-eldoc--pr-closes (pr details)
  "Return the issues the pull request PR closes, from its DETAILS, or nil.
An issue of another repository is named with it."
  (when-let* ((issues (let-alist details .closingIssuesReferences.nodes)))
    (mapconcat (lambda (issue)
                 (let-alist issue
                   (concat (approve-face (if (equal .repository.nameWithOwner
                                                    (let-alist pr .repository.nameWithOwner))
                                             (format "#%s" .number)
                                           (format "%s#%s" .repository.nameWithOwner .number))
                                         'approve-number)
                           " " (or .title "")
                           (if (equal .state "CLOSED") (approve-face " (closed)" 'approve-dim) ""))))
               issues ", ")))

(defun approve-eldoc--pr-mergeable (pr details)
  "Return whether the open pull request PR merges, from DETAILS, or nil."
  (when (equal (alist-get 'state pr) "OPEN")
    (pcase (alist-get 'mergeable details)
      ("MERGEABLE" "yes")
      ("CONFLICTING" (approve-face "no, it has conflicts" 'approve-error))
      ("UNKNOWN" "not known yet"))))

(defun approve-eldoc--pr-details-key (pr)
  "Return the key of PR's details: (OWNER REPO NUMBER WEB-HOST), or nil."
  (when-let* ((parsed (approve-parse-url (alist-get 'url pr))))
    (list (plist-get parsed :owner) (plist-get parsed :repo) (plist-get parsed :number)
          (plist-get parsed :host))))

(defun approve-eldoc-describe-pull-request (context details)
  "Describe the pull request of CONTEXT, a row of a list.
The echo line adds to the row what it lacks or cuts: the whole title,
the author's role, the state in words, the discussion, the size and
the branches.  DETAILS, once \\[approve-show-details] fetched them, add
who reviews it, assignees, labels, milestone, the issues it closes,
its commits, whether it merges, and the start of its text."
  (let ((pr (plist-get (plist-get context :thing) :pr)))
    (let-alist pr
      (let* ((number (approve-face (format "#%s" .number) 'approve-number))
             (state (approve-eldoc--pr-state pr))
             (files (and .changedFiles (approve-eldoc--count .changedFiles "file")))
             (size (approve-eldoc--pr-size pr))
             (commits (let-alist details .commitCount.totalCount)))
        (list
         :echo (string-trim-right
                (approve-eldoc--title
                 (concat number " " (approve-render-truncate (or .title "") 60))
                 (approve-render-join
                  (list (unless (equal state "open") state)
                        (approve-eldoc--pr-author pr)
                        (approve-eldoc--pr-review pr)
                        (approve-eldoc--pr-checks pr)
                        (approve-eldoc--pr-discussion pr)
                        (and size (concat size (and files (concat " in " files))))
                        (approve-eldoc--pr-branch pr)))))
         :details (when-let* ((key (approve-eldoc--pr-details-key pr)))
                    (cons 'pull-request key))
         :doc
         (concat
          (approve-eldoc--title number (approve-face (or .title "") 'bold))
          "\n"
          (approve-eldoc--row "State" state)
          (approve-eldoc--row "Author" (approve-eldoc--pr-author pr))
          (approve-eldoc--row "Repository" .repository.nameWithOwner)
          (approve-eldoc--row "Branch" (approve-eldoc--pr-branch pr))
          (approve-eldoc--row "Changes" (approve-render-join
                                         (list files size
                                               (and commits (approve-eldoc--count commits "commit")))))
          (approve-eldoc--row "Checks" (approve-eldoc--pr-checks pr t))
          (approve-eldoc--row "Review" (approve-eldoc--pr-review pr))
          (approve-eldoc--row "Reviewers" (approve-eldoc--pr-reviewers details))
          (approve-eldoc--row "Assignees" (mapconcat (lambda (user)
                                                       (approve-eldoc--login (alist-get 'login user)))
                                                     (let-alist details .assignees.nodes) " "))
          (approve-eldoc--row "Labels" (approve-eldoc--labels details))
          (approve-eldoc--row "Milestone"
                              (let-alist details
                                (and .milestone.title
                                     (concat .milestone.title
                                             (and .milestone.dueOn
                                                  (approve-face
                                                   (format-time-string
                                                    " (due %Y-%m-%d)"
                                                    (approve-render-parse-time .milestone.dueOn))
                                                   'approve-dim))))))
          (approve-eldoc--row "Closes" (approve-eldoc--pr-closes pr details))
          (approve-eldoc--row "Discussion" (approve-eldoc--pr-discussion pr))
          (approve-eldoc--row "Mergeable" (approve-eldoc--pr-mergeable pr details))
          (approve-eldoc--row "Opened" (and .createdAt (approve-eldoc--when .createdAt)))
          (approve-eldoc--row "Updated" (and .updatedAt (approve-eldoc--when .updatedAt)))
          (approve-eldoc--body details)
          "\n" (approve-eldoc--link .url) "\n"))))))

(defun approve-eldoc-describe-search (context _details)
  "Describe the search of CONTEXT, a list of the dashboard."
  (let* ((thing (plist-get context :thing))
         (search (or (plist-get context :value) ""))
         (total (plist-get thing :total))
         (found (and total (format "%d found" total))))
    (list :echo (string-trim-right (approve-eldoc--title (plist-get thing :title) search found))
          :doc (concat (approve-eldoc--title (plist-get thing :title) found)
                       "\n" (approve-eldoc--row "Search" search)
                       "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))

(defun approve-eldoc-describe-check (context _details)
  "Describe the check of CONTEXT."
  (let* ((check (plist-get (plist-get context :thing) :check)))
    (let-alist check
      (let* ((name (approve-face (approve-sections-check-name check) 'bold))
             (result (approve-sections-check-state check))
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
               (approve-eldoc--row "Duration" (approve-sections-check-duration check))
               "\n" (approve-eldoc--link (plist-get context :url)) "\n"))))))

(defun approve-eldoc-describe-file (context _details)
  "Describe the changed file of CONTEXT."
  (let* ((path (plist-get context :path))
         (file (approve-pr-file path))
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
                                           (format "resolved by @%s" .resolvedBy.login)
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
      (let ((names (mapcar (lambda (node) (concat "@" (alist-get 'login node)))
                           .reactors.nodes)))
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
                   (format "%s %d — %s" (approve-render-reaction-icon .content)
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
    (git-actor . approve-eldoc-describe-git-actor)
    (ref . approve-eldoc-describe-ref)
    (check . approve-eldoc-describe-check)
    (file . approve-eldoc-describe-file)
    (thread . approve-eldoc-describe-thread)
    (reactions . approve-eldoc-describe-reactions)
    (date . approve-eldoc-describe-date)
    (milestone . approve-eldoc-describe-milestone)
    (branch . approve-eldoc-describe-branch)
    (pull-request . approve-eldoc-describe-pull-request)
    (search . approve-eldoc-describe-search))
  "Describers of things at point, by context kind.
Each is called with the context and the details fetched for it (or
nil) and returns a plist: :echo, the line for the echo area; :doc,
the text for the eldoc buffer; :details, (KIND . KEY) of the details
query that would add to them (see `approve-query-details'), or nil.")

;;; The eldoc function

(defvar-local approve-eldoc--wanted nil
  "The context whose details \\[approve-show-details] asked for.")

(defvar approve-eldoc--shown nil
  "The last doc Approve gave eldoc: (BUFFER CONTEXT DOC).
It is in the eldoc buffer until another doc replaces it.")

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
  "Fetch the details SPEC, then document point again in this buffer.
A query that fails at once is cached as failed, as one GitHub refused."
  (let* ((key (approve-eldoc--key spec))
         (buffer (current-buffer))
         (fail (lambda (err)
                 (puthash key (list :error (if (stringp (cadr err)) (cadr err)
                                             (format "%s" err))
                                    :time (float-time))
                          approve-eldoc--details))))
    (puthash key (list :loading t :buffer buffer) approve-eldoc--details)
    (condition-case-unless-debug err
        (approve-query-details
         (car spec) (cdr spec)
         :callback (lambda (data)
                     (puthash key (list :data data) approve-eldoc--details)
                     (approve-eldoc--refresh buffer))
         :errorback (lambda (err)
                      (funcall fail err)
                      (approve-eldoc--refresh buffer)))
      (error (approve-log-condition "Asking for details" err)
             (funcall fail err)))))

(defun approve-eldoc--refresh (buffer)
  "Document again in BUFFER, whose details arrived.
When BUFFER is selected and the eldoc buffer is in a window, eldoc
runs again where point is.  Else, as when the eldoc buffer took the
cursor or was closed, the doc BUFFER put there is described anew in
place, without showing it again, so that no \"loading details…\"
stays."
  (when (buffer-live-p buffer)
    (if (and (eq buffer (window-buffer (selected-window)))
             (approve-eldoc--buffer-visible-p))
        (with-current-buffer buffer
          (eldoc-print-current-symbol-info t))
      (pcase-let ((`(,shown ,context ,doc) approve-eldoc--shown))
        (when (eq shown buffer)
          (let ((new (plist-get (with-current-buffer buffer
                                  (approve-eldoc-describe context))
                                :doc)))
            (when (approve-eldoc--replace-doc doc new)
              (setq approve-eldoc--shown (list buffer context new)))))))))

(defun approve-eldoc--replace-doc (old new)
  "Put the doc NEW where the eldoc buffer shows the doc OLD.
Return non-nil if it showed OLD."
  (when (and (boundp 'eldoc--doc-buffer) (buffer-live-p eldoc--doc-buffer))
    (with-current-buffer eldoc--doc-buffer
      (save-excursion
        (goto-char (point-min))
        (when (let ((case-fold-search nil)) (search-forward old nil t))
          (let ((inhibit-read-only t))
            (delete-region (match-beginning 0) (match-end 0))
            (insert new))
          t)))))

(defun approve-eldoc-describe (context &optional fetch)
  "Return the description of CONTEXT: a plist with :echo and :doc.
Details already cached are used.  With FETCH, missing details are
fetched (and eldoc runs again when they arrive); the doc then says
they are loading.  Return nil when nothing describes CONTEXT."
  (when-let* ((describer (alist-get (plist-get context :kind) approve-eldoc-describers)))
    (let* ((first (funcall describer context nil))
           (spec (plist-get first :details))
           (entry (and spec (approve-eldoc--cached spec))))
      (when (and spec (null entry) fetch)
        (approve-eldoc--fetch spec)
        (setq entry (approve-eldoc--cached spec)))
      (let ((result (if (plist-get entry :data)
                        (funcall describer context (plist-get entry :data))
                      first))
            (note (cond
                   ((plist-get entry :loading)
                    (concat (approve-icon 'loading 'approve-loading) " "
                            (approve-face "loading details…" 'approve-loading)))
                   ((plist-get entry :error)
                    (approve-face (format "details unavailable (%s)" (plist-get entry :error))
                                  'approve-dim)))))
        (list :echo (plist-get result :echo)
              :doc (concat (plist-get result :doc) (and note (concat "\n" note "\n"))))))))

(defun approve-eldoc-function (callback &rest _)
  "Document the thing at point for eldoc, calling CALLBACK.
The echo line comes from the store, or in a buffer of lists from the
rows; details are fetched only when \\[approve-show-details] asked
for this thing or the eldoc buffer is showing.  Return nil when
there is nothing to say, as in a PR buffer before its PR loaded."
  (when (or (approve-model-root-ref) (not (approve-model-meta :number)))
    (when-let* ((context (approve-context-at-point))
                (description
                 (approve-eldoc-describe
                  context
                  (or (approve-eldoc--buffer-visible-p)
                      (equal approve-eldoc--wanted (approve-eldoc--identity context))))))
      (setq approve-eldoc--shown
            (list (current-buffer) context (plist-get description :doc)))
      (funcall callback (plist-get description :doc)
               :echo (plist-get description :echo))
      t)))

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
