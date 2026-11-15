;;; test-approve-manage.el --- Tests for the commands that manage a PR -*- lexical-binding: t; -*-

;;; Commentary:

;; The metadata commands of a PR buffer (flow F12), run as a user
;; would in the medium PR: the command, what its picker offers and has
;; chosen already, what goes to GitHub (a fake answers the mutations
;; and the repository's metadata as GitHub would) and what the buffer
;; shows afterwards.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-manage)
(require 'approve-test-helpers "helpers")

(defconst test-manage--pr "PR_kwDOPCDj7SnvM_RcFp"
  "The node id of the medium PR.")

(defvar test-manage--requests nil
  "The mutations sent to the fake GitHub, the last first.")

(defvar test-manage--answer nil
  "A function of a mutation request that returns GitHub's data.")

(defun test-manage--page (path nodes)
  "Return the data of one last page of NODES at PATH."
  (let ((value `((totalCount . ,(length nodes)) (pageInfo (hasNextPage) (endCursor))
                 (nodes ,@nodes))))
    (dolist (key (reverse path))
      (setq value (list (cons key value))))
    value))

(defun test-manage--user (login id)
  "Return the user LOGIN with ID, as GitHub answers it."
  `((__typename . "User") (id . ,id) (login . ,login)
    (name . ,(capitalize (car (split-string login "-"))))))

(defconst test-manage--metadata
  `(("queries/metadata/labels.graphql"
     . ,(test-manage--page
         '(repository labels)
         '(((__typename . "Label") (id . "LA_bug") (name . "bug") (color . "d73a4a")
            (description . "Something is not working"))
           ((__typename . "Label") (id . "LA_kwDOk_ZExIS6GDGl-p") (name . "enhancement")
            (color . "a2eeef") (description . "New feature or request"))
           ((__typename . "Label") (id . "LA_kwDObXGcJQS9m86GUU") (name . "area: parser")
            (color . "0e8a16") (description . "The parser and lexer")))))
    ("queries/metadata/assignable-users.graphql"
     . ,(test-manage--page
         '(repository assignableUsers)
         (list (test-manage--user "alice-example" "U_kgDOUisnajVr3zkBPf")
               (test-manage--user "bob-example" "U_kgDOSBgazSKz7a68ik")
               (test-manage--user "erin-example" "U_erin"))))
    ("queries/metadata/milestones.graphql"
     . ,(test-manage--page
         '(repository milestones)
         '(((__typename . "Milestone") (id . "MI_later") (title . "Later") (state . "OPEN"))
           ((__typename . "Milestone") (id . "MI_13") (title . "v1.3") (state . "OPEN")
            (dueOn . "2026-12-31T00:00:00Z") (progressPercentage . 0.0))
           ((__typename . "Milestone") (id . "MI_kwDO0Uq23adVIwI2Ai") (title . "v1.2")
            (state . "OPEN") (dueOn . "2026-11-30T00:00:00Z")
            (progressPercentage . 62.5)))))
    ("queries/metadata/owner.graphql"
     (repositoryOwner (__typename . "Organization") (id . "O_acme") (login . "acme-example")))
    ("queries/metadata/teams.graphql"
     . ,(test-manage--page
         '(organization teams)
         '(((__typename . "Team") (id . "T_kwDOBtAPjZBk04C44y") (name . "Core")
            (slug . "core") (combinedSlug . "acme-example/core"))
           ((__typename . "Team") (id . "T_docs") (name . "Docs") (slug . "docs")
            (combinedSlug . "acme-example/docs"))))))
  "The repository's metadata, by query file, as GitHub answers it.")

(defun test-manage--transport (replay)
  "Return a transport: mutations and metadata faked, the rest REPLAY's."
  (lambda (request)
    (let* ((name (or (plist-get request :name) ""))
           (data (cond ((string-prefix-p "mutations/" name)
                        (push request test-manage--requests)
                        (funcall test-manage--answer request))
                       ((assoc name test-manage--metadata)
                        (copy-tree (cdr (assoc name test-manage--metadata)))))))
      (if data
          (let ((timer (run-at-time 0 nil (plist-get request :on-success)
                                    `((data . ,data)) nil)))
            (lambda () (cancel-timer timer)))
        (funcall replay request)))))

(defmacro test-manage--with-pr (&rest body)
  "Run BODY in the medium PR's buffer, GitHub faked for what changes."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((approve-api-transport (test-manage--transport approve-api-transport))
            (test-manage--requests nil)
            (test-manage--answer nil)
            (pr (approve-test-open-pr "medium")))
       (approve-metadata-clear-cache)
       (unwind-protect
           (progn
             (switch-to-buffer pr)
             (delete-other-windows)
             (with-current-buffer pr
               ,@body))
         (test-manage--kill-inputs)
         (approve-metadata-clear-cache)
         (kill-buffer pr)))))

(defun test-manage--wait ()
  "Wait until no action waits for GitHub."
  (approve-test-wait-for (lambda () (null (approve-session-get :inflight)))))

(defun test-manage--sent ()
  "Return the query files and the sorted variables of the mutations sent, in order."
  (reverse (mapcar (lambda (request)
                     (list (plist-get request :name)
                           (sort (copy-sequence (plist-get request :variables))
                                 (lambda (a b) (string< (car a) (car b))))))
                   test-manage--requests)))

(defvar test-manage--asked nil
  "What the last picker was given: (:initial INITIAL :candidates C :notes N).")

(defun test-manage--pick (&rest chosen)
  "Make the next pickers answer CHOSEN, and remember what they offered."
  (let ((answer (lambda (_prompt table _predicate _require initial &rest more)
                  (setq test-manage--asked
                        (list :initial initial
                              :default (nth 1 more)
                              :candidates (all-completions "" table)
                              :notes (let ((annotate (alist-get 'annotation-function
                                                                (cdr (funcall table "" nil
                                                                              'metadata)))))
                                       (mapcar (lambda (candidate)
                                                 (cons candidate
                                                       (when-let* ((note (funcall annotate
                                                                                  candidate)))
                                                         (substring-no-properties
                                                          (string-trim note)))))
                                               (all-completions "" table)))))
                  chosen)))
    (spy-on 'completing-read-multiple :and-call-fake answer)
    (spy-on 'completing-read :and-call-fake
            (lambda (&rest args) (car (apply answer args))))))

(defun test-manage--row (row)
  "Return the text of the metadata ROW."
  (approve-test-section-text (approve-test-find-section 'approve-meta-section row)))

(defun test-manage--input ()
  "Return the open input buffer, or nil."
  (seq-find (lambda (buffer) (string-prefix-p "*approve-input" (buffer-name buffer)))
            (buffer-list)))

(defun test-manage--kill-inputs ()
  "Kill every input buffer, keeping no draft."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*approve-input" (buffer-name buffer))
      (with-current-buffer buffer
        (setq approve-input--done t))
      (kill-buffer buffer))))

(describe "The metadata commands"
  (before-each
    (spy-on 'message))

  (describe "t (approve-edit-title)"
    (it "edits the title on one line, and the header line shows it"
      (test-manage--with-pr
        (setq test-manage--answer
              (lambda (_request)
                `((updatePullRequest
                   (pullRequest (__typename . "PullRequest") (id . ,test-manage--pr)
                                (title . "Rewrite the parser"))))))
        (call-interactively #'approve-edit-title)
        (let ((input (test-manage--input)))
          (expect (with-current-buffer input (buffer-string))
                  :to-equal "Rewrite the parser and engine scheduler")
          (expect (buffer-local-value 'approve-input-single-line-mode input) :to-be-truthy)
          (with-current-buffer input
            (erase-buffer)
            (insert "Rewrite the parser")
            (approve-input-commit)))
        (test-manage--wait)
        (expect (test-manage--sent)
                :to-equal `(("mutations/update-pull-request.graphql"
                             ((pullRequestId . ,test-manage--pr)
                              (title . "Rewrite the parser")))))
        (expect approve-pr--header-line :to-match "#202 Rewrite the parser")
        (expect approve-pr--header-line :not :to-match "scheduler")))

    (it "sends nothing when the title is unchanged, and needs the right to edit"
      (test-manage--with-pr
        (call-interactively #'approve-edit-title)
        (with-current-buffer (test-manage--input)
          (approve-input-commit))
        (test-manage--wait)
        (expect test-manage--requests :to-be nil)
        (approve-model-patch `((__typename . "PullRequest") (id . ,test-manage--pr)
                               (viewerCanUpdate)))
        (expect (call-interactively #'approve-edit-title)
                :to-throw 'user-error '("You can't edit this pull request")))))

  (describe "d (approve-edit-description)"
    (it "empties the description when sent empty"
      (test-manage--with-pr
        (setq test-manage--answer
              (lambda (_request)
                `((updatePullRequest
                   (pullRequest (__typename . "PullRequest") (id . ,test-manage--pr)
                                (body . "") (bodyHTML . ""))))))
        (call-interactively #'approve-edit-description)
        (with-current-buffer (test-manage--input)
          (erase-buffer)
          (approve-input-commit))
        (test-manage--wait)
        (expect (test-manage--sent)
                :to-equal `(("mutations/update-pull-request.graphql"
                             ((body . "") (pullRequestId . ,test-manage--pr)))))
        (expect (approve-test-section-text
                 (approve-test-find-section 'approve-description-section))
                :to-match "No description provided"))))

  (describe "l (approve-set-labels)"
    (it "offers the repository's labels, the PR's chosen, and changes them"
      (test-manage--with-pr
        (setq test-manage--answer
              (lambda (request)
                (if (string-match-p "add-labels" (plist-get request :name))
                    `((addLabelsToLabelable
                       (labelable (__typename . "PullRequest") (id . ,test-manage--pr)
                                  (labels (nodes ,@(alist-get 'nodes (approve-pr-field 'labels))
                                                 ((__typename . "Label") (id . "LA_bug")
                                                  (name . "bug") (color . "d73a4a")))))))
                  `((removeLabelsFromLabelable
                     (labelable (__typename . "PullRequest") (id . ,test-manage--pr)
                                (labels (nodes ((__typename . "Label")
                                                (id . "LA_kwDOk_ZExIS6GDGl-p")
                                                (name . "enhancement") (color . "a2eeef"))
                                               ((__typename . "Label") (id . "LA_bug")
                                                (name . "bug") (color . "d73a4a"))))))))))
        (test-manage--pick "enhancement" "bug")
        (call-interactively #'approve-set-labels)
        (expect (plist-get test-manage--asked :initial) :to-equal "enhancement,area: parser,")
        (expect (plist-get test-manage--asked :candidates)
                :to-equal '("area: parser" "bug" "enhancement"))
        (expect (cdr (assoc "bug" (plist-get test-manage--asked :notes)))
                :to-equal "Something is not working")
        (expect (cdr (assoc "enhancement" (plist-get test-manage--asked :notes)))
                :to-equal "✓      New feature or request")
        (test-manage--wait)
        (expect (test-manage--sent)
                :to-equal `(("mutations/add-labels-to-labelable.graphql"
                             ((labelIds . ["LA_bug"]) (labelableId . ,test-manage--pr)))
                            ("mutations/remove-labels-from-labelable.graphql"
                             ((labelIds . ["LA_kwDObXGcJQS9m86GUU"])
                              (labelableId . ,test-manage--pr)))))
        (expect (test-manage--row 'labels) :to-match "enhancement +bug")
        (expect (test-manage--row 'labels) :not :to-match "parser")))

    (it "sends nothing when the labels stay the same"
      (test-manage--with-pr
        (test-manage--pick "area: parser" "enhancement")
        (call-interactively #'approve-set-labels)
        (expect test-manage--requests :to-be nil)
        (expect 'message :to-have-been-called-with "Approve: no change to %s" "the labels")))

    (it "refuses when the viewer may not label the pull request"
      (test-manage--with-pr
        (approve-model-patch `((__typename . "PullRequest") (id . ,test-manage--pr)
                               (viewerCanLabel)))
        (expect (call-interactively #'approve-set-labels)
                :to-throw 'user-error '("You can't change the labels of this pull request")))))

  (describe "a (approve-set-assignees)"
    (it "offers who can be assigned, the assignees chosen, and changes them"
      (test-manage--with-pr
        (setq test-manage--answer
              (lambda (request)
                (if (string-match-p "add-assignees" (plist-get request :name))
                    `((addAssigneesToAssignable
                       (assignable (__typename . "PullRequest") (id . ,test-manage--pr)
                                   (assignees (nodes ,(test-manage--user "alice-example"
                                                                         "U_kgDOUisnajVr3zkBPf")
                                                     ,(test-manage--user "erin-example"
                                                                         "U_erin"))))))
                  `((removeAssigneesFromAssignable
                     (assignable (__typename . "PullRequest") (id . ,test-manage--pr)
                                 (assignees (nodes ,(test-manage--user "erin-example"
                                                                       "U_erin")))))))))
        (test-manage--pick "@erin-example")
        (call-interactively #'approve-set-assignees)
        (expect (plist-get test-manage--asked :initial) :to-equal "@alice-example,")
        (expect (plist-get test-manage--asked :candidates)
                :to-equal '("@alice-example" "@bob-example" "@erin-example"))
        (test-manage--wait)
        (expect (mapcar #'car (test-manage--sent))
                :to-equal '("mutations/add-assignees-to-assignable.graphql"
                            "mutations/remove-assignees-from-assignable.graphql"))
        (expect (cadr (car (test-manage--sent)))
                :to-equal `((assignableId . ,test-manage--pr) (assigneeIds . ["U_erin"])))
        (expect (test-manage--row 'assignees) :to-match "@erin-example")
        (expect (test-manage--row 'assignees) :not :to-match "alice"))))

  (describe "A (approve-request-reviewers)"
    (it "offers GitHub's suggestions first, then who is asked, people and teams"
      (test-manage--with-pr
        (approve-model-patch
         `((__typename . "PullRequest") (id . ,test-manage--pr)
           (suggestedReviewers ((isAuthor) (isCommenter . t)
                                (reviewer ,@(test-manage--user "erin-example" "U_erin"))))))
        (setq test-manage--answer
              (lambda (_request)
                `((requestReviews
                   (pullRequest
                    (__typename . "PullRequest") (id . ,test-manage--pr)
                    (reviewRequests
                     (nodes ((__typename . "ReviewRequest") (id . "RR_erin") (asCodeOwner)
                             (requestedReviewer ,@(test-manage--user "erin-example"
                                                                     "U_erin")))
                            ((__typename . "ReviewRequest") (id . "RR_docs") (asCodeOwner)
                             (requestedReviewer (__typename . "Team") (id . "T_docs")
                                                (slug . "docs")
                                                (combinedSlug . "acme-example/docs"))))))))))
        (test-manage--pick "@erin-example" "@acme-example/docs")
        (call-interactively #'approve-request-reviewers)
        (expect (plist-get test-manage--asked :initial)
                :to-equal "@dave-example,@acme-example/core,")
        (expect (plist-get test-manage--asked :candidates)
                :to-equal '("@erin-example" "@dave-example" "@acme-example/core"
                            "@bob-example" "@acme-example/docs"))
        (expect (cdr (assoc "@erin-example" (plist-get test-manage--asked :notes)))
                :to-equal "Erin  suggested, commented")
        (expect (cdr (assoc "@bob-example" (plist-get test-manage--asked :notes)))
                :to-equal "Bob  approved")
        (test-manage--wait)
        (expect (test-manage--sent)
                :to-equal `(("mutations/request-reviews.graphql"
                             ((botIds . []) (pullRequestId . ,test-manage--pr)
                              (teamIds . ["T_docs"]) (userIds . ["U_erin"])))))
        (expect (test-manage--row 'reviewers) :to-match "@acme-example/docs")
        (expect (test-manage--row 'reviewers) :not :to-match "dave"))))

  (describe "M (approve-set-milestone)"
    (it "offers the open milestones, those due first first, and takes the milestone away"
      (test-manage--with-pr
        (setq test-manage--answer
              (lambda (_request)
                `((updatePullRequest
                   (pullRequest (__typename . "PullRequest") (id . ,test-manage--pr)
                                (milestone))))))
        (test-manage--pick "No milestone")
        (call-interactively #'approve-set-milestone)
        (expect (plist-get test-manage--asked :default) :to-equal "v1.2")
        (expect (plist-get test-manage--asked :candidates)
                :to-equal '("No milestone" "v1.2" "v1.3" "Later"))
        (expect (cdr (assoc "v1.2" (plist-get test-manage--asked :notes)))
                :to-equal "✓ due 2026-11-30  62% done")
        (test-manage--wait)
        (expect (test-manage--sent)
                :to-equal `(("mutations/update-pull-request.graphql"
                             ((milestoneId . :null) (pullRequestId . ,test-manage--pr)))))
        (expect (test-manage--row 'milestone) :to-match "None")))

    (it "sends nothing when the milestone stays the same"
      (test-manage--with-pr
        (test-manage--pick "v1.2")
        (call-interactively #'approve-set-milestone)
        (expect test-manage--requests :to-be nil)))))

(describe "The metadata keys"
  (it "bind the commands in the PR buffer"
    (dolist (binding '(("t" . approve-edit-title) ("d" . approve-edit-description)
                       ("l" . approve-set-labels) ("a" . approve-set-assignees)
                       ("A" . approve-request-reviewers) ("M" . approve-set-milestone)))
      (expect (keymap-lookup approve-pr-mode-map (car binding)) :to-be (cdr binding)))))

(provide 'test-approve-manage)

;;; test-approve-manage.el ends here
