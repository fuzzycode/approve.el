;;; scratch.el --- Try Approve's writes on the scratch repository -*- lexical-binding: t; -*-

;;; Commentary:

;; Not packaged, and not run by `make test': it writes to GitHub.
;;
;; It runs every mutation of approve-mutations.el on a pull request of
;; the scratch repository, fuzzycode/test-approve (docs/development.org,
;; "Scratch repository"), and after each one compares what the store
;; holds with the pull request loaded anew from GitHub.  So it checks
;; what the unit tests can only assume: GitHub's answers have the shape
;; the mutations read, and the store they leave is GitHub's truth
;; (decision D35).  After each one, change detection must also find
;; nothing new on GitHub: Approve's own writes never count (D23).  It
;; cleans up what it can; reviews it submitted stay (GitHub deletes
;; only pending ones).
;;
;;   timeout 900 cask exec emacs -Q --batch -L lisp -l dev/scratch.el \
;;     -f approve-scratch-run-batch
;;
;; Requests go through the gh CLI (`approve-scratch-gh-transport'),
;; which must be logged in to an account that can write to the
;; repository; ghub's token is not needed.  It prints PASS or FAIL per
;; check and exits with 1 when one failed.  Never point it at another
;; repository.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'approve-api)
(require 'approve-graphql)
(require 'approve-model)
(require 'approve-queries)
(require 'approve-mutations)
(require 'approve-changes)
(require 'approve-metadata)

(defconst approve-scratch-repository '("fuzzycode" . "test-approve")
  "The only repository this script writes to.")

(defconst approve-scratch-number 8
  "The pull request it works on: #8, the review API experiments.
Its one file, src/server/main.go, has two hunks; the targets below
are lines of them.")

(defconst approve-scratch-path "src/server/main.go"
  "The file of `approve-scratch-number' the comments go on.")

(defconst approve-scratch-targets
  '((line :path "src/server/main.go" :side "RIGHT" :line 9)
    (range :path "src/server/main.go" :side "RIGHT" :line 51
           :start-side "RIGHT" :start-line 50)
    (mixed :path "src/server/main.go" :side "RIGHT" :line 54
           :start-side "LEFT" :start-line 50)
    (outside :path "src/server/main.go" :side "RIGHT" :line 30)
    (file :path "src/server/main.go"))
  "Where comments go: an added line, two added lines, a range from a
removed line to an added one, a line between the hunks (which takes
no comment) and the whole file.")

(defconst approve-scratch-mark "approve scratch check"
  "Every text this script writes starts with it.")

;;; Requests through gh

(defun approve-scratch--resource (request)
  "Return the REST resource of REQUEST with its parameters as a query."
  (let ((params (plist-get request :params)))
    (if params
        (concat (plist-get request :resource) "?"
                (url-build-query-string
                 (mapcar (lambda (param) (list (car param) (cdr param))) params)))
      (plist-get request :resource))))

(defun approve-scratch-gh-transport (request)
  "Run REQUEST, an `approve-api-transport' request, through `gh api'.
GraphQL and JSON REST only; gh's output is the response body."
  (let* ((graphql (eq (plist-get request :kind) 'graphql))
         (payload (plist-get request :payload))
         (input (and payload (make-temp-file "approve-gh" nil ".json")))
         (out (generate-new-buffer " *approve-gh*"))
         (err (generate-new-buffer " *approve-gh-error*")))
    (when input
      (with-temp-file input
        (insert (json-serialize payload :null-object :null :false-object nil))))
    (let ((process
           (let ((process-environment
                  (append '("NO_COLOR=1" "GH_NO_UPDATE_NOTIFIER=1" "GH_PROMPT_DISABLED=1")
                          process-environment)))
             (make-process
              :name "approve-gh" :buffer out :stderr err :noquery t
              ;; A pty would make gh colour its output for a terminal.
              :connection-type 'pipe
              :command `("gh" "api"
                         ,@(if graphql
                               '("graphql")
                             (list "-X" (plist-get request :method)
                                   (approve-scratch--resource request)))
                         ,@(and input (list "--input" input)))
              :sentinel
              (lambda (process _event)
                (unless (process-live-p process)
                  (let ((body (condition-case nil
                                  (json-parse-string
                                   (with-current-buffer out (buffer-string))
                                   :object-type 'alist :array-type 'list
                                   :null-object nil :false-object nil)
                                (error :none)))
                        (error-text (with-current-buffer err (string-trim (buffer-string)))))
                    (when input (delete-file input))
                    (kill-buffer out)
                    (when (buffer-live-p err) (kill-buffer err))
                    (if (eq body :none)
                        (funcall (plist-get request :on-failure)
                                 (list :status nil
                                       :error (list 'error (format "gh api: %s" error-text))))
                      (funcall (plist-get request :on-success) body nil)))))))))
      (lambda () (delete-process process)))))

;;; Waiting and reporting

(defvar approve-scratch--failures 0
  "How many checks failed.")

(defun approve-scratch--wait (predicate what &optional seconds)
  "Wait until PREDICATE returns non-nil, at most SECONDS (default 60).
Return its value; signal an error naming WHAT on timeout."
  (let ((deadline (+ (float-time) (or seconds 60)))
        value)
    (while (not (setq value (funcall predicate)))
      (when (> (float-time) deadline)
        (error "Timed out waiting for %s" what))
      (accept-process-output nil 0.05))
    value))

(defun approve-scratch--check (what ok &optional detail)
  "Report the check WHAT: passed when OK.  DETAIL says more on failure."
  (unless ok
    (cl-incf approve-scratch--failures))
  (princ (format "%s %s%s\n" (if ok "PASS" "FAIL") what
                 (if (and detail (not ok)) (format "\n     %s" detail) ""))))

(defun approve-scratch--run (function &rest args)
  "Call the mutation FUNCTION with ARGS and wait; return how it ended.
That is (t . RESULT) or (nil . ERROR)."
  (let ((outcome nil))
    (apply function (append args
                            (list :callback (lambda (result) (setq outcome (cons t result)))
                                  :errorback (lambda (err) (setq outcome (cons nil err))))))
    (approve-scratch--wait (lambda () outcome) (symbol-name function))))

(defun approve-scratch--ok (what outcome)
  "Check that OUTCOME, from `approve-scratch--run', succeeded; return its result."
  (approve-scratch--check what (car outcome)
                          (and (not (car outcome)) (approve-api-error-message (cdr outcome))))
  (and (car outcome) (cdr outcome)))

;;; Loading

(defun approve-scratch--load (buffer)
  "Load the scratch pull request into the store of BUFFER, anew."
  (with-current-buffer buffer
    (approve-model-init (list :host "github.com"
                              :owner (car approve-scratch-repository)
                              :repo (cdr approve-scratch-repository)
                              :number approve-scratch-number))
    (let ((left 0)
          (failed nil))
      (dolist (key '(core files threads timeline))
        (cl-incf left)
        (approve-query-run key
                           :callback (lambda () (cl-decf left))
                           :errorback (lambda (err) (setq failed err) (cl-decf left))))
      (approve-scratch--wait (lambda () (zerop left)) "the pull request")
      (when failed
        (error "Loading the pull request failed: %s" (approve-api-error-message failed))))))

(defun approve-scratch--facts ()
  "Return what the current store says about the review state of the PR.
An alist of: the files' viewed states; the threads, each with whether
it is resolved and its comments with their states; the pending review
and its comments; the reviews and comments of the conversation, in
order; the review decision; the labels, assignees, milestone, review
requests, title and description; whether it is a draft, how the viewer
hears of it, whether its conversation is locked, the reactions to its
description, and which comments of the conversation are hidden and why."
  `((viewed . ,(mapcar (lambda (file)
                         (cons (alist-get 'path file) (alist-get 'viewerViewedState file)))
                       (approve-pr-files)))
    (threads . ,(mapcar (lambda (thread)
                          (list (alist-get 'id thread) (alist-get 'isResolved thread)
                                (mapcar (lambda (comment)
                                          (cons (alist-get 'id comment)
                                                (alist-get 'state comment)))
                                        (approve-model-nodes (alist-get 'comments thread)))))
                        (approve-pr-list-fields 'reviewThreads '(id isResolved comments))))
    (pending . ,(when-let* ((review (approve-pr-pending-review)))
                  (cons (alist-get 'id review)
                        (mapcar (lambda (comment) (alist-get 'id comment))
                                (approve-model-nodes (alist-get 'comments review))))))
    (conversation . ,(delq nil (mapcar (lambda (item)
                                         (and (member (alist-get '__typename item)
                                                      '("IssueComment" "PullRequestReview"))
                                              (not (equal (alist-get 'state item) "PENDING"))
                                              (alist-get 'id item)))
                                       (approve-pr-list-fields
                                        'timelineItems '(__typename id state)))))
    (decision . ,(approve-pr-field 'reviewDecision))
    (labels . ,(mapcar (lambda (label) (alist-get 'name label)) (approve-pr-field 'labels)))
    (assignees . ,(mapcar (lambda (user) (alist-get 'login user))
                          (approve-pr-field 'assignees)))
    (milestone . ,(alist-get 'title (approve-pr-field 'milestone)))
    (title . ,(approve-pr-field 'title))
    (body . ,(approve-pr-field 'body))
    (draft . ,(approve-pr-field 'isDraft))
    (subscription . ,(approve-pr-field 'viewerSubscription))
    (locked . ,(list (approve-pr-field 'locked) (approve-pr-field 'activeLockReason)))
    (hidden . ,(delq nil (mapcar (lambda (item)
                                   (and (alist-get 'isMinimized item)
                                        (list (alist-get 'id item)
                                              (alist-get 'minimizedReason item))))
                                 (approve-pr-list-fields
                                  'timelineItems '(id isMinimized minimizedReason)))))
    (reactions . ,(mapcar (lambda (group)
                            (let-alist group
                              (list .content .viewerHasReacted .reactors.totalCount)))
                          (approve-pr-field 'reactionGroups)))
    (requested . ,(mapcar (lambda (request)
                            (let-alist (alist-get 'requestedReviewer request)
                              (or .login .combinedSlug)))
                          (approve-pr-field 'reviewRequests)))))

(defun approve-scratch--compare (what store)
  "Check that STORE's facts are GitHub's, after WHAT.
STORE is the buffer the mutations ran in.  GitHub's are those of the
PR loaded anew; a difference is looked at again twice, a few seconds
apart, as GitHub may answer a query with what it had a moment ago."
  (let ((fresh (generate-new-buffer " *approve-scratch-fresh*"))
        (ours (with-current-buffer store (approve-scratch--facts)))
        (theirs nil)
        (tries 0))
    (unwind-protect
        (while (progn
                 (approve-scratch--load fresh)
                 (setq theirs (with-current-buffer fresh (approve-scratch--facts)))
                 (cl-incf tries)
                 (and (not (equal ours theirs)) (< tries 3)))
          (sleep-for 3))
      (kill-buffer fresh))
    (approve-scratch--check
     (format "%s: the store is GitHub's%s" what
             (if (> tries 1) (format " (after %d loads)" tries) ""))
     (equal ours theirs)
     (mapconcat (lambda (key)
                  (format "%s\n       store:  %S\n       GitHub: %S" key
                          (alist-get key ours) (alist-get key theirs)))
                (seq-remove (lambda (key) (equal (alist-get key ours) (alist-get key theirs)))
                            (mapcar #'car ours))
                "\n     ")))
  (approve-scratch--unchanged what store))

(defun approve-scratch--unchanged (what store)
  "Check that change detection finds nothing new on GitHub after WHAT.
The writes were made through Approve, so STORE holds them (decision
D35) and they must not count as changes (D23).  A difference is
looked at again twice, a few seconds apart, as for the store."
  (with-current-buffer store
    (let ((tries 0)
          answer changes)
      (while (progn
               (setq answer nil)
               (approve-query-changes
                :callback (lambda (remote) (setq answer (cons t remote)))
                :errorback (lambda (err) (setq answer (cons nil err))))
               (approve-scratch--wait (lambda () answer) "the change check")
               (setq changes (if (car answer)
                                 (approve-changes-compare (cdr answer))
                               (approve-api-error-message (cdr answer))))
               (cl-incf tries)
               (and changes (< tries 3)))
        (sleep-for 3))
      (approve-scratch--check
       (format "%s: no change on GitHub for change detection" what)
       (null changes) (format "%S" changes)))))

;;; The checks

(defun approve-scratch--target (name)
  "Return the target NAME of `approve-scratch-targets'."
  (cdr (assq name approve-scratch-targets)))

(defun approve-scratch--text (what)
  "Return a comment text about WHAT, marked as this script's."
  (format "%s: %s (%s)" approve-scratch-mark what (format-time-string "%F %T")))

(defun approve-scratch--ref-id (ref)
  "Return the id of REF, or nil."
  (and (approve-model-ref-p ref) (approve-model-ref-id ref)))

(defun approve-scratch--comment-of (thread-id)
  "Return the id of the first comment of THREAD-ID in the store."
  (approve-scratch--ref-id
   (car (approve-model-nodes
         (approve-model-raw-field (approve-model-make-ref "PullRequestReviewThread" thread-id)
                                  'comments)))))

(defun approve-scratch--pending-review ()
  "Return the id of the pending review in the store, or nil."
  (alist-get 'id (approve-pr-pending-review)))

(defun approve-scratch--review-steps (store)
  "Run the checks of the review in STORE, a buffer holding the scratch PR."
  (with-current-buffer store
    (when (approve-scratch--pending-review)
      (error "The viewer has a pending review on #%d; submit or discard it first"
             approve-scratch-number))
    (let* ((published (seq-find (lambda (thread) (not (alist-get 'isResolved thread)))
                                (approve-pr-list-fields 'reviewThreads '(id isResolved))))
           (published (alist-get 'id published))
           (cleanup nil))
      ;; F04: viewed, then not.
      (dolist (viewed '(t nil))
        (approve-scratch--ok (format "mark %s %s" approve-scratch-path
                                     (if viewed "viewed" "not viewed"))
                             (approve-scratch--run #'approve-mutation-set-file-viewed
                                                   approve-scratch-path viewed))
        (approve-scratch--compare (if viewed "viewed" "not viewed") store))
      ;; F05: a line outside the hunks takes no comment.
      (let ((outcome (approve-scratch--run #'approve-mutation-add-thread
                                           (approve-scratch--target 'outside)
                                           (approve-scratch--text "outside the hunks"))))
        (approve-scratch--check "a line outside the hunks is refused"
                                (eq (car-safe (cdr outcome)) 'approve-mutation-refused)
                                (format "%S" outcome)))
      ;; F05: threads on a line, a range, a mixed range and the file start
      ;; a pending review; a reply joins it.
      (let ((threads nil))
        (dolist (name '(line range mixed file))
          (when-let* ((ref (approve-scratch--ok
                            (format "comment on the %s" name)
                            (approve-scratch--run #'approve-mutation-add-thread
                                                  (approve-scratch--target name)
                                                  (approve-scratch--text name)))))
            (push (cons name (approve-model-ref-id ref)) threads)))
        (approve-scratch--check "the comments started a pending review"
                                (approve-scratch--pending-review))
        (approve-scratch--compare "four comments" store)
        (let ((reply (approve-scratch--ok
                      "reply to a published thread, pending"
                      (approve-scratch--run #'approve-mutation-add-reply published
                                            (approve-scratch--text "pending reply")))))
          (approve-scratch--compare "a pending reply" store)
          ;; F08: edit a pending comment; delete one, and the pending reply.
          (approve-scratch--ok "edit a pending comment"
                               (approve-scratch--run #'approve-mutation-update-review-comment
                                                     (approve-scratch--comment-of
                                                      (alist-get 'line threads))
                                                     (approve-scratch--text "line, edited")))
          (approve-scratch--ok "delete a pending comment"
                               (approve-scratch--run #'approve-mutation-delete-review-comment
                                                     (approve-scratch--comment-of
                                                      (alist-get 'mixed threads))))
          (approve-scratch--ok "delete the pending reply"
                               (approve-scratch--run #'approve-mutation-delete-review-comment
                                                     (approve-scratch--ref-id reply)))
          (approve-scratch--compare "an edit and two deletes" store))
        ;; A pending review made without a body can't get one (D32).
        (approve-scratch--check
         "editing the body of a pending review without one is refused"
         (not (car (approve-scratch--run #'approve-mutation-update-review
                                         (approve-scratch--pending-review)
                                         (approve-scratch--text "body")))))
        ;; F09: submit; then edit the review, resolve and unresolve.
        (let ((review (approve-scratch--ok
                       "submit the review as a comment"
                       (approve-scratch--run #'approve-mutation-submit-review "COMMENT"
                                             :body (approve-scratch--text "submitted")))))
          (approve-scratch--check "nothing is pending after the submit"
                                  (null (approve-scratch--pending-review)))
          (approve-scratch--compare "the submit" store)
          (approve-scratch--ok "edit the submitted review's body"
                               (approve-scratch--run #'approve-mutation-update-review
                                                     (approve-scratch--ref-id review)
                                                     (approve-scratch--text "submitted, edited"))))
        (let ((line (alist-get 'line threads)))
          (approve-scratch--ok "resolve a thread"
                               (approve-scratch--run #'approve-mutation-resolve-thread line))
          (approve-scratch--compare "resolve" store)
          (approve-scratch--ok "unresolve it"
                               (approve-scratch--run #'approve-mutation-unresolve-thread line))
          (approve-scratch--compare "unresolve" store))
        (dolist (name '(line range file))
          (push (approve-scratch--comment-of (alist-get name threads)) cleanup)))
      ;; F07: a reply with nothing pending is published at once.
      (when-let* ((reply (approve-scratch--ok
                          "reply with nothing pending"
                          (approve-scratch--run #'approve-mutation-add-reply published
                                                (approve-scratch--text "published reply")))))
        (push (approve-model-ref-id reply) cleanup)
        (approve-scratch--check "nothing is pending after a published reply"
                                (null (approve-scratch--pending-review)))
        (approve-scratch--compare "a published reply" store))
      ;; F05: a single comment, as a review with a thread.
      (when-let* ((review (approve-scratch--ok
                           "a single comment"
                           (approve-scratch--run
                            #'approve-mutation-add-review :event "COMMENT"
                            :threads (list (append (approve-scratch--target 'range)
                                                   (list :body (approve-scratch--text
                                                                "single comment"))))))))
        (let ((thread (seq-find (lambda (thread)
                                  (equal (approve-model-raw-field
                                          (approve-model-make-ref
                                           "PullRequestReviewComment"
                                           (approve-scratch--comment-of (alist-get 'id thread)))
                                          'pullRequestReview)
                                         review))
                                (approve-pr-list-fields 'reviewThreads '(id)))))
          (approve-scratch--check "the single comment's thread is in the store" thread)
          (when thread
            (push (approve-scratch--comment-of (alist-get 'id thread)) cleanup)))
        (approve-scratch--compare "a single comment" store))
      ;; F09: a pending review with threads and a reply, discarded.
      (approve-scratch--run #'approve-mutation-add-thread (approve-scratch--target 'line)
                            (approve-scratch--text "to discard"))
      (approve-scratch--run #'approve-mutation-add-reply published
                            (approve-scratch--text "reply to discard"))
      (approve-scratch--ok "discard the pending review"
                           (approve-scratch--run #'approve-mutation-delete-review
                                                 (approve-scratch--pending-review)))
      (approve-scratch--compare "the discard" store)
      ;; A pending review made on purpose, with a body, then discarded.
      (approve-scratch--ok "start an empty review with a body"
                           (approve-scratch--run #'approve-mutation-add-review
                                                 :body (approve-scratch--text "empty")))
      (approve-scratch--check "it is pending" (approve-scratch--pending-review))
      (approve-scratch--compare "an empty pending review" store)
      ;; Its only comment deleted, does it stay, having a body?
      (when-let* ((thread (approve-scratch--ok
                           "comment in the review with a body"
                           (approve-scratch--run #'approve-mutation-add-thread
                                                 (approve-scratch--target 'line)
                                                 (approve-scratch--text "in a review with a body")))))
        (approve-scratch--ok "delete that comment"
                             (approve-scratch--run #'approve-mutation-delete-review-comment
                                                   (approve-scratch--comment-of
                                                    (approve-model-ref-id thread))))
        (approve-scratch--check "the review with a body is still pending"
                                (approve-scratch--pending-review))
        (approve-scratch--compare "a review with a body, its comment deleted" store))
      (when-let* ((review (approve-scratch--pending-review)))
        (approve-scratch--ok "discard it"
                             (approve-scratch--run #'approve-mutation-delete-review review))
        (approve-scratch--compare "the second discard" store))
      ;; F10: a conversation comment, edited, then deleted.
      (when-let* ((comment (approve-scratch--ok
                            "a conversation comment"
                            (approve-scratch--run #'approve-mutation-add-comment
                                                  (approve-scratch--text "conversation")))))
        (approve-scratch--compare "a conversation comment" store)
        (approve-scratch--ok "edit it"
                             (approve-scratch--run #'approve-mutation-update-issue-comment
                                                   (approve-model-ref-id comment)
                                                   (approve-scratch--text "conversation, edited")))
        (approve-scratch--ok "delete it"
                             (approve-scratch--run #'approve-mutation-delete-issue-comment
                                                   (approve-model-ref-id comment)))
        (approve-scratch--compare "a deleted conversation comment" store))
      ;; Clean up: the published comments this run made.
      (dolist (comment (delq nil cleanup))
        (approve-scratch--ok "clean up a published comment"
                             (approve-scratch--run #'approve-mutation-delete-review-comment
                                                   comment)))
      (approve-scratch--compare "the clean-up" store))))

(defun approve-scratch--named (kind name)
  "Return the id of the metadata KIND whose name, title or login is NAME."
  (alist-get 'id (seq-find (lambda (entry)
                             (equal (or (alist-get 'name entry) (alist-get 'title entry))
                                    name))
                           (approve-metadata-wait kind))))

(defun approve-scratch--viewer-id ()
  "Return the id of the viewer, who can be assigned in the repository."
  (alist-get 'id (seq-find (lambda (user)
                             (equal (alist-get 'login user) (car approve-scratch-repository)))
                           (approve-metadata-wait 'assignable-users))))

(defun approve-scratch--metadata-steps (store)
  "Run the checks of the PR's metadata in STORE, a buffer holding the scratch PR.
Each change is undone after it: #8 keeps its title, description and
subscription, stays ready for review and unlocked, and has no labels,
assignees, milestone or review requests."
  (with-current-buffer store
    ;; F08: the title and the description, changed and put back.
    (let ((title (approve-pr-field 'title))
          (body (approve-pr-field 'body)))
      (approve-scratch--ok "edit the title"
                           (approve-scratch--run #'approve-mutation-set-title
                                                 (concat title " (edited)")))
      (approve-scratch--compare "a new title" store)
      (approve-scratch--ok "put the title back"
                           (approve-scratch--run #'approve-mutation-set-title title))
      (approve-scratch--ok "edit the description"
                           (approve-scratch--run #'approve-mutation-set-description
                                                 (concat body "\n\n"
                                                         (approve-scratch--text "description"))))
      (approve-scratch--compare "a new description" store)
      (approve-scratch--ok "put the description back"
                           (approve-scratch--run #'approve-mutation-set-description body))
      (approve-scratch--compare "the title and description back" store))
    (let ((bug (approve-scratch--named 'labels "bug"))
          (docs (approve-scratch--named 'labels "documentation"))
          (viewer (approve-scratch--viewer-id))
          (milestone (approve-scratch--named 'milestones "0.2.0")))
      ;; F12: labels, added then removed.
      (approve-scratch--ok "add two labels"
                           (approve-scratch--run #'approve-mutation-set-labels
                                                 (list bug docs) nil))
      (approve-scratch--compare "two labels" store)
      (approve-scratch--ok "remove one"
                           (approve-scratch--run #'approve-mutation-set-labels nil (list docs)))
      (approve-scratch--compare "one label" store)
      (approve-scratch--ok "remove the other"
                           (approve-scratch--run #'approve-mutation-set-labels nil (list bug)))
      (approve-scratch--compare "no label" store)
      ;; F12: assignees.
      (approve-scratch--ok "assign the viewer"
                           (approve-scratch--run #'approve-mutation-set-assignees
                                                 (list viewer) nil))
      (approve-scratch--compare "an assignee" store)
      (approve-scratch--ok "unassign the viewer"
                           (approve-scratch--run #'approve-mutation-set-assignees
                                                 nil (list viewer)))
      (approve-scratch--compare "no assignee" store)
      ;; F12: the milestone, set and taken away.
      (approve-scratch--ok "set the milestone"
                           (approve-scratch--run #'approve-mutation-set-milestone milestone))
      (approve-scratch--compare "a milestone" store)
      (approve-scratch--ok "take the milestone away"
                           (approve-scratch--run #'approve-mutation-set-milestone nil))
      (approve-scratch--compare "no milestone" store)
      ;; F12: review requests.  GitHub drops the author from them without
      ;; a word; asking no one is a whole set too.
      (approve-scratch--ok "ask the author for a review"
                           (approve-scratch--run #'approve-mutation-request-reviews
                                                 :users (list viewer)))
      (approve-scratch--check "the author is not asked: GitHub drops them"
                              (null (approve-pr-field 'reviewRequests)))
      (approve-scratch--compare "the author asked" store)
      (approve-scratch--ok "ask no one for a review"
                           (approve-scratch--run #'approve-mutation-request-reviews))
      (approve-scratch--compare "no review request" store))
    ;; F12: a draft, and ready for review again.
    (approve-scratch--ok "convert to a draft"
                         (approve-scratch--run #'approve-mutation-set-draft t))
    (approve-scratch--compare "a draft" store)
    (approve-scratch--ok "mark ready for review"
                         (approve-scratch--run #'approve-mutation-set-draft nil))
    (approve-scratch--compare "ready for review" store)
    ;; F12: the subscription, ignored and back.  It needs the token's
    ;; notifications scope, which gh's token may lack: then it is skipped.
    (let* ((subscription (approve-pr-field 'viewerSubscription))
           (outcome (approve-scratch--run #'approve-mutation-set-subscription "IGNORED")))
      (if (and (not (car outcome))
               (string-match-p "notifications" (approve-api-error-message (cdr outcome))))
          (princ "SKIP the subscription: the token lacks the notifications scope\n")
        (approve-scratch--ok "ignore the pull request" outcome)
        (approve-scratch--compare "ignored" store)
        (approve-scratch--ok "subscribe again"
                             (approve-scratch--run #'approve-mutation-set-subscription
                                                   subscription))
        (approve-scratch--compare "the subscription back" store)))
    ;; F11: a reaction to the description, given and taken back.
    (let ((pr (approve-model-ref-id (approve-model-root-ref))))
      (approve-scratch--ok "react to the description"
                           (approve-scratch--run #'approve-mutation-react pr "EYES"))
      (approve-scratch--compare "a reaction" store)
      (approve-scratch--ok "take the reaction back"
                           (approve-scratch--run #'approve-mutation-react pr "EYES" :remove t))
      (approve-scratch--compare "no reaction" store))
    ;; Hide a conversation comment, show it, then delete it.
    (when-let* ((comment (approve-scratch--ok
                          "a conversation comment to hide"
                          (approve-scratch--run #'approve-mutation-add-comment
                                                (approve-scratch--text "to hide")))))
      (let ((id (approve-model-ref-id comment)))
        (approve-scratch--ok "hide it as outdated"
                             (approve-scratch--run #'approve-mutation-set-hidden id t
                                                   :reason "OUTDATED"))
        (approve-scratch--compare "a hidden comment" store)
        (approve-scratch--ok "show it again"
                             (approve-scratch--run #'approve-mutation-set-hidden id nil))
        (approve-scratch--compare "the comment shown" store)
        (approve-scratch--ok "delete it"
                             (approve-scratch--run #'approve-mutation-delete-issue-comment id))))
    ;; Lock the conversation with a reason, then unlock it.
    (approve-scratch--ok "lock the conversation"
                         (approve-scratch--run #'approve-mutation-set-locked t
                                               :reason "RESOLVED"))
    (approve-scratch--compare "locked" store)
    (approve-scratch--ok "unlock it"
                         (approve-scratch--run #'approve-mutation-set-locked nil))
    (approve-scratch--compare "unlocked" store)))

(defconst approve-scratch-step-groups
  '((review . approve-scratch--review-steps)
    (metadata . approve-scratch--metadata-steps))
  "The groups of checks, in the order they run: (NAME . FUNCTION).")

(defun approve-scratch-run (&optional only)
  "Run the checks on the scratch pull request; return the failures.
ONLY, a list of names of `approve-scratch-step-groups', runs those
groups alone."
  (interactive)
  (let ((approve-api-transport #'approve-scratch-gh-transport)
        (approve-api-timeout 60)
        (approve-scratch--failures 0)
        (store (generate-new-buffer " *approve-scratch*")))
    (unwind-protect
        (progn
          (approve-scratch--load store)
          (pcase-dolist (`(,name . ,function) approve-scratch-step-groups)
            (when (or (null only) (memq name only))
              (funcall function store))))
      (kill-buffer store))
    (princ (format "%d checks failed\n" approve-scratch--failures))
    approve-scratch--failures))

(defun approve-scratch-run-batch ()
  "Run `approve-scratch-run' in batch mode; exit 1 when a check failed.
The environment variable APPROVE_SCRATCH_ONLY names the groups of
`approve-scratch-step-groups' to run, such as \"metadata\"; without
it, all run."
  (setq approve-graphql-directory
        (expand-file-name "../graphql" (file-name-directory
                                        (or load-file-name
                                            (locate-library "approve-mutations")))))
  (kill-emacs (if (zerop (condition-case err
                             (approve-scratch-run
                              (mapcar #'intern
                                      (split-string (or (getenv "APPROVE_SCRATCH_ONLY") ""))))
                           (error (princ (format "ERROR %s\n" (error-message-string err)))
                                  1)))
                  0 1)))

(provide 'approve-scratch)

;;; scratch.el ends here
