;;; approve-mutations.el --- Change a pull request on GitHub, keeping the store true -*- lexical-binding: t; -*-

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

;; The changes Approve makes on GitHub, one function per mutation in
;; graphql/mutations/ (docs/architecture.org, "API map"):
;;
;;   `approve-mutation-set-file-viewed'         F04  viewed, or not
;;   `approve-mutation-add-thread'              F05  comment on lines or a file
;;   `approve-mutation-add-reply'               F07  reply to a thread
;;   `approve-mutation-resolve-thread'          F07  resolve a thread
;;   `approve-mutation-unresolve-thread'        F07  unresolve it
;;   `approve-mutation-add-review'              F05, F09  a single comment, or a
;;                                                   verdict with nothing pending
;;   `approve-mutation-submit-review'           F09  submit the pending review
;;   `approve-mutation-delete-review'           F09  discard it
;;   `approve-mutation-update-review'           F08  edit a review's body
;;   `approve-mutation-update-review-comment'   F08  edit a comment in a thread
;;   `approve-mutation-delete-review-comment'   F08  delete it
;;   `approve-mutation-add-comment'             F10  comment in the conversation
;;   `approve-mutation-update-issue-comment'    F08  edit such a comment
;;   `approve-mutation-delete-issue-comment'    F08  delete it
;;   `approve-mutation-set-labels'              F12  add and remove labels
;;   `approve-mutation-set-assignees'           F12  assign and unassign people
;;   `approve-mutation-request-reviews'         F12  who is asked for a review
;;   `approve-mutation-set-milestone'           F12  the milestone, or none
;;   `approve-mutation-set-title'               F08  the title
;;   `approve-mutation-set-description'         F08  the description
;;   `approve-mutation-set-draft'               F12  a draft, or ready for review
;;   `approve-mutation-set-subscription'        F12  how the viewer hears of it
;;   `approve-mutation-set-locked'              lock or unlock the conversation
;;   `approve-mutation-react'                   F11  give or take back a reaction
;;   `approve-mutation-set-hidden'              hide (minimize) a comment, or show it
;;   `approve-mutation-commit-files'            F23  commit files to the PR's branch
;;                                                   (a suggestion applied)
;;   `approve-mutation-merge'                   F13  merge the PR
;;   `approve-mutation-set-auto-merge'          F13  merge it once it may, or no more
;;   `approve-mutation-set-queued'              F13  in the merge queue, or out of it
;;   `approve-mutation-update-branch'           F13  bring its branch up to date
;;   `approve-mutation-set-closed'              F13  close or reopen it
;;   `approve-mutation-delete-head-branch'      F13  delete its branch
;;
;; Each one queues its mutation (`approve-api-mutate': one at a time,
;; decision D15) for the PR of the current buffer's store.  Once GitHub
;; has made the change, it keeps the store true (decision D35):
;;
;; - It patches the store with GitHub's answer, the changed object
;;   through the fragments the queries use (D08).  The mutations that
;;   change the PR's review state (a thread, a reply, a review added or
;;   submitted) select that state too (PullRequestReviewStateFields), so
;;   the pending review, the verdicts and the review requests are
;;   GitHub's word, not Approve's guess.
;; - It makes the list edits that follow: a new thread joins the PR's
;;   threads, a reply its thread, a submitted review and a comment the
;;   conversation (not a review that holds a reply alone: GitHub never
;;   lists those).  A deletion is list edits alone, since GitHub may
;;   still list what was deleted right after (D32): a deleted comment
;;   leaves its thread and its review's count, a thread left empty
;;   leaves the PR, and a review left with neither comments nor a body
;;   goes too, as GitHub deletes it.  The events GitHub adds to the
;;   conversation for a change of the PR's metadata ("added the bug
;;   label") come with the next load: no answer holds them.
;; - The mutations of the merge menu answer with the PR's merge state
;;   (PullRequestMergeStateFields, decision D42).  A merge or an update
;;   of the branch changes more (the commits, the diff, the
;;   conversation), which only a load brings.
;;
;; Then it calls CALLBACK with the ref of the entity it changed.
;; ERRORBACK gets an approve-api error: GitHub's, or
;; `approve-mutation-refused' when GitHub answered without an error but
;; made no change (a comment on a line outside the hunks).  The
;; commands give theirs (approve-actions.el), and redraw.
;;
;; Layer rules: this is the operations layer.  It calls approve-api and
;; writes the store; it never touches the text of a buffer and knows
;; nothing of commands.  Callbacks run in the buffer that made the
;; request (approve-api), so the store they write is that buffer's.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'approve-api)
(require 'approve-api-queue)
(require 'approve-log)
(require 'approve-model)
(require 'approve-queries)

(define-error 'approve-mutation-refused "GitHub did not make the change"
  'approve-api-error)

;;; Sending

(defun approve-mutation--pull-request-id ()
  "Return the node id of the PR of the current store.
Signal an error when it has not loaded."
  (if-let* ((root (approve-model-root-ref)))
      (approve-model-ref-id root)
    (error "The pull request has not loaded")))

(defun approve-mutation--variables (variables)
  "Return the alist VARIABLES without the ones whose value is nil.
GraphQL takes a missing optional variable as null, where approve-api
would send nil as false."
  (seq-remove (lambda (variable) (null (cdr variable))) variables))

(defun approve-mutation--apply (apply data)
  "Return (t . RESULT) of calling APPLY with DATA, or (nil . ERROR).
An approve-api error APPLY signals is the error.  Any other is a bug:
it is logged, and the error says where to look."
  (condition-case err
      (cons t (funcall apply data))
    (approve-api-error (cons nil err))
    (error
     (approve-log-condition "Applying the answer to a mutation" err)
     (cons nil (list 'approve-api-error
                     (format "%s (see %s)" (error-message-string err)
                             approve-log-buffer-name))))))

(cl-defun approve-mutation--send (query variables &key progress apply callback errorback)
  "Queue the mutation QUERY with VARIABLES, then APPLY GitHub's answer.
VARIABLES whose value is nil are left out.  APPLY is called with the
answer's data in the buffer that made the request: it makes the
store true and returns the result for CALLBACK.  An error it signals
goes to ERRORBACK instead, as GitHub's errors do; without ERRORBACK,
`approve-api-default-errorback' shows it.  PROGRESS is the
echo-area text while the mutation runs."
  (let ((errorback (or errorback #'approve-api-default-errorback)))
    (approve-api-mutate
     query (approve-mutation--variables variables)
     :host (approve-query-host)
     :progress progress
     :errorback errorback
     :callback (lambda (data)
                 (pcase-let ((`(,ok . ,result) (approve-mutation--apply apply data)))
                   (cond ((not ok) (funcall errorback result))
                         (callback (funcall callback result))))))))

(defun approve-mutation--answer (data mutation field &optional refusal)
  "Return FIELD of the answer to MUTATION in DATA, the data of a response.
When it is null, GitHub made no change: signal
`approve-mutation-refused' with REFUSAL, a message saying why."
  (or (alist-get field (alist-get mutation data))
      (signal 'approve-mutation-refused
              (list (or refusal (format "GitHub's answer has no %s" field))))))

;;; The store

(defun approve-mutation--without (object &rest fields)
  "Return OBJECT, an alist, without FIELDS."
  (seq-remove (lambda (field) (memq (car field) fields)) object))

(defun approve-mutation--patch (object)
  "Patch the store with OBJECT, an entity in an answer; return its ref.
The pull request in its `pullRequest' field, selected for the PR's
review state, is patched on its own and left out of the entity: an
entity holding the PR would read the whole PR whenever it is read.
The PR's `addedThreads' (see `approve-mutation-add-review') are left
out too: they are no list of the PR."
  (when-let* ((pull-request (alist-get 'pullRequest object)))
    (approve-model-patch (approve-mutation--without pull-request 'addedThreads)))
  (approve-model-patch (approve-mutation--without object 'pullRequest)))

(defun approve-mutation--stored-p (ref)
  "Return non-nil if the entity REF points to is in the store."
  (approve-model-has-entity-p (approve-model-ref-type ref) (approve-model-ref-id ref)))

(defun approve-mutation--nodes (ref field)
  "Return the nodes in FIELD of the entity REF points to, as stored."
  (approve-model-nodes (approve-model-raw-field ref field)))

(defun approve-mutation--remove (owner field ref)
  "Remove REF from the list in FIELD of OWNER, a ref, if it is there.
Return non-nil if it was."
  (when (member ref (approve-mutation--nodes owner field))
    (approve-model-remove-ref owner field ref)
    t))

(defun approve-mutation--empty-p (ref field)
  "Return non-nil if the list in FIELD of REF has no nodes, loaded or not."
  (let ((value (approve-model-raw-field ref field)))
    (and (null (approve-model-nodes value))
         (not (approve-model-truncated-p value)))))

(defun approve-mutation--publish (review)
  "Add REVIEW, a ref, to the PR's conversation, unless it is pending.
A pending review joins the conversation once submitted; one that is
there already stays where it is."
  (when (and (approve-model-ref-p review)
             (not (equal (approve-model-raw-field review 'state) "PENDING")))
    (approve-model-append-ref (approve-model-root-ref) 'timelineItems review)))

(defun approve-mutation--drop-comments (thread comments)
  "Remove COMMENTS, refs, from THREAD; drop THREAD when it is left empty."
  (dolist (comment comments)
    (approve-mutation--remove thread 'comments comment))
  (when (approve-mutation--empty-p thread 'comments)
    (approve-mutation--remove (approve-model-root-ref) 'reviewThreads thread)))

(defun approve-mutation--forget-review (review)
  "Remove REVIEW, a ref, and its comments from the PR's lists.
Its comments leave their threads, the threads they leave empty leave
the PR, and the review leaves the pending reviews and the
conversation."
  (let ((root (approve-model-root-ref)))
    (dolist (thread (approve-mutation--nodes root 'reviewThreads))
      (when-let* ((comments
                   (seq-filter (lambda (comment)
                                 (equal (approve-model-raw-field comment 'pullRequestReview)
                                        review))
                               (approve-mutation--nodes thread 'comments))))
        (approve-mutation--drop-comments thread comments)))
    (approve-mutation--remove root 'pendingReviews review)
    (approve-mutation--remove root 'timelineItems review)))

(defun approve-mutation--take-from-review (review comment)
  "Take COMMENT, a ref, from the comments of REVIEW, a ref, and its count.
Return how many comments REVIEW has left, or nil when the store
doesn't know."
  (when (approve-mutation--stored-p review)
    (approve-mutation--remove review 'comments comment)
    (if-let* ((count (alist-get 'totalCount (approve-model-raw-field review 'commentCount))))
        (let ((left (max 0 (1- count))))
          (approve-model-patch `((__typename . ,(approve-model-ref-type review))
                                 (id . ,(approve-model-ref-id review))
                                 (commentCount (totalCount . ,left))))
          left)
      (and (approve-model-raw-field review 'comments)
           (length (approve-mutation--nodes review 'comments))))))

(defun approve-mutation--forget-review-comment (comment)
  "Remove COMMENT, a ref to a review comment, from the PR's lists.
It leaves its threads, and a thread left empty leaves the PR.  It
leaves its review's comments and count too, and a review left with
neither comments nor a body is forgotten, pending or submitted:
GitHub deletes it (decision D35)."
  (let ((review (approve-model-raw-field comment 'pullRequestReview)))
    (dolist (thread (seq-filter (lambda (thread)
                                  (member comment (approve-mutation--nodes thread 'comments)))
                                (approve-mutation--nodes (approve-model-root-ref)
                                                         'reviewThreads)))
      (approve-mutation--drop-comments thread (list comment)))
    (when (and (approve-model-ref-p review)
               (eql (approve-mutation--take-from-review review comment) 0)
               (string-blank-p (or (approve-model-raw-field review 'body) "")))
      (approve-mutation--forget-review review))))

;;; Files

(cl-defun approve-mutation-set-file-viewed (path viewed &key callback errorback)
  "Mark the changed file PATH viewed, or not viewed when VIEWED is nil.
Once GitHub has, the file's `viewerViewedState' in the store is
\"VIEWED\" or \"UNVIEWED\", and CALLBACK gets that state.  ERRORBACK
gets the error otherwise.  (Flow F04.)

Example:
  (approve-mutation-set-file-viewed \"lisp/a.el\" t :callback #\\='ignore)"
  (let ((state (if viewed "VIEWED" "UNVIEWED")))
    (approve-mutation--send
     (if viewed
         "mutations/mark-file-as-viewed.graphql"
       "mutations/unmark-file-as-viewed.graphql")
     `((pullRequestId . ,(approve-mutation--pull-request-id)) (path . ,path))
     :progress (format "Marking %s %s" path (if viewed "viewed" "not viewed"))
     :apply (lambda (_data)
              (approve-model-edit-list
               (approve-model-root-ref) 'files
               (lambda (files)
                 (mapcar (lambda (file)
                           (if (equal (alist-get 'path file) path)
                               (cons (cons 'viewerViewedState state)
                                     (approve-mutation--without file 'viewerViewedState))
                             file))
                         files)))
              state)
     :callback callback :errorback errorback)))

;;; Threads

(defun approve-mutation--target (target)
  "Return the GraphQL fields of TARGET, the lines a comment is on.
TARGET is a plist (:path P :line N :side S :start-line N :start-side
S), as a `line' or `lines' context is.  Without :start-line, or with
the start the same as the end, it is one line.  Without :line, it is
the whole file at :path (which only a thread can be on)."
  (let* ((line (plist-get target :line))
         (side (plist-get target :side))
         (start-line (plist-get target :start-line))
         (start-side (or (plist-get target :start-side) side)))
    (approve-mutation--variables
     `((path . ,(plist-get target :path))
       (line . ,line)
       (side . ,side)
       ,@(when (and start-line
                    (not (and (eql start-line line) (equal start-side side))))
           `((startLine . ,start-line) (startSide . ,start-side)))))))

(cl-defun approve-mutation-add-thread (target body &key callback errorback)
  "Comment BODY on TARGET: a line, a range of lines or a whole file.
TARGET is a plist (:path P :line N :side S :start-line N :start-side
S); a `line', `lines' or `file' context will do.  Without :line, the
comment is on the whole file.  The thread joins the viewer's pending
review, or starts one (decision D32).

Once GitHub has it, the thread is the last of the PR's threads, the
PR's review state is GitHub's (with the pending review), and
CALLBACK gets the thread's ref.  ERRORBACK gets the error otherwise:
`approve-mutation-refused' when the line takes no comment (it is
outside the hunks, and GitHub answers with no thread).  (Flows F05,
F06.)

Example:
  (approve-mutation-add-thread
   \\='(:path \"a.go\" :side \"RIGHT\" :line 14 :start-side \"RIGHT\" :start-line 13)
   \"Can this fail?\" :callback #\\='ignore)"
  (approve-mutation--send
   "mutations/add-pull-request-review-thread.graphql"
   `((pullRequestId . ,(approve-mutation--pull-request-id))
     (body . ,body)
     (subjectType . ,(if (plist-get target :line) "LINE" "FILE"))
     ,@(approve-mutation--target target))
   :progress "Adding the comment"
   :apply (lambda (data)
            (let ((thread (approve-mutation--patch
                           (approve-mutation--answer data 'addPullRequestReviewThread 'thread
                                                     "that line takes no comment"))))
              (approve-model-append-ref (approve-model-root-ref) 'reviewThreads thread)
              thread))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-add-reply (thread-id body &key callback errorback)
  "Reply BODY to the review thread THREAD-ID.
The reply joins the viewer's pending review, or is published at
once, as a review of its own, when none is pending (decision D32).
GitHub's conversation never lists such a review, which holds only
the reply (decision D35), so it stays out of the store's too.

Once GitHub has it, the reply is the last comment of its thread, the
PR's review state is GitHub's, and CALLBACK gets the reply's ref.
ERRORBACK gets the error otherwise.  (Flow F07.)"
  (approve-mutation--send
   "mutations/add-pull-request-review-thread-reply.graphql"
   `((threadId . ,thread-id) (body . ,body))
   :progress "Sending the reply"
   :apply (lambda (data)
            (let ((comment (approve-mutation--patch
                            (approve-mutation--answer data 'addPullRequestReviewThreadReply
                                                      'comment)))
                  (thread (approve-model-make-ref "PullRequestReviewThread" thread-id)))
              (when (approve-mutation--stored-p thread)
                (approve-model-append-ref thread 'comments comment))
              comment))
   :callback callback :errorback errorback))

(defun approve-mutation--thread-state (query mutation thread-id progress callback errorback)
  "Run QUERY, the MUTATION that resolves or unresolves THREAD-ID.
PROGRESS, CALLBACK and ERRORBACK are as for `approve-mutation--send';
CALLBACK gets the thread's ref."
  (approve-mutation--send
   query `((threadId . ,thread-id))
   :progress progress
   :apply (lambda (data)
            (approve-mutation--patch (approve-mutation--answer data mutation 'thread)))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-resolve-thread (thread-id &key callback errorback)
  "Resolve the review thread THREAD-ID.
Once GitHub has, the thread in the store is resolved and CALLBACK
gets its ref.  ERRORBACK gets the error otherwise.  (Flow F07.)"
  (approve-mutation--thread-state "mutations/resolve-review-thread.graphql"
                                  'resolveReviewThread thread-id "Resolving the thread"
                                  callback errorback))

(cl-defun approve-mutation-unresolve-thread (thread-id &key callback errorback)
  "Unresolve the review thread THREAD-ID.
Once GitHub has, the thread in the store is unresolved and CALLBACK
gets its ref.  ERRORBACK gets the error otherwise.  (Flow F07.)"
  (approve-mutation--thread-state "mutations/unresolve-review-thread.graphql"
                                  'unresolveReviewThread thread-id "Unresolving the thread"
                                  callback errorback))

;;; Reviews

(defun approve-mutation--draft-thread (thread)
  "Return THREAD, a target plist with :body, as a DraftPullRequestReviewThread."
  (unless (plist-get thread :line)
    (error "A comment in a new review must be on lines: %S" thread))
  `((body . ,(plist-get thread :body)) ,@(approve-mutation--target thread)))

(defun approve-mutation--thread-in-review-p (thread review)
  "Return non-nil if THREAD, from an answer, was started in REVIEW, a ref."
  (let-alist thread
    (equal (alist-get 'id (alist-get 'pullRequestReview (car .comments.nodes)))
           (approve-model-ref-id review))))

(cl-defun approve-mutation-add-review (&key event body threads callback errorback)
  "Add a review of the PR: a verdict, a body, comments on lines.
EVENT is \"APPROVE\", \"REQUEST_CHANGES\" or \"COMMENT\", which
publishes the review, or nil, which leaves it pending.  BODY is its
text, or nil.  THREADS are comments on lines, each a target plist
\(see `approve-mutation-add-thread') with the comment's :body; a
comment on a whole file can't be one.  GitHub refuses a review while
the viewer has a pending one (decision D32).

Once GitHub has it, the review and its threads are in the store, the
threads the last of the PR's, a published review the last of the
conversation, the PR's review state is GitHub's, and CALLBACK gets
the review's ref.  ERRORBACK gets the error otherwise.  (Flows F05,
a single comment, and F09.)

Example, a single comment:
  (approve-mutation-add-review
   :event \"COMMENT\"
   :threads \\='((:path \"a.go\" :side \"RIGHT\" :line 14 :body \"Why?\")))"
  (approve-mutation--send
   "mutations/add-pull-request-review.graphql"
   `((pullRequestId . ,(approve-mutation--pull-request-id))
     (event . ,event)
     (body . ,body)
     (threads . ,(and threads (vconcat (mapcar #'approve-mutation--draft-thread threads))))
     (threadCount . ,(length threads)))
   :progress (if event "Sending the review" "Starting a review")
   :apply (lambda (data)
            (let* ((answer (approve-mutation--answer data 'addPullRequestReview
                                                     'pullRequestReview))
                   (added (let-alist answer .pullRequest.addedThreads.nodes))
                   (review (approve-mutation--patch answer)))
              (dolist (thread added)
                (when (approve-mutation--thread-in-review-p thread review)
                  (approve-model-append-ref (approve-model-root-ref) 'reviewThreads
                                            (approve-model-patch thread))))
              (approve-mutation--publish review)
              review))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-submit-review (event &key body callback errorback)
  "Submit the viewer's pending review with EVENT.
EVENT is \"APPROVE\", \"REQUEST_CHANGES\" or \"COMMENT\".  BODY, when
non-nil, is the review's text; without it, a body the review was
made with stays.  GitHub finds the review from the PR (decision
D32).

Once GitHub has it, the review and its comments are no longer
pending, the review is the last of the conversation (unless it was
there), the PR's review state is GitHub's (with no pending review),
and CALLBACK gets the review's ref.  ERRORBACK gets the error
otherwise.  (Flow F09.)"
  (approve-mutation--send
   "mutations/submit-pull-request-review.graphql"
   `((pullRequestId . ,(approve-mutation--pull-request-id))
     (event . ,event)
     (body . ,body))
   :progress "Submitting the review"
   :apply (lambda (data)
            (let ((review (approve-mutation--patch
                           (approve-mutation--answer data 'submitPullRequestReview
                                                     'pullRequestReview))))
              (approve-mutation--publish review)
              review))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-delete-review (review-id &key callback errorback)
  "Discard the pending review REVIEW-ID, with its threads and comments.
Once GitHub has, the review has left the PR's pending reviews, its
comments their threads, and the threads they left empty the PR;
CALLBACK gets the review's ref.  These are list edits, as a query
right after may still list them (decision D32).  ERRORBACK gets the
error otherwise.  (Flow F09.)"
  (approve-mutation--send
   "mutations/delete-pull-request-review.graphql"
   `((reviewId . ,review-id))
   :progress "Discarding the review"
   :apply (lambda (_data)
            (let ((review (approve-model-make-ref "PullRequestReview" review-id)))
              (approve-mutation--forget-review review)
              review))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-update-review (review-id body &key callback errorback)
  "Make BODY the text of the review REVIEW-ID.
GitHub refuses it for a pending review made without a body (decision
D32).  Once GitHub has it, the store holds the new text and CALLBACK
gets the review's ref.  ERRORBACK gets the error otherwise.  (Flow
F08.)"
  (approve-mutation--send
   "mutations/update-pull-request-review.graphql"
   `((reviewId . ,review-id) (body . ,body))
   :progress "Saving the review"
   :apply (lambda (data)
            (approve-mutation--patch
             (approve-mutation--answer data 'updatePullRequestReview 'pullRequestReview)))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-update-review-comment (comment-id body &key callback errorback)
  "Make BODY the text of the review comment COMMENT-ID.
Once GitHub has it, the store holds the new text and CALLBACK gets the
comment's ref.  ERRORBACK gets the error otherwise.  (Flow F08.)"
  (approve-mutation--send
   "mutations/update-pull-request-review-comment.graphql"
   `((commentId . ,comment-id) (body . ,body))
   :progress "Saving the comment"
   :apply (lambda (data)
            (approve-mutation--patch
             (approve-mutation--answer data 'updatePullRequestReviewComment
                                       'pullRequestReviewComment)))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-delete-review-comment (comment-id &key callback errorback)
  "Delete the review comment COMMENT-ID.
Once GitHub has, the comment has left its thread and its review's
count, a thread it left empty the PR, and a review it left with
neither comments nor a body the PR too, as GitHub deletes it,
pending or submitted (decisions D32, D35).  CALLBACK gets the
comment's ref.  ERRORBACK gets the error otherwise.  (Flow F08.)"
  (approve-mutation--send
   "mutations/delete-pull-request-review-comment.graphql"
   `((commentId . ,comment-id))
   :progress "Deleting the comment"
   :apply (lambda (_data)
            (let ((comment (approve-model-make-ref "PullRequestReviewComment" comment-id)))
              (approve-mutation--forget-review-comment comment)
              comment))
   :callback callback :errorback errorback))

;;; The conversation

(cl-defun approve-mutation-add-comment (body &key callback errorback)
  "Comment BODY in the conversation of the PR.
Once GitHub has it, the comment is the last of the conversation and
CALLBACK gets its ref.  ERRORBACK gets the error otherwise.  (Flow
F10.)"
  (approve-mutation--send
   "mutations/add-comment.graphql"
   `((subjectId . ,(approve-mutation--pull-request-id)) (body . ,body))
   :progress "Sending the comment"
   :apply (lambda (data)
            (let ((comment (approve-mutation--patch
                            (alist-get 'node (approve-mutation--answer data 'addComment
                                                                       'commentEdge)))))
              (approve-model-append-ref (approve-model-root-ref) 'timelineItems comment)
              comment))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-update-issue-comment (comment-id body &key callback errorback)
  "Make BODY the text of the conversation comment COMMENT-ID.
Once GitHub has it, the store holds the new text and CALLBACK gets the
comment's ref.  ERRORBACK gets the error otherwise.  (Flow F08.)"
  (approve-mutation--send
   "mutations/update-issue-comment.graphql"
   `((commentId . ,comment-id) (body . ,body))
   :progress "Saving the comment"
   :apply (lambda (data)
            (approve-mutation--patch
             (approve-mutation--answer data 'updateIssueComment 'issueComment)))
   :callback callback :errorback errorback))

(cl-defun approve-mutation-delete-issue-comment (comment-id &key callback errorback)
  "Delete the conversation comment COMMENT-ID.
Once GitHub has, the comment has left the conversation and CALLBACK
gets its ref.  ERRORBACK gets the error otherwise.  (Flow F08.)"
  (approve-mutation--send
   "mutations/delete-issue-comment.graphql"
   `((commentId . ,comment-id))
   :progress "Deleting the comment"
   :apply (lambda (_data)
            (let ((comment (approve-model-make-ref "IssueComment" comment-id)))
              (approve-mutation--remove (approve-model-root-ref) 'timelineItems comment)
              comment))
   :callback callback :errorback errorback))

;;; The pull request's metadata

(cl-defun approve-mutation--in-turn (steps &key callback errorback)
  "Run STEPS one after the other, each once GitHub confirmed the one before.
Each step is a function of (CALLBACK ERRORBACK) that sends one
mutation.  CALLBACK gets the result of the last step, or the PR's
ref when there are no STEPS.  ERRORBACK gets the error of the first
step that fails, and the steps after it are not sent; the ones
before stay done."
  (let ((errorback (or errorback #'approve-api-default-errorback)))
    (if (null steps)
        (when callback
          (funcall callback (approve-model-root-ref)))
      (funcall (car steps)
               (lambda (result)
                 (if (cdr steps)
                     (approve-mutation--in-turn (cdr steps)
                                                :callback callback :errorback errorback)
                   (when callback
                     (funcall callback result))))
               errorback))))

(defun approve-mutation--step (query variables mutation payload progress)
  "Return a step of `approve-mutation--in-turn' that sends QUERY with VARIABLES.
GitHub's answer to MUTATION holds what it changed, the PR or a
comment, in its field PAYLOAD; the step patches the store with it
and gives its ref.  PROGRESS is the echo-area text while it runs."
  (lambda (callback errorback)
    (approve-mutation--send
     query variables
     :progress progress
     :apply (lambda (data)
              (approve-mutation--patch (approve-mutation--answer data mutation payload)))
     :callback callback :errorback errorback)))

(cl-defun approve-mutation--add-and-remove (add remove &key add-query remove-query
                                                variables mutations payload what
                                                callback errorback)
  "Add the ids ADD to a list of the PR, then remove the ids REMOVE.
ADD-QUERY and REMOVE-QUERY are the mutations, MUTATIONS their names
in GitHub's answer, (ADDING . REMOVING), and PAYLOAD the field there
that holds the PR.  VARIABLES is a function of the PR's id and a
vector of ids that returns the variables.  WHAT names the list for
the echo area, such as \"labels\".  An empty ADD or REMOVE sends
nothing.  CALLBACK gets the PR's ref once GitHub confirmed both."
  (let ((pr (approve-mutation--pull-request-id)))
    (approve-mutation--in-turn
     (delq nil
           (list (and add (approve-mutation--step
                           add-query (funcall variables pr (vconcat add))
                           (car mutations) payload (format "Adding %s" what)))
                 (and remove (approve-mutation--step
                              remove-query (funcall variables pr (vconcat remove))
                              (cdr mutations) payload (format "Removing %s" what)))))
     :callback callback :errorback errorback)))

(cl-defun approve-mutation-set-labels (add remove &key callback errorback)
  "Add the labels ADD to the PR and remove the labels REMOVE, lists of ids.
Adding comes first, then removing, each once the one before is
done.  Once GitHub has made both, the PR's labels in the store are
GitHub's and CALLBACK gets the PR's ref.  ERRORBACK gets the error
of the first that fails.  (Flow F12.)

Example:
  (approve-mutation-set-labels \\='(\"LA_bug\") nil :callback #\\='ignore)"
  (approve-mutation--add-and-remove
   add remove
   :add-query "mutations/add-labels-to-labelable.graphql"
   :remove-query "mutations/remove-labels-from-labelable.graphql"
   :mutations '(addLabelsToLabelable . removeLabelsFromLabelable)
   :payload 'labelable
   :variables (lambda (pr ids) `((labelableId . ,pr) (labelIds . ,ids)))
   :what "labels" :callback callback :errorback errorback))

(cl-defun approve-mutation-set-assignees (add remove &key callback errorback)
  "Assign the users ADD to the PR and unassign the users REMOVE, lists of ids.
Assigning comes first, then unassigning, each once the one before is
done.  Once GitHub has made both, the PR's assignees in the store
are GitHub's and CALLBACK gets the PR's ref.  ERRORBACK gets the
error of the first that fails.  (Flow F12.)"
  (approve-mutation--add-and-remove
   add remove
   :add-query "mutations/add-assignees-to-assignable.graphql"
   :remove-query "mutations/remove-assignees-from-assignable.graphql"
   :mutations '(addAssigneesToAssignable . removeAssigneesFromAssignable)
   :payload 'assignable
   :variables (lambda (pr ids) `((assignableId . ,pr) (assigneeIds . ,ids)))
   :what "assignees" :callback callback :errorback errorback))

(cl-defun approve-mutation-request-reviews (&key users teams bots callback errorback)
  "Ask USERS, TEAMS and BOTS, lists of ids, for a review, and no one else.
The ids are the whole set of review requests: one asked before and
left out is asked no more, and no ids ask no one.  Asking someone
who has reviewed asks them again.  Once GitHub has it, the PR's
review state in the store is GitHub's, its review requests with it,
and CALLBACK gets the PR's ref.  ERRORBACK gets the error otherwise.
\(Flow F12.)

Example:
  (approve-mutation-request-reviews :users \\='(\"U_1\") :teams \\='(\"T_1\"))"
  (funcall (approve-mutation--step
            "mutations/request-reviews.graphql"
            `((pullRequestId . ,(approve-mutation--pull-request-id))
              (userIds . ,(vconcat users))
              (teamIds . ,(vconcat teams))
              (botIds . ,(vconcat bots)))
            'requestReviews 'pullRequest "Asking for reviews")
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-milestone (milestone-id &key callback errorback)
  "Put the PR in the milestone MILESTONE-ID, or in none when it is nil.
Once GitHub has, the PR's milestone in the store is GitHub's and
CALLBACK gets the PR's ref.  ERRORBACK gets the error otherwise.
\(Flow F12.)"
  (funcall (approve-mutation--step
            "mutations/update-pull-request.graphql"
            `((pullRequestId . ,(approve-mutation--pull-request-id))
              (milestoneId . ,(or milestone-id :null)))
            'updatePullRequest 'pullRequest
            (if milestone-id "Setting the milestone" "Taking the milestone away"))
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-title (title &key callback errorback)
  "Make TITLE the title of the PR.
Once GitHub has it, the store holds the new title and CALLBACK gets
the PR's ref.  ERRORBACK gets the error otherwise.  (Flow F08.)"
  (funcall (approve-mutation--step
            "mutations/update-pull-request.graphql"
            `((pullRequestId . ,(approve-mutation--pull-request-id)) (title . ,title))
            'updatePullRequest 'pullRequest "Saving the title")
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-description (body &key callback errorback)
  "Make BODY, markdown, the description of the PR; an empty BODY empties it.
Once GitHub has it, the store holds the new text, as markdown and as
GitHub renders it, and when it was edited; CALLBACK gets the PR's
ref.  ERRORBACK gets the error otherwise.  (Flow F08.)"
  (funcall (approve-mutation--step
            "mutations/update-pull-request.graphql"
            `((pullRequestId . ,(approve-mutation--pull-request-id))
              ;; An empty string, not nil: nil would leave the field out.
              (body . ,(or body "")))
            'updatePullRequest 'pullRequest "Saving the description")
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-draft (draft &key callback errorback)
  "Make the PR a draft when DRAFT is non-nil, else ready for review.
Once GitHub has, the store says whether it is a draft, its review
state is GitHub's (marking it ready may ask code owners for
reviews), and CALLBACK gets the PR's ref.  ERRORBACK gets the error
otherwise.  (Flow F12.)"
  (funcall (approve-mutation--step
            (if draft
                "mutations/convert-pull-request-to-draft.graphql"
              "mutations/mark-pull-request-ready-for-review.graphql")
            `((pullRequestId . ,(approve-mutation--pull-request-id)))
            (if draft 'convertPullRequestToDraft 'markPullRequestReadyForReview)
            'pullRequest
            (if draft "Converting to a draft" "Marking ready for review"))
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-subscription (state &key callback errorback)
  "Make STATE how the viewer hears of the PR.
STATE is \"SUBSCRIBED\" (all of it), \"UNSUBSCRIBED\" (when
participating or mentioned) or \"IGNORED\" (never).  Once GitHub
has it, the PR's `viewerSubscription' in the store is STATE and
CALLBACK gets the PR's ref.  ERRORBACK gets the error otherwise.
\(Flow F12.)"
  (funcall (approve-mutation--step
            "mutations/update-subscription.graphql"
            `((subscribableId . ,(approve-mutation--pull-request-id)) (state . ,state))
            'updateSubscription 'subscribable "Changing the subscription")
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-locked (locked &key reason callback errorback)
  "Lock the conversation of the PR when LOCKED is non-nil, else unlock it.
REASON, when locking, is \"OFF_TOPIC\", \"TOO_HEATED\", \"RESOLVED\",
\"SPAM\" or nil for none.  Once GitHub has, the store says whether
it is locked and why, and CALLBACK gets the PR's ref.  ERRORBACK
gets the error otherwise."
  (let ((pr (approve-mutation--pull-request-id)))
    (funcall (if locked
                 (approve-mutation--step
                  "mutations/lock-lockable.graphql"
                  `((lockableId . ,pr) (lockReason . ,reason))
                  'lockLockable 'lockedRecord "Locking the conversation")
               (approve-mutation--step
                "mutations/unlock-lockable.graphql" `((lockableId . ,pr))
                'unlockLockable 'unlockedRecord "Unlocking the conversation"))
             callback (or errorback #'approve-api-default-errorback))))

;;; Reactions

(cl-defun approve-mutation-react (subject-id content &key remove callback errorback)
  "React with CONTENT to SUBJECT-ID, or take the reaction back with REMOVE.
SUBJECT-ID is a comment, a review or the PR (its description).
CONTENT is GitHub's name of the reaction, such as \"THUMBS_UP\" or
\"HEART\".  Once GitHub has it, the subject's reactions in the store
are GitHub's and CALLBACK gets the subject's ref.  ERRORBACK gets the
error otherwise.  (Flow F11.)

Example:
  (approve-mutation-react \"IC_1\" \"HOORAY\" :callback #\\='ignore)"
  (funcall (approve-mutation--step
            (if remove "mutations/remove-reaction.graphql" "mutations/add-reaction.graphql")
            `((subjectId . ,subject-id) (content . ,content))
            (if remove 'removeReaction 'addReaction) 'subject
            (if remove "Taking the reaction back" "Reacting"))
           callback (or errorback #'approve-api-default-errorback)))

;;; Hidden comments

(cl-defun approve-mutation-set-hidden (subject-id hidden &key reason callback errorback)
  "Hide the comment or review SUBJECT-ID when HIDDEN is non-nil, else show it.
REASON, when hiding, is what GitHub shows for it: \"SPAM\",
\"ABUSE\", \"OFF_TOPIC\", \"OUTDATED\", \"DUPLICATE\", \"RESOLVED\"
or \"LOW_QUALITY\".  Once GitHub has, the store says whether it is
hidden and why, and CALLBACK gets its ref.  ERRORBACK gets the error
otherwise."
  (funcall (if hidden
               (approve-mutation--step
                "mutations/minimize-comment.graphql"
                `((subjectId . ,subject-id) (classifier . ,reason))
                'minimizeComment 'minimizedComment "Hiding the comment")
             (approve-mutation--step
              "mutations/unminimize-comment.graphql" `((subjectId . ,subject-id))
              'unminimizeComment 'unminimizedComment "Showing the comment"))
           callback (or errorback #'approve-api-default-errorback)))

;;; Commits

(defun approve-mutation--commit-message (message)
  "Return MESSAGE as a CommitMessage: its first line, then the rest.
The body is what follows the first line, without the blank lines
around it; it is left out when there is none."
  (let* ((lines (split-string (string-trim message) "\n"))
         (body (string-trim (string-join (cdr lines) "\n"))))
    `((headline . ,(string-trim (car lines)))
      ,@(and (not (string-empty-p body)) `((body . ,body))))))

(defun approve-mutation--stale-p (err)
  "Return non-nil if ERR says the branch is not where it was expected."
  (seq-some (lambda (error) (equal (alist-get 'type error) "STALE_DATA"))
            (plist-get (cddr err) :errors)))

(cl-defun approve-mutation-commit-files (files message &key callback errorback)
  "Commit FILES to the PR's branch with MESSAGE, as GitHub's button does.
FILES is a list of (PATH . TEXT): each file's whole new text.
MESSAGE's first line is the commit's headline, the rest its body.
The commit goes on the head branch in the head repository, a fork's
too, and only if the branch is still at the head the store holds:
else GitHub refuses, and ERRORBACK says the branch has moved.  The
commit is the viewer's, signed by GitHub (decision D41).

The store is not changed: a commit changes the diff, the threads and
the commits, which only loading the PR again brings.  It keeps the
head it had, so another commit before that load is refused too.
CALLBACK gets the new commit's oid.  ERRORBACK gets the error
otherwise.  (Flow F23.)

Example:
  (approve-mutation-commit-files (list (cons \"a.el\" text))
                                 \"Apply suggestion from @bob\"
                                 :callback #\='ignore)"
  (let-alist (approve-pr-fields '(headRefName headRefOid headRepository))
    (unless .headRepository.nameWithOwner
      (signal 'approve-mutation-refused (list "The pull request's branch is gone")))
    (let ((errorback (or errorback #'approve-api-default-errorback)))
      (approve-mutation--send
       "mutations/create-commit-on-branch.graphql"
       `((branch (repositoryNameWithOwner . ,.headRepository.nameWithOwner)
                 (branchName . ,.headRefName))
         (expectedHeadOid . ,.headRefOid)
         (message . ,(approve-mutation--commit-message message))
         (fileChanges
          (additions . ,(vconcat
                         (mapcar (lambda (file)
                                   `((path . ,(car file))
                                     (contents . ,(base64-encode-string
                                                   (encode-coding-string (cdr file)
                                                                         'utf-8-unix)
                                                   t))))
                                 files)))))
       :progress (format "Committing to %s" .headRefName)
       :apply (lambda (data)
                (let-alist (approve-mutation--answer data 'createCommitOnBranch 'commit)
                  .oid))
       :callback callback
       :errorback (lambda (err)
                    (funcall errorback
                             (if (approve-mutation--stale-p err)
                                 (list 'approve-api-graphql-error
                                       (format "%s has commits this buffer hasn't loaded; \
refresh it, then try again" .headRefName))
                               err)))))))

;;; Merging

(defun approve-mutation--merge-variables (method headline body)
  "Return the variables of a merge by METHOD with HEADLINE and BODY.
The merge is of the head the store holds.  A nil HEADLINE or BODY is
left out, so GitHub writes its own; an empty BODY is sent empty."
  `((pullRequestId . ,(approve-mutation--pull-request-id))
    (mergeMethod . ,method)
    (commitHeadline . ,headline)
    (commitBody . ,body)
    (expectedHeadOid . ,(approve-pr-field 'headRefOid))))

(cl-defun approve-mutation-merge (method &key headline body callback errorback)
  "Merge the PR by METHOD: \"MERGE\", \"SQUASH\" or \"REBASE\".
HEADLINE and BODY are the commit message; when nil, GitHub writes
its own.  The merge is of the head the store holds: when the branch
has moved, GitHub refuses.  GitHub also merges a closed PR, so the
caller checks first (decision D42).  Once GitHub has merged it, the
store has the PR's merge state and CALLBACK gets the PR's ref; the
new commits and events come with a load.  ERRORBACK gets the error
otherwise.  (Flow F13.)

Example:
  (approve-mutation-merge \"SQUASH\" :headline \"Retry (#12)\" :body \"\")"
  (funcall (approve-mutation--step
            "mutations/merge-pull-request.graphql"
            (approve-mutation--merge-variables method headline body)
            'mergePullRequest 'pullRequest "Merging")
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-auto-merge (enable &key method headline body
                                                  callback errorback)
  "Merge the PR by METHOD once it may, when ENABLE is non-nil; else no more.
That is auto-merge: GitHub merges it once the base's rules allow it.
METHOD, HEADLINE and BODY are as for `approve-mutation-merge'.  Once
GitHub has, the store has the PR's merge state, with its
`autoMergeRequest', and CALLBACK gets the PR's ref.  ERRORBACK gets
the error otherwise.  (Flow F13.)"
  (funcall (if enable
               (approve-mutation--step
                "mutations/enable-pull-request-auto-merge.graphql"
                (approve-mutation--merge-variables method headline body)
                'enablePullRequestAutoMerge 'pullRequest "Enabling auto-merge")
             (approve-mutation--step
              "mutations/disable-pull-request-auto-merge.graphql"
              `((pullRequestId . ,(approve-mutation--pull-request-id)))
              'disablePullRequestAutoMerge 'pullRequest "Disabling auto-merge"))
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-queued (queued &key callback errorback)
  "Add the PR to its base's merge queue when QUEUED is non-nil, else take it out.
It is added as the store holds it (its head).  Once GitHub has, the
store has the PR's merge state and CALLBACK gets the PR's ref.
ERRORBACK gets the error otherwise.  (Flow F13.)"
  (let ((pr (approve-mutation--pull-request-id))
        (entry (alist-get 'id (approve-pr-field 'mergeQueueEntry))))
    (unless (or queued entry)
      (signal 'approve-mutation-refused (list "The pull request is not in a merge queue")))
    (approve-mutation--send
     (if queued
         "mutations/enqueue-pull-request.graphql"
       "mutations/dequeue-pull-request.graphql")
     (if queued
         `((pullRequestId . ,pr) (expectedHeadOid . ,(approve-pr-field 'headRefOid)))
       `((id . ,entry)))
     :progress (if queued "Adding to the merge queue" "Taking out of the merge queue")
     :apply (lambda (data)
              (let ((entry (approve-mutation--answer
                            data (if queued 'enqueuePullRequest 'dequeuePullRequest)
                            'mergeQueueEntry)))
                (approve-mutation--patch (alist-get 'pullRequest entry))))
     :callback callback
     :errorback errorback)))

(cl-defun approve-mutation-update-branch (&key rebase callback errorback)
  "Bring the PR's branch up to date with its base: merge the base in.
With REBASE, rebase the branch on the base instead.  It starts from
the head the store holds: when the branch has moved, GitHub refuses.
GitHub makes the new commits a moment after it answers, so the store
keeps the head it had; CALLBACK gets the PR's ref, and the caller
waits for the new head (`approve-query-await-head' with :away) and
loads the PR.  ERRORBACK gets the error otherwise.  (Flow F13.)"
  (funcall (approve-mutation--step
            "mutations/update-pull-request-branch.graphql"
            `((pullRequestId . ,(approve-mutation--pull-request-id))
              (expectedHeadOid . ,(approve-pr-field 'headRefOid))
              (updateMethod . ,(if rebase "REBASE" "MERGE")))
            'updatePullRequestBranch 'pullRequest
            (if rebase "Rebasing the branch" "Updating the branch"))
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-set-closed (closed &key callback errorback)
  "Close the PR when CLOSED is non-nil, else reopen it.
Once GitHub has, the store has the PR's merge state (closed, and
whether the viewer may now reopen it or delete its branch) and
CALLBACK gets the PR's ref.  ERRORBACK gets the error otherwise.
\(Flow F13.)"
  (funcall (approve-mutation--step
            (if closed
                "mutations/close-pull-request.graphql"
              "mutations/reopen-pull-request.graphql")
            `((pullRequestId . ,(approve-mutation--pull-request-id)))
            (if closed 'closePullRequest 'reopenPullRequest) 'pullRequest
            (if closed "Closing" "Reopening"))
           callback (or errorback #'approve-api-default-errorback)))

(cl-defun approve-mutation-delete-head-branch (&key callback errorback)
  "Delete the PR's head branch.
GitHub allows it once the PR is merged or closed
\(`viewerCanDeleteHeadRef').  Once GitHub has, the PR in the store has
no head ref, and may delete none; CALLBACK gets the PR's ref.
ERRORBACK gets the error otherwise.  (Flow F13.)"
  (let ((pr (approve-model-root-ref))
        (ref (alist-get 'id (approve-pr-field 'headRef))))
    (unless ref
      (signal 'approve-mutation-refused (list "The pull request's branch is gone")))
    (approve-mutation--send
     "mutations/delete-ref.graphql" `((refId . ,ref))
     :progress (format "Deleting %s" (approve-pr-field 'headRefName))
     :apply (lambda (_data)
              ;; The answer holds nothing: a deletion is an edit (D35).
              (approve-model-patch `((__typename . "PullRequest")
                                     (id . ,(approve-model-ref-id pr))
                                     (headRef) (viewerCanDeleteHeadRef)))
              pr)
     :callback callback
     :errorback errorback)))

(provide 'approve-mutations)

;;; approve-mutations.el ends here
