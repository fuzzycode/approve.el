;;; approve-review.el --- The viewer's pending review: comment, submit, discard -*- lexical-binding: t; -*-

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

;; The rules of the viewer's pending review (docs/flows.org, F05 and
;; F09; decisions D22 and D32), between the commands and the mutations:
;;
;;   `approve-review-send-comment'  a comment on code joins the pending
;;                                  review, or starts one; or it is a
;;                                  single comment, published at once
;;   `approve-review-submit'        submit the pending review, or add a
;;                                  review with a verdict when none is
;;   `approve-review-why-not-verdict', `approve-review-why-not-body'
;;                                  what GitHub would refuse, said first
;;
;; and the commands that act on the review as a whole: the verdicts of
;; the submit transient (S, `approve-submit-review' in approve-menus.el),
;; its body, and X:
;;
;;   `approve-submit-approve'  `approve-submit-request-changes'
;;   `approve-submit-comment'  `approve-edit-review-body'
;;   `approve-discard-review'
;;
;; GitHub tracks the pending review, and the mutations keep the store's
;; copy of it true (decision D35), so this file only reads it
;; (`approve-pr-pending-review').  The body of the review being written
;; is an input draft, under `approve-review-body-target' (D32).
;;
;; Layer rules: this is the actions layer.  It reads the store, asks
;; for text and runs mutations through the action pipeline; it never
;; draws.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'approve-model)
(require 'approve-input)
(require 'approve-mutations)
(require 'approve-actions)
(require 'approve-pr)

(defcustom approve-comment-default 'review
  "Where \\<approve-pr-mode-map>\\[approve-comment] sends a comment on code.
`review' adds it to your pending review, starting one if needed (as
GitHub's \"Start a review\"), and \\[universal-argument] \\[approve-comment] posts a single
comment at once (\"Add single comment\").  `single' swaps the two.
While you have a pending review, every comment joins it: GitHub
allows no single comment then (decision D32).

  (setq approve-comment-default \\='single)"
  :type '(choice (const :tag "Add it to the pending review" review)
                 (const :tag "Post a single comment" single))
  :group 'approve)

(defconst approve-review-body-target '(review-body)
  "The input target of the body of the review being written.
Its draft is the body the review is submitted with (decision D32).")

;;; Reading the pending review

(defun approve-review-own-p ()
  "Return non-nil if the viewer wrote the pull request."
  (approve-pr-field 'viewerDidAuthor))

(defun approve-review-counts ()
  "Return (COMMENTS . REPLIES) of the pending review, or nil without one.
COMMENTS start threads; REPLIES answer threads."
  (when-let* ((review (approve-pr-pending-review)))
    (let ((comments (approve-model-nodes (alist-get 'comments review))))
      (let ((replies (seq-count (lambda (comment) (alist-get 'replyTo comment)) comments)))
        (cons (- (length comments) replies) replies)))))

(defun approve-review-summary ()
  "Return what the pending review holds, such as \"2 comments · 1 reply\".
Without a pending review, say so."
  (if-let* ((counts (approve-review-counts)))
      (let ((count (lambda (n one many)
                     (and (> n 0) (format "%d %s" n (if (= n 1) one many))))))
        (or (string-join (delq nil (list (funcall count (car counts) "comment" "comments")
                                         (funcall count (cdr counts) "reply" "replies")))
                         " · ")
            "no comments"))
    "no pending review"))

(defun approve-review-body ()
  "Return the body the review would be submitted with, or nil.
That is its draft, else the pending review's body."
  (let ((body (or (approve-input-draft approve-review-body-target)
                  (alist-get 'body (approve-pr-pending-review)))))
    (and body (not (string-blank-p body)) body)))

;;; Comments on code

(defun approve-review-single-p (prefix)
  "Return non-nil if a comment on code is a single comment.
PREFIX is the command's prefix argument: it swaps
`approve-comment-default'."
  (if (eq approve-comment-default 'single) (not prefix) (and prefix t)))

(defun approve-review-comment-note (single)
  "Return where a comment on code goes, for its input's header line.
SINGLE is non-nil when a single comment was asked for."
  (cond ((approve-pr-pending-review)
         (if single
             "pending review (no single comment while one is pending)"
           "pending review"))
        (single "single comment, published at once")
        (t "starts a review")))

(cl-defun approve-review-send-comment (target text &key single callback errorback)
  "Send TEXT, a comment on TARGET, where it goes (decisions D22, D32).
TARGET is as for `approve-mutation-add-thread': lines, or a whole
file.  With SINGLE and no pending review, the comment is published
at once: a review holding its thread, or for a whole file (which a
new review can't hold) a thread, then a submit.  Otherwise it joins
the pending review, or starts one.  CALLBACK gets the ref of the
thread, or of a single comment's review; ERRORBACK the error."
  (cond
   ((or (not single) (approve-pr-pending-review))
    (approve-mutation-add-thread target text :callback callback :errorback errorback))
   ((plist-get target :line)
    (approve-mutation-add-review :event "COMMENT"
                                 :threads (list (append (list :body text) target))
                                 :callback callback :errorback errorback))
   (t
    (approve-mutation-add-thread
     target text :errorback errorback
     :callback (lambda (_thread)
                 (approve-mutation-submit-review "COMMENT" :callback callback
                                                 :errorback errorback))))))

;;; Submitting

(defun approve-review-verdict-name (event)
  "Return the name of the verdict EVENT, as the submit menu says it."
  (pcase event
    ("APPROVE" "Approve")
    ("REQUEST_CHANGES" "Request changes")
    (_ "Comment")))

(defun approve-review-why-not-verdict (event)
  "Return why the viewer can't give the verdict EVENT, or nil.
GitHub refuses Approve and Request changes on one's own pull
request (decision D32)."
  (when (and (member event '("APPROVE" "REQUEST_CHANGES")) (approve-review-own-p))
    (format "GitHub doesn't let you %s your own pull request"
            (if (equal event "APPROVE") "approve" "request changes on"))))

(defun approve-review-why-not-body (event body)
  "Return why the verdict EVENT can't be sent with BODY, or nil.
Comment and Request changes need a body or comments (decision D32)."
  (when (and (member event '("REQUEST_CHANGES" "COMMENT"))
             (string-blank-p (or body ""))
             (let ((counts (approve-review-counts)))
               (or (null counts) (zerop (+ (car counts) (cdr counts))))))
    (format "%s needs a body when the review has no comments"
            (approve-review-verdict-name event))))

(cl-defun approve-review-submit (event body &key callback errorback)
  "Give the verdict EVENT with BODY: submit the pending review, or add one.
EVENT is \"APPROVE\", \"REQUEST_CHANGES\" or \"COMMENT\"; BODY may
be nil, and then a pending review keeps the body it has.  With no
pending review, a review is added and published at once (flow F09).
CALLBACK gets the review's ref, ERRORBACK the error."
  (let ((body (and body (not (string-blank-p body)) body)))
    (if (approve-pr-pending-review)
        (approve-mutation-submit-review event :body body
                                        :callback callback :errorback errorback)
      (approve-mutation-add-review :event event :body body
                                   :callback callback :errorback errorback))))

(defun approve-review--marks ()
  "Return the marks of an action on the whole pending review."
  (when-let* ((id (alist-get 'id (approve-pr-pending-review))))
    `((section approve-pending-review-section ,id))))

(defun approve-review--verdict (event no-body)
  "Give the verdict EVENT, asking for the body unless NO-BODY."
  (approve-action-context)
  (let ((what (format "submit the review (%s)" (approve-review-verdict-name event)))
        (why-not (approve-review-why-not-verdict event))
        (send (lambda (text callback errorback)
                (if-let* ((why (approve-review-why-not-body event text)))
                    (funcall errorback (list 'approve-mutation-refused why))
                  (approve-review-submit event text :callback callback
                                         :errorback errorback)))))
    (if no-body
        (approve-action-run :what what
                            :why-not (or why-not (approve-review-why-not-body event nil))
                            :marks (approve-review--marks)
                            :send send)
      (approve-action-run
       :what what :why-not why-not
       :input (list :prompt (format "Review body — %s" (approve-review-verdict-name event))
                    :context (approve-review-summary)
                    :target approve-review-body-target :name "review body"
                    :initial (alist-get 'body (approve-pr-pending-review))
                    :allow-empty t :commit-label "submit")
       :marks (approve-review--marks)
       :send send))))

(defun approve-submit-approve (&optional no-body)
  "Approve the pull request, submitting your pending review (flow F09).
The body is asked for first, starting from its draft; with a prefix
argument NO-BODY, approve at once without one."
  (interactive "P")
  (approve-review--verdict "APPROVE" no-body))

(defun approve-submit-request-changes (&optional no-body)
  "Request changes, submitting your pending review (flow F09).
The body is asked for first; with a prefix argument NO-BODY, submit
without one, which needs comments in the review."
  (interactive "P")
  (approve-review--verdict "REQUEST_CHANGES" no-body))

(defun approve-submit-comment (&optional no-body)
  "Submit your pending review as a comment, with no verdict (flow F09).
The body is asked for first; with a prefix argument NO-BODY, submit
without one, which needs comments in the review."
  (interactive "P")
  (approve-review--verdict "COMMENT" no-body))

(defun approve-edit-review-body ()
  "Write the body of the review you will submit, without submitting it.
It is kept as a draft, and the verdicts of the submit menu start
from it."
  (interactive)
  (approve-action-context)
  (approve-input-read :prompt "Review body" :context "kept until you submit"
                      :target approve-review-body-target :name "review body"
                      :initial (alist-get 'body (approve-pr-pending-review))
                      :allow-empty t :commit-label "keep"
                      :on-commit (lambda (_text)
                                   (message "Approve: the review body is kept for the submit"))))

;;; Discarding

(defun approve-discard-review ()
  "Discard your pending review, with its comments (flow F09).
GitHub deletes it; this asks first."
  (interactive)
  (approve-action-context)
  (let* ((review (or (approve-pr-pending-review)
                     (user-error "You have no pending review")))
         (id (alist-get 'id review)))
    (when (yes-or-no-p (format "Discard your pending review (%s)? " (approve-review-summary)))
      (approve-action-run
       :what "discard the review"
       :marks (approve-review--marks)
       :send (lambda (_text callback errorback)
               (approve-mutation-delete-review id :callback callback :errorback errorback))
       :done (lambda (_review)
               (approve-input-clear-draft approve-review-body-target))))))

(keymap-set approve-pr-mode-map "X" #'approve-discard-review)

(provide 'approve-review)

;;; approve-review.el ends here
