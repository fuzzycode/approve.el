;;; approve-thread.el --- Review threads and their comments -*- lexical-binding: t; -*-

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

;; A review thread is drawn as a box under the line it belongs to
;; (docs/ui.org, "Bottom: files and diff" and "Threads"):
;;
;;   ╭─ bob · 2 days ago ──────────────────────────
;;   │ Should this be configurable per call?
;;   │
;;   │ alice · 1 day ago
;;   │ Good idea, done in d4e5f6a.
;;   │ 👍 1
;;   ╰─ r reply · R resolve ────────────────────────
;;
;; The thread is an `approve-thread-section' (value: thread id); each
;; comment is an `approve-comment-section' (value: comment id).  The
;; bar left of the comments is decoration (`approve-render-prefix'), so
;; copying a comment gives its text alone.  Resolved threads start collapsed
;; to their heading.  Pending comments, which only the viewer sees
;; until the review is submitted, say "✎ Pending" in the `approve-pending'
;; face instead of their author and date; they have no background.
;; Comments that others published after the viewer's last review say
;; "new" after their byline, and their bar is heavy, ┃, in the
;; `approve-new' face (decision D44).
;;
;; `approve-thread-insert-comment-body' draws a comment's body,
;; reactions and hidden state; the conversation uses it too.
;;
;; Layer rules: this is the renderer layer.  It inserts text from the
;; data it is given; it never reads the store's internals, changes it
;; or calls the network.

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

(defclass approve-thread-section (magit-section) ()
  "A review thread; its value is the thread id.")
(defclass approve-comment-section (magit-section) ()
  "A comment in a review thread; its value is the comment id.")

(defun approve-thread-width ()
  "Return the width of a thread box: the text width plus its bar."
  (+ approve-fill-column 2))

(defun approve-thread--rule (left text)
  "Return a box rule: LEFT, TEXT, then lines up to the box width."
  (let* ((head (concat (approve-face left 'approve-thread-border) text))
         (fill (- (approve-thread-width) (string-width head) 1)))
    (concat head
            (if (> fill 0)
                (approve-face (concat (if (string-empty-p text) "" " ")
                                      (make-string (if (string-empty-p text) (1+ fill) fill)
                                                   ?─))
                              'approve-thread-border)
              ""))))

(defun approve-thread-pending-p (comment)
  "Return non-nil if COMMENT is in the viewer's pending review."
  (equal (alist-get 'state comment) "PENDING"))

(defun approve-thread-comment-byline (comment &optional new)
  "Return who wrote COMMENT and when: \"bob · 2 days ago\".
A pending comment says \"✎ Pending · you\" instead.  With NEW, the
comment is new since the viewer's last review, and the byline ends
in \"new\" (`approve-render-new')."
  (let-alist comment
    (approve-render-join
     (if (approve-thread-pending-p comment)
         (list (concat (approve-icon 'pending-comment 'approve-pending) " "
                       (approve-face "Pending" 'approve-pending))
               (approve-render-user .author))
       (list (approve-render-user .author)
             (approve-render-date .createdAt)
             (approve-render-edited comment)
             (and new (approve-render-new .id)))))))

(defun approve-thread-insert-comment-body (comment)
  "Insert the body of COMMENT at column 0: its text and reactions.
A hidden (minimized) comment is one dim line saying why."
  (let-alist comment
    (cond
     (.isMinimized
      (insert (approve-face (format "This comment was hidden%s."
                                    (if .minimizedReason
                                        (format " (%s)" (downcase .minimizedReason))
                                      ""))
                            'approve-dim)
              "\n"))
     ((not (approve-html-insert .bodyHTML))
      (insert (approve-render-empty "No description provided."))))
    (unless .isMinimized
      (when-let* ((reactions (approve-render-reactions-text .reactionGroups .id)))
        (insert reactions "\n")))))

(defun approve-thread--state (thread)
  "Return the state notes of THREAD's heading: outdated, resolved by."
  (let-alist thread
    (list (and .isOutdated (approve-face "outdated" 'approve-outdated))
          (and .isResolved
               (approve-face (if .resolvedBy
                                 (format "resolved by %s" .resolvedBy.login)
                               "resolved")
                             'approve-resolved)))))

(defun approve-thread--heading (thread first new)
  "Return the box heading of THREAD whose FIRST comment opens it.
NEW if FIRST is new since the viewer's last review."
  (approve-thread--rule
   "╭─ "
   (approve-render-join
    (cons (approve-thread-comment-byline first new) (approve-thread--state thread)))))

(defun approve-thread--footer (thread)
  "Return the closing rule of THREAD, with its key hints."
  (let-alist thread
    (approve-thread--rule
     "╰─"
     (if-let* ((hints (approve-render-hints
                       (and .viewerCanReply '(approve-reply "reply"))
                       (cond (.isResolved
                              (and .viewerCanUnresolve '(approve-resolve "unresolve")))
                             (.viewerCanResolve '(approve-resolve "resolve"))))))
         (concat " " hints)
       ""))))

(defun approve-thread--insert-comment (comment first new)
  "Insert COMMENT of a thread; FIRST if the thread's heading names it.
NEW if it is new since the viewer's last review: its byline says so,
and its bar is `approve-new' and heavy."
  (let ((start (point)))
    (magit-insert-section (approve-comment-section (alist-get 'id comment))
      (unless first
        (magit-insert-heading (approve-thread-comment-byline comment new)))
      (approve-thread-insert-comment-body comment))
    (approve-render-prefix start (point)
                           (if new
                               (approve-face "┃ " 'approve-new)
                             (approve-face "│ " 'approve-thread-border)))))

(defun approve-thread-insert (thread)
  "Insert THREAD, a resolved PullRequestReviewThread, as a box.
It starts at column 0; the caller indents it.  A thread does not
fold by itself, so no line of it stays behind when it is folded:
the diff line it is on, or the group it is in, folds it.  Comments
new since the viewer's last review are marked (decision D44)."
  (let* ((connection (alist-get 'comments thread))
         (comments (approve-model-nodes connection))
         (first (car comments))
         (since (approve-pr-last-review-time))
         (new (mapcar (lambda (comment) (and since (approve-pr-new-p comment since)))
                      comments))
         (more (- (or (approve-model-total-count connection) (length comments))
                  (length comments)))
         (bar (approve-face "│ " 'approve-thread-border))
         (start (point))
         (section
          (magit-insert-section (approve-thread-section (alist-get 'id thread))
            (magit-insert-heading (approve-thread--heading thread first (car new)))
            (cl-loop for comment in comments
                     for index from 0
                     for comment-new in new
                     do (unless (= index 0)
                          (let ((blank (point)))
                            (insert "\n")
                            (approve-render-prefix blank (point) bar)))
                     (approve-thread--insert-comment comment (= index 0) comment-new))
            (when (> more 0)
              (let ((line (point)))
                (insert (approve-face (format "… %d more comment%s on GitHub" more
                                              (if (= more 1) "" "s"))
                                      'approve-dim)
                        "\n")
                (approve-render-prefix line (point) bar)))
            (insert (approve-thread--footer thread) "\n"))))
    ;; No body to fold, whatever the visibility cache or a level
    ;; command says.
    (oset section content nil)
    (oset section hidden nil)
    (when (alist-get 'isResolved thread)
      (approve-face-region start (point) 'approve-resolved))))

(defun approve-thread-summary (thread)
  "Return a one-line summary of THREAD: its first comment's author and text."
  (let* ((first (car (approve-model-nodes (alist-get 'comments thread)))))
    (concat (approve-render-user (alist-get 'author first)) ": "
            (approve-render-truncate
             (approve-render-first-line (alist-get 'bodyHTML first)) 60))))

(provide 'approve-thread)

;;; approve-thread.el ends here
