;;; approve-ui-comments.el --- Comment UI components for Approve  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This file provides UI components for displaying comments in Approve
;; pull request buffers.
;;
;; It handles multiple types of comments:
;; - Conversation comments (top-level PR comments, IssueComment)
;; - Review comments (inline code comments, PullRequestReviewComment)
;; - Commit comments (comments on specific commits, CommitComment)
;;
;; The module provides reusable functions for rendering HTML comment bodies
;; and formatting comment metadata (author, timestamp, etc.) that can be
;; used across all comment types.

;;; Code:

(require 'cl-lib)
(require 'magit-section)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)
(require 'approve-ui-html)
(require 'approve-eldoc)

;;; Customization

(defgroup approve-comments nil
  "Comment display settings for Approve."
  :group 'approve
  :prefix "approve-comment-")

(defcustom approve-comment-fill-column 80
  "Column number to wrap comment text at.
This affects both plain text and HTML rendering."
  :group 'approve-comments
  :type 'integer)

(defcustom approve-comment-body-indent 2
  "Number of spaces to indent comment body content."
  :group 'approve-comments
  :type 'integer)

(defcustom approve-comment-empty-message "No comments yet."
  "Message to display when there are no comments."
  :group 'approve-comments
  :type 'string)

;;; Comment Body Rendering

(defun approve-comment--insert-body (body-html &optional indent)
  "Insert rendered comment body from BODY-HTML at point.
INDENT specifies additional indentation in spaces.
If BODY-HTML is nil or empty, inserts a placeholder message."
  (let ((indent-val (or indent approve-comment-body-indent))
        (indent-str (make-string (or indent approve-comment-body-indent) ?\s)))
    (if (or (null body-html) (string-empty-p (string-trim body-html)))
        (insert indent-str
                (propertize "No content" 'face 'approve-comment-empty-face)
                "\n")
      ;; Render HTML directly into buffer, preserving text properties
      (let ((approve-html-fill-column approve-comment-fill-column))
        (approve-html--insert-raw body-html indent-val)))))

;;; Comment Metadata Formatting

(defun approve-comment--format-header (author created-at &optional state edited-info)
  "Format a comment header line with AUTHOR and CREATED-AT timestamp.
AUTHOR is an alist with `login', `name', `email', `url' fields.
CREATED-AT is an ISO 8601 date string.
Optional STATE is a string like \"PENDING\", etc.
Optional EDITED-INFO is a cons cell (EDITED-P . LAST-EDITED-AT) where EDITED-P
is non-nil if the comment was edited and LAST-EDITED-AT is the ISO 8601 date
of the last edit."
  (let* ((author-text (approve-ui-format-actor author 'approve-comment-author-face))
         (timestamp (approve-ui-format-date created-at approve-timestamp-format))
         (timestamp-text (if timestamp
                             (propertize timestamp 'face 'approve-comment-timestamp-face)
                           ""))
         (state-text (approve-comment--format-state-indicator state))
         (edited-text (approve-comment--format-edited-indicator edited-info)))
    (concat author-text
            (when timestamp-text
              (concat " " (propertize "·" 'face 'shadow) " " timestamp-text))
            (when edited-text
              (concat " " edited-text))
            (when state-text
              (concat " " state-text)))))

(defun approve-comment--state-face (state)
  "Return the appropriate face for comment STATE."
  (pcase state
    ("APPROVED" 'approve-comment-state-approved-face)
    ("CHANGES_REQUESTED" 'approve-comment-state-changes-requested-face)
    ("COMMENTED" 'approve-comment-state-commented-face)
    ("DISMISSED" 'approve-comment-state-dismissed-face)
    ("PENDING" 'approve-comment-state-pending-face)
    (_ 'approve-comment-state-default-face)))

;;; Edited Indicator

(defun approve-comment--format-edited-indicator (edited-info)
  "Format an edited indicator from EDITED-INFO.
EDITED-INFO is a cons cell (EDITED-P . LAST-EDITED-AT) where EDITED-P
is non-nil if the comment was edited and LAST-EDITED-AT is the ISO 8601
date of the last edit (may be nil).
Returns a propertized string with eldoc hover showing the edit timestamp,
or nil if the comment was not edited."
  (when (and edited-info (car edited-info))
    (let* ((last-edited-at (cdr edited-info))
           (formatted-date (when last-edited-at
                             (approve-ui-format-date last-edited-at
                                                     approve-timestamp-format)))
           (hover-doc (if formatted-date
                          formatted-date
                        "Edited")))
      (approve-eldoc-propertize
       (approve-ui-propertize-face "(edited)" 'approve-comment-edited-face)
       "Last Edited"
       hover-doc
       'approve-comment-edited-face))))

(defun approve-comment--format-state-indicator (state)
  "Format a state indicator for STATE.
Returns a propertized string for PENDING state, or nil for other states."
  (when (equal state "PENDING")
    (approve-eldoc-propertize
     (approve-ui-propertize-face "(pending)" 'approve-comment-pending-face)
     "Status"
     "This comment is part of a pending review"
     'approve-comment-pending-face)))

(defun approve-comment--make-edited-info (comment)
  "Extract edited information from COMMENT.
Returns a cons cell (EDITED-P . LAST-EDITED-AT) suitable for passing to
`approve-comment--format-header'."
  (let ((includes-created-edit (alist-get 'includesCreatedEdit comment))
        (last-edited-at (alist-get 'lastEditedAt comment)))
    (cons includes-created-edit last-edited-at)))

;;; Reaction Formatting

(defconst approve-comment--reaction-emoji-map
  '(("THUMBS_UP" . "👍")
    ("THUMBS_DOWN" . "👎")
    ("LAUGH" . "😄")
    ("HOORAY" . "🎉")
    ("CONFUSED" . "😕")
    ("HEART" . "❤️")
    ("ROCKET" . "🚀")
    ("EYES" . "👀"))
  "Mapping from GitHub reaction types to emoji.")

(defun approve-comment--format-reactions (reaction-groups)
  "Format REACTION-GROUPS for display.
REACTION-GROUPS is a list of alists with `content' and `reactors' fields.
Returns a formatted string or nil if no reactions."
  (let ((reactions-with-count
         (cl-remove-if
          (lambda (rg)
            (zerop (or (alist-get 'totalCount (alist-get 'reactors rg)) 0)))
          reaction-groups)))
    (when reactions-with-count
      (mapconcat
       (lambda (rg)
         (let* ((content (alist-get 'content rg))
                (count (alist-get 'totalCount (alist-get 'reactors rg)))
                (emoji (or (alist-get content approve-comment--reaction-emoji-map
                                      nil nil #'string=)
                           content)))
           (propertize (format "%s %d" emoji count)
                       'face 'approve-comment-reaction-face)))
       reactions-with-count
       "  "))))

;;; Conversation Comments Section

(defun approve-insert-conversation-section ()
  "Insert the conversation section showing top-level PR comments.
This displays IssueComment entries from the PR's comments field."
  (when-let ((comments-data (approve-model-root 'comments)))
    (let* ((comments (approve-model-get-nodes comments-data))
           (comment-count (length comments))
           (total-count (approve-model-get-total-count comments-data))
           (truncated-p (approve-model-truncated-p comments-data)))
      (insert "\n")
      (magit-insert-section (conversation)
        (magit-insert-heading comment-count (if (= comment-count 1) "Comment" "Comments"))
        (if (zerop comment-count)
            (progn
              (insert (propertize approve-comment-empty-message
                                  'face 'approve-comment-empty-face))
              (insert "\n"))
          ;; Insert each comment
          (dolist (comment comments)
            (approve-comment--insert-issue-comment comment)))
        ;; Show truncation info
        (when truncated-p
          (insert (propertize
                   (format "(showing %d of %d)" comment-count (or total-count "?"))
                   'face 'approve-pagination-truncated-face)
                  "\n"))
        (insert "\n")))))

(defun approve-comment--insert-issue-comment (comment)
  "Insert a single issue COMMENT as a magit section.
COMMENT is an alist with author, body, bodyHTML, createdAt, id, reactionGroups,
includesCreatedEdit, and lastEditedAt."
  (let* ((id (alist-get 'id comment))
         (author (alist-get 'author comment))
         (body-html (alist-get 'bodyHTML comment))
         (created-at (alist-get 'createdAt comment))
         (reaction-groups (alist-get 'reactionGroups comment))
         (edited-info (approve-comment--make-edited-info comment)))
    (magit-insert-section (issue-comment id)
      (magit-insert-heading
        (approve-comment--format-header author created-at nil edited-info))
      ;; Comment body
      (approve-comment--insert-body body-html)
      ;; Reactions
      (when-let ((reactions-str (approve-comment--format-reactions reaction-groups)))
        (insert (make-string approve-comment-body-indent ?\s)
                reactions-str
                "\n"))
      (insert "\n"))))

(provide 'approve-ui-comments)
;;; approve-ui-comments.el ends here
