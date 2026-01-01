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
(require 'shr)
(require 'dom)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)
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

(defcustom approve-comment-blockquote-prefix "▌ "
  "Prefix string for blockquote lines.
This is inserted at the beginning of each line in a blockquote."
  :group 'approve-comments
  :type 'string)

;;; HTML Rendering

(defun approve-comment--shr-tag-pre (dom)
  "Custom rendering for <pre> tags in DOM.
Adds appropriate faces for code blocks."
  (let ((start (point)))
    (shr-ensure-newline)
    (shr-generic dom)
    (shr-ensure-newline)
    (add-face-text-property start (point) 'approve-comment-code-block-face)))

(defun approve-comment--shr-tag-code (dom)
  "Custom rendering for <code> tags in DOM.
Adds inline code face when not inside a <pre> block."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'approve-comment-code-inline-face)))

(defun approve-comment--shr-tag-blockquote (dom)
  "Custom rendering for <blockquote> tags in DOM.
Adds a visual border prefix and applies blockquote face."
  (shr-ensure-newline)
  (let ((start (point)))
    (shr-generic dom)
    (shr-ensure-newline)
    ;; Apply face to the entire blockquote content
    (add-face-text-property start (point) 'approve-comment-blockquote-face)
    ;; Add prefix to each line within the blockquote
    (save-excursion
      (goto-char start)
      (while (< (point) (point-max))
        (unless (eolp)
          (let ((prefix (propertize approve-comment-blockquote-prefix
                                    'face 'approve-comment-blockquote-border-face)))
            (insert prefix)))
        (forward-line 1)
        (when (>= (point) (point-max))
          (goto-char (point-max)))))))

(defun approve-comment--render-html (html &optional indent)
  "Render HTML string and return the result as a string.
INDENT is the number of spaces to add before each line (default 0).
The rendering is optimized for comment bodies with proper handling
of code blocks, links, and other common markdown-generated HTML."
  (let ((indent-pixels (* (or indent 0) (shr-string-pixel-width " "))))
    (with-temp-buffer
      (let ((shr-width approve-comment-fill-column)
            (shr-indentation indent-pixels)
            (shr-use-fonts nil)
            (shr-external-rendering-functions
             '((pre . approve-comment--shr-tag-pre)
               (code . approve-comment--shr-tag-code)
               (blockquote . approve-comment--shr-tag-blockquote)))
            (dom (with-temp-buffer
                   (insert html)
                   (libxml-parse-html-region (point-min) (point-max)))))
        (shr-insert-document dom))
      ;; Clean up excessive whitespace
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n"))
      ;; Remove trailing whitespace
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match ""))
      ;; Return trimmed result
      (string-trim (buffer-string)))))

(defun approve-comment--insert-body (body-html &optional indent)
  "Insert rendered comment body from BODY-HTML at point.
INDENT specifies additional indentation in spaces.
If BODY-HTML is nil or empty, inserts a placeholder message."
  (let ((indent-str (make-string (or indent approve-comment-body-indent) ?\s)))
    (if (or (null body-html) (string-empty-p (string-trim body-html)))
        (insert indent-str
                (propertize "No content" 'face 'approve-comment-empty-face)
                "\n")
      (let* ((rendered (approve-comment--render-html body-html indent))
             (lines (split-string rendered "\n")))
        (dolist (line lines)
          (insert indent-str line "\n"))))))

;;; Comment Metadata Formatting

(defun approve-comment--format-header (author created-at &optional state)
  "Format a comment header line with AUTHOR and CREATED-AT timestamp.
AUTHOR is an alist with `login', `name', `email', `url' fields.
CREATED-AT is an ISO 8601 date string.
Optional STATE is a string like \"APPROVED\", \"CHANGES_REQUESTED\", etc."
  (let* ((author-text (approve-ui-format-actor author 'approve-comment-author-face))
         (timestamp (approve-ui-format-date created-at approve-timestamp-format))
         (timestamp-text (if timestamp
                             (propertize timestamp 'face 'approve-comment-timestamp-face)
                           ""))
         (state-text (when state
                       (propertize (format "[%s]" state)
                                   'face (approve-comment--state-face state)))))
    (concat author-text
            (when timestamp-text
              (concat " " (propertize "·" 'face 'shadow) " " timestamp-text))
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
        (magit-insert-heading
          (format "%d %s"
                  (or total-count comment-count)
                  (if (= (or total-count comment-count) 1)
                      "Comment"
                    "Comments")))
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
COMMENT is an alist with author, body, bodyHTML, createdAt, id, reactionGroups."
  (let* ((id (alist-get 'id comment))
         (author (alist-get 'author comment))
         (body-html (alist-get 'bodyHTML comment))
         (created-at (alist-get 'createdAt comment))
         (reaction-groups (alist-get 'reactionGroups comment)))
    (magit-insert-section (issue-comment id)
      (magit-insert-heading
        (approve-comment--format-header author created-at))
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
