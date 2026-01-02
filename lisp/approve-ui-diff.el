;;; approve-ui-diff.el --- Diff rendering for Approve  -*- lexical-binding: t; -*-

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

;; This module provides diff rendering for Approve.
;;
;; Key responsibilities:
;; - Parsing and rendering unified diffs from GitHub's compare API
;; - Syntax highlighting for diff hunks using magit faces
;; - Integration with magit-section for collapsible file and hunk display
;; - Displaying inline review comments on diff lines
;;
;; The rendering mimics magit's commit diff display with:
;; - File sections that show filename and status
;; - Collapsible hunk sections within each file
;; - Proper diff highlighting (added/removed/context lines)
;; - Lines with comments are expandable to show the comment threads

;;; Code:

(require 'cl-lib)
(require 'magit-section)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)
(require 'approve-ui-comments)

;;; Customization

(defcustom approve-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    Never show fine differences.
`all'  Show fine differences for all displayed diff hunks.
t      Show fine differences for the current hunk only."
  :group 'approve
  :type '(choice (const :tag "No refinement" nil)
                 (const :tag "Immediately refine all hunks" all)
                 (const :tag "Refine current hunk only" t)))

(defcustom approve-diff-comment-indicator "💬"
  "Indicator shown on diff lines that have comments."
  :group 'approve
  :type 'string)

(defcustom approve-diff-comment-indent 4
  "Number of spaces to indent comments below diff lines."
  :group 'approve
  :type 'integer)

;;; Review Thread Lookup

(defun approve-diff--build-line-threads-index ()
  "Build an index of review threads by file path, line, and side.
Returns a hash table where keys are (PATH LINE SIDE) and values are
lists of thread alists.  Only includes LINE-type threads (not FILE-type).
LINE is the `line' field (new/right side) or `originalLine' (old/left side).
SIDE is \"LEFT\" or \"RIGHT\" based on `diffSide'."
  (let ((index (make-hash-table :test 'equal)))
    (when-let ((threads-data (approve-model-root 'reviewThreads)))
      (let ((threads (approve-model-get-nodes threads-data)))
        (dolist (thread threads)
          (let ((subject-type (alist-get 'subjectType thread))
                (path (alist-get 'path thread))
                (line (alist-get 'line thread))
                (original-line (alist-get 'originalLine thread))
                (diff-side (alist-get 'diffSide thread)))
            ;; Only include LINE comments, not FILE comments
            (when (and path (equal subject-type "LINE"))
              ;; Use line for RIGHT side, originalLine for LEFT side
              (let* ((effective-line (if (equal diff-side "LEFT")
                                         original-line
                                       line))
                     (key (list path effective-line diff-side)))
                (when effective-line
                  (puthash key
                           (cons thread (gethash key index))
                           index))))))))
    index))

(defun approve-diff--get-line-threads (index path line side)
  "Get threads from INDEX for PATH at LINE on SIDE.
SIDE should be \"LEFT\" or \"RIGHT\"."
  (gethash (list path line side) index))

(defun approve-diff--count-line-comments (threads)
  "Count total comments across THREADS."
  (let ((count 0))
    (dolist (thread threads count)
      (let ((comments-data (alist-get 'comments thread)))
        (setq count (+ count (length (approve-model-get-nodes comments-data))))))))

;;; Internal Functions

(defun approve-diff--file-status-string (status)
  "Convert GitHub file STATUS to display string."
  (pcase status
    ("added" "new file")
    ("removed" "deleted")
    ("modified" "modified")
    ("renamed" "renamed")
    ("copied" "copied")
    (_ status)))

(defun approve-diff--parse-hunk-header (header)
  "Parse a unified diff hunk HEADER line.
Returns a plist with :from-start, :from-count, :to-start, :to-count.
Example header: @@ -10,5 +12,7 @@ function_name"
  (when (string-match
         "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
         header)
    (list :from-start (string-to-number (match-string 1 header))
          :from-count (if (match-string 2 header)
                          (string-to-number (match-string 2 header))
                        1)
          :to-start (string-to-number (match-string 3 header))
          :to-count (if (match-string 4 header)
                        (string-to-number (match-string 4 header))
                      1))))

(defun approve-diff--split-patch-into-hunks (patch)
  "Split PATCH text into a list of hunks.
Each hunk is a cons (HEADER . CONTENT) where HEADER is the @@ line
and CONTENT is the diff lines for that hunk."
  (when (and patch (not (string-empty-p patch)))
    (let ((hunks nil)
          (current-header nil)
          (current-lines nil))
      (dolist (line (split-string patch "\n"))
        (cond
         ;; Hunk header
         ((string-prefix-p "@@" line)
          ;; Save previous hunk if any
          (when current-header
            (push (cons current-header (nreverse current-lines)) hunks))
          (setq current-header line)
          (setq current-lines nil))
         ;; Skip file headers (diff --git, index, ---, +++)
         ((or (string-prefix-p "diff --git" line)
              (string-prefix-p "index " line)
              (string-prefix-p "--- " line)
              (string-prefix-p "+++ " line)
              (string-prefix-p "new file" line)
              (string-prefix-p "deleted file" line)
              (string-prefix-p "similarity index" line)
              (string-prefix-p "rename from" line)
              (string-prefix-p "rename to" line)
              (string-prefix-p "Binary files" line))
          nil)
         ;; Content line (part of current hunk)
         (current-header
          (push line current-lines))))
      ;; Don't forget the last hunk
      (when current-header
        (push (cons current-header (nreverse current-lines)) hunks))
      (nreverse hunks))))

(defun approve-diff--face-for-line (line)
  "Return the appropriate face for diff LINE."
  (cond
   ((string-prefix-p "+" line) 'magit-diff-added)
   ((string-prefix-p "-" line) 'magit-diff-removed)
   (t 'magit-diff-context)))

(defun approve-diff--line-side (line)
  "Return the diff side for LINE.
Returns \"LEFT\" for removed lines, \"RIGHT\" for added/context lines."
  (if (string-prefix-p "-" line)
      "LEFT"
    "RIGHT"))

(defun approve-diff--format-comment-indicator (count)
  "Format a comment indicator for COUNT comments."
  (when (and count (> count 0))
    (propertize (format " %s%d" approve-diff-comment-indicator count)
                'font-lock-face 'approve-file-comment-indicator-face)))

;;; Thread Rendering (reusing from approve-ui-files.el patterns)

(defun approve-diff--sort-comments-chronologically (comments)
  "Sort COMMENTS chronologically by createdAt timestamp."
  (sort (copy-sequence comments)
        (lambda (a b)
          (let ((date-a (alist-get 'createdAt a))
                (date-b (alist-get 'createdAt b)))
            (cond
             ((null date-a) nil)
             ((null date-b) t)
             (t (string< date-a date-b)))))))

(defun approve-diff--insert-thread (thread)
  "Insert a review THREAD below a diff line."
  (let* ((id (alist-get 'id thread))
         (is-resolved (alist-get 'isResolved thread))
         (is-outdated (alist-get 'isOutdated thread))
         (comments-data (alist-get 'comments thread))
         (comments (approve-model-get-nodes comments-data))
         (sorted-comments (approve-diff--sort-comments-chronologically comments))
         (comment-count (length sorted-comments))
         (indent-str (make-string approve-diff-comment-indent ?\s)))
    (magit-insert-section (review-thread id)
      ;; Thread status header (if resolved/outdated)
      (when (or is-outdated is-resolved)
        (magit-insert-heading
          (concat
           indent-str
           (when is-outdated
             (propertize "(outdated)" 'face 'warning))
           (when (and is-outdated is-resolved)
             " ")
           (when is-resolved
             (propertize "(resolved)" 'face 'success)))))
      ;; Insert comments
      (let ((index 0))
        (dolist (comment sorted-comments)
          (approve-diff--insert-thread-comment comment
                                               (= index (1- comment-count)))
          (cl-incf index))))))

(defun approve-diff--insert-thread-comment (comment last-p)
  "Insert a single COMMENT from a review thread.
LAST-P is non-nil if this is the last comment in the thread."
  (let* ((id (alist-get 'id comment))
         (author (alist-get 'author comment))
         (body-html (alist-get 'bodyHTML comment))
         (created-at (alist-get 'createdAt comment))
         (state (alist-get 'state comment))
         (reaction-groups (alist-get 'reactionGroups comment))
         (edited-info (approve-comment--make-edited-info comment))
         (display-state (when (equal state "PENDING") state))
         (indent-str (make-string approve-diff-comment-indent ?\s)))
    (magit-insert-section (review-comment id)
      (magit-insert-heading
        (concat indent-str
                (approve-comment--format-header author created-at
                                                display-state edited-info)))
      ;; Comment body
      (approve-comment--insert-body body-html
                                    (+ approve-comment-body-indent
                                       approve-diff-comment-indent))
      ;; Reactions
      (when-let ((reactions-str (approve-comment--format-reactions reaction-groups)))
        (insert (make-string (+ approve-comment-body-indent
                                approve-diff-comment-indent) ?\s)
                reactions-str
                "\n"))
      ;; Spacing between comments (but not after the last)
      (unless last-p
        (insert indent-str "│\n")))))

;;; Line Rendering with Comments

(defun approve-diff--insert-line-with-comments (line line-threads)
  "Insert a diff LINE that has LINE-THREADS as an expandable section."
  (let* ((face (approve-diff--face-for-line line))
         (comment-count (approve-diff--count-line-comments line-threads))
         (indicator (approve-diff--format-comment-indicator comment-count))
         (line-id (list 'diff-line line (random))))  ; Unique ID for section
    (magit-insert-section (diff-line line-id t)  ; Start expanded to show indicator
      (magit-insert-heading
        (concat (propertize line 'font-lock-face face)
                indicator
                "\n"))
      ;; Insert threads
      (dolist (thread line-threads)
        (approve-diff--insert-thread thread)))))

(defun approve-diff--insert-line-plain (line)
  "Insert a plain diff LINE without comments."
  (let ((face (approve-diff--face-for-line line)))
    (insert (propertize (concat line "\n") 'font-lock-face face))))

;;; Hunk Rendering

(defun approve-diff--insert-hunk-content (lines path from-line to-line thread-index)
  "Insert hunk LINES with proper faces and inline comments.
PATH is the file path for looking up comments.
FROM-LINE is the starting line number on the left (old) side.
TO-LINE is the starting line number on the right (new) side.
THREAD-INDEX is the hash table mapping (path line side) to threads."
  (let ((left-line from-line)
        (right-line to-line))
    (dolist (line lines)
      (let* ((side (approve-diff--line-side line))
             (effective-line (if (equal side "LEFT") left-line right-line))
             (threads (approve-diff--get-line-threads
                       thread-index path effective-line side)))
        ;; Insert the line (with or without comments)
        (if threads
            (approve-diff--insert-line-with-comments line threads)
          (approve-diff--insert-line-plain line))
        ;; Update line counters
        (cond
         ((string-prefix-p "+" line)
          ;; Added line: only right side advances
          (cl-incf right-line))
         ((string-prefix-p "-" line)
          ;; Removed line: only left side advances
          (cl-incf left-line))
         (t
          ;; Context line: both sides advance
          (cl-incf left-line)
          (cl-incf right-line)))))))

(defun approve-diff--insert-hunk (header content path thread-index)
  "Insert a hunk section with HEADER and CONTENT lines.
PATH is the file path for looking up comments.
THREAD-INDEX is used for looking up line comments."
  (let ((parsed (approve-diff--parse-hunk-header header)))
    (magit-insert-section (hunk (list header parsed))
      (magit-insert-heading
        (propertize (concat header "\n") 'font-lock-face 'magit-diff-hunk-heading))
      (when parsed
        (approve-diff--insert-hunk-content
         content
         path
         (plist-get parsed :from-start)
         (plist-get parsed :to-start)
         thread-index)))))

(defun approve-diff--insert-file-diff (file thread-index)
  "Insert a file section for FILE from the diff data.
FILE is an alist from GitHub's compare API.
THREAD-INDEX is used for looking up line comments."
  (let* ((filename (alist-get 'filename file))
         (status (alist-get 'status file))
         (patch (alist-get 'patch file))
         (previous-filename (alist-get 'previous_filename file))
         (status-str (approve-diff--file-status-string status))
         (hunks (approve-diff--split-patch-into-hunks patch)))
    ;; Use file as section type with filename as value
    ;; Third arg is whether to start hidden (hide if no hunks/binary file)
    (magit-insert-section (file filename (null hunks))
      ;; File heading
      (magit-insert-heading
        (propertize
         (concat
          (when status-str (format "%-11s" status-str))
          (if (and previous-filename (not (string= previous-filename filename)))
              (format "%s -> %s" previous-filename filename)
            filename))
         'font-lock-face 'magit-diff-file-heading))
      ;; Insert hunks
      (when hunks
        (dolist (hunk hunks)
          (approve-diff--insert-hunk (car hunk) (cdr hunk)
                                     filename thread-index))))))

;;; Public Section Function

(defun approve-insert-diff-section ()
  "Insert the diff section in the PR review buffer.
Shows file diffs with expandable hunks, similar to magit commit view.
Lines with review comments are shown as expandable sections."
  (when-let ((diff-data (approve-model-root 'diff)))
    (let ((files (alist-get 'files diff-data))
          (thread-index (approve-diff--build-line-threads-index)))
      (when (and files (> (length files) 0))
        (insert "\n")
        (magit-insert-section (diff nil)
          (magit-insert-heading 2 "Diff")
          (dolist (file (append files nil))  ; Convert vector to list if needed
            (approve-diff--insert-file-diff file thread-index)))))))

(provide 'approve-ui-diff)
;;; approve-ui-diff.el ends here
