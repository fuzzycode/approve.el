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
;;
;; The rendering mimics magit's commit diff display with:
;; - File sections that show filename and status
;; - Collapsible hunk sections within each file
;; - Proper diff highlighting (added/removed/context lines)

;;; Code:

(require 'cl-lib)
(require 'magit-section)

(require 'approve-model)
(require 'approve-ui-faces)

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

(defun approve-diff--insert-hunk-content (lines)
  "Insert hunk LINES with proper faces."
  (dolist (line lines)
    (let ((face (approve-diff--face-for-line line)))
      (insert (propertize (concat line "\n") 'font-lock-face face)))))

(defun approve-diff--insert-hunk (header content)
  "Insert a hunk section with HEADER and CONTENT lines."
  (let ((parsed (approve-diff--parse-hunk-header header)))
    (magit-insert-section (hunk (list header parsed))
      (magit-insert-heading
        (propertize (concat header "\n") 'font-lock-face 'magit-diff-hunk-heading))
      (approve-diff--insert-hunk-content content))))

(defun approve-diff--insert-file-diff (file)
  "Insert a file section for FILE from the diff data.
FILE is an alist from GitHub's compare API."
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
          (approve-diff--insert-hunk (car hunk) (cdr hunk)))))))

;;; Public Section Function

(defun approve-insert-diff-section ()
  "Insert the diff section in the PR review buffer.
Shows file diffs with expandable hunks, similar to magit commit view."
  (when-let ((diff-data (approve-model-root 'diff)))
    (let ((files (alist-get 'files diff-data)))
      (when (and files (> (length files) 0))
        (insert "\n")
        (magit-insert-section (diff nil)
          (magit-insert-heading 2 "Diff")
          (dolist (file (append files nil))  ; Convert vector to list if needed
            (approve-diff--insert-file-diff file)))))))

(provide 'approve-ui-diff)
;;; approve-ui-diff.el ends here
