;;; approve-ui-html.el --- HTML rendering for Approve -*- lexical-binding: t; -*-

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

;; This file provides HTML rendering functionality for Approve.
;; It uses `shr' (Simple HTML Renderer) with custom tag handlers
;; to provide proper rendering of GitHub-generated HTML including:
;;
;; - Code blocks with syntax highlighting (using GitHub's PrismJS classes)
;; - Inline code
;; - Blockquotes with visual borders
;; - Task lists with checkboxes
;; - Ordered and unordered lists
;; - Tables with diff/suggested changes support
;;
;; The module is designed to be used anywhere GitHub HTML content needs
;; to be rendered, including PR descriptions, comments, and reviews.

;;; Code:

(require 'shr)
(require 'dom)

(require 'approve-ui-faces)
(require 'approve-ui-helpers)

;;; Customization

(defgroup approve-html nil
  "HTML rendering settings for Approve."
  :group 'approve
  :prefix "approve-html-")

(defcustom approve-html-fill-column 80
  "Column number to wrap HTML content at.
This controls how wide rendered text will be before wrapping."
  :group 'approve-html
  :type 'integer)

(defcustom approve-html-blockquote-prefix "▌ "
  "Prefix string for blockquote lines.
This is inserted at the beginning of each line in a blockquote."
  :group 'approve-html
  :type 'string)

;;; DOM Utilities

(defun approve-html--dom-classes (dom)
  "Extract all CSS classes from DOM element as a list of strings.
Returns nil if there are no classes."
  (when-let* ((class-attr (dom-attr dom 'class))
              (class-str (and (stringp class-attr) (string-trim class-attr))))
    (unless (string-empty-p class-str)
      (split-string class-str " " t))))

(defun approve-html--dom-has-class-p (dom class)
  "Return non-nil if DOM element has the CSS CLASS."
  (member class (approve-html--dom-classes dom)))

;;; Font Application

(defun approve-html--add-font (start end face)
  "Apply FACE to text between START and END.
Unlike `shr-add-font', this sets both `face' and `font-lock-face'
properties to ensure proper display in `magit-section-mode' buffers
where syntactic font-lock may be active."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
        (skip-chars-forward " "))
      (let ((line-end (min (line-end-position) end)))
        (add-face-text-property (point) line-end face t)
        (font-lock-append-text-property (point) line-end 'font-lock-face face))
      (if (< (line-end-position) end)
          (forward-line 1)
        (goto-char end)))))

;;; Custom Tag Renderers

(defun approve-html--tag-blockquote (dom)
  "Custom renderer for <blockquote> elements in DOM.
Adds a visual border prefix and applies blockquote face."
  (shr-ensure-newline)
  (let ((start (point)))
    (let ((shr-indentation (+ shr-indentation 2)))
      (shr-generic dom))
    (shr-ensure-newline)
    ;; Apply face to the entire blockquote content
    (approve-html--add-font start (point) 'approve-html-blockquote-face)
    ;; Add prefix to each line within the blockquote
    (save-excursion
      (goto-char start)
      (while (< (point) (point-max))
        (unless (eolp)
          (let ((prefix (approve-ui-propertize-face approve-html-blockquote-prefix
                                                    'approve-html-blockquote-border-face)))
            (insert prefix)))
        (forward-line 1)
        (when (>= (point) (point-max))
          (goto-char (point-max)))))))

(defun approve-html--tag-code (dom)
  "Custom renderer for inline <code> elements in DOM.
Applies inline code face to the content."
  (let ((start (point)))
    (shr-generic dom)
    (approve-html--add-font start (point) 'approve-html-inline-code-face)))

(defun approve-html--tag-pre (dom)
  "Custom renderer for <pre> elements in DOM with syntax highlighting.
Handles GitHub's syntax highlighting classes for proper colorization."
  (let ((start (point)))
    (shr-ensure-paragraph)
    (approve-html--process-pre-content dom)
    (shr-ensure-paragraph)
    (approve-html--add-font start (point) 'approve-html-code-block-face)))

(defun approve-html--process-pre-content (dom)
  "Process and insert the content of DOM.
DOM is expected to be a <pre> element containing code.
Handles nested spans with syntax highlighting classes."
  (dolist (child (dom-children dom))
    (cond
     ((stringp child)
      (insert child))
     ((eq (dom-tag child) 'span)
      (shr-generic child))
     (t
      (shr-generic child)))))

(defun approve-html--tag-span (dom)
  "Render <span> tags in DOM with GitHub syntax highlighting support.
Maps GitHub's PrismJS classes (pl-*) to Emacs font-lock faces."
  (let ((class (dom-attr dom 'class)))
    (cond
     ((and class (string-match "pl-\\([a-z0-9]+\\)" class))
      (let* ((type (match-string 1 class))
             (start (point))
             (face (approve-html--syntax-face type)))
        (insert (dom-text dom))
        (when face
          (approve-html--add-font start (point) face))))
     (t
      (shr-generic dom)))))

(defun approve-html--syntax-face (type)
  "Return the appropriate face for syntax highlighting TYPE.
TYPE is a GitHub PrismJS class suffix (after pl-)."
  (pcase type
    ;; Comments
    ("c" 'font-lock-comment-face)
    ("c1" 'font-lock-comment-face)
    ;; Keywords
    ("k" 'font-lock-keyword-face)
    ("kd" 'font-lock-keyword-face)  ; Keyword declaration
    ("kr" 'font-lock-keyword-face)  ; Keyword reserved
    ;; Strings
    ("s" 'font-lock-string-face)
    ("s1" 'font-lock-string-face)
    ("s2" 'font-lock-string-face)
    ("sr" 'font-lock-string-face)   ; String regex
    ;; Variables
    ("v" 'font-lock-variable-name-face)
    ("vi" 'font-lock-variable-name-face)
    ("smi" 'font-lock-variable-name-face)
    ;; Functions
    ("e" 'font-lock-function-name-face)   ; Entity
    ("en" 'font-lock-function-name-face)  ; Entity name
    ("m" 'font-lock-function-name-face)   ; Method
    ;; Types
    ("t" 'font-lock-type-face)
    ("nc" 'font-lock-type-face)  ; Name class
    ("nn" 'font-lock-type-face)  ; Name namespace
    ;; Constants and numbers
    ("n" 'font-lock-constant-face)
    ("mi" 'font-lock-constant-face)  ; Number integer
    ("mf" 'font-lock-constant-face)  ; Number float
    ("mb" 'font-lock-constant-face)  ; Number binary
    ("mh" 'font-lock-constant-face)  ; Number hex
    ("il" 'font-lock-constant-face)  ; Integer literal
    ;; Operators and punctuation
    ("o" 'font-lock-builtin-face)
    ("p" nil)   ; Punctuation - no special face
    ("pds" nil) ; Pseudo-data structure
    ;; Preprocessor
    ("pp" 'font-lock-preprocessor-face)
    ;; Built-ins
    ("bi" 'font-lock-builtin-face)
    ("nb" 'font-lock-builtin-face)  ; Name builtin
    ;; Others
    ("l" 'font-lock-warning-face)  ; Literal
    (_ nil)))

(defun approve-html--tag-li (dom)
  "Custom renderer for <li> tags in DOM, supporting GitHub task lists.
For regular list items, delegates to shr's default handling.
For task list items, renders checkboxes with appropriate faces."
  (let* ((checkbox (dom-by-class dom "task-list-item-checkbox"))
         (task-status (and checkbox (dom-attr checkbox 'checked))))
    (cond
     ;; Checked task list item
     (task-status
      (shr-ensure-newline)
      (insert (approve-ui-propertize-face "[✓] " 'approve-html-task-checked-face))
      (shr-generic dom)
      (unless (bolp)
        (insert "\n")))
     ;; Unchecked task list item
     (checkbox
      (shr-ensure-newline)
      (insert (approve-ui-propertize-face "[ ] " 'approve-html-task-unchecked-face))
      (shr-generic dom)
      (unless (bolp)
        (insert "\n")))
     ;; Regular list item - use default shr handling
     (t
      (shr-tag-li dom)))))

(defun approve-html--tag-td (dom)
  "Render <td> tags in DOM with special handling for diff lines.
Used for GitHub's suggested changes feature which displays diffs in tables."
  (cond
   ((approve-html--dom-has-class-p dom "blob-code-deletion")
    (let ((start (point)))
      (insert "- ")
      (shr-generic dom)
      (approve-html--add-font start (point) 'approve-diff-deletion-face)))
   ((approve-html--dom-has-class-p dom "blob-code-addition")
    (let ((start (point)))
      (insert "+ ")
      (shr-generic dom)
      (approve-html--add-font start (point) 'approve-diff-addition-face)))
   ((approve-html--dom-has-class-p dom "blob-code-context")
    (let ((start (point)))
      (insert "  ")
      (shr-generic dom)
      (approve-html--add-font start (point) 'approve-diff-context-face)))
   (t
    (shr-generic dom))))

(defun approve-html--tag-p (dom)
  "Render <p> tags in DOM, ensuring proper paragraph separation."
  (shr-generic dom)
  (shr-ensure-newline))

;;; Public API

(defun approve-html-render (html)
  "Render HTML string and return the result as a string.
The rendering handles GitHub-specific HTML features including:
- Syntax-highlighted code blocks
- Task lists with checkboxes
- Blockquotes with visual borders
- Suggested changes (inline diffs)

Note: Text properties (faces) are preserved in the returned string."
  (when (and html (not (string-empty-p (string-trim html))))
    (with-temp-buffer
      (approve-html--insert-raw html 0)
      ;; Return the buffer string with properties
      (buffer-string))))

(defun approve-html--insert-raw (html indent)
  "Insert rendered HTML directly at point with INDENT.
This is the core rendering function that inserts into the current buffer."
  (let* ((dom (with-temp-buffer
                (insert (string-trim html))
                (libxml-parse-html-region (point-min) (point-max))))
         (shr-width approve-html-fill-column)
         (shr-use-fonts nil)
         (shr-indentation indent)
         (shr-bullet "• ")
         (shr-external-rendering-functions
          '((blockquote . approve-html--tag-blockquote)
            (pre . approve-html--tag-pre)
            (span . approve-html--tag-span)
            (td . approve-html--tag-td)
            (code . approve-html--tag-code)
            (p . approve-html--tag-p)
            (li . approve-html--tag-li))))
    (shr-insert-document dom))
  ;; Clean up excessive whitespace
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))))

(defun approve-html-insert (html &optional indent)
  "Insert rendered HTML at point with optional INDENT.
INDENT is the number of spaces for indentation.
If HTML is nil or empty, does nothing.

This function inserts directly into the current buffer, preserving
all text properties including faces."
  (when (and html (not (string-empty-p (string-trim html))))
    (approve-html--insert-raw html (or indent 0))))

(provide 'approve-ui-html)
;;; approve-ui-html.el ends here
