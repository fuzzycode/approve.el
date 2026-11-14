;;; approve-faces.el --- Faces of Approve -*- lexical-binding: t; -*-

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

;; Every face Approve uses (docs/decisions.org, D16; docs/ui.org,
;; "Faces").  Each one inherits from a Magit or built-in face, so any
;; theme styles Approve out of the box, and each can be customized in
;; the group `approve-faces'.
;;
;; Text gets its face with `approve-face', which sets both `face' and
;; `font-lock-face' as Magit does, so a buffer with font-lock on keeps
;; it.  Labels take their colour from GitHub: `approve-label-face'
;; returns a face for a label colour, with a readable foreground.
;;
;; Layer rules: this is the renderer layer.  It defines faces and pure
;; helpers; it never reads the store or calls the network.  No other
;; file defines a face.

;;; Code:

(require 'color)
(require 'seq)
(require 'magit-section)
;; The faces inherit from magit's, which are in magit.el and the
;; libraries it loads (magit-diff, magit-log).  Without them they would
;; have no colour until magit itself is loaded.
(require 'magit)

(defgroup approve-faces nil
  "Faces used by Approve."
  :group 'approve
  :group 'faces
  :prefix "approve-")

;;; Headings and text

(defface approve-title
  '((t :inherit magit-section-heading :weight bold))
  "Face of the pull request title.
`approve-title-height' makes it larger."
  :group 'approve-faces)

(defface approve-heading
  '((t :inherit magit-section-heading))
  "Face of section headings, such as Description and Conversation."
  :group 'approve-faces)

(defface approve-secondary-heading
  '((t :inherit magit-section-secondary-heading))
  "Face of smaller headings, such as the names of metadata rows."
  :group 'approve-faces)

(defface approve-dim
  '((t :inherit shadow))
  "Face of hints, dates, separators and things that are done."
  :group 'approve-faces)

(defface approve-count
  '((t :inherit magit-section-child-count))
  "Face of counts after headings, such as (4)."
  :group 'approve-faces)

(defface approve-empty
  '((t :inherit shadow :slant italic))
  "Face of placeholders, such as \"No description provided.\"."
  :group 'approve-faces)

(defface approve-loading
  '((t :inherit shadow :slant italic))
  "Face of \"Loading…\" placeholders."
  :group 'approve-faces)

(defface approve-error
  '((t :inherit error))
  "Face of errors shown in a section, such as a failed request."
  :group 'approve-faces)

(defface approve-hint
  '((t :inherit shadow))
  "Face of the text of key hints, such as \"reply\" in \"r reply\"."
  :group 'approve-faces)

(defface approve-hint-key
  '((t :inherit help-key-binding))
  "Face of the key of key hints, such as \"r\" in \"r reply\"."
  :group 'approve-faces)

(defface approve-link
  '((t :inherit link))
  "Face of links in rendered text."
  :group 'approve-faces)

(defface approve-inflight
  '((t :inherit shadow :slant italic))
  "Face of things waiting for GitHub to confirm a change."
  :group 'approve-faces)

(defface approve-target
  '((t :inherit region :extend t))
  "Face of the lines a comment is being written on."
  :group 'approve-faces)

(defface approve-input-prompt
  '((t :inherit bold))
  "Face of what an input buffer is for, such as \"Comment on a.el:42\".
It starts the header line of the buffer."
  :group 'approve-faces)

(defface approve-input-context
  '((t :inherit font-lock-keyword-face))
  "Face of where the text of an input buffer goes, such as \"pending review\".
It follows the prompt in the header line."
  :group 'approve-faces)

;;; Things

(defface approve-user
  '((t :inherit magit-log-author))
  "Face of logins and names of people."
  :group 'approve-faces)

(defface approve-branch
  '((t :inherit magit-branch-remote))
  "Face of branch names."
  :group 'approve-faces)

(defface approve-hash
  '((t :inherit magit-hash))
  "Face of commit hashes."
  :group 'approve-faces)

(defface approve-path
  '((t :inherit magit-filename))
  "Face of file paths in lists."
  :group 'approve-faces)

(defface approve-date
  '((t :inherit shadow))
  "Face of dates and relative times."
  :group 'approve-faces)

(defface approve-label
  '((t :weight semi-bold))
  "Face every label inherits; the colours come from GitHub.
See `approve-label-face'."
  :group 'approve-faces)

(defface approve-reaction
  '((t :inherit default))
  "Face of reactions, such as 👍 3."
  :group 'approve-faces)

(defface approve-reaction-viewer
  '((t :inherit (bold approve-reaction)))
  "Face of reactions the viewer gave."
  :group 'approve-faces)

(defface approve-edited
  '((t :inherit shadow :slant italic))
  "Face of the \"edited\" marker on comments."
  :group 'approve-faces)

;;; States

(defface approve-state-open
  '((t :inherit success))
  "Face of the Open state badge."
  :group 'approve-faces)

(defface approve-state-closed
  '((t :inherit error))
  "Face of the Closed state badge."
  :group 'approve-faces)

(defface approve-state-merged
  '((t :inherit magit-tag))
  "Face of the Merged state badge."
  :group 'approve-faces)

(defface approve-state-draft
  '((t :inherit shadow))
  "Face of the Draft state badge."
  :group 'approve-faces)

(defface approve-approved
  '((t :inherit success))
  "Face of approvals."
  :group 'approve-faces)

(defface approve-changes-requested
  '((t :inherit error))
  "Face of requested changes."
  :group 'approve-faces)

(defface approve-commented
  '((t :inherit default))
  "Face of reviews that only commented."
  :group 'approve-faces)

(defface approve-requested
  '((t :inherit shadow))
  "Face of reviewers who were asked and have not reviewed yet."
  :group 'approve-faces)

(defface approve-pending
  '((t :inherit warning))
  "Face of the \"Pending\" label and icon of the viewer's pending comments.
Only the label is coloured; the comment itself has no background."
  :group 'approve-faces)

(defface approve-check-success
  '((t :inherit success))
  "Face of passed checks."
  :group 'approve-faces)

(defface approve-check-failure
  '((t :inherit error))
  "Face of failed checks."
  :group 'approve-faces)

(defface approve-check-pending
  '((t :inherit warning))
  "Face of checks that are queued or running."
  :group 'approve-faces)

(defface approve-viewed
  '((t :inherit shadow))
  "Face of files the viewer marked as viewed."
  :group 'approve-faces)

(defface approve-unviewed
  '((t :inherit shadow))
  "Face of the mark of files not viewed yet."
  :group 'approve-faces)

(defface approve-dismissed
  '((t :inherit warning))
  "Face of files viewed before they changed again."
  :group 'approve-faces)

(defface approve-file-added
  '((t :inherit magit-diffstat-added))
  "Face of the icon of an added file."
  :group 'approve-faces)

(defface approve-file-modified
  '((t :inherit shadow))
  "Face of the icon of a modified file, the usual change.
A change of mode or type uses it too."
  :group 'approve-faces)

(defface approve-file-deleted
  '((t :inherit magit-diffstat-removed))
  "Face of the icon of a deleted file."
  :group 'approve-faces)

(defface approve-file-moved
  '((t :inherit font-lock-keyword-face))
  "Face of the icon of a renamed or copied file.
It inherits a colour that differs from the added and deleted ones
in most themes, which magit's branch faces don't."
  :group 'approve-faces)

(defface approve-new
  '((t :inherit warning))
  "Face of things that are new since the viewer's last review.
A colour, not a background, like `approve-dismissed': both say
what changed since the viewer last looked."
  :group 'approve-faces)

;;; Threads and the conversation

(defface approve-thread-border
  '((t :inherit shadow))
  "Face of the box lines around threads and of the timeline."
  :group 'approve-faces)

(defface approve-resolved
  '((t :inherit shadow))
  "Face of resolved threads."
  :group 'approve-faces)

(defface approve-outdated
  '((t :inherit warning))
  "Face of the outdated marker of threads."
  :group 'approve-faces)

;;; Diffs

(defface approve-diff-file-heading
  '((t :inherit magit-diff-file-heading))
  "Face of file headings in the diff."
  :group 'approve-faces)

(defface approve-diff-hunk-heading
  '((t :inherit magit-diff-hunk-heading))
  "Face of hunk headings (@@ lines)."
  :group 'approve-faces)

(defface approve-diff-added
  '((t :inherit magit-diff-added))
  "Face of added lines."
  :group 'approve-faces)

(defface approve-diff-removed
  '((t :inherit magit-diff-removed))
  "Face of removed lines."
  :group 'approve-faces)

(defface approve-diff-context
  '((t :inherit magit-diff-context))
  "Face of context lines."
  :group 'approve-faces)

(defface approve-diff-bubble
  '((t :inherit default))
  "Face of the speech bubble on a diff line with threads.
It gives the bubble its colour; the line gives its background, so
a bubble on a removed line is not red."
  :group 'approve-faces)

(defface approve-diff-refine-added
  '((t :inherit diff-refine-added))
  "Face of the words that changed in an added line."
  :group 'approve-faces)

(defface approve-diff-refine-removed
  '((t :inherit diff-refine-removed))
  "Face of the words that changed in a removed line."
  :group 'approve-faces)

(defface approve-line-number
  '((t :inherit line-number))
  "Face of line numbers in the diff."
  :group 'approve-faces)

(defface approve-diffstat-added
  '((t :inherit magit-diffstat-added))
  "Face of added counts and bars in the file list."
  :group 'approve-faces)

(defface approve-diffstat-removed
  '((t :inherit magit-diffstat-removed))
  "Face of removed counts and bars in the file list."
  :group 'approve-faces)

;;; Rendered HTML

(defface approve-html-code
  '((t :inherit font-lock-constant-face))
  "Face of inline code in comments and descriptions."
  :group 'approve-faces)

(defface approve-html-code-block
  '((t :inherit magit-section-highlight :extend t))
  "Face of code blocks in comments and descriptions."
  :group 'approve-faces)

(defface approve-html-blockquote
  '((t :inherit shadow :slant italic))
  "Face of quoted text."
  :group 'approve-faces)

(defface approve-html-blockquote-border
  '((t :inherit shadow))
  "Face of the bar left of quoted text."
  :group 'approve-faces)

(defface approve-html-strike
  '((t :strike-through t))
  "Face of struck-through text."
  :group 'approve-faces)

(defface approve-html-task-done
  '((t :inherit success))
  "Face of the box of a done task."
  :group 'approve-faces)

(defface approve-html-task-open
  '((t :inherit shadow))
  "Face of the box of an open task."
  :group 'approve-faces)

(defface approve-html-summary
  '((t :inherit bold))
  "Face of the summary of a collapsible <details> block."
  :group 'approve-faces)

(defface approve-html-heading
  '((t :inherit bold))
  "Face of headings (h1 to h6) and alert titles in rendered text."
  :group 'approve-faces)

(defface approve-html-table-border
  '((t :inherit shadow))
  "Face of table borders."
  :group 'approve-faces)

(defface approve-html-table-header
  '((t :inherit bold))
  "Face of table header cells."
  :group 'approve-faces)

;;; Helpers

(defcustom approve-title-height 1.2
  "Height of the pull request title, relative to the default font.
Terminals ignore it.  For a title as tall as the rest:

  (setq approve-title-height 1.0)"
  :type 'number
  :group 'approve-faces)

(defun approve-face (string face)
  "Return STRING with FACE as its `face' and `font-lock-face'.
Setting both keeps the face whether or not font-lock is on, as
`magit--propertize-face' does.  A nil FACE returns STRING as it is.

Example: (insert (approve-face \"merged\" \\='approve-state-merged))"
  (if face
      (propertize string 'face face 'font-lock-face face)
    string))

(defun approve-face-add (string face)
  "Return a copy of STRING with FACE added under its faces.
Unlike `approve-face', faces STRING has already win over FACE; use
it to colour an icon or a part that has a face of its own."
  (let ((string (copy-sequence string))
        (pos 0))
    (add-face-text-property 0 (length string) face t string)
    ;; Mirror each run of `face' into `font-lock-face'.
    (while (< pos (length string))
      (let ((next (next-single-property-change pos 'face string
                                               (length string))))
        (put-text-property pos next 'font-lock-face
                           (get-text-property pos 'face string) string)
        (setq pos next)))
    string))

(defun approve-face-background (face)
  "Return a face holding only the background of FACE, or nil.
FACE is a face or a list of them; the first with a background, in
the current theme, gives it.  Nil when none has one.  Use it to put
text on the background of a line without taking its colours, such
as a bubble on a removed line, which would else be red.  It is the
theme's colour now: text drawn with it follows a theme change at
the next redraw.

Example: (approve-face-background \='approve-diff-removed)
         => (:background \"#4a3a58\")"
  (when-let* ((background (seq-some (lambda (face)
                                      (and (facep face) (face-background face nil t)))
                                    (ensure-list face))))
    (list :background background)))

(defun approve-face-region (start end face)
  "Add FACE under the faces of the text between START and END.
Like `add-face-text-property' with APPEND, but `font-lock-face' gets
the same faces as `face', so the text keeps them when font-lock
clears `face' (as it does in Magit buffers)."
  (when (< start end)
    (add-face-text-property start end face t)
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'face nil end)))
          (put-text-property pos next 'font-lock-face (get-text-property pos 'face))
          (setq pos next))))))

(defun approve--hex-color (color)
  "Return COLOR, a GitHub hex colour such as \"a2eeef\", as \"#a2eeef\".
Return nil unless COLOR is six hex digits, with or without a hash."
  (when (and (stringp color)
             (string-match (rx bos (? "#") (group (= 6 hex-digit)) eos) color))
    (concat "#" (downcase (match-string 1 color)))))

(defun approve-readable-foreground (background)
  "Return \"#000000\" or \"#ffffff\", whichever reads best on BACKGROUND.
BACKGROUND is a colour such as \"#a2eeef\".  The choice follows the
relative luminance of WCAG, as GitHub does for labels."
  (let* ((rgb (color-name-to-rgb background))
         (linear (mapcar (lambda (c)
                           (if (<= c 0.03928) (/ c 12.92)
                             (expt (/ (+ c 0.055) 1.055) 2.4)))
                         (or rgb '(1 1 1))))
         (luminance (+ (* 0.2126 (nth 0 linear))
                       (* 0.7152 (nth 1 linear))
                       (* 0.0722 (nth 2 linear)))))
    (if (> luminance 0.179) "#000000" "#ffffff")))

(defun approve-label-face (color)
  "Return the face of a label whose GitHub colour is COLOR.
COLOR is six hex digits, such as \"a2eeef\".  The face has COLOR as
background, a readable foreground, and inherits `approve-label'.
An invalid COLOR gives `approve-label' alone.

Example: (approve-face \" bug \" (approve-label-face \"d73a4a\"))"
  (if-let* ((background (approve--hex-color color)))
      (list :background background
            :foreground (approve-readable-foreground background)
            :inherit 'approve-label)
    'approve-label))

(provide 'approve-faces)

;;; approve-faces.el ends here
