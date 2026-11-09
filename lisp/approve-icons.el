;;; approve-icons.el --- Icons of Approve -*- lexical-binding: t; -*-

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

;; `approve-icon' turns a semantic name, such as `approved' or
;; `check-failure', into the text to draw (docs/decisions.org, D16 and
;; D26; docs/ui.org, "Icons").  Every icon Approve shows goes through
;; it.
;;
;; The glyphs are Codicons from nerd-icons (`approve-icons').  When
;; `approve-use-nerd-icons' is nil, or a graphical frame lacks the
;; nerd-icons font, each icon falls back to a Unicode character
;; (`approve-icon-fallbacks').  Both tables are user options, so any
;; icon can be swapped.
;;
;; Layer rules: this is the renderer layer.  It returns strings; it
;; never reads the store or calls the network.

;;; Code:

(require 'nerd-icons)
(require 'approve-faces)

(defvar approve-icon--cache (make-hash-table :test #'equal)
  "Icons made so far, by (NAME FACE NERD), for this session.")

(defvar approve-icon--font-found 'unknown
  "Whether a graphical frame has the nerd-icons font: t, nil or `unknown'.")

(defun approve-icon-clear-cache ()
  "Forget the icons made so far, and whether the font was found.
Changing an icon option through Customize or `setopt' does this."
  (clrhash approve-icon--cache)
  (setq approve-icon--font-found 'unknown))

(defun approve-icon--set (symbol value)
  "Set the icon option SYMBOL to VALUE and clear the icon cache."
  (set-default-toplevel-value symbol value)
  (approve-icon-clear-cache))

(defcustom approve-use-nerd-icons t
  "Whether icons are nerd-icons glyphs.
When non-nil, `approve-icon' uses the Codicons in `approve-icons',
unless a graphical frame lacks the font `nerd-icons-font-family';
a terminal is trusted to have it.  When nil, it uses the Unicode
characters in `approve-icon-fallbacks'.  Without a Nerd Font:

  (setopt approve-use-nerd-icons nil)"
  :type 'boolean
  :set #'approve-icon--set
  :group 'approve-faces)

(defcustom approve-icons
  '((open . "nf-cod-git_pull_request")
    (draft . "nf-cod-git_pull_request_draft")
    (merged . "nf-cod-git_merge")
    (closed . "nf-cod-git_pull_request_closed")
    (approved . "nf-cod-pass")
    (changes-requested . "nf-cod-request_changes")
    (commented . "nf-cod-comment")
    (requested . "nf-cod-watch")
    (viewed . "nf-cod-eye")
    (unviewed . "nf-cod-eye_closed")
    (dismissed . "nf-cod-history")
    (check-success . "nf-cod-pass_filled")
    (check-failure . "nf-cod-error")
    (check-pending . "nf-cod-loading")
    (pending-comment . "nf-cod-comment_draft")
    (comment . "nf-cod-comment_discussion")
    (label . "nf-cod-tag")
    (push . "nf-cod-repo_push")
    (commit . "nf-cod-git_commit")
    (issue . "nf-cod-issues")
    (milestone . "nf-cod-milestone")
    (person . "nf-cod-account")
    (bell . "nf-cod-bell")
    (lock . "nf-cod-lock")
    (external . "nf-cod-link_external")
    (loading . "nf-cod-sync")
    (error . "nf-cod-warning"))
  "Glyph of each icon: semantic name → nerd-icons Codicon name.
Names are those of `nerd-icons-codicon' (nf-cod-…), as listed by
`nerd-icons-insert'.  To draw approvals with a thumbs-up:

  (setf (alist-get \\='approved approve-icons) \"nf-cod-thumbsup\")
  (approve-icon-clear-cache)"
  :type '(alist :key-type symbol :value-type string)
  :set #'approve-icon--set
  :group 'approve-faces)

(defcustom approve-icon-fallbacks
  '((open . "●")
    (draft . "◌")
    (merged . "⮌")
    (closed . "⊘")
    (approved . "✓")
    (changes-requested . "✗")
    (commented . "💬")
    (requested . "○")
    (viewed . "✓")
    (unviewed . "·")
    (dismissed . "↻")
    (check-success . "✓")
    (check-failure . "✗")
    (check-pending . "◐")
    (pending-comment . "✎")
    (comment . "💬")
    (label . "🏷")
    (push . "↑")
    (commit . "●")
    (issue . "◉")
    (milestone . "⚑")
    (person . "👤")
    (bell . "●")
    (lock . "🔒")
    (external . "↗")
    (loading . "⟳")
    (error . "✗"))
  "Unicode text of each icon when nerd-icons are not used.
See `approve-use-nerd-icons'.  For example, a plainer comment icon:

  (setf (alist-get \\='comment approve-icon-fallbacks) \"#\")
  (approve-icon-clear-cache)"
  :type '(alist :key-type symbol :value-type string)
  :set #'approve-icon--set
  :group 'approve-faces)

(defun approve-icon-nerd-p ()
  "Return non-nil if icons are drawn with nerd-icons now.
That is when `approve-use-nerd-icons' is non-nil and, in a graphical
frame, the font `nerd-icons-font-family' exists (checked once)."
  (and approve-use-nerd-icons
       (or (not (display-graphic-p))
           (progn
             (when (eq approve-icon--font-found 'unknown)
               (setq approve-icon--font-found
                     (and (find-font (font-spec :family nerd-icons-font-family))
                          t)))
             approve-icon--font-found))))

(defun approve-icon--make (name face nerd)
  "Return the icon NAME with FACE, a glyph when NERD is non-nil.
Fall back to the Unicode text when the glyph is unknown, and to
\"?\" when NAME has no fallback either."
  (let ((glyph (and nerd
                    (when-let* ((code (alist-get name approve-icons)))
                      (condition-case nil
                          (nerd-icons-codicon code)
                        (error nil))))))
    (cond
     (glyph (if face (approve-face-add glyph face) glyph))
     (t (approve-face (or (alist-get name approve-icon-fallbacks) "?")
                      face)))))

(defun approve-icon (name &optional face)
  "Return the icon NAME as a string, with FACE if non-nil.
NAME is a semantic name from `approve-icons', such as `approved'.
The string is a Codicon glyph or a Unicode fallback, see
`approve-use-nerd-icons'.  Its width can be 1 or 2 columns: align
with `string-width'.  Icons are cached; do not modify the string.

Example: (insert (approve-icon \\='approved \\='approve-approved) \" bob\")"
  (let* ((nerd (approve-icon-nerd-p))
         (key (list name face nerd)))
    (or (gethash key approve-icon--cache)
        (puthash key (approve-icon--make name face nerd) approve-icon--cache))))

(provide 'approve-icons)

;;; approve-icons.el ends here
