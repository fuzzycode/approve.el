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
;; D31; docs/ui.org, "Icons").  Every icon Approve shows goes through
;; it.
;;
;; The glyphs are Material Design icons from nerd-icons, but for drafts,
;; which are Codicons' as in gh-dash (`approve-icons'); any other
;; nerd-icons glyph can take a place.  When
;; `approve-use-nerd-icons' is nil, or a graphical frame lacks the
;; nerd-icons font, each icon falls back to a Unicode character
;; (`approve-icon-fallbacks').  Both tables are user options, so any
;; icon can be swapped.
;;
;; Layer rules: this is the renderer layer.  It returns strings; it
;; never reads the store or calls the network.

;;; Code:

(require 'seq)
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
When non-nil, `approve-icon' uses the glyphs in `approve-icons',
unless a graphical frame lacks the font `nerd-icons-font-family';
a terminal is trusted to have it.  When nil, it uses the Unicode
characters in `approve-icon-fallbacks'.  Without a Nerd Font:

  (setopt approve-use-nerd-icons nil)"
  :type 'boolean
  :set #'approve-icon--set
  :group 'approve-faces)

(defcustom approve-icons
  '((open . "nf-md-source_pull")
    (draft . "nf-cod-git_pull_request_draft")
    (merged . "nf-md-source_merge")
    (closed . "nf-md-source_branch_remove")
    (approved . "nf-md-check_decagram")
    (changes-requested . "nf-md-alert_decagram")
    (commented . "nf-md-comment_text_outline")
    (requested . "nf-md-account_clock_outline")
    (viewed . "nf-md-checkbox_marked_outline")
    (unviewed . "nf-md-checkbox_blank_outline")
    (dismissed . "nf-md-update")
    (file-added . "nf-md-file_plus_outline")
    (file-modified . "nf-md-file_edit_outline")
    (file-deleted . "nf-md-file_remove_outline")
    (file-renamed . "nf-md-file_move_outline")
    (file-copied . "nf-md-file_multiple_outline")
    (file-changed . "nf-md-file_cog_outline")
    (check-success . "nf-md-check_circle")
    (check-failure . "nf-md-close_circle")
    (check-pending . "nf-md-progress_clock")
    (pending-comment . "nf-md-comment_edit_outline")
    (comment . "nf-md-comment_outline")
    (label . "nf-md-tag_outline")
    (push . "nf-md-arrow_up_bold_circle_outline")
    (commit . "nf-md-source_commit")
    (issue . "nf-md-record_circle_outline")
    (milestone . "nf-md-flag_outline")
    (person . "nf-md-account_outline")
    (bell . "nf-md-bell_outline")
    (lock . "nf-md-lock_outline")
    (external . "nf-md-open_in_new")
    (loading . "nf-md-loading")
    (error . "nf-md-alert_circle_outline")
    (reaction-thumbs-up . "nf-md-thumb_up_outline")
    (reaction-thumbs-down . "nf-md-thumb_down_outline")
    (reaction-laugh . "nf-md-emoticon_happy_outline")
    (reaction-hooray . "nf-md-party_popper")
    (reaction-confused . "nf-md-emoticon_confused_outline")
    (reaction-heart . "nf-md-heart_outline")
    (reaction-rocket . "nf-md-rocket_launch_outline")
    (reaction-eyes . "nf-md-eye_outline"))
  "Glyph of each icon: semantic name → nerd-icons name.
The defaults are Material Design icons (nf-md-…), but for drafts:
Codicons' pull request draft, as gh-dash draws them.  Any glyph
`nerd-icons-insert' lists will do, whatever its set: the prefix of
the name picks it (nf-md-, nf-cod-, nf-oct-, nf-fa-, …).  To draw
approvals with a thumbs-up:

  (setf (alist-get \\='approved approve-icons) \"nf-md-thumb_up_outline\")
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
    (file-added . "+")
    (file-modified . "~")
    (file-deleted . "−")
    (file-renamed . "→")
    (file-copied . "⧉")
    (file-changed . "⚙")
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
    (error . "✗")
    (reaction-thumbs-up . "👍")
    (reaction-thumbs-down . "👎")
    (reaction-laugh . "😄")
    (reaction-hooray . "🎉")
    (reaction-confused . "😕")
    (reaction-heart . "❤️")
    (reaction-rocket . "🚀")
    (reaction-eyes . "👀"))
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

(defconst approve-icon--families
  '(("nf-md-" . nerd-icons-mdicon)
    ("nf-cod-" . nerd-icons-codicon)
    ("nf-oct-" . nerd-icons-octicon)
    ("nf-fa-" . nerd-icons-faicon)
    ("nf-fae-" . nerd-icons-faicon)
    ("nf-dev-" . nerd-icons-devicon)
    ("nf-seti-" . nerd-icons-sucicon)
    ("nf-custom-" . nerd-icons-sucicon)
    ("nf-weather-" . nerd-icons-wicon)
    ("nf-linux-" . nerd-icons-flicon)
    ("nf-pom-" . nerd-icons-pomicon)
    ("nf-iec-" . nerd-icons-ipsicon)
    ("nf-pl-" . nerd-icons-powerline)
    ("nf-ple-" . nerd-icons-powerline))
  "The nerd-icons function that draws each prefix of glyph names.")

(defun approve-icon--glyph (code)
  "Return the nerd-icons glyph named CODE, such as \"nf-md-tag\", or nil."
  (when-let* ((family (seq-find (lambda (family) (string-prefix-p (car family) code))
                                approve-icon--families)))
    (condition-case nil
        (funcall (cdr family) code)
      (error nil))))

(defun approve-icon--make (name face nerd)
  "Return the icon NAME with FACE, a glyph when NERD is non-nil.
Fall back to the Unicode text when the glyph is unknown, and to
\"?\" when NAME has no fallback either."
  (let ((glyph (and nerd
                    (when-let* ((code (alist-get name approve-icons)))
                      (approve-icon--glyph code)))))
    (cond
     (glyph (if face (approve-face-add glyph face) glyph))
     (t (approve-face (or (alist-get name approve-icon-fallbacks) "?")
                      face)))))

(defun approve-icon (name &optional face)
  "Return the icon NAME as a string, with FACE if non-nil.
NAME is a semantic name from `approve-icons', such as `approved'.
The string is a nerd-icons glyph or a Unicode fallback, see
`approve-use-nerd-icons'.  Its width can be 1 or 2 columns: align
with `string-width'.  Icons are cached; do not modify the string.

Example: (insert (approve-icon \\='approved \\='approve-approved) \" bob\")"
  (let* ((nerd (approve-icon-nerd-p))
         (key (list name face nerd)))
    (or (gethash key approve-icon--cache)
        (puthash key (approve-icon--make name face nerd) approve-icon--cache))))

(provide 'approve-icons)

;;; approve-icons.el ends here
