;;; approve-render.el --- Shared helpers of Approve's renderers -*- lexical-binding: t; -*-

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

;; Small pieces every renderer draws the same way: people, dates,
;; reactions, key hints, loading and error lines, right-aligned text,
;; and indentation that copying leaves out.
;;
;; Things a reader can ask about (a person, a date, a label, reactions)
;; carry the text property `approve-thing', a plist with :kind and what
;; identifies it, such as (:kind user :login "bob").  The context
;; (approve-context.el) and eldoc read it.
;;
;; Layer rules: this is the renderer layer.  These helpers return or
;; insert text; they never read the store or call the network.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'parse-time)
(require 'approve-faces)
(require 'approve-icons)

;;; Options

(defcustom approve-page-width 88
  "Column at which right-aligned text in Approve buffers ends.
Counts, states and key hints on the right of headings line up
there.  It is a little wider than `approve-fill-column', which
fills text.  For a wide frame:

  (setq approve-page-width 110)"
  :type 'natnum
  :group 'approve)

(defcustom approve-show-hints t
  "Whether key hints such as \"r reply · R resolve\" are shown.
Hints only name commands that exist.  To hide them all:

  (setq approve-show-hints nil)"
  :type 'boolean
  :group 'approve)

;;; Things

(defun approve-thing (text thing)
  "Return TEXT with THING as its `approve-thing' property.
THING is a plist with :kind and what identifies it."
  (propertize text 'approve-thing thing))

(defun approve-render-user (actor &optional face)
  "Return the login of ACTOR, a user, bot or organization, as text.
FACE defaults to `approve-user'.  The text carries ACTOR as a thing
of kind `user'.  A missing actor (a deleted account) is \"ghost\",
as GitHub shows it."
  (let ((login (or (alist-get 'login actor) "ghost")))
    (approve-thing (approve-face login (or face 'approve-user))
                   (list :kind 'user :login login
                         :name (alist-get 'name actor)
                         :type (alist-get '__typename actor)
                         :id (alist-get 'id actor)
                         :url (alist-get 'url actor)))))

;;; Dates

(defvar approve-render-now nil
  "Time relative dates are counted from, or nil for the current time.
Tests bind it.")

(defun approve-render-parse-time (iso)
  "Return the time of ISO, an ISO 8601 string, or nil."
  (and (stringp iso) (not (string-empty-p iso))
       (ignore-errors (parse-iso8601-time-string iso))))

(defun approve-render-relative-time (iso)
  "Return how long ago ISO, an ISO 8601 time, was, such as \"3 days ago\"."
  (if-let* ((time (approve-render-parse-time iso)))
      (let* ((seconds (float-time (time-subtract (or approve-render-now (current-time))
                                                 time)))
             (future (< seconds 0))
             (seconds (abs seconds))
             (text (cl-loop for (unit . size) in '(("year" . 31536000)
                                                   ("month" . 2592000)
                                                   ("day" . 86400)
                                                   ("hour" . 3600)
                                                   ("minute" . 60))
                            for n = (floor seconds size)
                            when (>= n 1)
                            return (format "%d %s%s" n unit (if (= n 1) "" "s")))))
        (cond ((null text) "just now")
              (future (concat "in " text))
              (t (concat text " ago"))))
    ""))

(defun approve-render-date (iso &optional face)
  "Return ISO, an ISO 8601 time, as relative text with FACE.
FACE defaults to `approve-date'.  The text carries a thing of kind
`date', so eldoc can show the exact time."
  (approve-thing (approve-face (approve-render-relative-time iso)
                               (or face 'approve-date))
                 (list :kind 'date :value iso)))

;;; Layout

(defun approve-render-justify (left right &optional width)
  "Return LEFT and RIGHT on one line, RIGHT ending at column WIDTH.
WIDTH defaults to `approve-page-width'.  The gap is a space that
stretches (`:align-to'), so RIGHT lines up even after a larger font
in LEFT; at least one space stays between them.  Either may be nil."
  (let* ((left (or left ""))
         (right (or right "")))
    (if (string-empty-p right)
        left
      (concat left " "
              (propertize " " 'display
                          `(space :align-to ,(max 0 (- (or width approve-page-width)
                                                        (string-width right)))))
              right))))

(defun approve-render-join (parts &optional separator)
  "Return the non-empty strings of PARTS joined by SEPARATOR.
SEPARATOR defaults to a dim \" · \"."
  (mapconcat #'identity
             (seq-remove (lambda (part) (or (null part) (string-empty-p part))) parts)
             (or separator (approve-face " · " 'approve-dim))))

(defun approve-render-prefix (start end prefix)
  "Show PREFIX before every line between START and END.
PREFIX is display-only (`line-prefix' and `wrap-prefix'), so moving
and copying ignore it.  A prefix that is already there stays after
PREFIX, so nested prefixes add up."
  (let ((pos start))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'line-prefix nil end))
             (old (get-text-property pos 'line-prefix))
             (new (if old (concat prefix old) prefix)))
        (add-text-properties pos next (list 'line-prefix new 'wrap-prefix new))
        (setq pos next)))))

(defun approve-render-indent (columns)
  "Return a display-only prefix of COLUMNS spaces."
  (make-string columns ?\s))

(defun approve-render-pad (text width &optional left)
  "Return TEXT padded with spaces to WIDTH columns, on the right.
With LEFT, pad on the left, which aligns numbers.  Unlike
`string-pad', this counts columns (`string-width'), so wide
characters such as emoji line up."
  (let ((pad (make-string (max 0 (- width (string-width text))) ?\s)))
    (if left (concat pad text) (concat text pad))))

(defun approve-render-decoration (text)
  "Return TEXT marked as decoration, which copying leaves out.
Use it for line numbers, diff markers and gutters drawn as text."
  (propertize text 'approve-decoration t))

(defun approve-render-strip-decorations (string)
  "Return STRING without the text marked by `approve-render-decoration'.
Approve buffers filter copied text through it."
  (if (text-property-not-all 0 (length string) 'approve-decoration nil string)
      (let ((pos 0) (parts nil))
        (while (< pos (length string))
          (let ((next (or (next-single-property-change pos 'approve-decoration string)
                          (length string))))
            (unless (get-text-property pos 'approve-decoration string)
              (push (substring string pos next) parts))
            (setq pos next)))
        (apply #'concat (nreverse parts)))
    string))

;;; Hints

(defun approve-render-hints (&rest hints)
  "Return key hints, such as \"r reply · R resolve\", or nil.
Each of HINTS is (KEY COMMAND LABEL).  Only hints whose COMMAND is
defined are shown, so a hint appears with its command; none are
shown when `approve-show-hints' is nil."
  (when approve-show-hints
    (let ((shown (seq-filter (lambda (hint) (fboundp (nth 1 hint))) hints)))
      (when shown
        (mapconcat (lambda (hint)
                     (concat (approve-face (nth 0 hint) 'approve-hint-key) " "
                             (approve-face (nth 2 hint) 'approve-hint)))
                   shown (approve-face " · " 'approve-hint))))))

;;; States

(defun approve-render-state-badge (state &optional draft)
  "Return the badge of a pull request in STATE, a draft if DRAFT.
STATE is GitHub's OPEN, CLOSED or MERGED.  An open draft shows both
badges, as GitHub does."
  (let ((badge (lambda (icon text face)
                 (concat (approve-icon icon face) " " (approve-face text face)))))
    (string-join
     (delq nil (list (pcase state
                       ("MERGED" (funcall badge 'merged "Merged" 'approve-state-merged))
                       ("CLOSED" (funcall badge 'closed "Closed" 'approve-state-closed))
                       ("OPEN" (funcall badge 'open "Open" 'approve-state-open)))
                     (and draft (equal state "OPEN")
                          (funcall badge 'draft "Draft" 'approve-state-draft))))
     "  ")))

(defun approve-render-loading (&optional what)
  "Return a \"Loading…\" line, naming WHAT if non-nil."
  (concat (approve-icon 'loading 'approve-loading) " "
          (approve-face (if what (format "Loading %s…" what) "Loading…")
                        'approve-loading)
          "\n"))

(defun approve-render-failure (what err)
  "Return a line saying WHAT could not be loaded because of ERR.
ERR is an error from approve-api; its message is shown, with how to
retry."
  (concat (approve-icon 'error 'approve-error) " "
          (approve-face (format "Could not load %s: %s" what
                                (if (and (consp err) (stringp (cadr err)))
                                    (cadr err)
                                  (format "%s" err)))
                        'approve-error)
          (approve-face " · " 'approve-dim)
          (or (approve-render-hints '("g" approve-pr-refresh "to retry"))
              (approve-face "g to retry" 'approve-hint))
          "\n"))

(defun approve-render-empty (text)
  "Return TEXT as a placeholder line, such as \"No description provided.\"."
  (concat (approve-face text 'approve-empty) "\n"))

;;; Reactions

(defconst approve-render-reactions
  '(("THUMBS_UP" . "👍") ("THUMBS_DOWN" . "👎") ("LAUGH" . "😄")
    ("HOORAY" . "🎉") ("CONFUSED" . "😕") ("HEART" . "❤️")
    ("ROCKET" . "🚀") ("EYES" . "👀"))
  "Emoji of each GitHub reaction.")

(defun approve-render-reaction-emoji (content)
  "Return the emoji of the reaction CONTENT, such as \"THUMBS_UP\"."
  (or (cdr (assoc content approve-render-reactions)) content))

(defun approve-render-reaction-groups (groups)
  "Return the GROUPS of reactions that someone gave, in GitHub's order."
  (seq-filter (lambda (group)
                (> (or (let-alist group .reactors.totalCount) 0) 0))
              groups))

(defun approve-render-reactions-text (groups)
  "Return the reactions in GROUPS as text, such as \"👍 3  🎉 1\", or nil.
GROUPS are reactionGroups; reactions the viewer gave use
`approve-reaction-viewer'.  The text carries a thing of kind
`reactions' with GROUPS."
  (when-let* ((given (approve-render-reaction-groups groups)))
    (approve-thing
     (mapconcat (lambda (group)
                  (let-alist group
                    (approve-face (format "%s %d" (approve-render-reaction-emoji .content)
                                          .reactors.totalCount)
                                  (if .viewerHasReacted
                                      'approve-reaction-viewer
                                    'approve-reaction))))
                given "  ")
     (list :kind 'reactions :groups given))))

;;; Text of comments

(defun approve-render-edited (node)
  "Return \"edited\" if NODE, a comment or body, was edited, else nil."
  (when (alist-get 'lastEditedAt node)
    (approve-thing (approve-face "edited" 'approve-edited)
                   (list :kind 'date :value (alist-get 'lastEditedAt node)
                         :label "edited"))))

(defun approve-render-first-line (html)
  "Return the text of the first paragraph of HTML, on one line."
  (if (or (null html) (string-blank-p html))
      ""
    (let* ((dom (with-temp-buffer
                  (insert html)
                  (libxml-parse-html-region (point-min) (point-max))))
           (body (or (car (dom-by-tag dom 'body)) dom))
           (first (seq-find (lambda (child)
                              (if (stringp child)
                                  (not (string-blank-p child))
                                t))
                            (dom-children body))))
      (string-trim (replace-regexp-in-string
                    "[ \t\n]+" " "
                    (approve-render--dom-text first))))))

(defun approve-render--dom-text (dom)
  "Return the text in DOM and its children, joined by spaces."
  (cond ((stringp dom) dom)
        ((null dom) "")
        (t (mapconcat #'approve-render--dom-text (dom-children dom) " "))))

(defun approve-render-truncate (text width)
  "Return TEXT cut to WIDTH columns, ending in … when cut."
  (if (> (string-width text) width)
      (concat (truncate-string-to-width text (max 0 (1- width))) "…")
    text))

(provide 'approve-render)

;;; approve-render.el ends here
