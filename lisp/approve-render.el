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
(require 'magit-section)
(require 'approve-faces)
(require 'approve-icons)

;;; Options

(defcustom approve-page-width 110
  "Column at which right-aligned text in Approve buffers ends.
Counts, states and key hints on the right of headings line up
there, as do the statistics of the diff's files and the header
line's state.  It leaves room right of `approve-fill-column', which
fills text, for the indentation of threads and their hints.  The
lists of pull requests fit the window instead, and their lines end
where their rows do (`approve-list-columns').  For a narrow frame
or a side-by-side window:

  (setq approve-page-width 88)"
  :type 'natnum
  :group 'approve)

(defcustom approve-show-hints t
  "Whether key hints such as \"r reply · R resolve\" are shown.
They are in the PR buffer: under headings, after threads, at the end
of the conversation.  Each names the key that runs its command there,
so it follows your bindings and evil's.  Input buffers always name
their keys in the header line, and a failed load always says how to
retry.  To hide the others:

  (setq approve-show-hints nil)"
  :type 'boolean
  :group 'approve)

;;; Things

(defun approve-thing (text thing)
  "Return TEXT with THING as its `approve-thing' property.
THING is a plist with :kind and what identifies it."
  (propertize text 'approve-thing thing))

(defun approve-render-user (actor &optional face)
  "Return ACTOR, a user, bot or organization, as text: @login.
People are written as GitHub writes them in comments, with an @, so
a login is never mistaken for anything else (decision D33).  FACE
defaults to `approve-user'.  The text carries ACTOR as a thing of
kind `user'.  A missing actor (a deleted account) is @ghost, as
GitHub shows it."
  (let ((login (or (alist-get 'login actor) "ghost")))
    (approve-thing (approve-face (concat "@" login) (or face 'approve-user))
                   (list :kind 'user :login login
                         :name (alist-get 'name actor)
                         :type (alist-get '__typename actor)
                         :id (alist-get 'id actor)
                         :url (alist-get 'url actor)))))

(defun approve-render-git-actor (actor)
  "Return ACTOR, the Git author or committer of a commit, as text.
When GitHub links its e-mail to an account, it is that account,
as @login (`approve-render-user').  Else it is the name Git wrote,
without an @, carrying a thing of kind `git-actor' (with :name and
:email), so eldoc can say why there is no account."
  (let-alist actor
    (if .user
        (approve-render-user .user)
      (approve-thing (approve-face (or .name "?") 'approve-user)
                     (list :kind 'git-actor :name .name :email .email)))))

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

(defun approve-render-age (iso)
  "Return how long ago ISO, an ISO 8601 time, was, in a few columns.
That is \"now\" under a minute, then minutes, hours, days, weeks,
months and years: \"5m\", \"2h\", \"3d\", \"2w\", \"11mo\", \"1y\", at most
four columns, for lists.  A time to come is \"now\"; no time is \"\"."
  (if-let* ((time (approve-render-parse-time iso)))
      (let ((seconds (float-time (time-subtract (or approve-render-now (current-time))
                                                time))))
        (or (cl-loop for (unit . size) in '(("y" . 31536000) ("mo" . 2592000)
                                            ("w" . 604800) ("d" . 86400)
                                            ("h" . 3600) ("m" . 60))
                     for n = (floor seconds size)
                     when (>= n 1)
                     return (format "%d%s" n unit))
            "now"))
    ""))

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

(defun approve-render-window-width ()
  "Return how many columns the windows that show the current buffer have.
That is the narrowest one's, or `approve-page-width' when no window
shows the buffer."
  (if-let* ((windows (get-buffer-window-list (current-buffer) nil t)))
      (apply #'min (mapcar #'window-max-chars-per-line windows))
    approve-page-width))

(defun approve-render-width ()
  "Return the column at which rows of the current buffer should end.
That is `approve-page-width', or less when a window that shows the
buffer is narrower: the narrowest one.  It is the width when the
buffer is drawn; a window resized later shows the rows as they are
until the next redraw."
  (min approve-page-width (approve-render-window-width)))

(defun approve-render-wrap (items width &optional separator indent)
  "Return ITEMS joined by SEPARATOR, in lines at most WIDTH columns wide.
ITEMS are strings; a line breaks between them, never inside one, so
an item wider than WIDTH has a line of its own.  SEPARATOR defaults
to a space.  Each line after the first starts with INDENT, a string,
which does not count in WIDTH.

Example:
  (approve-render-wrap \='(\"@bob\" \"@carol\" \"@dave\") 12 \"  \" \"> \")
  => \"@bob  @carol\\n> @dave\""
  (let* ((separator (or separator " "))
         (gap (string-width separator))
         (lines nil) (line nil) (column 0))
    (dolist (item items)
      (let ((size (string-width item)))
        (cond ((null line)
               (setq line item column size))
              ((<= (+ column gap size) width)
               (setq line (concat line separator item)
                     column (+ column gap size)))
              (t (push line lines)
                 (setq line item column size)))))
    (when line
      (push line lines))
    (mapconcat #'identity (nreverse lines) (concat "\n" indent))))

(defun approve-render-join (parts &optional separator)
  "Return the non-empty strings of PARTS joined by SEPARATOR.
SEPARATOR defaults to a dim \" · \"."
  (mapconcat #'identity
             (seq-remove (lambda (part) (or (null part) (string-empty-p part))) parts)
             (or separator (approve-face " · " 'approve-dim))))

(defconst approve-render--prefix-inherits
  '(magit-section approve-line approve-thing invisible)
  "Properties a prefix takes from the first character of its line.
With them, the section, the diff line and the thing at point are the
same on the indentation as on the text after it.")

(defun approve-render--extend-sections (section from to)
  "Move SECTION's start back from TO to FROM, and its parents' that start there.
A finished section's start marker advances over text inserted at
it, which would leave a prefix inserted there outside the section."
  (while (and section
              (markerp (oref section start))
              (= (oref section start) to))
    (set-marker (oref section start) from)
    (setq section (oref section parent))))

(defun approve-render-prefix (start end prefix)
  "Indent every line between START and END with PREFIX.
PREFIX is inserted as text at the start of each line, marked as
decoration (`approve-render-decoration'), so copying leaves it out.
It keeps its own faces and takes the properties in
`approve-render--prefix-inherits' from its line, so commands work
on it as on the line.  Lines that wrap continue under their text:
PREFIX goes in front of the `wrap-prefix' too.  Prefixes nest: an
outer prefix, added later, goes in front of an inner one.  An empty
line gets PREFIX without its trailing spaces, so no line ends in
whitespace.  A line that starts before START is left as it is.

The indentation is text rather than a display-only `line-prefix'
because a collapsed section would lend the prefix of its hidden
body to the next heading, and moving down would stop in the hidden
text.  Text after END, point included, moves with what is inserted."
  (let ((tail (- (point-max) end))
        (blank (string-trim-right prefix)))
    (save-excursion
      (goto-char start)
      (unless (bolp)
        (forward-line 1))
      ;; END moves with the insertions; the text after it does not change.
      (while (< (point) (- (point-max) tail))
        (let* ((text (if (eolp) blank prefix))
               (length (length text)))
          (when (> length 0)
            (let ((section (get-text-property (point) 'magit-section)))
              (setq text (approve-render-decoration (copy-sequence text)))
              (dolist (prop approve-render--prefix-inherits)
                (when-let* ((value (get-text-property (point) prop)))
                  (put-text-property 0 length prop value text)))
              (insert text)
              (approve-render--extend-sections section (- (point) length) (point)))
            (let ((pos (- (point) length))
                  (eol (min (1+ (line-end-position)) (- (point-max) tail))))
              (while (< pos eol)
                (let ((next (next-single-property-change pos 'wrap-prefix nil eol))
                      (old (get-text-property pos 'wrap-prefix)))
                  (put-text-property pos next 'wrap-prefix
                                     (if old (concat prefix old) prefix))
                  (setq pos next)))))
          (forward-line 1))))))

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

(defun approve-render-key (command)
  "Return the key that runs COMMAND in the current buffer, as text, or nil.
The binding is looked up in the keymaps active here, so it is the
key the user would press: one they bound themselves, or evil's (\"i\"
for `approve-edit' in normal state).  Nil when no key runs COMMAND."
  (when-let* ((keys (and (fboundp command) (where-is-internal command nil t))))
    (key-description keys)))

(defun approve-render-key-hints (hints)
  "Return key hints for HINTS, such as \"r reply · R resolve\", or nil.
Each of HINTS is (COMMAND LABEL), or nil to skip.  The key of each is
the one that runs COMMAND in the current buffer (`approve-render-key');
a COMMAND no key runs is left out, so a hint appears with its command
and follows the user's bindings.  Unlike `approve-render-hints', this
ignores `approve-show-hints': input buffers always name their keys."
  (let ((shown (delq nil (mapcar (lambda (hint)
                                   (when-let* ((key (and hint (approve-render-key (car hint)))))
                                     (cons key (cadr hint))))
                                 hints))))
    (when shown
      (mapconcat (lambda (hint)
                   (concat (approve-face (car hint) 'approve-hint-key) " "
                           (approve-face (cdr hint) 'approve-hint)))
                 shown (approve-face " · " 'approve-hint)))))

(defun approve-render-hints (&rest hints)
  "Return key hints, such as \"r reply · R resolve\", or nil.
Each of HINTS is (COMMAND LABEL), or nil to skip; see
`approve-render-key-hints'.  None are shown when `approve-show-hints'
is nil.

Example:
  (approve-render-hints \='(approve-reply \"reply\")
                        \='(approve-resolve \"resolve\"))
  => \"r reply · R resolve\" in a PR buffer, without evil"
  (when approve-show-hints
    (approve-render-key-hints hints)))

(defun approve-render-retry-hint (&optional command)
  "Return the hint that says how to retry what failed to load.
COMMAND is the command that loads it again, `approve-pr-refresh' by
default.  The hint shows even when `approve-show-hints' is nil: it is
the way out."
  (let ((command (or command 'approve-pr-refresh)))
    (or (approve-render-key-hints `((,command "to retry")))
        (approve-face (substitute-command-keys (format "\\[%s] to retry" command))
                      'approve-hint))))

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

(defun approve-render-failure (what err &optional command)
  "Return a line saying WHAT could not be loaded because of ERR.
ERR is an error from approve-api; its message is shown, with how to
retry: COMMAND, `approve-pr-refresh' by default."
  (concat (approve-icon 'error 'approve-error) " "
          (approve-face (format "Could not load %s: %s" what
                                (if (and (consp err) (stringp (cadr err)))
                                    (cadr err)
                                  (format "%s" err)))
                        'approve-error)
          (approve-face " · " 'approve-dim)
          (approve-render-retry-hint command)
          "\n"))

(defun approve-render-empty (text)
  "Return TEXT as a placeholder line, such as \"No description provided.\"."
  (concat (approve-face text 'approve-empty) "\n"))

(defun approve-render-insert-fatal (headline message &optional hint command)
  "Insert the error of a whole buffer, which leaves nothing else to show.
HEADLINE says what failed, MESSAGE is the error's text, and HINT, if
non-nil, what to check; then how to retry with COMMAND,
`approve-pr-refresh' by default.  The lines under HEADLINE are
indented."
  (insert (approve-icon 'error 'approve-error) " " (approve-face headline 'approve-error)
          "\n")
  (let ((start (point)))
    (insert (approve-face message 'approve-dim) "\n\n")
    (when hint
      (insert (approve-face hint 'approve-dim) "\n"))
    (insert (approve-render-retry-hint command) "\n")
    (approve-render-prefix start (point) "  ")))

;;; Reactions

(defconst approve-render-reactions
  '(("THUMBS_UP" . reaction-thumbs-up) ("THUMBS_DOWN" . reaction-thumbs-down)
    ("LAUGH" . reaction-laugh) ("HOORAY" . reaction-hooray)
    ("CONFUSED" . reaction-confused) ("HEART" . reaction-heart)
    ("ROCKET" . reaction-rocket) ("EYES" . reaction-eyes))
  "Icon of each GitHub reaction: its name in `approve-icons'.
Reactions are flat glyphs like every other icon; the emoji GitHub
shows are their fallbacks (`approve-icon-fallbacks').")

(defun approve-render-reaction-icon (content &optional face)
  "Return the icon of the reaction CONTENT, such as \"THUMBS_UP\", with FACE.
An unknown CONTENT is returned as it is."
  (if-let* ((name (cdr (assoc content approve-render-reactions))))
      (approve-icon name face)
    content))

(defun approve-render-reaction-groups (groups)
  "Return the GROUPS of reactions that someone gave, in GitHub's order."
  (seq-filter (lambda (group)
                (> (or (let-alist group .reactors.totalCount) 0) 0))
              groups))

(defun approve-render-reactions-text (groups &optional subject)
  "Return the reactions in GROUPS as text, such as \"👍 3  🎉 1\", or nil.
GROUPS are reactionGroups; reactions the viewer gave use
`approve-reaction-viewer'.  The text carries a thing of kind
`reactions' with GROUPS and SUBJECT, the id of what was reacted to."
  (when-let* ((given (approve-render-reaction-groups groups)))
    (approve-thing
     (mapconcat (lambda (group)
                  (let-alist group
                    (let ((face (if .viewerHasReacted
                                    'approve-reaction-viewer
                                  'approve-reaction)))
                      (concat (approve-render-reaction-icon .content face) " "
                              (approve-face (number-to-string .reactors.totalCount)
                                            face)))))
                given "  ")
     (list :kind 'reactions :groups given :subject subject))))

;;; Text of comments

(defun approve-render-edited (node)
  "Return \"edited\" if NODE, a comment or body, was edited, else nil."
  (when (alist-get 'lastEditedAt node)
    (approve-thing (approve-face "edited" 'approve-edited)
                   (list :kind 'date :value (alist-get 'lastEditedAt node)
                         :label "edited"))))

(defun approve-render-new (id)
  "Return \"new\", the mark of the comment ID new since the last review.
The mark's `approve-new' property is ID: the next and previous new
comment are found by it (`approve-next-new-comment', decision D44)."
  (propertize (approve-face "new" 'approve-new) 'approve-new id))

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

(defun approve-render-shorten-path (path width)
  "Return PATH cut to WIDTH columns, keeping its first directory and its end.
The … replaces whole directories where it can, so the file name
stays.

Example:
  (approve-render-shorten-path \"internal/render/templates/page.tmpl\" 24)
  => \"internal/…/page.tmpl\""
  (if (<= (string-width path) width)
      path
    (let* ((slash (string-search "/" path))
           (head (if (and slash (<= (1+ slash) (/ width 2)))
                     (substring path 0 (1+ slash))
                   ""))
           (room (max 0 (- width (string-width head) 1)))
           (start (length path)))
      (while (and (> start 0)
                  (<= (string-width path (1- start)) room))
        (setq start (1- start)))
      ;; Start the end at a directory when there is one in it.
      (let ((directory (string-search "/" path start)))
        (when (and directory (< directory (1- (length path))))
          (setq start directory)))
      (concat head "…" (substring path start)))))

(provide 'approve-render)

;;; approve-render.el ends here
