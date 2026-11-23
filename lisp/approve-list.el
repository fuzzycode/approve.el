;;; approve-list.el --- Lists of pull requests -*- lexical-binding: t; -*-

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

;; The rows of the lists of pull requests, which the dashboard and the
;; search draw alike (docs/ui.org, "Dashboard" and "Search"):
;;
;;   ● #1234  Add retry logic to the GraphQL client  fuzzycode/approve  @alice  +340 −56  2h  ◐  ✗
;;
;; one row per pull request, in the columns of `approve-list-columns':
;; state, number, title, repository, author, changes, last update,
;; checks and review decision by default.  The columns fit their
;; longest text and the window (decision D38, amended):
;; `approve-list-layout' works out how wide each is, for every list of
;; a buffer at once, so they line up.  `approve-list-insert' inserts
;; the rows of a list, each a section of its own
;; (`approve-list-pr-section', whose value is the pull request's URL).
;; A row's text carries the pull request as a thing of kind
;; `pull-request', so the context at point (approve-context.el) is the
;; pull request anywhere on its row.
;;
;; Pull requests are alists in the shape of
;; graphql/fragments/pull-request-summary.graphql, as
;; `approve-query-search' gives them: lists keep no store.
;;
;; Layer rules: this is the renderer layer.  It draws the pull requests
;; it is given; it never reads a store, changes anything or calls the
;; network.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)

;;; Options

(defcustom approve-list-columns
  '((state 1) (number 6) (title nil) (repository 22) (author 14) (changes 12)
    (updated 4) (checks 1) (review 1))
  "The columns of the rows of the lists of pull requests, in order.
Each is (COLUMN WIDTH).  COLUMN is one of

  `state'       open, draft, merged or closed, as an icon
  `number'      the number, #123
  `title'       the title, dim for a draft
  `repository'  owner/name
  `author'      @login
  `changes'     the lines it adds and removes, +340 −56, which say
                how large it is
  `updated'     how long ago it changed: 5m, 2h, 3d, 2w, 4mo, 1y
  `checks'      the checks of its last commit: ✓ passed, ✗ failed,
                ◐ running, · none
  `review'      GitHub's review decision: ✓ approved, ✗ changes
                requested, ○ review required, · none

or a function that is called with the pull request, an alist in the
shape of graphql/fragments/pull-request-summary.graphql, and returns
the column's text.

A column is as wide as its longest text in the lists of the buffer,
so that nothing is cut when the window has room; the rows end where
the text does.  When the narrowest window that shows the buffer (or
`approve-page-width', or the selected window, when none does) has
too little room, columns are cut, ending in \"…\" (icons are never
cut).  Those of WIDTH nil, the title by default, give way first,
down to 10 columns; the others then give way down to WIDTH.  The
state leads the number by one space; other columns are two apart.

For lists of one repository, without it, and with wider authors:

  (setq approve-list-columns
        \\='((state 1) (number 6) (title nil) (author 20) (changes 12)
          (updated 4) (checks 1) (review 1)))"
  :type '(repeat (list (choice (const state) (const number) (const title)
                               (const repository) (const author) (const changes)
                               (const updated) (const checks) (const review) function)
                       (choice (const :tag "Gives way first" nil) natnum)))
  :group 'approve)

;;; Sections

(defclass approve-list-pr-section (magit-section) ()
  "A pull request in a list; its value is the pull request's URL.")

(defconst approve-list-indentation (approve-render-decoration "  ")
  "The indentation of the rows of a list, which copying leaves out.")

(defconst approve-list--narrowest 10
  "The fewest columns a column of nil width gives way to.")

;;; Cells

(defun approve-list--state (pr)
  "Return the icon of PR's state: open, draft, merged or closed."
  (let-alist pr
    (pcase .state
      ("MERGED" (approve-icon 'merged 'approve-state-merged))
      ("CLOSED" (approve-icon 'closed 'approve-state-closed))
      (_ (if .isDraft
             (approve-icon 'draft 'approve-state-draft)
           (approve-icon 'open 'approve-state-open))))))

(defun approve-list--number (pr)
  "Return PR's number, #123."
  (approve-face (format "#%s" (alist-get 'number pr)) 'approve-number))

(defun approve-list--title (pr)
  "Return PR's title, dim when it is a draft."
  (let-alist pr
    (if .isDraft
        (approve-face (or .title "") 'approve-draft-title)
      (or .title ""))))

(defun approve-list--repository (pr)
  "Return PR's repository, owner/name."
  (approve-face (or (let-alist pr .repository.nameWithOwner) "") 'approve-repository))

(defun approve-list--author (pr)
  "Return PR's author as @login (decision D33); a deleted account is @ghost."
  (approve-face (concat "@" (or (let-alist pr .author.login) "ghost")) 'approve-user))

(defun approve-list--changes (pr)
  "Return the lines PR adds and removes, as the parts (\"+340\" \"−56\").
Nil when GitHub didn't say."
  (let-alist pr
    (and .additions .deletions
         (list (approve-face (format "+%d" .additions) 'approve-diffstat-added)
               (approve-face (format "−%d" .deletions) 'approve-diffstat-removed)))))

(defun approve-list--updated (pr)
  "Return how long ago PR changed, such as \"2h\"."
  (approve-face (approve-render-age (alist-get 'updatedAt pr)) 'approve-date))

(defun approve-list--none ()
  "Return the mark of a column with nothing to say: a dim dot."
  (approve-face "·" 'approve-dim))

(defun approve-list--checks (pr)
  "Return the mark of the checks of PR's last commit."
  (pcase (let-alist pr .statusCheckRollup.state)
    ("SUCCESS" (approve-icon 'check-success 'approve-check-success))
    ((or "FAILURE" "ERROR") (approve-icon 'check-failure 'approve-check-failure))
    ((or "PENDING" "EXPECTED") (approve-icon 'check-pending 'approve-check-pending))
    (_ (approve-list--none))))

(defun approve-list--review (pr)
  "Return the mark of GitHub's review decision on PR."
  (pcase (alist-get 'reviewDecision pr)
    ("APPROVED" (approve-icon 'approved 'approve-approved))
    ("CHANGES_REQUESTED" (approve-icon 'changes-requested 'approve-changes-requested))
    ("REVIEW_REQUIRED" (approve-icon 'requested 'approve-requested))
    (_ (approve-list--none))))

(defconst approve-list--cells
  '((state approve-list--state icon)
    (number approve-list--number)
    (title approve-list--title)
    (repository approve-list--repository)
    (author approve-list--author)
    (changes approve-list--changes parts)
    (updated approve-list--updated right)
    (checks approve-list--checks icon)
    (review approve-list--review icon))
  "The columns of `approve-list-columns': (COLUMN FUNCTION . HOW).
FUNCTION returns the column's text for a pull request.  HOW is
`icon' for a column of icons, which is padded but never cut,
`right' for text aligned on the right, or `parts' for a list of
texts, each aligned on the right with those of the other rows.")

;;; Layout

(defun approve-list--gap (column)
  "Return the space after COLUMN: one after the state, else two."
  (if (eq column 'state) " " "  "))

(defun approve-list--how (column)
  "Return how COLUMN is drawn: `icon', `right', `parts' or nil."
  (car (cddr (assq column approve-list--cells))))

(defun approve-list--text (column pr)
  "Return the text of COLUMN for PR, as its function gives it.
A column of parts gives a list of texts.  A COLUMN that is neither
known nor a function shows \"?\"."
  (let* ((cell (assq column approve-list--cells))
         (function (if cell (cadr cell) column)))
    (if (functionp function) (funcall function pr) "?")))

(defun approve-list--parts-widths (texts)
  "Return the width of each part of TEXTS, the widest at its place.
TEXTS are the lists of parts of one column, a list per row."
  (cl-loop for place below (apply #'max 0 (mapcar #'length texts))
           collect (apply #'max 0 (mapcar (lambda (parts)
                                            (string-width (or (nth place parts) "")))
                                          texts))))

(defun approve-list--join-parts (parts widths)
  "Return PARTS, texts, each aligned on the right to its width in WIDTHS.
They are one space apart.  No PARTS is an empty text."
  (if (null parts)
      ""
    (mapconcat #'identity
               (cl-loop for width in widths
                        for place from 0
                        collect (approve-render-pad (or (nth place parts) "") width t))
               " ")))

(defun approve-list--grow (widths needs group room)
  "Widen the columns of GROUP toward what they need, by ROOM at most.
WIDTHS and NEEDS are vectors of the columns' widths and of what
their texts need; GROUP is a list of indexes into them.  WIDTHS
changes in place.  The columns share ROOM evenly, and what one
doesn't need goes to the others.  Return the room left."
  (let (short)
    (while (and (> room 0)
                (setq short (seq-filter (lambda (index)
                                          (< (aref widths index) (aref needs index)))
                                        group)))
      (let ((share (max 1 (/ room (length short)))))
        (dolist (index short)
          (let ((more (min share room (- (aref needs index) (aref widths index)))))
            (cl-incf (aref widths index) more)
            (cl-decf room more)))))
    room))

(defun approve-list-layout (prs &optional width)
  "Return how the rows of PRS, pull requests, are laid out.
The columns of `approve-list-columns' are as wide as their longest
text among PRS, in WIDTH columns at most: the narrowest window that
shows the current buffer by default (`approve-render-window-width').
When that is too little, the columns of nil width give way first,
sharing what is left, down to `approve-list--narrowest' columns;
then the others share it, down to their width.  Icons are never
cut, and the rows may then be wider than WIDTH.

The value is a list of (COLUMN WIDTH PARTS), PARTS being the widths
of the parts of a column of parts, else nil.  Lay out every list of
a buffer at once, so their columns line up."
  (let* ((width (or width (approve-render-window-width)))
         (columns approve-list-columns)
         (count (length columns))
         (widths (make-vector count 0))
         (needs (make-vector count 0))
         (parts (make-vector count nil))
         (first nil) (then nil))
    (cl-loop
     for (column size) in columns
     for index from 0
     for how = (approve-list--how column)
     for texts = (mapcar (lambda (pr) (approve-list--text column pr)) prs)
     do (if (eq how 'parts)
            (let ((places (approve-list--parts-widths texts)))
              (aset parts index places)
              (aset needs index (if places
                                    (+ (apply #'+ places) (1- (length places)))
                                  0)))
          (aset needs index (apply #'max 0 (mapcar (lambda (text) (string-width (or text "")))
                                                   texts))))
     do (cond ((eq how 'icon)
               (aset needs index (max (or size 0) (aref needs index)))
               (aset widths index (aref needs index)))
              ((null size)
               (aset widths index (min approve-list--narrowest (aref needs index)))
               (push index first))
              (t
               (aset widths index (min size (aref needs index)))
               (push index then))))
    (let ((room (- width
                   (string-width approve-list-indentation)
                   (apply #'+ (append widths nil))
                   (apply #'+ (mapcar (lambda (column)
                                        (string-width (approve-list--gap (car column))))
                                      (butlast columns))))))
      (setq room (approve-list--grow widths needs (nreverse first) room))
      (approve-list--grow widths needs (nreverse then) room))
    (cl-loop for (column) in columns
             for index from 0
             collect (list column (aref widths index) (aref parts index)))))

(defun approve-list-layout-width (layout)
  "Return how wide the rows of LAYOUT are, their indentation included.
LAYOUT is from `approve-list-layout'."
  (+ (string-width approve-list-indentation)
     (apply #'+ (mapcar #'cadr layout))
     (apply #'+ (mapcar (lambda (column) (string-width (approve-list--gap (car column))))
                        (butlast layout)))))

;;; Rows

(defun approve-list--cell (column width parts pr)
  "Return the text of COLUMN for PR, fitted to WIDTH columns.
PARTS are the widths of the parts of a column of parts."
  (let* ((how (approve-list--how column))
         (text (approve-list--text column pr))
         (text (if (eq how 'parts)
                   (approve-list--join-parts text parts)
                 (or text ""))))
    (pcase how
      ('icon (approve-render-pad text width))
      ('right (approve-render-pad (approve-render-truncate text width) width t))
      (_ (approve-render-pad (approve-render-truncate text width) width)))))

(defun approve-list-row (pr &optional layout)
  "Return the row of PR, a pull request, without its newline.
LAYOUT is from `approve-list-layout', that of PR alone when nil.
The row is indented and carries PR as a thing of kind
`pull-request', with its URL."
  (let* ((layout (or layout (approve-list-layout (list pr))))
         (cells (cl-loop for (column width parts) in layout
                         for rest on layout
                         collect (approve-list--cell column width parts pr)
                         when (cdr rest)
                         collect (approve-list--gap column)))
         (row (concat approve-list-indentation
                      (string-trim-right (apply #'concat cells)))))
    (approve-thing row (list :kind 'pull-request :pr pr :url (alist-get 'url pr)))))

(defun approve-list-insert (prs &optional empty layout)
  "Insert the rows of PRS, the pull requests of a list, at point.
Each row is a section of its own, `approve-list-pr-section'.  With
no PRS, insert EMPTY, \"No pull requests.\" by default, as a dim
placeholder.  The rows keep the order of PRS.  LAYOUT is from
`approve-list-layout', that of PRS alone when nil: a buffer of
several lists lays them out together, so they line up."
  (if (null prs)
      (insert approve-list-indentation
              (approve-render-empty (or empty "No pull requests.")))
    (let ((layout (or layout (approve-list-layout prs))))
      (dolist (pr prs)
        (magit-insert-section (approve-list-pr-section (alist-get 'url pr))
          (let ((row (approve-list-row pr layout)))
            ;; The newline is the row's too: point at its end is on it.
            (insert row (propertize "\n" 'approve-thing
                                    (get-text-property 0 'approve-thing row)))))))))

(provide 'approve-list)

;;; approve-list.el ends here
