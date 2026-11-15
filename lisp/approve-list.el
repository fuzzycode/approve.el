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
;;   ● #1234  Add retry logic to the GraphQL client   fuzzycode/approve  @alice  2h  ◐  ✗
;;
;; one row per pull request, in the columns of `approve-list-columns':
;; state, number, title, repository, author, last update, checks and
;; review decision by default.  `approve-list-insert' inserts the rows
;; of a list, each a section of its own (`approve-list-pr-section',
;; whose value is the pull request's URL).  A row's text carries the
;; pull request as a thing of kind `pull-request', so the context at
;; point (approve-context.el) is the pull request anywhere on its row.
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
  '((state 1) (number 6) (title nil) (repository 22) (author 14) (updated 4)
    (checks 1) (review 1))
  "The columns of the rows of the lists of pull requests, in order.
Each is (COLUMN WIDTH).  COLUMN is one of

  `state'       open, draft, merged or closed, as an icon
  `number'      the number, #123
  `title'       the title, dim for a draft
  `repository'  owner/name
  `author'      @login
  `updated'     how long ago it changed: 5m, 2h, 3d, 2w, 4mo, 1y
  `checks'      the checks of its last commit: ✓ passed, ✗ failed,
                ◐ running, · none
  `review'      GitHub's review decision: ✓ approved, ✗ changes
                requested, ○ review required, · none

or a function that is called with the pull request, an alist in the
shape of graphql/fragments/pull-request-summary.graphql, and returns
the column's text.

WIDTH is how many columns of text it takes; longer text is cut, ending
in \"…\" (icons are never cut).  nil takes the room the other columns
leave, up to `approve-page-width'; columns with nil share it.  The
state leads the number by one space; other columns are two apart.

For lists of one repository, without it, and with wider authors:

  (setq approve-list-columns
        \\='((state 1) (number 6) (title nil) (author 20) (updated 4)
          (checks 1) (review 1)))"
  :type '(repeat (list (choice (const state) (const number) (const title)
                               (const repository) (const author) (const updated)
                               (const checks) (const review) function)
                       (choice (const :tag "The room left" nil) natnum)))
  :group 'approve)

;;; Sections

(defclass approve-list-pr-section (magit-section) ()
  "A pull request in a list; its value is the pull request's URL.")

(defconst approve-list-indentation (approve-render-decoration "  ")
  "The indentation of the rows of a list, which copying leaves out.")

(defconst approve-list--narrowest 10
  "The fewest columns a column of nil width takes.")

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
    (updated approve-list--updated right)
    (checks approve-list--checks icon)
    (review approve-list--review icon))
  "The columns of `approve-list-columns': (COLUMN FUNCTION . HOW).
FUNCTION returns the column's text for a pull request.  HOW is
`icon' for a column of icons, which is padded but never cut, or
`right' for text aligned on the right.")

;;; Rows

(defun approve-list--gap (column)
  "Return the space after COLUMN: one after the state, else two."
  (if (eq column 'state) " " "  "))

(defun approve-list-widths ()
  "Return the columns of `approve-list-columns' with their widths.
The value is a list of (COLUMN . WIDTH), each WIDTH a number: those
of nil share the room up to `approve-page-width' that the
indentation, the other columns and the gaps between them leave."
  (let* ((columns approve-list-columns)
         (shared (seq-count (lambda (column) (null (cadr column))) columns))
         (used (+ (string-width approve-list-indentation)
                  (apply #'+ (mapcar (lambda (column) (or (cadr column) 0)) columns))
                  (apply #'+ (mapcar (lambda (column)
                                       (string-width (approve-list--gap (car column))))
                                     (butlast columns)))))
         (room (if (> shared 0)
                   (max approve-list--narrowest
                        (/ (- approve-page-width used) shared))
                 0)))
    (mapcar (lambda (column)
              (cons (car column) (or (cadr column) room)))
            columns)))

(defun approve-list--cell (column width pr)
  "Return the text of COLUMN for PR, fitted to WIDTH columns.
A COLUMN that is neither known nor a function shows \"?\"."
  (let* ((cell (assq column approve-list--cells))
         (function (if cell (cadr cell) column))
         (text (or (if (functionp function) (funcall function pr) "?") ""))
         (how (cddr cell)))
    (cond ((memq 'icon how) (approve-render-pad text width))
          ((memq 'right how) (approve-render-pad (approve-render-truncate text width)
                                                 width t))
          (t (approve-render-pad (approve-render-truncate text width) width)))))

(defun approve-list-row (pr &optional widths)
  "Return the row of PR, a pull request, without its newline.
WIDTHS are the columns from `approve-list-widths', computed when
nil.  The row is indented and carries PR as a thing of kind
`pull-request', with its URL."
  (let* ((widths (or widths (approve-list-widths)))
         (cells (cl-loop for (column . width) in widths
                         for rest on widths
                         collect (approve-list--cell column width pr)
                         when (cdr rest)
                         collect (approve-list--gap column)))
         (row (concat approve-list-indentation
                      (string-trim-right (apply #'concat cells)))))
    (approve-thing row (list :kind 'pull-request :pr pr :url (alist-get 'url pr)))))

(defun approve-list-insert (prs &optional empty)
  "Insert the rows of PRS, the pull requests of a list, at point.
Each row is a section of its own, `approve-list-pr-section'.  With
no PRS, insert EMPTY, \"No pull requests.\" by default, as a dim
placeholder.  The rows keep the order of PRS."
  (if (null prs)
      (insert approve-list-indentation
              (approve-render-empty (or empty "No pull requests.")))
    (let ((widths (approve-list-widths)))
      (dolist (pr prs)
        (magit-insert-section (approve-list-pr-section (alist-get 'url pr))
          (let ((row (approve-list-row pr widths)))
            ;; The newline is the row's too: point at its end is on it.
            (insert row (propertize "\n" 'approve-thing
                                    (get-text-property 0 'approve-thing row)))))))))

(provide 'approve-list)

;;; approve-list.el ends here
