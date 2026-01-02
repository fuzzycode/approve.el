;;; approve-search.el --- Search for GitHub Pull Requests  -*- lexical-binding: t; -*-

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

;; This module provides free-text search for GitHub Pull Requests.
;;
;; Features:
;; - Interactive search with GitHub's search syntax
;; - Search history with completion
;; - Tabulated list view of results
;; - Ability to refine search query while viewing results
;;
;; Usage:
;;   M-x approve-search
;;
;; The search history is automatically saved via `savehist-mode' if enabled.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)

(require 'approve-api-queries)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)

;; Forward declaration for approve-view-pr
(declare-function approve-view-pr "approve")

;;; Customization

(defgroup approve-search nil
  "Search for GitHub Pull Requests."
  :group 'approve
  :prefix "approve-search-")

(defcustom approve-search-default-limit 50
  "Default number of PRs to fetch in search results."
  :group 'approve-search
  :type 'integer)

(defcustom approve-search-title-width 50
  "Width of the title column in search results."
  :group 'approve-search
  :type 'integer)

(defcustom approve-search-repo-width 25
  "Width of the repository column in search results."
  :group 'approve-search
  :type 'integer)

(defcustom approve-search-author-width 15
  "Width of the author column in search results."
  :group 'approve-search
  :type 'integer)

;;; Search History

(defvar approve-search-history nil
  "History of search queries.
Add `approve-search-history' to `savehist-additional-variables'
to persist across sessions.")

;;; Buffer-local State

(defvar-local approve-search--current-query nil
  "The current search query.")

(defvar-local approve-search--loading nil
  "Non-nil when a search is in progress.")

(defvar-local approve-search--error nil
  "Error message from the last search, or nil.")

(defvar-local approve-search--result-count nil
  "Total number of results from the last search.")

(defvar-local approve-search--pr-data nil
  "Alist mapping PR URLs to their full data.")

;;; Time Formatting

(defun approve-search--format-relative-time (date-string)
  "Format DATE-STRING as a relative time (e.g., \"2h\", \"3d\").
DATE-STRING should be in ISO 8601 format."
  (when-let ((time (approve-ui-parse-iso-date date-string)))
    (let* ((now (current-time))
           (diff (float-time (time-subtract now time)))
           (minutes (/ diff 60))
           (hours (/ diff 3600))
           (days (/ diff 86400))
           (weeks (/ diff 604800)))
      (cond
       ((< minutes 1) "now")
       ((< minutes 60) (format "%dm" (floor minutes)))
       ((< hours 24) (format "%dh" (floor hours)))
       ((< days 7) (format "%dd" (floor days)))
       ((< weeks 52) (format "%dw" (floor weeks)))
       (t (format-time-string "%Y-%m-%d" time))))))

;;; Entry Formatting

(defun approve-search--format-state (pr)
  "Format the state indicator for PR."
  (let ((state (alist-get 'state pr))
        (is-draft (alist-get 'isDraft pr)))
    (cond
     (is-draft
      (approve-ui-propertize-face "Draft" 'approve-dashboard-draft-face))
     ((string= state "OPEN")
      (approve-ui-propertize-face "Open" 'approve-dashboard-state-open-face))
     ((string= state "MERGED")
      (approve-ui-propertize-face "Merged" 'approve-dashboard-state-merged-face))
     ((string= state "CLOSED")
      (approve-ui-propertize-face "Closed" 'approve-dashboard-state-closed-face))
     (t state))))

(defun approve-search--format-review (pr)
  "Format the review decision for PR."
  (let ((decision (alist-get 'reviewDecision pr)))
    (pcase decision
      ("APPROVED"
       (approve-ui-propertize-face "✓" 'approve-dashboard-review-approved-face))
      ("CHANGES_REQUESTED"
       (approve-ui-propertize-face "✗" 'approve-dashboard-review-changes-requested-face))
      ("REVIEW_REQUIRED"
       (approve-ui-propertize-face "○" 'approve-dashboard-review-required-face))
      (_ ""))))

(defun approve-search--truncate (str width)
  "Truncate STR to WIDTH characters, adding ellipsis if needed."
  (if (> (length str) width)
      (concat (substring str 0 (- width 1)) "…")
    str))

(defun approve-search--make-entry (pr)
  "Create a tabulated-list entry from PR data."
  (let* ((url (alist-get 'url pr))
         (number (alist-get 'number pr))
         (title (or (alist-get 'title pr) ""))
         (repo (alist-get 'repository pr))
         (owner (alist-get 'login (alist-get 'owner repo)))
         (repo-name (alist-get 'name repo))
         (full-repo (format "%s/%s" owner repo-name))
         (author (alist-get 'login (alist-get 'author pr)))
         (updated-at (alist-get 'updatedAt pr))
         (state-str (approve-search--format-state pr))
         (review-str (approve-search--format-review pr)))
    (list url
          (vector
           (approve-ui-propertize-face (format "#%d" number)
                                       'approve-dashboard-pr-number-face)
           review-str
           state-str
           (approve-ui-propertize-face
            (approve-search--truncate title approve-search-title-width)
            'approve-dashboard-pr-title-face)
           (approve-ui-propertize-face
            (approve-search--truncate full-repo approve-search-repo-width)
            'approve-dashboard-repo-face)
           (approve-ui-propertize-face
            (or author "")
            'approve-dashboard-author-face)
           (approve-ui-propertize-face
            (or (approve-search--format-relative-time updated-at) "")
            'approve-dashboard-time-face)))))

;;; Mode Definition

(defvar approve-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'approve-search-open-pr)
    (define-key map (kbd "o") #'approve-search-browse-pr)
    (define-key map (kbd "g") #'approve-search-refresh)
    (define-key map (kbd "s") #'approve-search-new-query)
    (define-key map (kbd "/") #'approve-search-new-query)
    map)
  "Keymap for `approve-search-mode'.")

(define-derived-mode approve-search-mode tabulated-list-mode "Approve-Search"
  "Major mode for displaying GitHub Pull Request search results.

\\{approve-search-mode-map}"
  :group 'approve-search
  (setq tabulated-list-format
        (vector
         '("PR" 7 t)
         '("R" 2 nil)
         '("State" 8 t)
         `("Title" ,approve-search-title-width t)
         `("Repository" ,approve-search-repo-width t)
         `("Author" ,approve-search-author-width t)
         '("Updated" 10 t)))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Updated" . nil))
  (tabulated-list-init-header)
  (hl-line-mode 1))

;;; Header Line

(defun approve-search--update-header ()
  "Update the header line to show current query and status."
  (setq header-line-format
        (concat
         (approve-ui-propertize-face "Search: " 'approve-dashboard-section-title-face)
         (or approve-search--current-query "(none)")
         (cond
          (approve-search--loading
           (concat " " (approve-ui-propertize-face "[Loading...]"
                                                   'approve-dashboard-loading-face)))
          (approve-search--error
           (concat " " (approve-ui-propertize-face
                        (format "[Error: %s]" approve-search--error)
                        'approve-dashboard-error-face)))
          (approve-search--result-count
           (concat " " (approve-ui-propertize-face
                        (format "[%d results]" approve-search--result-count)
                        'approve-dashboard-section-count-face)))
          (t ""))
         "  "
         (approve-ui-propertize-face "[s]earch [g]refresh [RET]open [o]browse"
                                     'font-lock-comment-face))))

;;; Data Fetching

(defun approve-search--execute (query)
  "Execute search for QUERY and update the buffer."
  (setq approve-search--current-query query
        approve-search--loading t
        approve-search--error nil
        approve-search--result-count nil
        approve-search--pr-data nil)
  (approve-search--update-header)
  (setq tabulated-list-entries nil)
  (tabulated-list-print t)
  (let ((buffer (current-buffer)))
    (approve-api-search-pull-requests
     query
     :limit approve-search-default-limit
     :buffer buffer
     :callback
     (lambda (data)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let* ((search (alist-get 'search data))
                  (nodes (alist-get 'nodes search))
                  (count (alist-get 'issueCount search)))
             (setq approve-search--loading nil
                   approve-search--result-count (or count (length nodes))
                   approve-search--pr-data
                   (mapcar (lambda (pr)
                             (cons (alist-get 'url pr) pr))
                           nodes)
                   tabulated-list-entries
                   (mapcar #'approve-search--make-entry nodes))
             (approve-search--update-header)
             (tabulated-list-print t)))))
     :error-callback
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq approve-search--loading nil
                 approve-search--error (format "%s" (cdr error)))
           (approve-search--update-header)))))))

;;; Interactive Commands

(defun approve-search-open-pr ()
  "Open the PR at point in an Approve review buffer."
  (interactive)
  (when-let ((url (tabulated-list-get-id)))
    (approve-view-pr url)))

(defun approve-search-browse-pr ()
  "Open the PR at point in a web browser."
  (interactive)
  (when-let ((url (tabulated-list-get-id)))
    (browse-url url)))

(defun approve-search-refresh ()
  "Re-execute the current search query."
  (interactive)
  (when approve-search--current-query
    (approve-search--execute approve-search--current-query)))

(defun approve-search-new-query ()
  "Prompt for a new search query."
  (interactive)
  (let ((query (read-string "Search PRs: "
                            approve-search--current-query
                            'approve-search-history)))
    (when (and query (not (string-empty-p query)))
      (add-to-history 'approve-search-history query)
      (approve-search--execute query))))

;;; Entry Point

(defconst approve-search--buffer-name "*Approve Search*"
  "Name of the search results buffer.")

;;;###autoload
(defun approve-search (query)
  "Search for GitHub Pull Requests matching QUERY.

QUERY uses GitHub's search syntax.  Some useful filters:
  is:open / is:closed / is:merged
  author:USERNAME
  review-requested:@me
  involves:@me
  repo:OWNER/NAME
  org:ORGNAME

The search history is available via completion and can be
persisted by adding `approve-search-history' to
`savehist-additional-variables'.

In the results buffer:
  RET - Open PR in Approve review buffer
  o   - Open PR in web browser
  s   - New search query
  g   - Refresh current search"
  (interactive
   (list (read-string "Search PRs: " nil 'approve-search-history)))
  (when (and query (not (string-empty-p query)))
    (add-to-history 'approve-search-history query)
    (let ((buffer (get-buffer-create approve-search--buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'approve-search-mode)
          (approve-search-mode))
        (approve-search--execute query))
      (switch-to-buffer buffer))))

(provide 'approve-search)
;;; approve-search.el ends here
