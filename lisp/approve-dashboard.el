;;; approve-dashboard.el --- Dashboard for GitHub Pull Requests  -*- lexical-binding: t; -*-

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

;; This module provides a dashboard view for GitHub Pull Requests.
;;
;; The dashboard displays configurable sections, each representing a
;; GitHub search query.  Users can quickly see PRs that need review,
;; their own PRs, and other queries of interest.
;;
;; Key features:
;; - Multiple configurable sections with GitHub search filters
;; - Magit-section based UI for familiar navigation
;; - Quick access to open PRs in review mode
;; - Per-section result limits
;;
;; Usage:
;;   M-x approve-dashboard
;;
;; Customization:
;;   See `approve-dashboard-sections' to configure sections.

;;; Code:

(require 'cl-lib)
(require 'magit-section)

(require 'approve-api-queries)
(require 'approve-pr-utils)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)

;; Forward declaration for approve-view-pr
(declare-function approve-view-pr "approve")

;;; Customization

(defgroup approve-dashboard nil
  "Dashboard for GitHub Pull Requests."
  :group 'approve
  :prefix "approve-dashboard-")

(defcustom approve-dashboard-sections
  '((:title "Needs My Review"
     :filter "is:open review-requested:@me"
     :limit 20)
    (:title "My Pull Requests"
     :filter "is:open author:@me"
     :limit 20)
    (:title "Involved"
     :filter "is:open involves:@me -author:@me -review-requested:@me"
     :limit 10))
  "List of sections to display in the dashboard.

Each section is a plist with the following keys:
  :title  - The section title (required)
  :filter - GitHub search filter (required).
            Can be a string or a function that returns a string.
            Note: \"is:pr\" is automatically added.
  :limit  - Maximum number of PRs to fetch (optional, default 20)

Example:
  \\='((:title \"Needs My Review\"
     :filter \"is:open review-requested:@me\"
     :limit 20)
    (:title \"My Pull Requests\"
     :filter \"is:open author:@me\"
     :limit 10)
    (:title \"Recent in Org\"
     :filter (lambda ()
               (format \"is:open org:%s\" my-github-org))
     :limit 15))"
  :group 'approve-dashboard
  :type '(repeat
          (plist :options
                 ((:title string)
                  (:filter (choice string function))
                  (:limit integer)))))

(defcustom approve-dashboard-default-limit 20
  "Default number of PRs to fetch per section.
Used when a section doesn't specify its own :limit."
  :group 'approve-dashboard
  :type 'integer)

(defcustom approve-dashboard-title-width 60
  "Maximum width for PR titles in the dashboard.
Titles longer than this will be truncated."
  :group 'approve-dashboard
  :type 'integer)

(defcustom approve-dashboard-repo-width 30
  "Maximum width for repository names in the dashboard.
Repository names longer than this will be truncated."
  :group 'approve-dashboard
  :type 'integer)

(defcustom approve-dashboard-empty-message "No pull requests found."
  "Message to display when a section has no results."
  :group 'approve-dashboard
  :type 'string)

;;; Buffer-local State

(defvar-local approve-dashboard--section-data nil
  "Alist mapping section titles to their fetched data.
Each entry is (TITLE . DATA) where DATA is the search results.")

(defvar-local approve-dashboard--section-loading nil
  "List of section titles currently being loaded.")

(defvar-local approve-dashboard--section-errors nil
  "Alist mapping section titles to error messages.")

;;; Mode Definition

(defvar approve-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'approve-dashboard-open-pr)
    (define-key map (kbd "g") #'approve-dashboard-refresh)
    (define-key map (kbd "G") #'approve-dashboard-refresh-section)
    (define-key map (kbd "o") #'approve-dashboard-browse-pr)
    map)
  "Keymap for `approve-dashboard-mode'.")

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal approve-dashboard-mode-map
    (kbd "RET") #'approve-dashboard-open-pr
    "gr" #'approve-dashboard-refresh
    "gR" #'approve-dashboard-refresh-section
    "o" #'approve-dashboard-browse-pr))

(define-derived-mode approve-dashboard-mode magit-section-mode "Approve-Dashboard"
  "Major mode for displaying a GitHub Pull Request dashboard.

\\{approve-dashboard-mode-map}"
  :group 'approve-dashboard
  (setq-local revert-buffer-function #'approve-dashboard-refresh
              truncate-lines t
              buffer-read-only t))

;;; Section Types

(defclass approve-dashboard-section (magit-section) ()
  "A dashboard section containing PR search results.")

(defclass approve-dashboard-pr-section (magit-section) ()
  "A single PR entry within a dashboard section.")

;;; PR Formatting

(defun approve-dashboard--format-pr-line (pr)
  "Format a complete PR line from PR data."
  (concat
   (approve-pr-format-review-status pr) " "
   (approve-pr-format-number pr 6) " "
   (approve-pr-pad-string
    (approve-pr-format-title pr approve-dashboard-title-width t)
    approve-dashboard-title-width) " "
   (approve-pr-pad-string
    (approve-pr-format-repo pr approve-dashboard-repo-width)
    approve-dashboard-repo-width) " "
   (approve-pr-format-time pr 5)))

;;; Section Rendering

(defun approve-dashboard--insert-pr (pr)
  "Insert a single PR entry for PR data."
  (let ((url (approve-pr-get-url pr)))
    (magit-insert-section (approve-dashboard-pr url)
      (insert (approve-dashboard--format-pr-line pr))
      (insert "\n"))))

(defun approve-dashboard--insert-section-content (title)
  "Insert the content for section with TITLE."
  (cond
   ;; Loading state
   ((member title approve-dashboard--section-loading)
    (insert "  ")
    (insert (approve-ui-propertize-face "Loading..." 'approve-dashboard-loading-face))
    (insert "\n"))
   ;; Error state
   ((alist-get title approve-dashboard--section-errors nil nil #'string=)
    (insert "  ")
    (insert (approve-ui-propertize-face
             (format "Error: %s"
                     (alist-get title approve-dashboard--section-errors nil nil #'string=))
             'approve-dashboard-error-face))
    (insert "\n"))
   ;; Data loaded
   ((alist-get title approve-dashboard--section-data nil nil #'string=)
    (let* ((data (alist-get title approve-dashboard--section-data nil nil #'string=))
           (nodes (alist-get 'nodes data)))
      (if (null nodes)
          (progn
            (insert "  ")
            (insert (approve-ui-propertize-face
                     approve-dashboard-empty-message
                     'approve-dashboard-empty-face))
            (insert "\n"))
        (dolist (pr nodes)
          (approve-dashboard--insert-pr pr)))))
   ;; No data yet (initial state)
   (t
    (insert "  ")
    (insert (approve-ui-propertize-face "Not loaded" 'approve-dashboard-loading-face))
    (insert "\n"))))

(defun approve-dashboard--get-section-count (title)
  "Get the PR count for section with TITLE."
  (when-let ((data (alist-get title approve-dashboard--section-data nil nil #'string=)))
    (length (alist-get 'nodes data))))

(defun approve-dashboard--insert-section (section-config)
  "Insert a dashboard section from SECTION-CONFIG plist."
  (let* ((title (plist-get section-config :title))
         (count (approve-dashboard--get-section-count title))
         (count-str (if count (format " (%d)" count) "")))
    (magit-insert-section (approve-dashboard-section title t)
      (magit-insert-heading
        (approve-ui-propertize-face title 'approve-dashboard-section-title-face)
        (approve-ui-propertize-face count-str 'approve-dashboard-section-count-face))
      (approve-dashboard--insert-section-content title))))

(defun approve-dashboard--render ()
  "Render the dashboard buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (approve-dashboard-root)
      (dolist (section-config approve-dashboard-sections)
        (approve-dashboard--insert-section section-config)
        (insert "\n")))))

;;; Data Fetching

(defun approve-dashboard--resolve-filter (filter)
  "Resolve FILTER to a string.
If FILTER is a function, call it and return the result.
If FILTER is a string, return it as-is."
  (cond
   ((functionp filter) (funcall filter))
   ((stringp filter) filter)
   (t (error "Invalid filter type: %S" filter))))

(defun approve-dashboard--fetch-section (section-config)
  "Fetch data for SECTION-CONFIG and update the buffer."
  (let* ((title (plist-get section-config :title))
         (filter-spec (plist-get section-config :filter))
         (filter (approve-dashboard--resolve-filter filter-spec))
         (limit (or (plist-get section-config :limit)
                    approve-dashboard-default-limit))
         (buffer (current-buffer)))
    ;; Mark as loading
    (push title approve-dashboard--section-loading)
    ;; Remove any previous error
    (setq approve-dashboard--section-errors
          (assoc-delete-all title approve-dashboard--section-errors))
    (approve-dashboard--render)
    ;; Fetch data
    (approve-api-search-pull-requests
     filter
     :limit limit
     :buffer buffer
     :callback
     (lambda (data)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           ;; Store data
           (setq approve-dashboard--section-data
                 (cons (cons title (alist-get 'search data))
                       (assoc-delete-all title approve-dashboard--section-data)))
           ;; Remove loading state
           (setq approve-dashboard--section-loading
                 (delete title approve-dashboard--section-loading))
           (approve-dashboard--render))))
     :error-callback
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           ;; Store error
           (setq approve-dashboard--section-errors
                 (cons (cons title (format "%s" error))
                       (assoc-delete-all title approve-dashboard--section-errors)))
           ;; Remove loading state
           (setq approve-dashboard--section-loading
                 (delete title approve-dashboard--section-loading))
           (approve-dashboard--render)))))))

(defun approve-dashboard--fetch-all-sections ()
  "Fetch data for all configured sections."
  (dolist (section-config approve-dashboard-sections)
    (approve-dashboard--fetch-section section-config)))

;;; Interactive Commands

(defun approve-dashboard-open-pr ()
  "Open the PR at point in an Approve review buffer."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq 'approve-dashboard-pr (oref section type))
      (let ((url (oref section value)))
        (when url
          (approve-view-pr url))))))

(defun approve-dashboard-browse-pr ()
  "Open the PR at point in a web browser."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq 'approve-dashboard-pr (oref section type))
      (let ((url (oref section value)))
        (when url
          (browse-url url))))))

(defun approve-dashboard-refresh (&rest _args)
  "Refresh all sections in the dashboard."
  (interactive)
  ;; Clear all cached data
  (setq approve-dashboard--section-data nil
        approve-dashboard--section-loading nil
        approve-dashboard--section-errors nil)
  ;; Fetch fresh data
  (approve-dashboard--fetch-all-sections))

(defun approve-dashboard-refresh-section ()
  "Refresh only the section at point."
  (interactive)
  (when-let ((section (magit-current-section)))
    ;; Navigate up to the dashboard section if we're on a PR
    (while (and section (not (eq 'approve-dashboard-section (oref section type))))
      (setq section (oref section parent)))
    (when (eq 'approve-dashboard-section (oref section type))
      (let* ((title (oref section value))
             (section-config (cl-find title approve-dashboard-sections
                                      :key (lambda (s) (plist-get s :title))
                                      :test #'string=)))
        (when section-config
          ;; Clear this section's data
          (setq approve-dashboard--section-data
                (assoc-delete-all title approve-dashboard--section-data))
          (approve-dashboard--fetch-section section-config))))))

;;; Entry Point

(defconst approve-dashboard--buffer-name "*Approve Dashboard*"
  "Name of the dashboard buffer.")

;;;###autoload
(defun approve-dashboard ()
  "Open the Approve PR dashboard.
Displays configurable sections of GitHub Pull Requests based on
search filters defined in `approve-dashboard-sections'."
  (interactive)
  (let ((buffer (get-buffer-create approve-dashboard--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'approve-dashboard-mode)
        (approve-dashboard-mode))
      (approve-dashboard-refresh))
    (switch-to-buffer buffer)))

(provide 'approve-dashboard)
;;; approve-dashboard.el ends here
