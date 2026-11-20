;;; approve-dashboard.el --- The dashboard of pull requests -*- lexical-binding: t; -*-

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

;; The dashboard (docs/ui.org, "Dashboard"; docs/flows.org, F01;
;; decision D39): the lists of pull requests a review starts from, a
;; section each, as `approve-dashboard-sections' says.
;;
;;   Approve — pull requests                      @bjornlarsson · 10:42 · g refresh
;;
;;   ▾ Needs my review (3)
;;     ● #1234  Add retry logic to the GraphQL client  fuzzycode/approve  @alice  +340 −56  2h  ◐  ✗
;;   ▸ Recently opened (10)
;;
;; - A list is a GitHub search (a string, or a function that returns
;;   one), or the pull requests opened in this Emacs session
;;   (`approve-pr-recent'), each of them asked for anew.  approve-list.el
;;   draws the rows, laid out together for every list so their columns
;;   line up, in the narrowest window that shows the dashboard; it is
;;   drawn again when that window's width changes.
;; - `approve-dashboard' shows the buffer and loads every list again; g
;;   (`approve-dashboard-refresh') does the same, G
;;   (`approve-dashboard-refresh-section') loads the list at point.  A
;;   list keeps its rows while it loads, and an answer to a request
;;   made before the list's last one is dropped.
;; - RET, o, y, TAB and K are the PR buffer's commands (approve-nav.el,
;;   approve-eldoc.el): on a row they open, copy or describe its pull
;;   request; on the heading of a search, o opens the search on GitHub
;;   and y copies it.  TAB folds a list, T every list, O every list but
;;   the one at point (in evil, zo zc za one list, zM zm and zR zr every
;;   list, zp the others).
;; - Like a PR buffer, the dashboard shows a token error once, instead
;;   of in every list.
;;
;; Layer rules: this is the buffers layer.  It runs queries
;; (approve-queries) and draws what they answer with the renderers.  It
;; keeps GitHub's answers as they are, in the buffer: lists have no
;; store.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'magit-section)
(require 'approve-api)
(require 'approve-queries)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-list)
(require 'approve-pr)
(require 'approve-sections)
(require 'approve-nav)
(require 'approve-eldoc)

;;; Options

(defcustom approve-dashboard-sections
  '((:title "Needs my review"
     :filter "is:open review-requested:@me sort:updated-desc")
    (:title "My pull requests"
     :filter "is:open author:@me sort:updated-desc")
    (:title "Involved"
     :filter "is:open involves:@me -author:@me -review-requested:@me sort:updated-desc"
     :limit 10 :hidden t)
    (:title "Recently opened" :recent t :limit 10 :hidden t))
  "The lists of the dashboard, in order.
Each is a plist:

  :title   the heading of the list; each list needs its own
  :filter  a GitHub search, such as \"is:open author:@me\", or a
           function of no arguments that returns one when the list
           loads.  \"is:pr\" is added when it says nothing of the type.
  :recent  non-nil for the pull requests opened in this Emacs
           session, newest first, instead of a search
  :limit   how many pull requests it shows, at most 100;
           `approve-dashboard-limit' when absent
  :hidden  non-nil to show it collapsed at first

A search finds pull requests in every state unless it says
\"is:open\", in the order GitHub likes unless it says \"sort:\".
For the open pull requests of an organization, newest first:

  (add-to-list \\='approve-dashboard-sections
               \\='(:title \"My team\"
                 :filter (lambda () (format \"is:open org:%s\" my-org))
                 :limit 10)
               t)"
  :type '(repeat (plist :options ((:title string)
                                  (:filter (choice string function))
                                  (:recent boolean)
                                  (:limit natnum)
                                  (:hidden boolean))))
  :group 'approve)

(defcustom approve-dashboard-limit 20
  "How many pull requests a list of the dashboard shows, at most.
A list's :limit in `approve-dashboard-sections' says otherwise for
it.  GitHub sends at most 100 at once.

  (setq approve-dashboard-limit 30)"
  :type 'natnum
  :group 'approve)

;;; State

(defconst approve-dashboard-buffer-name "*approve-dashboard*"
  "The name of the dashboard's buffer.")

(defvar-local approve-dashboard--lists nil
  "What the lists of the dashboard hold, as an alist of (TITLE . STATE).
STATE is a plist:
:loaded   non-nil once a request for it has answered
:prs      the pull requests of the last answer, in order
:total    how many there are in all: what the search finds, or the
          rows of the recently opened
:search   the search it sent, is:pr included, or nil
:loading  non-nil while a request for it is out
:error    the error of its last request, or nil
:serial   the number of its last request; the answers to earlier
          ones are dropped.")

(defvar approve-dashboard--serial 0
  "The number of the last request for a list of the dashboard.")

(defvar-local approve-dashboard--viewer nil
  "The viewer, a user alist, once GitHub has said who it is.")

(defvar-local approve-dashboard--refreshed nil
  "When every list of the dashboard was last loaded again, or nil.")

(defvar-local approve-dashboard--fatal nil
  "A token error, shown instead of the lists, or nil.")

(defvar-local approve-dashboard--redraw-timer nil
  "Timer of the scheduled redraw, or nil.")

(defvar-local approve-dashboard--window-width nil
  "The width of the windows the dashboard was last drawn for, or nil.
It is `approve-render-window-width' then; its rows fit it.")

(defun approve-dashboard--state (title)
  "Return the state of the list TITLE, a plist, or nil."
  (cdr (assoc title approve-dashboard--lists)))

(defun approve-dashboard--put (title &rest props)
  "Set PROPS, keys and values, in the state of the list TITLE."
  (let ((state (approve-dashboard--state title)))
    (cl-loop for (key value) on props by #'cddr
             do (setq state (plist-put state key value)))
    (setf (alist-get title approve-dashboard--lists nil nil #'equal) state)))

(defun approve-dashboard--limit (spec)
  "Return how many pull requests the list of SPEC shows, at most."
  (min 100 (or (plist-get spec :limit) approve-dashboard-limit)))

(defun approve-dashboard--filter (spec)
  "Return the search of SPEC: its :filter, or what that function returns."
  (let ((filter (plist-get spec :filter)))
    (cond ((functionp filter) (funcall filter))
          ((stringp filter) filter)
          (t (error "The list %S has neither a :filter nor :recent"
                    (plist-get spec :title))))))

(defun approve-dashboard--search-url (search)
  "Return the URL of SEARCH on GitHub, the pull requests it finds."
  (format "https://%s/search?q=%s&type=pullrequests"
          (approve-api-web-host) (url-hexify-string search)))

;;; Loading

(defun approve-dashboard--note-fatal (err)
  "Make ERR the error of the whole dashboard if it is a token error.
Every request fails the same way then; the echo area says it once."
  (when (and (eq (car-safe err) 'approve-api-unauthorized)
             (not approve-dashboard--fatal))
    (setq approve-dashboard--fatal err)
    (message "Approve: could not load the dashboard: %s"
             (car (split-string (approve-api-error-message err) "\n")))))

(defun approve-dashboard--load-search (spec answer)
  "Ask GitHub for the pull requests of SPEC's search; call ANSWER with them.
ANSWER takes the keys and values to set in the list's state."
  (let ((search (approve-query-pull-request-search (approve-dashboard--filter spec))))
    (approve-dashboard--put (plist-get spec :title) :search search)
    (approve-query-search
     search
     :first (approve-dashboard--limit spec)
     :callback (lambda (result)
                 (funcall answer :loaded t :prs (alist-get 'nodes result)
                          :total (alist-get 'issueCount result)))
     :errorback (lambda (err) (funcall answer :error err)))))

(defun approve-dashboard--load-recent (spec answer)
  "Ask GitHub for the pull requests opened in the session, as SPEC limits.
Each is asked for by itself, all at once; ANSWER is called once
they all answered, with the rows in the order they were opened.  A
pull request GitHub no longer has, or that the token can't read, is
left out; any other error is the list's."
  (let* ((prs (seq-take (approve-pr-recent) (approve-dashboard--limit spec)))
         (rows (make-vector (length prs) nil))
         (left (length prs))
         (failure nil)
         (settle (lambda ()
                   (when (zerop (cl-decf left))
                     (if failure
                         (funcall answer :error failure)
                       (let ((rows (delq nil (append rows nil))))
                         (funcall answer :loaded t :prs rows :total (length rows))))))))
    (if (null prs)
        (funcall answer :loaded t :prs nil :total 0)
      (seq-do-indexed
       (lambda (pr index)
         (approve-query-summary
          pr
          :callback (lambda (row)
                      (aset rows index row)
                      (funcall settle))
          :errorback (lambda (err)
                       (unless (eq (car-safe err) 'approve-api-not-found)
                         (setq failure (or failure err)))
                       (funcall settle))))
       prs))))

(defun approve-dashboard--load (spec)
  "Load the list of SPEC, an entry of `approve-dashboard-sections', again.
Its rows stay until the answer arrives, which redraws the buffer.
The answer to an earlier request for the list is dropped."
  (let* ((title (plist-get spec :title))
         (serial (cl-incf approve-dashboard--serial))
         (buffer (current-buffer))
         (answer (lambda (&rest props)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (eql serial (plist-get (approve-dashboard--state title) :serial))
                         (apply #'approve-dashboard--put title :loading nil props)
                         (approve-dashboard--note-fatal (plist-get props :error))
                         (approve-dashboard--schedule-redraw)))))))
    (approve-dashboard--put title :serial serial :loading t :error nil)
    (condition-case err
        (if (plist-get spec :recent)
            (approve-dashboard--load-recent spec answer)
          (approve-dashboard--load-search spec answer))
      ;; A filter that fails, or a list without one, is that list's error.
      (error (funcall answer :error err)))))

(defun approve-dashboard--load-viewer ()
  "Ask GitHub who the viewer is, for the title line."
  (approve-query-viewer
   :callback (lambda (viewer)
               (setq approve-dashboard--viewer viewer)
               (approve-dashboard--schedule-redraw))
   :errorback (lambda (err)
                (approve-dashboard--note-fatal err)
                (approve-dashboard--schedule-redraw))))

;;; Drawing

(defclass approve-dashboard-list-section (magit-section) ()
  "A list of the dashboard; its value is the list's title.")

(defclass approve-dashboard-more-section (magit-section) ()
  "The line that says how many more pull requests a search finds.")

(defun approve-dashboard--title-line (width)
  "Return the title line: what the buffer is, who the viewer is, and when.
Its right part ends at column WIDTH."
  (approve-render-justify
   (approve-face "Approve — pull requests" 'approve-title)
   (approve-render-join
    (list (and approve-dashboard--viewer (approve-render-user approve-dashboard--viewer))
          (and approve-dashboard--refreshed
               (approve-face (format-time-string "%H:%M" approve-dashboard--refreshed)
                             'approve-dim))
          (approve-render-hints '(approve-dashboard-refresh "refresh"))))
   width))

(defun approve-dashboard--search-thing (spec state)
  "Return the thing of the search of SPEC, whose list has STATE, or nil."
  (when-let* ((search (plist-get state :search)))
    (list :kind 'search :title (plist-get spec :title) :search search
          :total (plist-get state :total) :url (approve-dashboard--search-url search))))

(defun approve-dashboard--insert-more (count thing)
  "Insert the line that says COUNT more pull requests are found.
It carries THING, the list's search, which `approve-browse' opens."
  (magit-insert-section (approve-dashboard-more-section)
    (insert (approve-thing
             (concat approve-list-indentation
                     (approve-face (format "… %d more" count) 'approve-dim)
                     (when-let* ((hint (approve-render-hints
                                        '(approve-browse "show them on GitHub"))))
                       (concat (approve-face " · " 'approve-dim) hint))
                     "\n")
             thing))))

(defun approve-dashboard--rows (spec)
  "Return the pull requests the list of SPEC shows, in order.
None before its first answer, or when its last request failed."
  (let ((state (approve-dashboard--state (plist-get spec :title))))
    (and (plist-get state :loaded)
         (not (plist-get state :error))
         (plist-get state :prs))))

(defun approve-dashboard--insert-list (spec layout width)
  "Insert the list of SPEC, an entry of `approve-dashboard-sections'.
Its rows are laid out by LAYOUT, from `approve-list-layout'; what
its heading has on the right ends at column WIDTH."
  (let* ((title (plist-get spec :title))
         (state (approve-dashboard--state title))
         (prs (plist-get state :prs))
         (total (plist-get state :total))
         (thing (approve-dashboard--search-thing spec state))
         (shown (and (plist-get state :loaded) (not (plist-get state :error)))))
    (magit-insert-section (approve-dashboard-list-section title (plist-get spec :hidden))
      (let ((heading (approve-sections-heading
                      title (and shown total)
                      (and shown (plist-get state :loading)
                           (concat (approve-icon 'loading 'approve-loading) " "
                                   (approve-face "loading…" 'approve-loading)))
                      width)))
        (magit-insert-heading (if thing (approve-thing heading thing) heading)))
      (cond
       ((plist-get state :error)
        (insert approve-list-indentation
                (approve-render-failure "the pull requests" (plist-get state :error)
                                        'approve-dashboard-refresh-section)))
       ((not (plist-get state :loaded))
        (insert approve-list-indentation (approve-render-loading)))
       (t
        (approve-list-insert prs (if (plist-get spec :recent)
                                     "None opened in this session."
                                   "No pull requests.")
                             layout)
        (when (and total (> total (length prs)))
          (approve-dashboard--insert-more (- total (length prs)) thing)))))))

(defun approve-dashboard--insert ()
  "Insert the dashboard: its title line, then its lists or a token error.
The rows of every list are laid out together, so their columns line
up, in the narrowest window that shows the dashboard; what is on the
right of the title line and the headings ends where the rows do."
  (let* ((prs (seq-mapcat #'approve-dashboard--rows approve-dashboard-sections))
         (layout (approve-list-layout prs))
         (width (if prs (approve-list-layout-width layout) (approve-render-width))))
    (setq approve-dashboard--window-width (approve-render-window-width))
    (insert (approve-dashboard--title-line width) "\n\n")
    (if approve-dashboard--fatal
        (approve-render-insert-fatal "Could not use the GitHub token."
                                     (approve-api-error-message approve-dashboard--fatal)
                                     (approve-pr-token-hint)
                                     'approve-dashboard-refresh)
      (dolist (spec approve-dashboard-sections)
        (approve-dashboard--insert-list spec layout width)))))

(defun approve-dashboard--cancel-redraw ()
  "Cancel the scheduled redraw of the current buffer."
  (when (timerp approve-dashboard--redraw-timer)
    (cancel-timer approve-dashboard--redraw-timer))
  (setq approve-dashboard--redraw-timer nil))

(defun approve-dashboard-redraw ()
  "Draw the dashboard again from what its lists hold.
Point stays on the same row, and collapsed lists stay collapsed."
  (approve-dashboard--cancel-redraw)
  (approve-redraw-sections #'approve-dashboard--insert))

(defun approve-dashboard--redraw-now (buffer)
  "Redraw BUFFER, the dashboard, if it is alive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (approve-dashboard-redraw))))

(defun approve-dashboard--schedule-redraw ()
  "Redraw the dashboard after `approve-pr-redraw-delay'.
Answers that arrive before then share that one redraw."
  (unless (timerp approve-dashboard--redraw-timer)
    (setq approve-dashboard--redraw-timer
          (run-at-time approve-pr-redraw-delay nil
                       #'approve-dashboard--redraw-now (current-buffer)))))

(defun approve-dashboard--window-resized (_window)
  "Redraw the dashboard when the windows that show it are no longer as wide.
Its rows fit the narrowest one.  It is in the dashboard's
`window-size-change-functions', which also run when a window starts
showing it."
  (unless (eql approve-dashboard--window-width (approve-render-window-width))
    (approve-dashboard--schedule-redraw)))

;;; Commands

(defun approve-dashboard--spec-at-point ()
  "Return the entry of `approve-dashboard-sections' of the list at point.
Nil when point is in none."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'approve-dashboard-list-section)))
      (setq section (oref section parent)))
    (and section
         (seq-find (lambda (spec) (equal (plist-get spec :title) (oref section value)))
                   approve-dashboard-sections))))

(defun approve-dashboard--check-mode ()
  "Signal a `user-error' unless the current buffer is the dashboard."
  (unless (derived-mode-p 'approve-dashboard-mode)
    (user-error "Not in the Approve dashboard")))

(defun approve-dashboard-refresh (&optional clear-caches)
  "Load every list of the dashboard again.
The lists keep their rows until the new ones arrive.  With a prefix
argument CLEAR-CACHES, also ask who the viewer is and forget cached
rendering and extended details (`approve-pr-clear-cache-functions')."
  (interactive "P")
  (approve-dashboard--check-mode)
  (when clear-caches
    (setq approve-dashboard--viewer nil)
    (run-hooks 'approve-pr-clear-cache-functions))
  (let ((titles (mapcar (lambda (spec) (plist-get spec :title)) approve-dashboard-sections)))
    (setq approve-dashboard--lists
          (seq-filter (lambda (entry) (member (car entry) titles)) approve-dashboard--lists)))
  (setq approve-dashboard--fatal nil
        approve-dashboard--refreshed (current-time))
  (unless approve-dashboard--viewer
    (approve-dashboard--load-viewer))
  (dolist (spec approve-dashboard-sections)
    (approve-dashboard--load spec))
  (approve-dashboard-redraw))

(defun approve-dashboard--list-preflight ()
  "Return the entry of the list at point, or say to move to one.
It is the preflight of `approve-dashboard-refresh-section', which
the dashboard's menu asks whether to dim it (decision D06)."
  (or (approve-dashboard--spec-at-point)
      (user-error "Put point on a list of the dashboard")))

(defun approve-dashboard-refresh-section ()
  "Load the list at point again; the others stay as they are."
  (interactive)
  (approve-dashboard--check-mode)
  (approve-dashboard--load (approve-dashboard--list-preflight))
  (approve-dashboard-redraw))

(put 'approve-dashboard-refresh-section 'approve-preflight
     #'approve-dashboard--list-preflight)

(defun approve-dashboard--list-sections ()
  "Return the sections of the dashboard's lists, in order."
  (and magit-root-section
       (seq-filter (lambda (section)
                     (eq (oref section type) 'approve-dashboard-list-section))
                   (oref magit-root-section children))))

(defun approve-dashboard-lists-folded-p ()
  "Return non-nil if every list of the dashboard is folded."
  (seq-every-p (lambda (section) (oref section hidden))
               (approve-dashboard--list-sections)))

(defun approve-dashboard-fold-lists ()
  "Fold every list of the dashboard to its heading.
Point in a list goes to its heading.  Each list stays folded when
the dashboard loads again, until it is unfolded."
  (interactive)
  (approve-dashboard--check-mode)
  (mapc #'magit-section-hide (approve-dashboard--list-sections)))

(defun approve-dashboard-unfold-lists ()
  "Unfold every list of the dashboard."
  (interactive)
  (approve-dashboard--check-mode)
  (mapc #'magit-section-show (approve-dashboard--list-sections)))

(defun approve-dashboard-toggle-lists ()
  "Fold every list of the dashboard, or unfold them all if they are folded."
  (interactive)
  (if (approve-dashboard-lists-folded-p)
      (approve-dashboard-unfold-lists)
    (approve-dashboard-fold-lists)))

;;; Mode

(defvar-keymap approve-dashboard-mode-map
  :doc "Keymap of `approve-dashboard-mode'."
  :parent magit-section-mode-map)

;; Bound here rather than in the `defvar-keymap', which leaves a map
;; that exists as it is: loading this file again then binds new keys.
(keymap-set approve-dashboard-mode-map "g" #'approve-dashboard-refresh)
(keymap-set approve-dashboard-mode-map "G" #'approve-dashboard-refresh-section)
(keymap-set approve-dashboard-mode-map "RET" #'approve-visit)
(keymap-set approve-dashboard-mode-map "<remap> <magit-section-toggle>"
            #'approve-section-toggle)
(keymap-set approve-dashboard-mode-map "T" #'approve-dashboard-toggle-lists)
(keymap-set approve-dashboard-mode-map "O" #'approve-fold-others)
(keymap-set approve-dashboard-mode-map "o" #'approve-browse)
(keymap-set approve-dashboard-mode-map "y" #'approve-copy)
(keymap-set approve-dashboard-mode-map "K" #'approve-show-details)

(define-derived-mode approve-dashboard-mode magit-section-mode "Approve-Dashboard"
  "Major mode of the dashboard of pull requests.
Its lists come from `approve-dashboard-sections';
\\[approve-dashboard-refresh] loads them again, \\[approve-dashboard-refresh-section] the one at point.

\\{approve-dashboard-mode-map}"
  :group 'approve
  (setq-local revert-buffer-function #'approve-dashboard--revert)
  (setq-local truncate-lines t)
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'approve-render-strip-decorations '((depth . 90)))
  (add-hook 'kill-buffer-hook #'approve-dashboard--cancel-redraw nil t)
  (add-hook 'window-size-change-functions #'approve-dashboard--window-resized nil t)
  (approve-eldoc-setup)
  ;; Evil's bindings load with the first buffer that may need them
  ;; (decision D17); evil sets this buffer's keymaps up after this.
  (when (featurep 'evil)
    (require 'approve-evil)))

(defun approve-dashboard--revert (&rest _)
  "Load every list again, as `revert-buffer-function'."
  (approve-dashboard-refresh))

;;;###autoload
(defun approve-dashboard ()
  "Show the dashboard of pull requests, every list loaded anew (flow F01).
The lists are those of `approve-dashboard-sections': searches on
GitHub, and the pull requests opened in this Emacs session.  RET on
a row opens its pull request.  Return the buffer."
  (interactive)
  (let ((buffer (get-buffer-create approve-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'approve-dashboard-mode)
        (approve-dashboard-mode)))
    ;; Shown first, so that its rows fit the window from the start.
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (approve-dashboard-refresh))
    buffer))

(provide 'approve-dashboard)

;;; approve-dashboard.el ends here
