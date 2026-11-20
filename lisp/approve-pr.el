;;; approve-pr.el --- The pull request buffer -*- lexical-binding: t; -*-

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

;; The buffer that shows one pull request, `approve-pr-mode'
;; (docs/ui.org, "PR buffer"; docs/flows.org, F01, F02 and F17).
;;
;; - `approve-pr-open' finds or makes the buffer of a PR, draws its
;;   skeleton and runs the loading plan (`approve-pr-load').  It
;;   remembers the PRs opened in the session, newest first
;;   (`approve-pr-recent'), for the dashboard's "Recently opened".
;; - Each query of the plan is tracked in the session under its key:
;;   :loading while it runs, :errors when it failed.  Every page that
;;   arrives schedules a redraw; redraws coalesce
;;   (`approve-pr-redraw-delay').
;; - `approve-pr-redraw' erases the buffer and runs
;;   `approve-pr-sections-hook' (decision D13).  Point stays on the
;;   same section, line and column; collapsed sections stay collapsed.
;;   `approve-redraw-sections' does that part, for every Approve
;;   buffer of sections (the dashboard too).
;; - The header line sums up the PR and what is still loading.
;; - `g' (`approve-pr-refresh') runs the plan again, keeping the text
;;   on screen until data arrives; each list is replaced whole.
;; - Data some sections need only when expanded (the checks) is loaded
;;   after the command that expanded them (`approve-pr-lazy-loaders').
;;
;; Layer rules: this is the buffers layer.  It runs operations
;; (approve-queries) and renderers; renderers never call it back except
;; through the section hook.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-api)
(require 'approve-api-url)
(require 'approve-model)
(require 'approve-session)
(require 'approve-queries)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-html)
(require 'approve-render)

;;; Options

(defcustom approve-pr-sections-hook
  '(approve-insert-header-section
    approve-insert-pending-review-section
    approve-insert-description-section
    approve-insert-checks-section
    approve-insert-commits-section
    approve-insert-conversation-section
    approve-insert-diff-section)
  "Functions that insert the sections of a pull request buffer.
Each is called with no arguments, at point, in the PR buffer, and
reads the store through selectors.  The default ones are defined in
approve-sections.el and the files it names.  Change it with
`approve-add-section-hook', for example to drop the commits:

  (remove-hook \\='approve-pr-sections-hook
               #\\='approve-insert-commits-section)

The list of changed files, with their sizes and the threads on
whole files, is not shown by default (decision D05, amended).  This
puts it back, before the diff:

  (approve-add-section-hook \\='approve-pr-sections-hook
                            #\\='approve-insert-files-section
                            #\\='approve-insert-diff-section)"
  :type 'hook
  :group 'approve)

(defcustom approve-pr-redraw-delay 0.05
  "Seconds to wait before redrawing after data arrived.
Data that arrives meanwhile is drawn in the same redraw."
  :type 'number
  :group 'approve)

(defvar approve-pr-lazy-loaders nil
  "Data loaded when a section that shows it is expanded.
Each entry is (KEY . PREDICATE): after each command, when PREDICATE
returns non-nil in the PR buffer and the query KEY (see
`approve-query-run') is neither loaded, loading nor failed, it is
run.  The sections that need such data add entries.")

;;; Mode

(defvar-keymap approve-pr-mode-map
  :doc "Keymap of `approve-pr-mode'."
  :parent magit-section-mode-map
  "g" #'approve-pr-refresh)

(defvar-local approve-pr--header-line nil
  "The header line, computed at each redraw.")
;; Without this, the header line would drop the text's faces and the
;; space that aligns the state on the right.
(put 'approve-pr--header-line 'risky-local-variable t)

(defvar-local approve-pr--redraw-timer nil
  "Timer of the scheduled redraw, or nil.")

(defvar approve-pr-redraw-hook nil
  "Hook run in a PR buffer after each redraw.")

(defvar approve-pr-load-hook nil
  "Hook run in a PR buffer when its loading plan starts.
That is a first load and each refresh (`approve-pr-load').  Use it to
forget what the data being loaded makes stale.")

(defvar approve-pr-clear-cache-functions (list #'approve-html-clear-cache)
  "Functions that forget session caches, run by \\[universal-argument] \\[approve-pr-refresh].")

(define-derived-mode approve-pr-mode magit-section-mode "Approve"
  "Major mode of a buffer showing one GitHub pull request.
The buffer is read-only and drawn from data loaded from GitHub;
\\[approve-pr-refresh] loads it again.

\\{approve-pr-mode-map}"
  :group 'approve
  (setq-local revert-buffer-function #'approve-pr--revert)
  (setq-local magit-section-highlight-current nil)
  (setq-local header-line-format 'approve-pr--header-line)
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'approve-render-strip-decorations '((depth . 90)))
  (add-hook 'post-command-hook #'approve-pr--run-lazy-loaders nil t)
  (add-hook 'kill-buffer-hook #'approve-pr--cancel-redraw nil t)
  ;; Evil's bindings load with the first buffer that may need them
  ;; (decision D17); evil sets this buffer's keymaps up after this.
  (when (featurep 'evil)
    (require 'approve-evil)))

(defun approve-pr--revert (&rest _)
  "Refresh the PR, as `revert-buffer-function'."
  (approve-pr-refresh))

;;; Buffers

(defconst approve-pr--url-regexp
  (rx string-start
      "http" (? "s") "://"
      (group (+ (not (in "/"))))         ; host
      "/" (group (+ (not (in "/"))))     ; owner
      "/" (group (+ (not (in "/"))))     ; repository
      "/pull/" (group (+ digit))          ; number
      (? (in "/?#") (* anything))
      string-end)
  "Regexp matching a pull request URL.
Groups: 1 host, 2 owner, 3 repository, 4 number.")

(defun approve-parse-url (url)
  "Parse URL of a GitHub pull request.
Return a plist (:host HOST :owner OWNER :repo REPO :number NUMBER),
or nil when URL is not a pull request URL.  HOST is the web host as
written in URL, such as \"github.com\" or a GitHub Enterprise host.
Anything after the number (\"/files\", a query, an anchor) is ignored.

Example:
  (approve-parse-url \"https://github.com/magit/magit/pull/5000/files\")
  => (:host \"github.com\" :owner \"magit\" :repo \"magit\" :number 5000)"
  (when (stringp url)
    (let ((url (string-trim url)))
      (when (string-match approve-pr--url-regexp url)
        (list :host (downcase (match-string 1 url))
              :owner (match-string 2 url)
              :repo (match-string 3 url)
              :number (string-to-number (match-string 4 url)))))))

(defun approve-pr-buffer-name (owner repo number)
  "Return the name of the buffer of PR NUMBER in OWNER/REPO."
  (format "*approve: %s/%s#%s*" owner repo number))

(defun approve-pr-buffer (pr)
  "Return the live PR buffer of PR, a plist from `approve-parse-url', or nil."
  (when-let* ((buffer (get-buffer (approve-pr-buffer-name
                                   (plist-get pr :owner) (plist-get pr :repo)
                                   (plist-get pr :number)))))
    (and (buffer-live-p buffer)
         (eq (buffer-local-value 'major-mode buffer) 'approve-pr-mode)
         buffer)))

(defun approve-pr-setup-buffer (pr)
  "Return a new PR buffer for PR, with its skeleton drawn.
PR is a plist with :host, :owner, :repo and :number.  Nothing is
loaded yet; see `approve-pr-load'."
  (let ((buffer (get-buffer-create
                 (approve-pr-buffer-name (plist-get pr :owner) (plist-get pr :repo)
                                         (plist-get pr :number)))))
    (with-current-buffer buffer
      (approve-pr-mode)
      (approve-model-init (list :host (or (plist-get pr :host) "github.com")
                                :owner (plist-get pr :owner)
                                :repo (plist-get pr :repo)
                                :number (plist-get pr :number)))
      (approve-pr-redraw))
    buffer))

(defconst approve-pr--recent-kept 50
  "How many pull requests `approve-pr-recent' remembers.")

(defvar approve-pr--recent nil
  "The pull requests opened in this session, newest first.
Each is a plist from `approve-parse-url'.")

(defun approve-pr-recent ()
  "Return the pull requests opened in this Emacs session, newest first.
Each is a plist from `approve-parse-url', (:host :owner :repo
:number); a pull request is listed once, where it was last opened.
The list is kept in memory only, for the session (decision D39)."
  approve-pr--recent)

(defun approve-pr--same-p (a b)
  "Return non-nil if A and B, plists from `approve-parse-url', are one PR.
Host, owner and repository are compared ignoring case, as GitHub does."
  (and (equal (plist-get a :number) (plist-get b :number))
       (cl-every (lambda (key)
                   (string-equal-ignore-case (or (plist-get a key) "")
                                             (or (plist-get b key) "")))
                 '(:host :owner :repo))))

(defun approve-pr-remember (pr)
  "Put PR first in the pull requests opened in the session.
PR is a plist from `approve-parse-url'.  See `approve-pr-recent'."
  (let ((pr (list :host (or (plist-get pr :host) "github.com")
                  :owner (plist-get pr :owner) :repo (plist-get pr :repo)
                  :number (plist-get pr :number))))
    (setq approve-pr--recent
          (seq-take (cons pr (seq-remove (lambda (old) (approve-pr--same-p old pr))
                                         approve-pr--recent))
                    approve-pr--recent-kept))))

(defun approve-pr-open (pr)
  "Show the buffer of PR, loading it first if there is none (flow F01).
PR is a plist from `approve-parse-url'.  An existing buffer is shown
as it is; \\<approve-pr-mode-map>\\[approve-pr-refresh] loads it again.  Either way the PR goes first
in the ones opened in the session (`approve-pr-recent').  Return
the buffer."
  (approve-pr-remember pr)
  (let ((buffer (or (approve-pr-buffer pr)
                    (with-current-buffer (approve-pr-setup-buffer pr)
                      (approve-pr-load)
                      (current-buffer)))))
    (pop-to-buffer-same-window buffer)
    buffer))

;;; Loading

(defun approve-pr-loading-p (&optional key)
  "Return non-nil if the query KEY is running, or any query without KEY."
  (if key
      (approve-session-member-p :loading key)
    (and (approve-session-get :loading) t)))

(defun approve-pr-error (key)
  "Return the error of the query KEY, or nil."
  (approve-session-map-get :errors key))

(defun approve-pr-loaded-p (key)
  "Return non-nil if the query KEY has loaded at least once."
  (approve-session-member-p :loaded key))

(defun approve-pr--fatal-p (key err)
  "Return non-nil if ERR, from the query KEY, leaves nothing to show.
That is any error of the core query, which has the pull request
itself, and a token error from any query, as every query fails the
same way then."
  (or (eq key 'core) (eq (car-safe err) 'approve-api-unauthorized)))

(defun approve-pr--name ()
  "Return \"OWNER/REPO#NUMBER\" of the current PR buffer."
  (format "%s/%s#%s" (approve-model-meta :owner) (approve-model-meta :repo)
          (approve-model-meta :number)))

(defun approve-pr--note-fatal (key err)
  "Make ERR, from the query KEY, the error of the whole buffer if it is one.
The buffer then shows that error alone, instead of each section
saying it again.  A token error replaces another one, as it is the
cause.  The echo area says it too."
  (when (approve-pr--fatal-p key err)
    (let ((old (approve-session-get :fatal)))
      (when (or (null old)
                (and (eq (car-safe err) 'approve-api-unauthorized)
                     (not (eq (car-safe old) 'approve-api-unauthorized))))
        (approve-session-put :fatal err)
        (message "Approve: could not load %s: %s" (approve-pr--name)
                 (car (split-string (approve-api-error-message err) "\n")))))))

(defun approve-pr-run-query (key &optional atomic)
  "Run the query KEY for the current PR buffer, tracking it in the session.
With ATOMIC, the query replaces its data at the end only (a refresh).
Each page schedules a redraw."
  (let ((buffer (current-buffer)))
    (approve-session-add :loading key)
    (approve-session-map-remove :errors key)
    (approve-query-run
     key
     :atomic atomic
     :on-page (lambda (_page) (approve-pr-schedule-redraw buffer))
     :callback (lambda ()
                 (approve-session-remove :loading key)
                 (approve-session-add :loaded key)
                 (approve-pr-schedule-redraw buffer))
     :errorback (lambda (err)
                  (approve-session-remove :loading key)
                  (approve-session-map-put :errors key err)
                  (approve-pr--note-fatal key err)
                  (approve-pr-schedule-redraw buffer)))))

(defun approve-pr-load (&optional refresh)
  "Run the loading plan of the current PR buffer (flow F02).
With REFRESH, each query replaces its data once it has all of it,
and lazily loaded data that was loaded before is loaded again."
  (approve-session-put :fatal nil)
  (run-hooks 'approve-pr-load-hook)
  (dolist (entry approve-query-plan)
    (approve-pr-run-query (car entry) refresh))
  (when refresh
    (dolist (loader approve-pr-lazy-loaders)
      (when (approve-pr-loaded-p (car loader))
        (approve-pr-run-query (car loader) t))))
  (approve-pr-update-header-line)
  (approve-pr-schedule-redraw))

(defun approve-pr-refresh (&optional clear-caches)
  "Load the pull request of this buffer again (flow F17).
The buffer keeps its text until the data arrives.  With a prefix
argument CLEAR-CACHES, also forget cached rendering, images and
extended details (`approve-pr-clear-cache-functions')."
  (interactive "P")
  (unless (derived-mode-p 'approve-pr-mode)
    (user-error "Not in an Approve pull request buffer"))
  (when clear-caches
    (run-hooks 'approve-pr-clear-cache-functions))
  (approve-pr-load t))

(defun approve-pr-load-local-patch (path)
  "Compute the diff of PATH from its blobs, as GitHub sent none.
Tracked in the session under (diff . PATH); the buffer redraws when
it is in."
  (let ((buffer (current-buffer))
        (key (cons 'diff path)))
    (unless (approve-pr-loading-p key)
      (approve-session-add :loading key)
      (approve-session-map-remove :errors key)
      (approve-query-local-patch
       path
       :callback (lambda ()
                   (approve-session-remove :loading key)
                   (approve-pr-schedule-redraw buffer))
       :errorback (lambda (err)
                    (approve-session-remove :loading key)
                    (approve-session-map-put :errors key err)
                    (approve-pr-schedule-redraw buffer)))
      (approve-pr-redraw))))

(defun approve-pr--run-lazy-loaders ()
  "Run the lazy loaders whose data is wanted now and missing."
  (when (and (approve-model-root-ref) (not (approve-session-get :fatal)))
    (dolist (loader approve-pr-lazy-loaders)
      (let ((key (car loader)))
        (unless (or (approve-pr-loaded-p key)
                    (approve-pr-loading-p key)
                    (approve-pr-error key))
          (when (funcall (cdr loader))
            (approve-pr-run-query key)))))))

;;; Images

(defun approve-pr--fetch-images ()
  "Fetch the images the last redraw wanted; redraw as each arrives."
  (let ((buffer (current-buffer)))
    (dolist (url (approve-html-take-wanted-images))
      (approve-api-fetch-url
       url
       :callback (lambda (data)
                   (approve-html-put-image url data)
                   (approve-pr-schedule-redraw buffer))
       :errorback (lambda (_message)
                    (approve-html-put-image url nil))))))

;;; Redrawing

(defun approve-pr--cancel-redraw ()
  "Cancel the scheduled redraw of the current buffer."
  (when (timerp approve-pr--redraw-timer)
    (cancel-timer approve-pr--redraw-timer))
  (setq approve-pr--redraw-timer nil))

(defun approve-pr-schedule-redraw (&optional buffer)
  "Redraw BUFFER (default: the current one) after `approve-pr-redraw-delay'.
Calls before then share that one redraw."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (timerp approve-pr--redraw-timer)
          (setq approve-pr--redraw-timer
                (run-at-time approve-pr-redraw-delay nil
                             #'approve-pr--redraw-now buffer)))))))

(defun approve-pr--redraw-now (buffer)
  "Redraw BUFFER now if it is alive; the scheduled redraw."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq approve-pr--redraw-timer nil)
      (approve-pr-redraw))))

(defun approve-pr--position (window)
  "Return where point is in WINDOW, or in the buffer when WINDOW is nil.
The value is a plist: :ident of the section, :offset in lines from
its start, :column, :line, and :rows from the top of WINDOW."
  (let ((pos (if window (window-point window) (point))))
    (save-excursion
      (goto-char pos)
      (let ((section (or (magit-section-at) magit-root-section)))
        (list :window window
              :ident (and section (magit-section-ident section))
              :offset (if section
                          (count-lines (oref section start) (line-beginning-position))
                        0)
              :column (current-column)
              :line (line-number-at-pos)
              :rows (and window
                         (ignore-errors
                           (count-screen-lines (window-start window) pos nil window))))))))

(defun approve-pr--find-section (ident)
  "Return the section with IDENT, or its nearest ancestor that exists.
The value is (SECTION . EXACT), EXACT non-nil when it is IDENT's own.
The root does not count as an ancestor: then the value is nil."
  (let ((exact t) section)
    (while (and (cdr ident)
                (not (setq section (ignore-errors (magit-get-section ident)))))
      (setq ident (cdr ident) exact nil))
    (and section (cons section exact))))

(defun approve-pr--restore (position)
  "Move point back to POSITION from `approve-pr--position'.
Return the new point."
  (let ((found (and (plist-get position :ident)
                    (approve-pr--find-section (plist-get position :ident)))))
    (if found
        (let ((section (car found)))
          (goto-char (oref section start))
          (when (cdr found)
            (forward-line (plist-get position :offset))
            (when (>= (point) (oref section end))
              (goto-char (oref section end))
              (unless (bobp) (forward-line -1))))
          (move-to-column (plist-get position :column)))
      (goto-char (point-min))
      (forward-line (1- (plist-get position :line))))
    (point)))

(defun approve-pr--detach-markers ()
  "Detach the markers of the sections about to be erased.
A marker stays in its buffer until it is garbage-collected, and
every insertion moves each one, so a redraw would otherwise pay for
the markers of earlier redraws."
  (when magit-root-section
    (magit-map-sections
     (lambda (section)
       (dolist (marker (list (oref section start) (oref section content)
                             (oref section end)))
         (when (markerp marker)
           (set-marker marker nil))))
     magit-root-section)))

(defun approve-redraw-sections (insert)
  "Erase the current buffer and draw its sections again with INSERT.
INSERT is called with no arguments, at point, inside the root
section (`approve-root-section').  Point stays on the same section,
line and column, in each window showing the buffer, and collapsed
sections stay collapsed (decision D13).  Every Approve buffer of
sections redraws this way: the PR buffer and the dashboard."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (positions (cons (approve-pr--position nil)
                          (mapcar #'approve-pr--position windows)))
         (inhibit-read-only t))
    (approve-pr--detach-markers)
    (erase-buffer)
    (save-excursion
      (magit-insert-section (approve-root-section)
        (funcall insert))
      ;; Inserting only records which sections are collapsed; this hides
      ;; their bodies, as `magit-refresh-buffer' does.
      (magit-section-show magit-root-section))
    (approve-pr--restore (car positions))
    (dolist (position (cdr positions))
      (let ((window (plist-get position :window))
            (point (save-excursion (approve-pr--restore position))))
        (set-window-point window point)
        (when-let* ((rows (plist-get position :rows)))
          (set-window-start window
                            (save-excursion
                              (goto-char point)
                              (vertical-motion (- (max 0 (1- rows))) window)
                              (point))
                            t))))))

(defun approve-pr-redraw ()
  "Draw the current PR buffer again from its store.
The buffer is erased and `approve-pr-sections-hook' is run.  Point
stays on the same section, line and column, in each window showing
the buffer, and collapsed sections stay collapsed (decision D13)."
  (approve-pr--cancel-redraw)
  (approve-redraw-sections
   (lambda ()
     (if-let* ((fatal (approve-session-get :fatal)))
         (approve-pr--insert-fatal fatal)
       (magit-run-section-hook 'approve-pr-sections-hook))))
  (approve-pr-update-header-line)
  (approve-pr--fetch-images)
  (run-hooks 'approve-pr-redraw-hook))

(defun approve-pr-token-hint ()
  "Return what to check when GitHub gets no usable token."
  (format "Approve asks ghub for the token of `%s' (`approve-api-auth')."
          approve-api-auth))

(defun approve-pr--insert-fatal (err)
  "Insert ERR as the error of the whole buffer: what failed, why, what to do.
See `approve-pr--note-fatal'."
  (insert (approve-face (approve-pr--name) 'approve-title) "\n\n")
  (approve-render-insert-fatal
   (pcase (car-safe err)
     ('approve-api-not-found "Pull request not found, or the token has no access to it.")
     ('approve-api-unauthorized "Could not use the GitHub token.")
     (_ "Could not load the pull request."))
   (approve-api-error-message err)
   (pcase (car-safe err)
     ('approve-api-not-found
      (format "Check the URL, and the token ghub finds for `%s'." approve-api-auth))
     ('approve-api-unauthorized (approve-pr-token-hint)))))

;;; Header line

(defvar approve-pr-header-line-functions nil
  "Functions that return parts of the header line, or nil.
Each is called with no arguments in the PR buffer after a redraw;
the parts follow the number and title, joined with \" · \".  Use it
for notices, such as changes on GitHub; the state is on the right.")

(defun approve-pr--loading-names ()
  "Return the names of the queries still loading, for the header line."
  (let ((names '((core . "pull request") (files . "files") (threads . "threads")
                 (timeline . "conversation") (commits . "commits")
                 (patches . "diff") (checks . "checks"))))
    (delq nil (mapcar (lambda (entry)
                        (and (approve-pr-loading-p (car entry)) (cdr entry)))
                      names))))

(defun approve-pr-update-header-line ()
  "Compute the header line of the current PR buffer.
It is the number and title, what is still loading and the parts of
`approve-pr-header-line-functions', with the state on the right.  It
stays in view while the buffer scrolls.  A redraw computes it; call
this for a change that shows only there."
  ;; Redisplay draws the header line anew only when told to.
  (force-mode-line-update)
  (setq approve-pr--header-line
        (let* ((loaded (approve-model-root-ref))
               (badge (if loaded
                          (approve-render-state-badge (approve-pr-field 'state)
                                                      (approve-pr-field 'isDraft))
                        ""))
               (loading (and (not (approve-session-get :fatal))
                             (approve-pr--loading-names)))
               (notes (append
                       (and loading
                            (list (approve-face
                                   (format "loading %s…" (string-join loading ", "))
                                   'approve-loading)))
                       (and loaded (mapcar #'funcall approve-pr-header-line-functions))))
               (notes (approve-render-join notes))
               (number (format "#%s " (approve-model-meta :number)))
               (room (- approve-page-width (string-width number) (string-width badge)
                        (if (string-empty-p notes) 0 (+ 3 (string-width notes)))
                        3))
               (title (approve-render-truncate (or (and loaded (approve-pr-field 'title)) "")
                                               (max 10 room)))
               (face (list (list :height approve-title-height) 'approve-title)))
          (concat " "
                  (approve-render-justify
                   (approve-render-join
                    (list (propertize (string-trim-right (concat number title))
                                      'face face)
                          notes))
                   badge)))))

(provide 'approve-pr)

;;; approve-pr.el ends here
