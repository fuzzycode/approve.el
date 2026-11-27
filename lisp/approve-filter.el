;;; approve-filter.el --- Filters and order of the diff -*- lexical-binding: t; -*-

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

;; Filters narrow the diff of a PR buffer to the files that pass them,
;; so a big pull request can be read a slice at a time (decision D54;
;; docs/ui.org, "Filter").  Each filter is an entry of
;; `approve-filters', (KEY DESCRIPTION FUNCTION), which the filter menu
;; (=, `approve-filter-menu' in approve-menus.el) lists:
;;
;;   r  Path matching regexp…    `approve-filter-path-regexp'
;;   o  I am a code owner        `approve-filter-viewer-owns'
;;   t  My team is a code owner  `approve-filter-team-owns'
;;
;; Turning a filter on calls its FUNCTION in the PR buffer, which may
;; ask for something (the regexp) and wait for data (the CODEOWNERS
;; file and the viewer's teams, repository metadata), and returns the
;; predicate on a changed file.  The filters that are on are the
;; session's :filters; with several on, a file passes them all, or
;; one of them (`approve-filter-combine', & in the menu).  They hold
;; across g, until turned off or the buffer is killed.
;;
;; The diff and the file list draw only the files that pass, and say
;; how many of all they are (approve-sections.el reads the session);
;; ]u, ]t, j / and j j go through those files alone (approve-nav.el).
;;
;;   (approve-filter-register "c" "C++ files" #'my-cpp-filter)
;;
;; The same menu puts the diff's files in an order of
;; `approve-diff-orders' (decision D58): s goes to the next one, and
;; `approve-diff-order-set' picks one by name.  The order is the
;; session's :diff-order, held across g; a buffer starts in
;; `approve-diff-order'.  Built in: GitHub's, by path, and a header
;; before the sources of its name.
;;
;;   (approve-diff-order-register 'largest "Largest first" #'my-size-key)
;;
;; Layer rules: this is the buffers layer.  It turns filters on and
;; off and sets the order in the session, and redraws; the filters'
;; functions may ask the user and wait for repository metadata
;; (approve-metadata.el).  Only the renderers apply the predicates
;; and the orders' keys.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'approve-model)
(require 'approve-session)
(require 'approve-metadata)
(require 'approve-codeowners)
(require 'approve-pr)
(require 'approve-sections)
(require 'approve-input)

(defcustom approve-filters
  '(("r" "Path matching regexp…" approve-filter-path-regexp)
    ("o" "I am a code owner" approve-filter-viewer-owns)
    ("t" "My team is a code owner" approve-filter-team-owns))
  "Filters of the diff, in the filter menu: (KEY DESCRIPTION FUNCTION).
KEY turns the filter on or off in the menu (\\<approve-pr-mode-map>\\[approve-filter-menu]); \"&\" and \"DEL\" are
the menu's own.  DESCRIPTION says what it keeps.  FUNCTION is called
with no argument in the PR buffer when the filter is turned on.  It
may ask for something and wait for data, and returns a predicate:
a function of a changed file, an alist with `path', `changeType',
`additions', `deletions' and `viewerViewedState', that returns
non-nil for a file the diff shows.  The predicate is called in the
PR buffer each time it draws, so it must be quick and must not call
the network.  FUNCTION may return a plist instead, (:predicate
PREDICATE :label LABEL), where LABEL says in a few words what the
filter keeps, such as \"path ~ _test\": the diff's heading and the
menu show it.  A FUNCTION that signals, a `user-error' or
\\[keyboard-quit] while it waits, leaves the filter off.

To add a filter of the files you haven't viewed:

  (defun my-approve-unviewed ()
    (lambda (file)
      (not (equal (alist-get \\='viewerViewedState file) \"VIEWED\"))))
  (approve-filter-register \"u\" \"Not viewed\" #\\='my-approve-unviewed)"
  :type '(repeat (list (string :tag "Key")
                       (string :tag "Description")
                       (function :tag "Function")))
  :group 'approve)

(defcustom approve-filter-combine 'and
  "How the diff's filters combine when several are on, at first.
`and' shows the files that pass every filter, `or' those that pass
one of them.  \\<approve-pr-mode-map>\\[approve-filter-menu] & switches between them in a buffer.  To
show the files that any filter keeps:

  (setq approve-filter-combine \\='or)"
  :type '(choice (const :tag "Files that pass every filter" and)
                 (const :tag "Files that pass one of the filters" or))
  :group 'approve)

(defconst approve-filter-menu-keys '("&" "DEL" "s")
  "The keys the filter menu keeps for itself: combine, clear, sort.")

(defun approve-filter-register (key description function)
  "Add a filter of the diff on KEY, saying DESCRIPTION, made by FUNCTION.
It replaces the filter that KEY had in `approve-filters', or goes
last in the menu.  See `approve-filters' for FUNCTION.

Example:
  (approve-filter-register
   \"g\" \"Go files\"
   (lambda ()
     (lambda (file) (string-suffix-p \".go\" (alist-get \\='path file)))))"
  (when (member key approve-filter-menu-keys)
    (error "The filter menu keeps %s for itself" key))
  (let ((entry (list key description function)))
    (setq approve-filters
          (if (assoc key approve-filters)
              (mapcar (lambda (old) (if (equal (car old) key) entry old)) approve-filters)
            (append approve-filters (list entry))))
    entry))

;;; The filters that are on

(defun approve-filter-get (key)
  "Return the filter on KEY when it is on, a plist of the session's :filters."
  (seq-find (lambda (filter) (equal (plist-get filter :key) key))
            (approve-session-get :filters)))

(defun approve-filter-combination ()
  "Return how the filters on combine in this buffer: `and' or `or'."
  (or (approve-session-get :filter-combine) approve-filter-combine))

(defun approve-filter-summary ()
  "Return how many files the diff shows, such as \"5 of 12 files\"."
  (let* ((all (approve-pr-files))
         (total (or (and (approve-model-root-ref) (approve-pr-field 'changedFiles))
                    (length all))))
    (if (approve-sections-filtered-p)
        (format "%d of %d files" (length (approve-sections-filter-files all)) total)
      (format "%d file%s" total (if (= total 1) "" "s")))))

;;; Turning them on and off

(defun approve-filter--check ()
  "Signal a `user-error' unless this is a PR buffer that has loaded."
  (unless (derived-mode-p 'approve-pr-mode)
    (user-error "Not in an Approve pull request buffer"))
  (unless (approve-model-root-ref)
    (user-error "Wait until the pull request has loaded")))

(defun approve-filter--make (entry value)
  "Return the filter of ENTRY, from `approve-filters', that is on.
VALUE is what its function returned: a predicate, or a plist with
:predicate and :label."
  (pcase-let* ((`(,key ,description ,_) entry)
               (predicate (if (functionp value) value (plist-get value :predicate))))
    (unless (functionp predicate)
      (error "The filter on %s returned no predicate: %S" key value))
    (list :key key :description description
          :label (and (not (functionp value)) (plist-get value :label))
          :predicate predicate)))

(defun approve-filter--changed ()
  "Redraw the PR buffer after the filters changed, and say what it shows."
  (approve-pr-redraw)
  (message "Diff: %s" (approve-filter-summary)))

(defun approve-filter--read-key ()
  "Read a filter of `approve-filters' with completion; return its key."
  (let* ((candidates (mapcar (lambda (entry)
                               (cons (format "%s  %s%s" (car entry) (nth 1 entry)
                                             (if (approve-filter-get (car entry)) "  (on)" ""))
                                     (car entry)))
                             approve-filters))
         (choice (approve-input-with-submit-key
                   (completing-read "Turn on or off the filter: " candidates nil t))))
    (cdr (assoc choice candidates))))

(defun approve-filter-toggle (key)
  "Turn the diff's filter on KEY on, or off when it is on (decision D54).
KEY is a key of `approve-filters'; interactively, a filter is read
with completion.  Turning it on calls its function, which may ask
for something or wait for data; the diff then shows only the files
that pass the filters on.  The filter stays on across \\<approve-pr-mode-map>\\[approve-pr-refresh]."
  (interactive (list (progn (approve-filter--check) (approve-filter--read-key))))
  (approve-filter--check)
  (if (approve-filter-get key)
      (approve-session-put :filters (seq-remove (lambda (filter)
                                                  (equal (plist-get filter :key) key))
                                                (approve-session-get :filters)))
    (let* ((entry (or (assoc key approve-filters)
                      (user-error "No filter of the diff on %s" key)))
           (filter (approve-filter--make entry (funcall (nth 2 entry)))))
      (unless (approve-session-get :filter-combine)
        (approve-session-put :filter-combine approve-filter-combine))
      (approve-session-put :filters (append (approve-session-get :filters) (list filter)))))
  (approve-filter--changed))

(defun approve-filter-clear ()
  "Turn every filter of the diff off: it shows every file again."
  (interactive)
  (approve-filter--check)
  (approve-session-put :filters nil)
  (approve-filter--changed))

(defun approve-filter-toggle-combine ()
  "Switch how the diff's filters combine: every one, or any one of them.
With `and', the diff shows the files that pass every filter on;
with `or', those that pass one of them.  `approve-filter-combine'
says which a buffer starts with."
  (interactive)
  (approve-filter--check)
  (approve-session-put :filter-combine
                       (if (eq (approve-filter-combination) 'or) 'and 'or))
  (approve-filter--changed))

;;; The order of the files

(defun approve-diff-order-register (name description key)
  "Add an order of the diff's files, NAME, saying DESCRIPTION, sorted by KEY.
It replaces the order NAME had in `approve-diff-orders', or goes
last.  See `approve-diff-orders' for KEY.

Example, the files with the most changes first:
  (approve-diff-order-register
   \='largest \"Largest first\"
   (lambda (file)
     (- (+ (alist-get \='additions file) (alist-get \='deletions file)))))"
  (let ((entry (list name description key)))
    (setq approve-diff-orders
          (if (assq name approve-diff-orders)
              (mapcar (lambda (old) (if (eq (car old) name) entry old)) approve-diff-orders)
            (append approve-diff-orders (list entry))))
    entry))

(defun approve-diff-order-current ()
  "Return the name of the order the diff's files are in, in this buffer."
  (or (approve-session-get :diff-order) approve-diff-order))

(defun approve-diff-order--read ()
  "Read an order of `approve-diff-orders' with completion; return its name."
  (let* ((current (approve-diff-order-current))
         (candidates (mapcar (lambda (entry)
                               (cons (format "%s  %s%s" (car entry) (nth 1 entry)
                                             (if (eq (car entry) current) "  (now)" ""))
                                     (car entry)))
                             approve-diff-orders))
         (choice (approve-input-with-submit-key
                   (completing-read "Put the diff's files in the order: " candidates nil t))))
    (cdr (assoc choice candidates))))

(defun approve-diff-order-set (name)
  "Put the diff's files in the order NAME of `approve-diff-orders' (D58).
Interactively, the order is read with completion.  The diff, the
file list, the thread picker and the feedback mode then go through
the files in it.  It holds across \\<approve-pr-mode-map>\\[approve-pr-refresh]; a buffer starts in
`approve-diff-order'."
  (interactive (list (progn (approve-filter--check) (approve-diff-order--read))))
  (approve-filter--check)
  (let ((entry (or (assq name approve-diff-orders)
                   (user-error "No order of the diff is named %s" name))))
    (approve-session-put :diff-order name)
    (approve-pr-redraw)
    (message "Diff: %s" (nth 1 entry))))

(defun approve-diff-order-cycle ()
  "Put the diff's files in the next order of `approve-diff-orders'.
After the last, the first again."
  (interactive)
  (approve-filter--check)
  (let ((names (mapcar #'car approve-diff-orders)))
    (unless names
      (user-error "There is no order of the diff (`approve-diff-orders')"))
    (approve-diff-order-set (or (cadr (memq (approve-diff-order-current) names))
                                (car names)))))

;;; The filters Approve has

(defun approve-filter-path-regexp ()
  "Ask for a regexp; keep the files whose path it matches.
Case matters when the regexp has an upper-case letter, as in
`occur' (`case-fold-search')."
  (let ((regexp (read-regexp "Keep the files whose path matches regexp: ")))
    (when (string-empty-p regexp)
      (user-error "No regexp: the filter stays off"))
    (condition-case err
        (string-match-p regexp "")
      (invalid-regexp (user-error "Invalid regexp %s: %s" regexp (cadr err))))
    (let ((fold (and case-fold-search (isearch-no-upper-case-p regexp t))))
      (list :predicate (lambda (file)
                         (let ((case-fold-search fold))
                           (string-match-p regexp (alist-get 'path file))))
            :label (concat "path ~ " regexp)))))

(defun approve-filter--codeowners ()
  "Return the CODEOWNERS of the pull request's base branch, waiting for it.
Signal a `user-error' when the branch has none."
  (or (approve-metadata-wait 'codeowners)
      (user-error "%s/%s has no CODEOWNERS on %s (in .github/, the root or docs/)"
                  (approve-model-meta :owner) (approve-model-meta :repo)
                  (approve-pr-field 'baseRefName))))

(defun approve-filter--owned-by (codeowners owners)
  "Return a predicate of a file: one of OWNERS owns it by CODEOWNERS.
CODEOWNERS is from `approve-filter--codeowners'; OWNERS are written
as CODEOWNERS writes them, such as \"@alice\"."
  (let ((owned-p (approve-codeowners-predicate (plist-get codeowners :rules) owners)))
    (lambda (file)
      (funcall owned-p (alist-get 'path file)))))

(defun approve-filter-viewer-owns ()
  "Keep the files you are a code owner of, by your login.
The owners are those of the CODEOWNERS file on the pull request's
base branch, as GitHub reads it; the files your teams own are
`approve-filter-team-owns''s."
  (let* ((codeowners (approve-filter--codeowners))
         (viewer (concat "@" (or (approve-model-meta :viewer)
                                 (user-error "Wait until the pull request has loaded")))))
    (list :predicate (approve-filter--owned-by codeowners (list viewer))
          :label (concat "owned by " viewer))))

(defun approve-filter--team-slugs (teams)
  "Return the slugs of TEAMS and their ancestors, such as \"acme/web\".
A member of a child team is one of its parent's too."
  (delete-dups
   (mapcan (lambda (team)
             (cons (alist-get 'combinedSlug team)
                   (mapcar (lambda (ancestor) (alist-get 'combinedSlug ancestor))
                           (alist-get 'nodes (alist-get 'ancestors team)))))
           teams)))

(defun approve-filter-team-owns ()
  "Keep the files one of your teams is a code owner of.
Your teams are those of the organization that owns the repository
that you are a member of, and their parents.  The owners are those
of the CODEOWNERS file on the pull request's base branch."
  ;; The teams load while the CODEOWNERS file is waited for.
  (approve-metadata-fetch 'viewer-teams)
  (let* ((codeowners (approve-filter--codeowners))
         (teams (or (approve-metadata-wait 'viewer-teams)
                    (user-error "You are in no team of %s" (approve-model-meta :owner))))
         (slugs (approve-filter--team-slugs teams)))
    (list :predicate (approve-filter--owned-by
                      codeowners (mapcar (lambda (slug) (concat "@" slug)) slugs))
          :label (concat "owned by @" (alist-get 'combinedSlug (car teams))
                         (if (cdr teams) (format " +%d" (length (cdr teams))) "")))))

(provide 'approve-filter)

;;; approve-filter.el ends here
