;;; approve-file.el --- The whole file of a pull request -*- lexical-binding: t; -*-

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

;; The file view: one changed file whole, at the pull request's head
;; or its base, in the file's own major mode (docs/ui.org, "File
;; view"; docs/flows.org, F15; decision D43):
;;
;;   *approve-file: acme/rocket internal/parser/parser.go @ 6ffac57*
;;
;; - `approve-file-show' shows a version of a changed file; RET on a
;;   diff line and F on a file (`approve-visit-file') call it.  The
;;   text is the file at the commit, fetched through the API
;;   (`approve-query-blob') and kept for the session by repository,
;;   commit and path (`approve-file-fetch'); C-u g forgets it.
;; - The buffer is in the major mode `set-auto-mode' chooses for the
;;   file, with the mode's hooks never run: no checker or language
;;   server starts for a file that is not on disk.  Font-lock, imenu
;;   and which-function work.  `approve-file-mode', a minor mode, adds
;;   the keys and the header line; `approve-file-mode-hook' is where a
;;   user turns on more.
;; - The buffer shares the store and session of its PR buffer
;;   (`approve-model-store-buffer'), so the commands that read them
;;   work here.  The lines in the diff carry `approve-line' as in the
;;   diff: c comments on them, RET goes to them in the diff.  The
;;   others are the context `code' (`approve-context-file'), on which
;;   c says why GitHub takes no comment.
;; - Its decorations are overlays, as the buffer holds the file's own
;;   text: a mark in the fringe (the margin in a terminal) on the
;;   lines the pull request changed and where it removed lines, and
;;   the threads under their lines as in the diff, with the speech
;;   bubble at the right edge; TAB folds them.  Threads are read here
;;   and acted on in the diff (D43), so their boxes name no keys.
;; - Each redraw of the PR buffer redraws its file views
;;   (`approve-pr-redraw-hook'); a view whose commit is no longer the
;;   pull request's head (or base) fetches the new version.  Killing
;;   the PR buffer kills its file views.
;;
;; Layer rules: this is the buffers layer.  It runs the blob query and
;; the renderers, and reads the store through selectors; it never
;; changes the store.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'approve-api)
(require 'approve-log)
(require 'approve-model)
(require 'approve-session)
(require 'approve-context)
(require 'approve-queries)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-thread)
(require 'approve-pr)
(require 'approve-diff)

(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function which-func-ff-hook "which-func" ())
(defvar which-function-mode)

;;; Options

(defcustom approve-file-line-numbers t
  "Whether the file view shows line numbers.
They are `display-line-numbers-mode''s, the numbers the diff shows on
the same side.  To hide them:

  (setq approve-file-line-numbers nil)"
  :type 'boolean
  :group 'approve)

(defcustom approve-file-display-action '(display-buffer-same-window)
  "How the file view is shown when it opens: a `display-buffer' action.
By default the file takes the window of the pull request, and
\\<approve-file-mode-map>\\[quit-window] goes back to it.  To show it in another window:

  (setq approve-file-display-action
        \\='((display-buffer-reuse-window display-buffer-pop-up-window)))"
  :type 'sexp
  :group 'approve)

;;; Fringe marks

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'approve-file-change [#b00111100] nil nil '(center t)))

(defun approve-file--mark (kind face)
  "Return the mark of KIND for the left fringe, in FACE.
KIND is `changed', a bar on a line this version has and the other
has not, or `removed', a triangle where the other version has lines
this one has not.  A terminal, which has no fringe, shows it in the
margin."
  (propertize " " 'display
              (if (display-graphic-p)
                  (list 'left-fringe
                        (if (eq kind 'changed) 'approve-file-change 'right-triangle)
                        face)
                (list '(margin left-margin)
                      (approve-face (if (eq kind 'changed) "▌" "▸") face)))))

;;; The texts of files

(defvar approve-file--blobs (make-hash-table :test #'equal)
  "The texts of files fetched in this session: (HOST OWNER REPO OID PATH) → text.")

(defun approve-file-clear-cache ()
  "Forget the texts of files fetched in this session."
  (clrhash approve-file--blobs))

(add-hook 'approve-pr-clear-cache-functions #'approve-file-clear-cache)

(cl-defun approve-file-fetch (oid path &key callback errorback)
  "Call CALLBACK with the text of PATH at commit OID of the PR's repository.
The text comes from the session's cache at once, or else from GitHub
\(`approve-query-blob'), which then caches it; it is \"\" when the
file does not exist at OID.  ERRORBACK is called with the error,
also for a binary file or one too large for GitHub's API.

Example:
  (approve-file-fetch (approve-pr-field \\='headRefOid) \"go.mod\"
                      :callback (lambda (text) (message \"%d\" (length text))))"
  (let ((key (list (approve-model-meta :host) (approve-model-meta :owner)
                   (approve-model-meta :repo) oid path)))
    (if-let* ((text (gethash key approve-file--blobs)))
        (funcall callback text)
      (approve-query-blob oid path
                          :callback (lambda (text)
                                      (puthash key text approve-file--blobs)
                                      (funcall callback text))
                          :errorback errorback))))

;;; State of a file view

(defvar-local approve-file--folds nil
  "The lines whose threads were folded or unfolded here: line → state.
STATE is `folded' or `shown'; the other lines follow
`approve-diff-threads-folded-p'.")

(defvar-local approve-file--changes nil
  "The first line of each change in this version, in order.")

(defvar-local approve-file--threads nil
  "A hash table from the lines of this version to their threads.")

(defvar-local approve-file--elsewhere 0
  "How many threads of the file show only in the diff.")

(defvar-local approve-file--loading nil
  "The commit whose version is being fetched for this view, or nil.")

(defvar-local approve-file--error nil
  "The error of the last fetch of a new version for this view, or nil.")

(defvar-local approve-file--header-line nil
  "The header line, computed at each redraw.")
;; Without this, the header line would drop the text's faces and the
;; space that aligns the hints on the right.
(put 'approve-file--header-line 'risky-local-variable t)

;;; Mode

(defvar-keymap approve-file-mode-map
  :doc "Keymap of `approve-file-mode'.
RET, o and y are bound by approve-nav.el, c, s and v by
approve-commands.el, and ? by approve-menus.el."
  "n" #'approve-file-next-change
  "p" #'approve-file-previous-change
  "] t" #'approve-file-next-thread
  "[ t" #'approve-file-previous-thread
  "TAB" #'approve-file-toggle-threads
  "<tab>" #'approve-file-toggle-threads
  "b" #'approve-file-toggle-version
  "g" #'approve-file-refresh
  "q" #'quit-window)

(define-minor-mode approve-file-mode
  "Minor mode of the file view: a pull request's changed file, whole.
The buffer is in the file's major mode and read-only.  The fringe
marks what the pull request changed; threads sit under their lines.
\\<approve-file-mode-map>\\[approve-file-next-change] and \\[approve-file-previous-change] move between changes, \\[approve-file-next-thread] to the next unresolved
thread, \\[approve-file-toggle-threads] folds the threads of a line, \\[approve-file-toggle-version] shows the other version,
\\[quit-window] goes back.

\\{approve-file-mode-map}"
  :lighter " Approve"
  :keymap approve-file-mode-map
  :group 'approve
  (when approve-file-mode
    (setq-local header-line-format 'approve-file--header-line)
    ;; Evil's bindings load with the first buffer that may need them
    ;; (decision D17); a minor mode's need its keymaps normalized.
    (when (featurep 'evil)
      (require 'approve-evil)
      (when (and (bound-and-true-p evil-local-mode) (fboundp 'evil-normalize-keymaps))
        (evil-normalize-keymaps)))))

;;; Versions of a changed file

(defun approve-file--side-oid (side)
  "Return the PR's commit on SIDE: its base for \"LEFT\", else its head."
  (approve-pr-field (if (equal side "LEFT") 'baseRefOid 'headRefOid)))

(defun approve-file--pr-file (path)
  "Return the changed file PATH from the store, or nil."
  (seq-find (lambda (file) (equal (alist-get 'path file) path)) (approve-pr-files)))

(defun approve-file--name (path side)
  "Return the path of the changed file PATH in its version on SIDE.
That is the old path for the base of a renamed or copied file."
  (or (and (equal side "LEFT")
           (alist-get 'previous_filename (approve-diff-file-entry path)))
      path))

(defun approve-file--why-not (path side)
  "Return why the changed file PATH has no version on SIDE to show, or nil.
A binary file is found out by GitHub's answer: one whose mode alone
changed looks the same in the store."
  (let ((file (approve-file--pr-file path)))
    (cond ((null file) (format "%s is not a file of this pull request" path))
          ((and (equal side "LEFT") (equal (alist-get 'changeType file) "ADDED"))
           (format "%s is new in this pull request: it has no base version" path))
          ((and (equal side "RIGHT") (equal (alist-get 'changeType file) "DELETED"))
           (format "%s is deleted by this pull request: it has no head version" path)))))

(defun approve-file--hunks (path)
  "Return the parsed patch of the changed file PATH, from the store."
  (approve-diff-parse (alist-get 'patch (approve-diff-file-entry path))))

(defun approve-file--short (oid)
  "Return OID shortened as GitHub writes it."
  (substring oid 0 (min 7 (length oid))))

(defun approve-file-version-label ()
  "Return which version the current file view shows, such as \"head 6ffac57\".
It is \"\" outside a file view."
  (if-let* ((oid (plist-get approve-context-file :oid)))
      (format "%s %s" (if (equal (plist-get approve-context-file :side) "LEFT") "base" "head")
              (approve-file--short oid))
    ""))

;;; What a version shows

(defun approve-file--blocks (hunks left)
  "Return the changes of HUNKS as the version on one side shows them.
The version is the base when LEFT, else the head.  Each change is
\(LINE . COUNT): COUNT lines from LINE that this version has and the
other has not (added lines in the head, removed ones in the base),
or COUNT 0 where the other version has lines this one has not,
before LINE.  They are in order."
  (let ((blocks nil))
    (dolist (hunk hunks)
      (let ((old (approve-diff-hunk-old-start hunk))
            (new (approve-diff-hunk-new-start hunk))
            (start nil) (count 0))
        (cl-flet ((finish ()
                    (when start
                      (push (cons start count) blocks))
                    (setq start nil count 0)))
          (dolist (line (approve-diff-hunk-lines hunk))
            (pcase (car line)
              ('ctx (finish) (cl-incf old) (cl-incf new))
              ((and kind (or 'add 'del))
               (unless start
                 (setq start (if left old new)))
               (when (eq kind (if left 'del 'add))
                 (cl-incf count))
               (if (eq kind 'add) (cl-incf new) (cl-incf old)))))
          (finish))))
    (nreverse blocks)))

(defun approve-file--diff-lines (path hunks left)
  "Return the lines of the version that are in the diff of PATH.
HUNKS are its parsed patch; the version is the base when LEFT, else
the head.  Each is (LINE . PROPERTY): the line's number and its
`approve-line', the same as the diff's, so a comment on it is the
same as one in the diff.  They are in order."
  (let ((lines nil))
    (dolist (hunk hunks)
      (dolist (line (approve-diff-hunk-lines hunk))
        (pcase-let ((`(,kind ,old ,new ,_) line))
          (when (memq kind (if left '(del ctx) '(add ctx)))
            (push (cons (if left old new)
                        (list :path path :side (if (eq kind 'del) "LEFT" "RIGHT")
                              :old old :new new :kind kind))
                  lines)))))
    (sort lines (lambda (a b) (< (car a) (car b))))))

(defun approve-file--thread-index (threads hunks left)
  "Sort THREADS of a file by the lines of one version they go under.
HUNKS are the file's parsed patch; the version is the base when
LEFT, else the head.  A thread goes under the line its side and line
name, or under a context line by its number on the other side.
Return a plist: :lines, a hash table from line to its threads, in
order; :elsewhere, how many threads only the diff shows (on the
whole file, outdated, or on lines this version does not have)."
  (let ((where (make-hash-table :test #'equal))
        (lines (make-hash-table))
        (elsewhere 0))
    (dolist (hunk hunks)
      (dolist (line (approve-diff-hunk-lines hunk))
        (pcase-let ((`(,kind ,old ,new ,_) line))
          (pcase kind
            ('add (unless left (puthash (cons "RIGHT" new) new where)))
            ('del (when left (puthash (cons "LEFT" old) old where)))
            ('ctx (let ((here (if left old new)))
                    (puthash (cons "RIGHT" new) here where)
                    (puthash (cons "LEFT" old) here where)))))))
    (dolist (thread threads)
      (let-alist thread
        (if-let* ((line (and (not (equal .subjectType "FILE"))
                             (gethash (cons .diffSide .line) where))))
            (puthash line (append (gethash line lines) (list thread)) lines)
          (cl-incf elsewhere))))
    (list :lines lines :elsewhere elsewhere)))

(defun approve-file--map-line (hunks line left)
  "Return the line of the other version where LINE of one version is.
HUNKS are the file's parsed patch; LINE is in the base when LEFT,
else in the head.  An unchanged line is the same line there; a
changed one goes to the line it replaced (the last one, past the
end of those), or to where it was added or removed."
  (let ((delta 0))
    (max 1 (or (catch 'found
                 (dolist (hunk hunks)
                   (let ((this (if left (approve-diff-hunk-old-start hunk)
                                 (approve-diff-hunk-new-start hunk)))
                         (other (if left (approve-diff-hunk-new-start hunk)
                                  (approve-diff-hunk-old-start hunk)))
                         (mine nil) (theirs nil) (at nil))
                     (when (< line this)
                       (throw 'found (+ line delta)))
                     (cl-flet ((finish ()
                                 (when-let* ((index (seq-position (reverse mine) line)))
                                   (throw 'found (if theirs
                                                     (nth (min index (1- (length theirs)))
                                                          (reverse theirs))
                                                   at)))
                                 (setq mine nil theirs nil at nil)))
                       (dolist (entry (approve-diff-hunk-lines hunk))
                         (pcase (car entry)
                           ('ctx (finish)
                                 (when (= this line)
                                   (throw 'found other))
                                 (cl-incf this)
                                 (cl-incf other))
                           ((and kind (or 'add 'del))
                            (unless (or mine theirs)
                              (setq at other))
                            (if (eq kind (if left 'del 'add))
                                (progn (push this mine) (cl-incf this))
                              (push other theirs)
                              (cl-incf other)))))
                       (finish))
                     (setq delta (- other this))))
                 nil)
               (+ line delta)))))

;;; Drawing

(defun approve-file--at-lines (entries function)
  "Call FUNCTION with each of ENTRIES, point at the start of its line.
ENTRIES are (LINE . DATA), in order of LINE; FUNCTION gets DATA.
Lines past the end of the buffer are left out."
  (save-excursion
    (goto-char (point-min))
    (let ((at 1))
      (catch 'end
        (dolist (entry entries)
          (when (> (car entry) at)
            (unless (zerop (forward-line (- (car entry) at)))
              (throw 'end nil))
            (setq at (car entry)))
          (when (= (car entry) at)
            (funcall function (cdr entry))))))))

(defun approve-file--overlay (start end)
  "Return a new overlay of the file view from START to END."
  (let ((overlay (make-overlay start end nil t)))
    (overlay-put overlay 'approve-file t)
    overlay))

(defun approve-file--put-marks (blocks left)
  "Mark the changes of BLOCKS in the fringe; the version is the base when LEFT.
See `approve-file--blocks'."
  (let* ((own (if left 'approve-file-change-removed 'approve-file-change-added))
         (other (if left 'approve-file-change-added 'approve-file-change-removed))
         (changed (cons 'changed (approve-file--mark 'changed own)))
         (removed (cons 'removed (approve-file--mark 'removed other)))
         (lines (count-lines (point-min) (point-max)))
         (entries nil))
    (dolist (block blocks)
      (if (zerop (cdr block))
          (push (cons (min (car block) (max 1 lines)) removed) entries)
        (dotimes (i (cdr block))
          (push (cons (+ (car block) i) changed) entries))))
    (approve-file--at-lines
     (sort entries (lambda (a b) (< (car a) (car b))))
     (lambda (mark)
       (let ((overlay (approve-file--overlay (point) (point))))
         (overlay-put overlay 'approve-file-mark (car mark))
         (overlay-put overlay 'before-string (cdr mark)))))))

(defun approve-file--threads-text (threads)
  "Return THREADS drawn as the diff draws them, indented, as a string.
They are drawn outside the file view, so their boxes name no keys:
their commands work in the diff (decision D43)."
  (with-temp-buffer
    (dolist (thread threads)
      (let ((start (point)))
        (approve-thread-insert thread)
        (approve-render-prefix start (point) "  ")))
    (buffer-string)))

(defun approve-file--folded-p (line threads)
  "Return non-nil if THREADS, under LINE, are folded in this view."
  (pcase (and approve-file--folds (gethash line approve-file--folds))
    ('folded t)
    ('shown nil)
    (_ (approve-diff-threads-folded-p threads))))

(defun approve-file--put-threads (lines)
  "Show the threads of LINES, a hash table from line to threads, under them.
Each line gets the speech bubble of the diff at its end; its threads
follow it unless they are folded.  Both are the before-string of an
overlay on the line's newline, as the text is the file's own: the
threads are rows of the line, so the line numbers of the lines
after them stay theirs."
  (let ((asking (mapcar (lambda (thread) (alist-get 'id thread))
                        (approve-pr-threads-asking-for-changes)))
        (entries nil))
    (maphash (lambda (line threads) (push (cons line (cons line threads)) entries)) lines)
    (approve-file--at-lines
     (sort entries (lambda (a b) (< (car a) (car b))))
     (lambda (entry)
       (let* ((threads (cdr entry))
              (end (line-end-position))
              (overlay (approve-file--overlay end (min (1+ end) (point-max)))))
         (overlay-put overlay 'approve-file-threads threads)
         (overlay-put overlay 'before-string
                      (concat (approve-diff-thread-bubble threads nil asking)
                              (unless (approve-file--folded-p (car entry) threads)
                                (concat "\n" (string-remove-suffix
                                              "\n" (approve-file--threads-text threads)))))))))))

(defun approve-file--decorate ()
  "Draw the marks, the diff's lines and the threads of the file view anew."
  (let* ((path (plist-get approve-context-file :path))
         (left (equal (plist-get approve-context-file :side) "LEFT"))
         (hunks (approve-file--hunks path))
         (blocks (approve-file--blocks hunks left))
         (index (approve-file--thread-index (approve-pr-threads-for-file path) hunks left)))
    (with-silent-modifications
      (remove-overlays (point-min) (point-max) 'approve-file t)
      (remove-list-of-text-properties (point-min) (point-max) '(approve-line))
      (approve-file--at-lines (approve-file--diff-lines path hunks left)
                              (lambda (property)
                                (put-text-property (point) (min (1+ (line-end-position))
                                                                (point-max))
                                                   'approve-line property)))
      (approve-file--put-marks blocks left)
      (approve-file--put-threads (plist-get index :lines)))
    (setq approve-file--changes (delete-dups (mapcar #'car blocks))
          approve-file--threads (plist-get index :lines)
          approve-file--elsewhere (plist-get index :elsewhere))))

(defun approve-file--count (count noun)
  "Return \"COUNT NOUNs\", or nil when COUNT is zero."
  (unless (zerop count)
    (format "%d %s%s" count noun (if (= count 1) "" "s"))))

(defun approve-file--hint (commands label)
  "Return the hint of COMMANDS with LABEL, such as \"n/p change\", or nil.
The keys are those that run COMMANDS here, joined by a slash; nil
when none does or `approve-show-hints' is nil."
  (when-let* ((approve-show-hints)
              (keys (delq nil (mapcar #'approve-render-key commands))))
    (concat (approve-face (string-join keys "/") 'approve-hint-key) " "
            (approve-face label 'approve-hint))))

(defun approve-file--viewed-p (path)
  "Return non-nil if the viewer marked the changed file PATH viewed."
  (equal (alist-get 'viewerViewedState (approve-file--pr-file path)) "VIEWED"))

(defun approve-file--update-header-line ()
  "Compute the header line of the file view.
It says which version this is, how many changes and threads it
shows, how many threads only the diff shows, whether the file is
viewed, and what is loading; then its keys, on the right."
  (force-mode-line-update)
  (setq approve-file--header-line
        (let* ((path (plist-get approve-context-file :path))
               (head (equal (plist-get approve-context-file :side) "RIGHT"))
               (threads (and approve-file--threads
                             (apply #'append (hash-table-values approve-file--threads))))
               (notes
                (list (concat (approve-face (if head "head" "base") 'approve-heading) " "
                              (approve-face (approve-file--short
                                             (plist-get approve-context-file :oid))
                                            'approve-hash))
                      (approve-file--count (length approve-file--changes) "change")
                      (approve-file--count (length threads) "thread")
                      (and (> approve-file--elsewhere 0)
                           (approve-face (format "%d more in the diff" approve-file--elsewhere)
                                         'approve-dim))
                      (and (approve-file--viewed-p path)
                           (concat (approve-icon 'viewed 'approve-viewed) " "
                                   (approve-face "viewed" 'approve-viewed)))
                      (and approve-file--loading
                           (concat (approve-icon 'loading 'approve-loading) " "
                                   (approve-face "loading the new version…" 'approve-loading)))
                      (and approve-file--error
                           (approve-face (format "could not load the new version: %s"
                                                 (approve-api-error-message approve-file--error))
                                         'approve-error))))
               (hints (approve-render-join
                       (list (approve-file--hint '(approve-file-next-change
                                                   approve-file-previous-change)
                                                 "change")
                             (approve-file--hint '(approve-file-next-thread) "thread")
                             (approve-file--hint '(approve-file-toggle-version)
                                                 (if head "base" "head"))
                             (approve-file--hint '(approve-comment) "comment")
                             (approve-file--hint '(quit-window) "back"))
                       (approve-face " · " 'approve-hint))))
          (concat " " (approve-render-justify (approve-render-join notes) hints)))))

(defun approve-file-redraw ()
  "Draw the decorations of the current file view again, from the store.
A view whose commit is no longer the pull request's (on its side)
fetches the new version, and shows it once it is in."
  (when (and approve-context-file (approve-model-root-ref))
    (let ((oid (approve-file--side-oid (plist-get approve-context-file :side))))
      (if (and oid (not (equal oid (plist-get approve-context-file :oid))))
          (approve-file--follow oid)
        (approve-file--decorate)))
    (approve-file--update-header-line)))

;;; Showing a version

(defun approve-file--set-major-mode (name)
  "Put the current buffer in the major mode of the file NAME, without its hooks.
The mode is the one `set-auto-mode' chooses for NAME and the text.
Its hooks are delayed and never run (decision D43): no checker or
language server starts for a file that is not on disk.  Font-lock
and which-function are turned on as their global modes would."
  (condition-case err
      (let ((buffer-file-name (expand-file-name name "/"))
            (enable-local-variables nil)
            (inhibit-message t)
            (message-log-max nil))
        (delay-mode-hooks (set-auto-mode)))
    (error
     (approve-log 'debug "No major mode for %s: %s" name (error-message-string err))
     (fundamental-mode)))
  (when (bound-and-true-p global-font-lock-mode)
    (turn-on-font-lock-if-desired))
  (when (and (bound-and-true-p which-function-mode) (fboundp 'which-func-ff-hook))
    (which-func-ff-hook)))

(defun approve-file--buffer-name (name oid)
  "Return the name of the file view of the file NAME at commit OID."
  (format "*approve-file: %s/%s %s @ %s*" (approve-model-meta :owner)
          (approve-model-meta :repo) name (approve-file--short oid)))

(defun approve-file--goto-line (line)
  "Move point to the start of LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun approve-file--show-text (owner view text line)
  "Show TEXT in the current buffer, the version VIEW of a changed file.
OWNER is the PR buffer whose store the view shares; VIEW is the
plist of `approve-context-file'.  Point goes to LINE, or to the
first change when LINE is nil, in every window that shows it."
  (setq buffer-undo-list t)
  (remove-overlays (point-min) (point-max) 'approve-file t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text))
  (approve-file--set-major-mode (plist-get view :name))
  ;; The major mode killed the buffer's local variables.
  (setq approve-model-store-buffer owner
        approve-context-file view
        approve-file--folds (make-hash-table)
        buffer-undo-list t
        buffer-read-only t)
  (set-buffer-modified-p nil)
  (unless (display-graphic-p)
    (setq left-margin-width 1))
  (approve-file-mode 1)
  (display-line-numbers-mode (if approve-file-line-numbers 1 -1))
  (rename-buffer (approve-file--buffer-name (plist-get view :name) (plist-get view :oid)) t)
  (approve-file-redraw)
  (approve-file--goto-line (or line (car approve-file--changes) 1))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-point window (point)))
  ;; What other buffers of the PR mark here (lines in flight, the
  ;; lines of a comment being written) goes on the new text.
  (run-hooks 'approve-pr-redraw-hook))

(defun approve-file--follow (oid)
  "Show the version of the file view at OID, the pull request's new commit.
It is fetched once; meanwhile the view keeps the old one, and the
header line says it is loading."
  (unless (equal approve-file--loading oid)
    (setq approve-file--loading oid
          approve-file--error nil)
    (let* ((buffer (current-buffer))
           (owner approve-model-store-buffer)
           (path (plist-get approve-context-file :path))
           (side (plist-get approve-context-file :side))
           (view (list :path path :name (approve-file--name path side) :side side :oid oid)))
      (approve-file-fetch
       oid (plist-get view :name)
       :callback (lambda (text)
                   (when (and (buffer-live-p buffer) (buffer-live-p owner))
                     (with-current-buffer buffer
                       (when (equal approve-file--loading oid)
                         (approve-file--show-text owner view text (line-number-at-pos))))))
       :errorback (lambda (err)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (when (equal approve-file--loading oid)
                          (setq approve-file--error err)
                          (approve-file--update-header-line)))))))))

(defun approve-file--views (owner)
  "Return the live file views of the PR buffer OWNER."
  (seq-filter (lambda (buffer)
                (and (buffer-local-value 'approve-file-mode buffer)
                     (eq (buffer-local-value 'approve-model-store-buffer buffer) owner)))
              (buffer-list)))

(defun approve-file--view-of (owner path)
  "Return the file view of the changed file PATH of the PR buffer OWNER, or nil."
  (seq-find (lambda (buffer)
              (equal (plist-get (buffer-local-value 'approve-context-file buffer) :path)
                     path))
            (approve-file--views owner)))

(defun approve-file--display (owner view text line source)
  "Show TEXT, the version VIEW of a changed file, at LINE.
OWNER is the PR buffer; SOURCE the buffer it was asked from.  The
file's view is reused, or made.  It is shown by
`approve-file-display-action' when SOURCE is still the selected
window's buffer; else the echo area says it is ready."
  (let ((buffer (or (approve-file--view-of owner (plist-get view :path))
                    (generate-new-buffer "*approve-file*"))))
    (with-current-buffer buffer
      (approve-file--show-text owner view text line))
    (cond
     ((eq buffer (window-buffer (selected-window))))
     ((eq source (window-buffer (selected-window)))
      (let ((point (with-current-buffer buffer (point))))
        (pop-to-buffer buffer approve-file-display-action)
        (goto-char point)))
     (t (message "Approve: %s is ready in %s" (plist-get view :name) (buffer-name buffer))))
    buffer))

(defun approve-file-show (path &optional side line)
  "Show the changed file PATH whole: its version on SIDE, point on LINE (F15).
SIDE is \"RIGHT\" for the pull request's head (the default) and
\"LEFT\" for its base.  LINE nil means the first change.  Call it in
a buffer of the pull request: the PR buffer or a file view.  The
text is fetched unless the session has it, and shown once it is in;
the echo area says why when it can't be (a binary file, one too
large for GitHub's API).  Signal a `user-error' when the file has no
such version: an added file has no base, a deleted one no head.

Example:
  (approve-file-show \"internal/parser/parser.go\" \"RIGHT\" 15)"
  (let* ((side (or side "RIGHT"))
         (why (approve-file--why-not path side)))
    (when why
      (user-error "%s" why))
    (let* ((owner (approve-model-owner-buffer))
           (name (approve-file--name path side))
           (oid (or (approve-file--side-oid side)
                    (user-error "Wait until the pull request has loaded")))
           (view (list :path path :name name :side side :oid oid))
           (source (current-buffer)))
      (approve-file-fetch
       oid name
       :callback (lambda (text)
                   (when (buffer-live-p owner)
                     (approve-file--display owner view text line source)))
       :errorback (lambda (err)
                    (message "Approve: could not show %s: %s" name
                             (approve-api-error-message err)))))))

;;; Commands

(defun approve-file--check ()
  "Signal a `user-error' unless the current buffer is a file view."
  (unless (and approve-file-mode approve-context-file)
    (user-error "Not in an Approve file view")))

(defun approve-file--visit-preflight ()
  "Return (PATH SIDE LINE) of what `approve-visit-file' shows.
A diff line shows its version at that line, a thread its line, a
file its head (its base when it was deleted) at the first change.
Signal a `user-error' when point is on no file, or the file has no
such version."
  (unless (approve-model-root-ref)
    (user-error (if (derived-mode-p 'approve-pr-mode)
                    "Wait until the pull request has loaded"
                  "Not in an Approve pull request buffer")))
  (let* ((context (or (seq-find (lambda (context) (plist-get context :path))
                                (delq nil (list (approve-context-at-point)
                                                (approve-context-of-sections))))
                      (user-error "Put point on a file or a line of the diff")))
         (path (plist-get context :path))
         (deleted (equal (alist-get 'changeType (approve-file--pr-file path)) "DELETED"))
         (target (if (and (memq (plist-get context :kind) '(line lines thread code))
                          (plist-get context :line))
                     (list path (plist-get context :side) (plist-get context :line))
                   (list path (if deleted "LEFT" "RIGHT") nil))))
    (when-let* ((why (approve-file--why-not path (nth 1 target))))
      (user-error "%s" why))
    target))

(put 'approve-visit-file 'approve-preflight #'approve-file--visit-preflight)

(defun approve-visit-file ()
  "Show the whole file at point, in its own major mode (flow F15).
On a line of the diff, the file's version on that line's side: the
head for added and unchanged lines, the base for removed ones, point
on the line.  On a file, its head (its base when it was deleted) at
the first change.  \\<approve-file-mode-map>\\[approve-file-toggle-version] shows the other version there."
  (interactive)
  (apply #'approve-file-show (approve-file--visit-preflight)))

(defun approve-file-next-change ()
  "Move to the next change the pull request made in the file."
  (interactive)
  (approve-file--check)
  (let ((here (line-number-at-pos)))
    (approve-file--goto-line (or (seq-find (lambda (line) (> line here)) approve-file--changes)
                                 (user-error "No change after this")))))

(defun approve-file-previous-change ()
  "Move to the previous change the pull request made in the file."
  (interactive)
  (approve-file--check)
  (let ((here (line-number-at-pos)))
    (approve-file--goto-line (or (seq-find (lambda (line) (< line here))
                                           (reverse approve-file--changes))
                                 (user-error "No change before this")))))

(defun approve-file--unresolved-lines ()
  "Return the lines with an unresolved thread, in order."
  (let ((lines nil))
    (maphash (lambda (line threads)
               (when (seq-some (lambda (thread) (not (alist-get 'isResolved thread))) threads)
                 (push line lines)))
             approve-file--threads)
    (sort lines #'<)))

(defun approve-file--goto-threads (line)
  "Move to LINE and unfold its threads."
  (when (approve-file--folded-p line (gethash line approve-file--threads))
    (puthash line 'shown approve-file--folds)
    (approve-file-redraw))
  (approve-file--goto-line line))

(defun approve-file-next-thread ()
  "Move to the next line with an unresolved thread, and unfold its threads."
  (interactive)
  (approve-file--check)
  (let ((here (line-number-at-pos)))
    (approve-file--goto-threads
     (or (seq-find (lambda (line) (> line here)) (approve-file--unresolved-lines))
         (user-error "No unresolved thread after this")))))

(defun approve-file-previous-thread ()
  "Move to the previous line with an unresolved thread, and unfold its threads."
  (interactive)
  (approve-file--check)
  (let ((here (line-number-at-pos)))
    (approve-file--goto-threads
     (or (seq-find (lambda (line) (< line here)) (reverse (approve-file--unresolved-lines)))
         (user-error "No unresolved thread before this")))))

(defun approve-file-toggle-threads ()
  "Fold the threads of the line at point, or unfold them.
A folded line keeps its speech bubble."
  (interactive)
  (approve-file--check)
  (let* ((line (line-number-at-pos))
         (threads (or (gethash line approve-file--threads)
                      (user-error "No threads on this line"))))
    (puthash line (if (approve-file--folded-p line threads) 'shown 'folded)
             approve-file--folds)
    (approve-file-redraw)))

(defun approve-file--toggle-preflight ()
  "Return the side `approve-file-toggle-version' shows.
Signal a `user-error' outside a file view, or when the file has no
version there."
  (approve-file--check)
  (let* ((path (plist-get approve-context-file :path))
         (side (if (equal (plist-get approve-context-file :side) "LEFT") "RIGHT" "LEFT")))
    (when-let* ((why (approve-file--why-not path side)))
      (user-error "%s" why))
    side))

(put 'approve-file-toggle-version 'approve-preflight #'approve-file--toggle-preflight)

(defun approve-file-toggle-version ()
  "Show the file's other version: the base after the head, the head after the base.
Point goes to the same place in it, as far as the diff tells."
  (interactive)
  (let* ((side (approve-file--toggle-preflight))
         (path (plist-get approve-context-file :path)))
    (approve-file-show path side
                       (approve-file--map-line (approve-file--hunks path) (line-number-at-pos)
                                               (equal side "RIGHT")))))

(defun approve-file-refresh (&optional clear-caches)
  "Load the pull request again, and with it the file's changes and threads.
That is \\<approve-pr-mode-map>\\[approve-pr-refresh] in its PR buffer (flow F17); with a prefix argument
CLEAR-CACHES, the session's caches are forgotten too.  A new commit
on the file's side brings its new version."
  (interactive "P")
  (approve-file--check)
  (with-current-buffer (approve-model-owner-buffer)
    (approve-pr-refresh clear-caches)))

;;; Following the PR buffer

(defun approve-file--redraw-views ()
  "Redraw the file views of the current PR buffer.
It is on `approve-pr-redraw-hook'."
  (when (derived-mode-p 'approve-pr-mode)
    (dolist (buffer (approve-file--views (current-buffer)))
      (with-current-buffer buffer
        (approve-file-redraw)))))

(defun approve-file--kill-views ()
  "Kill the file views of the PR buffer being killed."
  (when (derived-mode-p 'approve-pr-mode)
    (mapc #'kill-buffer (approve-file--views (current-buffer)))))

(defun approve-file--setup-pr-buffer ()
  "Make the current PR buffer kill its file views when it is killed.
It is on `approve-pr-mode-hook'."
  (add-hook 'kill-buffer-hook #'approve-file--kill-views nil t))

(add-hook 'approve-pr-redraw-hook #'approve-file--redraw-views)
(add-hook 'approve-pr-mode-hook #'approve-file--setup-pr-buffer)

(keymap-set approve-pr-mode-map "F" #'approve-visit-file)

(provide 'approve-file)

;;; approve-file.el ends here
