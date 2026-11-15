;;; approve-diff.el --- The diff of a pull request -*- lexical-binding: t; -*-

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

;; The Diff section: every changed file, its hunks and lines, and the
;; review threads where they belong (docs/ui.org, "Bottom: files and
;; diff"; decision D12: our own parser and renderer).
;;
;; - `approve-diff-parse' turns a unified patch (from the REST files
;;   list, under the extra data's :patches) into hunks and lines, with
;;   old and new line numbers.  Parses are cached by patch.
;; - Each file is an `approve-file-section' (value: path).  It starts
;;   collapsed and its body is inserted only when it is expanded, so a
;;   PR of any size redraws fast.
;; - Lines are plain text, not sections: each carries the property
;;   `approve-line', (:path P :side LEFT|RIGHT :old N :new M :kind
;;   add|del|ctx).  Line numbers and the +/- marker are decoration
;;   (decision D28): copying leaves them out.
;; - A line with threads becomes an `approve-line-section' (value:
;;   (PATH SIDE LINE)): the line is its heading, with a speech bubble
;;   and the count at the right edge, and its threads are its body, so
;;   folding the line folds them away.  The bubble is an overlay's
;;   before-string on the line's newline, not text, so moving up and
;;   down never puts point in it.  Lines whose threads are all
;;   resolved start folded.  Threads do not fold by themselves.
;;   `approve-toggle-threads' (T) folds or unfolds every thread at
;;   once, and lines drawn later follow (the session's :threads).
;;   File-level threads are under "On this file" (folded when all are
;;   resolved), and threads whose line is no longer in the diff under
;;   "Outdated".
;; - Changed words in pairs of removed and added lines are marked when
;;   `approve-diff-refine-hunk' is non-nil.
;;
;; Layer rules: this is the renderer layer.  It reads the store through
;; selectors and inserts text; it never changes the store or calls the
;; network.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-model)
(require 'approve-session)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-render)
(require 'approve-thread)
(require 'approve-pr)
(require 'approve-sections)

;;; Options

(defcustom approve-diff-refine-hunk t
  "Whether changed words are marked in pairs of removed and added lines.
Like Magit's `magit-diff-refine-hunk', but for every hunk shown.
Only lines that are alike enough are compared.  To turn it off:

  (setq approve-diff-refine-hunk nil)"
  :type 'boolean
  :group 'approve)

(defcustom approve-diff-line-numbers t
  "Whether diff lines show their old and new line numbers.
The numbers are drawn as text, but copying leaves them out.  To
hide them:

  (setq approve-diff-line-numbers nil)"
  :type 'boolean
  :group 'approve)

(defcustom approve-diff-fontify t
  "Whether code in the diff is coloured by the file's major mode.
When non-nil, expanding a file fontifies its old and new lines in
the major mode `auto-mode-alist' gives its name (without running
the mode's hooks), the way `diff-font-lock-syntax' does, once per
patch (decision D30).  To turn it off:

  (setq approve-diff-fontify nil)"
  :type 'boolean
  :group 'approve)

;;; Section classes

(defclass approve-diff-section (magit-section) ()
  "The diff of the pull request.")
(defclass approve-file-section (magit-section) ()
  "The diff of one file; its value is the path.")
(defclass approve-hunk-section (magit-section) ()
  "One hunk; its value is (PATH . HEADER).")
(defclass approve-line-section (magit-section) ()
  "A diff line with threads; its value is (PATH SIDE LINE).")
(defclass approve-file-threads-section (magit-section) ()
  "File-level (value `file') or outdated (value `outdated') threads.")

;;; Parsing

(cl-defstruct (approve-diff-hunk (:constructor approve-diff-hunk-create)
                                 (:copier nil))
  "One hunk of a patch.
HEADER is the @@ line; OLD-START and NEW-START the first line
numbers; LINES a list of (KIND OLD NEW TEXT), KIND being `add',
`del', `ctx' or `nonl' (\"No newline at end of file\"), OLD and NEW
the line numbers (nil on the side a line is not on)."
  header old-start new-start lines)

(defconst approve-diff--hunk-header-regexp
  (rx bol "@@ -" (group (+ digit)) (? "," (+ digit))
      " +" (group (+ digit)) (? "," (+ digit)) " @@")
  "Regexp matching a hunk header; groups: old start, new start.")

(defvar approve-diff--parses (make-hash-table :test #'eq :weakness 'key)
  "Parsed patches by patch string.")

(defun approve-diff--parse-hunk (header lines)
  "Return the hunk with HEADER whose body is LINES, in order."
  (string-match approve-diff--hunk-header-regexp header)
  (let* ((old-start (string-to-number (match-string 1 header)))
         (new-start (string-to-number (match-string 2 header)))
         (old old-start)
         (new new-start)
         (parsed
          (mapcar (lambda (line)
                    (let ((marker (if (string-empty-p line) ?\s (aref line 0)))
                          (text (if (string-empty-p line) "" (substring line 1))))
                      (pcase marker
                        (?+ (prog1 (list 'add nil new text) (cl-incf new)))
                        (?- (prog1 (list 'del old nil text) (cl-incf old)))
                        (?\\ (list 'nonl nil nil line))
                        (_ (prog1 (list 'ctx old new text) (cl-incf old) (cl-incf new))))))
                  lines)))
    (approve-diff-hunk-create :header header :old-start old-start
                              :new-start new-start :lines parsed)))

(defun approve-diff-parse (patch)
  "Return the hunks of PATCH, a unified diff without file headers.
PATCH is the `patch' of a file in GitHub's REST files list.  Lines
before the first hunk header are ignored.  Results are cached by
PATCH, so parsing it again is free.

Example:
  (approve-diff-hunk-lines (car (approve-diff-parse \"@@ -1 +1 @@\\n-a\\n+b\")))
  => ((del 1 nil \"a\") (add nil 1 \"b\"))"
  (if (or (null patch) (string-empty-p patch))
      nil
    (or (gethash patch approve-diff--parses)
        (puthash patch
                 (let ((hunks nil) (header nil) (lines nil))
                   (dolist (line (split-string patch "\n"))
                     (cond
                      ((string-match-p approve-diff--hunk-header-regexp line)
                       (when header
                         (push (approve-diff--parse-hunk header (nreverse lines)) hunks))
                       (setq header line lines nil))
                      (header (push line lines))))
                   (when header
                     ;; A patch ending in a newline leaves an empty last line.
                     (when (and lines (string-empty-p (car lines)))
                       (pop lines))
                     (push (approve-diff--parse-hunk header (nreverse lines)) hunks))
                   (nreverse hunks))
                 approve-diff--parses))))

;;; Files

(defun approve-diff-file-entry (path)
  "Return the REST entry of PATH from the extra data, or nil."
  (when-let* ((table (approve-model-extra :patches)))
    (gethash path table)))

(defun approve-diff--change-icon (type)
  "Return the icon of GitHub's file change TYPE, in its face."
  (pcase type
    ("ADDED" (approve-icon 'file-added 'approve-file-added))
    ("DELETED" (approve-icon 'file-deleted 'approve-file-deleted))
    ("RENAMED" (approve-icon 'file-renamed 'approve-file-moved))
    ("COPIED" (approve-icon 'file-copied 'approve-file-moved))
    ("CHANGED" (approve-icon 'file-changed 'approve-file-modified))
    (_ (approve-icon 'file-modified 'approve-file-modified))))

(defun approve-diff--binary-p (file entry)
  "Return non-nil if FILE (from GraphQL) with REST ENTRY is binary."
  (and entry (not (alist-get 'patch entry))
       (zerop (+ (alist-get 'additions file) (alist-get 'deletions file)))
       (not (member (alist-get 'status entry) '("renamed" "copied")))))

(defun approve-diff--moved (old new)
  "Return the path of a file moved from OLD to NEW, as git's diffstat has it.
The directories both share are written once:
\"internal/engine/{queue.go → scheduler.go}\"."
  (let ((shared 0) (i 0) (limit (min (length old) (length new))))
    (while (and (< i limit) (eq (aref old i) (aref new i)))
      (when (eq (aref old i) ?/)
        (setq shared (1+ i)))
      (setq i (1+ i)))
    (if (> shared 0)
        (format "%s{%s → %s}" (substring new 0 shared)
                (substring old shared) (substring new shared))
      (format "%s → %s" old new))))

(defun approve-diff--file-heading (file entry &optional threads width asking)
  "Return the heading of the diff of FILE (from GraphQL) and its REST ENTRY.
It is the viewed mark (`approve-sections-viewed-mark'), an icon of
the change (`approve-diff--change-icon') and the path; the size is
in the file list.  THREADS, (UNRESOLVED . TOTAL) of the threads on
the file's lines or nil, is counted at the page's right edge,
padded on the left to WIDTH columns, so the counts of all headings
line up; red with ASKING, when one of them asks for changes.  A path
too long for its room loses directories in the middle
\(`approve-render-shorten-path')."
  (let-alist file
    (let* ((old (alist-get 'previous_filename entry))
           (marks (concat (approve-sections-viewed-mark .viewerViewedState) " "
                          (approve-diff--change-icon .changeType) " "))
           (count (approve-sections-thread-count threads asking))
           (count (if (string-empty-p count) count
                    (approve-render-pad count (or width 0) t)))
           (room (- approve-page-width
                    (string-width approve-sections-indentation)
                    (string-width marks)
                    (if (string-empty-p count) 0 (+ 2 (string-width count))))))
      (approve-render-justify
       (concat marks
               (approve-face (approve-render-shorten-path
                              (if (and old (not (equal old .path)))
                                  (approve-diff--moved old .path)
                                .path)
                              (max 10 room))
                             'approve-diff-file-heading))
       count))))

;;; Threads

(defun approve-diff-thread-index (threads lines)
  "Sort THREADS of one file by where they go in its diff.
LINES is a hash table of the (SIDE . LINE) pairs the diff shows.
Return a plist: :file (file-level threads), :outdated (threads whose
line is not in the diff), :lines, a hash table from (SIDE . LINE)
to the threads drawn under that line, in order, and :asking, the ids
of the threads that ask for changes that still stand
\(`approve-pr-threads-asking-for-changes')."
  (let ((index (make-hash-table :test #'equal))
        (file nil) (outdated nil))
    (dolist (thread threads)
      (let-alist thread
        (let ((key (cons .diffSide .line)))
          (cond ((equal .subjectType "FILE") (push thread file))
                ((and .line (gethash key lines))
                 (puthash key (append (gethash key index) (list thread)) index))
                (t (push thread outdated))))))
    (list :file (nreverse file) :outdated (nreverse outdated) :lines index
          :asking (mapcar (lambda (thread) (alist-get 'id thread))
                          (approve-pr-threads-asking-for-changes)))))

(defun approve-diff--line-keys (hunks)
  "Return a hash table of the (SIDE . LINE) pairs HUNKS show."
  (let ((keys (make-hash-table :test #'equal)))
    (dolist (hunk hunks)
      (dolist (line (approve-diff-hunk-lines hunk))
        (pcase-let ((`(,kind ,old ,new ,_) line))
          (pcase kind
            ('add (puthash (cons "RIGHT" new) t keys))
            ('del (puthash (cons "LEFT" old) t keys))
            ('ctx (puthash (cons "RIGHT" new) t keys)
                  (puthash (cons "LEFT" old) t keys))))))
    keys))

;;; Word refinement

(defconst approve-diff--refine-max-tokens 200
  "Lines with more tokens than this are not refined.")

(defun approve-diff--tokens (text)
  "Return TEXT split into words, spaces and single other characters.
Each token is (START . END) in TEXT."
  (let ((pos 0) (tokens nil))
    (while (string-match "\\w+\\|\\s-+\\|." text pos)
      (push (cons (match-beginning 0) (match-end 0)) tokens)
      (setq pos (match-end 0)))
    (nreverse tokens)))

(defun approve-diff--lcs-marks (a b)
  "Return the tokens of strings A and B that are not in their common part.
The value is (A-CHANGED . B-CHANGED), lists of (START . END), or nil
when the lines are too long or too different to be worth marking."
  (let* ((ta (vconcat (approve-diff--tokens a)))
         (tb (vconcat (approve-diff--tokens b)))
         (n (length ta)) (m (length tb)))
    (when (and (> n 0) (> m 0)
               (<= n approve-diff--refine-max-tokens)
               (<= m approve-diff--refine-max-tokens))
      (let ((table (make-vector (* (1+ n) (1+ m)) 0))
            (word (lambda (tokens i text)
                    (let ((token (aref tokens i)))
                      (substring text (car token) (cdr token))))))
        (cl-loop for i from (1- n) downto 0 do
                 (cl-loop for j from (1- m) downto 0 do
                          (aset table (+ (* i (1+ m)) j)
                                (if (equal (funcall word ta i a) (funcall word tb j b))
                                    (1+ (aref table (+ (* (1+ i) (1+ m)) (1+ j))))
                                  (max (aref table (+ (* (1+ i) (1+ m)) j))
                                       (aref table (+ (* i (1+ m)) (1+ j))))))))
        (let ((common (aref table 0)))
          ;; Lines with little in common are rewritten, not edited.
          (when (and (> common 0) (>= (* 2 common) (min n m)))
            (let ((i 0) (j 0) (changed-a nil) (changed-b nil))
              (while (and (< i n) (< j m))
                (cond ((equal (funcall word ta i a) (funcall word tb j b))
                       (cl-incf i) (cl-incf j))
                      ((>= (aref table (+ (* (1+ i) (1+ m)) j))
                           (aref table (+ (* i (1+ m)) (1+ j))))
                       (push (aref ta i) changed-a) (cl-incf i))
                      (t (push (aref tb j) changed-b) (cl-incf j))))
              (while (< i n) (push (aref ta i) changed-a) (cl-incf i))
              (while (< j m) (push (aref tb j) changed-b) (cl-incf j))
              (cons (nreverse changed-a) (nreverse changed-b)))))))))

(defun approve-diff--refine (lines)
  "Return a hash table from the lines of LINES to their changed parts.
Removed and added lines are paired in order within each run of
removed lines followed by added ones.  A value is a list of (START
. END) in the line's text."
  (let ((marks (make-hash-table :test #'eq))
        (rest lines))
    (while rest
      (let (dels adds)
        (while (and rest (eq (car (car rest)) 'del))
          (push (pop rest) dels))
        (while (and rest (eq (car (car rest)) 'add))
          (push (pop rest) adds))
        (when (and dels adds)
          (cl-loop for del in (nreverse dels)
                   for add in (nreverse adds)
                   for changed = (approve-diff--lcs-marks (nth 3 del) (nth 3 add))
                   when changed
                   do (puthash del (car changed) marks)
                   (puthash add (cdr changed) marks)))
        (unless (or dels adds)
          (pop rest))))
    marks))

;;; Syntax

(defun approve-diff--major-mode (path)
  "Return the major mode `auto-mode-alist' gives PATH, or nil."
  (let ((mode (assoc-default path auto-mode-alist #'string-match)))
    (when (consp mode)
      (setq mode (car mode)))
    (and mode (symbolp mode) (fboundp mode) mode)))

(defun approve-diff--faces-only (string)
  "Return STRING with only its `face' property kept."
  (let ((result (substring-no-properties string))
        (pos 0))
    (while (< pos (length string))
      (let ((next (or (next-single-property-change pos 'face string) (length string)))
            (face (get-text-property pos 'face string)))
        (when face
          (put-text-property pos next 'face face result))
        (setq pos next)))
    result))

(defun approve-diff--fontify-codes (mode codes)
  "Return CODES, a list of lines, fontified together in MODE.
The mode's hooks do not run.  Return nil if fontifying fails."
  (condition-case err
      (with-temp-buffer
        (insert (string-join codes "\n"))
        (let ((enable-local-variables nil)
              (inhibit-message t)
              (message-log-max nil))
          (delay-mode-hooks (funcall mode))
          (font-lock-ensure))
        (goto-char (point-min))
        (let ((lines nil))
          (dotimes (_ (length codes))
            (push (approve-diff--faces-only
                   (buffer-substring (point) (line-end-position)))
                  lines)
            (forward-line 1))
          (nreverse lines)))
    (error
     (approve-log 'debug "Fontifying in %s failed: %s" mode (error-message-string err))
     nil)))

(defvar approve-diff--fontified (make-hash-table :test #'eq :weakness 'key)
  "Fontified lines by patch string: (MODE . TABLE), see `approve-diff--fontify'.")

(defun approve-diff--fontify (path patch hunks)
  "Return a hash table from the lines of HUNKS to their code, fontified.
HUNKS are the parse of PATCH.  The new side (context and added
lines) and the old side (removed lines) are fontified as two texts
in the major mode of PATH, once per patch.  Return nil when
`approve-diff-fontify' is nil or PATH has no mode."
  (when-let* ((approve-diff-fontify)
              (mode (approve-diff--major-mode path)))
    (let ((cached (gethash patch approve-diff--fontified)))
      (if (eq (car cached) mode)
          (cdr cached)
        (let ((table (make-hash-table :test #'eq))
              (new nil) (old nil))
          (dolist (hunk hunks)
            (dolist (line (approve-diff-hunk-lines hunk))
              (pcase (car line)
                ((or 'add 'ctx) (push line new))
                ('del (push line old)))))
          (dolist (side (list (nreverse new) (nreverse old)))
            (cl-loop for line in side
                     for code in (approve-diff--fontify-codes
                                  mode (mapcar (lambda (line) (nth 3 line)) side))
                     do (puthash line code table)))
          (puthash patch (cons mode table) approve-diff--fontified)
          table)))))

;;; Lines

(defun approve-diff--number-width (hunks)
  "Return the width of the line numbers of HUNKS."
  (let ((largest 0))
    (dolist (hunk hunks)
      (dolist (line (approve-diff-hunk-lines hunk))
        (setq largest (max largest (or (nth 1 line) 0) (or (nth 2 line) 0)))))
    (max 3 (length (number-to-string largest)))))

(defun approve-diff--gutter-width (width)
  "Return the columns before the code of a line, for numbers WIDTH wide."
  (if approve-diff-line-numbers (+ (* 2 width) 3) 1))

(defun approve-diff--line-text (path line width changed &optional fontified)
  "Return LINE of PATH as text: numbers WIDTH wide, marker, code.
CHANGED lists the (START . END) of its changed words, or nil;
FONTIFIED is its code with syntax faces, or nil."
  (pcase-let* ((`(,kind ,old ,new ,code) line)
               (face (pcase kind
                       ('add 'approve-diff-added)
                       ('del 'approve-diff-removed)
                       ('nonl 'approve-dim)
                       (_ 'approve-diff-context)))
               (numbers (if approve-diff-line-numbers
                            (approve-face
                             (format (format "%%%ds %%%ds " width width)
                                     (if old (number-to-string old) "")
                                     (if new (number-to-string new) ""))
                             'approve-line-number)
                          ""))
               (marker (pcase kind ('add "+") ('del "-") ('nonl "") (_ " ")))
               (code (copy-sequence (or fontified code))))
    (when changed
      (let ((refine (if (eq kind 'del) 'approve-diff-refine-removed
                      'approve-diff-refine-added)))
        (dolist (part changed)
          (add-face-text-property (car part) (cdr part) refine nil code))))
    (let ((text (concat (approve-render-decoration numbers)
                        (approve-render-decoration (approve-face marker face))
                        (approve-face-add code face)
                        (approve-face "\n" face))))
      (unless (eq kind 'nonl)
        (put-text-property 0 (length text) 'approve-line
                           (list :path path
                                 :side (if (eq kind 'del) "LEFT" "RIGHT")
                                 :old old :new new :kind kind)
                           text))
      text)))

(defun approve-diff--insert-threads (threads gutter)
  "Insert THREADS, each indented by GUTTER columns."
  (dolist (thread threads)
    (let ((start (point)))
      (approve-thread-insert thread)
      (approve-render-prefix start (point) (make-string gutter ?\s)))))

(defun approve-diff--settled-p (threads)
  "Return non-nil if every one of THREADS is resolved and none pending.
Such threads start folded away."
  (seq-every-p (lambda (thread)
                 (and (alist-get 'isResolved thread)
                      (not (when-let* ((first (car (approve-model-nodes
                                                    (alist-get 'comments thread)))))
                             (approve-thread-pending-p first)))))
               threads))

(defun approve-diff--folded-p (threads)
  "Return non-nil if a line's (or a file's) THREADS start folded.
\\<approve-pr-mode-map>That is when \\[approve-toggle-threads] folded every thread (the
session's :threads is `folded'), not when it unfolded them (`shown'),
and otherwise when they are settled (`approve-diff--settled-p')."
  (pcase (approve-session-get :threads)
    ('folded t)
    ('shown nil)
    (_ (approve-diff--settled-p threads))))

(defun approve-diff--bubble (threads face &optional asking)
  "Return the speech bubble for THREADS, on a diff line whose face is FACE.
The bubble and the number of unresolved threads (or of all, dim,
when all are resolved) end at the page's right edge, as the counts
of the file headings do.  It takes the line's background, not its
colour (`approve-diff-bubble'): a bubble on a removed line is no
more alarming than one on an added line.  An overlay string would
else take the colour of the text under it.  It is red
\(`approve-thread-mark-changes-requested') when one of THREADS asks
for changes that still stand: its id is in ASKING (decision D34).
The cursor, at the end of the line, is drawn where the bubble
starts."
  (let* ((count (approve-sections-thread-count
                 (cons (seq-count (lambda (thread) (not (alist-get 'isResolved thread)))
                                  threads)
                       (length threads))
                 (seq-some (lambda (thread) (member (alist-get 'id thread) asking))
                           threads)))
         (background (approve-face-background face))
         (bubble (concat (propertize " " 'cursor t)
                         (propertize " " 'display
                                     `(space :align-to ,(- approve-page-width
                                                           (string-width count))))
                         count)))
    (approve-face-add (if background (approve-face-add bubble background) bubble)
                      'approve-diff-bubble)))

(defun approve-diff--insert-bubble (threads &optional asking)
  "Show the bubble of THREADS on the line just inserted, before its newline.
ASKING is as for `approve-diff--bubble'.  The bubble is an overlay's
before-string, not text: moving up and down never puts point in it,
copying leaves it out, and it goes with the line when the buffer is
redrawn (the overlay evaporates)."
  (let* ((newline (1- (point)))
         (overlay (make-overlay newline (point) nil t)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'approve-bubble t)
    (overlay-put overlay 'before-string
                 (approve-diff--bubble threads (get-text-property newline 'face) asking))))

(defun approve-diff--insert-hunk (path hunk width index &optional fontified)
  "Insert HUNK of PATH: its header, lines, and the threads under lines.
WIDTH is the width of line numbers; INDEX is the file's thread
index, from `approve-diff-thread-index'; FONTIFIED maps lines to
their code with syntax faces, or is nil."
  (let* ((lines (approve-diff-hunk-lines hunk))
         (marks (and approve-diff-refine-hunk (approve-diff--refine lines)))
         (by-line (plist-get index :lines))
         (gutter (approve-diff--gutter-width width)))
    (magit-insert-section (approve-hunk-section (cons path (approve-diff-hunk-header hunk)))
      (magit-insert-heading
        (approve-face (concat (approve-diff-hunk-header hunk) "\n")
                      'approve-diff-hunk-heading))
      (dolist (line lines)
        (let* ((text (approve-diff--line-text path line width (and marks (gethash line marks))
                                              (and fontified (gethash line fontified))))
               (side (if (eq (car line) 'del) "LEFT" "RIGHT"))
               (number (if (eq (car line) 'del) (nth 1 line) (nth 2 line)))
               (threads (append (gethash (cons side number) by-line)
                                (and (eq (car line) 'ctx)
                                     (gethash (cons "LEFT" (nth 1 line)) by-line)))))
          (if (null threads)
              (insert text)
            ;; The line is the heading of its threads: it folds them,
            ;; and its bubble says they are there.
            (magit-insert-section (approve-line-section (list path side number)
                                                        (approve-diff--folded-p threads))
              (magit-insert-heading text)
              (approve-diff--insert-bubble threads (plist-get index :asking))
              (approve-diff--insert-threads threads gutter))))))))

(defun approve-diff--insert-thread-group (value title threads hide &optional note)
  "Insert THREADS under a heading TITLE, as a section with VALUE.
HIDE collapses it at first; NOTE is dim text on the right."
  (when threads
    (magit-insert-section (approve-file-threads-section value hide)
      (magit-insert-heading
        (approve-render-justify (approve-sections-heading title (length threads))
                                (and note (approve-face note 'approve-dim))))
      (approve-diff--insert-threads threads 2))))

(defun approve-diff--insert-file-body (file)
  "Insert the body of the diff of FILE: threads on it, hunks, outdated."
  (let* ((path (alist-get 'path file))
         (entry (approve-diff-file-entry path))
         (patch (alist-get 'patch entry))
         (hunks (approve-diff-parse patch))
         (start (point))
         (index (approve-diff-thread-index (approve-pr-threads-for-file path)
                                           (approve-diff--line-keys hunks))))
    (let ((on-file (plist-get index :file)))
      (approve-diff--insert-thread-group 'file "On this file" on-file
                                         (approve-diff--folded-p on-file)))
    (cond
     (hunks
      (let ((width (approve-diff--number-width hunks))
            (fontified (approve-diff--fontify path patch hunks)))
        (dolist (hunk hunks)
          (approve-diff--insert-hunk path hunk width index fontified))))
     ((approve-pr-error 'patches)
      (insert (approve-render-failure "the diff" (approve-pr-error 'patches))))
     ((null entry)
      (insert (if (approve-pr-loading-p 'patches)
                  (approve-render-loading)
                (approve-render-empty "Diff not available."))))
     ((approve-diff--binary-p file entry)
      (insert (approve-render-empty "Binary file not shown.")))
     ((zerop (+ (alist-get 'additions file) (alist-get 'deletions file)))
      (insert (approve-render-empty "File renamed without changes.")))
     ((approve-pr-loading-p (cons 'diff path))
      (insert (approve-render-loading "the diff")))
     ((approve-pr-error (cons 'diff path))
      (insert (approve-render-failure "the diff" (approve-pr-error (cons 'diff path)))))
     (t
      (insert (approve-thing
               (concat (approve-face "Diff not shown by GitHub (too large)" 'approve-empty)
                       (if-let* ((hints (approve-render-hints
                                         '(approve-visit "load it anyway")
                                         '(approve-browse "open it on GitHub"))))
                           (concat (approve-face " · " 'approve-dim) hints)
                         ""))
               (list :kind 'load-diff :path path))
              "\n")))
    (approve-diff--insert-thread-group 'outdated "Outdated" (plist-get index :outdated)
                                       (not (eq (approve-session-get :threads) 'shown))
                                       "threads on lines no longer in the diff")
    (approve-render-prefix start (point) "    ")))

(defun approve-insert-diff-section ()
  "Insert the diff: every changed file, collapsed, its body drawn when expanded.
The heading says how many files the viewer has viewed and how many
threads on lines are unresolved; each file's heading counts its own."
  (let* ((files (approve-pr-files))
         (counts (approve-sections-thread-counts 'line))
         (asking (approve-sections-asking-paths 'line))
         (width (apply #'max 0 (mapcar (lambda (file)
                                         (string-width (approve-sections-thread-count
                                                        (gethash (alist-get 'path file)
                                                                 counts))))
                                       files))))
    (magit-insert-section (approve-diff-section)
      (magit-insert-heading
        (approve-sections-heading
         "Diff" (and files (or (approve-pr-field 'changedFiles) (length files)))
         (approve-render-join
          (list (and (approve-pr-loading-p 'patches)
                     (approve-face "loading the patches…" 'approve-loading))
                (and files
                     (format "%d/%d viewed" (approve-sections-viewed-count files)
                             (or (approve-pr-field 'changedFiles) (length files))))
                (and files (approve-sections-unresolved counts))))))
      (if (null files)
          (let ((start (point)))
            (insert (or (approve-sections-state 'files "the files") ""))
            (approve-sections-insert-body start))
        (dolist (file files)
          (let ((path (alist-get 'path file)))
            (magit-insert-section (approve-file-section path t)
              (magit-insert-heading
                approve-sections-indentation
                (approve-diff--file-heading file (approve-diff-file-entry path)
                                           (gethash path counts) width
                                           (member path asking)))
              (magit-insert-section-body
                (approve-diff--insert-file-body file))))))
      (insert "\n"))))

;;; Ranges of lines

;; A range is what a comment is written on: a context plist of kind
;; `line' or `lines' (approve-context.el), with :path, :side and :line
;; for its last line and, for several lines, :start-side and
;; :start-line for its first.  A LEFT end names a removed or context
;; line by its old number, a RIGHT end an added or context line by its
;; new number, as GitHub's review comments do.

(defun approve-diff--range-ends (range)
  "Return the ends of RANGE: ((START-SIDE . START-LINE) SIDE . LINE)."
  (cons (cons (or (plist-get range :start-side) (plist-get range :side))
              (or (plist-get range :start-line) (plist-get range :line)))
        (cons (plist-get range :side) (plist-get range :line))))

(defun approve-diff--end-p (end kind old new)
  "Return non-nil if the line of KIND with numbers OLD and NEW is END.
END is (SIDE . LINE)."
  (if (equal (car end) "LEFT")
      (and (memq kind '(del ctx)) (eql old (cdr end)))
    (and (memq kind '(add ctx)) (eql new (cdr end)))))

(defun approve-diff--take-range (ends items line-of)
  "Return the ITEMS from the first to the last end of ENDS, or nil.
LINE-OF returns (KIND OLD NEW) of an item.  Nil when the first end
is missing, or the last one does not follow it."
  (let ((state 'before) (taken nil))
    (while (and items (not (eq state 'done)))
      (let ((item (pop items)))
        (pcase-let ((`(,kind ,old ,new) (funcall line-of item)))
          (when (and (eq state 'before) (approve-diff--end-p (car ends) kind old new))
            (setq state 'in))
          (when (eq state 'in)
            (push item taken)
            (when (approve-diff--end-p (cdr ends) kind old new)
              (setq state 'done))))))
    (and (eq state 'done) (nreverse taken))))

(defun approve-diff-range-lines (range)
  "Return the lines of RANGE in its file's patch, in order.
Each is (KIND OLD NEW TEXT), as in `approve-diff-hunk-lines'.  Nil
when RANGE is not within one hunk of the patch in the store.

Example:
  (approve-diff-range-lines \='(:path \"a.go\" :side \"RIGHT\" :line 14
                              :start-side \"LEFT\" :start-line 13))
  => ((del 13 nil \"…\") (del 14 nil \"…\") (add nil 13 \"…\") (add nil 14 \"…\"))"
  (let ((ends (approve-diff--range-ends range))
        (patch (alist-get 'patch (approve-diff-file-entry (plist-get range :path)))))
    (seq-some (lambda (hunk)
                (approve-diff--take-range ends (approve-diff-hunk-lines hunk)
                                          #'identity))
              (approve-diff-parse patch))))

(defun approve-diff--file-section (path)
  "Return the section of the diff of PATH in the current buffer, or nil."
  (when-let* ((diff (and magit-root-section
                         (seq-find (lambda (section)
                                     (eq (oref section type) 'approve-diff-section))
                                   (oref magit-root-section children)))))
    (seq-find (lambda (section) (equal (oref section value) path))
              (oref diff children))))

(defun approve-diff-range-positions (range)
  "Return where each line of RANGE starts in the current buffer, in order.
Only diff lines count, not the threads under them.  Nil when the
range is not drawn: its file's body was never expanded, or the
buffer has no such lines."
  (when-let* ((file (approve-diff--file-section (plist-get range :path))))
    (let ((starts nil))
      (save-excursion
        (goto-char (or (oref file content) (oref file end)))
        (while (< (point) (oref file end))
          (when (get-text-property (point) 'approve-line)
            (push (point) starts))
          (forward-line 1)))
      (approve-diff--take-range
       (approve-diff--range-ends range) (nreverse starts)
       (lambda (start)
         (let ((line (get-text-property start 'approve-line)))
           (list (plist-get line :kind) (plist-get line :old) (plist-get line :new))))))))

(provide 'approve-diff)

;;; approve-diff.el ends here
