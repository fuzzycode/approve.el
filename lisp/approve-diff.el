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
;;   (PATH SIDE LINE)) with the threads under it.  File-level threads
;;   are under "On this file", and threads whose line is no longer in
;;   the diff under "Outdated".
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

(defun approve-diff--change-type (type)
  "Return GitHub's file change TYPE in words."
  (pcase type
    ("ADDED" "added") ("DELETED" "deleted") ("MODIFIED" "modified")
    ("RENAMED" "renamed") ("COPIED" "copied") ("CHANGED" "changed")
    (_ (downcase (or type "")))))

(defun approve-diff--viewed (state)
  "Return the viewed state of a file's diff heading, from GitHub's STATE."
  (concat (approve-sections-viewed-mark state) " "
          (pcase state
            ("VIEWED" (approve-face "viewed" 'approve-viewed))
            ("DISMISSED" (approve-face "changed since viewed" 'approve-dismissed))
            (_ (approve-face "not viewed" 'approve-unviewed)))))

(defun approve-diff--binary-p (file entry)
  "Return non-nil if FILE (from GraphQL) with REST ENTRY is binary."
  (and entry (not (alist-get 'patch entry))
       (zerop (+ (alist-get 'additions file) (alist-get 'deletions file)))
       (not (member (alist-get 'status entry) '("renamed" "copied")))))

(defun approve-diff--file-heading (file entry)
  "Return the heading of the diff of FILE (from GraphQL) and its REST ENTRY."
  (let-alist file
    (let ((old (alist-get 'previous_filename entry)))
      (approve-render-justify
       (approve-face (if (and old (not (equal old .path)))
                         (format "%s → %s" old .path)
                       .path)
                     'approve-diff-file-heading)
       (approve-render-join
        (list (approve-face (approve-diff--change-type .changeType) 'approve-dim)
              (if (approve-diff--binary-p file entry)
                  (approve-face "binary" 'approve-dim)
                (approve-sections-counts .additions .deletions))
              (approve-diff--viewed .viewerViewedState))
        "   ")))))

;;; Threads

(defun approve-diff-thread-index (threads lines)
  "Sort THREADS of one file by where they go in its diff.
LINES is a hash table of the (SIDE . LINE) pairs the diff shows.
Return a plist: :file (file-level threads), :outdated (threads whose
line is not in the diff) and :lines, a hash table from (SIDE . LINE)
to the threads drawn under that line, in order."
  (let ((index (make-hash-table :test #'equal))
        (file nil) (outdated nil))
    (dolist (thread threads)
      (let-alist thread
        (let ((key (cons .diffSide .line)))
          (cond ((equal .subjectType "FILE") (push thread file))
                ((and .line (gethash key lines))
                 (puthash key (append (gethash key index) (list thread)) index))
                (t (push thread outdated))))))
    (list :file (nreverse file) :outdated (nreverse outdated) :lines index)))

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

(defun approve-diff--line-text (path line width changed)
  "Return LINE of PATH as text: numbers WIDTH wide, marker, code.
CHANGED lists the (START . END) of its changed words, or nil."
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
               (code (copy-sequence code)))
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
  "Insert THREADS, each indented by GUTTER columns, display-only."
  (dolist (thread threads)
    (let ((start (point)))
      (approve-thread-insert thread)
      (approve-render-prefix start (point) (make-string gutter ?\s)))))

(defun approve-diff--insert-hunk (path hunk width index)
  "Insert HUNK of PATH: its header, lines, and the threads under lines.
WIDTH is the width of line numbers; INDEX is the file's thread
index, from `approve-diff-thread-index'."
  (let* ((lines (approve-diff-hunk-lines hunk))
         (marks (and approve-diff-refine-hunk (approve-diff--refine lines)))
         (by-line (plist-get index :lines))
         (gutter (approve-diff--gutter-width width)))
    (magit-insert-section (approve-hunk-section (cons path (approve-diff-hunk-header hunk)))
      (magit-insert-heading
        (approve-face (concat (approve-diff-hunk-header hunk) "\n")
                      'approve-diff-hunk-heading))
      (dolist (line lines)
        (let* ((text (approve-diff--line-text path line width (and marks (gethash line marks))))
               (side (if (eq (car line) 'del) "LEFT" "RIGHT"))
               (number (if (eq (car line) 'del) (nth 1 line) (nth 2 line)))
               (threads (append (gethash (cons side number) by-line)
                                (and (eq (car line) 'ctx)
                                     (gethash (cons "LEFT" (nth 1 line)) by-line)))))
          (if (null threads)
              (insert text)
            (magit-insert-section (approve-line-section (list path side number))
              (magit-insert-heading text)
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
    (approve-diff--insert-thread-group 'file "On this file" (plist-get index :file) nil)
    (cond
     (hunks
      (let ((width (approve-diff--number-width hunks)))
        (dolist (hunk hunks)
          (approve-diff--insert-hunk path hunk width index))))
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
     (t
      (insert (approve-face "Diff not shown by GitHub (too large)" 'approve-empty)
              (if-let* ((hints (approve-render-hints '("o" approve-browse "open it on GitHub"))))
                  (concat (approve-face " · " 'approve-dim) hints)
                "")
              "\n")))
    (approve-diff--insert-thread-group 'outdated "Outdated" (plist-get index :outdated) t
                                       "threads on lines no longer in the diff")
    (approve-render-prefix start (point) "    ")))

(defun approve-insert-diff-section ()
  "Insert the diff: every changed file, collapsed, its body drawn when expanded."
  (let ((files (approve-pr-files)))
    (magit-insert-section (approve-diff-section)
      (magit-insert-heading
        (approve-render-justify (approve-face "Diff" 'approve-heading)
                                (and (approve-pr-loading-p 'patches)
                                     (approve-face "loading the patches…" 'approve-loading))))
      (if (null files)
          (let ((start (point)))
            (insert (or (approve-sections-state 'files "the files") ""))
            (approve-sections-insert-body start))
        (dolist (file files)
          (let ((path (alist-get 'path file)))
            (magit-insert-section (approve-file-section path t)
              (let ((start (point)))
                (magit-insert-heading
                  (approve-diff--file-heading file (approve-diff-file-entry path)))
                (approve-render-prefix start (point) "  "))
              (magit-insert-section-body
                (approve-diff--insert-file-body file))))))
      (insert "\n"))))

(provide 'approve-diff)

;;; approve-diff.el ends here
