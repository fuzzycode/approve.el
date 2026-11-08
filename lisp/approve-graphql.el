;;; approve-graphql.el --- Load GraphQL files with fragment includes -*- lexical-binding: t; -*-

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

;; Queries, mutations and fragments live as .graphql files under the
;; graphql/ directory (docs/decisions.org, D08).  A file pulls in
;; another one with an include directive, written where the text
;; should go:
;;
;;   ${include:fragments/user.graphql}
;;
;; `approve-graphql-load' returns a file's text with every include
;; replaced by the included file's own expanded text.  Includes are
;; resolved recursively; each file is included at most once per load,
;; so two fragments may share a third without defining it twice.  A
;; cycle or a missing file signals an `approve-graphql-error'.
;;
;; Loaded texts are cached by name and reloaded when one of the files
;; involved changes on disk.
;;
;; This file belongs to the transport layer (docs/architecture.org,
;; "Layers").  It only reads files: it never calls the network and
;; knows nothing about the store, buffers or rendering.

;;; Code:

(require 'seq)
(require 'approve-log)

(define-error 'approve-graphql-error "GraphQL file error" 'approve-error)
(define-error 'approve-graphql-missing-file "GraphQL file not found"
  'approve-graphql-error)
(define-error 'approve-graphql-include-cycle "GraphQL include cycle"
  'approve-graphql-error)

(defun approve-graphql--default-directory (library-directory)
  "Return the graphql directory that belongs to LIBRARY-DIRECTORY.
LIBRARY-DIRECTORY holds approve-graphql.el.  An installed package
keeps graphql/ next to the Lisp files; the source tree keeps it one
level up, next to lisp/."
  (let ((beside (expand-file-name "graphql" library-directory)))
    (if (file-directory-p beside)
        beside
      (expand-file-name "../graphql" library-directory))))

(defvar approve-graphql-directory
  (approve-graphql--default-directory
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory that holds the .graphql files.
Names given to `approve-graphql-load' and include paths are relative
to it.  The default is found from where this library was loaded:
graphql/ beside it (an installed package) or ../graphql (the source
tree).")

(defconst approve-graphql--include-regexp
  (rx "${include:" (group (+ (not (in "}")))) "}")
  "Regexp matching an include directive.
Group 1 is the path of the included file.")

(defvar approve-graphql--cache (make-hash-table :test 'equal)
  "Loaded texts, keyed by (DIRECTORY . FILE).
DIRECTORY is `approve-graphql-directory' and FILE the absolute name
of the loaded file.  Each value is (TEXT . STAMPS); STAMPS is a list
of (FILE . MTIME) for every file the text was built from.")

(defun approve-graphql-clear-cache ()
  "Forget every loaded GraphQL text.
The next `approve-graphql-load' reads the files again.  Edited files
are noticed without this; it is for files added or removed by hand."
  (interactive)
  (clrhash approve-graphql--cache))

(defun approve-graphql--mtime (file)
  "Return the modification time of FILE, or nil if it does not exist."
  (file-attribute-modification-time (file-attributes file)))

(defun approve-graphql--display-name (file)
  "Return FILE relative to `approve-graphql-directory' when it is inside."
  (let ((relative (file-relative-name file approve-graphql-directory)))
    (if (string-prefix-p ".." relative) file relative)))

(defun approve-graphql--read (file stack)
  "Return the contents of FILE, or signal that it is missing.
STACK lists the files that include FILE, innermost first."
  (unless (file-regular-p file)
    (signal 'approve-graphql-missing-file
            (cons (approve-graphql--display-name file)
                  (when stack
                    (list :included-from
                          (approve-graphql--display-name (car stack)))))))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun approve-graphql--expand (file stack seen)
  "Return the text of FILE with its includes expanded.
STACK lists the files being expanded, innermost first, to detect
cycles.  SEEN maps every file read in this load to its modification
time, taken before reading; an include of a file already in SEEN
expands to the empty string."
  (puthash file (approve-graphql--mtime file) seen)
  (let ((text (approve-graphql--read file stack))
        (stack (cons file stack))
        (start 0)
        (parts nil))
    (while (string-match approve-graphql--include-regexp text start)
      (let ((begin (match-beginning 0))
            (end (match-end 0))
            (included (expand-file-name (match-string 1 text)
                                        approve-graphql-directory)))
        (push (substring text start begin) parts)
        (when (member included stack)
          (signal 'approve-graphql-include-cycle
                  (list (mapconcat #'approve-graphql--display-name
                                   (reverse (cons included stack))
                                   " -> "))))
        (when (eq (gethash included seen 'unseen) 'unseen)
          (push (approve-graphql--expand included stack seen) parts))
        (setq start end)))
    (push (substring text start) parts)
    (apply #'concat (nreverse parts))))

(defun approve-graphql--fresh-p (stamps)
  "Return non-nil if no file in STAMPS changed since it was read.
STAMPS is a list of (FILE . MTIME)."
  (seq-every-p (lambda (stamp)
                 (equal (approve-graphql--mtime (car stamp)) (cdr stamp)))
               stamps))

(defun approve-graphql-load (name)
  "Return the text of the GraphQL file NAME with its includes expanded.
NAME is relative to `approve-graphql-directory', or absolute.  Each
${include:PATH} directive is replaced by the expanded text of PATH,
which is relative to `approve-graphql-directory' too.  A file is
included at most once per load: a repeated directive becomes empty.

Signal `approve-graphql-missing-file' when a file does not exist and
`approve-graphql-include-cycle' when files include each other.

The result is cached; it is rebuilt when any of its files changes.

Example:
  (approve-graphql-load \"queries/pull-request.graphql\")"
  (let* ((file (expand-file-name name approve-graphql-directory))
         (key (cons approve-graphql-directory file))
         (cached (gethash key approve-graphql--cache)))
    (if (and cached (approve-graphql--fresh-p (cdr cached)))
        (car cached)
      (let* ((seen (make-hash-table :test 'equal))
             (text (approve-graphql--expand file nil seen))
             (stamps nil))
        (maphash (lambda (seen-file mtime)
                   (push (cons seen-file mtime) stamps))
                 seen)
        (approve-log 'debug "Loaded GraphQL %s (%d files, %d chars)"
                     (approve-graphql--display-name file)
                     (length stamps) (length text))
        (puthash key (cons text stamps) approve-graphql--cache)
        text))))

(provide 'approve-graphql)

;;; approve-graphql.el ends here
