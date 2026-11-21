;;; approve-codeowners.el --- Who owns a file, as CODEOWNERS says -*- lexical-binding: t; -*-

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

;; The code owners of a file, read from the text of a CODEOWNERS file
;; as GitHub reads it (decision D54):
;;
;;   (setq rules (approve-codeowners-parse text))
;;   (approve-codeowners-owners rules "src/app.js")
;;   => ("@acme/web" "@alice")
;;
;; Each line is a pattern and its owners: @login, @org/team or an
;; e-mail address.  A # that starts a word starts a comment, and a
;; backslash keeps a space or a # in a pattern.  Patterns are
;; gitignore's, as GitHub's documentation describes them: a leading
;; or middle slash anchors one to the root, else it matches at any
;; depth; a trailing slash, or a pattern that names a directory,
;; takes everything under it, but dir/* only the files right in it;
;; * and ? stay within a directory, ** crosses them.  GitHub has no
;; negation (!) nor character ranges ([ ]) here, and paths are
;; case-sensitive.  The last pattern that matches a path wins, and
;; one without owners leaves its files without any.
;;
;; Layer rules: this is the model layer, and pure: it reads text and
;; paths, nothing else.  Where the text comes from is
;; approve-metadata.el's business.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defun approve-codeowners--words (line)
  "Return the words of the CODEOWNERS LINE, without its comment.
A backslash keeps the next character in the word, a space or a #,
and stays in it for `approve-codeowners-pattern-regexp'; a # that
starts a word starts a comment."
  (let ((words nil) (word nil) (escape nil))
    (catch 'comment
      (dolist (char (string-to-list line))
        (cond (escape
               (push char word)
               (setq escape nil))
              ((eq char ?\\)
               (push char word)
               (setq escape t))
              ((memq char '(?\s ?\t ?\r))
               (when word
                 (push (concat (nreverse word)) words)
                 (setq word nil)))
              ((and (eq char ?#) (null word))
               (throw 'comment nil))
              (t (push char word))))
      nil)
    (when word
      (push (concat (nreverse word)) words))
    (nreverse words)))

(defun approve-codeowners--segment-regexp (segment)
  "Return the regexp of SEGMENT, a part of a pattern between slashes.
* is any text within a directory, ? one character of it, and a
backslash makes the next character literal."
  (let ((parts nil) (escape nil))
    (dolist (char (string-to-list segment))
      (cond (escape
             (push (regexp-quote (string char)) parts)
             (setq escape nil))
            ((eq char ?\\) (setq escape t))
            ((eq char ?*) (push "[^/]*" parts))
            ((eq char ??) (push "[^/]" parts))
            (t (push (regexp-quote (string char)) parts))))
    (apply #'concat (nreverse parts))))

(defun approve-codeowners-pattern-regexp (pattern)
  "Return a regexp that matches the paths CODEOWNERS PATTERN covers.
The paths are relative to the root of the repository, without a
leading slash.

Examples:
  (approve-codeowners-pattern-regexp \"*.js\")    ; any .js, at any depth
  (approve-codeowners-pattern-regexp \"/docs/\")  ; everything under docs/
  (approve-codeowners-pattern-regexp \"docs/*\")  ; the files right in docs/"
  (let ((segments (split-string pattern "/")))
    (if (equal (car segments) "")
        ;; A leading slash: from the root.
        (setq segments (cdr segments))
      ;; One name alone, with or without a trailing slash, is at any
      ;; depth, as if it started with **/.
      (when (and (or (null (cdr segments))
                     (and (null (cddr segments)) (equal (cadr segments) "")))
                 (not (equal (car segments) "**")))
        (push "**" segments)))
    ;; A trailing slash is /**: everything under the directory.
    (when (and (cdr segments) (equal (car (last segments)) ""))
      (setcar (last segments) "**"))
    (let ((last (1- (length segments)))
          (slash nil)
          (parts nil))
      (cl-loop
       for segment in segments
       for index from 0
       do (cond
           ((equal segment "**")
            (cond ((= index 0 last) (push ".+" parts))
                  ((= index 0) (push "\\(?:.+/\\)?" parts) (setq slash nil))
                  ((= index last) (push "/.*" parts))
                  (t (push "\\(?:/.+\\)?" parts) (setq slash t))))
           ((equal segment "*")
            ;; dir/* is the files right in dir, not deeper ones.
            (when slash (push "/" parts))
            (push "[^/]+" parts)
            (setq slash t))
           (t
            (when slash (push "/" parts))
            (push (approve-codeowners--segment-regexp segment) parts)
            ;; A name that is a directory takes what is under it.
            (when (= index last) (push "\\(?:/.*\\)?" parts))
            (setq slash t))))
      (concat "\\`" (apply #'concat (nreverse parts)) "\\'"))))

(defun approve-codeowners-parse (text)
  "Return the rules of TEXT, the content of a CODEOWNERS file.
Each rule is (REGEXP . OWNERS), OWNERS as written (\"@alice\",
\"@acme/web\", \"bob@example.com\"), nil for a pattern without
owners.  They come last line first, as the last pattern that
matches a path wins (`approve-codeowners-owners')."
  (let ((rules nil))
    (dolist (line (split-string (or text "") "\n"))
      (when-let* ((words (approve-codeowners--words line)))
        (push (cons (approve-codeowners-pattern-regexp (car words)) (cdr words))
              rules)))
    rules))

(defun approve-codeowners-owners (rules path)
  "Return the owners of PATH by RULES, from `approve-codeowners-parse'.
They are those of the last pattern that matches PATH, or nil when
none matches or it names no owners."
  (let ((case-fold-search nil))
    (cdr (seq-find (lambda (rule) (string-match-p (car rule) path)) rules))))

(defun approve-codeowners-predicate (rules owners)
  "Return a function of a path: non-nil if one of OWNERS owns it by RULES.
OWNERS are written as CODEOWNERS writes them, such as \"@alice\" or
\"@acme/web\", and compared ignoring case, as GitHub does.  Each
path is matched once; the answer is kept for the next time.

Example:
  (funcall (approve-codeowners-predicate rules \\='(\"@alice\")) \"src/app.js\")"
  (let ((answers (make-hash-table :test #'equal))
        (none (make-symbol "none")))
    (lambda (path)
      (let ((answer (gethash path answers none)))
        (if (not (eq answer none))
            answer
          (puthash path
                   (and (seq-some (lambda (owner)
                                    (seq-some (lambda (wanted)
                                                (string-equal-ignore-case owner wanted))
                                              owners))
                                  (approve-codeowners-owners rules path))
                        t)
                   answers))))))

(provide 'approve-codeowners)

;;; approve-codeowners.el ends here
