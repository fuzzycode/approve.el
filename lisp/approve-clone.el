;;; approve-clone.el --- The user's local clones -*- lexical-binding: t; -*-

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

;; Approve needs no local clone (decision D36), but uses the ones the
;; user has for more.  It finds them in `approve-clone-directories',
;; which is Magit's `magit-repository-directories' unless set (D37):
;;
;;   (setq approve-clone-directories '(("~/workspace" . 1)))
;;
;; - `approve-clone-list' gives the clones, in the order listed.
;; - `approve-clone-with-commit' gives the first clone that has a
;;   commit: a sha names one commit anywhere, so every clone is asked.
;; - `approve-clone-show-commit' shows a commit in Magit from that
;;   clone.  approve.el puts it on `approve-visit-commit-functions', so
;;   RET on a commit shows it there, and on GitHub when no clone has it.
;;
;; Layer rules: this is the operations layer, and only the clone
;; modules touch a clone.  It reads clones through Magit and never
;; changes them.  The core never requires it: approve.el does, and
;; puts its functions on the hooks the core offers.

;;; Code:

(require 'seq)
(require 'magit-git)
(require 'magit-diff)
(require 'magit-repos)

(defcustom approve-clone-directories 'magit
  "Where Approve finds the user's local clones, or `magit' for Magit's.
A list of (DIRECTORY . DEPTH) as in `magit-repository-directories':
DIRECTORY is a clone, or holds clones as deep as DEPTH (0 is the
directory itself; a negative DEPTH looks inside clones too).  The
symbol `magit', the default, means the value of
`magit-repository-directories'; nil means no clones.

Approve works without a clone.  With one, \\`RET' on a commit shows
it in Magit from the first clone that has it, instead of on GitHub.

  (setq approve-clone-directories
        \\='((\"~/workspace\" . 1) (\"~/src/emacs\" . 0)))"
  :type '(choice (const :tag "Those of `magit-repository-directories'" magit)
                 (repeat (cons directory (integer :tag "Depth"))))
  :group 'approve)

(defun approve-clone--directories ()
  "Return the (DIRECTORY . DEPTH) entries to find clones in."
  (if (eq approve-clone-directories 'magit)
      magit-repository-directories
    approve-clone-directories))

(defun approve-clone-list ()
  "Return the top directories of the user's clones, in the order listed.
They are found in `approve-clone-directories' each time, as Magit
lists repositories, so a clone made since is found too."
  (let ((magit-repository-directories (approve-clone--directories)))
    (delete-dups (mapcar #'expand-file-name (magit-list-repos)))))

(defun approve-clone--oid-p (oid)
  "Return non-nil if OID looks like a commit's sha, perhaps abbreviated."
  (and (stringp oid)
       (string-match-p "\\`[[:xdigit:]]\\{4,64\\}\\'" oid)))

(defun approve-clone-with-commit (oid)
  "Return the top directory of the first clone that has the commit OID.
OID is a sha, perhaps abbreviated.  Every clone of
`approve-clone-list' is asked in turn; nil when none has it."
  (and (approve-clone--oid-p oid)
       (seq-find (lambda (clone)
                   (let ((default-directory clone))
                     (magit-git-success "cat-file" "-e" (concat oid "^{commit}"))))
                 (approve-clone-list))))

(defun approve-clone-show-commit (oid &optional _url)
  "Show the commit OID with `magit-show-commit', from a clone that has it.
The clone is the first of `approve-clone-list' that has it; Magit
shows it where `magit-display-buffer-function' says, with the diff
arguments of its revision buffers.  Return the clone's directory,
or nil when no clone has OID.  For `approve-visit-commit-functions'."
  (when-let* ((clone (approve-clone-with-commit oid)))
    (let ((default-directory clone))
      (magit-show-commit oid (car (magit-diff-arguments 'magit-revision-mode))))
    clone))

(provide 'approve-clone)

;;; approve-clone.el ends here
