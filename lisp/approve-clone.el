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
;; A pull request finds its clone, and a clone's branch its pull
;; request, through the remotes (decision D45):
;;
;; - `approve-clone-remotes' reads a clone's remotes from its config
;;   file, without git, so the clones of a repository are found at
;;   once among many.
;; - `approve-clone-repository' says which GitHub repository a remote's
;;   URL names: on github.com or the host of `approve-api-host', an
;;   SSH alias followed through ssh -G.
;;
;; Layer rules: this is the operations layer, and only the clone
;; modules touch a clone.  It reads clones through Magit and never
;; changes them.  The core never requires it: approve.el does, and
;; puts its functions on the hooks the core offers.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'magit-git)
(require 'magit-diff)
(require 'magit-repos)
(require 'approve-api)

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

;;; Remotes

(defun approve-clone-parse-remote (url)
  "Return the repository a remote URL names, (:host :owner :repo), or nil.
URL is scp-like (git@github.com:OWNER/NAME.git) or a URL with a
scheme (https, ssh, git), with or without a user, a port and the
\".git\"; the host is in lower case.  nil when it names no
OWNER/NAME, such as a local path.

Example:
  (approve-clone-parse-remote \"git@github.com:magit/magit.git\")
  => (:host \"github.com\" :owner \"magit\" :repo \"magit\")"
  (when (and (stringp url)
             (or (string-match
                  (rx bos (in alpha) (* (in alnum "+.-")) "://"
                      (? (+ (not (in "@/"))) "@")
                      (group (+ (not (in "/:")))) (? ":" (* digit)) "/"
                      (group (+ (not (in "/")))) "/" (group (+? (not (in "/"))))
                      (? ".git") (? "/") eos)
                  url)
                 (string-match
                  (rx bos (? (+ (not (in "@/:"))) "@")
                      (group (+ (not (in "/:")))) ":" (? "/")
                      (group (+ (not (in "/")))) "/" (group (+? (not (in "/"))))
                      (? ".git") (? "/") eos)
                  url)))
    (list :host (downcase (match-string 1 url))
          :owner (match-string 2 url)
          :repo (match-string 3 url))))

(defun approve-clone--ssh-url-p (url)
  "Return non-nil if URL is reached through SSH, whose hosts may be aliases."
  (or (string-match-p "\\`\\(?:git\\+\\)?ssh://" url)
      (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" url))))

(defvar approve-clone--ssh-hosts (make-hash-table :test #'equal)
  "The host names SSH aliases stand for, asked of ssh this session.")

(defun approve-clone--ssh-hostname (alias)
  "Return the host name SSH connects to for ALIAS, in lower case.
It is the \"hostname\" of `ssh -G', which applies ~/.ssh/config, asked
once a session for each alias; ALIAS itself without ssh."
  (or (gethash alias approve-clone--ssh-hosts)
      (puthash alias
               (or (and (executable-find "ssh")
                        (with-temp-buffer
                          (and (zerop (call-process "ssh" nil t nil "-G" "--" alias))
                               (progn (goto-char (point-min))
                                      (re-search-forward "^hostname \\(.+\\)$" nil t))
                               (downcase (match-string 1)))))
                   alias)
               approve-clone--ssh-hosts)))

(defun approve-clone-github-hosts ()
  "Return the web hosts of GitHub that Approve talks to, in lower case.
They are github.com and the host of `approve-api-host', for GitHub
Enterprise."
  (delete-dups (list "github.com" (downcase (approve-api-web-host)))))

(defun approve-clone-repository (url)
  "Return the GitHub repository a remote URL names, (:host :owner :repo).
:host is a GitHub web host (`approve-clone-github-hosts').  An SSH
host that is none of them is looked up as an alias of
~/.ssh/config.  nil when URL names no repository on GitHub."
  (when-let* ((repository (approve-clone-parse-remote url)))
    (let* ((hosts (approve-clone-github-hosts))
           (host (plist-get repository :host))
           (host (if (or (member host hosts) (not (approve-clone--ssh-url-p url)))
                     host
                   (approve-clone--ssh-hostname host))))
      (when (member host hosts)
        (plist-put repository :host host)))))

(defun approve-clone-same-repository-p (one other)
  "Return non-nil if repositories ONE and OTHER are the same.
Each is (:host :owner :repo); GitHub ignores the case of names."
  (and one other
       (cl-every (lambda (key)
                   (eq t (compare-strings (plist-get one key) nil nil
                                          (plist-get other key) nil nil t)))
                 '(:host :owner :repo))))

(defun approve-clone--common-dir (clone)
  "Return the git directory CLONE shares with its worktrees, or nil.
CLONE is a top directory.  It is read from files: .git is the
directory, or a file naming it (a worktree, a submodule), whose
commondir file names the shared one."
  (let ((dot (expand-file-name ".git" clone)))
    (cond
     ((file-directory-p dot) (file-name-as-directory dot))
     ((file-regular-p dot)
      (with-temp-buffer
        (insert-file-contents dot)
        (when (re-search-forward "^gitdir: *\\(.+?\\) *$" nil t)
          (let* ((gitdir (file-name-as-directory
                          (expand-file-name (match-string 1) clone)))
                 (common (expand-file-name "commondir" gitdir)))
            (if (file-readable-p common)
                (with-temp-buffer
                  (insert-file-contents common)
                  (file-name-as-directory
                   (expand-file-name (string-trim (buffer-string)) gitdir)))
              gitdir))))))))

(defun approve-clone-remotes (clone)
  "Return the remotes of CLONE, ((NAME . URL) ...), in the order of its config.
CLONE is a top directory, a worktree's too.  They are read from the
config file, without git: a remote's first url.  Includes and
`insteadOf' rewrites are not followed."
  (when-let* ((dir (approve-clone--common-dir clone))
              (config (expand-file-name "config" dir))
              ((file-readable-p config)))
    (with-temp-buffer
      (insert-file-contents config)
      (let ((remote nil) (remotes nil))
        (while (not (eobp))
          (cond
           ((looking-at "[ \t]*\\[[ \t]*remote[ \t]+\"\\([^\"]*\\)\"[ \t]*\\]")
            (setq remote (match-string 1)))
           ((looking-at "[ \t]*\\[")
            (setq remote nil))
           ((and remote (not (assoc remote remotes))
                 (let ((case-fold-search t))
                   (looking-at "[ \t]*url[ \t]*=[ \t]*\"?\\([^\"\n]*?\\)\"?[ \t]*$")))
            (push (cons remote (match-string 1)) remotes)))
          (forward-line 1))
        (nreverse remotes)))))

(defun approve-clone-remote-repository (clone remote)
  "Return the GitHub repository of REMOTE of CLONE, or nil.
REMOTE is a remote's name; the value is as `approve-clone-repository'."
  (when-let* ((url (cdr (assoc remote (approve-clone-remotes clone)))))
    (approve-clone-repository url)))

(provide 'approve-clone)

;;; approve-clone.el ends here
