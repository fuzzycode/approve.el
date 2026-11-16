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
;; - `approve-clone-branch-head' gives a buffer's clone, its branch, and
;;   the repository and branch it goes to on GitHub.
;; - `approve-clone-branch-pull-requests' asks GitHub, once a session,
;;   for the pull requests a branch heads (a fork's into its parent
;;   too); `approve-clone-shown-pull-request' is the one Approve shows,
;;   the open one updated last unless `approve-clone-pick-pull-request'
;;   picked another.
;;
;; Layer rules: this is the operations layer, and only the clone
;; modules touch a clone.  It reads clones through Magit and never
;; changes them; what it asks GitHub stays out of the store.  Its two
;; commands, showing a commit and picking a branch's pull request, are
;; the clone's own.  The core never requires it: approve.el does, and
;; puts its functions on the hooks the core offers.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'parse-time)
(require 'magit-git)
(require 'magit-diff)
(require 'magit-repos)
(require 'approve-api)
(require 'approve-log)

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

;;; A buffer's branch

(defconst approve-clone--ref-format
  (concat "--format=%(refname:lstrip=2)%09%(upstream:remotename)%09%(upstream:remoteref)"
          "%09%(push:remotename)%09%(push)%09%(worktreepath)")
  "The `git for-each-ref' format of a branch, one line per branch.
It says where the branch pulls from and pushes to, and the worktree
that has it checked out.  Fields are separated by tabs, which no
branch or remote name holds.")

(defun approve-clone--ref-fields (line)
  "Return the branch that LINE of `approve-clone--ref-format' describes.
The value is a plist of :branch, :upstream-remote, :upstream-ref,
:push-remote, :push and :worktree, each nil when empty."
  (let ((fields (mapcar (lambda (field) (and (not (string-empty-p field)) field))
                        (split-string line "\t"))))
    (list :branch (nth 0 fields) :upstream-remote (nth 1 fields)
          :upstream-ref (nth 2 fields) :push-remote (nth 3 fields)
          :push (nth 4 fields) :worktree (nth 5 fields))))

(defun approve-clone--branch-target (fields remotes)
  "Return where the branch of FIELDS is on its remote: (REMOTE NAME HOW).
FIELDS are as `approve-clone--ref-fields'; REMOTES are the clone's
\(NAME . URL).  HOW is `push' when git says where the branch pushes
to, or where it pushes is not where it pulls from (a triangular
setup, whose `simple' push git can't resolve); `upstream' when it is
the branch it tracks; `name' for the same name on the remote a push
goes to, `origin' unless the config says otherwise.  nil when the
branch goes to no remote."
  (cl-destructuring-bind (&key branch upstream-remote upstream-ref push-remote push
                               &allow-other-keys)
      fields
    (let ((upstream-remote (and (not (equal upstream-remote ".")) upstream-remote))
          (push-remote (or push-remote (and (assoc "origin" remotes) "origin"))))
      (cond
       ((and push push-remote
             (string-prefix-p (format "refs/remotes/%s/" push-remote) push))
        (list push-remote
              (string-remove-prefix (format "refs/remotes/%s/" push-remote) push)
              'push))
       ((and push-remote upstream-remote (not (equal push-remote upstream-remote)))
        (list push-remote branch 'push))
       ((and upstream-remote upstream-ref (string-prefix-p "refs/heads/" upstream-ref))
        (list upstream-remote (string-remove-prefix "refs/heads/" upstream-ref) 'upstream))
       (push-remote
        (list push-remote branch 'name))))))

(defun approve-clone--branches (clone &optional branch)
  "Return the branches of CLONE as plists of `approve-clone--ref-fields'.
With BRANCH, only that one.  One git call."
  (let ((default-directory clone))
    (mapcar #'approve-clone--ref-fields
            (magit-git-lines "for-each-ref" approve-clone--ref-format
                             (if branch (concat "refs/heads/" branch) "refs/heads")))))

(defun approve-clone-branch-head (&optional directory)
  "Return the branch checked out where DIRECTORY is, and its place on GitHub.
DIRECTORY is `default-directory' by default.  The value is a plist:
:clone, the top directory; :branch, the branch checked out; :remote,
the remote it goes to; and :head, the repository and branch on
GitHub, (:host :owner :repo :branch).  The branch goes where it
pushes to, else where it pulls from, else to the same name on
\"origin\" (decision D45).  nil outside a clone, on a detached HEAD,
and when the branch goes to no GitHub repository.

The clone is the directory above DIRECTORY that holds .git.  Outside
a clone it costs that look; in a clone with no remote on GitHub,
reading its config file too; else two git calls through Magit."
  (let ((directory (or directory default-directory)))
    (when-let* ((top (locate-dominating-file directory ".git"))
                (clone (file-name-as-directory (expand-file-name top)))
                (remotes (approve-clone-remotes clone))
                ((seq-some (lambda (remote) (approve-clone-repository (cdr remote)))
                           remotes))
                (branch (let ((default-directory clone))
                          (magit-get-current-branch)))
                (fields (car (approve-clone--branches clone branch)))
                (target (approve-clone--branch-target fields remotes))
                (repository (approve-clone-repository
                             (cdr (assoc (car target) remotes)))))
      (list :clone clone :branch branch :remote (car target)
            :head (append repository (list :branch (cadr target)))))))

;;; The pull requests of a branch

(defvar approve-clone--pull-requests (make-hash-table :test #'equal)
  "The pull requests of the branches asked about this session, by key.
A key is `approve-clone--head-key''s; a value, the list of
`approve-clone--branch-pull-requests'.")

(defvar approve-clone--asking (make-hash-table :test #'equal)
  "Branches whose pull requests are on their way, by key: who waits.
Each waiter is (BUFFER CALLBACK ERRORBACK), newest first.")

(defvar approve-clone--picked (make-hash-table :test #'equal)
  "The pull request picked for a branch this session, by key: its id.")

(defvar approve-clone-picked-functions nil
  "Functions called when a pull request is picked for a branch.
Each is called with the branch, (:host :owner :repo :branch), and the
pull request picked, as in `approve-clone-branch-pull-requests'.")

(defun approve-clone-clear-cache ()
  "Forget the pull requests of branches asked about this session.
The ones picked are kept."
  (clrhash approve-clone--pull-requests))

(defun approve-clone--head-key (head)
  "Return the cache key of HEAD, a branch on GitHub (:host :owner :repo :branch)."
  (list (downcase (plist-get head :host)) (downcase (plist-get head :owner))
        (downcase (plist-get head :repo)) (plist-get head :branch)))

(defun approve-clone--name-with-owner (repository)
  "Return OWNER/NAME of REPOSITORY, (:host :owner :repo)."
  (format "%s/%s" (plist-get repository :owner) (plist-get repository :repo)))

(defun approve-clone--branch-pull-requests (data head)
  "Return the pull requests in DATA whose head is HEAD.
DATA is the answer to graphql/queries/branch-pull-requests.graphql;
HEAD is the branch, (:host :owner :repo :branch).  Each pull request
is an alist in the shape of the query's nodes, once each; open ones
come first, then the rest, each by the time they were updated, the
latest first."
  (let* ((name (approve-clone--name-with-owner head))
         (nodes (let-alist data
                  (append .repository.ref.associatedPullRequests.nodes
                          .repository.pullRequests.nodes
                          .repository.parent.pullRequests.nodes)))
         (ours (seq-filter
                (lambda (pr)
                  (let-alist pr
                    (and (equal .headRefName (plist-get head :branch))
                         .headRepository.nameWithOwner
                         (eq t (compare-strings .headRepository.nameWithOwner nil nil
                                                name nil nil t)))))
                nodes)))
    (sort (seq-uniq ours (lambda (a b) (equal (alist-get 'id a) (alist-get 'id b))))
          (lambda (a b)
            (let ((open-a (equal (alist-get 'state a) "OPEN"))
                  (open-b (equal (alist-get 'state b) "OPEN")))
              (if (eq open-a open-b)
                  (string> (alist-get 'updatedAt a) (alist-get 'updatedAt b))
                open-a))))))

(defun approve-clone--buffer ()
  "Return the buffer the questions about branches are answered in.
It lives for the session, so an answer is kept even when the buffer
that asked was killed meanwhile."
  (get-buffer-create " *approve-clone*"))

(defun approve-clone--settle (key function argument)
  "End the question about KEY: call each waiter's FUNCTION with ARGUMENT.
FUNCTION is `cadr' for the callback or `caddr' for the errorback; each
runs in its waiter's buffer, and not at all when it was killed."
  (let ((waiters (reverse (gethash key approve-clone--asking))))
    (remhash key approve-clone--asking)
    (dolist (waiter waiters)
      (when-let* ((continuation (funcall function waiter))
                  ((buffer-live-p (car waiter))))
        (with-current-buffer (car waiter)
          (condition-case-unless-debug err
              (funcall continuation argument)
            (error (approve-log-condition "The pull requests of a branch" err))))))))

(cl-defun approve-clone-branch-pull-requests (head &key refresh callback errorback)
  "Get the pull requests of HEAD, a branch on GitHub, asking GitHub once.
HEAD is (:host :owner :repo :branch), as `approve-clone-branch-head'
gives it.  CALLBACK is called with the list, open ones first, each by
the time it was updated, the latest first; each is a row of the lists
\(graphql/fragments/pull-request-summary.graphql) with headRefOid and
headRepository.  It is called at once when the branch was asked about
this session, unless REFRESH.  ERRORBACK is called with the error
\(see `approve-api-graphql'), which is not kept.  Both run in the
current buffer, and not at all when it was killed.  One query, of
cost 1 (decision D45); questions about a branch already on their way
wait for its answer.

Example:
  (approve-clone-branch-pull-requests
   \='(:host \"github.com\" :owner \"magit\" :repo \"magit\" :branch \"main\")
   :callback (lambda (prs) (message \"%d\" (length prs))))"
  (let* ((key (approve-clone--head-key head))
         (none (make-symbol "none"))
         (known (if refresh none (gethash key approve-clone--pull-requests none)))
         (waiter (list (current-buffer) callback errorback)))
    (cond
     ((not (eq known none))
      (when callback
        (funcall callback known)))
     ((gethash key approve-clone--asking)
      (push waiter (gethash key approve-clone--asking)))
     (t
      (puthash key (list waiter) approve-clone--asking)
      (approve-api-graphql
       "queries/branch-pull-requests.graphql"
       `((owner . ,(plist-get head :owner))
         (name . ,(plist-get head :repo))
         (branch . ,(plist-get head :branch))
         (qualifiedName . ,(concat "refs/heads/" (plist-get head :branch))))
       :buffer (approve-clone--buffer)
       :host (approve-api-host-from-web-host (plist-get head :host))
       :progress nil
       :callback (lambda (data)
                   (let ((prs (approve-clone--branch-pull-requests data head)))
                     (puthash key prs approve-clone--pull-requests)
                     (approve-clone--settle key #'cadr prs)))
       :errorback (lambda (err)
                    (approve-clone--settle key #'caddr err)))))
    nil))

(defun approve-clone-shown-pull-request (head prs &optional any-state)
  "Return the pull request of HEAD that Approve shows, among PRS.
HEAD is the branch, (:host :owner :repo :branch); PRS its pull
requests, as `approve-clone-branch-pull-requests' gives them.  It is
the one picked for the branch this session, else the open one
updated last.  nil when none is open, unless ANY-STATE: then the
closed or merged one updated last."
  (let ((picked (gethash (approve-clone--head-key head) approve-clone--picked)))
    (or (and picked
             (seq-find (lambda (pr) (equal (alist-get 'id pr) picked)) prs))
        (seq-find (lambda (pr) (equal (alist-get 'state pr) "OPEN")) prs)
        (and any-state (car prs)))))

(defun approve-clone--ago (iso)
  "Return how long ago the ISO 8601 time ISO was, in words.
Example: \"3 hours ago\"."
  (let ((seconds (float-time (time-subtract nil (parse-iso8601-time-string iso)))))
    (cl-flet ((ago (count unit)
                (format "%d %s%s ago" count unit (if (= count 1) "" "s"))))
      (cond ((< seconds 60) "just now")
            ((< seconds 3600) (ago (floor seconds 60) "minute"))
            ((< seconds 86400) (ago (floor seconds 3600) "hour"))
            (t (ago (floor seconds 86400) "day"))))))

(defun approve-clone--describe-pull-request (pr)
  "Return the line PR is listed as when one is picked, without its number.
Example: \"SP8: the clone and its lines  open · into main · 2 hours ago\"."
  (let-alist pr
    (format "%s  %s · into %s · %s"
            .title (if (and (eq .isDraft t) (equal .state "OPEN")) "draft" (downcase .state))
            .baseRefName (approve-clone--ago .updatedAt))))

(defun approve-clone-read-pull-request (prompt prs &optional default)
  "Read one of PRS with PROMPT, DEFAULT the one first offered.
PRS are pull requests as `approve-clone-branch-pull-requests' gives
them; return the one read."
  (let* ((choices (mapcar (lambda (pr)
                            (cons (format "#%s %s" (alist-get 'number pr)
                                          (approve-clone--describe-pull-request pr))
                                  pr))
                          prs))
         (default (car (rassq default choices)))
         (choice (completing-read prompt
                                  (lambda (string predicate action)
                                    (if (eq action 'metadata)
                                        '(metadata (display-sort-function . identity))
                                      (complete-with-action action choices string predicate)))
                                  nil t nil nil default)))
    (cdr (assoc choice choices))))

(defun approve-clone-wait-pull-requests (head)
  "Return the pull requests of HEAD, asking GitHub first if need be.
HEAD is a branch on GitHub, (:host :owner :repo :branch).  It waits,
saying so in the echo area; \\[keyboard-quit] stops waiting.  Signal a
`user-error' with GitHub's message when the question fails."
  (let* ((none (make-symbol "none"))
         (prs none)
         (failure nil))
    (approve-clone-branch-pull-requests
     head
     :callback (lambda (answer) (setq prs answer))
     :errorback (lambda (err) (setq failure err)))
    (when (eq prs none)
      (with-temp-message (format "Asking GitHub for the pull requests of %s..."
                                 (plist-get head :branch))
        (while (and (eq prs none) (not failure))
          (accept-process-output nil 0.05))))
    (when failure
      (user-error "Could not ask GitHub for the pull requests of %s: %s"
                  (plist-get head :branch) (approve-api-error-message failure)))
    prs))

(defun approve-clone-pick-pull-request ()
  "Pick which pull request of the current branch Approve shows here.
The branch is the one checked out where the current buffer is; its
pull requests are offered, open ones first, the one shown now first
of all.  The choice is kept for the session, for every buffer of the
branch (decision D45), and `approve-clone-picked-functions' are run."
  (interactive)
  (let* ((branch (or (approve-clone-branch-head)
                     (user-error "No branch on GitHub here")))
         (head (plist-get branch :head))
         (prs (or (approve-clone-wait-pull-requests head)
                  (user-error "%s has no pull request on GitHub" (plist-get head :branch))))
         (shown (approve-clone-shown-pull-request head prs t))
         (pr (approve-clone-read-pull-request
              (format "Pull request of %s: " (plist-get head :branch)) prs shown)))
    (puthash (approve-clone--head-key head) (alist-get 'id pr) approve-clone--picked)
    (run-hook-with-args 'approve-clone-picked-functions head pr)
    (message "Showing #%s for %s" (alist-get 'number pr) (plist-get head :branch))
    pr))

(provide 'approve-clone)

;;; approve-clone.el ends here
