;;; approve-metadata.el --- Repository metadata, fetched once per session -*- lexical-binding: t; -*-

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

;; Lists about a repository that commands need now and then, fetched
;; the first time one asks and kept for the Emacs session (decision
;; D24):
;;
;;   mentionable-users  who can be @mentioned, as UserFields alists
;;   assignable-users   who can be assigned, as UserFields alists; the
;;                      people one can ask for a review, too
;;   labels             the labels, by name, as LabelFields alists
;;   milestones         the open milestones by due date, those without
;;                      one first, as MilestoneFields alists
;;   teams              the teams of the organization that owns the
;;                      repository, by name, as TeamFields alists; nil
;;                      when a user owns it
;;   viewer-teams       the teams of that organization the viewer is
;;                      in, by name, as TeamFields alists with their
;;                      ancestors; nil when a user owns it
;;   codeowners         the CODEOWNERS file of the pull request's base
;;                      branch, as a plist (:path :branch :rules), its
;;                      rules from `approve-codeowners-parse'; nil when
;;                      the branch has none.  One for each branch.
;;   refs               the 100 issues and pull requests updated last,
;;                      as alists of __typename, id, number, title,
;;                      state (and isDraft for pull requests)
;;   emoji              GitHub's emoji, (NAME . GLYPH) such as
;;                      ("tada" . "🎉"); GLYPH is nil for GitHub's own
;;                      images, such as :octocat:.  The same for every
;;                      repository of a host.
;;
;;   (approve-metadata-fetch 'emoji)            ; once; nothing if on its way
;;   (approve-metadata 'mentionable-users)      ; what was fetched, or nil
;;   (approve-metadata-wait 'labels)            ; fetched, waiting if need be
;;
;; Completion fetches in the background and makes do with what is
;; there (`approve-metadata-fetch', `approve-metadata'); a command that
;; needs a list before it can ask anything, such as the labels to
;; choose from, waits for it (`approve-metadata-wait').
;;
;; The repository is the one of the current buffer's store.  The cache
;; is shared by every buffer, keyed by API host, repository and kind;
;; `approve-metadata-clear-cache' (C-u g) empties it.  A failed fetch
;; is logged by approve-api and not kept, so the next one tries again.
;;
;; Layer rules: this is the operations layer.  It calls approve-api and
;; reads the store's meta data; it never touches the text of a buffer,
;; and what it fetches stays out of the store.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'approve-api)
(require 'approve-api-paging)
(require 'approve-codeowners)
(require 'approve-log)
(require 'approve-model)
(require 'approve-queries)

(defcustom approve-metadata-max-pages 10
  "How many pages of 100 a list of repository metadata takes, at most.
The lists are the people who can be mentioned or assigned, the
labels, the open milestones and the teams.  The repositories of
large organizations can have thousands of people and teams; the
first pages hold most of those one looks for.  To fetch more:

  (setq approve-metadata-max-pages 30)"
  :type 'natnum
  :group 'approve)

(defconst approve-metadata-kinds
  '((mentionable-users . "the people who can be mentioned")
    (assignable-users . "the people who can be assigned")
    (labels . "the labels")
    (milestones . "the open milestones")
    (teams . "the teams")
    (viewer-teams . "your teams")
    (codeowners . "the code owners")
    (refs . "the issues and pull requests updated last")
    (emoji . "GitHub's emoji"))
  "The kinds of metadata `approve-metadata-fetch' knows, and what they are.")

(defvar approve-metadata--cache (make-hash-table :test #'equal)
  "Metadata fetched this session, by key (`approve-metadata--key').")

(defvar approve-metadata--waiting (make-hash-table :test #'equal)
  "Fetches on their way, by key: who waits for them, newest first.
Each is (CALLBACK . ERRORBACK), as given to `approve-metadata-fetch'.")

(defun approve-metadata-clear-cache ()
  "Forget the metadata fetched this session.
Fetches on their way still finish and keep what they get."
  (clrhash approve-metadata--cache))

(defun approve-metadata--host ()
  "Return the API host of the current buffer's repository."
  (or (approve-query-host) approve-api-host "api.github.com"))

(defun approve-metadata--key (kind)
  "Return the cache key of KIND for the current buffer's repository.
The code owners are those of the pull request's base branch."
  (pcase kind
    ('emoji (list (approve-metadata--host) kind))
    ('codeowners (list (approve-metadata--host) (approve-model-meta :owner)
                       (approve-model-meta :repo) kind (approve-metadata--branch)))
    (_ (list (approve-metadata--host) (approve-model-meta :owner)
             (approve-model-meta :repo) kind))))

(defun approve-metadata--branch ()
  "Return the base branch of the current buffer's pull request, or nil."
  (and (approve-model-root-ref) (approve-pr-field 'baseRefName)))

(defun approve-metadata (kind)
  "Return the metadata KIND of the current buffer's repository, or nil.
It is nil too until it has been fetched: see `approve-metadata-fetch'
and `approve-metadata-wait'.  KIND is a key of `approve-metadata-kinds'.

Example: (alist-get \\='login (car (approve-metadata \\='mentionable-users)))"
  (gethash (approve-metadata--key kind) approve-metadata--cache))

;;; Fetching

(defun approve-metadata--buffer ()
  "Return the buffer metadata requests answer in.
It lives for the session, so a fetch ends even when the buffer that
asked for it was killed meanwhile."
  (get-buffer-create " *approve-metadata*"))

(defun approve-metadata--emoji (urls)
  "Return GitHub's emoji URLS, from REST /emojis, as (NAME . GLYPH).
GitHub draws a Unicode emoji from an image named after its code
points, such as unicode/1f44d.png; GLYPH is made of them, or nil for
GitHub's own images."
  (mapcar (lambda (entry)
            (let ((url (format "%s" (cdr entry))))
              (cons (format "%s" (car entry))
                    (and (string-match "/unicode/\\([0-9a-f-]+\\)\\.png" url)
                         (apply #'string
                                (mapcar (lambda (hex) (string-to-number hex 16))
                                        (split-string (match-string 1 url) "-")))))))
          urls))

(defconst approve-metadata--lists
  '((mentionable-users "queries/metadata/mentionable-users.graphql"
                       repository mentionableUsers)
    (assignable-users "queries/metadata/assignable-users.graphql"
                      repository assignableUsers)
    (labels "queries/metadata/labels.graphql" repository labels)
    (milestones "queries/metadata/milestones.graphql" repository milestones))
  "The metadata that is a paged list of the repository: (KIND QUERY . PATH).
QUERY takes $owner, $name and $after; PATH leads from its data to the
list.")

(cl-defun approve-metadata--pages (query variables &key path host callback errorback)
  "Fetch the pages of QUERY with VARIABLES; call CALLBACK with their nodes.
PATH lists the keys from the data to the paged connection, HOST is
the API host.  At most `approve-metadata-max-pages' pages are
fetched.  ERRORBACK is called with the error of a page that fails."
  (let ((nodes nil))
    (approve-api-graphql-all
     query variables
     :path path :max-pages approve-metadata-max-pages
     :buffer (approve-metadata--buffer) :host host :progress nil
     :on-page (lambda (data _page)
                (let ((connection data))
                  (dolist (key path)
                    (setq connection (alist-get key connection)))
                  (setq nodes (append nodes (alist-get 'nodes connection)))))
     :callback (lambda () (funcall callback nodes))
     :errorback errorback)))

(defun approve-metadata--teams (owner host callback errorback &optional login)
  "Fetch the teams of OWNER, on HOST, when it is an organization.
Call CALLBACK with them, or with nil when OWNER is a user, who has
no teams; ERRORBACK with the error.  With LOGIN, only the teams that
LOGIN is a member of, each with its ancestors."
  (approve-api-graphql
   "queries/metadata/owner.graphql" `((owner . ,owner))
   :buffer (approve-metadata--buffer) :host host :progress nil
   :callback (lambda (data)
               (if (equal (let-alist data .repositoryOwner.__typename) "Organization")
                   (approve-metadata--pages
                    (if login
                        "queries/metadata/viewer-teams.graphql"
                      "queries/metadata/teams.graphql")
                    `((owner . ,owner) ,@(and login `((login . ,login))))
                    :path '(organization teams) :host host
                    :callback callback :errorback errorback)
                 (funcall callback nil)))
   :errorback errorback))

(defconst approve-metadata--codeowners-paths
  '((github . ".github/CODEOWNERS") (root . "CODEOWNERS") (docs . "docs/CODEOWNERS"))
  "Where GitHub looks for CODEOWNERS, in its order: (ALIAS . PATH).
ALIAS is the query's name for the file and its variable.")

(defun approve-metadata--codeowners (owner repo branch host callback errorback)
  "Fetch the CODEOWNERS file of BRANCH in OWNER/REPO, on HOST.
Call CALLBACK with (:path :branch :rules) for the first file of
`approve-metadata--codeowners-paths' that BRANCH has, or nil when
it has none; ERRORBACK with the error."
  (approve-api-graphql
   "queries/metadata/codeowners.graphql"
   `((owner . ,owner) (name . ,repo)
     ,@(mapcar (lambda (entry) (cons (car entry) (format "%s:%s" branch (cdr entry))))
               approve-metadata--codeowners-paths))
   :buffer (approve-metadata--buffer) :host host :progress nil
   :callback (lambda (data)
               (funcall
                callback
                (seq-some (lambda (entry)
                            (when-let* ((text (alist-get 'text (alist-get (car entry)
                                                                         (alist-get 'repository data)))))
                              (list :path (cdr entry) :branch branch
                                    :rules (approve-codeowners-parse text))))
                          approve-metadata--codeowners-paths)))
   :errorback errorback))

(defun approve-metadata--request (kind callback errorback)
  "Fetch KIND for the current buffer's repository.
CALLBACK is called with the metadata, ERRORBACK with the error."
  (let ((buffer (approve-metadata--buffer))
        (host (approve-query-host))
        (owner (approve-model-meta :owner))
        (repo (approve-model-meta :repo))
        (paged (alist-get kind approve-metadata--lists)))
    (cond
     (paged
      (approve-metadata--pages (car paged) `((owner . ,owner) (name . ,repo))
                               :path (cdr paged) :host host
                               :callback callback :errorback errorback))
     ((eq kind 'teams)
      (approve-metadata--teams owner host callback errorback))
     ((eq kind 'viewer-teams)
      (approve-metadata--teams owner host callback errorback
                               (approve-model-meta :viewer)))
     ((eq kind 'codeowners)
      (approve-metadata--codeowners owner repo (approve-metadata--branch) host
                                    callback errorback))
     ((eq kind 'refs)
      (approve-api-graphql
       "queries/metadata/recent-refs.graphql"
       `((search . ,(format "repo:%s/%s sort:updated-desc" owner repo)))
       :buffer buffer :host host :progress nil
       :callback (lambda (data) (funcall callback (let-alist data .search.nodes)))
       :errorback errorback))
     ((eq kind 'emoji)
      (approve-api-rest
       "GET" "/emojis"
       :buffer buffer :host host :progress nil
       :callback (lambda (urls) (funcall callback (approve-metadata--emoji urls)))
       :errorback errorback))
     (t (error "Unknown metadata: %s" kind)))))

(defun approve-metadata--settle (key function argument)
  "End the fetch of KEY: call FUNCTION of each waiter with ARGUMENT.
A waiter is (CALLBACK . ERRORBACK); FUNCTION is `car' or `cdr'.  One
that signals is logged and doesn't keep the others waiting."
  (let ((waiters (reverse (gethash key approve-metadata--waiting))))
    (remhash key approve-metadata--waiting)
    (dolist (waiter waiters)
      (when-let* ((continuation (funcall function waiter)))
        (condition-case-unless-debug err
            (funcall continuation argument)
          (error (approve-log-condition "Repository metadata" err)))))))

(cl-defun approve-metadata-fetch (kind &key callback errorback)
  "Fetch the metadata KIND of the current buffer's repository, once.
Nothing is sent when it was fetched before or is on its way.
CALLBACK, if non-nil, is called with the metadata once it is there,
at once when it was fetched before.  ERRORBACK, if non-nil, is
called with the error when the fetch fails (see
`approve-api-graphql').  KIND is a key of `approve-metadata-kinds'.

Example:
  (approve-metadata-fetch
   \\='emoji :callback (lambda (emoji) (message \"%d\" (length emoji))))"
  (unless (assq kind approve-metadata-kinds)
    (error "Unknown metadata: %s" kind))
  (when-let* ((missing (approve-metadata--missing kind)))
    (error "Can't fetch %s without %s" kind missing))
  (let* ((key (approve-metadata--key kind))
         (none (make-symbol "none"))
         (value (gethash key approve-metadata--cache none))
         (waiter (cons callback errorback)))
    (cond
     ((not (eq value none))
      (when callback
        (funcall callback value)))
     ((not (eq (gethash key approve-metadata--waiting none) none))
      (push waiter (gethash key approve-metadata--waiting)))
     (t
      (puthash key (list waiter) approve-metadata--waiting)
      (approve-metadata--request
       kind
       (lambda (value)
         (puthash key value approve-metadata--cache)
         (approve-metadata--settle key #'car value))
       (lambda (err)
         (approve-metadata--settle key #'cdr err)))))
    nil))

;;; Waiting

(defun approve-metadata--what (kind)
  "Return what KIND is, for the current buffer's repository.
Example: \"the labels of acme/rocket\"."
  (let ((what (or (alist-get kind approve-metadata-kinds)
                  (error "Unknown metadata: %s" kind))))
    (pcase kind
      ('emoji what)
      ('viewer-teams (format "%s in %s" what (approve-model-meta :owner)))
      ('codeowners (format "%s of %s/%s on %s" what (approve-model-meta :owner)
                           (approve-model-meta :repo) (approve-metadata--branch)))
      (_ (format "%s of %s/%s" what (approve-model-meta :owner)
                 (approve-model-meta :repo))))))

(defun approve-metadata--missing (kind)
  "Return what the current buffer lacks to fetch KIND, or nil.
The code owners need the pull request's base branch, the viewer's
teams the viewer, which the pull request's first query brings."
  (pcase kind
    ('codeowners (and (null (approve-metadata--branch)) "the pull request's base branch"))
    ('viewer-teams (and (null (approve-model-meta :viewer)) "who you are"))))

(defun approve-metadata-wait (kind)
  "Return the metadata KIND of the current buffer's repository.
Fetch it first, when it hasn't been, and wait for it, saying so in
the echo area; \\[keyboard-quit] stops waiting, and the fetch goes on
in the background.  Signal a `user-error' with GitHub's message when
the fetch fails.  It is for commands that need the list before they
can ask anything, such as the labels to choose from.  KIND is a key
of `approve-metadata-kinds'.

Example:
  (mapcar (lambda (label) (alist-get \\='name label))
          (approve-metadata-wait \\='labels))"
  (unless (or (eq kind 'emoji) (approve-model-meta :owner))
    (user-error "No repository here"))
  (when (approve-metadata--missing kind)
    (user-error "Wait until the pull request has loaded"))
  (let* ((what (approve-metadata--what kind))
         (none (make-symbol "none"))
         (value (gethash (approve-metadata--key kind) approve-metadata--cache none))
         (failure nil))
    (when (eq value none)
      (approve-metadata-fetch kind
                              :callback (lambda (fetched) (setq value fetched))
                              :errorback (lambda (err) (setq failure err)))
      (with-temp-message (format "Fetching %s..." what)
        (while (and (eq value none) (not failure))
          (accept-process-output nil 0.05))))
    (when failure
      (user-error "Could not fetch %s: %s" what (approve-api-error-message failure)))
    value))

(provide 'approve-metadata)

;;; approve-metadata.el ends here
