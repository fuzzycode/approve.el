;;; approve-queries.el --- Read a pull request from GitHub into the store -*- lexical-binding: t; -*-

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

;; The queries of the loading plan (docs/architecture.org, "Loading a
;; PR"; docs/flows.org, F02), one function each, loading what they get
;; into the store of the current buffer:
;;
;;   `approve-query-core'      the PR without its long lists, the viewer
;;   `approve-query-list'      files, threads, timeline, commits, checks
;;   `approve-query-patches'   the patch text of each file (REST)
;;
;; Change detection (`approve-query-changes') asks GitHub without
;; loading the store: its caller compares the answer with the store.
;; The lists of pull requests (`approve-query-search',
;; `approve-query-summary') keep what GitHub sends as it is, without a
;; store, as does `approve-query-viewer'.
;;
;; The PR is the one in the store's meta data (`approve-model-meta').
;; `approve-query-plan' lists the plan, so the buffer that loads can
;; run it and track each query under its key.
;;
;; A first load is progressive: each page goes into the store as it
;; arrives.  A refresh is atomic per query: pages are collected and
;; go into the store together once the last one is in, so a list never
;; shrinks to its first page while the rest is on the way.
;;
;; Patches have no node ids; they live in the extra data under
;; :patches, a hash table from path to the REST file entry (filename,
;; status, patch, previous_filename, additions, deletions, sha).
;;
;; Layer rules: this is the operations layer.  It calls approve-api and
;; writes the store; it never touches the text of a buffer.  Callbacks
;; run in the buffer that made the request (approve-api), so the store
;; they write is that buffer's.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'diff)
(require 'approve-api)
(require 'approve-api-paging)
(require 'approve-model)

(defconst approve-query-plan
  '((core)
    (files "queries/pull-request-files.graphql" repository pullRequest files)
    (threads "queries/pull-request-threads.graphql" repository pullRequest reviewThreads)
    (timeline "queries/pull-request-timeline.graphql" repository pullRequest timelineItems)
    (commits "queries/pull-request-commits.graphql" repository pullRequest commits)
    (patches))
  "The queries that load a PR: (KEY QUERY . PATH).
KEY names the query in the session (:loading, :errors); QUERY and
PATH are given to `approve-query-list'.  Core and patches have
functions of their own.  The checks query is not in the plan: it
loads when the Checks section is expanded.")

(defconst approve-query-checks
  '(checks "queries/pull-request-checks.graphql"
           repository pullRequest statusCheckRollup contexts)
  "The lazy checks query, in the form of `approve-query-plan'.")

(defun approve-query-variables ()
  "Return the GraphQL variables naming the PR of the current store."
  `((owner . ,(approve-model-meta :owner))
    (name . ,(approve-model-meta :repo))
    (number . ,(approve-model-meta :number))))

(defun approve-query-host ()
  "Return the API host of the PR of the current store, or nil.
Nil means `approve-api-host'; it is used when the store's web host
is unknown."
  (when-let* ((host (approve-model-meta :host)))
    (approve-api-host-from-web-host host)))

(defun approve-query--pull-request (data)
  "Return the pullRequest in DATA, a response's data."
  (let-alist data .repository.pullRequest))

(cl-defun approve-query-core (&key callback errorback)
  "Load the PR core and the viewer into the store.
The pullRequest becomes the root, and the viewer's login goes into
the meta data (:viewer).  CALLBACK is called with no arguments once
the store has it; ERRORBACK with the error otherwise.  A response
without a pull request fails with `approve-api-not-found'."
  (approve-api-graphql
   "queries/pull-request.graphql" (approve-query-variables)
   :host (approve-query-host)
   :progress nil
   :errorback errorback
   :callback
   (lambda (data)
     (let ((pull-request (approve-query--pull-request data)))
       (if (null pull-request)
           (funcall (or errorback #'approve-api-default-errorback)
                    (list 'approve-api-not-found "Pull request not found"))
         (approve-model-load pull-request t)
         (when-let* ((login (let-alist data .viewer.login)))
           (approve-model-set-meta :viewer login))
         (when-let* ((viewer (alist-get 'viewer data)))
           (approve-model-set-meta :viewer-ref (approve-model-load viewer)))
         (when callback
           (funcall callback)))))))

(cl-defun approve-query-list (query path &key on-page callback errorback atomic)
  "Load every page of the PR's list at PATH, fetched with QUERY.
QUERY is a file in graphql/queries/, PATH the keys from the data to
the paged connection, as in `approve-query-plan'.  The first page
replaces the list in the store and later pages are appended.  With
ATOMIC, pages are collected and go into the store together after
the last one.  ON-PAGE is called with the page number after each
page went into the store (not with ATOMIC); CALLBACK with no
arguments at the end; ERRORBACK with the error otherwise."
  (let ((pages nil))
    (approve-api-graphql-all
     query (approve-query-variables)
     :path path
     :host (approve-query-host)
     :progress nil
     :errorback errorback
     :on-page (lambda (data page)
                (let ((pull-request (approve-query--pull-request data)))
                  (cond
                   (atomic (push pull-request pages))
                   ((= page 1) (approve-model-load pull-request t))
                   (t (approve-model-load-page pull-request)))
                  (when (and on-page (not atomic))
                    (funcall on-page page))))
     :callback (lambda ()
                 (when atomic
                   (let ((pages (nreverse pages)))
                     (approve-model-load (car pages) t)
                     (mapc #'approve-model-load-page (cdr pages))))
                 (when callback
                   (funcall callback))))))

(defun approve-query--patch-entry (file)
  "Return the REST FILE entry as kept under :patches."
  (seq-filter (lambda (field)
                (memq (car field) '(filename status patch previous_filename
                                             additions deletions changes sha)))
              file))

(cl-defun approve-query-patches (&key on-page callback errorback atomic)
  "Load the patch of every file of the PR into the extra data.
The REST files list is fetched page by page (at most 3000 files)
into a hash table from path to entry, under :patches.  A first load
fills the table as pages arrive and calls ON-PAGE with the page
number; with ATOMIC, a new table replaces the old one at the end.
CALLBACK is called with no arguments at the end, ERRORBACK with the
error otherwise."
  (let ((table (make-hash-table :test #'equal)))
    (unless atomic
      (approve-model-set-extra :patches table))
    (approve-api-rest-all
     (format "/repos/%s/%s/pulls/%s/files" (approve-model-meta :owner)
             (approve-model-meta :repo) (approve-model-meta :number))
     :host (approve-query-host)
     :progress nil
     :errorback errorback
     :on-page (lambda (files page)
                (dolist (file files)
                  (puthash (alist-get 'filename file)
                           (approve-query--patch-entry file) table))
                (when (and on-page (not atomic))
                  (funcall on-page page)))
     :callback (lambda ()
                 (when atomic
                   (approve-model-set-extra :patches table))
                 (when callback
                   (funcall callback))))))

(cl-defun approve-query-run (key &key on-page callback errorback atomic)
  "Run the query named KEY in `approve-query-plan' or the checks query.
ON-PAGE, CALLBACK, ERRORBACK and ATOMIC are as for
`approve-query-list'; the core query has one page."
  (pcase key
    ('core (approve-query-core :callback callback :errorback errorback))
    ('patches (approve-query-patches :on-page on-page :callback callback
                                     :errorback errorback :atomic atomic))
    (_
     (let ((entry (if (eq key 'checks)
                      approve-query-checks
                    (or (assq key approve-query-plan)
                        (error "Unknown query %s" key)))))
       (approve-query-list (nth 1 entry) (nthcdr 2 entry)
                           :on-page on-page :callback callback
                           :errorback errorback :atomic atomic)))))

;;; Diffs GitHub did not send

(cl-defun approve-query-blob (oid path &key exact callback errorback)
  "Fetch the text of PATH at commit OID of the PR's repository.
CALLBACK is called with the text, or \"\" when the file does not exist
at OID (it was added or deleted).  ERRORBACK is called with the
error, also when the file is binary or too large for GitHub to send.
With EXACT, the text must be the file's bytes, to be written back:
ERRORBACK is called too when it is not (the file is not UTF-8, so
GitHub's text is not its content)."
  (approve-api-graphql
   "queries/blob.graphql"
   `((owner . ,(approve-model-meta :owner)) (name . ,(approve-model-meta :repo))
     (expression . ,(format "%s:%s" oid path)))
   :host (approve-query-host)
   :progress (format "Fetching %s" path)
   :errorback errorback
   :callback
   (lambda (data)
     (let-alist data
       (cond
        ((null .repository.object) (funcall callback ""))
        (.repository.object.isBinary
         (funcall errorback (list 'approve-api-error "The file is binary")))
        ((or .repository.object.isTruncated (null .repository.object.text))
         (funcall errorback (list 'approve-api-error
                                  "The file is too large for GitHub's API")))
        ((and exact (not (eql (string-bytes (encode-coding-string
                                             .repository.object.text 'utf-8-unix))
                              .repository.object.byteSize)))
         (funcall errorback (list 'approve-api-error
                                  (format "%s is not UTF-8 text" path))))
        (t (funcall callback .repository.object.text)))))))

(defun approve-query--unified-diff (old new)
  "Return the unified diff from text OLD to text NEW, from its first hunk.
It is computed by the `diff' program on temporary files (decision
D36 allows a local diff of fetched blobs)."
  (let ((old-file (make-temp-file "approve-old"))
        (new-file (make-temp-file "approve-new")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region old nil old-file nil 'silent)
            (write-region new nil new-file nil 'silent))
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8-unix))
              (call-process diff-command nil t nil "-u" old-file new-file))
            (goto-char (point-min))
            (if (re-search-forward "^@@" nil t)
                (buffer-substring-no-properties (match-beginning 0) (point-max))
              "")))
      (delete-file old-file)
      (delete-file new-file))))

(cl-defun approve-query-local-patch (path &key callback errorback)
  "Compute the patch of PATH from its blobs, for a file GitHub sent none of.
The base is the PR's base commit (at the old path of a renamed file),
the head its head commit.  The patch goes into the file's entry under
:patches, then CALLBACK is called with no arguments; ERRORBACK gets
the error of a failed fetch."
  (let* ((entry (gethash path (approve-model-extra :patches)))
         (old-path (or (alist-get 'previous_filename entry) path))
         (base (approve-pr-field 'baseRefOid))
         (head (approve-pr-field 'headRefOid)))
    (approve-query-blob
     base old-path
     :errorback errorback
     :callback
     (lambda (old)
       (approve-query-blob
        head path
        :errorback errorback
        :callback
        (lambda (new)
          (let ((table (approve-model-extra :patches)))
            (puthash path (cons (cons 'patch (approve-query--unified-diff old new))
                                (assq-delete-all 'patch (copy-alist (gethash path table))))
                     table))
          (when callback
            (funcall callback))))))))

;;; Markdown

(cl-defun approve-query-changes (&key callback errorback)
  "Ask GitHub what tells whether the PR changed since it was loaded.
That is change detection (decision D23, docs/flows.org F17).
CALLBACK is called with the pullRequest of
queries/pull-request-changes.graphql: its head, commit count, title,
state, draft state, last edit, and the newest conversation comments
and reviews with their last edits.  It does not go into the store:
the caller compares it with the store.  ERRORBACK is called with
the error."
  (approve-api-graphql
   "queries/pull-request-changes.graphql" (approve-query-variables)
   :host (approve-query-host)
   :progress nil
   :errorback errorback
   :callback (lambda (data)
               (if-let* ((pull-request (approve-query--pull-request data)))
                   (when callback
                     (funcall callback pull-request))
                 (funcall (or errorback #'approve-api-default-errorback)
                          (list 'approve-api-not-found "Pull request not found"))))))

(cl-defun approve-query-await-head (oid &key callback (tries 10))
  "Call CALLBACK once GitHub's head of the PR is OID, the commit just made.
GitHub moves a pull request's head a moment after a commit to its
branch, so the PR loaded at once could still show the old one.  The
head is asked for (the change detection query) up to TRIES times, a
second apart; CALLBACK is called after the last try too, and when
the question fails, so it always comes."
  (let ((buffer (current-buffer)))
    (approve-query-changes
     :callback (lambda (remote)
                 (if (or (equal (alist-get 'headRefOid remote) oid) (<= tries 1))
                     (funcall callback)
                   (run-at-time 1 nil
                                (lambda ()
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (approve-query-await-head oid :callback callback
                                                                :tries (1- tries))))))))
     :errorback (lambda (_err) (funcall callback)))))

(cl-defun approve-query-markdown (text &key callback errorback)
  "Render TEXT, GitHub-flavoured markdown, as GitHub shows a comment.
Mentions and references such as #12 are read in the PR's repository.
CALLBACK is called with the HTML, ERRORBACK with the error.  It
changes nothing on GitHub (REST POST /markdown)."
  (approve-api-rest "POST" "/markdown"
                    :payload `((text . ,text) (mode . "gfm")
                               (context . ,(format "%s/%s" (approve-model-meta :owner)
                                                   (approve-model-meta :repo))))
                    :reader 'text
                    :host (approve-query-host)
                    :progress "Rendering the preview"
                    :callback callback
                    :errorback errorback))

;;; Lists

(defun approve-query-pull-request-search (search)
  "Return SEARCH, GitHub's search syntax, made to find pull requests only.
\"is:pr\" goes in front unless SEARCH says is:pr or type:pr already.

Example:
  (approve-query-pull-request-search \"is:open author:@me\")
  => \"is:pr is:open author:@me\""
  (let ((search (string-trim (or search ""))))
    (if (string-match-p "\\(?:\\`\\|[[:space:]]\\)\\(?:is\\|type\\):pr\\(?:\\'\\|[[:space:]]\\)"
                        search)
        search
      (string-trim (concat "is:pr " search)))))

(cl-defun approve-query-search (search &key (first 20) after host callback errorback)
  "Ask GitHub for the pull requests SEARCH finds, the first FIRST of them.
SEARCH is GitHub's search syntax, such as \"is:open
review-requested:@me\"; \"is:pr\" is added when it says nothing of
the type.  AFTER is the endCursor of the page before, for the next
page.  HOST is the API host, `approve-api-host' by default.

CALLBACK is called with GitHub's search: an alist of issueCount (how
many it finds in all), pageInfo and nodes, the pull requests in the
shape of graphql/fragments/pull-request-summary.graphql, as GitHub
sends them (they go into no store).  ERRORBACK is called with the
error.

Example:
  (approve-query-search
   \"is:open author:@me\" :first 10
   :callback (lambda (search)
               (message \"%d found\" (alist-get \='issueCount search))))"
  (approve-api-graphql
   "queries/search-pull-requests.graphql"
   `((search . ,(approve-query-pull-request-search search))
     (first . ,first)
     ,@(and after `((after . ,after))))
   :host host
   :progress nil
   :errorback errorback
   :callback
   (lambda (data)
     (let ((search (alist-get 'search data)))
       (when callback
         (funcall callback
                  (cons (cons 'nodes (seq-filter (lambda (node) (alist-get 'number node))
                                                 (alist-get 'nodes search)))
                        (assq-delete-all 'nodes (copy-alist search)))))))))

(cl-defun approve-query-summary (pr &key callback errorback)
  "Ask GitHub for PR as a row of the lists.
PR is a plist from `approve-parse-url', (:host :owner :repo
:number); its host says which GitHub to ask.  CALLBACK is called
with the pull request, an alist in the shape of
graphql/fragments/pull-request-summary.graphql, as GitHub sends it,
which goes into no store.  ERRORBACK is called with the error, which
is `approve-api-not-found' when GitHub has no such pull request or
the token can't read it.

Example:
  (approve-query-summary
   \='(:host \"github.com\" :owner \"magit\" :repo \"magit\" :number 5000)
   :callback (lambda (pr) (message \"%s\" (alist-get \='title pr))))"
  (approve-api-graphql
   "queries/pull-request-summary.graphql"
   `((owner . ,(plist-get pr :owner))
     (name . ,(plist-get pr :repo))
     (number . ,(plist-get pr :number)))
   :host (approve-api-host-from-web-host (or (plist-get pr :host) "github.com"))
   :progress nil
   :errorback errorback
   :callback
   (lambda (data)
     (if-let* ((pull-request (approve-query--pull-request data)))
         (when callback
           (funcall callback pull-request))
       (funcall (or errorback #'approve-api-default-errorback)
                (list 'approve-api-not-found "Pull request not found"))))))

(cl-defun approve-query-viewer (&key host callback errorback)
  "Ask GitHub who the token belongs to.
HOST is the API host, `approve-api-host' by default.  CALLBACK is
called with the viewer, an alist in the shape of
graphql/fragments/user.graphql (login, name, url); ERRORBACK with
the error."
  (approve-api-graphql
   "queries/viewer.graphql" nil
   :host host
   :progress nil
   :errorback errorback
   :callback (lambda (data)
               (when callback
                 (funcall callback (alist-get 'viewer data))))))

;;; Details (eldoc)

(defun approve-query--details-request (kind key)
  "Return (QUERY VARIABLES PATH HOST) of the details query of KIND for KEY.
PATH leads from the response's data to the details.  HOST is the API
host to ask, or nil for the store's."
  (pcase kind
    ('user (list "queries/details/user.graphql" `((login . ,key)) '(user)))
    ('team (let ((parts (split-string key "/")))
             (list "queries/details/team.graphql"
                   `((org . ,(car parts)) (slug . ,(cadr parts)))
                   '(organization team))))
    ('ref (list "queries/details/ref.graphql"
                `((owner . ,(nth 0 key)) (name . ,(nth 1 key)) (number . ,(nth 2 key)))
                '(repository issueOrPullRequest)))
    ('pull-request (list "queries/details/pull-request.graphql"
                         `((owner . ,(nth 0 key)) (name . ,(nth 1 key))
                           (number . ,(nth 2 key)))
                         '(repository pullRequest)
                         (approve-api-host-from-web-host (or (nth 3 key) "github.com"))))
    ('commit (list "queries/details/commit.graphql"
                   `((owner . ,(approve-model-meta :owner))
                     (name . ,(approve-model-meta :repo)) (oid . ,key))
                   '(repository object)))
    ('comment-code (list "queries/details/comment-code.graphql" `((id . ,key)) '(node)))
    ('reactors (list "queries/details/reactors.graphql" `((id . ,key)) '(node)))
    (_ (error "No details query for %s" kind))))

(cl-defun approve-query-details (kind key &key callback errorback)
  "Fetch the extended details of the thing of KIND identified by KEY.
They are for eldoc (decision D27) and do not go into the store.
KIND and KEY are: `user' and a login, `team' and \"org/slug\", `ref'
and (OWNER REPO NUMBER), `pull-request' and (OWNER REPO NUMBER
WEB-HOST) for a row of a list, which asks the row's own GitHub,
`commit' and an oid of the PR's repository, `comment-code' and a
review comment id, `reactors' and the id of what was reacted to.
CALLBACK is called with the details, an alist; ERRORBACK with the
error, also when GitHub has no such thing."
  (pcase-let ((`(,query ,variables ,path ,host)
               (approve-query--details-request kind key)))
    (approve-api-graphql
     query variables
     :host (or host (approve-query-host))
     :progress nil
     :errorback errorback
     :callback
     (lambda (data)
       (let ((details data))
         (dolist (step path)
           (setq details (alist-get step details)))
         (if details
             (funcall callback details)
           (funcall (or errorback #'approve-api-default-errorback)
                    (list 'approve-api-not-found "Not found"))))))))

(provide 'approve-queries)

;;; approve-queries.el ends here
