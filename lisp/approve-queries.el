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

(provide 'approve-queries)

;;; approve-queries.el ends here
