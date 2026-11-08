;;; approve-model.el --- Normalized data store for Approve -*- lexical-binding: t; -*-

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

;; The data of one pull request, held buffer-locally (docs/architecture.org,
;; "Data model"; decision D10).
;;
;; GraphQL responses are normalized into a store keyed by `__typename'
;; and `id': every object with both becomes an entity, and the places
;; that held it hold a ref, (:approve-ref TYPENAME ID), instead.
;; Loading the same entity again merges its fields, so a mutation
;; response patches every place the entity appears.  Connections are
;; unwrapped to their nodes, keeping `totalCount' and `pageInfo' when
;; they were selected.  Later pages of a list are appended with
;; `approve-model-load-page'; creations and deletions need explicit
;; list edits (`approve-model-append-ref', `approve-model-remove-ref').
;;
;; Next to the store live the PR's meta data (host, owner, repo,
;; number, viewer), data without node ids (patches, the commit range)
;; and `approve-session', the UI state that is never sent anywhere.
;;
;; Selectors (`approve-pr', `approve-pr-threads-for-file', ...) are
;; the only way renderers read data; they hide the store layout and
;; resolve refs only for what they return.  They read through the
;; root, so every PR-level query loads its pullRequest with SET-ROOT
;; (see `approve-model-load'): any of them may arrive first or alone.
;;
;; The split layout's diff buffer sets `approve-model-store-buffer' to
;; its PR buffer; every function here then reads and writes the state
;; of that buffer.
;;
;; Layer rules: this is the model layer.  It never calls the network
;; and never touches the text of a buffer; it only holds buffer-local
;; data.  It requires nothing from Approve but `approve-log'.

;;; Code:

(require 'seq)
(require 'approve-log)

;;; Errors

(define-error 'approve-model-error "Approve model error" 'approve-error)
(define-error 'approve-model-not-found "Entity not found in the store"
  'approve-model-error)
(define-error 'approve-model-invalid-data "Invalid data for the store"
  'approve-model-error)
(define-error 'approve-model-invalid-ref "Not an entity ref"
  'approve-model-error)

;;; Buffer-local state

(defvar-local approve-model--store nil
  "The normalized store: a hash table of typename → (id → alist).")

(defvar-local approve-model--root nil
  "Ref to the root entity, the PullRequest of the buffer.")

(defvar-local approve-model--meta nil
  "Plist about the PR of the buffer: :host :owner :repo :number :viewer.")

(defvar-local approve-model--extra nil
  "Plist of data without node ids.
:patches maps a path to its parsed patch (from REST); :range is the
commit range under review (nil means all changes).")

(defvar-local approve-session nil
  "Plist of UI state that is never sent anywhere.
:loading   list of section keys still loading (a set)
:errors    alist of section key → error
:inflight  list of target idents with a mutation in flight (a set)
:drafts    alist of target → unsent text
:layout    `single' or `split'
Read and change it with `approve-session-get' and friends.")

(defvar-local approve-model-store-buffer nil
  "Buffer whose store this buffer uses, or nil for its own.
The diff buffer of the split layout sets it to its PR buffer, so
both buffers read and write one store and one session.")

(defvar approve-model--append-pages nil
  "Non-nil while `approve-model-load-page' normalizes the outer level.")

(defun approve-model--owner ()
  "Return the buffer that owns the state of the current buffer.
Signal `approve-model-error' if `approve-model-store-buffer' names a
killed buffer."
  (let ((owner approve-model-store-buffer))
    (cond ((null owner) (current-buffer))
          ((buffer-live-p owner) owner)
          (t (signal 'approve-model-error
                     (list "The buffer owning the store was killed" owner))))))

(defmacro approve-model--with-store (&rest body)
  "Run BODY in the buffer that owns the store of the current buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (approve-model--owner)
     ,@body))

(defun approve-model-init (&optional meta)
  "Start an empty store for the current buffer, with META.
META is a plist with :host :owner :repo :number and :viewer.  The
root, the extra data and `approve-session' are reset too.  Call it
once when a PR buffer is set up; refreshing reloads data into the
existing store instead.

Example:
  (approve-model-init \\='(:host \"github.com\" :owner \"magit\"
                          :repo \"magit\" :number 5000))"
  (approve-model--with-store
    (setq approve-model--store (make-hash-table :test #'equal)
          approve-model--root nil
          approve-model--meta meta
          approve-model--extra nil
          approve-session nil)))

(defun approve-model-meta (&optional key)
  "Return the value of KEY in the meta plist, or the whole plist.
Example: (approve-model-meta :owner) => \"magit\"."
  (approve-model--with-store
    (if key (plist-get approve-model--meta key) approve-model--meta)))

(defun approve-model-set-meta (key value)
  "Set KEY of the meta plist to VALUE and return VALUE."
  (approve-model--with-store
    (setq approve-model--meta (plist-put approve-model--meta key value))
    value))

(defun approve-model-extra (key)
  "Return the extra data under KEY, such as :patches or :range."
  (approve-model--with-store
    (plist-get approve-model--extra key)))

(defun approve-model-set-extra (key value)
  "Set the extra data under KEY to VALUE and return VALUE."
  (approve-model--with-store
    (setq approve-model--extra (plist-put approve-model--extra key value))
    value))

;;; Refs

(defun approve-model-make-ref (typename id)
  "Return a ref to the entity with TYPENAME and ID."
  (list :approve-ref typename id))

(defun approve-model-ref-p (object)
  "Return non-nil if OBJECT is a ref."
  (and (consp object) (eq (car object) :approve-ref)))

(defun approve-model-ref-type (ref)
  "Return the typename of REF."
  (nth 1 ref))

(defun approve-model-ref-id (ref)
  "Return the id of REF."
  (nth 2 ref))

(defun approve-model--object-p (value)
  "Return non-nil if VALUE is a JSON object, an alist with symbol keys.
Lists of objects, lists of refs and refs themselves are not objects."
  (and (consp value)
       (consp (car value))
       (let ((key (caar value)))
         (and key (symbolp key) (not (keywordp key))))))

;;; Connections

(defun approve-model-connection-p (value)
  "Return non-nil if VALUE is a connection with paging data.
Such a value is ((nodes . LIST) (totalCount . N) (pageInfo . ...)),
with at least one of `totalCount' and `pageInfo'.  A connection
selected without them is stored as its plain list of nodes."
  (and (approve-model--object-p value)
       (assq 'nodes value)
       (or (assq 'totalCount value) (assq 'pageInfo value))
       t))

(defun approve-model-nodes (value)
  "Return the nodes of VALUE, a connection or a plain list."
  (if (approve-model-connection-p value)
      (alist-get 'nodes value)
    value))

(defun approve-model-total-count (value)
  "Return the `totalCount' of connection VALUE, or nil."
  (and (approve-model-connection-p value)
       (alist-get 'totalCount value)))

(defun approve-model-page-info (value)
  "Return the `pageInfo' of connection VALUE, or nil."
  (and (approve-model-connection-p value)
       (alist-get 'pageInfo value)))

(defun approve-model-has-next-page-p (value)
  "Return non-nil if connection VALUE says there is a next page."
  (alist-get 'hasNextPage (approve-model-page-info value)))

(defun approve-model-truncated-p (value)
  "Return non-nil if connection VALUE holds fewer nodes than exist.
That is when there is a next page, or when `totalCount' is larger
than the number of nodes."
  (or (and (approve-model-has-next-page-p value) t)
      (let ((total (approve-model-total-count value)))
        (and total (> total (length (approve-model-nodes value)))))))

(defun approve-model--make-connection (nodes total-count page-info)
  "Return a connection of NODES, keeping TOTAL-COUNT and PAGE-INFO.
Return just NODES when both are absent.  TOTAL-COUNT and PAGE-INFO
are the (KEY . VALUE) pairs of the original, or nil."
  (if (or total-count page-info)
      `((nodes . ,nodes)
        ,@(and total-count (list total-count))
        ,@(and page-info (list page-info)))
    nodes))

;;; Normalization

(defun approve-model--type-table (typename)
  "Return the table of entities of TYPENAME, creating it if needed."
  (or (gethash typename approve-model--store)
      (puthash typename (make-hash-table :test #'equal) approve-model--store)))

(defun approve-model--entity (typename id)
  "Return the stored alist of the entity with TYPENAME and ID, or nil."
  (and approve-model--store
       (when-let* ((table (gethash typename approve-model--store)))
         (gethash id table))))

(defun approve-model--normalize (value)
  "Store the entities in VALUE and return VALUE with refs in their place."
  (cond
   ((approve-model--object-p value)
    (cond ((alist-get 'id value) (approve-model--normalize-entity value))
          ((assq 'nodes value) (approve-model--normalize-connection value))
          (t (approve-model--normalize-fields value))))
   ((or (consp value) (vectorp value))
    (mapcar #'approve-model--normalize value))
   (t value)))

(defun approve-model--normalize-fields (object)
  "Return OBJECT with each field value normalized."
  (mapcar (lambda (field)
            (cons (car field) (approve-model--normalize (cdr field))))
          object))

(defun approve-model--normalize-connection (connection)
  "Return CONNECTION unwrapped to its nodes, keeping its paging data.
Only the outermost paged connections of a page are appended: below a
connection that carries `pageInfo', the nodes are normalized as a
first page.  A connection without `pageInfo' (such as `headCommit:
commits(last: 1)') passes the page on to its nodes."
  (let ((nodes (let ((approve-model--append-pages
                      (and approve-model--append-pages
                           (not (assq 'pageInfo connection)))))
                 (approve-model--normalize (alist-get 'nodes connection)))))
    (approve-model--make-connection nodes
                                    (assq 'totalCount connection)
                                    (assq 'pageInfo connection))))

(defun approve-model--normalize-entity (object)
  "Store OBJECT as an entity, merged with any stored one; return a ref.
Signal `approve-model-invalid-data' if OBJECT has an `id' but no
`__typename' (decision D08)."
  (let ((typename (alist-get '__typename object))
        (id (alist-get 'id object)))
    (cond
     ((null typename)
      (signal 'approve-model-invalid-data
              (list "Object with an id but no __typename; select it"
                    id (mapcar #'car object))))
     ((not (stringp id))
      (approve-model--normalize-fields object))
     (t
      (let* ((append-pages approve-model--append-pages)
             (fields (approve-model--normalize-fields object))
             (table (approve-model--type-table typename))
             (old (gethash id table)))
        (puthash id (if old
                        (approve-model--merge old fields append-pages)
                      fields)
                 table)
        (approve-model-make-ref typename id))))))

(defun approve-model--page-p (value)
  "Return non-nil if VALUE is a connection that carries `pageInfo'."
  (and (approve-model-connection-p value) (assq 'pageInfo value)))

(defun approve-model--append-page (old new)
  "Return connection OLD with the nodes of page NEW appended.
Nodes already in OLD are skipped.  `totalCount' and `pageInfo' come
from NEW, or from OLD where NEW lacks them."
  (let ((seen (make-hash-table :test #'equal))
        (nodes (reverse (approve-model-nodes old))))
    (dolist (node nodes)
      (puthash node t seen))
    (dolist (node (approve-model-nodes new))
      (unless (gethash node seen)
        (puthash node t seen)
        (push node nodes)))
    (approve-model--make-connection
     (nreverse nodes)
     (or (assq 'totalCount new) (assq 'totalCount old))
     (or (assq 'pageInfo new) (assq 'pageInfo old)))))

(defun approve-model--merge (old new append-pages)
  "Return entity alist OLD with the fields of NEW merged in.
Fields of NEW replace those of OLD.  When APPEND-PAGES is non-nil, a
connection with `pageInfo' in NEW is appended to the connection in
OLD instead (`approve-model--append-page')."
  (nconc
   (mapcar (lambda (field)
             (let ((old-field (assq (car field) old)))
               (if (and append-pages
                        old-field
                        (approve-model--page-p (cdr field))
                        (approve-model-connection-p (cdr old-field)))
                   (cons (car field)
                         (approve-model--append-page (cdr old-field)
                                                     (cdr field)))
                 field)))
           new)
   (seq-remove (lambda (field) (assq (car field) new)) old)))

;;; Loading

(defun approve-model-load (data &optional set-root)
  "Normalize DATA into the store and return it with refs for entities.
DATA is decoded JSON as ghub returns it: objects are alists with
symbol keys and arrays are lists.  Every object with `__typename' and
a string `id' becomes an entity; loading an entity again merges its
fields (new values win), and a connection loaded again replaces the
old one (a first page).  If DATA itself is an entity, the result is
its ref.  With SET-ROOT, make that ref the root; signal
`approve-model-invalid-data' if DATA is not an entity.

Selectors read through the root, and any query of the PR may arrive
first or alone (another may fail).  So load the pullRequest of every
PR-level query (core, files, threads, timeline, commits, checks)
with SET-ROOT t; it is the same ref each time.

An object with an `id' but no `__typename' signals
`approve-model-invalid-data'; the store may then hold the part of
DATA that was normalized before it.

Example:
  (approve-model-load (let-alist response .data.repository.pullRequest) t)"
  (approve-model--with-store
    (unless approve-model--store
      (setq approve-model--store (make-hash-table :test #'equal)))
    (let ((result (approve-model--normalize data)))
      (when set-root
        (unless (approve-model-ref-p result)
          (signal 'approve-model-invalid-data
                  (list "The root must be an entity" (car-safe result))))
        (setq approve-model--root result))
      result)))

(defun approve-model-load-page (data)
  "Load DATA, a later page of a paged list, into the store.
Like `approve-model-load', except for the outermost connections in
DATA that carry `pageInfo' and belong to an entity already in the
store: their nodes are appended to the stored ones (nodes already
there are skipped), and their `pageInfo' and `totalCount' replace
the stored ones.  Connections inside the appended nodes are first
pages and replace as usual.  Load first pages with
`approve-model-load'."
  (let ((approve-model--append-pages t))
    (approve-model-load data)))

(defun approve-model-patch (data)
  "Patch the store with DATA, a mutation response.
The same as `approve-model-load': entities in DATA are merged with
the stored ones.  Creations and deletions also need a list edit
\(`approve-model-append-ref', `approve-model-remove-ref')."
  (approve-model-load data))

;;; Reading

(defconst approve-model--back-pointer-fields '(pullRequestReview replyTo)
  "Fields that point back up the graph, resolved one level only.
A comment's `pullRequestReview' leads to the review and from there
to all its comments; resolving that fully for every comment shown
would cost the size of the review per comment.  So a back-pointer
resolves to its entity's own fields, with the entity's refs left as
refs; read further with `approve-model-get-ref'.")

(defun approve-model--resolve (value path)
  "Return VALUE with its refs replaced by their entities, resolved.
PATH is the list of refs being resolved on the way to VALUE; a ref
on it stays a ref, which ends cycles.  A ref to an entity that is
not in the store also stays a ref.  Fields in
`approve-model--back-pointer-fields' resolve one level only."
  (cond
   ((approve-model-ref-p value)
    (let ((entity (approve-model--resolvable value path)))
      (if entity
          (approve-model--resolve entity (cons value path))
        value)))
   ((approve-model--object-p value)
    (mapcar (lambda (field)
              (cons (car field)
                    (if (and (memq (car field)
                                   approve-model--back-pointer-fields)
                             (approve-model-ref-p (cdr field)))
                        (or (copy-alist
                             (approve-model--resolvable (cdr field) path))
                            (cdr field))
                      (approve-model--resolve (cdr field) path))))
            value))
   ((consp value)
    (mapcar (lambda (item) (approve-model--resolve item path)) value))
   (t value)))

(defun approve-model--resolvable (value path)
  "Return the stored entity of VALUE if it should be resolved, else nil.
That is when VALUE is a ref that is not on PATH and whose entity is
in the store."
  (and (approve-model-ref-p value)
       (not (member value path))
       (approve-model--entity (approve-model-ref-type value)
                              (approve-model-ref-id value))))

(defun approve-model--raw (node)
  "Return the stored alist of NODE if it is a ref, else NODE."
  (if (approve-model-ref-p node)
      (approve-model--entity (approve-model-ref-type node)
                             (approve-model-ref-id node))
    node))

(defun approve-model-get (typename id &optional field)
  "Return the entity with TYPENAME and ID, with its refs resolved.
With FIELD, a symbol, return only that field, resolved.  With FIELD
a list of symbols, return an alist of those fields.  Signal
`approve-model-not-found' if the entity is not in the store.

Refs are resolved through the store, except a ref to an entity that
is being resolved on the same path (a cycle, such as a review's
comment pointing back to the review): it stays a ref.  Back-pointers
\(`approve-model--back-pointer-fields', such as a comment's
`pullRequestReview') resolve one level: the entity's own fields,
with its refs left as refs.

Example:
  (approve-model-get \"User\" \"U_kgDO\" \\='login) => \"octocat\""
  (approve-model--with-store
    (let ((entity (approve-model--entity typename id))
          (path (list (approve-model-make-ref typename id))))
      (unless entity
        (signal 'approve-model-not-found (list typename id)))
      (cond ((null field) (approve-model--resolve entity path))
            ((symbolp field)
             (approve-model--resolve (alist-get field entity) path))
            (t (mapcar (lambda (name)
                         (cons name (approve-model--resolve
                                     (alist-get name entity) path)))
                       field))))))

(defun approve-model-get-ref (ref &optional field)
  "Return the entity REF points to, like `approve-model-get' with FIELD.
Signal `approve-model-invalid-ref' if REF is not a ref."
  (unless (approve-model-ref-p ref)
    (signal 'approve-model-invalid-ref (list ref)))
  (approve-model-get (approve-model-ref-type ref) (approve-model-ref-id ref)
                     field))

(defun approve-model-root-ref ()
  "Return the ref to the root entity, or nil before it is loaded."
  (approve-model--with-store
    approve-model--root))

(defun approve-model-root (&optional field)
  "Return the root entity, or its FIELD, like `approve-model-get'.
Signal `approve-model-not-found' when no root is set."
  (let ((root (approve-model-root-ref)))
    (unless root
      (signal 'approve-model-not-found (list "No root entity")))
    (approve-model-get-ref root field)))

(defun approve-model-has-entity-p (typename id)
  "Return non-nil if the entity with TYPENAME and ID is in the store."
  (approve-model--with-store
    (and (approve-model--entity typename id) t)))

;;; Changing the store

(defun approve-model--set-field (ref field value)
  "Set FIELD of the entity of REF to VALUE.
Signal `approve-model-not-found' if the entity is not stored."
  (let* ((typename (approve-model-ref-type ref))
         (id (approve-model-ref-id ref))
         (entity (approve-model--entity typename id)))
    (unless entity
      (signal 'approve-model-not-found (list typename id)))
    (puthash id
             (if (assq field entity)
                 (mapcar (lambda (pair)
                           (if (eq (car pair) field) (cons field value) pair))
                         entity)
               (append entity (list (cons field value))))
             (approve-model--type-table typename))))

(defun approve-model--edit-list (owner-ref field function)
  "Replace the nodes in FIELD of OWNER-REF with FUNCTION's result.
FUNCTION gets the nodes and returns the new nodes.  A connection
keeps its `pageInfo', and its `totalCount' moves by the change in
length.  Return the new value of FIELD."
  (unless (approve-model-ref-p owner-ref)
    (signal 'approve-model-invalid-ref (list owner-ref)))
  (approve-model--with-store
    (let* ((value (alist-get field (approve-model--raw owner-ref)))
           (nodes (approve-model-nodes value))
           (new-nodes (funcall function nodes))
           (total (assq 'totalCount value))
           (new-value
            (if (approve-model-connection-p value)
                (approve-model--make-connection
                 new-nodes
                 (and total
                      (cons 'totalCount
                            (max 0 (+ (or (cdr total) 0)
                                      (- (length new-nodes)
                                         (length nodes))))))
                 (assq 'pageInfo value))
              new-nodes)))
      (approve-model--set-field owner-ref field new-value)
      new-value)))

(defun approve-model-append-ref (owner-ref field ref)
  "Append REF to the list in FIELD of the entity OWNER-REF points to.
FIELD holds a plain list or a connection; a connection's
`totalCount' grows by one.  Nothing changes if REF is already
there.  Return the new value of FIELD.  Signal
`approve-model-not-found' if the owner is not in the store.

Example, after a reply was created:
  (approve-model-append-ref thread-ref \\='comments
                            (approve-model-patch comment))"
  (approve-model--edit-list owner-ref field
                            (lambda (nodes)
                              (if (member ref nodes)
                                  nodes
                                (append nodes (list ref))))))

(defun approve-model-remove-ref (owner-ref field ref)
  "Remove REF from the list in FIELD of the entity OWNER-REF points to.
FIELD holds a plain list or a connection; a connection's
`totalCount' shrinks by the number of refs removed.  Return the new
value of FIELD.  Signal `approve-model-not-found' if the owner is
not in the store."
  (approve-model--edit-list owner-ref field
                            (lambda (nodes) (remove ref nodes))))

(defun approve-model-delete (ref)
  "Remove the entity REF points to from the store.
Refs to it elsewhere stay; remove them from their lists with
`approve-model-remove-ref'.  Return non-nil if it was stored."
  (unless (approve-model-ref-p ref)
    (signal 'approve-model-invalid-ref (list ref)))
  (approve-model--with-store
    (when-let* ((table (and approve-model--store
                            (gethash (approve-model-ref-type ref)
                                     approve-model--store)))
                ((gethash (approve-model-ref-id ref) table)))
      (remhash (approve-model-ref-id ref) table)
      t)))

;;; Session state

(defun approve-session-get (key)
  "Return the session value under KEY, such as :layout."
  (approve-model--with-store
    (plist-get approve-session key)))

(defun approve-session-put (key value)
  "Set the session value under KEY to VALUE and return VALUE.
Example: (approve-session-put :layout \\='split)."
  (approve-model--with-store
    (setq approve-session (plist-put approve-session key value))
    value))

(defun approve-session-add (key item)
  "Add ITEM to the set under KEY, such as :loading or :inflight.
Items are compared with `equal'; an item is kept once.
Example: (approve-session-add :inflight \\='(thread \"PRRT_1\"))."
  (let ((items (approve-session-get key)))
    (unless (member item items)
      (approve-session-put key (cons item items)))
    nil))

(defun approve-session-remove (key item)
  "Remove ITEM from the set under KEY."
  (approve-session-put key (remove item (approve-session-get key)))
  nil)

(defun approve-session-member-p (key item)
  "Return non-nil if ITEM is in the set under KEY."
  (and (member item (approve-session-get key)) t))

(defun approve-session-map-get (key map-key)
  "Return the value of MAP-KEY in the map under KEY, or nil.
Maps are :errors (section key → error) and :drafts (target → text).
Example: (approve-session-map-get :drafts \\='(reply \"PRRT_1\"))."
  (cdr (assoc map-key (approve-session-get key))))

(defun approve-session-map-put (key map-key value)
  "Set MAP-KEY to VALUE in the map under KEY and return VALUE."
  (approve-session-put key (cons (cons map-key value)
                                 (assoc-delete-all
                                  map-key (copy-sequence
                                           (approve-session-get key)))))
  value)

(defun approve-session-map-remove (key map-key)
  "Remove MAP-KEY from the map under KEY."
  (approve-session-put key (assoc-delete-all
                            map-key (copy-sequence (approve-session-get key))))
  nil)

;;; Selectors

(defun approve-model--root-field (field)
  "Return FIELD of the root entity as stored (refs unresolved), or nil."
  (when approve-model--root
    (alist-get field (approve-model--raw approve-model--root))))

(defun approve-model--resolve-nodes (nodes)
  "Return NODES, each resolved from the root's point of view."
  (let ((path (list approve-model--root)))
    (mapcar (lambda (node) (approve-model--resolve node path)) nodes)))

(defun approve-pr ()
  "Return the PullRequest, fully resolved, or nil before it is loaded.
This resolves the whole graph under the PR; to read one field, use
`approve-pr-field'."
  (approve-model--with-store
    (when approve-model--root
      (approve-model--resolve approve-model--root nil))))

(defun approve-pr-field (field)
  "Return FIELD of the PullRequest, resolved, or nil before it is loaded.
Example: (approve-pr-field \\='title) => \"Fix the frobnicator\"."
  (approve-model--with-store
    (when approve-model--root
      (approve-model--resolve (approve-model--root-field field)
                              (list approve-model--root)))))

(defun approve-pr-files ()
  "Return the PR's changed files, a list of alists.
Each has `path', `additions', `deletions', `changeType' and
`viewerViewedState'."
  (approve-model--with-store
    (approve-model--resolve-nodes
     (approve-model-nodes (approve-model--root-field 'files)))))

(defun approve-pr-threads ()
  "Return the PR's review threads, resolved, in store order."
  (approve-model--with-store
    (approve-model--resolve-nodes
     (approve-model-nodes (approve-model--root-field 'reviewThreads)))))

(defun approve-pr-threads-for-file (path)
  "Return the review threads whose `path' is PATH, resolved, in order.
Only the matching threads are resolved."
  (approve-model--with-store
    (approve-model--resolve-nodes
     (seq-filter (lambda (node)
                   (equal (alist-get 'path (approve-model--raw node)) path))
                 (approve-model-nodes
                  (approve-model--root-field 'reviewThreads))))))

(defun approve-pr-pending-review ()
  "Return the viewer's pending review, resolved, or nil.
It is the first review in the PR's `pendingReviews' whose `state' is
\"PENDING\"; once submitted, its state changes and it drops out."
  (approve-model--with-store
    (when-let* ((node (seq-find
                       (lambda (node)
                         (equal (alist-get 'state (approve-model--raw node))
                                "PENDING"))
                       (approve-model-nodes
                        (approve-model--root-field 'pendingReviews)))))
      (car (approve-model--resolve-nodes (list node))))))

(defun approve-model--viewer-can-field (what)
  "Return the field name `viewerCan<What>' for the symbol WHAT.
Example: `merge-as-admin' => `viewerCanMergeAsAdmin'."
  (intern (concat "viewerCan"
                  (mapconcat #'capitalize
                             (split-string (symbol-name what) "-" t) ""))))

(defun approve-pr-viewer-can (what)
  "Return non-nil if the viewer can do WHAT to the PR.
WHAT is a symbol naming a `viewerCan*' field of the PullRequest in
Lisp case: `update' reads `viewerCanUpdate', `merge-as-admin' reads
`viewerCanMergeAsAdmin'.  A field that was not loaded reads as nil.

Example: (approve-pr-viewer-can \\='update) => t"
  (approve-model--with-store
    (approve-model--root-field (approve-model--viewer-can-field what))))

(provide 'approve-model)

;;; approve-model.el ends here
