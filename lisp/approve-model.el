;;; approve-model.el --- Data model for Approve  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This module provides the data model layer for Approve.
;;
;; Key features:
;; - Buffer-local normalized data store
;; - Automatic normalization using GraphQL `__typename` and `id` fields
;; - Easy field access via `approve-model-get' and `with-approve-entity'
;; - Seamless patching when mutations return updated data
;;
;; The data model uses a normalized store where entities are indexed by
;; their type and ID.  This allows efficient updates when mutations return
;; partial data, and prevents data duplication.
;;
;; Example structure:
;;   {
;;     "PullRequest" => { "PR_123" => { ... } }
;;     "User" => { "U_456" => { ... }, "U_789" => { ... } }
;;     "IssueComment" => { "IC_101" => { ... } }
;;   }
;;
;; References between entities are stored as refs: (:ref TYPE ID)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Custom Variables

(defgroup approve-model nil
  "Data model settings for Approve."
  :group 'approve
  :prefix "approve-model-")

;;; Error Handling

(define-error 'approve-model-error "Approve model error")
(define-error 'approve-model-not-found "Entity not found" 'approve-model-error)
(define-error 'approve-model-invalid-ref "Invalid entity reference" 'approve-model-error)

;;; Buffer-Local Store

(defvar-local approve-model--store nil
  "Buffer-local normalized data store.
A hash table mapping typename to another hash table of id -> entity.")

(defvar-local approve-model--root nil
  "Buffer-local root entity reference.
A plist with :type and :id pointing to the main entity (e.g., the PR).")

(defvar-local approve-model--metadata nil
  "Buffer-local metadata for the store.
Contains information like owner, repo, PR number.")

;;; Reference Type

(defconst approve-model--ref-marker :approve-ref
  "Marker symbol to identify entity references.")

(defun approve-model-ref-p (obj)
  "Return non-nil if OBJ is an entity reference."
  (and (listp obj)
       (eq (car obj) approve-model--ref-marker)))

(defun approve-model-make-ref (typename id)
  "Create a reference to an entity with TYPENAME and ID."
  (list approve-model--ref-marker typename id))

(defun approve-model-ref-type (ref)
  "Return the typename from REF."
  (nth 1 ref))

(defun approve-model-ref-id (ref)
  "Return the id from REF."
  (nth 2 ref))

;;; Store Initialization

(defun approve-model-init (&optional metadata)
  "Initialize the buffer-local data store with optional METADATA.
METADATA is a plist that can contain :owner, :repo, :number, etc."
  (setq approve-model--store (make-hash-table :test 'equal))
  (setq approve-model--root nil)
  (setq approve-model--metadata metadata))

(defun approve-model-clear ()
  "Clear the buffer-local data store."
  (when approve-model--store
    (clrhash approve-model--store))
  (setq approve-model--root nil))

(defun approve-model-initialized-p ()
  "Return non-nil if the store has been initialized."
  (not (null approve-model--store)))

;;; Internal Normalization

(defun approve-model--get-type-store (typename)
  "Get or create the hash table for TYPENAME in the store."
  (or (gethash typename approve-model--store)
      (let ((type-store (make-hash-table :test 'equal)))
        (puthash typename type-store approve-model--store)
        type-store)))

(defun approve-model--extract-typename (data)
  "Extract __typename from DATA, return nil if not present."
  (alist-get '__typename data))

(defun approve-model--extract-id (data)
  "Extract id from DATA, return nil if not present."
  (alist-get 'id data))

(defun approve-model--normalizable-p (data)
  "Return non-nil if DATA can be normalized (has __typename and id)."
  (and (listp data)
       (approve-model--extract-typename data)
       (approve-model--extract-id data)))

(defun approve-model--normalize-value (value)
  "Normalize VALUE, returning a ref if it's an entity, or the processed value."
  (cond
   ;; Normalizable entity - store it and return a ref
   ((approve-model--normalizable-p value)
    (approve-model--normalize-entity value))

   ;; List/vector of items - process each
   ((and (listp value) (not (approve-model--alist-p value)))
    (mapcar #'approve-model--normalize-value value))

   ((vectorp value)
    (vconcat (mapcar #'approve-model--normalize-value value)))

   ;; Nested alist (not an entity) - process values
   ((approve-model--alist-p value)
    (approve-model--normalize-alist value))

   ;; Primitive value - return as-is
   (t value)))

(defun approve-model--alist-p (obj)
  "Return non-nil if OBJ is an alist (list of cons cells with symbol keys).
Entity references and lists of references are not considered alists.
An alist has dotted pairs like ((key . value) ...), not lists like ((:key ...))"
  (and (listp obj)
       (not (null obj))
       (not (approve-model-ref-p obj))
       (cl-every (lambda (item)
                   (and (consp item)
                        (symbolp (car item))
                        ;; Must be a dotted pair, not a list starting with symbol
                        ;; A dotted pair (a . b) has (cdr item) as atom or cons
                        ;; A list (:key x y) has (cdr item) as a list
                        ;; We check: not a reference by testing if it looks like (:symbol ...)
                        (not (and (keywordp (car item))
                                  (listp (cdr item))
                                  (not (null (cdr item)))))))
                 obj)))

(defun approve-model--normalize-alist (alist)
  "Normalize all values in ALIST, returning a new alist."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (approve-model--normalize-value (cdr pair))))
          alist))

(defun approve-model--normalize-entity (data)
  "Normalize entity DATA and store it.  Return a reference to the entity."
  (let* ((typename (approve-model--extract-typename data))
         (id (approve-model--extract-id data))
         (type-store (approve-model--get-type-store typename))
         ;; Normalize nested values
         (normalized-data (approve-model--normalize-alist data)))
    ;; Store the normalized entity
    (puthash id normalized-data type-store)
    ;; Return a reference
    (approve-model-make-ref typename id)))

(defun approve-model--unwrap-nodes (data)
  "Recursively unwrap GraphQL connection patterns in DATA.
Converts { nodes: [...] } to just [...] and { totalCount: N, nodes: [...] }
to the unwrapped list while preserving totalCount in metadata if needed.
Vectors are converted to lists for consistency."
  (cond
   ;; Handle connection objects with 'nodes' key
   ((and (approve-model--alist-p data)
         (assq 'nodes data))
    (let ((nodes (alist-get 'nodes data)))
      (if (or (listp nodes) (vectorp nodes))
          (approve-model--unwrap-nodes nodes)
        nodes)))

   ;; List - process each item
   ((and (listp data) (not (approve-model--alist-p data)))
    (mapcar #'approve-model--unwrap-nodes data))

   ;; Vector - convert to list and process each item
   ((vectorp data)
    (mapcar #'approve-model--unwrap-nodes (append data nil)))

   ;; Alist - process all values recursively
   ((approve-model--alist-p data)
    (mapcar (lambda (pair)
              (cons (car pair)
                    (approve-model--unwrap-nodes (cdr pair))))
            data))

   ;; Primitive
   (t data)))

;;; Public API - Store Operations

(defun approve-model-load (data &optional set-root)
  "Load DATA into the normalized store.
If SET-ROOT is non-nil and DATA is normalizable, set it as the root entity.
DATA is typically the response from a GraphQL query.
Returns a reference to the top-level entity if normalizable, else the data."
  (unless approve-model--store
    (approve-model-init))
  ;; First unwrap GraphQL connection patterns
  (let* ((unwrapped (approve-model--unwrap-nodes data))
         (result (approve-model--normalize-value unwrapped)))
    (when (and set-root (approve-model-ref-p result))
      (setq approve-model--root result))
    result))

(defun approve-model-patch (data)
  "Patch the store with DATA from a mutation response.
Entities in DATA will be merged with existing entities.
New entities will be added.  Returns the reference to the patched entity."
  (approve-model-load data))

(defun approve-model-get (typename id &optional field)
  "Get an entity by TYPENAME and ID, optionally extracting FIELD.
If FIELD is provided, return just that field's value.
If FIELD is a list, return an alist of those fields.
Automatically resolves references in the returned data."
  (unless approve-model--store
    (signal 'approve-model-not-found (list typename id)))
  (let* ((type-store (gethash typename approve-model--store))
         (entity (when type-store (gethash id type-store))))
    (unless entity
      (signal 'approve-model-not-found (list typename id)))
    (cond
     ;; Single field
     ((and field (symbolp field))
      (approve-model--resolve (alist-get field entity)))
     ;; Multiple fields
     ((and field (listp field))
      (mapcar (lambda (f)
                (cons f (approve-model--resolve (alist-get f entity))))
              field))
     ;; Whole entity
     (t (approve-model--resolve-entity entity)))))

(defun approve-model-get-ref (ref &optional field)
  "Get an entity by REF, optionally extracting FIELD.
REF must be a valid entity reference created by `approve-model-make-ref'."
  (unless (approve-model-ref-p ref)
    (signal 'approve-model-invalid-ref (list ref)))
  (approve-model-get (approve-model-ref-type ref)
                     (approve-model-ref-id ref)
                     field))

(defun approve-model-root (&optional field)
  "Get the root entity, optionally extracting FIELD."
  (unless approve-model--root
    (signal 'approve-model-not-found '("No root entity set")))
  (approve-model-get-ref approve-model--root field))

(defun approve-model-metadata (&optional key)
  "Get store metadata, or specific KEY from metadata."
  (if key
      (plist-get approve-model--metadata key)
    approve-model--metadata))

(defun approve-model-set-metadata (key value)
  "Set metadata KEY to VALUE."
  (setq approve-model--metadata
        (plist-put approve-model--metadata key value)))

;;; Reference Resolution

(defun approve-model--resolve (value)
  "Resolve VALUE, following references to get actual data."
  (cond
   ;; Reference - look it up
   ((approve-model-ref-p value)
    (condition-case nil
        (approve-model-get-ref value)
      (approve-model-not-found value)))  ; Return the ref if not found

   ;; List of items - resolve each
   ((and (listp value) (not (approve-model--alist-p value)))
    (mapcar #'approve-model--resolve value))

   ;; Vector - convert to list and resolve each
   ((vectorp value)
    (mapcar #'approve-model--resolve (append value nil)))

   ;; Alist - resolve values
   ((approve-model--alist-p value)
    (mapcar (lambda (pair)
              (cons (car pair)
                    (approve-model--resolve (cdr pair))))
            value))

   ;; Primitive
   (t value)))

(defun approve-model--resolve-entity (entity)
  "Resolve all references within ENTITY."
  (approve-model--resolve entity))

;;; Query Helpers

(defun approve-model-entities (typename)
  "Get all entities of TYPENAME as a list."
  (when-let ((type-store (gethash typename approve-model--store)))
    (let (entities)
      (maphash (lambda (_id entity)
                 (push (approve-model--resolve-entity entity) entities))
               type-store)
      entities)))

(defun approve-model-entity-ids (typename)
  "Get all IDs for entities of TYPENAME."
  (when-let ((type-store (gethash typename approve-model--store)))
    (hash-table-keys type-store)))

(defun approve-model-has-entity-p (typename id)
  "Return non-nil if an entity with TYPENAME and ID exists."
  (when-let ((type-store (gethash typename approve-model--store)))
    (gethash id type-store)))

;;; Convenience Macros

(defun approve-model--field-to-var (field)
  "Convert FIELD symbol to a variable name.
Handles camelCase to lisp-case conversion."
  (let* ((case-fold-search nil)  ; Make regexp matching case-sensitive
         (name (symbol-name field))
         ;; Insert hyphen before each uppercase letter that follows lowercase
         (converted (replace-regexp-in-string
                     "\\([a-z]\\)\\([A-Z]\\)"
                     "\\1-\\2"
                     name)))
    (intern (downcase converted))))

(defmacro with-approve-entity (binding &rest body)
  "Execute BODY with entity fields bound.

BINDING is (ENTITY-SPEC FIELDS) where:
  ENTITY-SPEC is one of:
    - (:root) - use root entity
    - (:ref REF) - use a reference
    - (TYPENAME ID) - use typename and id

  FIELDS is a list of field names to bind as local variables.
  Field names are converted to lisp-style: bodyHTML -> body-html

Example:
  (with-approve-entity ((:root) (title body state author))
    (message \"PR: %s [%s]\" title state))"
  (declare (indent 1) (debug ((sexp sexp) body)))
  (let* ((entity-spec (car binding))
         (fields (cadr binding))
         (entity-var (gensym "entity")))
    `(let* ((,entity-var
             ,(cond
               ((eq (car entity-spec) :root)
                '(approve-model-root))
               ((eq (car entity-spec) :ref)
                `(approve-model-get-ref ,(cadr entity-spec)))
               (t
                `(approve-model-get ',(car entity-spec) ,(cadr entity-spec)))))
            ,@(mapcar
               (lambda (field)
                 (let ((var-name (approve-model--field-to-var field)))
                   `(,var-name (alist-get ',field ,entity-var))))
               fields))
       ,@body)))

(defmacro approve-model-let (bindings &rest body)
  "Execute BODY with entity field BINDINGS.

BINDINGS is a list of (VAR ENTITY-SPEC FIELD) forms where:
  VAR is the variable to bind
  ENTITY-SPEC is (:root), (:ref REF), or (TYPENAME ID)
  FIELD is the field to extract

Example:
  (approve-model-let ((title (:root) title)
                      (author-login (:root) author))
    (message \"%s by %s\" title (alist-get \\='login author-login)))"
  (declare (indent 1) (debug ((&rest (sexp sexp sexp)) body)))
  `(let ,(mapcar
          (lambda (binding)
            (let ((var (nth 0 binding))
                  (entity-spec (nth 1 binding))
                  (field (nth 2 binding)))
              `(,var
                ,(cond
                  ((eq (car entity-spec) :root)
                   `(approve-model-root ',field))
                  ((eq (car entity-spec) :ref)
                   `(approve-model-get-ref ,(cadr entity-spec) ',field))
                  (t
                   `(approve-model-get ',(car entity-spec) ,(cadr entity-spec) ',field))))))
          bindings)
     ,@body))

;;; Update Operations

(defun approve-model-update (typename id field value)
  "Update FIELD to VALUE for entity with TYPENAME and ID.
Returns the updated entity."
  (let* ((type-store (approve-model--get-type-store typename))
         (entity (gethash id type-store)))
    (unless entity
      (signal 'approve-model-not-found (list typename id)))
    (let ((updated (cons (cons field value)
                         (assq-delete-all field entity))))
      (puthash id updated type-store)
      updated)))

(defun approve-model-update-root (field value)
  "Update FIELD to VALUE for the root entity."
  (unless approve-model--root
    (signal 'approve-model-not-found '("No root entity set")))
  (approve-model-update (approve-model-ref-type approve-model--root)
                        (approve-model-ref-id approve-model--root)
                        field value))

(defun approve-model-delete (typename id)
  "Delete entity with TYPENAME and ID from the store."
  (when-let ((type-store (gethash typename approve-model--store)))
    (remhash id type-store)))

;;; Debug/Development Helpers

(defun approve-model-debug-dump ()
  "Return a string representation of the current store for debugging."
  (if (not approve-model--store)
      "Store not initialized"
    (let ((output (list "=== Approve Model Store ===")))
      (push (format "Root: %S" approve-model--root) output)
      (push (format "Metadata: %S" approve-model--metadata) output)
      (push "--- Entities by Type ---" output)
      (maphash
       (lambda (typename type-store)
         (push (format "\n[%s] (%d entities)"
                       typename (hash-table-count type-store))
               output)
         (maphash
          (lambda (id _entity)
            (push (format "  - %s" id) output))
          type-store))
       approve-model--store)
      (string-join (nreverse output) "\n"))))

(defun approve-model-stats ()
  "Return statistics about the current store."
  (if (not approve-model--store)
      '(:initialized nil)
    (let ((type-counts nil)
          (total 0))
      (maphash
       (lambda (typename type-store)
         (let ((count (hash-table-count type-store)))
           (push (cons typename count) type-counts)
           (cl-incf total count)))
       approve-model--store)
      (list :initialized t
            :total-entities total
            :types-count (hash-table-count approve-model--store)
            :by-type type-counts
            :has-root (not (null approve-model--root))))))

(provide 'approve-model)
;;; approve-model.el ends here
