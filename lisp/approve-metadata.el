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
(require 'approve-model)
(require 'approve-queries)

(defcustom approve-metadata-user-pages 10
  "How many pages of 100 mentionable users are fetched, at most.
The repositories of large organizations can have thousands of
people who can be mentioned; the first pages complete most
mentions.  To fetch more:

  (setq approve-metadata-user-pages 30)"
  :type 'natnum
  :group 'approve)

(defconst approve-metadata-kinds '(mentionable-users refs emoji)
  "The kinds of metadata `approve-metadata-fetch' knows.")

(defvar approve-metadata--cache (make-hash-table :test #'equal)
  "Metadata fetched this session, by key (`approve-metadata--key').")

(defvar approve-metadata--waiting (make-hash-table :test #'equal)
  "Fetches on their way, by key: the callbacks waiting for them.")

(defun approve-metadata-clear-cache ()
  "Forget the metadata fetched this session.
Fetches on their way still finish and keep what they get."
  (clrhash approve-metadata--cache))

(defun approve-metadata--host ()
  "Return the API host of the current buffer's repository."
  (or (approve-query-host) approve-api-host "api.github.com"))

(defun approve-metadata--key (kind)
  "Return the cache key of KIND for the current buffer's repository."
  (if (eq kind 'emoji)
      (list (approve-metadata--host) kind)
    (list (approve-metadata--host) (approve-model-meta :owner)
          (approve-model-meta :repo) kind)))

(defun approve-metadata (kind)
  "Return the metadata KIND of the current buffer's repository, or nil.
It is nil too until it has been fetched: see `approve-metadata-fetch'.
KIND is one of `approve-metadata-kinds'.

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

(defun approve-metadata--request (kind callback errorback)
  "Fetch KIND for the current buffer's repository.
CALLBACK is called with the metadata, ERRORBACK with the error."
  (let ((buffer (approve-metadata--buffer))
        (host (approve-query-host))
        (owner (approve-model-meta :owner))
        (repo (approve-model-meta :repo)))
    (pcase kind
      ('mentionable-users
       (let ((users nil))
         (approve-api-graphql-all
          "queries/metadata/mentionable-users.graphql"
          `((owner . ,owner) (name . ,repo))
          :path '(repository mentionableUsers)
          :max-pages approve-metadata-user-pages
          :buffer buffer :host host :progress nil
          :on-page (lambda (data _page)
                     (setq users (append users
                                         (let-alist data .repository.mentionableUsers.nodes))))
          :callback (lambda () (funcall callback users))
          :errorback errorback)))
      ('refs
       (approve-api-graphql
        "queries/metadata/recent-refs.graphql"
        `((search . ,(format "repo:%s/%s sort:updated-desc" owner repo)))
        :buffer buffer :host host :progress nil
        :callback (lambda (data) (funcall callback (let-alist data .search.nodes)))
        :errorback errorback))
      ('emoji
       (approve-api-rest
        "GET" "/emojis"
        :buffer buffer :host host :progress nil
        :callback (lambda (urls) (funcall callback (approve-metadata--emoji urls)))
        :errorback errorback))
      (_ (error "Unknown metadata: %s" kind)))))

(cl-defun approve-metadata-fetch (kind &key callback)
  "Fetch the metadata KIND of the current buffer's repository, once.
Nothing is sent when it was fetched before or is on its way.
CALLBACK, if non-nil, is called with the metadata once it is there,
at once when it was fetched before; it is not called when the fetch
fails.  KIND is one of `approve-metadata-kinds'.

Example:
  (approve-metadata-fetch
   \\='emoji :callback (lambda (emoji) (message \"%d\" (length emoji))))"
  (let* ((key (approve-metadata--key kind))
         (none (make-symbol "none"))
         (value (gethash key approve-metadata--cache none)))
    (cond
     ((not (eq value none))
      (when callback
        (funcall callback value)))
     ((not (eq (gethash key approve-metadata--waiting none) none))
      (when callback
        (push callback (gethash key approve-metadata--waiting))))
     (t
      (puthash key (and callback (list callback)) approve-metadata--waiting)
      (approve-metadata--request
       kind
       (lambda (value)
         (let ((callbacks (reverse (gethash key approve-metadata--waiting))))
           (remhash key approve-metadata--waiting)
           (puthash key value approve-metadata--cache)
           (dolist (callback callbacks)
             (funcall callback value))))
       (lambda (_err)
         (remhash key approve-metadata--waiting)))))
    nil))

(provide 'approve-metadata)

;;; approve-metadata.el ends here
