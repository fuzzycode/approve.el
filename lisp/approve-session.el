;;; approve-session.el --- UI session state of a PR buffer -*- lexical-binding: t; -*-

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

;; `approve-session': the UI state of a PR buffer that is never sent
;; anywhere (docs/architecture.org, "Data model"): which sections are
;; still loading, which failed, which targets have a mutation in
;; flight, unsent drafts, and the layout.
;;
;;   (approve-session-add :loading 'files)
;;   (approve-session-map-put :drafts '(reply "PRRT_1") text)
;;
;; The session lives next to the store, in the buffer that owns it
;; (`approve-model-owner-buffer'), so the split layout's diff buffer
;; shares its PR buffer's session.  `approve-model-init' resets it.
;;
;; Layer rules: this is the model layer.  It never calls the network
;; and never touches the text of a buffer; it only holds buffer-local
;; data.  It requires nothing from Approve but `approve-model'.

;;; Code:

(require 'approve-model)

(defvar-local approve-session nil
  "Plist of UI state that is never sent anywhere.
:loading   list of section keys still loading (a set)
:errors    alist of section key → error
:inflight  list of target idents with a mutation in flight (a set)
:drafts    alist of target → unsent text
:layout    `single' or `split'
:threads   `folded' or `shown' once every thread of the diff was
           folded or unfolded at once, else nil
Read and change it with `approve-session-get' and friends; they use
the session of `approve-model-owner-buffer'.")

(defun approve-session-get (key)
  "Return the session value under KEY, such as :layout."
  (with-current-buffer (approve-model-owner-buffer)
    (plist-get approve-session key)))

(defun approve-session-put (key value)
  "Set the session value under KEY to VALUE and return VALUE.
Example: (approve-session-put :layout \\='split)."
  (with-current-buffer (approve-model-owner-buffer)
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

(provide 'approve-session)

;;; approve-session.el ends here
