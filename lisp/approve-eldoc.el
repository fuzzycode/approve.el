;;; approve-eldoc.el --- Eldoc support for Approve -*- lexical-binding: t; -*-

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

;; This module provides eldoc-based hover documentation for Approve buffers.
;;
;; When the cursor is on an item that has extended information (like a
;; username, label, etc.), the additional details are shown via eldoc.
;;
;; To add hover documentation to any text, use `approve-eldoc-propertize':
;;
;;   (insert (approve-eldoc-propertize "@octocat" "octocat" "The Octocat - octocat@github.com"))
;;
;; The system uses text properties to store documentation:
;; - `approve-eldoc-thing': Short identifier (shown before the doc)
;; - `approve-eldoc-doc': The documentation string
;; - `approve-eldoc-face': Optional face for the thing

;;; Code:

(require 'eldoc)

;;; Text Property Names

(defconst approve-eldoc--thing-property 'approve-eldoc-thing
  "Text property for the short identifier/name of the documented item.")

(defconst approve-eldoc--doc-property 'approve-eldoc-doc
  "Text property for the documentation string.")

(defconst approve-eldoc--face-property 'approve-eldoc-face
  "Text property for the optional face to use for the thing.")

;;; Public API

(defun approve-eldoc-propertize (text thing doc &optional face)
  "Add hover documentation properties to TEXT.
THING is a short identifier for what TEXT represents (e.g., username).
DOC is the documentation string to display on hover.
FACE is an optional face to use when displaying THING in eldoc.

Returns TEXT with the appropriate text properties set.
If DOC is nil or empty, returns TEXT unchanged (no hover docs)."
  (if (and doc (not (string-empty-p doc)))
      (propertize text
                  approve-eldoc--thing-property thing
                  approve-eldoc--doc-property doc
                  approve-eldoc--face-property face)
    text))

(defun approve-eldoc-format-user (_login &optional name email)
  "Format hover documentation for a user.
_LOGIN is the GitHub username (reserved for future use).
NAME is the user's display name (may be nil).
EMAIL is the user's email (may be nil).

Returns a formatted documentation string, or nil if neither NAME nor EMAIL."
  (let ((parts nil))
    (when (and name (not (string-empty-p name)))
      (push name parts))
    (when (and email (not (string-empty-p email)))
      (push (format "<%s>" email) parts))
    (when parts
      (string-join (nreverse parts) " "))))

;;; Eldoc Backend

(defun approve-eldoc--documentation-function (callback &rest _ignored)
  "Eldoc documentation function for Approve buffers.
CALLBACK is called with the documentation if available."
  (let ((doc (get-text-property (point) approve-eldoc--doc-property))
        (thing (get-text-property (point) approve-eldoc--thing-property))
        (face (get-text-property (point) approve-eldoc--face-property)))
    (when doc
      (funcall callback doc
               :thing thing
               :face (or face 'font-lock-variable-name-face)))))

;;; Mode Setup

(defun approve-eldoc-setup ()
  "Set up eldoc support for the current Approve buffer."
  (add-hook 'eldoc-documentation-functions
            #'approve-eldoc--documentation-function nil t)
  (eldoc-mode 1))

(provide 'approve-eldoc)
;;; approve-eldoc.el ends here
