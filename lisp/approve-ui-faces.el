;;; approve-ui-faces.el --- Faces for Approve -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (c) 2025 Björn Larsson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This file contains all face definitions for Approve and any related
;; utility functions.

;;; Code:

(defgroup approve-faces nil
  "Faces used by Approve."
  :group 'approve
  :group 'faces)

;;; Section Faces

(defface approve-section-heading-face
  '((t :inherit magit-section-heading))
  "Face for top-level section headings."
  :group 'approve-faces)

;;; Header Faces

(defface approve-title-face
  '((t :inherit approve-section-heading-face :height 1.3))
  "Face for the PR title."
  :group 'approve-faces)

(defface approve-pr-number-face
  '((t :inherit font-lock-comment-face))
  "Face for the PR number."
  :group 'approve-faces)

(defface approve-header-title-face
  '((t :inherit magit-section-secondary-heading))
  "Face for header titles like \"Title:\", \"Author:\", etc."
  :group 'approve-faces)

;;; Author Faces

(defface approve-author-face
  '((t :inherit font-lock-keyword-face))
  "Face for the PR author username."
  :group 'approve-faces)

;;; Utility Functions

(defun approve-ui--propertize-face (string face)
  "Propertize STRING with FACE using both `face' and `font-lock-face'.
This ensures the face is applied correctly in buffers where
`font-lock-mode' is active."
  (propertize string 'face face 'font-lock-face face))

(provide 'approve-ui-faces)
;;; approve-ui-faces.el ends here
