;;; approve-ui-headers.el --- Header sections for Approve UI  -*- lexical-binding: t; -*-

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

;; This module provides header section rendering for Approve.
;;
;; Header sections display key PR information at the top of the review buffer:
;; - Title and PR number
;; - State (open, closed, merged, draft)
;; - Author information
;; - Labels
;; - Assignees
;; - Reviewers and review status
;; - Milestone
;; - Branch information
;;
;; Each section is designed to be independently insertable via hooks,
;; allowing users to customize which headers appear and in what order.

;;; Code:

(require 'magit-section)
(require 'cl-lib)

(require 'approve-model)

;;; Customization

(defcustom approve-review-header-sections-hook
  '()
  "Hook run to insert header sections in the PR review buffer."
  :group 'approve
  :type 'hook)

(defun approve-insert-header-section ()
  "Insert the header section in the PR review buffer."
  (run-hooks 'approve-review-header-sections-hook))

(provide 'approve-ui-headers)
;;; approve-ui-headers.el ends here
