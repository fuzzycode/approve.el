;;; approve-log.el --- Logging utilities for Approve -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson
;;
;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/bjornlarsson/approve
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is part of Approve.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Debug logging for the Approve package.  Enable by setting
;; `approve-debug' to non-nil.  Messages are written to *Messages*
;; with an [approve] prefix.

;;; Code:

(defgroup approve nil
  "Review GitHub pull requests in Emacs."
  :group 'tools
  :prefix "approve-")

(defcustom approve-debug nil
  "When non-nil, log debug messages to *Messages*."
  :type 'boolean
  :group 'approve)

(defun approve-log--message (format-string &rest args)
  "Log a debug message when `approve-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message' with an [approve] prefix."
  (when approve-debug
    (apply #'message (concat "[approve] " format-string) args)))

(provide 'approve-log)

;;; approve-log.el ends here
