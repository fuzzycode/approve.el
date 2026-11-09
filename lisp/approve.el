;;; approve.el --- Review GitHub pull requests -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; Maintainer: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/fuzzycode/approve.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eldoc "1.14") (transient "0.7") (magit-section "4.0") (magit "4.0") (markdown-mode "2.5") (ghub "5.0") (dash "2.19") (nerd-icons "0.1"))
;; Keywords: tools, vc

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

;; Approve reviews GitHub pull requests from Emacs: find them, read them,
;; comment on code and submit reviews.  All durable state lives on GitHub.
;;
;; This file is the entry point: the `approve' customization group, the
;; autoloaded commands, and parsing of pull request URLs.  It sits in the
;; top layer (docs/architecture.org, "Layers") and may require any other
;; module; no other module may require it.

;;; Code:

(require 'seq)
(require 'thingatpt)

(defgroup approve nil
  "Review GitHub pull requests."
  :group 'tools
  :prefix "approve-"
  :link '(url-link :tag "Homepage" "https://github.com/fuzzycode/approve.el"))

(require 'approve-pr)
(require 'approve-sections)
(require 'approve-conversation)
(require 'approve-diff)
(require 'approve-context)
(require 'approve-nav)
(require 'approve-menus)
(require 'approve-eldoc)

(declare-function magit-add-section-hook "magit-section")

;;;###autoload
(defalias 'approve-add-section-hook #'magit-add-section-hook
  "Add a section inserter to an Approve section hook.
Takes the same arguments as `magit-add-section-hook'; use it to add,
remove or reorder the sections of Approve buffers.")

;;; Pull request URLs

(defun approve--default-url ()
  "Return a pull request URL at point or in the kill ring, or nil."
  (seq-find #'approve-parse-url
            (delq nil (list (thing-at-point 'url t)
                            (ignore-errors (current-kill 0 t))))))

;;;###autoload
(defun approve (url)
  "Open the GitHub pull request at URL for review (flow F01).
Interactively, the prompt offers a pull request URL at point or in
the kill ring.  An open buffer of the pull request is shown as it
is; \\[approve-pr-refresh] there loads it again.

Example: (approve \"https://github.com/magit/magit/pull/5000\")"
  (interactive
   (let ((default (approve--default-url)))
     (list (read-string (format-prompt "Pull request URL" default)
                        nil nil default))))
  (approve-pr-open (or (approve-parse-url url)
                       (user-error "Not a pull request URL: %s" url))))

(provide 'approve)

;;; approve.el ends here
