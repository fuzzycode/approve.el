;;; approve-menus.el --- Transient menus of Approve -*- lexical-binding: t; -*-

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

;; The transients of Approve (decision D06; docs/ui.org,
;; "Transients").  For now the jump menu, `approve-jump' (j): the
;; sections of the PR, the next work (unviewed files, unresolved
;; threads) and any changed file by name.  Suffixes that do not apply
;; are shown dimmed, so the menu never changes shape.
;;
;; Layer rules: this is the entry points and menus layer.  It only
;; arranges commands defined below it.

;;; Code:

(require 'transient)
(require 'approve-model)
(require 'approve-pr)
(require 'approve-nav)

(defun approve-menus--no-pending-review-p ()
  "Return non-nil if the viewer has no pending review."
  (not (approve-pr-pending-review)))

;;;###autoload (autoload 'approve-jump "approve-menus" nil t)
(transient-define-prefix approve-jump ()
  "Jump to a part of the pull request, or to the next thing to review."
  [["Sections"
    ("d" "Description" approve-jump-description)
    ("c" "Conversation" approve-jump-conversation)
    ("k" "Checks" approve-jump-checks)
    ("m" "Commits" approve-jump-commits)
    ("f" "Files" approve-jump-files)
    ("p" "Pending review" approve-jump-pending-review
     :inapt-if approve-menus--no-pending-review-p)]
   ["Next to review"
    ("u" "Next unviewed file" approve-next-unviewed-file)
    ("U" "Previous unviewed file" approve-previous-unviewed-file)
    ("t" "Next unresolved thread" approve-next-thread)
    ("T" "Previous unresolved thread" approve-previous-thread)]
   ["Find"
    ("/" "File…" approve-jump-file)]])

(keymap-set approve-pr-mode-map "j" #'approve-jump)

(provide 'approve-menus)

;;; approve-menus.el ends here
