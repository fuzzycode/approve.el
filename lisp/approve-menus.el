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
;; "Transients"):
;;
;;   j  `approve-jump'           the sections of the PR, the next work
;;                               (unviewed files, unresolved threads)
;;                               and any changed file by name
;;   S  `approve-submit-review'  a verdict for the pending review, its
;;                               body, or discarding it (flow F09)
;;
;; Suffixes that do not apply are shown dimmed, so a menu never
;; changes shape.
;;
;; Layer rules: this is the entry points and menus layer.  It only
;; arranges commands defined below it.

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'approve-model)
(require 'approve-pr)
(require 'approve-nav)
(require 'approve-review)
(require 'approve-faces)

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

;;; Submit review (S)

(defun approve-menus--submit-heading ()
  "Return the heading of the submit menu: the PR and the pending review."
  (format "Submit review  #%s  %s" (approve-pr-field 'number)
          (propertize (if (approve-pr-pending-review)
                          (concat "pending: " (approve-review-summary))
                        (approve-review-summary))
                      'face 'approve-dim)))

(defun approve-menus--body-description ()
  "Return the body suffix's description, with the start of the body."
  (if-let* ((body (approve-review-body)))
      (format "Edit body…  %s"
              (propertize (format "\"%s\"" (truncate-string-to-width
                                             (car (split-string body "\n")) 40 nil nil "…"))
                          'face 'approve-dim))
    "Edit body…"))

(defun approve-menus--approve-inapt-p ()
  "Return non-nil if the viewer may not approve."
  (approve-review-why-not-verdict "APPROVE"))

(defun approve-menus--request-changes-inapt-p ()
  "Return non-nil if the viewer may not request changes."
  (approve-review-why-not-verdict "REQUEST_CHANGES"))

;;;###autoload (autoload 'approve-submit-review "approve-menus" nil t)
(transient-define-prefix approve-submit-review ()
  "Submit your review of the pull request, or discard it (flow F09).
A verdict asks for the review's body, starting from its draft; with
\\[universal-argument] before it, the review is submitted without a body.  With no pending
review, a verdict adds a review and submits it at once."
  [:description approve-menus--submit-heading
   ["Verdict"
    ("a" "Approve" approve-submit-approve :inapt-if approve-menus--approve-inapt-p)
    ("r" "Request changes" approve-submit-request-changes
     :inapt-if approve-menus--request-changes-inapt-p)
    ("c" "Comment" approve-submit-comment)]
   ["Body"
    ("e" approve-edit-review-body :description approve-menus--body-description)]
   ["Other"
    ("X" "Discard pending review" approve-discard-review
     :inapt-if approve-menus--no-pending-review-p)]])

(keymap-set approve-pr-mode-map "S" #'approve-submit-review)

(provide 'approve-menus)

;;; approve-menus.el ends here
