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
;;   ?  `approve-dispatch'       every command of a PR buffer, by what
;;                               it works on
;;   ?  `approve-dashboard-dispatch'  every command of the dashboard
;;   j  `approve-jump'           the sections of the PR, the next work
;;                               (unviewed files, unresolved threads)
;;                               and any changed file by name
;;   S  `approve-submit-review'  a verdict for the pending review, its
;;                               body, or discarding it (flow F09)
;;
;; The dispatch lists the commands that exist, no more: a new command
;; joins it, and any other menu it fits, in the change that adds it.
;; Every command bound in a PR buffer, or in the dashboard, is
;; reachable from its dispatch.
;;
;; Suffixes that do not apply are shown dimmed, so a menu never
;; changes shape.  A command that changes the PR says itself whether
;; it can run here: its preflight (`approve-action-why-not').
;;
;; Layer rules: this is the entry points and menus layer.  It only
;; arranges commands defined below it.

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'approve-model)
(require 'approve-pr)
(require 'approve-nav)
(require 'approve-actions)
(require 'approve-review)
(require 'approve-commands)
(require 'approve-eldoc)
(require 'approve-dashboard)
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
(put 'approve-submit-review 'approve-preflight #'approve-action-context)

;;; Dispatch (?)

(defun approve-menus--dispatch-heading ()
  "Return the heading of the dispatch menu: the number and title of the PR."
  (concat (propertize "Approve" 'face 'transient-heading) "  "
          (propertize (string-trim-right
                       (format "#%s %s" (approve-model-meta :number)
                               (truncate-string-to-width
                                (or (and (approve-model-root-ref) (approve-pr-field 'title)) "")
                                60 nil nil "…")))
                      'face 'approve-title)))

(defun approve-menus--threads-description ()
  "Return the description of `approve-toggle-threads': what it will do."
  (if (eq (approve-session-get :threads) 'folded)
      "Unfold all threads"
    "Fold all threads"))

;;;###autoload (autoload 'approve-dispatch "approve-menus" nil t)
(transient-define-prefix approve-dispatch ()
  "Show the commands of the pull request buffer (decision D06).
Those that can't run at point, or that you may not use, are dimmed."
  [:description approve-menus--dispatch-heading
   ["Review"
    ("c" "Comment at point" approve-comment
     :inapt-if (lambda () (approve-action-why-not 'approve-comment)))
    ("s" "Suggest change" approve-suggest
     :inapt-if (lambda () (approve-action-why-not 'approve-suggest)))
    ("v" "Toggle viewed" approve-toggle-viewed
     :inapt-if (lambda () (approve-action-why-not 'approve-toggle-viewed)))
    ("S" "Submit review…" approve-submit-review
     :inapt-if (lambda () (approve-action-why-not 'approve-submit-review)))
    ("X" "Discard pending review" approve-discard-review
     :inapt-if (lambda () (approve-action-why-not 'approve-discard-review)))]
   ["Comments"
    ("r" "Reply" approve-reply
     :inapt-if (lambda () (approve-action-why-not 'approve-reply)))
    ("R" "Resolve / unresolve" approve-resolve
     :inapt-if (lambda () (approve-action-why-not 'approve-resolve)))
    ("e" "Edit at point" approve-edit
     :inapt-if (lambda () (approve-action-why-not 'approve-edit)))
    ("k" "Delete at point" approve-delete
     :inapt-if (lambda () (approve-action-why-not 'approve-delete)))
    ("C" "Conversation comment" approve-comment-on-pr
     :inapt-if (lambda () (approve-action-why-not 'approve-comment-on-pr)))]
   ["Buffer"
    ("g" "Refresh" approve-pr-refresh)
    ("RET" "Visit at point" approve-visit)
    ("o" "Open in browser" approve-browse)
    ("y" "Copy URL / sha" approve-copy)
    ("j" "Jump…" approve-jump)
    ("K" "Details (eldoc)" approve-show-details)
    ("T" approve-toggle-threads :description approve-menus--threads-description)]])

(keymap-set approve-pr-mode-map "?" #'approve-dispatch)

;;; Dashboard dispatch (?)

;;;###autoload (autoload 'approve-dashboard-dispatch "approve-menus" nil t)
(transient-define-prefix approve-dashboard-dispatch ()
  "Show the commands of the dashboard (decision D06).
Those that can't run at point are dimmed."
  [:description (lambda () (concat (propertize "Approve" 'face 'transient-heading) "  "
                                   (propertize "Dashboard" 'face 'approve-title)))
   ["Pull request"
    ("RET" "Open" approve-visit)
    ("o" "Open in browser" approve-browse)
    ("y" "Copy URL / search" approve-copy)
    ("K" "Details (eldoc)" approve-show-details)]
   ["Lists"
    ("g" "Refresh all" approve-dashboard-refresh)
    ("G" "Refresh list at point" approve-dashboard-refresh-section
     :inapt-if (lambda () (approve-action-why-not 'approve-dashboard-refresh-section)))]])

(keymap-set approve-dashboard-mode-map "?" #'approve-dashboard-dispatch)

(provide 'approve-menus)

;;; approve-menus.el ends here
