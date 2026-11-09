;;; approve-context.el --- What is at point in an Approve buffer -*- lexical-binding: t; -*-

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

;; `approve-context-at-point' says what point is on, as one plist that
;; every DWIM command and eldoc read (docs/architecture.org, "Context
;; at point"):
;;
;;   (approve-context-at-point)
;;   => (:kind line :path "lisp/a.el" :side "RIGHT" :line 42 :old 41 :new 42
;;       :line-kind add :url "https://github.com/…/files#diff-…R42")
;;
;; It looks, in order, at the region (diff lines → `lines'), the thing
;; under point (`approve-thing': a user, label, commit, issue link…),
;; the diff line (`approve-line'), and the section point is in, walking
;; up to the first section that means something.  Anything else is the
;; pull request itself (`pr').  Every context carries :url when there
;; is one, for opening and copying.
;;
;; Layer rules: this is the model layer.  It reads the text properties
;; and sections of the current buffer and the store; it never changes
;; them, inserts text or calls the network.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-model)

(defun approve-context--entity (typename id fields)
  "Return FIELDS of the entity TYPENAME ID as an alist, or nil if absent."
  (when (and id (approve-model-has-entity-p typename id))
    (approve-model-get typename id fields)))

(defun approve-context--pr-url ()
  "Return the URL of the PR of the buffer."
  (or (approve-pr-field 'url)
      (format "https://%s/%s/%s/pull/%s"
              (or (approve-model-meta :host) "github.com")
              (approve-model-meta :owner) (approve-model-meta :repo)
              (approve-model-meta :number))))

(defun approve-context-file-url (path &optional side line)
  "Return the URL of PATH in the PR's files, at LINE on SIDE if given.
SIDE is \"LEFT\" or \"RIGHT\", as GitHub's anchors want."
  (concat (approve-context--pr-url) "/files#diff-" (secure-hash 'sha256 path)
          (if line (format "%s%d" (if (equal side "LEFT") "L" "R") line) "")))

(defun approve-context--file (path)
  "Return the context of the changed file PATH."
  (let ((file (seq-find (lambda (file) (equal (alist-get 'path file) path))
                        (approve-pr-files))))
    (list :kind 'file :path path
          :viewed (alist-get 'viewerViewedState file)
          :url (approve-context-file-url path))))

(defun approve-context--line-plist (line)
  "Return the context of the diff LINE, an `approve-line' plist."
  (let* ((side (plist-get line :side))
         (number (if (equal side "LEFT") (plist-get line :old) (plist-get line :new))))
    (list :kind 'line :path (plist-get line :path) :side side :line number
          :old (plist-get line :old) :new (plist-get line :new)
          :line-kind (plist-get line :kind)
          :url (approve-context-file-url (plist-get line :path) side number))))

(defun approve-context--region ()
  "Return the `lines' context of the diff lines in the region, or nil.
The region must start and end on diff lines of one file."
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (save-excursion
                  (goto-char (region-end))
                  ;; A region ending at the start of a line excludes it.
                  (if (and (bolp) (> (point) start)) (1- (point)) (point))))
           (first (save-excursion (goto-char start)
                                  (get-text-property (line-beginning-position)
                                                     'approve-line)))
           (last (save-excursion (goto-char end)
                                 (get-text-property (line-beginning-position)
                                                    'approve-line))))
      (when (and first last
                 (equal (plist-get first :path) (plist-get last :path)))
        (let ((from (approve-context--line-plist first))
              (to (approve-context--line-plist last)))
          (list :kind 'lines :path (plist-get to :path)
                :side (plist-get to :side) :line (plist-get to :line)
                :start-side (plist-get from :side) :start-line (plist-get from :line)
                :url (plist-get to :url)))))))

(defun approve-context--thing (pos)
  "Return the context of the `approve-thing' at POS, or nil."
  (when-let* ((thing (get-text-property pos 'approve-thing)))
    (let ((kind (plist-get thing :kind)))
      (append
       (list :kind kind)
       (pcase kind
         ('user (list :value (plist-get thing :login)
                      :url (or (plist-get thing :url)
                               (format "https://github.com/%s" (plist-get thing :login)))))
         ('team (list :value (plist-get thing :slug) :url (plist-get thing :url)))
         ('label (list :value (plist-get thing :name) :url (plist-get thing :url)))
         ('commit (list :value (plist-get thing :oid) :url (plist-get thing :url)))
         ('check (let ((check (plist-get thing :check)))
                   (list :value (or (alist-get 'name check) (alist-get 'context check))
                         :id (alist-get 'id check) :url (plist-get thing :url))))
         ('ref (list :value (plist-get thing :number) :url (plist-get thing :url)))
         ('milestone (list :value (plist-get thing :title) :url (plist-get thing :url)))
         ('branch (list :value (plist-get thing :name)))
         ('date (list :value (plist-get thing :value)))
         ('load-diff (list :path (plist-get thing :path)
                           :url (approve-context-file-url (plist-get thing :path))))
         (_ (list :url (plist-get thing :url))))
       (list :thing thing)))))

(defun approve-context--link (pos)
  "Return a `link' context for a plain link at POS, or nil."
  (when-let* ((url (get-text-property pos 'approve-url)))
    (list :kind 'link :value url :url url)))

(defun approve-context--thread (id)
  "Return the context of the thread ID."
  (let ((thread (approve-context--entity
                 "PullRequestReviewThread" id
                 '(path line diffSide isResolved viewerCanReply viewerCanResolve
                        viewerCanUnresolve comments))))
    (let-alist thread
      (list :kind 'thread :thread-id id :path .path :line .line :side .diffSide
            :resolved (and .isResolved t)
            :can-reply (and .viewerCanReply t)
            :can-resolve (and (if .isResolved .viewerCanUnresolve .viewerCanResolve) t)
            :url (alist-get 'url (car (approve-model-nodes .comments)))))))

(defun approve-context--comment (kind typename id &optional thread-id)
  "Return the context of a comment of KIND, a TYPENAME with ID.
THREAD-ID is the id of its thread, for review comments."
  (let-alist (approve-context--entity typename id
                                      '(url viewerCanUpdate viewerCanDelete state))
    (append (list :kind kind :comment-id id
                  :can-update (and .viewerCanUpdate t)
                  :can-delete (and .viewerCanDelete t)
                  :url .url)
            (and thread-id (list :thread-id thread-id))
            (and .state (list :state .state)))))

(defun approve-context--parent-value (section type)
  "Return the value of the nearest ancestor of SECTION of TYPE, or nil."
  (let ((parent (oref section parent)))
    (while (and parent (not (eq (oref parent type) type)))
      (setq parent (oref parent parent)))
    (and parent (oref parent value))))

(defun approve-context--section (section)
  "Return the context SECTION means, or nil to look at its parent."
  (let ((value (oref section value)))
    (pcase (oref section type)
      ('approve-thread-section (approve-context--thread value))
      ('approve-comment-section
       (approve-context--comment 'review-comment "PullRequestReviewComment" value
                                 (approve-context--parent-value
                                  section 'approve-thread-section)))
      ('approve-pending-comment-section
       (approve-context--comment 'review-comment "PullRequestReviewComment" value))
      ('approve-issue-comment-section
       (approve-context--comment 'issue-comment "IssueComment" value))
      ('approve-review-section
       (let-alist (approve-context--entity "PullRequestReview" value '(state url))
         (list :kind 'review :review-id value :state .state :url .url)))
      ('approve-description-section
       (list :kind 'description :can-update (approve-pr-viewer-can 'update)
             :url (approve-context--pr-url)))
      ((or 'approve-file-section 'approve-file-entry-section)
       (approve-context--file value))
      ('approve-hunk-section (approve-context--file (car value)))
      ('approve-file-threads-section
       (approve-context--file (approve-context--parent-value section
                                                             'approve-file-section)))
      ('approve-commit-section
       (list :kind 'commit :value value
             :url (format "%s/commits/%s" (approve-context--pr-url) value)))
      ('approve-meta-section (list :kind 'meta :row value))
      ('approve-pending-review-section
       (list :kind 'pending-review :review-id value))
      ('approve-event-section (list :kind 'event :id value)))))

(defun approve-context--from-sections (pos)
  "Return the context of the innermost meaningful section at POS, or nil."
  (let ((section (magit-section-at pos))
        (context nil))
    (while (and section (not context))
      (setq context (approve-context--section section)
            section (oref section parent)))
    context))

(defun approve-context-at-point (&optional pos)
  "Return what is at POS (default: point) in an Approve buffer, as a plist.
:kind says what it is; the table in docs/architecture.org (\"Context
at point\") lists the kinds and their keys.  An active region over
diff lines gives `lines'.  When nothing more specific is at POS, the
context is the pull request, kind `pr'.

Example, on an added line:
  (:kind line :path \"a.el\" :side \"RIGHT\" :line 42 :old nil :new 42
   :line-kind add :url \"…/files#diff-…R42\")"
  (let ((pos (or pos (point))))
    (or (and (= pos (point)) (approve-context--region))
        (approve-context--thing pos)
        (when-let* ((line (get-text-property pos 'approve-line)))
          (approve-context--line-plist line))
        (approve-context--link pos)
        (approve-context--from-sections pos)
        (list :kind 'pr :url (approve-context--pr-url)))))

(provide 'approve-context)

;;; approve-context.el ends here
