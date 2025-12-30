;;; approve-ui-changes.el --- Changes UI components for Approve  -*- lexical-binding: t; -*-

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

;; This file provides UI components for displaying file changes
;; in Approve pull request buffers.
;;
;; The display format follows the magit commit buffer style:
;;
;;   2 files changed, 19 insertions(+), 10 deletions(-)
;;   ✓ lisp/approve-ui-headers.el | 12 ++----------
;;   • lisp/approve-ui-helpers.el | 17 +++++++++++++++++
;;
;; Files are marked with a viewed state indicator:
;;   ✓ (checkmark) - File has been viewed
;;   • (bullet)    - File has not been viewed
;;   ↻ (refresh)   - File has new changes since last viewed (dismissed)

;;; Code:

(require 'cl-lib)
(require 'magit-section)

(require 'approve-model)
(require 'approve-ui-faces)
(require 'approve-ui-helpers)

;;; Customization

(defcustom approve-diffstat-graph-width 30
  "Maximum width of the diffstat graph (the +/- bar).
This is the maximum number of +/- characters shown for a file."
  :group 'approve
  :type 'integer)

(defcustom approve-file-viewed-indicator "✓"
  "Indicator shown for files that have been viewed."
  :group 'approve
  :type 'string)

(defcustom approve-file-unviewed-indicator "•"
  "Indicator shown for files that have not been viewed."
  :group 'approve
  :type 'string)

(defcustom approve-file-dismissed-indicator "↻"
  "Indicator shown for files with new changes since last viewed."
  :group 'approve
  :type 'string)

;;; Internal Constants

(defconst approve--diffstat-graph-char-added ?+
  "Character used to represent added lines in diffstat graph.")

(defconst approve--diffstat-graph-char-removed ?-
  "Character used to represent removed lines in diffstat graph.")

;;; Private Functions

(defun approve--diffstat-summary-string (file-count additions deletions)
  "Format summary string for FILE-COUNT files with ADDITIONS and DELETIONS.
Returns a string like \"2 files changed, 19 insertions(+), 10 deletions(-)\"."
  (let ((file-word (if (= file-count 1) "file" "files"))
        (ins-word (if (= additions 1) "insertion" "insertions"))
        (del-word (if (= deletions 1) "deletion" "deletions")))
    (format "%d %s changed, %d %s(+), %d %s(-)"
            file-count file-word
            additions ins-word
            deletions del-word)))

(defun approve--diffstat-scale-counts (additions deletions max-changes)
  "Scale ADDITIONS and DELETIONS to fit within `approve-diffstat-graph-width'.
MAX-CHANGES is the maximum total changes for any file in the changeset.
Returns a cons cell (scaled-additions . scaled-deletions)."
  (let ((total (+ additions deletions)))
    (if (or (zerop total) (zerop max-changes))
        (cons 0 0)
      (let* ((scale (min 1.0 (/ (float approve-diffstat-graph-width) max-changes)))
             (scaled-add (round (* additions scale)))
             (scaled-del (round (* deletions scale))))
        ;; Ensure at least 1 character for non-zero values
        (when (and (> additions 0) (zerop scaled-add))
          (setq scaled-add 1))
        (when (and (> deletions 0) (zerop scaled-del))
          (setq scaled-del 1))
        (cons scaled-add scaled-del)))))

(defun approve--diffstat-graph-string (additions deletions max-changes)
  "Create diffstat graph string for ADDITIONS and DELETIONS.
MAX-CHANGES is used for scaling.  Returns a string like \"+++++-----\"."
  (let* ((scaled (approve--diffstat-scale-counts additions deletions max-changes))
         (add-count (car scaled))
         (del-count (cdr scaled))
         (add-str (make-string add-count approve--diffstat-graph-char-added))
         (del-str (make-string del-count approve--diffstat-graph-char-removed)))
    (concat
     (approve-ui-propertize-face add-str 'approve-diffstat-added-face)
     (approve-ui-propertize-face del-str 'approve-diffstat-removed-face))))

(defun approve--diffstat-file-max-changes (files)
  "Return the maximum total changes for any file in FILES.
FILES is a list of file alists with `additions' and `deletions' keys."
  (let ((max-val 0))
    (dolist (file files max-val)
      (let ((total (+ (or (alist-get 'additions file) 0)
                      (or (alist-get 'deletions file) 0))))
        (when (> total max-val)
          (setq max-val total))))))

(defun approve--diffstat-max-path-width (files)
  "Return the maximum path width for FILES."
  (let ((max-width 0))
    (dolist (file files max-width)
      (let ((width (length (alist-get 'path file))))
        (when (> width max-width)
          (setq max-width width))))))

(defun approve--diffstat-max-count-width (files)
  "Return the width needed for the largest change count in FILES."
  (let ((max-count 0))
    (dolist (file files)
      (let ((total (+ (or (alist-get 'additions file) 0)
                      (or (alist-get 'deletions file) 0))))
        (when (> total max-count)
          (setq max-count total))))
    (length (number-to-string max-count))))

(defun approve--format-viewed-indicator (viewed-state)
  "Format the viewed state indicator for VIEWED-STATE.
VIEWED-STATE is one of \"VIEWED\", \"UNVIEWED\", or \"DISMISSED\"."
  (pcase viewed-state
    ("VIEWED"
     (approve-ui-propertize-face approve-file-viewed-indicator
                                 'approve-file-viewed-face))
    ("DISMISSED"
     (approve-ui-propertize-face approve-file-dismissed-indicator
                                 'approve-file-dismissed-face))
    (_  ; Default to unviewed for nil or "UNVIEWED"
     (approve-ui-propertize-face approve-file-unviewed-indicator
                                 'approve-file-unviewed-face))))

(defun approve--format-diffstat-line (file path-width count-width max-changes)
  "Format a single diffstat line for FILE.
PATH-WIDTH is the width to use for the path column.
COUNT-WIDTH is the width for the change count column.
MAX-CHANGES is used for scaling the graph."
  (let* ((path (alist-get 'path file))
         (additions (or (alist-get 'additions file) 0))
         (deletions (or (alist-get 'deletions file) 0))
         (viewed-state (alist-get 'viewerViewedState file))
         (total (+ additions deletions))
         (graph (approve--diffstat-graph-string additions deletions max-changes))
         (indicator (approve--format-viewed-indicator viewed-state)))
    (format "%s %s | %s %s"
            indicator
            (approve-ui-propertize-face
             (format (format "%%-%ds" path-width) path)
             'approve-diffstat-file-face)
            (approve-ui-propertize-face
             (format (format "%%%dd" count-width) total)
             'approve-diffstat-count-face)
            graph)))

;;; Public Section Functions

(defun approve-insert-changes-section ()
  "Insert the changes section in the PR review buffer.
Shows a diffstat summary followed by per-file changes."
  (when-let ((files-data (approve-model-root 'files)))
    (let* ((files (approve-model-get-nodes files-data))
           (file-count (length files))
           (additions (or (approve-model-root 'additions) 0))
           (deletions (or (approve-model-root 'deletions) 0))
           (truncated-p (approve-model-truncated-p files-data)))
      (when (> file-count 0)
        (insert "\n")
        (magit-insert-section (changes)
          (magit-insert-heading
            (approve--diffstat-summary-string file-count additions deletions))
          ;; Insert individual file lines
          (let ((max-changes (approve--diffstat-file-max-changes files))
                (path-width (approve--diffstat-max-path-width files))
                (count-width (approve--diffstat-max-count-width files)))
            (dolist (file files)
              (let ((path (alist-get 'path file)))
                (magit-insert-section (file path)
                  (insert (approve--format-diffstat-line
                           file path-width count-width max-changes)
                          "\n")))))
          ;; Show truncation info at the end of the section
          (when truncated-p
            (let ((total-count (approve-model-get-total-count files-data)))
              (insert (propertize
                       (format "(showing %d of %d files)"
                               file-count (or total-count "?"))
                       'face 'approve-pagination-truncated-face)
                      "\n")))
          (insert "\n"))))))

(provide 'approve-ui-changes)
;;; approve-ui-changes.el ends here
