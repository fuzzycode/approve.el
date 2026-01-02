;;; approve-file-view.el --- File viewing for Approve  -*- lexical-binding: t; -*-

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

;; This module provides file viewing functionality for Approve.
;;
;; Features:
;; - View file contents from a pull request with syntax highlighting
;; - Fringe indicators for added, modified, and deleted lines
;; - Navigation between diff hunks
;; - Toggle viewed state while viewing a file
;; - Caching of downloaded files for fast subsequent access
;;
;; The file viewer uses the standard Emacs major mode for syntax highlighting
;; based on the file extension.  Diff information is extracted from the
;; PR's diff data and displayed using fringe bitmaps.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'fringe)

(require 'approve-api-mutations)
(require 'approve-model)
(require 'approve-ui-faces)


;;; Customization

(defgroup approve-file-view nil
  "Settings for Approve file viewer."
  :group 'approve
  :prefix "approve-file-view-")

(defcustom approve-file-view-cache-directory
  (expand-file-name "approve-file-cache" temporary-file-directory)
  "Directory for caching downloaded files.
Files are cached by owner/repo/sha/path to avoid redundant downloads."
  :group 'approve-file-view
  :type 'directory)

(defcustom approve-file-view-fringe-side 'left-fringe
  "Which fringe to use for diff indicators.
Can be `left-fringe' or `right-fringe'."
  :group 'approve-file-view
  :type '(choice (const :tag "Left fringe" left-fringe)
                 (const :tag "Right fringe" right-fringe)))

;;; Faces

(defface approve-file-view-added-face
  '((t :inherit diff-added))
  "Face for lines added in the diff."
  :group 'approve-faces)

(defface approve-file-view-deleted-face
  '((t :inherit diff-removed))
  "Face for lines deleted in the diff."
  :group 'approve-faces)

(defface approve-file-view-modified-face
  '((t :inherit diff-changed))
  "Face for lines modified in the diff."
  :group 'approve-faces)

(defface approve-file-view-fringe-added-face
  '((t :foreground "green3"))
  "Face for fringe indicator on added lines."
  :group 'approve-faces)

(defface approve-file-view-fringe-deleted-face
  '((t :foreground "red3"))
  "Face for fringe indicator on deleted lines."
  :group 'approve-faces)

(defface approve-file-view-fringe-modified-face
  '((t :foreground "orange"))
  "Face for fringe indicator on modified lines."
  :group 'approve-faces)

;;; Fringe Bitmaps

(defvar approve-file-view--fringe-bitmaps-defined nil
  "Non-nil if fringe bitmaps have been defined.")

(defun approve-file-view--define-fringe-bitmaps ()
  "Define fringe bitmaps for diff indicators."
  (unless approve-file-view--fringe-bitmaps-defined
    (when (display-graphic-p)
      ;; Solid bar for added/modified lines
      (define-fringe-bitmap 'approve-fringe-added
        [#b11111100
         #b11111100
         #b11111100
         #b11111100
         #b11111100
         #b11111100
         #b11111100
         #b11111100]
        nil nil 'center)
      ;; Triangle pointing left for deleted lines (shows where lines were removed)
      (define-fringe-bitmap 'approve-fringe-deleted
        [#b00000100
         #b00001100
         #b00011100
         #b00111100
         #b01111100
         #b00111100
         #b00011100
         #b00001100
         #b00000100]
        nil nil 'center)
      (setq approve-file-view--fringe-bitmaps-defined t))))

;;; Buffer-local Variables

(defvar-local approve-file-view--pr-buffer nil
  "The Approve PR buffer this file view is associated with.")

(defvar-local approve-file-view--file-path nil
  "The path of the file being viewed.")

(defvar-local approve-file-view--owner nil
  "Repository owner for the current file.")

(defvar-local approve-file-view--repo nil
  "Repository name for the current file.")

(defvar-local approve-file-view--head-sha nil
  "Head SHA for the current file.")

(defvar-local approve-file-view--diff-hunks nil
  "List of diff hunks for the current file.
Each hunk is a plist with :start-line, :line-count, and :type.")

(defvar-local approve-file-view--overlays nil
  "List of overlays created for diff indicators.")

(defvar-local approve-file-view--change-positions nil
  "Sorted list of line numbers where changes occur.
Used for navigation between changes.")

;;; Minor Mode

(defvar approve-file-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'approve-file-view-next-change)
    (define-key map (kbd "p") #'approve-file-view-previous-change)
    (define-key map (kbd "v") #'approve-file-view-toggle-viewed)
    (define-key map (kbd "q") #'approve-file-view-quit)
    (define-key map (kbd "g") #'approve-file-view-refresh)
    map)
  "Keymap for `approve-file-view-mode'.")

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal approve-file-view-mode-map
    "]]" #'approve-file-view-next-change
    "[[" #'approve-file-view-previous-change
    "v" #'approve-file-view-toggle-viewed
    "q" #'approve-file-view-quit
    "gr" #'approve-file-view-refresh))

(define-minor-mode approve-file-view-mode
  "Minor mode for viewing PR files with diff indicators.

\\{approve-file-view-mode-map}"
  :lighter " APR-View"
  :keymap approve-file-view-mode-map
  (if approve-file-view-mode
      (progn
        (approve-file-view--define-fringe-bitmaps)
        (setq buffer-read-only t))
    (approve-file-view--clear-overlays)
    (setq buffer-read-only nil)))

;;; Cache Management

(defun approve-file-view--cache-path (owner repo sha file-path)
  "Return the cache file path for OWNER/REPO at SHA for FILE-PATH."
  (let* ((safe-path (replace-regexp-in-string "/" "_" file-path))
         (cache-dir (expand-file-name
                     (format "%s/%s/%s" owner repo sha)
                     approve-file-view-cache-directory)))
    (expand-file-name safe-path cache-dir)))

(defun approve-file-view--ensure-cache-dir (cache-path)
  "Ensure the directory for CACHE-PATH exists."
  (let ((dir (file-name-directory cache-path)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun approve-file-view--cached-p (owner repo sha file-path)
  "Return non-nil if the file is cached."
  (let ((cache-path (approve-file-view--cache-path owner repo sha file-path)))
    (file-exists-p cache-path)))

(defun approve-file-view--read-cache (owner repo sha file-path)
  "Read cached file content for OWNER/REPO at SHA for FILE-PATH.
Returns nil if not cached."
  (let ((cache-path (approve-file-view--cache-path owner repo sha file-path)))
    (when (file-exists-p cache-path)
      (with-temp-buffer
        (insert-file-contents cache-path)
        (buffer-string)))))

(defun approve-file-view--write-cache (owner repo sha file-path content)
  "Write CONTENT to cache for OWNER/REPO at SHA for FILE-PATH."
  (let ((cache-path (approve-file-view--cache-path owner repo sha file-path)))
    (approve-file-view--ensure-cache-dir cache-path)
    (with-temp-file cache-path
      (insert content))))

;;; Diff Parsing

(defun approve-file-view--parse-patch (patch-text)
  "Parse PATCH-TEXT (unified diff for a single file) into hunk information.
Returns a list of plists with :line and :type (added or deleted)."
  (when (and patch-text (> (length patch-text) 0))
    (let ((hunks nil))
      (with-temp-buffer
        (insert patch-text)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            ;; Hunk header: @@ -old-start,old-count +new-start,new-count @@
            (when (string-match "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                                line)
              (let* ((new-start (string-to-number (match-string 3 line)))
                     (current-new-line new-start))
                ;; Parse the hunk content
                (forward-line 1)
                (while (and (not (eobp))
                            (not (looking-at "^@@")))
                  (let* ((hunk-line (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                         (first-char (if (> (length hunk-line) 0)
                                         (aref hunk-line 0)
                                       ?\s)))
                    (pcase first-char
                      (?+
                       ;; Added line - show at this line number
                       (push (list :line current-new-line :type 'added) hunks)
                       (cl-incf current-new-line))
                      (?-
                       ;; Deleted line - show indicator at current position
                       ;; (where the line would have been)
                       (push (list :line current-new-line :type 'deleted) hunks))
                      (_
                       ;; Context line (space) or other
                       (cl-incf current-new-line))))
                  (forward-line 1))
                ;; Go back one line since we overshot (or reached eobp)
                (unless (eobp)
                  (forward-line -1)))))
          (forward-line 1)))
      (nreverse hunks))))

(defun approve-file-view--get-file-diff (file-path)
  "Get the diff data for FILE-PATH from the PR model.
Returns the patch string or nil."
  (when-let ((diff-data (approve-model-root 'diff)))
    (let ((files (alist-get 'files diff-data)))
      ;; files comes from REST API as vector but model resolves it to a list
      (when (and files (> (length files) 0))
        (cl-loop for file in files
                 ;; GitHub uses 'filename' in the compare API
                 when (string= (alist-get 'filename file) file-path)
                 return (alist-get 'patch file))))))

;;; Overlay Management

(defun approve-file-view--clear-overlays ()
  "Remove all diff indicator overlays."
  (mapc #'delete-overlay approve-file-view--overlays)
  (setq approve-file-view--overlays nil))

(defun approve-file-view--create-fringe-overlay (line type)
  "Create a fringe overlay at LINE for change TYPE.
TYPE is one of `added', `deleted', or `modified'."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (ov (make-overlay bol (1+ eol) nil t nil))
           (bitmap (pcase type
                     ('added 'approve-fringe-added)
                     ('deleted 'approve-fringe-deleted)
                     (_ 'approve-fringe-added)))
           (face (pcase type
                   ('added 'approve-file-view-fringe-added-face)
                   ('deleted 'approve-file-view-fringe-deleted-face)
                   (_ 'approve-file-view-fringe-modified-face))))
      (overlay-put ov 'approve-file-view t)
      (overlay-put ov 'approve-change-type type)
      (overlay-put ov 'before-string
                   (propertize " " 'display
                               (list approve-file-view-fringe-side
                                     bitmap face)))
      (push ov approve-file-view--overlays)
      ov)))

(defun approve-file-view--apply-diff-indicators (hunks)
  "Apply diff indicators to the buffer based on HUNKS."
  (approve-file-view--clear-overlays)
  (let ((change-lines nil)
        (last-deleted-line nil))
    (dolist (hunk hunks)
      (let ((line (plist-get hunk :line))
            (type (plist-get hunk :type)))
        (when (and line (> line 0) (<= line (line-number-at-pos (point-max))))
          (pcase type
            ('added
             (approve-file-view--create-fringe-overlay line 'added)
             (push line change-lines))
            ('deleted
             ;; For deleted lines, we show the indicator at the line
             ;; where the deletion happened (consolidate multiple deletions)
             (unless (equal line last-deleted-line)
               (approve-file-view--create-fringe-overlay line 'deleted)
               (push line change-lines)
               (setq last-deleted-line line)))))))
    ;; Store sorted unique positions for navigation
    (setq approve-file-view--change-positions
          (sort (delete-dups change-lines) #'<))))

;;; File Fetching

(defun approve-file-view--fetch-file (owner repo sha file-path callback)
  "Fetch FILE-PATH at SHA from OWNER/REPO and call CALLBACK with content.
Uses cache if available."
  (if-let ((cached (approve-file-view--read-cache owner repo sha file-path)))
      (funcall callback cached)
    ;; Fetch from GitHub REST API
    (let ((resource (format "/repos/%s/%s/contents/%s"
                            (url-hexify-string owner)
                            (url-hexify-string repo)
                            file-path)))
      (approve-api-rest
       "GET" resource
       :query `((ref . ,sha))
       :callback (lambda (response)
                   (let ((content (alist-get 'content response))
                         (encoding (alist-get 'encoding response)))
                     (when content
                       (let ((decoded (if (string= encoding "base64")
                                          (base64-decode-string
                                           (replace-regexp-in-string "\n" "" content))
                                        content)))
                         ;; Cache the content
                         (approve-file-view--write-cache owner repo sha file-path decoded)
                         (funcall callback decoded)))))
       :error-callback (lambda (err)
                         (message "Failed to fetch file: %s" err))
       :progress-message (format "Fetching %s..." file-path)))))

;;; Buffer Setup

(defun approve-file-view--buffer-name (owner repo file-path)
  "Generate buffer name for viewing FILE-PATH from OWNER/REPO."
  (format "*Approve: %s/%s - %s*" owner repo file-path))

(defun approve-file-view--set-major-mode (file-path)
  "Set the appropriate major mode for FILE-PATH.
Uses `auto-mode-alist' to determine the mode based on filename."
  (let ((buffer-file-name file-path)
        (enable-local-variables nil))  ; Don't process local variables
    (set-auto-mode t)
    ;; Ensure font-lock mode is enabled for syntax highlighting
    (font-lock-mode 1)
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure))))

(defun approve-file-view--setup-buffer (content file-path owner repo head-sha pr-buffer)
  "Set up a file view buffer with CONTENT for FILE-PATH.
OWNER, REPO, HEAD-SHA identify the file source.
PR-BUFFER is the originating Approve buffer."
  (let ((buffer-name (approve-file-view--buffer-name owner repo file-path))
        (patch nil))
    ;; Get the patch from the PR buffer first (before switching buffers)
    (with-current-buffer pr-buffer
      (setq patch (approve-file-view--get-file-diff file-path)))
    ;; Now set up the file view buffer
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Kill all local variables to start fresh (important for re-opening)
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min)))
      ;; Set major mode for syntax highlighting BEFORE enabling minor mode
      (approve-file-view--set-major-mode file-path)
      ;; Store metadata (after major mode, as it may kill local variables)
      (setq approve-file-view--pr-buffer pr-buffer
            approve-file-view--file-path file-path
            approve-file-view--owner owner
            approve-file-view--repo repo
            approve-file-view--head-sha head-sha)
      ;; Parse and apply diff indicators
      (when patch
        (let ((hunks (approve-file-view--parse-patch patch)))
          (setq approve-file-view--diff-hunks hunks)
          (approve-file-view--apply-diff-indicators hunks)))
      ;; Enable minor mode last (it sets buffer-read-only)
      (approve-file-view-mode 1)
      ;; Display buffer
      (switch-to-buffer (current-buffer)))))

;;; Navigation

(defun approve-file-view-next-change ()
  "Jump to the next changed line."
  (interactive)
  (unless approve-file-view--change-positions
    (user-error "No changes in this file"))
  (let* ((current-line (line-number-at-pos))
         (next-line (cl-find-if (lambda (l) (> l current-line))
                                approve-file-view--change-positions)))
    (if next-line
        (progn
          (goto-char (point-min))
          (forward-line (1- next-line))
          (message "Change %d/%d"
                   (1+ (cl-position next-line approve-file-view--change-positions))
                   (length approve-file-view--change-positions)))
      (message "No more changes"))))

(defun approve-file-view-previous-change ()
  "Jump to the previous changed line."
  (interactive)
  (unless approve-file-view--change-positions
    (user-error "No changes in this file"))
  (let* ((current-line (line-number-at-pos))
         (prev-line (cl-find-if (lambda (l) (< l current-line))
                                (reverse approve-file-view--change-positions))))
    (if prev-line
        (progn
          (goto-char (point-min))
          (forward-line (1- prev-line))
          (message "Change %d/%d"
                   (1+ (cl-position prev-line approve-file-view--change-positions))
                   (length approve-file-view--change-positions)))
      (message "No previous changes"))))

;;; Commands

(defun approve-file-view-toggle-viewed ()
  "Toggle the viewed state of the current file."
  (interactive)
  (unless (and approve-file-view--pr-buffer
               (buffer-live-p approve-file-view--pr-buffer))
    (user-error "PR buffer no longer available"))
  (let ((file-path approve-file-view--file-path)
        (pr-buffer approve-file-view--pr-buffer))
    (with-current-buffer pr-buffer
      (let* ((current-state (approve-file-view--get-viewed-state file-path))
             (pr-id (approve-model-root 'id))
             (should-mark (not (string= current-state "VIEWED"))))
        (if should-mark
            (approve-api-mutation-mark-file-as-viewed
             pr-id file-path
             :buffer pr-buffer
             :on-success
             (lambda (data)
               (with-current-buffer pr-buffer
                 (when-let ((pr-data (alist-get 'pullRequest
                                                (alist-get 'markFileAsViewed data))))
                   (approve-model-patch pr-data)))
               (message "Marked %s as viewed" file-path))
             :on-error
             (lambda (err)
               (message "Failed to mark file as viewed: %s" err)))
          (approve-api-mutation-unmark-file-as-viewed
           pr-id file-path
           :buffer pr-buffer
           :on-success
           (lambda (data)
             (with-current-buffer pr-buffer
               (when-let ((pr-data (alist-get 'pullRequest
                                              (alist-get 'unmarkFileAsViewed data))))
                 (approve-model-patch pr-data)))
             (message "Marked %s as unviewed" file-path))
           :on-error
           (lambda (err)
             (message "Failed to mark file as unviewed: %s" err))))))))

(defun approve-file-view--get-viewed-state (path)
  "Get the viewed state for file at PATH from the PR model."
  (when-let ((files-data (approve-model-root 'files)))
    (let ((files (approve-model-get-nodes files-data)))
      (cl-loop for file in files
               when (string= (alist-get 'path file) path)
               return (alist-get 'viewerViewedState file)))))

(defun approve-file-view-quit ()
  "Quit the file view buffer and return to PR buffer."
  (interactive)
  (let ((pr-buffer approve-file-view--pr-buffer))
    (kill-buffer (current-buffer))
    (when (and pr-buffer (buffer-live-p pr-buffer))
      (switch-to-buffer pr-buffer))))

(defun approve-file-view-refresh ()
  "Refresh the current file view."
  (interactive)
  (when (and approve-file-view--owner
             approve-file-view--repo
             approve-file-view--head-sha
             approve-file-view--file-path
             approve-file-view--pr-buffer)
    (let ((owner approve-file-view--owner)
          (repo approve-file-view--repo)
          (sha approve-file-view--head-sha)
          (file-path approve-file-view--file-path)
          (pr-buffer approve-file-view--pr-buffer))
      ;; Clear cache to force re-fetch
      (let ((cache-path (approve-file-view--cache-path owner repo sha file-path)))
        (when (file-exists-p cache-path)
          (delete-file cache-path)))
      ;; Re-fetch and display
      (approve-file-view--fetch-file
       owner repo sha file-path
       (lambda (content)
         (approve-file-view--setup-buffer
          content file-path owner repo sha pr-buffer))))))

;;; Public API

(defun approve-file-view-open (file-path)
  "Open FILE-PATH from the current PR for viewing.
This is the main entry point for viewing a file from an Approve buffer."
  (approve-with-pr-buffer
    (let* ((owner (approve-model-metadata :owner))
           (repo (approve-model-metadata :repo))
           (head-sha (approve-model-root 'headRefOid))
           (pr-buffer (current-buffer)))
      (unless head-sha
        (user-error "Cannot determine file version (no head SHA)"))
      (approve-file-view--fetch-file
       owner repo head-sha file-path
       (lambda (content)
         (approve-file-view--setup-buffer
          content file-path owner repo head-sha pr-buffer))))))

(provide 'approve-file-view)
;;; approve-file-view.el ends here
