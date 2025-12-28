;;; approve-input.el --- Input mode for Approve  -*- lexical-binding: t; -*-

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

;; This module provides an input mode for editing text in Approve.
;;
;; The mode derives from `gfm-mode' (GitHub Flavored Markdown) and is used
;; for editing PR titles, descriptions, comments, and other text content.
;;
;; Key features:
;; - Single-line or multi-line input modes
;; - Help header showing available keybindings
;; - Commit changes with C-c C-c
;; - Abort changes with C-c C-k
;; - Callback-based architecture for flexibility
;;
;; Usage:
;;   (approve-input-read
;;    :prompt \"Edit PR title\"
;;    :initial \"Current title\"
;;    :multiline nil
;;    :on-commit (lambda (text) (message \"New title: %s\" text))
;;    :on-abort (lambda () (message \"Cancelled\")))

;;; Code:

(require 'markdown-mode)

;;; Customization

(defgroup approve-input nil
  "Input mode settings for Approve."
  :group 'approve
  :prefix "approve-input-")

(defcustom approve-input-window-setup 'split-below
  "How to display the input buffer.
Possible values:
  `split-below' - Split the current window horizontally
  `split-right' - Split the current window vertically
  `current'     - Use the current window
  `other'       - Use another window"
  :group 'approve-input
  :type '(choice (const :tag "Split below" split-below)
                 (const :tag "Split right" split-right)
                 (const :tag "Current window" current)
                 (const :tag "Other window" other)))

(defcustom approve-input-header-face 'font-lock-comment-face
  "Face used for the header line in input buffers."
  :group 'approve-input
  :type 'face)

;;; Variables

(defvar-local approve-input--prompt nil
  "The prompt/title shown in the header.")

(defvar-local approve-input--multiline nil
  "Non-nil if this is a multiline input buffer.")

(defvar-local approve-input--on-commit nil
  "Callback function called with the input text when committed.")

(defvar-local approve-input--on-abort nil
  "Callback function called with no arguments when aborted.")

(defvar-local approve-input--source-buffer nil
  "The buffer from which the input was initiated.")

(defvar-local approve-input--source-window nil
  "The window from which the input was initiated.")

(defvar-local approve-input--content-start nil
  "Marker for the start of editable content.")

;;; Mode Definition

(defvar approve-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'approve-input-commit)
    (define-key map (kbd "C-c C-k") #'approve-input-abort)
    map)
  "Keymap for `approve-input-mode'.")

(define-derived-mode approve-input-mode gfm-mode "Approve-Input"
  "Major mode for editing text input in Approve.

This mode derives from `gfm-mode' for GitHub Flavored Markdown support.
Use `C-c C-c' to commit the changes and `C-c C-k' to abort.

\\{approve-input-mode-map}"
  :group 'approve-input
  (setq-local header-line-format
              '(:eval (approve-input--header-line)))
  (setq buffer-offer-save nil))

;;; Header Line

(defun approve-input--header-line ()
  "Generate the header line for the input buffer."
  (let* ((prompt (or approve-input--prompt "Input"))
         (help (substitute-command-keys
                "\\<approve-input-mode-map>Commit `\\[approve-input-commit]', Abort `\\[approve-input-abort]'")))
    (concat
     (propertize (format " %s " prompt) 'face 'bold)
     " — "
     (propertize help 'face approve-input-header-face))))

;;; Buffer Management

(defun approve-input--buffer-name (prompt)
  "Generate a buffer name for input with PROMPT."
  (format "*Approve: %s*" (or prompt "Input")))

(defun approve-input--create-buffer (prompt initial multiline)
  "Create an input buffer with PROMPT, INITIAL content and MULTILINE flag."
  (let* ((buffer-name (approve-input--buffer-name prompt))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (approve-input-mode)
      (setq approve-input--prompt prompt
            approve-input--multiline multiline
            approve-input--content-start (point-min-marker))
      (when initial
        (insert initial))
      (goto-char (point-min)))
    buffer))

(defun approve-input--display-buffer (buffer)
  "Display BUFFER according to `approve-input-window-setup'."
  (let ((window (pcase approve-input-window-setup
                  ('split-below
                   (select-window (split-window-below)))
                  ('split-right
                   (select-window (split-window-right)))
                  ('current
                   (selected-window))
                  ('other
                   (select-window (next-window)))
                  (_ (selected-window)))))
    (set-window-buffer window buffer)
    window))

(defun approve-input--get-content ()
  "Get the editable content from the current input buffer."
  (let ((content (buffer-substring-no-properties
                  (or approve-input--content-start (point-min))
                  (point-max))))
    (if approve-input--multiline
        content
      ;; For single-line input, take only the first line and trim
      (string-trim (car (split-string content "\n"))))))

(defun approve-input--cleanup ()
  "Clean up the input buffer and restore window configuration."
  (let ((buffer (current-buffer))
        (source-window approve-input--source-window)
        (source-buffer approve-input--source-buffer))
    (quit-window t)
    (when (and source-window (window-valid-p source-window))
      (select-window source-window))
    (when (and source-buffer (buffer-live-p source-buffer))
      (set-buffer source-buffer))))

;;; Commands

(defun approve-input-commit ()
  "Commit the current input and call the commit callback."
  (interactive)
  (let ((content (approve-input--get-content))
        (callback approve-input--on-commit))
    (approve-input--cleanup)
    (when callback
      (funcall callback content))))

(defun approve-input-abort ()
  "Abort the current input and call the abort callback."
  (interactive)
  (let ((callback approve-input--on-abort))
    (approve-input--cleanup)
    (when callback
      (funcall callback))))

;;; Public API

(cl-defun approve-input-read (&key prompt initial multiline on-commit on-abort)
  "Open an input buffer for editing text.

PROMPT is the title shown in the header line.
INITIAL is the initial content to populate the buffer with.
MULTILINE if non-nil allows multi-line input, otherwise only the
  first line is used when committing.
ON-COMMIT is a function called with the input text when the user
  commits with `C-c C-c'.
ON-ABORT is a function called with no arguments when the user
  aborts with `C-c C-k'.

Returns the input buffer."
  (let* ((source-buffer (current-buffer))
         (source-window (selected-window))
         (buffer (approve-input--create-buffer prompt initial multiline)))
    (with-current-buffer buffer
      (setq approve-input--on-commit on-commit
            approve-input--on-abort on-abort
            approve-input--source-buffer source-buffer
            approve-input--source-window source-window))
    (approve-input--display-buffer buffer)
    buffer))

(provide 'approve-input)
;;; approve-input.el ends here
