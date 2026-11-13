;;; approve-input.el --- Buffers to write comments in -*- lexical-binding: t; -*-

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

;; `approve-input-read' opens a buffer below the current one, to write
;; a comment, a reply, a review body, a description or a title in
;; (docs/ui.org, "Comment input buffer"; docs/architecture.org, "Input
;; buffers").  The text is markdown, in `approve-input-mode', derived
;; from `gfm-mode' (decision D11).  C-c C-c hands it to the command
;; that opened the buffer; C-c C-k aborts.
;;
;;   (approve-input-read
;;    :prompt "Reply to bob on a.el:42" :context "pending review"
;;    :target '(reply "PRRT_1") :name "reply a.el:42"
;;    :on-commit (lambda (text) (message "Sending %s" text)))
;;
;; - A target names what the text is for.  Text that was aborted, or
;;   sent and not confirmed by GitHub yet, stays as a draft under its
;;   target in the session of the buffer the input was opened from
;;   (:drafts), and opening the same target again starts from it.  The
;;   command that sent the text drops the draft with
;;   `approve-input-clear-draft' once GitHub has it, so a failed request
;;   loses nothing (flow F00, decision D14).  Killing the PR buffer
;;   drops its drafts (decision D02 allows that).
;; - The header line says what the text is for and where it goes, and
;;   names the keys.
;; - The lines a comment is on (:lines, a range as approve-diff.el
;;   reads them) stay highlighted in the buffer it was opened from,
;;   across its redraws, until the text is sent or aborted.
;; - Opening the same target twice shows the buffer already open.
;;
;; Layer rules: this is the input layer, beside the buffers.  It keeps
;; drafts in the session of the buffer it was opened from and never
;; sends anything itself: the caller's ON-COMMIT does.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'markdown-mode)
(require 'approve-session)
(require 'approve-faces)
(require 'approve-render)
(require 'approve-diff)

;;; Options

(defcustom approve-input-display-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . 12))
  "How an input buffer is shown: a `display-buffer' action.
The default shows it where it is already shown, else below the
window it was asked for in, 12 lines high.  When it is done, its
window goes away again (or shows what it showed before).  To write
in the window of the pull request instead:

  (setq approve-input-display-action
        \\='((display-buffer-same-window)))"
  :type 'sexp
  :group 'approve)

;;; State of an input buffer

(defvar-local approve-input--source nil
  "The buffer the input was opened from; drafts and ON-COMMIT go there.")

(defvar-local approve-input--source-window nil
  "The window that was selected when the input was opened.")

(defvar-local approve-input--target nil
  "What the text is for, the key of its draft, or nil for no draft.")

(defvar-local approve-input--initial nil
  "The text the caller started the buffer with, or nil.")

(defvar-local approve-input--prompt nil
  "What the text is for, as the header line says it.")

(defvar-local approve-input--context nil
  "Where the text goes, as the header line says it after the prompt.")

(defvar-local approve-input--commit-label "send"
  "What \\<approve-input-mode-map>\\[approve-input-commit] does, for the header line.")

(defvar-local approve-input--allow-empty nil
  "Non-nil if an empty text may be sent.")

(defvar-local approve-input--on-commit nil
  "Function called with the text, in the source buffer, when it is sent.")

(defvar-local approve-input--on-abort nil
  "Function called with no arguments, in the source buffer, on abort.")

(defvar-local approve-input--done nil
  "Non-nil once the text was sent or aborted.")

(defvar-local approve-input--lines nil
  "The range of diff lines the text is about, or nil.
A context plist of kind `line' or `lines', as approve-diff.el reads
ranges.")

(defvar-local approve-input--overlays nil
  "Overlays highlighting `approve-input--lines' in the source buffer.")

;;; Modes

(defvar-keymap approve-input-mode-map
  :doc "Keymap of `approve-input-mode'."
  "C-c C-c" #'approve-input-commit
  "C-c C-k" #'approve-input-abort)

(define-derived-mode approve-input-mode gfm-mode "Approve-Input"
  "Major mode of the buffers comments and other text are written in.
The text is GitHub-flavoured markdown.  \\[approve-input-commit] sends it,
\\[approve-input-abort] aborts and keeps it as a draft.  Lines are not
filled: GitHub keeps a comment's line breaks, so long lines wrap on
screen (`visual-line-mode').

\\{approve-input-mode-map}"
  :group 'approve
  (setq-local header-line-format '(:eval (approve-input--header-line)))
  (visual-line-mode 1)
  (add-hook 'kill-buffer-hook #'approve-input--killed nil t)
  ;; Evil users start in insert state (decision D17).
  (when (featurep 'evil)
    (require 'approve-evil)))

(defvar-keymap approve-input-single-line-mode-map
  :doc "Keymap of `approve-input-single-line-mode'."
  "RET" #'approve-input-commit)

(define-minor-mode approve-input-single-line-mode
  "Minor mode of an input buffer for one line, such as a title.
\\<approve-input-single-line-mode-map>\\[approve-input-commit] sends the first line, trimmed."
  :keymap approve-input-single-line-mode-map
  :group 'approve)

;;; Drafts

(defun approve-input-draft (target)
  "Return the draft of TARGET in the current PR buffer's session, or nil."
  (approve-session-map-get :drafts target))

(defun approve-input-clear-draft (target)
  "Forget the draft of TARGET in the current PR buffer's session.
Call it once GitHub has the text that was sent for TARGET."
  (approve-session-map-remove :drafts target))

(defun approve-input--keep-draft (text)
  "Keep TEXT as the draft of this input's target, in its source buffer.
A blank TEXT, or the text the input started with, drops the draft
instead.  Return non-nil if a draft was kept."
  (let ((target approve-input--target)
        (initial approve-input--initial))
    (when (and target (buffer-live-p approve-input--source))
      (with-current-buffer approve-input--source
        (if (or (string-blank-p text) (equal text initial))
            (progn (approve-input-clear-draft target) nil)
          (approve-session-map-put :drafts target text)
          t)))))

;;; Header line

(defun approve-input--key (command)
  "Return the key of COMMAND in an input buffer, as text, or nil."
  (when-let* ((keys (where-is-internal command (current-local-map) t)))
    (key-description keys)))

(defun approve-input--hints ()
  "Return the key hints of the header line, or nil."
  (apply #'approve-render-hints
         (delq nil (list (list (approve-input--key #'approve-input-commit)
                               #'approve-input-commit approve-input--commit-label)
                         (list (approve-input--key #'approve-input-abort)
                               #'approve-input-abort "abort")))))

(defun approve-input--header-line ()
  "Return the header line of an input buffer.
It says what the text is for and where it goes, then the keys, at
the right edge."
  (let ((left (approve-render-join
               (list (approve-face (or approve-input--prompt "Text") 'approve-input-prompt)
                     (and approve-input--context
                          (approve-face-add approve-input--context
                                            'approve-input-context)))))
        (hints (approve-input--hints)))
    (concat " " left
            (and hints
                 (concat " "
                         (propertize " " 'display
                                     `(space :align-to (- right ,(1+ (string-width hints)))))
                         hints)))))

;;; Opening

(defun approve-input--open-from (source)
  "Return the input buffers opened from SOURCE and not done yet."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'approve-input-mode)
                       (not approve-input--done)
                       (eq approve-input--source source))))
              (buffer-list)))

(defun approve-input--find (source target)
  "Return the live input buffer opened from SOURCE for TARGET, or nil."
  (and target
       (seq-find (lambda (buffer)
                   (equal (buffer-local-value 'approve-input--target buffer) target))
                 (approve-input--open-from source))))

;;; Highlighting the lines

(defun approve-input--unhighlight ()
  "Remove the highlight of this input's lines."
  (mapc #'delete-overlay approve-input--overlays)
  (setq approve-input--overlays nil))

(defun approve-input--highlight ()
  "Highlight this input's lines in its source buffer, where they are drawn."
  (approve-input--unhighlight)
  (when (and approve-input--lines (buffer-live-p approve-input--source))
    (let ((lines approve-input--lines))
      (setq approve-input--overlays
            (with-current-buffer approve-input--source
              (mapcar (lambda (start)
                        (let ((overlay (make-overlay
                                        start (save-excursion
                                                (goto-char start)
                                                (line-beginning-position 2)))))
                          (overlay-put overlay 'face 'approve-target)
                          overlay))
                      (approve-diff-range-positions lines)))))))

(defun approve-input--rehighlight ()
  "Highlight the lines of the inputs open from this buffer again.
A redraw draws the lines anew, so it runs after each one."
  (dolist (buffer (approve-input--open-from (current-buffer)))
    (with-current-buffer buffer
      (approve-input--highlight))))

(cl-defun approve-input-read (&key prompt context target name initial lines
                                   single-line allow-empty (commit-label "send")
                                   on-commit on-abort)
  "Open a buffer to write text in, and return it.
PROMPT says what the text is for, such as \"Comment on a.el:41–42\";
CONTEXT where it goes, such as \"pending review\".  Both start the
header line.  NAME names the buffer: *approve-input: NAME*.

TARGET is what the text is for, any value compared with `equal',
such as (reply \"PRRT_1\").  The draft of TARGET, if there is one,
fills the buffer; else INITIAL does.  An aborted text, or a sent one,
becomes the draft of TARGET (in the session of the current buffer)
until the caller drops it with `approve-input-clear-draft'.  When an
input for TARGET is already open, it is shown instead.  With no
TARGET, no draft is kept.

LINES is the range of diff lines the text is about, a context plist
of kind `line' or `lines' (from `approve-context-at-point').  They
stay highlighted in the current buffer until the text is sent or
aborted.

SINGLE-LINE asks for one line (a title): RET sends it, and only its
first line counts.  ALLOW-EMPTY lets an empty text be sent (a review
body).  COMMIT-LABEL says what sending does, for the header line:
\"send\" by default.

ON-COMMIT is called with the text when it is sent; ON-ABORT with no
arguments when it is aborted.  Both run in the current buffer (the
one the input is for), after the input's window is gone.  Leading
blank lines and trailing whitespace are not part of the text.

Example:
  (approve-input-read :prompt \"Edit comment\" :target \\='(edit \"IC_1\")
                      :name \"edit IC_1\" :initial \"Old text\"
                      :on-commit (lambda (text) (message \"%s\" text)))"
  (let ((source (current-buffer)))
    (if-let* ((open (approve-input--find source target)))
        (progn (pop-to-buffer open approve-input-display-action) open)
      (let ((draft (and target (approve-input-draft target)))
            (window (selected-window))
            (buffer (generate-new-buffer (format "*approve-input: %s*" (or name "text")))))
        (with-current-buffer buffer
          (approve-input-mode)
          (setq approve-input--source source
                approve-input--source-window window
                approve-input--target target
                approve-input--initial initial
                approve-input--prompt prompt
                approve-input--context context
                approve-input--commit-label commit-label
                approve-input--allow-empty allow-empty
                approve-input--on-commit on-commit
                approve-input--on-abort on-abort
                approve-input--lines lines)
          (when single-line
            (approve-input-single-line-mode 1))
          (insert (or draft initial ""))
          (approve-input--highlight))
        (add-hook 'approve-pr-redraw-hook #'approve-input--rehighlight nil t)
        (pop-to-buffer buffer approve-input-display-action)
        (when draft
          (message "Approve: your draft is back"))
        buffer))))

;;; Sending and aborting

(defun approve-input-text ()
  "Return the text of the current input buffer as it would be sent.
That is without leading blank lines and trailing whitespace; in a
single-line input, the first line, trimmed."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (if approve-input-single-line-mode
        (string-trim (car (split-string text "\n")))
      (string-trim-right (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)+" "" text)))))

(defun approve-input--close ()
  "Close the current input buffer: its window, then the buffer itself."
  (let ((buffer (current-buffer))
        (source-window approve-input--source-window))
    (setq approve-input--done t)
    (approve-input--unhighlight)
    (when-let* ((window (get-buffer-window buffer)))
      (quit-restore-window window 'kill))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when (window-live-p source-window)
      (select-window source-window))))

(defun approve-input-commit ()
  "Send the text: hand it to the command that opened this buffer.
The text stays the draft of its target until GitHub has it."
  (interactive)
  (let ((text (approve-input-text))
        (source approve-input--source)
        (on-commit approve-input--on-commit))
    (when (and (string-empty-p text) (not approve-input--allow-empty))
      (user-error "There is nothing to send; %s aborts"
                  (or (approve-input--key #'approve-input-abort) "approve-input-abort")))
    (unless (buffer-live-p source)
      (user-error "The buffer this text was for is gone; copy the text from here"))
    (approve-input--keep-draft text)
    (approve-input--close)
    (when on-commit
      (with-current-buffer source
        (funcall on-commit text)))))

(defun approve-input-abort ()
  "Abort: close this buffer, keeping its text as a draft of its target.
Writing for the same target again starts from the draft."
  (interactive)
  (let ((kept (approve-input--keep-draft (approve-input-text)))
        (source approve-input--source)
        (on-abort approve-input--on-abort))
    (approve-input--close)
    (when kept
      (message "Approve: kept your text as a draft"))
    (when (and on-abort (buffer-live-p source))
      (with-current-buffer source
        (funcall on-abort)))))

(defun approve-input--killed ()
  "Keep the text of an input buffer killed without sending or aborting.
The highlight of its lines goes away in any case."
  (approve-input--unhighlight)
  (unless approve-input--done
    (setq approve-input--done t)
    (approve-input--keep-draft (approve-input-text))))

(provide 'approve-input)

;;; approve-input.el ends here
