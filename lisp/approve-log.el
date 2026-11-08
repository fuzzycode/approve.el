;;; approve-log.el --- Debug log for Approve -*- lexical-binding: t; -*-

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

;; The *approve-log* buffer: what Approve did, for debugging.  Each
;; entry has a time, a level and a message.  Entries below
;; `approve-log-level' are dropped before they are formatted, so
;; logging at `debug' costs almost nothing when it is off.
;;
;;   (approve-log 'debug "GraphQL %s took %d ms" name ms)
;;
;; Messages are redacted before they are written: tokens and
;; Authorization headers never reach the buffer (see
;; `approve-log-redact').
;;
;; This file is the bottom of the transport layer: it requires nothing
;; from Approve, and every other module may require it.  It also
;; defines `approve-error', the parent of every Approve error type,
;; because it is the one file all modules load first.

;;; Code:

(define-error 'approve-error "Approve error")

(defgroup approve-log nil
  "Debug log of Approve."
  :group 'approve
  :prefix "approve-log-")

(defconst approve-log-levels '(debug info warn error)
  "Log levels, from the most to the least verbose.")

(defcustom approve-log-level 'warn
  "Least severe level that is written to the *approve-log* buffer.
Entries of this level and more severe ones are kept; less severe
ones are dropped.  Set it to `debug' to log every request with its
cost and timing, for example:

  (setq approve-log-level \\='debug)"
  :type '(choice (const :tag "Everything (debug)" debug)
                 (const :tag "Info and up" info)
                 (const :tag "Warnings and errors" warn)
                 (const :tag "Errors only" error))
  :group 'approve-log)

(defcustom approve-log-max-lines 5000
  "Maximum number of lines kept in the *approve-log* buffer.
The oldest lines are deleted when the buffer grows past this.
For example, keep more history while debugging:

  (setq approve-log-max-lines 20000)"
  :type 'natnum
  :group 'approve-log)

(defconst approve-log-buffer-name "*approve-log*"
  "Name of the buffer that holds the log.")

;;; Redaction

(defconst approve-log--redactions
  (list
   ;; Authorization headers in any notation: "Authorization: token X",
   ;; ("Authorization" . "Bearer X"), (Authorization . "X").
   (cons (rx (group "authorization" (? "\"") (* blank) (in ":.=")
                    (* blank) (? "\"")
                    (? (or "token" "bearer" "basic") (+ blank)))
             (+ (not (in "\"' \t\n)"))))
         "\\1[REDACTED]")
   ;; GitHub tokens: classic (ghp_, gho_, ghu_, ghs_, ghr_) and fine-grained.
   (cons (rx (or (seq "gh" (in "pousr") "_" (>= 20 alnum))
                 (seq "github_pat_" (>= 20 (in alnum "_")))))
         "[REDACTED]"))
  "Pairs of (REGEXP . REPLACEMENT) applied by `approve-log-redact'.")

(defun approve-log-redact (string)
  "Return STRING with tokens and Authorization headers replaced.
The secret part becomes \"[REDACTED]\"; the rest stays readable.

Example:
  (approve-log-redact \"Authorization: token ghp_abc...\")
  => \"Authorization: token [REDACTED]\""
  (let ((case-fold-search t))
    (dolist (redaction approve-log--redactions string)
      (setq string (replace-regexp-in-string
                    (car redaction) (cdr redaction) string t)))))

;;; The log buffer

(define-derived-mode approve-log-mode special-mode "Approve-Log"
  "Major mode of the *approve-log* buffer."
  (setq-local truncate-lines t))

(defun approve-log-enabled-p (level)
  "Return non-nil if entries of LEVEL are written to the log."
  (<= (length (memq level approve-log-levels))
      (length (memq approve-log-level approve-log-levels))))

(defun approve-log--buffer ()
  "Return the log buffer, creating it if needed."
  (or (get-buffer approve-log-buffer-name)
      (with-current-buffer (get-buffer-create approve-log-buffer-name)
        (approve-log-mode)
        (current-buffer))))

(defun approve-log--trim ()
  "Delete the oldest lines past `approve-log-max-lines'."
  (let ((excess (- (count-lines (point-min) (point-max))
                   approve-log-max-lines)))
    (when (> excess 0)
      (goto-char (point-min))
      (forward-line excess)
      (delete-region (point-min) (point)))))

(defun approve-log (level format-string &rest args)
  "Write a message at LEVEL to the *approve-log* buffer.
LEVEL is one of `approve-log-levels'.  FORMAT-STRING and ARGS are
as for `format-message'.  Nothing is formatted when LEVEL is below
`approve-log-level'.  The message is redacted (`approve-log-redact')
before it is written.  Return nil.

Example:
  (approve-log \\='info \"Loaded %d files\" 42)"
  (unless (memq level approve-log-levels)
    (error "Unknown log level: %S" level))
  (when (approve-log-enabled-p level)
    (let ((text (approve-log-redact (apply #'format-message format-string args)))
          (stamp (format-time-string "%H:%M:%S.%3N")))
      (with-current-buffer (approve-log--buffer)
        (let ((inhibit-read-only t)
              (at-end (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert (format "%s %-5s %s\n" stamp (upcase (symbol-name level))
                            text))
            (approve-log--trim))
          (when at-end
            (goto-char (point-max)))))))
  nil)

(defun approve-log-condition (context condition)
  "Log CONDITION, an error caught in CONTEXT, at level `error'.
CONTEXT is a short string saying what Approve was doing.  CONDITION is
the error as bound by `condition-case'.  The entry holds the error's
message and its data, so a bug can be reported from the log.

Example:
  (condition-case err (do-something)
    (error (approve-log-condition \"redraw\" err)))"
  (approve-log 'error "%s: %s %S" context
               (error-message-string condition) condition))

;;;###autoload
(defun approve-log-show ()
  "Show the *approve-log* buffer."
  (interactive)
  (display-buffer (approve-log--buffer)))

(provide 'approve-log)

;;; approve-log.el ends here
