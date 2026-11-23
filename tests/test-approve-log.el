;;; test-approve-log.el --- Tests for approve-log -*- lexical-binding: t; -*-

;;; Commentary:

;; The log buffer: levels, trimming and redaction.

;;; Code:

(require 'buttercup)
(require 'approve-log)

(defun test-approve-log--text ()
  "Return the text of the log buffer, or nil if there is none."
  (when-let* ((buffer (get-buffer approve-log-buffer-name)))
    (with-current-buffer buffer
      (buffer-string))))

(describe "approve-log"
  (before-each
    (when-let* ((buffer (get-buffer approve-log-buffer-name)))
      (kill-buffer buffer)))

  (it "writes entries at or above approve-log-level"
    (let ((approve-log-level 'warn))
      (approve-log 'warn "careful %d" 1)
      (approve-log 'error "broken")
      (expect (test-approve-log--text) :to-match "WARN  careful 1")
      (expect (test-approve-log--text) :to-match "ERROR broken")))

  (it "drops entries below approve-log-level without formatting them"
    (let ((approve-log-level 'warn))
      (approve-log 'debug "%d" "would fail if formatted")
      (approve-log 'info "noise")
      (expect (test-approve-log--text) :to-be nil)))

  (it "logs everything at level debug"
    (let ((approve-log-level 'debug))
      (approve-log 'debug "request %s" "core")
      (expect (test-approve-log--text) :to-match "DEBUG request core")))

  (it "signals an error for an unknown level"
    (expect (approve-log 'loud "x") :to-throw 'error))

  (it "keeps at most approve-log-max-lines lines"
    (let ((approve-log-level 'debug)
          (approve-log-max-lines 3))
      (dotimes (i 5)
        (approve-log 'info "line %d" i))
      (let ((text (test-approve-log--text)))
        (expect (length (split-string text "\n" t)) :to-equal 3)
        (expect text :not :to-match "line 1")
        (expect text :to-match "line 4"))))

  (it "redacts tokens before writing"
    (let ((approve-log-level 'debug))
      (approve-log 'debug "headers %S"
                   '(("Authorization" . "token ghp_0123456789abcdefghijABCDEFGHIJ")))
      (expect (test-approve-log--text) :not :to-match "ghp_")
      (expect (test-approve-log--text) :to-match "REDACTED")))

  (it "logs a caught condition with its message and data"
    (let ((approve-log-level 'error))
      (condition-case err
          (signal 'approve-error '("it broke" 42))
        (approve-error (approve-log-condition "redraw" err)))
      (expect (test-approve-log--text)
              :to-match "ERROR redraw: Approve error: \"it broke\", 42")
      (expect (test-approve-log--text) :to-match "(approve-error \"it broke\" 42)")))

  (it "makes a read-only buffer in approve-log-mode"
    (let ((approve-log-level 'error))
      (approve-log 'error "x")
      (with-current-buffer approve-log-buffer-name
        (expect major-mode :to-be 'approve-log-mode)
        (expect buffer-read-only :to-be-truthy))))

  (it "makes its buffer in the home directory, whatever buffer logs first"
    (let ((approve-log-level 'error))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory temporary-file-directory))
        (approve-log 'error "x"))
      (expect (buffer-local-value 'default-directory (get-buffer approve-log-buffer-name))
              :to-equal (expand-file-name "~/")))))

(describe "approve-log-redact"
  (it "redacts the secret of an Authorization header in any notation"
    (dolist (case '(("Authorization: token abc123" . "Authorization: token [REDACTED]")
                    ("authorization: Bearer abc123" . "authorization: Bearer [REDACTED]")
                    ("(\"Authorization\" . \"token abc123\")"
                     . "(\"Authorization\" . \"token [REDACTED]\")")
                    ("(Authorization . \"abc123\")"
                     . "(Authorization . \"[REDACTED]\")")))
      (expect (approve-log-redact (car case)) :to-equal (cdr case))))

  (it "redacts GitHub tokens anywhere"
    (expect (approve-log-redact "x gho_0123456789abcdefghijKLMN y")
            :to-equal "x [REDACTED] y")
    (expect (approve-log-redact "github_pat_11AAAA_0123456789abcdefghij")
            :to-equal "[REDACTED]"))

  (it "leaves other text alone"
    (expect (approve-log-redact "POST /graphql cost 1, 4999 left")
            :to-equal "POST /graphql cost 1, 4999 left")))

(describe "approve-error"
  (it "is an error"
    (expect (memq 'error (get 'approve-error 'error-conditions)) :to-be-truthy)))

;;; test-approve-log.el ends here
