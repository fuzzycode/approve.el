;;; test-approve-log.el --- Tests for approve-log -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;;; Commentary:

;; Buttercup tests for the logging utilities.

;;; Code:

(require 'buttercup)
(require 'approve-log)

(describe "approve-log--message"

  (it "logs a message when approve-debug is non-nil"
    (let ((approve-debug t))
      (spy-on 'message)
      (approve-log--message "test %s" "value")
      (expect 'message :to-have-been-called-with "[approve] test %s" "value")))

  (it "does not log when approve-debug is nil"
    (let ((approve-debug nil))
      (spy-on 'message)
      (approve-log--message "test %s" "value")
      (expect 'message :not :to-have-been-called)))

  (it "handles messages without format arguments"
    (let ((approve-debug t))
      (spy-on 'message)
      (approve-log--message "simple message")
      (expect 'message :to-have-been-called-with "[approve] simple message"))))

;;; test-approve-log.el ends here
