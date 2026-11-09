;;; test-speed.el --- Soft checks of the speed budgets -*- lexical-binding: t; -*-

;;; Commentary:

;; The speed budgets of docs/architecture.org ("Speed"), measured with
;; the fixtures by `approve-dev-benchmark'.  They are soft checks: the
;; timings are printed, and a test fails only when a case takes more
;; than `test-speed-slack' times its budget, which catches a change
;; that makes drawing much slower without failing on a slow machine.
;; In CI, `make build' runs first, so the compiled code is measured;
;; uncompiled code is about four times slower.

;;; Code:

(require 'buttercup)
(require 'approve-dev)
(require 'approve-test-helpers "helpers")

(defconst test-speed-slack 6
  "How many times its budget a case may take before the test fails.")

(describe "The speed budgets"
  (it "hold, within a generous slack, for the fixtures"
    (let* ((approve-graphql-directory approve-test-graphql-directory)
           (results (let ((inhibit-message t)) (approve-dev-benchmark 3))))
      (dolist (result results)
        (princ (format "  %-14s %6.1f ms (budget %d ms)\n"
                       (nth 0 result) (nth 1 result) (nth 2 result)))
        (expect (nth 1 result) :to-be-less-than (* test-speed-slack (nth 2 result)))))))

;;; test-speed.el ends here
