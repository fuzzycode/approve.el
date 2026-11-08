;;; helpers.el --- Shared helpers for Approve's tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Fixtures and the fake API for tests (tests/fixtures/README.org):
;;
;;   (approve-test-with-fixture "medium"
;;     (approve-api-graphql "queries/pull-request.graphql"
;;                          (approve-test-fixture-variables "medium")
;;                          :callback (lambda (data) (setq loaded data)))
;;     (approve-test-wait-for (lambda () loaded)))
;;
;; - `approve-test-fixture' reads a scenario once per test run.
;; - `approve-test-with-fixture' answers Approve's requests from
;;   scenarios through `approve-dev-make-replay-transport'.
;; - `approve-test-wait-for' runs timers until async callbacks are done.
;; - `approve-test-fixture-data' gives a GraphQL response's data
;;   directly, for tests that load the store without the API.
;;
;; Load it with (require 'approve-test-helpers "helpers"); tests/ is
;; on the load path.

;;; Code:

(require 'cl-lib)
(require 'approve-api)
(require 'approve-dev)

(defvar approve-test--fixtures (make-hash-table :test #'equal)
  "Scenarios read so far, by name.")

(defconst approve-test-graphql-directory
  (expand-file-name "../graphql"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The graphql/ directory of the source tree, whose queries tests send.")

(defun approve-test-fixture (name)
  "Return the scenario NAME from tests/fixtures/, read once per run.
NAME is \"small\", \"medium\" or \"large\"; the value is the plist of
`approve-dev-read-scenario'.  Do not change it: later tests share it."
  (or (gethash name approve-test--fixtures)
      (puthash name (approve-dev-read-scenario name) approve-test--fixtures)))

(defun approve-test-fixture-meta (name)
  "Return the meta plist of scenario NAME, for `approve-model-init'."
  (approve-dev-scenario-meta (approve-test-fixture name)))

(defun approve-test-fixture-variables (name)
  "Return the GraphQL variables of scenario NAME's pull request."
  (let ((scenario (approve-test-fixture name)))
    `((owner . ,(plist-get scenario :owner))
      (name . ,(plist-get scenario :repo))
      (number . ,(plist-get scenario :number)))))

(defun approve-test-fixture-data (name query &optional page)
  "Return the data of scenario NAME's response to QUERY, page PAGE.
QUERY is a file name such as \"queries/pull-request.graphql\"; PAGE
counts from 1, the default.  Signal an error if there is none."
  (let ((bodies (cl-loop for exchange in (plist-get (approve-test-fixture name)
                                                    :exchanges)
                         for key = (plist-get exchange :request)
                         when (and (eq (plist-get key :kind) 'graphql)
                                   (equal (plist-get key :name) query))
                         collect (plist-get exchange :body))))
    (alist-get 'data (or (nth (1- (or page 1)) bodies)
                         (error "Scenario %s has no page %s of %s"
                                name (or page 1) query)))))

(defmacro approve-test-with-fixture (names &rest body)
  "Run BODY with Approve's requests answered from the scenarios NAMES.
NAMES is a scenario name or a list of them.  `approve-api-transport'
replays them without latency; a request they do not answer fails
with `approve-api-not-found'.  Queries come from the source tree's
graphql/ (`approve-test-graphql-directory'), whatever another test
left in `approve-graphql-directory'.  Requests have no timeout, and
progress messages are kept out of the test output.  Wait for the
answers with `approve-test-wait-for' inside BODY."
  (declare (indent 1) (debug t))
  `(let ((approve-api-transport
          (approve-dev-make-replay-transport
           (mapcar #'approve-test-fixture (ensure-list ,names))))
         (approve-graphql-directory approve-test-graphql-directory)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (inhibit-message t))
     ,@body))

(defun approve-test-wait-for (predicate &optional timeout)
  "Run timers and process output until PREDICATE returns non-nil.
Return its value.  Signal an error after TIMEOUT seconds (default 5),
so a callback that never comes fails the test instead of hanging it."
  (let ((deadline (+ (float-time) (or timeout 5)))
        value)
    (while (not (setq value (funcall predicate)))
      (when (> (float-time) deadline)
        (error "Timed out after %s seconds waiting for %S"
               (or timeout 5) predicate))
      (accept-process-output nil 0.01))
    value))

(provide 'approve-test-helpers)

;;; helpers.el ends here
