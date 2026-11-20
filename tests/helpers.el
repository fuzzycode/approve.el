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
;; - Outside those, `approve-test-offline-transport' fails every
;;   request, so no test reaches GitHub.
;;
;; Load it with (require 'approve-test-helpers "helpers"); tests/ is
;; on the load path.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'approve-api)
(require 'approve-dev)
(require 'approve-pr)

(defvar approve-test--fixtures (make-hash-table :test #'equal)
  "Scenarios read so far, by name.")

(defun approve-test-offline-transport (request)
  "Fail REQUEST without touching the network, as no test may (D19).
It is the transport of every test that does not bind its own, so a
request no test answers, such as a prefetch, fails quietly instead
of reaching GitHub."
  (let ((timer (run-at-time 0 nil (plist-get request :on-failure)
                            (list :error '(error "No network in tests")))))
    (lambda () (cancel-timer timer))))

(setq approve-api-transport #'approve-test-offline-transport)

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
left in `approve-graphql-directory'.  Requests have no timeout,
progress messages are kept out of the test output, and relative
dates count from `approve-test-now'.  Wait for the
answers with `approve-test-wait-for' inside BODY."
  (declare (indent 1) (debug t))
  `(let ((approve-api-transport
          (approve-dev-make-replay-transport
           (mapcar #'approve-test-fixture (ensure-list ,names))))
         (approve-graphql-directory approve-test-graphql-directory)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (approve-render-now approve-test-now)
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

(defconst approve-test-now (encode-time (list 0 0 12 4 10 2026 nil nil t))
  "The time tests count relative dates from: 2026-10-04 12:00 UTC.
The fixtures' events happen between 2026-10-01 and 2026-10-03.")

(defun approve-test-pr-plist (name)
  "Return the PR of scenario NAME as `approve-parse-url' would."
  (let ((scenario (approve-test-fixture name)))
    (list :host "github.com" :owner (plist-get scenario :owner)
          :repo (plist-get scenario :repo) :number (plist-get scenario :number))))

(defun approve-test-open-pr (name &optional no-wait)
  "Return a PR buffer of scenario NAME, loaded through the fake API.
Run it inside `approve-test-with-fixture'.  Unless NO-WAIT, wait
until every query has settled and redraw.  Kill the buffer after."
  (let ((buffer (approve-pr-setup-buffer (approve-test-pr-plist name))))
    (with-current-buffer buffer
      (approve-pr-load)
      (unless no-wait
        (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
        (approve-pr-redraw)))
    buffer))

(defun approve-test-find-section (type &optional value)
  "Return the first section of TYPE (and VALUE, if given) in the buffer."
  (let (found)
    (magit-map-sections
     (lambda (section)
       (when (and (not found)
                  (eq (oref section type) type)
                  (or (null value) (equal (oref section value) value)))
         (setq found section))))
    found))

(defun approve-test-section-text (section)
  "Return the text of SECTION without properties."
  (buffer-substring-no-properties (oref section start) (oref section end)))

(defun approve-test-show-file-list ()
  "Show the file list in the current PR buffer, before the diff, and redraw.
It is not in `approve-pr-sections-hook' by default (decision D05,
amended); this adds it to the buffer's own value, as a user could."
  (approve-add-section-hook 'approve-pr-sections-hook #'approve-insert-files-section
                            #'approve-insert-diff-section nil t)
  (approve-pr-redraw))

(defun approve-test-fail-queries (transport names)
  "Return TRANSPORT, except that requests for NAMES fail with HTTP 502.
NAMES are GraphQL query files or REST resources."
  (lambda (request)
    (if (member (or (plist-get request :name) (plist-get request :resource)) names)
        (let ((timer (run-at-time 0 nil (plist-get request :on-failure)
                                  (list :status 502
                                        :payload '((message . "Bad Gateway"))))))
          (lambda () (cancel-timer timer)))
      (funcall transport request))))

(provide 'approve-test-helpers)

;;; helpers.el ends here
