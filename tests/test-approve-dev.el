;;; test-approve-dev.el --- Tests for approve-dev -*- lexical-binding: t; -*-

;;; Commentary:

;; The fixture tools the tests rely on: scenario files read back as
;; ghub decodes JSON, the replay transport, the recording transport,
;; scrubbing, offline mode and the recorder.

;;; Code:

(require 'buttercup)
(require 'approve-dev)

(defun test-approve-dev--wait (predicate)
  "Run timers until PREDICATE returns non-nil, at most two seconds."
  (let ((deadline (+ (float-time) 2)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defconst test-approve-dev--scenario
  (list :name "tiny" :description "A tiny scenario"
        :host "github.com" :owner "acme" :repo "rocket" :number 7
        :viewer "vera"
        :exchanges
        (list (list :request '(:kind graphql :name "queries/pull-request.graphql"
                                     :variables ((owner . "acme") (name . "rocket")
                                                 (number . 7)))
                    :status 200
                    :headers '(("X-RateLimit-Remaining" . "4999"))
                    :body '((data (repository (pullRequest (__typename . "PullRequest")
                                                           (id . "PR_kw1")
                                                           (title . "Fix \"quotes\"\n\ttabs é")
                                                           (isDraft . :false)
                                                           (locked . t)
                                                           (labels (nodes . []))
                                                           (milestone . nil)
                                                           (progress . 50.5)))
                                  (rateLimit (cost . 1)))))
              (list :request '(:kind rest :method "GET"
                                     :resource "/repos/acme/rocket/pulls/7/files"
                                     :params ((per_page . 100) (page . 1)))
                    :status 200
                    :headers nil
                    :body '(((filename . "a.go") (patch . "@@ -1 +1 @@\n-a\n+b"))
                            ((filename . "b.png"))))
              (list :request '(:kind graphql :name "queries/pull-request-files.graphql"
                                     :variables ((owner . "acme") (name . "rocket")
                                                 (number . 404)))
                    :status 404
                    :headers nil
                    :body '((message . "Not Found")))))
  "A scenario written to a temporary file by the tests.")

(defmacro test-approve-dev--with-file (var &rest body)
  "Run BODY with VAR bound to a temporary scenario file name."
  (declare (indent 1))
  `(let ((,var (make-temp-file "approve-dev-" nil ".json")))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defun test-approve-dev--quiet ()
  "Keep progress messages out of the test output."
  (spy-on 'message))

(describe "approve-dev scenario files"
  (it "read back as ghub decodes JSON"
    (test-approve-dev--with-file file
      (approve-dev-write-scenario test-approve-dev--scenario file)
      (let* ((scenario (approve-dev-read-scenario file))
             (core (car (plist-get scenario :exchanges))))
        (expect (plist-get scenario :name) :to-equal "tiny")
        (expect (plist-get scenario :owner) :to-equal "acme")
        (expect (plist-get scenario :number) :to-equal 7)
        (expect (plist-get scenario :viewer) :to-equal "vera")
        (expect (plist-get core :request)
                :to-equal '(:kind graphql :name "queries/pull-request.graphql"
                                  :variables ((owner . "acme") (name . "rocket")
                                              (number . 7))))
        (expect (plist-get core :headers)
                :to-equal '(("X-RateLimit-Remaining" . "4999")))
        (expect (plist-get core :body)
                :to-equal '((data (repository (pullRequest (__typename . "PullRequest")
                                                           (id . "PR_kw1")
                                                           (title . "Fix \"quotes\"\n\ttabs é")
                                                           (isDraft)
                                                           (locked . t)
                                                           (labels (nodes))
                                                           (milestone)
                                                           (progress . 50.5)))
                                  (rateLimit (cost . 1)))))
        (expect (plist-get (nth 1 (plist-get scenario :exchanges)) :body)
                :to-equal '(((filename . "a.go") (patch . "@@ -1 +1 @@\n-a\n+b"))
                            ((filename . "b.png")))))))

  (it "write false, empty arrays and null as JSON does"
    (test-approve-dev--with-file file
      (approve-dev-write-scenario test-approve-dev--scenario file)
      (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (expect text :to-match "\"isDraft\": false")
        (expect text :to-match "\"nodes\": \\[\\]")
        (expect text :to-match "\"milestone\": null")
        (expect text :to-match "\"title\": \"Fix \\\\\"quotes\\\\\"\\\\n\\\\ttabs é\""))))

  (it "rejects a file of another format"
    (test-approve-dev--with-file file
      (with-temp-file file (insert "{\"approveFixture\": 99}"))
      (expect (approve-dev-read-scenario file) :to-throw 'approve-dev-error))))

(describe "approve-dev-make-replay-transport"
  (before-each (test-approve-dev--quiet))

  (it "answers a GraphQL request later, whatever the order of its variables"
    (let ((approve-api-transport
           (approve-dev-make-replay-transport (list test-approve-dev--scenario)))
          (approve-api-timeout nil)
          received)
      (approve-api-graphql "queries/pull-request.graphql"
                           '((number . 7) (owner . "acme") (name . "rocket"))
                           :callback (lambda (data) (setq received data)))
      (expect received :to-be nil)
      (expect (test-approve-dev--wait (lambda () received)) :to-be-truthy)
      (expect (let-alist received .repository.pullRequest.id) :to-equal "PR_kw1")))

  (it "answers a REST request whatever the order and type of its parameters"
    (let ((approve-api-transport
           (approve-dev-make-replay-transport (list test-approve-dev--scenario)))
          (approve-api-timeout nil)
          received)
      (approve-api-rest "GET" "/repos/acme/rocket/pulls/7/files"
                        :params '((page . "1") (per_page . 100))
                        :callback (lambda (files) (setq received files)))
      (test-approve-dev--wait (lambda () received))
      (expect (mapcar (lambda (file) (alist-get 'filename file)) received)
              :to-equal '("a.go" "b.png"))))

  (it "answers the same request every time it is made"
    (let ((approve-api-transport
           (approve-dev-make-replay-transport (list test-approve-dev--scenario)))
          (approve-api-timeout nil)
          (count 0))
      (dotimes (_ 2)
        (approve-api-graphql "queries/pull-request.graphql"
                             '((owner . "acme") (name . "rocket") (number . 7))
                             :callback (lambda (_) (setq count (1+ count)))))
      (test-approve-dev--wait (lambda () (= count 2)))
      (expect count :to-equal 2)))

  (it "fails a request no exchange matches as not found, and logs it"
    (let ((approve-api-transport
           (approve-dev-make-replay-transport (list test-approve-dev--scenario)))
          (approve-api-timeout nil)
          (approve-log-level 'warn)
          failed)
      (approve-api-graphql "queries/pull-request.graphql"
                           '((owner . "acme") (name . "rocket") (number . 8))
                           :errorback (lambda (err) (setq failed err)))
      (test-approve-dev--wait (lambda () failed))
      (expect (car failed) :to-be 'approve-api-not-found)
      (expect (approve-api-error-message failed) :to-match "no fixture for graphql")
      (expect (with-current-buffer approve-log-buffer-name (buffer-string))
              :to-match "no fixture for graphql queries/pull-request.graphql")))

  (it "replays a recorded failure with its status"
    (let ((approve-api-transport
           (approve-dev-make-replay-transport (list test-approve-dev--scenario)))
          (approve-api-timeout nil)
          failed)
      (approve-api-graphql "queries/pull-request-files.graphql"
                           '((owner . "acme") (name . "rocket") (number . 404))
                           :errorback (lambda (err) (setq failed err)))
      (test-approve-dev--wait (lambda () failed))
      (expect (car failed) :to-be 'approve-api-not-found)
      (expect (approve-api-error-message failed) :to-equal "Not Found")))

  (it "does not answer an aborted request"
    (let* ((transport (approve-dev-make-replay-transport
                       (list test-approve-dev--scenario)))
           answered
           (abort (funcall transport
                           (list :kind 'graphql :name "queries/pull-request.graphql"
                                 :variables '((owner . "acme") (name . "rocket")
                                              (number . 7))
                                 :on-success (lambda (&rest _) (setq answered t))
                                 :on-failure (lambda (&rest _) (setq answered t))))))
      (funcall abort)
      (sleep-for 0.05)
      (expect answered :to-be nil))))

(describe "approve-dev-make-recording-transport"
  (it "records each exchange with only the headers that matter"
    (let* ((recorded nil)
           (inner (lambda (request)
                    (run-at-time 0 nil (plist-get request :on-success)
                                 '((data (viewer (login . "vera"))))
                                 '(("Authorization" . "token ghp_abcdefghijklmnopqrstuvwxyz")
                                   ("Set-Cookie" . "secret")
                                   ("X-RateLimit-Remaining" . "4998")
                                   ("Link" . "<https://x?page=2>; rel=\"next\"")))
                    nil))
           (approve-api-transport
            (approve-dev-make-recording-transport
             inner (lambda (exchange) (push exchange recorded))))
           (approve-api-timeout nil)
           received)
      (spy-on 'message)
      (approve-api-graphql "queries/pull-request.graphql"
                           '((owner . "acme") (name . "rocket") (number . 7))
                           :callback (lambda (data) (setq received data)))
      (test-approve-dev--wait (lambda () received))
      (expect received :to-equal '((viewer (login . "vera"))))
      (expect recorded
              :to-equal
              '((:request (:kind graphql :name "queries/pull-request.graphql"
                                 :variables ((owner . "acme") (name . "rocket")
                                             (number . 7)))
                          :status 200
                          :headers (("X-RateLimit-Remaining" . "4998")
                                    ("Link" . "<https://x?page=2>; rel=\"next\""))
                          :body ((data (viewer (login . "vera")))))))))

  (it "records HTTP failures but not network errors"
    (let* ((recorded nil)
           (failures (list '(:status 404 :payload ((message . "Not Found")))
                           '(:error (error connection-failed))))
           (inner (lambda (request)
                    (run-at-time 0 nil (plist-get request :on-failure)
                                 (pop failures))
                    nil))
           (approve-api-transport
            (approve-dev-make-recording-transport
             inner (lambda (exchange) (push exchange recorded))))
           (approve-api-timeout nil)
           (errors 0))
      (spy-on 'message)
      (dotimes (_ 2)
        (approve-api-rest "GET" "/x" :errorback (lambda (_) (setq errors (1+ errors)))))
      (test-approve-dev--wait (lambda () (= errors 2)))
      (expect (length recorded) :to-equal 1)
      (expect (plist-get (car recorded) :status) :to-equal 404))))

(describe "approve-dev-scrub-people"
  (it "replaces people's logins, names and emails with stable fakes"
    (let* ((scenario
            (list :name "s" :owner "acme" :viewer "vera"
                  :exchanges
                  (list (list :request nil :status 200
                              :body '((data
                                       (author (__typename . "User") (id . "U_1")
                                               (login . "alice") (url . "https://github.com/alice")
                                               (name . "Alice A"))
                                       (bot (__typename . "Bot") (login . "ci")
                                            (url . "https://github.com/apps/ci"))
                                       (owner (__typename . "User") (login . "acme"))
                                       (commit (author (name . "Alice A")
                                                       (email . "alice@a.org")
                                                       (user (__typename . "User")
                                                             (login . "alice")
                                                             (name . "Alice A")))))
                                      (viewer (__typename . "User") (login . "vera")
                                              (name . nil)))))))
           (scrubbed (approve-dev-scrub-people scenario)))
      (expect (plist-get scrubbed :viewer) :to-equal "user-1")
      (expect (plist-get (car (plist-get scrubbed :exchanges)) :body)
              :to-equal
              '((data
                 (author (__typename . "User") (id . "U_1")
                         (login . "user-2") (url . "https://github.com/user-2")
                         (name . "User 2"))
                 (bot (__typename . "Bot") (login . "ci")
                      (url . "https://github.com/apps/ci"))
                 (owner (__typename . "User") (login . "acme"))
                 (commit (author (name . "Person 1")
                                 (email . "person-1@example.com")
                                 (user (__typename . "User")
                                       (login . "user-2")
                                       (name . "User 2")))))
                (viewer (__typename . "User") (login . "user-1") (name))))
      (expect (plist-get (car (plist-get scenario :exchanges)) :body)
              :not :to-equal (plist-get (car (plist-get scrubbed :exchanges)) :body)))))

(describe "approve-dev-offline-mode"
  (it "serves the fixtures and restores the transport it replaced"
    (let* ((directory (make-temp-file "approve-dev-fixtures-" t))
           (approve-dev-fixtures-directory directory)
           (approve-dev-offline-latency 0)
           (approve-api-timeout nil)
           (before (lambda (_request) nil))
           (approve-api-transport before)
           received)
      (spy-on 'message)
      (unwind-protect
          (progn
            (approve-dev-write-scenario test-approve-dev--scenario
                                        (approve-dev-scenario-file "tiny" directory))
            (approve-dev-offline-mode 1)
            (expect approve-api-transport :not :to-be before)
            (approve-api-graphql "queries/pull-request.graphql"
                                 '((owner . "acme") (name . "rocket") (number . 7))
                                 :callback (lambda (data) (setq received data)))
            (test-approve-dev--wait (lambda () received))
            (expect received :to-be-truthy)
            (approve-dev-offline-mode 1)
            (approve-dev-offline-mode -1)
            (expect approve-api-transport :to-be before))
        (approve-dev-offline-mode -1)
        (delete-directory directory t)))))

(describe "approve-dev-record"
  (it "records the loading plan through the installed transport and writes it"
    (let* ((directory (make-temp-file "approve-dev-record-" t))
           (requests nil)
           (inner (lambda (request)
                    (push (approve-dev-request-key request) requests)
                    (run-at-time
                     0 nil (plist-get request :on-success)
                     (if (eq (plist-get request :kind) 'rest)
                         '(((filename . "a.go")))
                       '((data (repository (pullRequest (__typename . "PullRequest")
                                                        (id . "PR_1")))
                               (viewer (__typename . "User") (login . "vera")))))
                     '(("X-RateLimit-Remaining" . "4990")))
                    nil))
           (approve-api-transport inner)
           (approve-api-timeout nil)
           written)
      (spy-on 'message)
      (unwind-protect
          (progn
            (approve-dev-record "https://github.com/acme/rocket/pull/7" "rec"
                                :scrub nil :directory directory
                                :callback (lambda (file) (setq written file)))
            (expect approve-api-transport :not :to-be inner)
            (test-approve-dev--wait (lambda () written))
            (expect approve-api-transport :to-be inner)
            (let ((scenario (approve-dev-read-scenario written)))
              (expect (plist-get scenario :owner) :to-equal "acme")
              (expect (plist-get scenario :viewer) :to-equal "vera")
              (expect (mapcar (lambda (exchange)
                                (let ((key (plist-get exchange :request)))
                                  (or (plist-get key :name) (plist-get key :resource))))
                              (plist-get scenario :exchanges))
                      :to-equal '("queries/pull-request.graphql"
                                  "queries/pull-request-files.graphql"
                                  "queries/pull-request-threads.graphql"
                                  "queries/pull-request-timeline.graphql"
                                  "queries/pull-request-commits.graphql"
                                  "queries/pull-request-checks.graphql"
                                  "/repos/acme/rocket/pulls/7/files"))
              (expect (length requests) :to-equal 7)))
        (delete-directory directory t)))))

;;; test-approve-dev.el ends here
