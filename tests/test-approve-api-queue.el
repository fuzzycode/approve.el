;;; test-approve-api-queue.el --- Tests for approve-api-queue -*- lexical-binding: t; -*-

;;; Commentary:

;; The mutation queue through a fake transport: order, one at a time,
;; queries passing the queue, failures, pausing on a rate limit,
;; errors in callbacks and clearing.

;;; Code:

(require 'buttercup)
(require 'approve-api-queue)
(require 'approve-test-helpers "helpers")

(defvar test-approve-api-queue--sent nil
  "Request plists the fake transport received, newest first.")

(defun test-approve-api-queue--transport (request)
  "Fake `approve-api-transport': remember REQUEST, answer nothing yet."
  (push request test-approve-api-queue--sent)
  nil)

(defun test-approve-api-queue--names ()
  "Return the query texts sent so far, oldest first."
  (reverse (mapcar (lambda (request) (plist-get request :query))
                   test-approve-api-queue--sent)))

(defun test-approve-api-queue--answer (&optional failure)
  "Answer the newest request: with data, or fail it with FAILURE."
  (let ((request (car test-approve-api-queue--sent)))
    (if failure
        (funcall (plist-get request :on-failure) failure)
      (funcall (plist-get request :on-success)
               `((data . ((query . ,(plist-get request :query)))))
               nil))))

(defmacro test-approve-api-queue--with-fake (&rest body)
  "Run BODY with the fake transport and an empty queue."
  (declare (indent 0))
  `(let ((approve-api-transport #'test-approve-api-queue--transport)
         (approve-api-timeout nil)
         (approve-api--in-flight nil)
         (approve-api--rate-limits (make-hash-table :test 'equal))
         (approve-api-queue--waiting nil)
         (approve-api-queue--running nil)
         (approve-api-queue--timer nil)
         (test-approve-api-queue--sent nil))
     (unwind-protect
         (progn ,@body)
       (when approve-api-queue--timer
         (cancel-timer approve-api-queue--timer)))))

(describe "approve-api-mutate"
  (before-each
    (spy-on 'message :and-call-fake
            (lambda (format-string &rest args)
              (and format-string (apply #'format-message format-string args)))))

  (it "runs mutations one at a time, oldest first"
    (test-approve-api-queue--with-fake
      (let ((done nil))
        (dolist (name '("m1" "m2" "m3"))
          (approve-api-mutate name nil :callback (lambda (data) (push (alist-get 'query data) done))))
        (expect (test-approve-api-queue--names) :to-equal '("m1"))
        (expect (approve-api-queue-length) :to-equal 3)
        (test-approve-api-queue--answer)
        (expect (test-approve-api-queue--names) :to-equal '("m1" "m2"))
        (test-approve-api-queue--answer)
        (test-approve-api-queue--answer)
        (expect (test-approve-api-queue--names) :to-equal '("m1" "m2" "m3"))
        (expect (reverse done) :to-equal '("m1" "m2" "m3"))
        (expect (approve-api-queue-length) :to-equal 0))))

  (it "lets queries pass while a mutation is in flight"
    (test-approve-api-queue--with-fake
      (approve-api-mutate "m1" nil)
      (approve-api-mutate "m2" nil)
      (approve-api-graphql "q1" nil)
      (expect (test-approve-api-queue--names) :to-equal '("m1" "q1"))))

  (it "reports a failed mutation to its errorback and goes on with the next"
    (test-approve-api-queue--with-fake
      (let (received)
        (approve-api-mutate "m1" nil :errorback (lambda (err) (setq received err)))
        (approve-api-mutate "m2" nil)
        (test-approve-api-queue--answer '(:status 502))
        (expect (car received) :to-be 'approve-api-http-error)
        (expect (test-approve-api-queue--names) :to-equal '("m1" "m2")))))

  (it "goes on with the next when a callback signals"
    (test-approve-api-queue--with-fake
      (approve-api-mutate "m1" nil :callback (lambda (_) (error "Bug")))
      (approve-api-mutate "m2" nil)
      (let ((debug-on-error nil))
        (test-approve-api-queue--answer))
      (expect (test-approve-api-queue--names) :to-equal '("m1" "m2"))))

  (it "pauses on a rate limit until the reset, then resumes"
    (test-approve-api-queue--with-fake
      (let ((received nil)
            (reset (+ (time-convert nil 'integer) 3600)))
        (approve-api-mutate "m1" nil :errorback (lambda (err) (setq received err)))
        (approve-api-mutate "m2" nil)
        (test-approve-api-queue--answer
         `(:status 403 :headers (("x-ratelimit-remaining" . "0")
                                 ("x-ratelimit-reset" . ,(number-to-string reset)))))
        (expect (car received) :to-be 'approve-api-rate-limited)
        (expect (test-approve-api-queue--names) :to-equal '("m1"))
        (expect (approve-api-queue-length) :to-equal 1)
        (expect (- (timer-until approve-api-queue--timer nil))
                :to-be-close-to 3600 -1)
        (approve-api-mutate "m3" nil)
        (expect (test-approve-api-queue--names) :to-equal '("m1"))
        (approve-api-queue-resume)
        (expect (test-approve-api-queue--names) :to-equal '("m1" "m2"))
        (test-approve-api-queue--answer)
        (expect (test-approve-api-queue--names) :to-equal '("m1" "m2" "m3")))))

  (it "resumes by itself when the pause ends"
    (test-approve-api-queue--with-fake
      (approve-api-mutate "m1" nil :errorback #'ignore)
      (approve-api-mutate "m2" nil)
      (test-approve-api-queue--answer '(:status 429))
      (expect (test-approve-api-queue--names) :to-equal '("m1"))
      ;; Bring the end of the pause (a minute away) forward.
      (let ((timer approve-api-queue--timer))
        (cancel-timer timer)
        (timer-set-time timer (current-time))
        (timer-activate timer))
      (approve-test-wait-for (lambda () (null approve-api-queue--timer)))
      (expect (test-approve-api-queue--names) :to-equal '("m1" "m2"))))

  (it "clears waiting mutations, cancelling them, while the running one finishes"
    (test-approve-api-queue--with-fake
      (let ((cancelled nil) (finished nil))
        (approve-api-mutate "m1" nil :callback (lambda (_) (setq finished t)))
        (approve-api-mutate "m2" nil :errorback (lambda (err) (push err cancelled)))
        (approve-api-mutate "m3" nil :errorback (lambda (err) (push err cancelled)))
        (approve-api-queue-clear)
        (expect (mapcar #'car cancelled)
                :to-equal '(approve-api-cancelled approve-api-cancelled))
        (expect (approve-api-queue-length) :to-equal 1)
        (test-approve-api-queue--answer)
        (expect finished :to-be t)
        (expect (test-approve-api-queue--names) :to-equal '("m1"))
        (expect (approve-api-queue-length) :to-equal 0)))))

;;; test-approve-api-queue.el ends here
