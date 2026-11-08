;;; approve-api-queue.el --- Serialized GitHub mutations for Approve -*- lexical-binding: t; -*-

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

;; Mutations go through one first-in, first-out queue and run one at a
;; time (docs/decisions.org, D15): GitHub's secondary rate limits
;; punish bursts of mutations, and order matters (create the review,
;; then add threads to it).  Queries never wait for the queue.
;;
;;   (approve-api-mutate "mutations/x.graphql" vars :callback ...)
;;
;; When a mutation hits a rate limit, it is reported to its errorback
;; (not retried) and the queue pauses until the limit resets.
;;
;; This file belongs to the transport layer with approve-api.el, whose
;; requests it runs through approve-api's internal request functions;
;; like it, it knows nothing about the store, the content of buffers or
;; rendering.

;;; Code:

(require 'approve-api)

(defvar approve-api-queue--waiting nil
  "Mutations waiting to start, oldest first.")

(defvar approve-api-queue--running nil
  "The mutation in flight, or nil.")

(defvar approve-api-queue--timer nil
  "Timer that resumes the paused queue, or nil when it is not paused.")

(defun approve-api-queue--next ()
  "Start the oldest waiting mutation unless one runs or the queue is paused."
  (when (and approve-api-queue--waiting
             (not approve-api-queue--running)
             (not approve-api-queue--timer))
    (approve-api--start
     (setq approve-api-queue--running (pop approve-api-queue--waiting)))))

(defun approve-api-queue--pause (reset)
  "Pause the queue until RESET, in seconds since the epoch."
  (when approve-api-queue--timer
    (cancel-timer approve-api-queue--timer))
  (approve-log 'info "Mutation queue paused until %s"
               (format-time-string "%T" reset))
  (setq approve-api-queue--timer
        (run-at-time (max 1 (- reset (float-time))) nil
                     #'approve-api-queue-resume)))

(defun approve-api-queue--settled (request _ok result)
  "Go on after the mutation REQUEST ended with RESULT.
A rate limit pauses the queue until it resets."
  (when (eq request approve-api-queue--running)
    (setq approve-api-queue--running nil))
  (when-let* ((reset (and (eq (car-safe result) 'approve-api-rate-limited)
                          (approve-api-error-reset result))))
    (approve-api-queue--pause reset))
  (approve-api-queue--next))

(defun approve-api-queue-resume ()
  "Resume the mutation queue now if a rate limit paused it."
  (interactive)
  (when approve-api-queue--timer
    (cancel-timer approve-api-queue--timer)
    (setq approve-api-queue--timer nil)
    (approve-log 'info "Mutation queue resumed"))
  (approve-api-queue--next))

(defun approve-api-queue-length ()
  "Return the number of mutations waiting or in flight."
  (+ (length approve-api-queue--waiting) (if approve-api-queue--running 1 0)))

(defun approve-api-queue-clear ()
  "Drop the mutations that have not started.
Each one's errorback gets `approve-api-cancelled'.  The mutation in
flight, if any, still finishes."
  (interactive)
  (let ((dropped approve-api-queue--waiting))
    (setq approve-api-queue--waiting nil)
    (dolist (request dropped)
      (approve-api--settle request nil
                           (approve-api--condition 'approve-api-cancelled
                                                   "Mutation cancelled")))))

(cl-defun approve-api-mutate (query variables
                                    &key callback errorback
                                    (buffer (current-buffer))
                                    (timeout approve-api-timeout)
                                    (progress approve-api-default-progress)
                                    host)
  "Queue the GraphQL mutation QUERY with VARIABLES.
Mutations run one at a time, oldest first; each starts after the
previous one ended.  One that fails with `approve-api-rate-limited'
goes to its errorback, and the queue pauses until the limit resets.
CALLBACK, ERRORBACK, BUFFER, TIMEOUT, PROGRESS and HOST are as for
`approve-api-graphql'.  QUERY is loaded now; TIMEOUT counts from
when the mutation starts.

Example:
  (approve-api-mutate \"mutations/resolve-review-thread.graphql\"
                      \\='((threadId . \"PRRT_x\"))
                      :callback (lambda (data) (message \"%S\" data)))"
  (setq approve-api-queue--waiting
        (append approve-api-queue--waiting
                (list (approve-api--make-request
                       (approve-api--graphql-spec query variables host)
                       :callback callback :errorback errorback
                       :buffer buffer :timeout timeout :progress progress
                       :on-settle #'approve-api-queue--settled))))
  (approve-api-queue--next))

(provide 'approve-api-queue)

;;; approve-api-queue.el ends here
