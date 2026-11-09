;;; test-approve-queries.el --- Tests for approve-queries -*- lexical-binding: t; -*-

;;; Commentary:

;; The detail queries: which query and variables each kind sends, and
;; how the answer is picked out of the response.  The loading plan
;; itself is tested through the PR buffer (test-approve-pr.el).

;;; Code:

(require 'buttercup)
(require 'approve-queries)
(require 'approve-test-helpers "helpers")

(defun test-queries--answering (body)
  "Return a transport that answers every request with BODY, recording it.
The requests are pushed to `test-queries--requests'."
  (lambda (request)
    (push request test-queries--requests)
    (let ((timer (run-at-time 0 nil (plist-get request :on-success) body nil)))
      (lambda () (cancel-timer timer)))))

(defvar test-queries--requests nil
  "Requests the transport of the current test saw.")

(describe "approve-query-details"
  :var (result failure)
  (before-each
    (setq test-queries--requests nil result nil failure nil)
    (approve-model-init '(:host "github.com" :owner "acme" :repo "rocket" :number 7)))

  (it "asks for a user by login and gives the user"
    (let ((approve-api-transport (test-queries--answering
                                  '((data (user (login . "bob") (bio . "hi"))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-details 'user "bob" :callback (lambda (data) (setq result data)))
      (approve-test-wait-for (lambda () result))
      (expect result :to-equal '((login . "bob") (bio . "hi")))
      (let ((request (car test-queries--requests)))
        (expect (plist-get request :name) :to-equal "queries/details/user.graphql")
        (expect (plist-get request :variables) :to-equal '((login . "bob")))
        (expect (plist-get request :host) :to-equal "api.github.com"))))

  (it "sends the parts of a team, a reference and a commit"
    (let ((approve-api-transport (test-queries--answering '((data (x . 1)))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-details 'team "acme/core" :errorback #'ignore)
      (approve-query-details 'ref '("acme" "other" 12) :errorback #'ignore)
      (approve-query-details 'commit "abc123" :errorback #'ignore)
      (approve-test-wait-for (lambda () (= (length test-queries--requests) 3)))
      (expect (mapcar (lambda (request) (plist-get request :variables))
                      (reverse test-queries--requests))
              :to-equal '(((org . "acme") (slug . "core"))
                          ((owner . "acme") (name . "other") (number . 12))
                          ((owner . "acme") (name . "rocket") (oid . "abc123"))))))

  (it "fails as not found when GitHub has no such thing"
    (let ((approve-api-transport (test-queries--answering
                                  '((data (repository (id . "R") (object))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-details 'commit "abc123" :callback (lambda (data) (setq result data))
                             :errorback (lambda (err) (setq failure err)))
      (approve-test-wait-for (lambda () failure))
      (expect (car failure) :to-be 'approve-api-not-found)
      (expect result :to-be nil))))

;;; test-approve-queries.el ends here
