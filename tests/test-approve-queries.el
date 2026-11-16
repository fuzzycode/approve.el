;;; test-approve-queries.el --- Tests for approve-queries -*- lexical-binding: t; -*-

;;; Commentary:

;; The detail queries: which query and variables each kind sends, and
;; how the answer is picked out of the response.  The search of the
;; lists of pull requests.  The loading plan itself is tested through
;; the PR buffer (test-approve-pr.el).

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

  (it "asks a pull request's own GitHub for its details"
    (let ((approve-api-transport (test-queries--answering '((data (x . 1)))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-details 'pull-request '("acme" "other" 12 "git.example.com")
                             :errorback #'ignore)
      (approve-test-wait-for (lambda () test-queries--requests))
      (let ((request (car test-queries--requests)))
        (expect (plist-get request :name) :to-equal "queries/details/pull-request.graphql")
        (expect (plist-get request :variables)
                :to-equal '((owner . "acme") (name . "other") (number . 12)))
        (expect (plist-get request :host) :to-equal "git.example.com/api/v3"))))

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

(describe "approve-query-search"
  :var (result)
  (before-each
    (setq test-queries--requests nil result nil))

  (it "asks for pull requests only, a page at a time"
    (let ((approve-api-transport (test-queries--answering
                                  '((data (search (issueCount . 0) (nodes))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-search "is:open review-requested:@me" :errorback #'ignore)
      (approve-query-search "is:pr author:@me" :first 5 :after "Y3Vy" :errorback #'ignore)
      (approve-query-search "type:pr  label:bug" :errorback #'ignore)
      (approve-query-search "is:private" :errorback #'ignore)
      (approve-test-wait-for (lambda () (= (length test-queries--requests) 4)))
      (let ((requests (reverse test-queries--requests)))
        (expect (plist-get (car requests) :name)
                :to-equal "queries/search-pull-requests.graphql")
        (expect (mapcar (lambda (request) (plist-get request :variables)) requests)
                :to-equal '(((search . "is:pr is:open review-requested:@me") (first . 20))
                            ((search . "is:pr author:@me") (first . 5) (after . "Y3Vy"))
                            ((search . "type:pr  label:bug") (first . 20))
                            ((search . "is:pr is:private") (first . 20)))))))

  (it "gives GitHub's search, with the pull requests among its nodes"
    (let ((approve-api-transport
           (test-queries--answering
            '((data (search (issueCount . 2)
                            (pageInfo (hasNextPage . t) (endCursor . "Y3Vy"))
                            (nodes ((__typename . "PullRequest") (number . 7)
                                    (title . "Retry"))
                                   nil))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-search "author:@me" :callback (lambda (search) (setq result search)))
      (approve-test-wait-for (lambda () result))
      (let-alist result
        (expect .issueCount :to-be 2)
        (expect .pageInfo.endCursor :to-equal "Y3Vy")
        (expect (mapcar (lambda (node) (alist-get 'number node)) .nodes)
                :to-equal '(7))))))

(describe "approve-query-summary"
  :var (result failure)
  (before-each
    (setq test-queries--requests nil result nil failure nil))

  (it "asks the PR's own GitHub for it by number, and gives it"
    (let ((approve-api-transport
           (test-queries--answering
            '((data (repository (__typename . "Repository") (id . "R_1")
                                (pullRequest (__typename . "PullRequest") (id . "PR_1")
                                             (number . 5) (title . "Retry")))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-summary '(:host "git.example.com" :owner "acme" :repo "rocket" :number 5)
                             :callback (lambda (pr) (setq result pr)))
      (approve-test-wait-for (lambda () result))
      (expect (alist-get 'title result) :to-equal "Retry")
      (let ((request (car test-queries--requests)))
        (expect (plist-get request :name) :to-equal "queries/pull-request-summary.graphql")
        (expect (plist-get request :variables)
                :to-equal '((owner . "acme") (name . "rocket") (number . 5)))
        (expect (plist-get request :host) :to-equal "git.example.com/api/v3"))))

  (it "fails as not found when GitHub has no such pull request"
    (let ((approve-api-transport
           (test-queries--answering
            '((data (repository (__typename . "Repository") (id . "R_1") (pullRequest))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t))
      (approve-query-summary '(:host "github.com" :owner "acme" :repo "rocket" :number 5)
                             :callback (lambda (pr) (setq result pr))
                             :errorback (lambda (err) (setq failure err)))
      (approve-test-wait-for (lambda () failure))
      (expect (car failure) :to-be 'approve-api-not-found)
      (expect result :to-be nil))))

(describe "approve-query-viewer"
  (it "asks who the token belongs to, and gives the user"
    (let ((approve-api-transport
           (test-queries--answering
            '((data (viewer (__typename . "User") (id . "U_1") (login . "me"))))))
          (approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t)
          (viewer nil))
      (setq test-queries--requests nil)
      (approve-query-viewer :callback (lambda (user) (setq viewer user)))
      (approve-test-wait-for (lambda () viewer))
      (expect (alist-get 'login viewer) :to-equal "me")
      (expect (plist-get (car test-queries--requests) :name)
              :to-equal "queries/viewer.graphql"))))

(describe "approve-query-blob"
  :var (result failure)
  (before-each
    (setq test-queries--requests nil result nil failure nil)
    (approve-model-init '(:host "github.com" :owner "acme" :repo "rocket" :number 7)))

  (cl-flet ((fetch (text bytes &optional exact)
              (let ((approve-api-transport
                     (test-queries--answering
                      `((data (repository (object (__typename . "Blob") (id . "B_1")
                                                  (text . ,text) (isBinary . nil)
                                                  (isTruncated . nil)
                                                  (byteSize . ,bytes)))))))
                    (approve-graphql-directory approve-test-graphql-directory)
                    (inhibit-message t))
                (approve-query-blob "abc" "a.txt" :exact exact
                                    :callback (lambda (text) (setq result text))
                                    :errorback (lambda (err) (setq failure err)))
                (approve-test-wait-for (lambda () (or result failure))))))
    (it "gives a file's text at a commit"
      (fetch "été\r\n" 7)
      (expect result :to-equal "été\r\n")
      (expect (plist-get (car test-queries--requests) :variables)
              :to-equal '((owner . "acme") (name . "rocket") (expression . "abc:a.txt"))))

    (it "refuses a text that is not the file's bytes when they must be exact"
      (fetch "été\r\n" 9 t)
      (expect result :to-be nil)
      (expect (approve-api-error-message failure) :to-equal "a.txt is not UTF-8 text")
      (setq result nil failure nil)
      (fetch "été\r\n" 7 t)
      (expect result :to-equal "été\r\n"))))

(describe "approve-query-await-head"
  (before-each
    (setq test-queries--requests nil)
    (approve-model-init '(:host "github.com" :owner "acme" :repo "rocket" :number 7)))

  (it "asks for the head until it is the commit, or tries no more"
    (let ((approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t)
          (done nil))
      (let ((approve-api-transport
             (test-queries--answering
              '((data (repository (pullRequest (__typename . "PullRequest") (id . "PR")
                                               (headRefOid . "new"))))))))
        (approve-query-await-head "new" :callback (lambda () (setq done t)))
        (approve-test-wait-for (lambda () done))
        (expect (length test-queries--requests) :to-equal 1)
        (expect (plist-get (car test-queries--requests) :name)
                :to-equal "queries/pull-request-changes.graphql"))
      (setq done nil test-queries--requests nil)
      (let ((approve-api-transport
             (test-queries--answering
              '((data (repository (pullRequest (__typename . "PullRequest") (id . "PR")
                                               (headRefOid . "old"))))))))
        (approve-query-await-head "new" :tries 2 :callback (lambda () (setq done t)))
        (approve-test-wait-for (lambda () done) 5)
        (expect (length test-queries--requests) :to-equal 2))))

  (it "with :away, asks until the head is any other commit"
    (let ((approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t)
          (done nil)
          (approve-api-transport
           (test-queries--answering
            '((data (repository (pullRequest (__typename . "PullRequest") (id . "PR")
                                             (headRefOid . "merged"))))))))
      (approve-query-await-head "old" :away t :callback (lambda () (setq done t)))
      (approve-test-wait-for (lambda () done))
      (expect (length test-queries--requests) :to-equal 1))))

(describe "approve-query-merge-message"
  (before-each
    (setq test-queries--requests nil)
    (approve-model-init '(:host "github.com" :owner "acme" :repo "rocket" :number 7)))

  (it "asks for GitHub's message of a method and gives its headline and body"
    (let ((approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t)
          (result nil)
          (approve-api-transport
           (test-queries--answering
            '((data (repository
                     (pullRequest (viewerMergeHeadlineText . "Rewrite the parser (#7)")
                                  (viewerMergeBodyText . "* One\n\n* Two"))))))))
      (approve-query-merge-message "SQUASH" :callback (lambda (message) (setq result message)))
      (approve-test-wait-for (lambda () result))
      (expect result :to-equal '("Rewrite the parser (#7)" . "* One\n\n* Two"))
      (let ((request (car test-queries--requests)))
        (expect (plist-get request :name)
                :to-equal "queries/pull-request-merge-message.graphql")
        (expect (alist-get 'method (plist-get request :variables)) :to-equal "SQUASH")
        (expect (alist-get 'number (plist-get request :variables)) :to-equal 7))))

  (it "fails as not found when GitHub has no such pull request"
    (let ((approve-graphql-directory approve-test-graphql-directory)
          (inhibit-message t)
          (failure nil)
          (approve-api-transport (test-queries--answering
                                  '((data (repository (__typename . "Repository")
                                                      (id . "R_1") (pullRequest)))))))
      (approve-query-merge-message "MERGE" :callback #'ignore
                                   :errorback (lambda (err) (setq failure err)))
      (approve-test-wait-for (lambda () failure))
      (expect (car failure) :to-be 'approve-api-not-found))))

;;; test-approve-queries.el ends here
