;;; test-approve-metadata.el --- Tests for repository metadata -*- lexical-binding: t; -*-

;;; Commentary:

;; Mentionable users, recent issues and pull requests, and emoji: what
;; goes to GitHub, what is kept, and that it is fetched once.

;;; Code:

(require 'buttercup)
(require 'approve-metadata)
(require 'approve-test-helpers "helpers")

(defvar test-metadata--sent nil
  "Requests the fake transport received, newest first.")

(defvar test-metadata--answers nil
  "Alist of request name or resource → function of the request giving the body.
A request with no answer fails with HTTP 502.")

(defun test-metadata--transport (request)
  "Answer REQUEST from `test-metadata--answers', later."
  (push request test-metadata--sent)
  (let* ((answer (cdr (assoc (or (plist-get request :name) (plist-get request :resource))
                             test-metadata--answers)))
         (timer (if answer
                    (run-at-time 0 nil (plist-get request :on-success)
                                 (funcall answer request) nil)
                  (run-at-time 0 nil (plist-get request :on-failure)
                               '(:status 502 :payload ((message . "Bad Gateway")))))))
    (lambda () (cancel-timer timer))))

(defun test-metadata--users-page (request)
  "Return a page of mentionable users for REQUEST: two pages in all."
  (let ((second (alist-get 'after (plist-get request :variables))))
    `((data (repository
             (__typename . "Repository") (id . "R_1")
             (mentionableUsers
              (totalCount . 2)
              (pageInfo (hasNextPage . ,(not second)) (endCursor . "c1"))
              (nodes ((__typename . "User") (id . ,(if second "U_2" "U_1"))
                      (login . ,(if second "bob-example" "alice-example"))
                      (name . ,(if second "Bob" "Alice"))))))))))

(defmacro test-metadata--with-repo (&rest body)
  "Run BODY in a buffer of the repository acme-example/rocket, faked."
  (declare (indent 0))
  `(let ((approve-api-transport #'test-metadata--transport)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (inhibit-message t))
     (with-temp-buffer
       (approve-model-init '(:host "github.com" :owner "acme-example" :repo "rocket"
                                   :number 1))
       ,@body)))

(describe "approve-metadata"
  (before-each
    (approve-metadata-clear-cache)
    (clrhash approve-metadata--waiting)
    (setq test-metadata--sent nil
          test-metadata--answers
          `(("queries/metadata/mentionable-users.graphql" . test-metadata--users-page)
            ("queries/metadata/recent-refs.graphql"
             . ,(lambda (_)
                  '((data (search (nodes ((__typename . "Issue") (id . "I_1") (number . 12)
                                          (title . "Crash on empty files") (state . "OPEN"))))))))
            ("/emojis"
             . ,(lambda (_)
                  '((\+1 . "https://github.githubassets.com/images/icons/emoji/unicode/1f44d.png?v8")
                    (family_man_woman_girl
                     . "https://github.githubassets.com/images/icons/emoji/unicode/1f468-200d-1f469-200d-1f467.png?v8")
                    (octocat . "https://github.githubassets.com/images/icons/emoji/octocat.png?v8")))))))

  (it "fetches who can be mentioned in the repository, page after page"
    (test-metadata--with-repo
      (approve-metadata-fetch 'mentionable-users)
      (approve-test-wait-for (lambda () (approve-metadata 'mentionable-users)))
      (expect (mapcar (lambda (user) (alist-get 'login user))
                      (approve-metadata 'mentionable-users))
              :to-equal '("alice-example" "bob-example"))
      (expect (plist-get (car (last test-metadata--sent)) :variables)
              :to-equal '((owner . "acme-example") (name . "rocket")))
      (expect (length test-metadata--sent) :to-equal 2)))

  (it "fetches the issues and pull requests of the repository updated last"
    (test-metadata--with-repo
      (approve-metadata-fetch 'refs)
      (approve-test-wait-for (lambda () (approve-metadata 'refs)))
      (expect (alist-get 'title (car (approve-metadata 'refs)))
              :to-equal "Crash on empty files")
      (expect (plist-get (car test-metadata--sent) :variables)
              :to-equal '((search . "repo:acme-example/rocket sort:updated-desc")))))

  (it "fetches GitHub's emoji with their glyphs, for every repository of the host"
    (test-metadata--with-repo
      (approve-metadata-fetch 'emoji)
      (approve-test-wait-for (lambda () (approve-metadata 'emoji)))
      (expect (approve-metadata 'emoji)
              :to-equal '(("+1" . "👍") ("family_man_woman_girl" . "👨‍👩‍👧") ("octocat")))
      (approve-model-set-meta :repo "other")
      (expect (approve-metadata 'emoji) :not :to-be nil)
      (expect (approve-metadata 'refs) :to-be nil)))

  (it "fetches once, and calls back whoever asks when it is there"
    (test-metadata--with-repo
      (let ((calls nil))
        (approve-metadata-fetch 'refs :callback (lambda (refs) (push (length refs) calls)))
        (approve-metadata-fetch 'refs :callback (lambda (refs) (push (length refs) calls)))
        (expect (length test-metadata--sent) :to-equal 1)
        (approve-test-wait-for (lambda () (= (length calls) 2)))
        (approve-metadata-fetch 'refs :callback (lambda (refs) (push (length refs) calls)))
        (expect calls :to-equal '(1 1 1))
        (expect (length test-metadata--sent) :to-equal 1))))

  (it "keeps nothing from a failed fetch, so the next one tries again"
    (test-metadata--with-repo
      (setq test-metadata--answers nil)
      (approve-metadata-fetch 'emoji)
      (approve-test-wait-for (lambda () (zerop (hash-table-count approve-metadata--waiting))))
      (expect (approve-metadata 'emoji) :to-be nil)
      (approve-metadata-fetch 'emoji)
      (expect (length test-metadata--sent) :to-equal 2)))

  (it "forgets what it fetched when the cache is cleared"
    (test-metadata--with-repo
      (approve-metadata-fetch 'refs)
      (approve-test-wait-for (lambda () (approve-metadata 'refs)))
      (approve-metadata-clear-cache)
      (expect (approve-metadata 'refs) :to-be nil))))

;;; test-approve-metadata.el ends here
