;;; test-approve-metadata.el --- Tests for repository metadata -*- lexical-binding: t; -*-

;;; Commentary:

;; Mentionable and assignable users, labels, milestones, teams, recent
;; issues and pull requests, and emoji: what goes to GitHub, what is
;; kept, that it is fetched once, and how a command waits for it.

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

(defun test-metadata--page (path nodes)
  "Return an answer to a request: one page, the last, of NODES at PATH."
  (lambda (_request)
    (let ((value `((totalCount . ,(length nodes))
                   (pageInfo (hasNextPage) (endCursor))
                   (nodes ,@nodes))))
      (dolist (key (reverse path))
        (setq value (list (cons key value))))
      (list (cons 'data value)))))

(defun test-metadata--owner (request)
  "Answer REQUEST for the owner: acme-example is an organization, others users."
  (let ((login (alist-get 'owner (plist-get request :variables))))
    `((data (repositoryOwner
             (__typename . ,(if (equal login "acme-example") "Organization" "User"))
             (id . "O_1") (login . ,login))))))

(defun test-metadata--names (requests)
  "Return the query names of REQUESTS, oldest first."
  (reverse (mapcar (lambda (request) (plist-get request :name)) requests)))

(defmacro test-metadata--with-repo (&rest body)
  "Run BODY in a buffer of the repository acme-example/rocket, faked."
  (declare (indent 0))
  `(let ((approve-api-transport #'test-metadata--transport)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (inhibit-message t))
     (with-temp-buffer
       ;; A fresh plist: tests change the owner and the repository.
       (approve-model-init (list :host "github.com" :owner "acme-example" :repo "rocket"
                                 :number 1))
       ,@body)))

(describe "approve-metadata"
  (before-each
    (approve-metadata-clear-cache)
    (clrhash approve-metadata--waiting)
    (setq test-metadata--sent nil
          test-metadata--answers
          `(("queries/metadata/mentionable-users.graphql" . test-metadata--users-page)
            ("queries/metadata/assignable-users.graphql"
             . ,(test-metadata--page
                 '(repository assignableUsers)
                 '(((__typename . "User") (id . "U_1") (login . "alice-example")))))
            ("queries/metadata/labels.graphql"
             . ,(test-metadata--page
                 '(repository labels)
                 '(((__typename . "Label") (id . "LA_1") (name . "bug") (color . "d73a4a"))
                   ((__typename . "Label") (id . "LA_2") (name . "docs") (color . "0075ca")))))
            ("queries/metadata/milestones.graphql"
             . ,(test-metadata--page
                 '(repository milestones)
                 '(((__typename . "Milestone") (id . "MI_1") (number . 3) (title . "1.0")))))
            ("queries/metadata/owner.graphql" . test-metadata--owner)
            ("queries/metadata/teams.graphql"
             . ,(test-metadata--page
                 '(organization teams)
                 '(((__typename . "Team") (id . "T_1") (name . "Core") (slug . "core")
                    (combinedSlug . "acme-example/core")))))
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

  (it "fetches no more pages than approve-metadata-max-pages"
    (test-metadata--with-repo
      (let ((approve-metadata-max-pages 1))
        (approve-metadata-fetch 'mentionable-users)
        (approve-test-wait-for (lambda () (approve-metadata 'mentionable-users)))
        (expect (mapcar (lambda (user) (alist-get 'login user))
                        (approve-metadata 'mentionable-users))
                :to-equal '("alice-example"))
        (expect (length test-metadata--sent) :to-equal 1))))

  (it "fetches who can be assigned, the labels and the open milestones of the repository"
    (test-metadata--with-repo
      (dolist (kind '(assignable-users labels milestones))
        (approve-metadata-fetch kind))
      (approve-test-wait-for (lambda () (approve-metadata 'milestones)))
      (approve-test-wait-for (lambda () (approve-metadata 'labels)))
      (approve-test-wait-for (lambda () (approve-metadata 'assignable-users)))
      (expect (alist-get 'login (car (approve-metadata 'assignable-users)))
              :to-equal "alice-example")
      (expect (mapcar (lambda (label) (alist-get 'name label)) (approve-metadata 'labels))
              :to-equal '("bug" "docs"))
      (expect (alist-get 'title (car (approve-metadata 'milestones))) :to-equal "1.0")
      (expect (test-metadata--names test-metadata--sent)
              :to-equal '("queries/metadata/assignable-users.graphql"
                          "queries/metadata/labels.graphql"
                          "queries/metadata/milestones.graphql"))
      (dolist (request test-metadata--sent)
        (expect (plist-get request :variables)
                :to-equal '((owner . "acme-example") (name . "rocket"))))))

  (it "fetches the teams of the organization that owns the repository"
    (test-metadata--with-repo
      (approve-metadata-fetch 'teams)
      (approve-test-wait-for (lambda () (approve-metadata 'teams)))
      (expect (alist-get 'combinedSlug (car (approve-metadata 'teams)))
              :to-equal "acme-example/core")
      (expect (test-metadata--names test-metadata--sent)
              :to-equal '("queries/metadata/owner.graphql"
                          "queries/metadata/teams.graphql"))
      (expect (plist-get (car test-metadata--sent) :variables)
              :to-equal '((owner . "acme-example")))))

  (it "knows a user who owns a repository has no teams, and asks no more"
    (test-metadata--with-repo
      (approve-model-set-meta :owner "erin-example")
      (let ((teams 'none))
        (approve-metadata-fetch 'teams :callback (lambda (fetched) (setq teams fetched)))
        (approve-test-wait-for (lambda () (not (eq teams 'none))))
        (expect teams :to-be nil)
        (approve-metadata-fetch 'teams)
        (expect (test-metadata--names test-metadata--sent)
                :to-equal '("queries/metadata/owner.graphql")))))

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

  (it "tells whoever waits for a fetch that fails, and only them"
    (test-metadata--with-repo
      (setq test-metadata--answers nil)
      (let ((errors nil)
            (values nil))
        (approve-metadata-fetch 'labels
                                :callback (lambda (labels) (push labels values))
                                :errorback (lambda (err) (push (car err) errors)))
        (approve-metadata-fetch 'labels :errorback (lambda (err) (push (car err) errors)))
        (approve-metadata-fetch 'labels)
        (approve-test-wait-for (lambda () (= (length errors) 2)))
        (expect values :to-be nil)
        (expect (length test-metadata--sent) :to-equal 1))))

  (it "signals an error for metadata it doesn't know"
    (test-metadata--with-repo
      (expect (approve-metadata-fetch 'colours) :to-throw 'error)
      (expect test-metadata--sent :to-be nil)
      (expect (hash-table-count approve-metadata--waiting) :to-equal 0)))

  (it "forgets what it fetched when the cache is cleared"
    (test-metadata--with-repo
      (approve-metadata-fetch 'refs)
      (approve-test-wait-for (lambda () (approve-metadata 'refs)))
      (approve-metadata-clear-cache)
      (expect (approve-metadata 'refs) :to-be nil))))

(describe "approve-metadata-wait"
  (before-each
    (approve-metadata-clear-cache)
    (clrhash approve-metadata--waiting)
    (setq test-metadata--sent nil
          test-metadata--answers
          `(("queries/metadata/labels.graphql"
             . ,(test-metadata--page
                 '(repository labels)
                 '(((__typename . "Label") (id . "LA_1") (name . "bug"))))))))

  (it "waits for what isn't fetched yet, and returns it"
    (test-metadata--with-repo
      (expect (mapcar (lambda (label) (alist-get 'name label))
                      (approve-metadata-wait 'labels))
              :to-equal '("bug"))
      (expect (length test-metadata--sent) :to-equal 1)))

  (it "returns at once what was fetched before"
    (test-metadata--with-repo
      (approve-metadata-wait 'labels)
      (spy-on 'accept-process-output)
      (expect (length (approve-metadata-wait 'labels)) :to-equal 1)
      (expect 'accept-process-output :not :to-have-been-called)
      (expect (length test-metadata--sent) :to-equal 1)))

  (it "returns an empty list as fetched, without asking again"
    (test-metadata--with-repo
      (setq test-metadata--answers
            `(("queries/metadata/labels.graphql"
               . ,(test-metadata--page '(repository labels) nil))))
      (expect (approve-metadata-wait 'labels) :to-be nil)
      (expect (approve-metadata-wait 'labels) :to-be nil)
      (expect (length test-metadata--sent) :to-equal 1)))

  (it "says what it could not fetch, and why"
    (test-metadata--with-repo
      (setq test-metadata--answers nil)
      (let ((err (condition-case err
                     (approve-metadata-wait 'labels)
                   (user-error err))))
        (expect (car err) :to-be 'user-error)
        (expect (cadr err)
                :to-match "\\`Could not fetch the labels of acme-example/rocket: ."))
      (expect (hash-table-count approve-metadata--waiting) :to-equal 0)))

  (it "needs a repository for a repository's lists"
    (let ((inhibit-message t))
      (with-temp-buffer
        (expect (approve-metadata-wait 'labels) :to-throw 'user-error)))))

;;; test-approve-metadata.el ends here
