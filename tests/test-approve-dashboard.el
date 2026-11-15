;;; test-approve-dashboard.el --- Tests for the dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; The dashboard (docs/ui.org, "Dashboard"; decision D39): a list per
;; entry of `approve-dashboard-sections', from GitHub's searches or the
;; pull requests opened in the session; loading, errors, a refresh of
;; one list, answers that come too late, and the commands on its rows
;; and headings.  A fake transport answers the requests.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-menus)
(require 'approve-test-helpers "helpers")

(defvar test-dashboard--requests nil
  "The requests the transport of the current test saw, newest first.")

(defvar test-dashboard--pending 0
  "How many requests the transport has not answered yet.")

(defvar test-dashboard--searches nil
  "What the fake GitHub finds: an alist of (SEARCH TOTAL . PRS).
SEARCH is the search as sent, is:pr included.")

(defvar test-dashboard--failing nil
  "An alist of (QUERY . STATUS): requests of QUERY fail with STATUS.")

(defvar test-dashboard--delays nil
  "Seconds to wait before answering, by request: a list used up in order.")

(defconst test-dashboard--viewer
  '((__typename . "User") (id . "U_me") (login . "me-example") (name . "Me")
    (url . "https://github.com/me-example"))
  "The viewer the fake GitHub knows.")

(defun test-dashboard--pr (number &rest fields)
  "Return pull request NUMBER of fuzzycode/approve as a search finds it.
FIELDS is a plist of field names and values that replace the
defaults: by @alice, open, updated two hours before `approve-test-now'."
  (let ((pr (copy-tree `((__typename . "PullRequest")
                         (id . ,(format "PR_%s" number))
                         (number . ,number)
                         (url . ,(format "https://github.com/fuzzycode/approve/pull/%s" number))
                         (title . ,(format "Pull request %s" number))
                         (state . "OPEN")
                         (isDraft)
                         (createdAt . "2026-10-01T12:00:00Z")
                         (updatedAt . "2026-10-04T10:00:00Z")
                         (author (__typename . "User") (id . "U_1") (login . "alice")
                                 (url . "https://github.com/alice"))
                         (repository (__typename . "Repository") (id . "R_1")
                                     (nameWithOwner . "fuzzycode/approve")
                                     (url . "https://github.com/fuzzycode/approve"))
                         (baseRefName . "main")
                         (headRefName . "feature/retry")
                         (additions . 340)
                         (deletions . 56)
                         (changedFiles . 12)
                         (reviewDecision . "CHANGES_REQUESTED")
                         (statusCheckRollup (__typename . "StatusCheckRollup")
                                            (id . ,(format "SCR_%s" number))
                                            (state . "PENDING"))))))
    (cl-loop for (field value) on fields by #'cddr
             do (setf (alist-get field pr) value))
    pr))

(defun test-dashboard--reply (request)
  "Return the fake GitHub's reply to REQUEST: (:body BODY) or (:status N)."
  (let ((name (plist-get request :name))
        (variables (plist-get request :variables)))
    (if-let* ((status (alist-get name test-dashboard--failing nil nil #'equal)))
        (list :status status)
      (pcase name
        ("queries/viewer.graphql"
         `(:body ((data (viewer ,@test-dashboard--viewer)))))
        ("queries/search-pull-requests.graphql"
         (let ((found (cdr (assoc (alist-get 'search variables) test-dashboard--searches))))
           `(:body ((data (search (issueCount . ,(or (car found) 0))
                                  (pageInfo (hasNextPage) (endCursor))
                                  (nodes ,@(cdr found))))))))
        ("queries/pull-request-summary.graphql"
         (let ((number (alist-get 'number variables)))
           (if (equal (alist-get 'owner variables) "gone")
               '(:body ((data (repository (__typename . "Repository") (id . "R_2")
                                          (pullRequest)))))
             `(:body ((data (repository (__typename . "Repository") (id . "R_1")
                                        (pullRequest ,@(test-dashboard--pr number)))))))))
        (_ (list :status 404))))))

(defun test-dashboard--transport (request)
  "Answer REQUEST as the fake GitHub would, after its delay, if any."
  (push request test-dashboard--requests)
  (cl-incf test-dashboard--pending)
  (let* ((reply (test-dashboard--reply request))
         (timer (run-at-time
                 (or (pop test-dashboard--delays) 0) nil
                 (lambda ()
                   (cl-decf test-dashboard--pending)
                   (if (plist-get reply :status)
                       (funcall (plist-get request :on-failure)
                                (list :status (plist-get reply :status)
                                      :payload `((message . ,(if (= (plist-get reply :status) 401)
                                                                 "Bad credentials"
                                                               "Bad Gateway")))))
                     (funcall (plist-get request :on-success) (plist-get reply :body) nil))))))
    (lambda () (cancel-timer timer))))

(defmacro test-dashboard--with (sections &rest body)
  "Run BODY with the dashboard's lists SECTIONS and the fake GitHub.
No pull request was opened before; the dashboard is killed after."
  (declare (indent 1) (debug t))
  `(let ((approve-api-transport #'test-dashboard--transport)
         (approve-graphql-directory approve-test-graphql-directory)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (approve-render-now approve-test-now)
         (approve-dashboard-sections ,sections)
         (approve-pr--recent nil)
         (test-dashboard--requests nil)
         (test-dashboard--pending 0)
         (test-dashboard--failing nil)
         (test-dashboard--delays nil)
         (inhibit-message t))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buffer (get-buffer approve-dashboard-buffer-name)))
         (kill-buffer buffer)))))

(defun test-dashboard--settle ()
  "Wait until every request is answered and the dashboard is redrawn."
  (approve-test-wait-for
   (lambda () (and (zerop test-dashboard--pending)
                   (not (buffer-local-value 'approve-dashboard--redraw-timer
                                            (get-buffer approve-dashboard-buffer-name)))))))

(defun test-dashboard--open ()
  "Open the dashboard, wait until it has loaded, and return its buffer."
  (let ((buffer (approve-dashboard)))
    (test-dashboard--settle)
    (set-buffer buffer)
    (goto-char (point-min))
    buffer))

(defun test-dashboard--searches-sent ()
  "Return the searches sent, oldest first, as (SEARCH . FIRST)."
  (mapcar (lambda (request)
            (let ((variables (plist-get request :variables)))
              (cons (alist-get 'search variables) (alist-get 'first variables))))
          (seq-filter (lambda (request)
                        (equal (plist-get request :name)
                               "queries/search-pull-requests.graphql"))
                      (reverse test-dashboard--requests))))

(defun test-dashboard--goto (text)
  "Move point to the start of the first TEXT in the buffer."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (error "%S is not in the buffer" text))
  (goto-char (match-beginning 0)))

(defun test-dashboard--suffixes (prefix)
  "Return the suffixes of the transient PREFIX, as plists with :key and :command."
  (let ((found nil))
    (cl-labels ((walk (x)
                  (cond ((and (consp x) (keywordp (car x)) (plist-member x :command))
                         (push x found))
                        ((consp x) (walk (car x)) (walk (cdr x)))
                        ((vectorp x) (mapc #'walk x)))))
      (walk (get prefix 'transient--layout)))
    (nreverse found)))

(defconst test-dashboard--two
  '((:title "Needs my review" :filter "is:open review-requested:@me")
    (:title "My pull requests" :filter "is:open author:@me"))
  "Two lists of searches.")

(describe "The dashboard"
  :var (saved-icons)
  (before-all
    (setq saved-icons approve-use-nerd-icons
          approve-use-nerd-icons nil)
    (approve-icon-clear-cache))
  (after-all
    (setq approve-use-nerd-icons saved-icons)
    (approve-icon-clear-cache))
  (before-each
    (setq test-dashboard--searches
          (copy-tree
           `(("is:pr is:open review-requested:@me" 2
              ,(test-dashboard--pr 1234 'title "Add retry logic to the GraphQL client")
              ,(test-dashboard--pr 88 'title "Fix crash when opening empty files"))
             ("is:pr is:open author:@me" 0)))))

  (it "draws a title line, then a list per section with its count and rows"
    (test-dashboard--with (append test-dashboard--two
                                  '((:title "Recently opened" :recent t)))
      (test-dashboard--open)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (expect text :to-match "\\`Approve — pull requests +@me-example · [0-9][0-9]:[0-9][0-9] · g refresh\n\n")
        (expect text :to-match "^Needs my review (2)\n  ● #1234 +Add retry logic to the GraphQL client")
        (expect text :to-match "^  ● #88 +Fix crash when opening empty files")
        (expect text :to-match "^My pull requests (0)\n  No pull requests\\.\n")
        (expect text :to-match "^Recently opened (0)\n  None opened in this session\\.\n"))
      (expect major-mode :to-be 'approve-dashboard-mode)
      (expect (buffer-name) :to-equal "*approve-dashboard*")))

  (it "sends each search with is:pr and its limit, calling a filter function then"
    (let ((org "acme"))
      (test-dashboard--with `((:title "Mine" :filter "is:open author:@me" :limit 5)
                              (:title "Team" :filter ,(lambda () (format "org:%s" org)))
                              (:title "Many" :filter "is:open" :limit 500))
        (test-dashboard--open)
        (expect (test-dashboard--searches-sent)
                :to-equal '(("is:pr is:open author:@me" . 5)
                            ("is:pr org:acme" . 20)
                            ("is:pr is:open" . 100))))))

  (it "shows a list as loading until it answers, then keeps its rows while it loads again"
    (test-dashboard--with test-dashboard--two
      (with-current-buffer (approve-dashboard)
        (expect (buffer-string) :to-match "^Needs my review\n  ⟳ Loading…")
        (test-dashboard--settle)
        (test-dashboard--goto "#1234")
        (approve-dashboard-refresh-section)
        (expect (buffer-string) :to-match "^Needs my review (2) +⟳ loading…")
        (expect (buffer-string) :to-match "#1234")
        (test-dashboard--settle)
        (expect (buffer-string) :not :to-match "loading…"))))

  (it "collapses the lists marked :hidden at first"
    (test-dashboard--with '((:title "Shown" :filter "is:open author:@me")
                            (:title "Folded" :filter "is:open review-requested:@me" :hidden t))
      (test-dashboard--open)
      (expect (oref (approve-test-find-section 'approve-dashboard-list-section "Shown") hidden)
              :to-be nil)
      (expect (oref (approve-test-find-section 'approve-dashboard-list-section "Folded") hidden)
              :to-be-truthy)))

  (it "says how many more pull requests a search finds than it shows"
    (setf (cadr (assoc "is:pr is:open review-requested:@me" test-dashboard--searches)) 23)
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (expect (buffer-string) :to-match "^Needs my review (23)\n")
      (expect (buffer-string) :to-match "^  … 21 more · o show them on GitHub\n")))

  (it "loads only the list at point again with G, keeping point on its row"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (test-dashboard--goto "#88")
      (forward-char 3)
      (setq test-dashboard--requests nil)
      (approve-dashboard-refresh-section)
      (test-dashboard--settle)
      (expect (test-dashboard--searches-sent)
              :to-equal '(("is:pr is:open review-requested:@me" . 20)))
      (expect (buffer-substring-no-properties (line-beginning-position) (point))
              :to-equal "  ● #88")
      (goto-char (point-min))
      (expect (approve-dashboard-refresh-section) :to-throw 'user-error)
      (expect (keymap-lookup approve-dashboard-mode-map "G")
              :to-be 'approve-dashboard-refresh-section)))

  (it "drops an answer that comes after the answer to a later request"
    (test-dashboard--with (list (car test-dashboard--two))
      (test-dashboard--open)
      (test-dashboard--goto "#1234")
      ;; The first request answers late, with a list that has changed since.
      (setq test-dashboard--delays '(0.2 0))
      (setcdr (assoc "is:pr is:open review-requested:@me" test-dashboard--searches)
              (list 1 (test-dashboard--pr 7 'title "Old answer")))
      (approve-dashboard-refresh-section)
      (setcdr (assoc "is:pr is:open review-requested:@me" test-dashboard--searches)
              (list 1 (test-dashboard--pr 8 'title "New answer")))
      (approve-dashboard-refresh-section)
      (test-dashboard--settle)
      (expect (buffer-string) :to-match "New answer")
      (expect (buffer-string) :not :to-match "Old answer")))

  (it "shows a list's error with how to retry it"
    (test-dashboard--with test-dashboard--two
      (setq test-dashboard--failing '(("queries/search-pull-requests.graphql" . 502)))
      (test-dashboard--open)
      (expect (buffer-string)
              :to-match "^Needs my review\n  ✗ Could not load the pull requests: Bad Gateway · G to retry")
      (expect (buffer-string) :to-match "@me-example")))

  (it "shows a filter that fails as its list's error"
    (test-dashboard--with `((:title "Broken" :filter ,(lambda () (error "No organization set")))
                            (:title "Nothing"))
      (test-dashboard--open)
      (expect (buffer-string) :to-match "^Broken\n  ✗ Could not load the pull requests: No organization set")
      (expect (buffer-string) :to-match "^Nothing\n  ✗ Could not load the pull requests: The list \"Nothing\" has neither")))

  (it "shows a token error once, instead of every list"
    (test-dashboard--with test-dashboard--two
      (setq test-dashboard--failing '(("queries/search-pull-requests.graphql" . 401)
                                      ("queries/viewer.graphql" . 401)))
      (test-dashboard--open)
      (let ((text (buffer-string)))
        (expect text :to-match "^✗ Could not use the GitHub token\\.\n  Bad credentials")
        (expect text :to-match "Approve asks ghub for the token")
        (expect text :to-match "g to retry")
        (expect text :not :to-match "Needs my review"))))

  (it "lists the pull requests opened in the session, newest first, without those GitHub lacks"
    (test-dashboard--with '((:title "Recently opened" :recent t :limit 3))
      (approve-pr-remember '(:host "github.com" :owner "fuzzycode" :repo "approve" :number 1))
      (approve-pr-remember '(:host "github.com" :owner "gone" :repo "away" :number 2))
      (approve-pr-remember '(:host "github.com" :owner "fuzzycode" :repo "approve" :number 3))
      (approve-pr-remember '(:host "git.example.com" :owner "fuzzycode" :repo "approve" :number 4))
      (test-dashboard--open)
      (let ((summaries (seq-filter (lambda (request)
                                     (equal (plist-get request :name)
                                            "queries/pull-request-summary.graphql"))
                                   (reverse test-dashboard--requests))))
        (expect (mapcar (lambda (request) (alist-get 'number (plist-get request :variables)))
                        summaries)
                :to-equal '(4 3 2))
        (expect (plist-get (car summaries) :host) :to-equal "git.example.com/api/v3")
        (expect (plist-get (cadr summaries) :host) :to-equal "api.github.com"))
      (expect (buffer-string) :to-match "^Recently opened (2)\n  ● #4 .*\n  ● #3 ")
      (expect (buffer-string) :not :to-match "#2 ")))

  (it "loads every list again each time it is asked for"
    (test-dashboard--with test-dashboard--two
      (let ((buffer (test-dashboard--open)))
        (expect (length (test-dashboard--searches-sent)) :to-be 2)
        (expect (approve-dashboard) :to-be buffer)
        (test-dashboard--settle)
        (expect (length (test-dashboard--searches-sent)) :to-be 4))))

  (it "opens a row's pull request with RET, and folds its list with TAB"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (test-dashboard--goto "Fix crash")
      (spy-on 'approve-pr-open)
      (approve-visit)
      (expect 'approve-pr-open :to-have-been-called-with
              '(:host "github.com" :owner "fuzzycode" :repo "approve" :number 88))
      (approve-section-toggle)
      (expect (oref (approve-test-find-section 'approve-dashboard-list-section
                                               "Needs my review")
                    hidden)
              :to-be-truthy)))

  (it "folds the list at point with zc and unfolds it with zo, from a row too"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (let ((list (approve-test-find-section 'approve-dashboard-list-section "Needs my review")))
        (test-dashboard--goto "Fix crash")
        (approve-section-hide)
        (expect (oref list hidden) :to-be-truthy)
        (expect (point) :to-equal (marker-position (oref list start)))
        (approve-section-hide)
        (expect (oref list hidden) :to-be-truthy)
        (approve-section-show)
        (expect (oref list hidden) :to-be nil)
        (approve-section-show)
        (expect (oref list hidden) :to-be nil))))

  (it "folds and unfolds every list, and T toggles them all"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (let ((hidden (lambda ()
                      (mapcar (lambda (title)
                                (and (oref (approve-test-find-section
                                            'approve-dashboard-list-section title)
                                           hidden)
                                     t))
                              '("Needs my review" "My pull requests")))))
        (test-dashboard--goto "#88")
        (approve-dashboard-fold-lists)
        (expect (funcall hidden) :to-equal '(t t))
        (expect (approve-dashboard-lists-folded-p) :to-be-truthy)
        (expect (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                :to-match "\\`Needs my review (2)")
        ;; Lists stay folded when the dashboard loads again.
        (approve-dashboard-refresh)
        (test-dashboard--settle)
        (expect (funcall hidden) :to-equal '(t t))
        (approve-dashboard-unfold-lists)
        (expect (funcall hidden) :to-equal '(nil nil))
        (expect (keymap-lookup approve-dashboard-mode-map "T")
                :to-be 'approve-dashboard-toggle-lists)
        (approve-dashboard-toggle-lists)
        (expect (funcall hidden) :to-equal '(t t))
        (approve-dashboard-toggle-lists)
        (expect (funcall hidden) :to-equal '(nil nil)))))

  (it "opens a list's search on GitHub with o on its heading, and copies it with y"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (test-dashboard--goto "My pull requests")
      (spy-on 'browse-url)
      (approve-browse)
      (expect 'browse-url :to-have-been-called-with
              "https://github.com/search?q=is%3Apr%20is%3Aopen%20author%3A%40me&type=pullrequests")
      (approve-copy)
      (expect (current-kill 0) :to-equal "is:pr is:open author:@me")
      (test-dashboard--goto "#1234")
      (approve-copy)
      (expect (current-kill 0) :to-equal "https://github.com/fuzzycode/approve/pull/1234")
      (goto-char (point-min))
      (expect (approve-browse) :to-throw 'user-error)))

  (it "describes a pull request and a search in the echo area"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (test-dashboard--goto "#1234")
      (expect (substring-no-properties
               (plist-get (approve-eldoc-describe (approve-context-at-point)) :echo))
              :to-equal "#1234 — fuzzycode/approve · feature/retry → main · +340 −56 · 12 files")
      (let ((doc (plist-get (approve-eldoc-describe (approve-context-at-point)) :doc)))
        (expect doc :to-match "Add retry logic to the GraphQL client")
        (expect doc :to-match "^Review +changes requested")
        (expect doc :to-match "^Checks +◐ running"))
      (test-dashboard--goto "Needs my review")
      (expect (substring-no-properties
               (plist-get (approve-eldoc-describe (approve-context-at-point)) :echo))
              :to-equal "Needs my review — is:pr is:open review-requested:@me — 2 found")
      (goto-char (point-max))
      (expect (approve-context-at-point) :to-be nil))))

(describe "The dashboard's menu"
  (it "opens on ?, with the dashboard's keys, and reaches every command it binds"
    (expect (keymap-lookup approve-dashboard-mode-map "?") :to-be 'approve-dashboard-dispatch)
    (let ((suffixes (test-dashboard--suffixes 'approve-dashboard-dispatch))
          (bound nil))
      (dolist (suffix suffixes)
        (expect (commandp (plist-get suffix :command)) :to-be-truthy)
        (expect (keymap-lookup approve-dashboard-mode-map (plist-get suffix :key))
                :to-be (plist-get suffix :command)))
      (map-keymap (lambda (_event binding)
                    (when (and (symbolp binding)
                               (string-prefix-p "approve-" (symbol-name binding)))
                      (push binding bound)))
                  approve-dashboard-mode-map)
      (expect (seq-difference (delete-dups bound)
                              (cons 'approve-dashboard-dispatch
                                    (mapcar (lambda (suffix) (plist-get suffix :command))
                                            suffixes)))
              :to-equal nil)))

  (it "dims G outside a list"
    (test-dashboard--with test-dashboard--two
      (test-dashboard--open)
      (expect (approve-action-why-not 'approve-dashboard-refresh-section)
              :to-equal "Put point on a list of the dashboard")
      (test-dashboard--goto "#1234")
      (expect (approve-action-why-not 'approve-dashboard-refresh-section) :to-be nil))))

;;; test-approve-dashboard.el ends here
