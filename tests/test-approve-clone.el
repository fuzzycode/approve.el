;;; test-approve-clone.el --- Tests for the user's local clones -*- lexical-binding: t; -*-

;;; Commentary:

;; Finding clones in `approve-clone-directories', the clone that has a
;; commit, and showing a commit from it in Magit (decision D37).  The
;; clones are throwaway repositories made with git in a temporary
;; directory; the tests are skipped where git is missing.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-clone)
(require 'approve-test-helpers "helpers")

(defun test-clone--git (directory &rest args)
  "Run git with ARGS in DIRECTORY; return its output, trimmed.
Signal an error when git fails.  The user's git config is left out."
  (let ((default-directory (file-name-as-directory directory))
        (process-environment (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null")
                                     process-environment)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil
                            "-c" "user.name=Approve Test" "-c" "user.email=test@example.com"
                            "-c" "commit.gpgsign=false" args))
        (error "Git %s failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun test-clone--make (directory message)
  "Make a clone in DIRECTORY with one commit of MESSAGE; return its sha."
  (make-directory directory t)
  (test-clone--git directory "init" "-q")
  (test-clone--git directory "commit" "-q" "--allow-empty" "-m" message)
  (test-clone--git directory "rev-parse" "HEAD"))

(defvar test-clone--root nil
  "The temporary directory of the clones of a test.")

(defvar test-clone--shas nil
  "The sha of the commit of each clone of a test, an alist by name.")

(defmacro test-clone--with-clones (&rest body)
  "Run BODY with clones \"one\" and \"two\" and a plain directory in a root.
`test-clone--root' is the root and `test-clone--shas' has each
clone's commit; the root is the only entry of
`approve-clone-directories'."
  (declare (indent 0) (debug t))
  `(progn
     (assume (executable-find "git") "git is not installed")
     (let* ((test-clone--root (file-name-as-directory (make-temp-file "approve-clone-" t)))
            (test-clone--shas
             (list (cons "one" (test-clone--make (expand-file-name "one" test-clone--root)
                                                 "First"))
                   (cons "two" (test-clone--make (expand-file-name "two" test-clone--root)
                                                 "Second"))))
            (approve-clone-directories (list (cons test-clone--root 1))))
       (make-directory (expand-file-name "notes" test-clone--root))
       (unwind-protect
           (progn ,@body)
         (delete-directory test-clone--root t)))))

(defun test-clone--dir (name)
  "Return the top directory of the clone NAME of the test."
  (file-name-as-directory (expand-file-name name test-clone--root)))

(describe "approve-clone-list"
  (it "lists the clones in the directories, expanded, leaving the rest out"
    (test-clone--with-clones
      (expect (sort (approve-clone-list) #'string<)
              :to-equal (list (test-clone--dir "one") (test-clone--dir "two")))))

  (it "takes a clone itself at depth 0, and lists it once"
    (test-clone--with-clones
      (let ((approve-clone-directories `((,(test-clone--dir "two") . 0)
                                         (,test-clone--root . 1))))
        (expect (car (approve-clone-list)) :to-equal (test-clone--dir "two"))
        (expect (length (approve-clone-list)) :to-equal 2))))

  (it "uses Magit's directories by default, and none when nil"
    (test-clone--with-clones
      (let ((approve-clone-directories 'magit)
            (magit-repository-directories `((,(test-clone--dir "one") . 0))))
        (expect (approve-clone-list) :to-equal (list (test-clone--dir "one"))))
      (let ((approve-clone-directories nil)
            (magit-repository-directories `((,test-clone--root . 1))))
        (expect (approve-clone-list) :to-be nil)))))

(describe "approve-clone-with-commit"
  (it "finds the clone that has the commit, by its sha or a prefix of it"
    (test-clone--with-clones
      (let ((sha (alist-get "two" test-clone--shas nil nil #'equal)))
        (expect (approve-clone-with-commit sha) :to-equal (test-clone--dir "two"))
        (expect (approve-clone-with-commit (substring sha 0 7))
                :to-equal (test-clone--dir "two")))))

  (it "finds nothing for a commit no clone has, or what is not a sha"
    (test-clone--with-clones
      (expect (approve-clone-with-commit (make-string 40 ?a)) :to-be nil)
      (expect (approve-clone-with-commit "HEAD") :to-be nil)
      (expect (approve-clone-with-commit "--all") :to-be nil)
      (expect (approve-clone-with-commit nil) :to-be nil))))

(describe "approve-clone-show-commit"
  (it "shows the commit in Magit from the clone that has it"
    (test-clone--with-clones
      (let ((sha (alist-get "one" test-clone--shas nil nil #'equal))
            (shown nil))
        (spy-on 'magit-show-commit
                :and-call-fake (lambda (rev &rest _)
                                 (setq shown (cons rev default-directory))))
        (expect (approve-clone-show-commit sha "https://github.com/a/b/commit/x")
                :to-equal (test-clone--dir "one"))
        (expect shown :to-equal (cons sha (test-clone--dir "one"))))))

  (it "opens Magit's revision buffer of the commit"
    (test-clone--with-clones
      (let ((sha (alist-get "two" test-clone--shas nil nil #'equal)))
        (save-window-excursion
          (approve-clone-show-commit sha)
          (let ((buffer (seq-find (lambda (buffer)
                                    (with-current-buffer buffer
                                      (derived-mode-p 'magit-revision-mode)))
                                  (buffer-list))))
            (unwind-protect
                (with-current-buffer buffer
                  (expect default-directory :to-equal (test-clone--dir "two"))
                  (expect (buffer-string) :to-match "Second"))
              (kill-buffer buffer)))))))

  (it "shows nothing and returns nil when no clone has the commit"
    (test-clone--with-clones
      (spy-on 'magit-show-commit)
      (expect (approve-clone-show-commit (make-string 40 ?b)) :to-be nil)
      (expect 'magit-show-commit :not :to-have-been-called)))

  (it "is how RET shows a commit, once approve is loaded"
    (expect (memq #'approve-clone-show-commit approve-visit-commit-functions)
            :to-be-truthy)))

;;; Remotes

(describe "approve-clone-parse-remote"
  (it "reads the host, owner and name of scp-like and scheme URLs"
    (dolist (url '("git@github.com:fuzzycode/test-approve.git"
                   "git@github.com:fuzzycode/test-approve"
                   "github.com:fuzzycode/test-approve.git"
                   "https://github.com/fuzzycode/test-approve.git"
                   "https://github.com/fuzzycode/test-approve"
                   "https://github.com/fuzzycode/test-approve/"
                   "https://user@GitHub.com/fuzzycode/test-approve.git"
                   "ssh://git@github.com/fuzzycode/test-approve.git"
                   "ssh://git@github.com:22/fuzzycode/test-approve.git"
                   "git+ssh://git@github.com/fuzzycode/test-approve.git"
                   "git://github.com/fuzzycode/test-approve.git"))
      (expect (approve-clone-parse-remote url)
              :to-equal '(:host "github.com" :owner "fuzzycode" :repo "test-approve"))))

  (it "keeps dots in a name and drops only the .git"
    (expect (approve-clone-parse-remote "git@github.com:magit/magit.el.git")
            :to-equal '(:host "github.com" :owner "magit" :repo "magit.el")))

  (it "reads other hosts too, and nothing from a local path"
    (expect (approve-clone-parse-remote "git@git.example.com:team/project.git")
            :to-equal '(:host "git.example.com" :owner "team" :repo "project"))
    (expect (approve-clone-parse-remote "/srv/git/project.git") :to-be nil)
    (expect (approve-clone-parse-remote "../project") :to-be nil)
    (expect (approve-clone-parse-remote nil) :to-be nil)))

(describe "approve-clone-repository"
  (before-each
    (clrhash approve-clone--ssh-hosts))

  (it "names a repository on github.com"
    (let ((approve-api-host nil))
      (expect (approve-clone-repository "https://github.com/magit/magit.git")
              :to-equal '(:host "github.com" :owner "magit" :repo "magit"))))

  (it "names one on the GitHub Enterprise of approve-api-host"
    (let ((approve-api-host "git.example.com/api/v3"))
      (expect (approve-clone-repository "git@git.example.com:team/project.git")
              :to-equal '(:host "git.example.com" :owner "team" :repo "project"))))

  (it "names nothing on other hosts, asking ssh only about SSH ones"
    (let ((approve-api-host nil))
      (spy-on 'approve-clone--ssh-hostname :and-call-fake #'identity)
      (expect (approve-clone-repository "https://gitlab.com/team/project.git") :to-be nil)
      (expect 'approve-clone--ssh-hostname :not :to-have-been-called)
      (expect (approve-clone-repository "git@gitlab.com:team/project.git") :to-be nil)
      (expect 'approve-clone--ssh-hostname :to-have-been-called-with "gitlab.com")))

  (it "follows an SSH alias to the host it stands for"
    (let ((approve-api-host nil))
      (puthash "github-work" "github.com" approve-clone--ssh-hosts)
      (expect (approve-clone-repository "git@github-work:fuzzycode/test-approve.git")
              :to-equal '(:host "github.com" :owner "fuzzycode" :repo "test-approve")))))

(describe "approve-clone-same-repository-p"
  (it "ignores case, as GitHub does"
    (expect (approve-clone-same-repository-p
             '(:host "github.com" :owner "Magit" :repo "magit")
             '(:host "github.com" :owner "magit" :repo "Magit"))
            :to-be-truthy)
    (expect (approve-clone-same-repository-p
             '(:host "github.com" :owner "magit" :repo "magit")
             '(:host "github.com" :owner "magit" :repo "forge"))
            :not :to-be-truthy)
    (expect (approve-clone-same-repository-p nil '(:host "github.com")) :to-be nil)))

(describe "approve-clone-remotes"
  (it "reads the remotes from the config file, in order"
    (test-clone--with-clones
      (let ((one (test-clone--dir "one")))
        (test-clone--git one "remote" "add" "origin" "git@github.com:me/project.git")
        (test-clone--git one "remote" "add" "upstream" "https://github.com/them/project")
        (expect (approve-clone-remotes one)
                :to-equal '(("origin" . "git@github.com:me/project.git")
                            ("upstream" . "https://github.com/them/project")))
        (expect (approve-clone-remote-repository one "upstream")
                :to-equal '(:host "github.com" :owner "them" :repo "project"))
        (expect (approve-clone-remote-repository one "nowhere") :to-be nil))))

  (it "reads a worktree's from the clone it shares them with"
    (test-clone--with-clones
      (let ((one (test-clone--dir "one"))
            (tree (expand-file-name "tree" test-clone--root)))
        (test-clone--git one "remote" "add" "origin" "git@github.com:me/project.git")
        (test-clone--git one "worktree" "add" "-q" "-b" "other" tree)
        (expect (approve-clone-remotes tree)
                :to-equal '(("origin" . "git@github.com:me/project.git"))))))

  (it "is nil for a clone without remotes, or a directory that is none"
    (test-clone--with-clones
      (expect (approve-clone-remotes (test-clone--dir "two")) :to-be nil)
      (expect (approve-clone-remotes (expand-file-name "notes" test-clone--root))
              :to-be nil))))

;;; A buffer's branch

(defmacro test-clone--with-branches (&rest body)
  "Run BODY in the clone \"one\" of `test-clone--with-clones', set up with branches.
Its remotes are origin (me/project) and fork (you/project) on
github.com.  Its branches: feature tracks origin/feature; local-name
tracks origin/other-name; tri pulls from origin/main and pushes to
fork; bare has no config.  Git and Magit run without the user's
config."
  (declare (indent 0) (debug t))
  `(test-clone--with-clones
     (let ((process-environment (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null")
                                        process-environment))
           (approve-api-host nil)
           (one (test-clone--dir "one")))
       (test-clone--git one "remote" "add" "origin" "git@github.com:me/project.git")
       (test-clone--git one "remote" "add" "fork" "https://github.com/you/project")
       (dolist (branch '("feature" "local-name" "tri" "bare"))
         (test-clone--git one "branch" branch))
       (dolist (config '(("branch.feature.remote" "origin")
                         ("branch.feature.merge" "refs/heads/feature")
                         ("branch.local-name.remote" "origin")
                         ("branch.local-name.merge" "refs/heads/other-name")
                         ("branch.tri.remote" "origin")
                         ("branch.tri.merge" "refs/heads/main")
                         ("branch.tri.pushRemote" "fork")))
         (apply #'test-clone--git one "config" config))
       ,@body)))

(defun test-clone--head-on (directory branch)
  "Check out BRANCH where DIRECTORY is; return its :head and :remote."
  (test-clone--git directory "checkout" "-q" branch)
  (let ((head (approve-clone-branch-head directory)))
    (list (plist-get head :remote) (plist-get head :head))))

(describe "approve-clone-branch-head"
  (it "goes where the branch pushes: its own name on the remote it tracks"
    (test-clone--with-branches
      (expect (test-clone--head-on one "feature")
              :to-equal '("origin" (:host "github.com" :owner "me" :repo "project"
                                    :branch "feature")))))

  (it "goes to the branch it tracks under another name"
    (test-clone--with-branches
      (expect (test-clone--head-on one "local-name")
              :to-equal '("origin" (:host "github.com" :owner "me" :repo "project"
                                    :branch "other-name")))))

  (it "goes to the fork it pushes to, not the base it pulls from"
    (test-clone--with-branches
      (expect (test-clone--head-on one "tri")
              :to-equal '("fork" (:host "github.com" :owner "you" :repo "project"
                                  :branch "tri")))))

  (it "goes to the same name on origin when nothing is set"
    (test-clone--with-branches
      (expect (test-clone--head-on one "bare")
              :to-equal '("origin" (:host "github.com" :owner "me" :repo "project"
                                    :branch "bare")))))

  (it "names the clone and the branch, from a subdirectory too"
    (test-clone--with-branches
      (test-clone--git one "checkout" "-q" "feature")
      (make-directory (expand-file-name "lisp" one))
      (let ((head (approve-clone-branch-head (expand-file-name "lisp/" one))))
        (expect (plist-get head :clone) :to-equal one)
        (expect (plist-get head :branch) :to-equal "feature"))))

  (it "works in a worktree, with the clone's remotes"
    (test-clone--with-branches
      (let ((tree (file-name-as-directory (expand-file-name "tree" test-clone--root))))
        (test-clone--git one "worktree" "add" "-q" tree "local-name")
        (let ((head (approve-clone-branch-head tree)))
          (expect (plist-get head :clone) :to-equal tree)
          (expect (plist-get (plist-get head :head) :branch) :to-equal "other-name")))))

  (it "is nil on a detached HEAD, outside a clone, and off GitHub"
    (test-clone--with-branches
      (test-clone--git one "checkout" "-q" "--detach")
      (expect (approve-clone-branch-head one) :to-be nil)
      (expect (approve-clone-branch-head (expand-file-name "notes/" test-clone--root))
              :to-be nil)
      (let ((two (test-clone--dir "two")))
        (expect (approve-clone-branch-head two) :to-be nil)
        (test-clone--git two "remote" "add" "origin" "https://gitlab.com/me/project.git")
        (expect (approve-clone-branch-head two) :to-be nil)))))

;;; The pull requests of a branch

(defconst test-clone--head
  '(:host "github.com" :owner "me" :repo "project" :branch "feature")
  "The branch whose pull requests the tests ask for.")

(defun test-clone--pr (id state updated &rest props)
  "Return a pull request ID in STATE, UPDATED at that time, as GitHub sends it.
PROPS may give :number, :head (the head repository), :branch and :draft."
  `((__typename . "PullRequest") (id . ,id)
    (number . ,(or (plist-get props :number) 1))
    (title . ,(format "Pull request %s" id))
    (state . ,state) (isDraft . ,(if (plist-get props :draft) t :json-false))
    (updatedAt . ,updated) (baseRefName . "main")
    (headRefName . ,(or (plist-get props :branch) "feature"))
    (headRefOid . "abc")
    (headRepository (__typename . "Repository") (id . "R_head")
                    (nameWithOwner . ,(or (plist-get props :head) "me/project")))))

(defun test-clone--answer (&rest lists)
  "Return the data of an answer whose LISTS are (:ref :own :parent) nodes."
  `((repository
     (__typename . "Repository") (id . "R_head")
     (ref . ,(and (plist-member lists :ref)
                  `((__typename . "Ref") (id . "REF")
                    (associatedPullRequests (nodes ,@(plist-get lists :ref))))))
     (pullRequests (nodes ,@(plist-get lists :own)))
     (parent . ,(and (plist-member lists :parent)
                     `((__typename . "Repository") (id . "R_base")
                       (pullRequests (nodes ,@(plist-get lists :parent)))))))
    (rateLimit (cost . 1) (remaining . 4999) (resetAt . "2026-11-16T20:00:00Z"))))

(describe "approve-clone--branch-pull-requests"
  (it "keeps the branch's own, once each, open first and the latest first"
    (let* ((merged (test-clone--pr "PR_1" "MERGED" "2026-11-16T10:00:00Z"))
           (old (test-clone--pr "PR_2" "OPEN" "2026-11-10T10:00:00Z"))
           (new (test-clone--pr "PR_3" "OPEN" "2026-11-15T10:00:00Z"))
           (fork (test-clone--pr "PR_4" "OPEN" "2026-11-16T11:00:00Z" :head "someone/project"))
           (other (test-clone--pr "PR_5" "OPEN" "2026-11-16T11:00:00Z" :branch "other"))
           (prs (approve-clone--branch-pull-requests
                 (test-clone--answer :ref (list old merged) :own (list new old fork other)
                                     :parent (list new))
                 test-clone--head)))
      (expect (mapcar (lambda (pr) (alist-get 'id pr)) prs)
              :to-equal '("PR_3" "PR_2" "PR_1"))))

  (it "matches the head repository whatever its case, and copes without lists"
    (let ((pr (test-clone--pr "PR_1" "OPEN" "2026-11-16T10:00:00Z" :head "Me/Project")))
      (expect (length (approve-clone--branch-pull-requests
                       (test-clone--answer :own (list pr)) test-clone--head))
              :to-equal 1)
      (expect (approve-clone--branch-pull-requests (test-clone--answer) test-clone--head)
              :to-be nil))))

(defvar test-clone--sent nil
  "Requests the fake transport received, newest first.")

(defvar test-clone--reply nil
  "Function of a request giving the data of its answer, or nil to fail it.")

(defun test-clone--transport (request)
  "Answer REQUEST with `test-clone--reply', later."
  (push request test-clone--sent)
  (let ((timer (if test-clone--reply
                   (run-at-time 0 nil (plist-get request :on-success)
                                `((data . ,(funcall test-clone--reply request))) nil)
                 (run-at-time 0 nil (plist-get request :on-failure)
                              '(:status 502 :payload ((message . "Bad Gateway")))))))
    (lambda () (cancel-timer timer))))

(defmacro test-clone--with-api (&rest body)
  "Run BODY with the fake transport and empty caches."
  (declare (indent 0) (debug t))
  `(let ((approve-api-transport #'test-clone--transport)
         (approve-api-timeout nil)
         (approve-api-host nil)
         (inhibit-message t)
         (test-clone--sent nil)
         (test-clone--reply
          (lambda (_request)
            (test-clone--answer
             :own (list (test-clone--pr "PR_1" "OPEN" "2026-11-16T10:00:00Z" :number 15)
                        (test-clone--pr "PR_2" "OPEN" "2026-11-16T12:00:00Z" :number 16))))))
     (clrhash approve-clone--pull-requests)
     (clrhash approve-clone--asking)
     (clrhash approve-clone--picked)
     ,@body))

(describe "approve-clone-branch-pull-requests"
  (it "asks the head repository about the branch, on its host"
    (test-clone--with-api
      (let (prs)
        (approve-clone-branch-pull-requests test-clone--head :callback (lambda (v) (setq prs v)))
        (approve-test-wait-for (lambda () prs))
        (let ((request (car test-clone--sent)))
          (expect (plist-get request :name) :to-equal "queries/branch-pull-requests.graphql")
          (expect (plist-get request :variables)
                  :to-equal '((owner . "me") (name . "project") (branch . "feature")
                              (qualifiedName . "refs/heads/feature")))
          (expect (plist-get request :host) :to-equal "api.github.com"))
        (expect (mapcar (lambda (pr) (alist-get 'number pr)) prs) :to-equal '(16 15)))))

  (it "asks once a session, and once for questions at the same time"
    (test-clone--with-api
      (let (one two three)
        (approve-clone-branch-pull-requests test-clone--head :callback (lambda (v) (setq one v)))
        (approve-clone-branch-pull-requests test-clone--head :callback (lambda (v) (setq two v)))
        (approve-test-wait-for (lambda () (and one two)))
        (approve-clone-branch-pull-requests test-clone--head :callback (lambda (v) (setq three v)))
        (expect three :to-equal one)
        (expect (length test-clone--sent) :to-equal 1)
        (approve-clone-branch-pull-requests test-clone--head :refresh t
                                            :callback (lambda (v) (setq three (list v))))
        (approve-test-wait-for (lambda () (= (length three) 1)))
        (expect (length test-clone--sent) :to-equal 2))))

  (it "keeps no failure, and answers in the buffer that asked, if it lives"
    (test-clone--with-api
      (let ((test-clone--reply nil) (failed nil) (answered nil)
            (asker (generate-new-buffer " asker"))
            (gone (generate-new-buffer " gone")))
        (with-current-buffer asker
          (approve-clone-branch-pull-requests
           test-clone--head :errorback (lambda (_) (setq failed (current-buffer)))))
        (with-current-buffer gone
          (approve-clone-branch-pull-requests
           test-clone--head :errorback (lambda (_) (setq answered t))))
        (kill-buffer gone)
        (approve-test-wait-for (lambda () failed))
        (expect failed :to-be asker)
        (expect answered :to-be nil)
        (kill-buffer asker)
        (setq test-clone--reply (lambda (_) (test-clone--answer)))
        (let ((prs 'none))
          (approve-clone-branch-pull-requests test-clone--head :callback (lambda (v) (setq prs v)))
          (approve-test-wait-for (lambda () (not (eq prs 'none))))
          (expect prs :to-be nil)
          (expect (length test-clone--sent) :to-equal 2))))))

(describe "approve-clone-shown-pull-request"
  (it "is the one picked, else the open one updated last, else none"
    (test-clone--with-api
      (let ((open (test-clone--pr "PR_1" "OPEN" "2026-11-15T10:00:00Z"))
            (other (test-clone--pr "PR_2" "OPEN" "2026-11-14T10:00:00Z"))
            (merged (test-clone--pr "PR_3" "MERGED" "2026-11-16T10:00:00Z")))
        (expect (approve-clone-shown-pull-request test-clone--head (list open other merged))
                :to-be open)
        (puthash (approve-clone--head-key test-clone--head) "PR_2" approve-clone--picked)
        (expect (approve-clone-shown-pull-request test-clone--head (list open other merged))
                :to-be other)
        (clrhash approve-clone--picked)
        (expect (approve-clone-shown-pull-request test-clone--head (list merged)) :to-be nil)
        (expect (approve-clone-shown-pull-request test-clone--head (list merged) t)
                :to-be merged)))))

(describe "approve-clone-pick-pull-request"
  (it "offers the branch's pull requests and keeps the one picked"
    (test-clone--with-api
      (let ((picked nil) (offered nil))
        (spy-on 'approve-clone-branch-head
                :and-return-value (list :clone "/tmp/x/" :branch "feature" :remote "origin"
                                        :head test-clone--head))
        (spy-on 'completing-read
                :and-call-fake (lambda (_prompt collection &rest args)
                                 (setq offered (all-completions "" collection))
                                 (expect (nth 4 args) :to-match "\\`#16 ")
                                 (car (last offered))))
        (let ((approve-clone-picked-functions
               (list (lambda (head pr) (setq picked (list head (alist-get 'number pr)))))))
          (approve-clone-pick-pull-request))
        (expect (length offered) :to-equal 2)
        (expect (car offered) :to-match "\\`#16 Pull request PR_2  open · into main · ")
        (expect picked :to-equal (list test-clone--head 15))
        (expect (alist-get 'number (approve-clone-shown-pull-request
                                    test-clone--head (approve-clone-wait-pull-requests
                                                      test-clone--head)))
                :to-equal 15))))

  (it "says so where there is no branch, or no pull request"
    (test-clone--with-api
      (spy-on 'approve-clone-branch-head :and-return-value nil)
      (expect (approve-clone-pick-pull-request) :to-throw 'user-error)
      (spy-on 'approve-clone-branch-head
              :and-return-value (list :clone "/tmp/x/" :branch "feature" :head test-clone--head))
      (setq test-clone--reply (lambda (_) (test-clone--answer)))
      (expect (approve-clone-pick-pull-request) :to-throw 'user-error))))

(describe "approve-clone-set-pull-request"
  (it "makes a pull request, of any state, the one a branch shows"
    (test-clone--with-api
      (let* ((open (test-clone--pr "PR_1" "OPEN" "2026-11-15T10:00:00Z"))
             (merged (test-clone--pr "PR_3" "MERGED" "2026-11-16T10:00:00Z"))
             (told nil)
             (approve-clone-picked-functions (list (lambda (head pr) (push (cons head pr) told)))))
        (approve-clone-set-pull-request test-clone--head merged)
        (expect (approve-clone-shown-pull-request test-clone--head (list open merged))
                :to-be merged)
        (expect told :to-equal (list (cons test-clone--head merged)))))))

(describe "approve-clone-wait-pull-requests"
  (it "asks GitHub once a session, and again when told to"
    (test-clone--with-api
      (expect (mapcar (lambda (pr) (alist-get 'number pr))
                      (approve-clone-wait-pull-requests test-clone--head))
              :to-equal '(16 15))
      (approve-clone-wait-pull-requests test-clone--head)
      (expect (length test-clone--sent) :to-equal 1)
      (setq test-clone--reply (lambda (_) (test-clone--answer)))
      (expect (approve-clone-wait-pull-requests test-clone--head t) :to-be nil)
      (expect (length test-clone--sent) :to-equal 2))))

;;; The clone's state

(describe "approve-clone-state"
  (it "says whether HEAD is GitHub's head, ahead, behind or diverged from it"
    (test-clone--with-clones
      (let* ((one (test-clone--dir "one"))
             (first (alist-get "one" test-clone--shas nil nil #'equal))
             (second (progn (test-clone--git one "commit" "-q" "--allow-empty" "-m" "2")
                            (test-clone--git one "rev-parse" "HEAD")))
             (third (progn (test-clone--git one "commit" "-q" "--allow-empty" "-m" "3")
                           (test-clone--git one "rev-parse" "HEAD"))))
        (expect (approve-clone-state one third) :to-equal '(:state same :ahead 0 :behind 0))
        (expect (approve-clone-state one first) :to-equal '(:state ahead :ahead 2 :behind 0))
        (test-clone--git one "checkout" "-q" "--detach" first)
        (expect (approve-clone-state one third) :to-equal '(:state behind :ahead 0 :behind 2))
        (test-clone--git one "commit" "-q" "--allow-empty" "-m" "rewritten")
        (expect (approve-clone-state one second)
                :to-equal '(:state diverged :ahead 1 :behind 1)))))

  (it "says the clone lacks a commit it hasn't"
    (test-clone--with-clones
      (expect (approve-clone-state (test-clone--dir "one") (make-string 40 ?a))
              :to-equal '(:state missing))
      (expect (approve-clone-state (test-clone--dir "one")
                                   (alist-get "two" test-clone--shas nil nil #'equal))
              :to-equal '(:state missing)))))

(describe "approve-clone-head"
  (it "reads the branch and its commit from the clone's files, packed or not"
    (test-clone--with-clones
      (let* ((one (test-clone--dir "one"))
             (branch (test-clone--git one "rev-parse" "--abbrev-ref" "HEAD"))
             (first (alist-get "one" test-clone--shas nil nil #'equal)))
        (expect (approve-clone-head one) :to-equal (cons branch first))
        (test-clone--git one "commit" "-q" "--allow-empty" "-m" "Second")
        (let ((second (test-clone--git one "rev-parse" "HEAD")))
          (expect (approve-clone-head one) :to-equal (cons branch second))
          (test-clone--git one "pack-refs" "--all")
          (expect (file-exists-p (expand-file-name (concat ".git/refs/heads/" branch) one))
                  :to-be nil)
          (expect (approve-clone-head one) :to-equal (cons branch second))
          (test-clone--git one "checkout" "-q" "--detach" first)
          (expect (approve-clone-head one) :to-equal (cons nil first))))))

  (it "reads a worktree's own HEAD, and nothing outside a clone"
    (test-clone--with-clones
      (let ((one (test-clone--dir "one"))
            (tree (expand-file-name "tree" test-clone--root)))
        (test-clone--git one "worktree" "add" "-q" "-b" "other" tree)
        (test-clone--git tree "commit" "-q" "--allow-empty" "-m" "Other")
        (expect (approve-clone-head tree)
                :to-equal (cons "other" (test-clone--git tree "rev-parse" "HEAD")))
        (expect (approve-clone-head (expand-file-name "notes" test-clone--root))
                :to-be nil)))))

(describe "approve-clone-file-text"
  (it "reads a file at a commit as git has it, carriage returns included"
    (test-clone--with-clones
      (let ((one (test-clone--dir "one")))
        (make-directory (expand-file-name "src" one))
        (with-temp-file (expand-file-name "src/main.c" one)
          (set-buffer-file-coding-system 'utf-8-unix)
          (insert "int main() {\r\n  return 0; /* å */\r\n}"))
        (test-clone--git one "add" ".")
        (test-clone--git one "commit" "-q" "-m" "Main")
        (let ((oid (test-clone--git one "rev-parse" "HEAD")))
          (expect (approve-clone-file-text one oid "src/main.c")
                  :to-equal "int main() {\r\n  return 0; /* å */\r\n}")
          (expect (approve-clone-file-text one (substring oid 0 7) "src/main.c")
                  :to-equal "int main() {\r\n  return 0; /* å */\r\n}")))))

  (it "gives \"\" for a file the commit hasn't, and nil for a commit the clone hasn't"
    (test-clone--with-clones
      (let ((one (test-clone--dir "one")))
        (expect (approve-clone-file-text one (alist-get "one" test-clone--shas nil nil #'equal)
                                         "src/main.c")
                :to-equal "")
        (expect (approve-clone-file-text one (alist-get "two" test-clone--shas nil nil #'equal)
                                         "src/main.c")
                :to-be nil)
        (expect (approve-clone-file-text one "not a sha" "src/main.c") :to-be nil)))))

;;; A pull request's worktree

(defconst test-clone--pr-head
  '(:host "github.com" :owner "me" :repo "project" :branch "feature")
  "The head of the pull request whose worktree the tests look for.")

(defconst test-clone--pr-base
  '(:host "github.com" :owner "them" :repo "project")
  "The base repository of that pull request.")

(defmacro test-clone--with-worktrees (&rest body)
  "Run BODY with clones of the pull request's repositories, and worktrees.
Clone \"one\" has origin me/project; its branch feature (tracking
origin/feature) is checked out in the worktree trees/pushes, and mine
\(tracking origin/feature too) in trees/tracks, which are not listed.  Clone \"two\" has
upstream them/project and a branch feature of its own, which goes
nowhere.  The clone \"three\", elsewhere, has nothing to do with them."
  (declare (indent 0) (debug t))
  `(test-clone--with-clones
     (let* ((process-environment (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null")
                                         process-environment))
            (approve-api-host nil)
            (one (test-clone--dir "one"))
            (two (test-clone--dir "two"))
            (pushes (file-name-as-directory (expand-file-name "trees/pushes" test-clone--root)))
            (tracks (file-name-as-directory (expand-file-name "trees/tracks" test-clone--root))))
       (clrhash approve-clone--worktrees)
       (test-clone--make (expand-file-name "three" test-clone--root) "Third")
       (test-clone--git (test-clone--dir "three") "remote" "add" "origin"
                        "https://github.com/me/other.git")
       (test-clone--git one "remote" "add" "origin" "git@github.com:me/project.git")
       (test-clone--git two "remote" "add" "upstream" "https://github.com/them/project")
       (test-clone--git two "branch" "feature")
       (dolist (branch '("feature" "mine"))
         (test-clone--git one "branch" branch)
         (test-clone--git one "config" (format "branch.%s.remote" branch) "origin")
         (test-clone--git one "config" (format "branch.%s.merge" branch) "refs/heads/feature"))
       (test-clone--git one "worktree" "add" "-q" pushes "feature")
       (test-clone--git one "worktree" "add" "-q" tracks "mine")
       ,@body)))

(defun test-clone--truenames (worktrees)
  "Return the true names of the :worktree of WORKTREES, with their :how."
  (mapcar (lambda (worktree)
            (list (file-truename (plist-get worktree :worktree)) (plist-get worktree :how)))
          worktrees))

(describe "approve-clone-repository-clones"
  (it "finds the clones of the head and the base repository"
    (test-clone--with-worktrees
      (expect (sort (approve-clone-repository-clones (list test-clone--pr-head test-clone--pr-base))
                    #'string<)
              :to-equal (list one two))
      (expect (approve-clone-repository-clones (list test-clone--pr-base))
              :to-equal (list two)))))

(describe "approve-clone-pull-request-worktrees"
  (it "finds the worktrees with the head branch, the one that pushes first"
    (test-clone--with-worktrees
      (expect (test-clone--truenames
               (approve-clone-pull-request-worktrees test-clone--pr-head test-clone--pr-base))
              :to-equal (list (list (file-truename pushes) 'push)
                              (list (file-truename tracks) 'upstream)))))

  (it "lists each once, also when the worktrees are listed as clones"
    (test-clone--with-worktrees
      (let ((approve-clone-directories
             (list (cons test-clone--root 1)
                   (cons (expand-file-name "trees" test-clone--root) 1))))
        (expect (length (approve-clone-pull-request-worktrees
                         test-clone--pr-head test-clone--pr-base))
                :to-equal 2))))

  (it "finds none for a branch no clone has checked out"
    (test-clone--with-worktrees
      (expect (approve-clone-pull-request-worktrees
               (plist-put (copy-sequence test-clone--pr-head) :branch "other")
               test-clone--pr-base)
              :to-be nil))))

(describe "approve-clone-pull-request-worktree"
  (it "takes the one that pushes to the head branch at once"
    (test-clone--with-worktrees
      (spy-on 'completing-read)
      (expect (file-truename (approve-clone-pull-request-worktree
                              test-clone--pr-head test-clone--pr-base))
              :to-equal (file-truename pushes))
      (expect 'completing-read :not :to-have-been-called)))

  (it "asks among those as good, once a session"
    (test-clone--with-worktrees
      (test-clone--git one "config" "branch.feature.merge" "refs/heads/elsewhere")
      (test-clone--git one "branch" "-m" "feature" "local")
      (test-clone--git one "config" "branch.local.merge" "refs/heads/feature")
      (spy-on 'completing-read
              :and-call-fake (lambda (_prompt choices &rest _)
                               (expect (length choices) :to-equal 2)
                               (car (seq-find (lambda (choice)
                                                (string-match-p "tracks/" (car choice)))
                                              choices))))
      (let ((first (approve-clone-pull-request-worktree test-clone--pr-head test-clone--pr-base)))
        (expect (file-truename first) :to-equal (file-truename tracks))
        (expect (approve-clone-pull-request-worktree test-clone--pr-head test-clone--pr-base)
                :to-equal first)
        (expect 'completing-read :to-have-been-called-times 1))))

  (it "is nil when no clone has the branch"
    (test-clone--with-worktrees
      (expect (approve-clone-pull-request-worktree
               (plist-put (copy-sequence test-clone--pr-head) :branch "other")
               test-clone--pr-base)
              :to-be nil))))

;;; test-approve-clone.el ends here
