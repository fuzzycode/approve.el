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

;;; test-approve-clone.el ends here
