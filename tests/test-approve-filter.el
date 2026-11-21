;;; test-approve-filter.el --- Tests for the filters of the diff -*- lexical-binding: t; -*-

;;; Commentary:

;; The filters of the diff (decision D54) in the medium PR: turning
;; them on and off, how they combine, what the diff and the file list
;; then draw and say, the regexp and code-owner filters, and the jumps
;; that follow them.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-menus)
(require 'approve-test-helpers "helpers")

(defun test-filter--suffix (suffix)
  "Return a filter function keeping the files whose path ends with SUFFIX."
  (lambda ()
    (lambda (file) (string-suffix-p suffix (alist-get 'path file)))))

(defconst test-filter--filters
  `(("g" "Go files" ,(test-filter--suffix ".go"))
    ("m" "Markdown" ,(test-filter--suffix ".md"))
    ("_" "Tests" ,(lambda ()
                    (list :predicate (lambda (file)
                                       (string-match-p "_test" (alist-get 'path file)))
                          :label "path ~ _test")))
    ("0" "Nothing" ,(lambda () #'ignore))
    ("!" "Broken" ,(lambda () (lambda (_file) (error "Broken filter")))))
  "Filters for the tests, on top of Approve's own.")

(defmacro test-filter--with-medium (&rest body)
  "Run BODY in a freshly loaded medium PR buffer, with the test filters."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let ((approve-filters (append test-filter--filters approve-filters))
           (approve-filter-combine 'and)
           (buffer (approve-test-open-pr "medium")))
       (unwind-protect
           (with-current-buffer buffer
             (goto-char (point-min))
             ,@body)
         (kill-buffer buffer)))))

(defun test-filter--heading ()
  "Return the first line of the diff's section."
  (car (split-string (approve-test-section-text
                      (approve-test-find-section 'approve-diff-section))
                     "\n")))

(defun test-filter--paths ()
  "Return the paths of the files the diff draws, in order."
  (mapcar (lambda (section) (oref section value))
          (oref (approve-test-find-section 'approve-diff-section) children)))

(describe "The filters of the diff"
  (it "draw only the files that pass, and the heading says how many of all"
    (test-filter--with-medium
      (approve-filter-toggle "g")
      (expect (length (test-filter--paths)) :to-equal 19)
      (expect (seq-every-p (lambda (path) (string-suffix-p ".go" path)) (test-filter--paths))
              :to-be-truthy)
      (expect (test-filter--heading)
              :to-match (concat "\\`Diff (19 of 30) +filter: Go files · 4/19 viewed"
                                " · [0-9]+ unresolved threads? · [0-9]+ new comments?\\'"))
      (expect (approve-filter-summary) :to-equal "19 of 30 files")))

  (it "count only the threads and new comments of the files shown"
    (test-filter--with-medium
      (approve-filter-toggle "m")
      ;; One Markdown file has a thread on a line; none has a new comment.
      (expect (test-filter--heading)
              :to-match "\\`Diff (3 of 30) +filter: Markdown · 0/3 viewed · 1 unresolved thread\\'")))

  (it "turn off with their key again, and all at once"
    (test-filter--with-medium
      (approve-filter-toggle "g")
      (approve-filter-toggle "g")
      (expect (length (test-filter--paths)) :to-equal 30)
      (expect (test-filter--heading) :to-match "\\`Diff (30) +8/30 viewed")
      (approve-filter-toggle "g")
      (approve-filter-toggle "m")
      (approve-filter-clear)
      (expect (approve-session-get :filters) :to-be nil)
      (expect (length (test-filter--paths)) :to-equal 30)))

  (it "keep the files that pass every filter, or one of them, as asked"
    (test-filter--with-medium
      (approve-filter-toggle "g")
      (approve-filter-toggle "_")
      (expect (length (test-filter--paths)) :to-equal 6)
      (expect (test-filter--heading)
              :to-match "\\`Diff (6 of 30) +filter: Go files and path ~ _test · ")
      (approve-filter-toggle "_")
      (approve-filter-toggle "m")
      (expect (length (test-filter--paths)) :to-equal 0)
      (approve-filter-toggle-combine)
      (expect (length (test-filter--paths)) :to-equal 22)
      (expect (test-filter--heading)
              :to-match "\\`Diff (22 of 30) +filter: Go files or Markdown · ")
      (approve-filter-toggle-combine)
      (expect (approve-filter-combination) :to-be 'and)))

  (it "start combined as approve-filter-combine says"
    (test-filter--with-medium
      (let ((approve-filter-combine 'or))
        (approve-filter-toggle "g")
        (approve-filter-toggle "m")
        (expect (length (test-filter--paths)) :to-equal 22))))

  (it "say when no file passes, and how to change it"
    (test-filter--with-medium
      (approve-filter-toggle "0")
      (expect (test-filter--heading) :to-match "\\`Diff (0 of 30) +filter: Nothing\\'")
      (expect (approve-test-section-text (approve-test-find-section 'approve-diff-section))
              :to-match "No file passes the filter · = change it")))

  (it "let every file pass a filter that fails, and still draw"
    (test-filter--with-medium
      (let ((debug-on-error nil))
        (approve-filter-toggle "!"))
      (expect (length (test-filter--paths)) :to-equal 30)
      (expect (test-filter--heading) :to-match "\\`Diff (30 of 30) +filter: Broken")))

  (it "hold across a redraw and a refresh, until the buffer is killed"
    (test-filter--with-medium
      (approve-filter-toggle "g")
      (approve-pr-refresh)
      (approve-test-wait-for (lambda () (not (approve-pr-loading-p))))
      (approve-pr-redraw)
      (expect (length (test-filter--paths)) :to-equal 19)))

  (it "filter the file list too, when it is shown"
    (test-filter--with-medium
      (approve-test-show-file-list)
      (approve-filter-toggle "_")
      (let ((list (approve-test-find-section 'approve-files-section)))
        (expect (length (oref list children)) :to-equal 6)
        (expect (car (split-string (approve-test-section-text list) "\n"))
                :to-match "\\`Files changed (6 of 30) +\\+[0-9]+ −[0-9]+ +filter: path ~ _test"))))

  (it "need a pull request that has loaded"
    (approve-test-with-fixture "medium"
      (let ((buffer (approve-pr-setup-buffer (approve-test-pr-plist "medium"))))
        (unwind-protect
            (with-current-buffer buffer
              (expect (approve-filter-toggle "r") :to-throw 'user-error))
          (kill-buffer buffer)))))

  (it "can be registered, replacing the filter a key had"
    (let ((approve-filters (copy-tree approve-filters)))
      (approve-filter-register "x" "Extra" #'ignore)
      (approve-filter-register "r" "Other regexp" #'ignore)
      (expect (car (last approve-filters)) :to-equal '("x" "Extra" ignore))
      (expect (assoc "r" approve-filters) :to-equal '("r" "Other regexp" ignore))
      (expect (approve-filter-register "&" "Taken" #'ignore) :to-throw 'error))))

(describe "The regexp filter"
  (it "keeps the files whose path matches what you type"
    (test-filter--with-medium
      (spy-on 'read-regexp :and-return-value "^internal/\\(store\\|util\\)/")
      (approve-filter-toggle "r")
      (expect (test-filter--paths)
              :to-equal '("internal/store/migrations.go" "internal/store/store.go"
                          "internal/store/store_test.go" "internal/util/strings.go"))
      (expect (test-filter--heading)
              :to-match "filter: path ~ \\^internal/\\\\(store\\\\|util\\\\)/")))

  (it "minds case only when the regexp has an upper-case letter"
    (test-filter--with-medium
      (spy-on 'read-regexp :and-return-value "readme")
      (approve-filter-toggle "r")
      (expect (test-filter--paths) :to-equal '("README.md"))
      (approve-filter-toggle "r")
      (spy-on 'read-regexp :and-return-value "Readme")
      (approve-filter-toggle "r")
      (expect (test-filter--paths) :to-equal nil)))

  (it "stays off without a regexp, or with one Emacs can't read"
    (test-filter--with-medium
      (spy-on 'read-regexp :and-return-value "")
      (expect (approve-filter-toggle "r") :to-throw 'user-error)
      (spy-on 'read-regexp :and-return-value "\\(")
      (expect (approve-filter-toggle "r") :to-throw 'user-error)
      (expect (approve-session-get :filters) :to-be nil))))

(describe "The code-owner filters"
  :var (codeowners teams)
  (before-each
    (setq codeowners (list :path ".github/CODEOWNERS" :branch "main"
                           :rules (approve-codeowners-parse
                                   (concat "* @acme-example/core\n"
                                           "/internal/parser/ @Vera-Example\n"
                                           "/pkg/ @acme-example/web\n"
                                           "/pkg/api/types.go @acme-example/eng\n")))
          teams '(((combinedSlug . "acme-example/web")
                   (ancestors (nodes ((combinedSlug . "acme-example/eng")))))))
    (spy-on 'approve-metadata-fetch)
    (spy-on 'approve-metadata-wait
            :and-call-fake (lambda (kind)
                             (pcase kind
                               ('codeowners codeowners)
                               ('viewer-teams teams)))))

  (it "keep the files you own, by CODEOWNERS on the base branch"
    (test-filter--with-medium
      (approve-filter-toggle "o")
      (expect (test-filter--paths)
              :to-equal '("internal/parser/lexer.go" "internal/parser/parser.go"
                          "internal/parser/parser_test.go"
                          "internal/parser/testdata/basic.golden"))
      (expect (test-filter--heading) :to-match "filter: owned by @vera-example")))

  (it "keep the files your teams own, and their parents"
    (test-filter--with-medium
      (approve-filter-toggle "t")
      (expect (test-filter--paths)
              :to-equal '("pkg/api/client.go" "pkg/api/client_test.go" "pkg/api/types.go"))
      (expect (test-filter--heading) :to-match "filter: owned by @acme-example/web")
      (expect 'approve-metadata-fetch :to-have-been-called-with 'viewer-teams)))

  (it "combine: yours or your teams'"
    (test-filter--with-medium
      (approve-filter-toggle "o")
      (approve-filter-toggle "t")
      (expect (test-filter--paths) :to-equal nil)
      (approve-filter-toggle-combine)
      (expect (length (test-filter--paths)) :to-equal 7)))

  (it "stay off when the branch has no CODEOWNERS, or you are in no team"
    (test-filter--with-medium
      (setq teams nil)
      (expect (approve-filter-toggle "t") :to-throw 'user-error)
      (setq codeowners nil)
      (expect (approve-filter-toggle "o") :to-throw 'user-error)
      (expect (approve-session-get :filters) :to-be nil))))

(describe "Moving through a filtered diff"
  (it "goes to the unviewed files that pass, and nowhere else"
    (test-filter--with-medium
      (approve-filter-toggle "_")
      (goto-char (oref (approve-test-find-section 'approve-diff-section) start))
      (let ((seen nil))
        (condition-case nil
            (while t
              (approve-next-unviewed-file)
              (push (oref (magit-current-section) value) seen))
          (user-error nil))
        ;; parser_test.go is DISMISSED, the others UNVIEWED: all six.
        (expect (length seen) :to-equal 6))))

  (it "offers the files that pass, and says a hidden one is hidden"
    (test-filter--with-medium
      (approve-filter-toggle "m")
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (expect collection :to-equal '("CHANGELOG.md" "README.md" "docs/usage.md"))
                "README.md"))
      (call-interactively #'approve-jump-file)
      (expect (oref (magit-current-section) value) :to-equal "README.md")
      (expect (approve-nav-goto-file "go.mod") :to-throw 'user-error)
      (expect (condition-case err (approve-nav-goto-file "go.mod")
                (user-error (cadr err)))
              :to-equal "The filter of the diff hides go.mod; = changes it")))

  (it "offers the threads of the files that pass"
    (test-filter--with-medium
      (approve-filter-toggle "g")
      (let ((paths (delete-dups (mapcar (lambda (candidate)
                                          (plist-get (cdr candidate) :path))
                                        (approve-nav--thread-candidates nil)))))
        (expect paths :not :to-be nil)
        (expect (seq-every-p (lambda (path) (string-suffix-p ".go" path)) paths)
                :to-be-truthy))
      (approve-filter-toggle "g")
      (approve-filter-toggle "0")
      (expect (approve-action-why-not 'approve-jump-thread)
              :to-equal "The filter of the diff hides every thread"))))

(defun test-filter--suffix-plist (suffix)
  "Return the plist of SUFFIX, a suffix as `transient-parse-suffix' gives it.
\(CLASS . PLIST) from transient 0.8, (LEVEL CLASS PLIST) before."
  (if (keywordp (cadr suffix)) (cdr suffix) (nth 2 suffix)))

(describe "The filter menu"
  (it "opens on =, and from the dispatch"
    (expect (keymap-lookup approve-pr-mode-map "=") :to-be 'approve-filter-menu)
    (expect (get 'approve-filter-menu 'transient--prefix) :to-be-truthy))

  (it "lists the filters of approve-filters, saying which are on and what they keep"
    (test-filter--with-medium
      (approve-filter-toggle "_")
      (let ((suffixes (mapcar (lambda (suffix)
                                (let ((plist (test-filter--suffix-plist suffix)))
                                  (list (plist-get plist :key)
                                        (substring-no-properties
                                         (plist-get plist :description)))))
                              (approve-menus--filter-suffixes nil))))
        (expect (assoc "g" suffixes) :to-equal '("g" "Go files"))
        (expect (assoc "_" suffixes) :to-equal '("_" "Tests (path ~ _test)"))
        (expect (assoc "o" suffixes) :to-equal '("o" "I am a code owner")))
      (approve-filter-toggle "g")
      (expect (substring-no-properties (approve-menus--filter-description
                                        (assoc "g" approve-filters)))
              :to-equal "Go files (on)")
      (call-interactively (approve-menus--filter-command "g"))
      (expect (approve-filter-get "g") :to-be nil))))

(provide 'test-approve-filter)

;;; test-approve-filter.el ends here
