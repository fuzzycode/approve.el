;;; test-approve-feedback.el --- Tests for the feedback mode -*- lexical-binding: t; -*-

;;; Commentary:

;; The feedback mode (flow F21, decisions D45 and D46) in a buffer of
;; the medium PR's internal/parser/parser.go, in a temporary directory
;; that stands for the clone: the branch, its pull requests and the
;; clone's state are the clone module's answers, faked here; the pull
;; request loads through the fake API.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-feedback)
(require 'approve-test-helpers "helpers")

(defconst test-feedback--path "internal/parser/parser.go"
  "The medium PR's file with threads of every kind.")

(defconst test-feedback--head
  '(:host "github.com" :owner "acme-example" :repo "rocket" :branch "parser-rewrite")
  "The branch of the clone on GitHub.")

(defun test-feedback--row (number &optional state)
  "Return a pull request of the branch: NUMBER in STATE, open by default."
  `((__typename . "PullRequest") (id . ,(format "PR_%d" number)) (number . ,number)
    (url . ,(format "https://github.com/acme-example/rocket/pull/%d" number))
    (title . ,(format "Pull request %d" number)) (state . ,(or state "OPEN")) (isDraft)
    (updatedAt . "2026-10-03T10:00:00Z") (baseRefName . "main")
    (headRefName . "parser-rewrite")
    (repository (__typename . "Repository") (id . "R_1")
                (nameWithOwner . "acme-example/rocket"))))

(defvar test-feedback--prs nil
  "The pull requests of the clone's branch, as GitHub would list them.")

(defvar test-feedback--root nil
  "The temporary directory that stands for the clone.")

(defun test-feedback--pr-buffers ()
  "Return the live PR buffers."
  (seq-filter (lambda (buffer) (eq (buffer-local-value 'major-mode buffer) 'approve-pr-mode))
              (buffer-list)))

(defmacro test-feedback--with-clone (&rest body)
  "Run BODY in a buffer of the medium PR's file in a fake clone.
The branch heads `test-feedback--prs' (the medium PR #202 and #203,
open, unless BODY binds it otherwise), and is a commit ahead of
GitHub.  The buffer, the PR buffers and the clone are gone after."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let* ((test-feedback--root (file-name-as-directory
                                  (make-temp-file "approve-feedback-" t)))
            (file (expand-file-name test-feedback--path test-feedback--root)))
       (make-directory (file-name-directory file) t)
       (with-temp-file file
         (dotimes (n 600)
           (insert (format "line %d\n" (1+ n)))))
       (spy-on 'approve-clone-branch-head
               :and-return-value (list :clone test-feedback--root :branch "parser-rewrite"
                                       :remote "origin" :head test-feedback--head))
       (spy-on 'approve-clone-wait-pull-requests
               :and-call-fake (lambda (_head) test-feedback--prs))
       (spy-on 'approve-clone-state :and-return-value '(:state ahead :ahead 1 :behind 0))
       (let ((buffer (find-file-noselect file)))
         (unwind-protect
             (with-current-buffer buffer
               ,@body)
           (with-current-buffer buffer
             (set-buffer-modified-p nil))
           (kill-buffer buffer)
           (mapc #'kill-buffer (test-feedback--pr-buffers))
           (delete-directory test-feedback--root t))))))

(defun test-feedback--wait ()
  "Wait until the pull request of the current buffer has loaded."
  (approve-test-wait-for (lambda () (and (approve-feedback--loaded-p)
                                         (not (approve-pr-loading-p)))))
  (with-current-buffer approve-model-store-buffer
    (approve-pr-redraw)))

(defun test-feedback--lighter ()
  "Return the lighter of the current buffer, without its properties."
  (substring-no-properties approve-feedback--lighter))

(describe "approve-feedback-mode"
  (it "shows the branch's pull request, loaded in a PR buffer it doesn't show"
    (let ((test-feedback--prs (list (test-feedback--row 202) (test-feedback--row 203))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (expect approve-feedback-mode :to-be t)
        (expect (buffer-name approve-model-store-buffer)
                :to-equal "*approve: acme-example/rocket#202*")
        (expect (get-buffer-window approve-model-store-buffer t) :to-be nil)
        (expect (test-feedback--lighter) :to-match "#202 …")
        (test-feedback--wait)
        (expect approve-feedback--path :to-equal test-feedback--path)
        (expect (test-feedback--lighter) :not :to-match "…")
        ;; Five unresolved threads in the file, the first of two open
        ;; pull requests, a commit not pushed.
        (expect (test-feedback--lighter) :to-match "#202 .*5 1/2 ↑1\\'")
        (let ((tooltip (get-text-property 1 'help-echo approve-feedback--lighter)))
          (expect tooltip :to-match "acme-example/rocket#202")
          (expect tooltip :to-match "parser-rewrite into main · 1 of 2 open pull requests")
          (expect tooltip :to-match "1 commit not pushed")
          (expect tooltip :to-match "5 unresolved threads in this file")))))

  (it "shares the store of the PR buffer that is open"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (let ((pr (approve-test-open-pr "medium")))
          (approve-feedback-mode 1)
          (expect approve-model-store-buffer :to-be pr)
          (expect (test-feedback--pr-buffers) :to-equal (list pr))
          (expect (approve-pr-field 'number) :to-equal 202)
          (expect (test-feedback--lighter) :not :to-match "1/")))))

  (it "asks before a merged pull request, and stays off when declined"
    (let ((test-feedback--prs (list (test-feedback--row 202 "MERGED"))))
      (test-feedback--with-clone
        (spy-on 'y-or-n-p :and-return-value nil)
        (approve-feedback-mode 1)
        (expect (spy-calls-args-for 'y-or-n-p 0)
                :to-equal '("#202 is merged: show its threads anyway? "))
        (expect approve-feedback-mode :to-be nil)
        (expect (test-feedback--pr-buffers) :to-equal nil)
        (spy-on 'y-or-n-p :and-return-value t)
        (approve-feedback-mode 1)
        (expect approve-feedback-mode :to-be t)
        ;; The row says it is merged until the store says otherwise.
        (expect (test-feedback--lighter) :to-match "#202 … merged\\'"))))

  (it "says why it can't turn on, and stays off"
    (let ((test-feedback--prs nil))
      (test-feedback--with-clone
        (expect (approve-feedback-mode 1) :to-throw 'user-error '("No pull request for parser-rewrite"))
        (expect approve-feedback-mode :to-be nil)
        (spy-on 'approve-clone-branch-head :and-return-value nil)
        (expect (approve-feedback-mode 1) :to-throw 'user-error)
        (expect approve-feedback-mode :to-be nil))))

  (it "takes away what it added when turned off"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (approve-feedback-mode -1)
        (expect approve-model-store-buffer :to-be nil)
        (expect approve-feedback--lighter :to-be nil)
        (expect approve-feedback--pull-request :to-be nil)))))

(describe "The feedback mode's PR buffer"
  (it "redraws the clone's buffers when it redraws"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (spy-on 'approve-feedback-redraw)
        (with-current-buffer approve-model-store-buffer
          (approve-pr-redraw))
        (expect 'approve-feedback-redraw :to-have-been-called-times 1))))

  (it "is opened and loaded again when it is killed"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (let ((old approve-model-store-buffer))
          (kill-buffer old)
          (approve-test-wait-for (lambda () (buffer-live-p approve-model-store-buffer)))
          (expect approve-model-store-buffer :not :to-be old)
          (test-feedback--wait)
          (expect (approve-pr-field 'number) :to-equal 202)))))

  (it "is the one picked for the branch"
    (let ((test-feedback--prs (list (test-feedback--row 202) (test-feedback--row 203))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (let ((pr (test-feedback--row 203)))
          (run-hook-with-args 'approve-clone-picked-functions test-feedback--head pr)
          (expect approve-feedback--pull-request :to-be pr)
          (expect (buffer-name approve-model-store-buffer)
                  :to-equal "*approve: acme-example/rocket#203*")
          (expect (test-feedback--lighter) :to-match "#203 … 2/2"))))))

(describe "approve-feedback-refresh"
  (it "loads the pull request again in its PR buffer"
    (let ((test-feedback--prs (list (test-feedback--row 202))))
      (test-feedback--with-clone
        (approve-feedback-mode 1)
        (test-feedback--wait)
        (let ((owner approve-model-store-buffer))
          (spy-on 'approve-pr-refresh)
          (approve-feedback-refresh)
          (expect 'approve-pr-refresh :to-have-been-called-with nil)
          (expect (spy-context-current-buffer (spy-calls-most-recent 'approve-pr-refresh))
                  :to-be owner)))))

  (it "says the mode is off"
    (with-temp-buffer
      (expect (approve-feedback-refresh) :to-throw 'user-error))))

;;; test-approve-feedback.el ends here
