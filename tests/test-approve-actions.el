;;; test-approve-actions.el --- Tests for the action pipeline -*- lexical-binding: t; -*-

;;; Commentary:

;; The pipeline every change on GitHub runs (flow F00), in the medium
;; PR, with a fake SEND that the tests answer: what it refuses, the
;; text it asks for, the marks while GitHub has not answered, and what
;; success and failure leave behind.  One test goes through the
;; mutation queue to a fake transport.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-actions)
(require 'approve-test-helpers "helpers")

(defconst test-actions--path "internal/parser/parser.go"
  "The file of the medium PR the tests act on; it is expanded.")

(defconst test-actions--range
  '(:kind lines :path "internal/parser/parser.go"
          :start-side "RIGHT" :start-line 13 :side "RIGHT" :line 14)
  "Two added lines of parser.go in the medium PR.")

(defvar test-actions--sent nil
  "What the fake SEND got: (TEXT CALLBACK ERRORBACK BUFFER), or nil.")

(defvar test-actions--said nil
  "The last message shown, or nil.")

(defun test-actions--send (text callback errorback)
  "Fake SEND: remember TEXT, CALLBACK and ERRORBACK, answer nothing yet."
  (setq test-actions--sent (list text callback errorback (current-buffer))))

(defun test-actions--answer (result)
  "Confirm the last SEND with RESULT, as GitHub would."
  (funcall (nth 1 test-actions--sent) result))

(defun test-actions--refuse (err)
  "Fail the last SEND with ERR, as GitHub would."
  (funcall (nth 2 test-actions--sent) err))

(defun test-actions--marked (buffer)
  "Return the lines of BUFFER marked in flight, as (TEXT ICON) in order.
ICON is non-nil when the loading icon follows the line."
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (list (string-trim (approve-render-strip-decorations
                                  (buffer-substring (overlay-start overlay)
                                                    (overlay-end overlay))))
                    (and (overlay-get overlay 'after-string) t)))
            (sort (seq-filter (lambda (overlay)
                                (eq (overlay-get overlay 'face) 'approve-inflight))
                              (overlays-in (point-min) (point-max)))
                  (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defun test-actions--kill-inputs ()
  "Kill every input buffer, keeping no draft."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*approve-input" (buffer-name buffer))
      (with-current-buffer buffer
        (setq approve-input--done t))
      (kill-buffer buffer))))

(defun test-actions--file-marks ()
  "Return marks of the heading and the entry of `test-actions--path'."
  `((section approve-file-section ,test-actions--path)
    (section approve-file-entry-section ,test-actions--path)))

(defun test-actions--goto (text)
  "Move point to the first TEXT in the current buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(describe "The action pipeline"
  :var (pr)
  (before-all
    (approve-test-with-fixture "medium"
      (setq pr (approve-test-open-pr "medium"))
      (with-current-buffer pr
        (magit-section-show (approve-test-find-section 'approve-file-section
                                                       test-actions--path)))))
  (after-all
    (kill-buffer pr))
  (before-each
    (setq test-actions--sent nil
          test-actions--said nil)
    (spy-on 'message :and-call-fake
            (lambda (format-string &rest args)
              (setq test-actions--said
                    (and format-string (apply #'format-message format-string args)))))
    (switch-to-buffer pr)
    (delete-other-windows)
    (with-current-buffer pr
      (deactivate-mark)
      (approve-session-put :inflight nil)
      (approve-session-put :drafts nil)
      (approve-action--decorate)))
  (after-each
    (test-actions--kill-inputs))

  (describe "approve-action-context"
    (it "is the context at point when the command applies to its kind"
      (with-current-buffer pr
        (test-actions--goto "internal/parser/lexer.go")
        (let ((context (approve-action-context '(file) "Put point on a file")))
          (expect (plist-get context :kind) :to-be 'file)
          (expect (plist-get context :path) :to-equal "internal/parser/lexer.go"))
        (expect (plist-get (approve-action-context) :kind) :to-be 'file)
        (expect (plist-get (approve-action-context
                            (lambda (context) (plist-get context :path)))
                           :kind)
                :to-be 'file)))

    (it "says what the command needs elsewhere"
      (with-current-buffer pr
        (test-actions--goto "internal/parser/lexer.go")
        (expect (approve-action-context '(thread) "Put point on a thread")
                :to-throw 'user-error '("Put point on a thread"))
        (expect (approve-action-context #'ignore "Put point on a thread")
                :to-throw 'user-error '("Put point on a thread"))))

    (it "applies to the section around point when not to the thing at point"
      (with-current-buffer pr
        (goto-char (oref (approve-test-find-section 'approve-issue-comment-section
                                                    "IC_kwDOVM-W6WgSu2tHHg")
                         start))
        (search-forward "@bob-example")
        (backward-char 2)
        (expect (plist-get (approve-context-at-point) :kind) :to-be 'user)
        (expect (plist-get (approve-action-context '(issue-comment)) :kind)
                :to-be 'issue-comment)
        (expect (plist-get (approve-action-context '(user issue-comment)) :kind)
                :to-be 'user)))

    (it "refuses where no pull request has loaded"
      (with-temp-buffer
        (expect (approve-action-context) :to-throw 'user-error
                '("Not in an Approve pull request buffer")))))

  (it "says why not, before asking for text or sending"
    (with-current-buffer pr
      (expect (approve-action-run :why-not "You can't resolve this thread"
                                  :input '(:prompt "Reply" :target (reply "x"))
                                  :send #'test-actions--send)
              :to-throw 'user-error '("You can't resolve this thread")))
    (expect test-actions--sent :to-be nil)
    (expect (one-window-p) :to-be-truthy))

  (it "marks the target in flight while GitHub has not answered, across redraws"
    (with-current-buffer pr
      (approve-action-run :marks (test-actions--file-marks) :send #'test-actions--send)
      (expect (car test-actions--sent) :to-be nil)
      (expect (nth 3 test-actions--sent) :to-be pr)
      (expect (length (approve-session-get :inflight)) :to-equal 1)
      (let ((marked (test-actions--marked pr)))
        (expect (length marked) :to-equal 2)
        (dolist (line marked)
          (expect (car line) :to-match "parser\\.go")
          (expect (cadr line) :to-be t)))
      (approve-pr-redraw)
      (expect (length (test-actions--marked pr)) :to-equal 2)
      (test-actions--answer nil)
      (expect (test-actions--marked pr) :to-be nil)
      (expect (approve-session-get :inflight) :to-be nil)))

  (it "marks diff lines, with the icon after the last one"
    (with-current-buffer pr
      (approve-action-run :marks `((lines ,test-actions--range)) :send #'test-actions--send)
      (expect (test-actions--marked pr)
              :to-equal '(("if err != nil {" nil)
                          ("return nil, fmt.Errorf(\"lookup 14: %w\", err)" t)))
      (test-actions--refuse '(approve-api-error "Nope"))
      (expect (test-actions--marked pr) :to-be nil)))

  (it "keeps the marks of two actions apart"
    (with-current-buffer pr
      (approve-action-run :marks (test-actions--file-marks) :send #'test-actions--send)
      (let ((first test-actions--sent))
        (approve-action-run :marks (test-actions--file-marks) :send #'test-actions--send)
        (expect (length (approve-session-get :inflight)) :to-equal 2)
        (funcall (nth 1 first) nil)
        (expect (length (approve-session-get :inflight)) :to-equal 1)
        (expect (length (test-actions--marked pr)) :to-equal 2)
        (test-actions--answer nil)
        (expect (test-actions--marked pr) :to-be nil))))

  (it "on success, runs DONE with the result, then redraws with the marks gone"
    (spy-on 'approve-pr-redraw :and-call-through)
    (let (done)
      (with-current-buffer pr
        (approve-action-run :marks (test-actions--file-marks) :send #'test-actions--send
                            :done (lambda (result)
                                    (setq done (list result (current-buffer)
                                                     (length (approve-session-get :inflight))))))
        (test-actions--answer 'the-result))
      (expect done :to-equal (list 'the-result pr 1))
      (expect 'approve-pr-redraw :to-have-been-called)
      (expect (test-actions--marked pr) :to-be nil)
      (expect test-actions--said :to-be nil)))

  (it "on failure, drops the marks and says what failed and why"
    (let ((done nil))
      (with-current-buffer pr
        (approve-action-run :what "resolve the thread" :marks (test-actions--file-marks)
                            :send #'test-actions--send
                            :done (lambda (_) (setq done t)))
        (test-actions--refuse '(approve-api-graphql-error "Could not resolve thread")))
      (expect done :to-be nil)
      (expect (test-actions--marked pr) :to-be nil)
      (expect (with-current-buffer pr (approve-session-get :inflight)) :to-be nil)
      (expect test-actions--said
              :to-equal "Approve: could not resolve the thread: Could not resolve thread")))

  (it "says when a rate limit resets"
    (with-current-buffer pr
      (approve-action-run :what "reply" :send #'test-actions--send)
      (test-actions--refuse (list 'approve-api-rate-limited "Rate limit"
                                  :reset (float-time (encode-time '(0 30 14 1 1 2027 nil nil nil))))))
    (expect test-actions--said :to-equal
            "Approve: could not reply: Rate limit (the limit resets at 14:30)"))

  (it "asks for the text first, starting from its draft, and drops the draft once sent"
    (with-current-buffer pr
      (approve-session-map-put :drafts '(reply "PRRT_x") "Draft"))
    (let ((input (with-current-buffer pr
                   (approve-action-run :what "reply"
                                       :input '(:prompt "Reply to @bob" :target (reply "PRRT_x"))
                                       :marks (test-actions--file-marks)
                                       :send #'test-actions--send))))
      (expect test-actions--sent :to-be nil)
      (with-current-buffer input
        (expect (buffer-string) :to-equal "Draft")
        (goto-char (point-max))
        (insert " and more")
        (approve-input-commit)))
    (expect (car test-actions--sent) :to-equal "Draft and more")
    (expect (length (test-actions--marked pr)) :to-equal 2)
    (expect (with-current-buffer pr (approve-input-draft '(reply "PRRT_x")))
            :to-equal "Draft and more")
    (with-current-buffer pr
      (test-actions--answer nil))
    (expect (with-current-buffer pr (approve-input-draft '(reply "PRRT_x"))) :to-be nil))

  (it "keeps the text as a draft when GitHub refuses it, and says so"
    (let ((input (with-current-buffer pr
                   (approve-action-run :what "reply"
                                       :input '(:target (reply "PRRT_y"))
                                       :send #'test-actions--send))))
      (with-current-buffer input
        (insert "Precious")
        (approve-input-commit)))
    (with-current-buffer pr
      (test-actions--refuse '(approve-api-network-error "Network down")))
    (expect (with-current-buffer pr (approve-input-draft '(reply "PRRT_y")))
            :to-equal "Precious")
    (expect test-actions--said :to-equal
            "Approve: could not reply: Network down; your text is kept as a draft"))

  (it "sends nothing when the text is aborted"
    (with-current-buffer (with-current-buffer pr
                           (approve-action-run :input '(:target (reply "PRRT_z"))
                                               :send #'test-actions--send))
      (insert "Later")
      (approve-input-abort))
    (expect test-actions--sent :to-be nil)
    (expect (with-current-buffer pr (approve-input-draft '(reply "PRRT_z")))
            :to-equal "Later"))

  (it "drops the marks when SEND fails before it starts"
    (with-current-buffer pr
      (expect (approve-action-run :marks (test-actions--file-marks)
                                  :send (lambda (&rest _) (error "Bug")))
              :to-throw 'error '("Bug"))
      (expect (approve-session-get :inflight) :to-be nil)
      (expect (test-actions--marked pr) :to-be nil)))

  (it "drops the marks and redraws when DONE fails"
    (spy-on 'approve-pr-redraw :and-call-through)
    (with-current-buffer pr
      (approve-action-run :marks (test-actions--file-marks) :send #'test-actions--send
                          :done (lambda (_) (error "Bug")))
      (expect (test-actions--answer nil) :to-throw 'error '("Bug"))
      (expect (approve-session-get :inflight) :to-be nil)
      (expect 'approve-pr-redraw :to-have-been-called)
      (expect (test-actions--marked pr) :to-be nil)))

  (it "patches the store from a mutation through the queue, then redraws"
    (let* ((thread (with-current-buffer pr
                     (oref (approve-test-find-section 'approve-thread-section) value)))
           (sent nil)
           (approve-api-timeout nil)
           (approve-api-transport
            (lambda (request)
              (setq sent request)
              (run-at-time 0 nil (plist-get request :on-success)
                           `((data (resolveReviewThread
                                    (thread (__typename . "PullRequestReviewThread")
                                            (id . ,thread) (isResolved . t)))))
                           nil)
              nil)))
      (spy-on 'approve-pr-redraw :and-call-through)
      (with-current-buffer pr
        (expect (approve-model-get "PullRequestReviewThread" thread 'isResolved) :to-be nil)
        (approve-action-run
         :what "resolve the thread"
         :marks `((section approve-thread-section ,thread))
         :send (lambda (_text callback errorback)
                 (approve-api-mutate
                  "mutation($id: ID!) { resolveReviewThread(input: {threadId: $id}) { thread { __typename id isResolved } } }"
                  `((id . ,thread))
                  :callback (lambda (data) (funcall callback (approve-model-patch data)))
                  :errorback errorback)))
        (expect (test-actions--marked pr) :not :to-be nil)
        (approve-test-wait-for (lambda () (null (approve-session-get :inflight))))
        (expect (plist-get sent :variables) :to-equal `((id . ,thread)))
        (expect (approve-model-get "PullRequestReviewThread" thread 'isResolved) :to-be t)
        (expect 'approve-pr-redraw :to-have-been-called)
        (expect (test-actions--marked pr) :to-be nil)))))

(provide 'test-approve-actions)

;;; test-approve-actions.el ends here
