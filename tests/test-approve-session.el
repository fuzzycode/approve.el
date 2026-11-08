;;; test-approve-session.el --- Tests for approve-session -*- lexical-binding: t; -*-

;;; Commentary:

;; The session state: values, sets and maps, per buffer, shared with
;; the buffer that owns the store, and reset by approve-model-init.

;;; Code:

(require 'buttercup)
(require 'approve-session)

(describe "approve-session"
  (before-each
    (approve-model-init))

  (it "gets and puts values"
    (approve-session-put :layout 'split)
    (expect (approve-session-get :layout) :to-be 'split))

  (it "keeps sets without duplicates"
    (approve-session-add :loading 'files)
    (approve-session-add :loading 'threads)
    (approve-session-add :loading 'files)
    (expect (approve-session-member-p :loading 'files) :to-be t)
    (expect (length (approve-session-get :loading)) :to-equal 2)
    (approve-session-remove :loading 'files)
    (expect (approve-session-member-p :loading 'files) :to-be nil)
    (expect (approve-session-member-p :loading 'threads) :to-be t))

  (it "compares set items with equal"
    (approve-session-add :inflight (list 'thread "PRRT_1"))
    (expect (approve-session-member-p :inflight (list 'thread "PRRT_1"))
            :to-be t)
    (approve-session-remove :inflight (list 'thread "PRRT_1"))
    (expect (approve-session-get :inflight) :to-be nil))

  (it "keeps maps by key"
    (approve-session-map-put :drafts '(reply "PRRT_1") "draft")
    (approve-session-map-put :errors 'files '(error "timeout"))
    (expect (approve-session-map-get :drafts (list 'reply "PRRT_1"))
            :to-equal "draft")
    (approve-session-map-put :drafts '(reply "PRRT_1") "longer draft")
    (expect (approve-session-map-get :drafts '(reply "PRRT_1"))
            :to-equal "longer draft")
    (expect (length (approve-session-get :drafts)) :to-equal 1)
    (approve-session-map-remove :drafts '(reply "PRRT_1"))
    (expect (approve-session-map-get :drafts '(reply "PRRT_1")) :to-be nil)
    (expect (approve-session-map-get :errors 'files)
            :to-equal '(error "timeout")))

  (it "is not shared between buffers"
    (approve-session-put :layout 'single)
    (with-temp-buffer
      (expect (approve-session-get :layout) :to-be nil)
      (approve-session-put :layout 'split))
    (expect (approve-session-get :layout) :to-be 'single))

  (it "is reset by approve-model-init"
    (approve-session-put :layout 'split)
    (approve-session-map-put :drafts 'x "text")
    (approve-model-init)
    (expect (approve-session-get :layout) :to-be nil)
    (expect (approve-session-get :drafts) :to-be nil))

  (describe "with approve-model-store-buffer"
    (let (pr-buffer diff-buffer)
      (before-each
        (setq pr-buffer (generate-new-buffer " *test-pr*")
              diff-buffer (generate-new-buffer " *test-diff*"))
        (with-current-buffer pr-buffer
          (approve-model-init))
        (with-current-buffer diff-buffer
          (setq approve-model-store-buffer pr-buffer)))

      (after-each
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list pr-buffer diff-buffer)))

      (it "uses the session of the buffer that owns the store"
        (with-current-buffer diff-buffer
          (approve-session-map-put :drafts 'x "text")
          (approve-session-add :inflight 'y))
        (with-current-buffer pr-buffer
          (expect (approve-session-map-get :drafts 'x) :to-equal "text")
          (expect (approve-session-member-p :inflight 'y) :to-be t))
        (with-current-buffer diff-buffer
          (expect approve-session :to-be nil)
          (expect (approve-session-member-p :inflight 'y) :to-be t))))))

(provide 'test-approve-session)

;;; test-approve-session.el ends here
