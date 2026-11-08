;;; test-approve-api-paging.el --- Tests for approve-api-paging -*- lexical-binding: t; -*-

;;; Commentary:

;; The paging helpers through a fake transport: following cursors,
;; stopping at the last page or max-pages, and errors.

;;; Code:

(require 'buttercup)
(require 'approve-api-paging)

(defvar test-approve-api-paging--sent nil
  "Request plists the fake transport received, newest first.")

(defun test-approve-api-paging--transport (request)
  "Fake `approve-api-transport': remember REQUEST, answer nothing yet."
  (push request test-approve-api-paging--sent)
  nil)

(defun test-approve-api-paging--succeed (value)
  "Answer the newest request with VALUE."
  (funcall (plist-get (car test-approve-api-paging--sent) :on-success) value nil))

(defun test-approve-api-paging--fail (failure)
  "Fail the newest request with the FAILURE plist."
  (funcall (plist-get (car test-approve-api-paging--sent) :on-failure) failure))

(defmacro test-approve-api-paging--with-fake (&rest body)
  "Run BODY with the fake transport and fresh request state."
  (declare (indent 0))
  `(let ((approve-api-transport #'test-approve-api-paging--transport)
         (approve-api-timeout nil)
         (approve-api--in-flight nil)
         (approve-api--rate-limits (make-hash-table :test 'equal))
         (test-approve-api-paging--sent nil))
     ,@body))

(defun test-approve-api-paging--quiet ()
  "Keep progress and error messages out of the test output."
  (spy-on 'message :and-call-fake
          (lambda (format-string &rest args)
            (and format-string (apply #'format-message format-string args)))))

(defun test-approve-api-paging--page (cursor more)
  "Return GraphQL data for a files page ending at CURSOR, with MORE pages."
  `((repository . ((pullRequest . ((files . ((nodes . (((path . ,cursor))))
                                              (pageInfo . ((hasNextPage . ,more)
                                                           (endCursor . ,cursor)))))))))))

(describe "approve-api-graphql-all"
  (before-each (test-approve-api-paging--quiet))

  (it "follows the cursor until hasNextPage is false, then calls back"
    (test-approve-api-paging--with-fake
      (let ((pages nil) (done 0))
        (approve-api-graphql-all "query { a }" '((number . 1) (after . "stale"))
                                 :path '(repository pullRequest files)
                                 :on-page (lambda (data page) (push (cons page data) pages))
                                 :callback (lambda () (setq done (1+ done))))
        (expect (plist-get (car test-approve-api-paging--sent) :variables)
                :to-equal '((number . 1)))
        (test-approve-api-paging--succeed `((data . ,(test-approve-api-paging--page "c1" t))))
        (expect (plist-get (car test-approve-api-paging--sent) :variables)
                :to-equal '((after . "c1") (number . 1)))
        (test-approve-api-paging--succeed `((data . ,(test-approve-api-paging--page "c2" nil))))
        (expect (length test-approve-api-paging--sent) :to-equal 2)
        (expect (mapcar #'car pages) :to-equal '(2 1))
        (expect (cdr (assq 1 pages)) :to-equal (test-approve-api-paging--page "c1" t))
        (expect done :to-equal 1))))

  (it "stops after max-pages"
    (test-approve-api-paging--with-fake
      (let ((done nil))
        (approve-api-graphql-all "query { a }" nil
                                 :path '(repository pullRequest files)
                                 :max-pages 1
                                 :callback (lambda () (setq done t)))
        (test-approve-api-paging--succeed `((data . ,(test-approve-api-paging--page "c1" t))))
        (expect (length test-approve-api-paging--sent) :to-equal 1)
        (expect done :to-be t))))

  (it "stops at an error and reports it to the errorback"
    (test-approve-api-paging--with-fake
      (let ((done nil) (received nil))
        (approve-api-graphql-all "query { a }" nil
                                 :path '(repository pullRequest files)
                                 :callback (lambda () (setq done t))
                                 :errorback (lambda (err) (setq received err)))
        (test-approve-api-paging--succeed `((data . ,(test-approve-api-paging--page "c1" t))))
        (test-approve-api-paging--fail '(:status 502))
        (expect (car received) :to-be 'approve-api-http-error)
        (expect done :to-be nil)
        (expect (length test-approve-api-paging--sent) :to-equal 2)))))

(describe "approve-api-rest-all"
  (before-each (test-approve-api-paging--quiet))

  (it "asks page after page while pages are full, stopping at a short page"
    (test-approve-api-paging--with-fake
      (let ((pages nil) (done nil))
        (approve-api-rest-all "/repos/o/r/pulls/1/files" :per-page 2
                              :on-page (lambda (value page) (push (cons page value) pages))
                              :callback (lambda () (setq done t)))
        (expect (plist-get (car test-approve-api-paging--sent) :params)
                :to-equal '((per_page . 2) (page . 1)))
        (test-approve-api-paging--succeed '(a b))
        (expect (plist-get (car test-approve-api-paging--sent) :params)
                :to-equal '((per_page . 2) (page . 2)))
        (test-approve-api-paging--succeed '(c))
        (expect (length test-approve-api-paging--sent) :to-equal 2)
        (expect pages :to-equal '((2 c) (1 a b)))
        (expect done :to-be t))))

  (it "stops at max-pages and defaults to 100 per page"
    (test-approve-api-paging--with-fake
      (let ((done nil))
        (approve-api-rest-all "/x" :max-pages 1 :callback (lambda () (setq done t)))
        (expect (plist-get (car test-approve-api-paging--sent) :params)
                :to-equal '((per_page . 100) (page . 1)))
        (test-approve-api-paging--succeed (make-list 100 'file))
        (expect (length test-approve-api-paging--sent) :to-equal 1)
        (expect done :to-be t)))))


;;; test-approve-api-paging.el ends here
