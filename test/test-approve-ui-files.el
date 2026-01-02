;;; test-approve-ui-files.el --- Tests for approve-ui-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;;; Commentary:

;; Tests for the approve-ui-files module.

;;; Code:

(require 'buttercup)
(require 'magit-section)
(require 'approve-model)
(require 'approve-ui-files)

;;; Test Helpers

(defun test--setup-model-with-files (files &optional additions deletions)
  "Set up the model with FILES and optional PR-level ADDITIONS/DELETIONS.
FILES is a list of alists with additions, deletions, path, and changeType."
  (approve-model-init)
  (let ((pr-data `((__typename . "PullRequest")
                   (id . "PR_123")
                   (additions . ,(or additions 0))
                   (deletions . ,(or deletions 0))
                   (files . ((nodes . ,(vconcat files))
                             (totalCount . ,(length files)))))))
    (approve-model-load pr-data t)))

;;; Tests

(describe "approve-ui-files"
  (describe "approve--diffstat-summary-string"
    (it "formats singular file correctly"
      (expect (approve--diffstat-summary-string 1 5 3)
              :to-equal "1 file changed, 5 insertions(+), 3 deletions(-)"))

    (it "formats plural files correctly"
      (expect (approve--diffstat-summary-string 3 19 10)
              :to-equal "3 files changed, 19 insertions(+), 10 deletions(-)"))

    (it "formats singular insertion correctly"
      (expect (approve--diffstat-summary-string 2 1 5)
              :to-equal "2 files changed, 1 insertion(+), 5 deletions(-)"))

    (it "formats singular deletion correctly"
      (expect (approve--diffstat-summary-string 2 10 1)
              :to-equal "2 files changed, 10 insertions(+), 1 deletion(-)"))

    (it "handles zero insertions"
      (expect (approve--diffstat-summary-string 1 0 5)
              :to-equal "1 file changed, 0 insertions(+), 5 deletions(-)"))

    (it "handles zero deletions"
      (expect (approve--diffstat-summary-string 1 10 0)
              :to-equal "1 file changed, 10 insertions(+), 0 deletions(-)")))

  (describe "approve--diffstat-scale-counts"
    (it "returns zero for zero total"
      (expect (approve--diffstat-scale-counts 0 0 100)
              :to-equal '(0 . 0)))

    (it "returns unscaled values when within limit"
      (let ((approve-diffstat-graph-width 30))
        (expect (approve--diffstat-scale-counts 10 5 15)
                :to-equal '(10 . 5))))

    (it "scales values when exceeding limit"
      (let ((approve-diffstat-graph-width 30))
        (expect (approve--diffstat-scale-counts 60 40 100)
                :to-equal '(18 . 12))))

    (it "ensures at least 1 for non-zero additions"
      (let ((approve-diffstat-graph-width 30))
        (expect (car (approve--diffstat-scale-counts 1 100 101))
                :to-be-greater-than 0)))

    (it "ensures at least 1 for non-zero deletions"
      (let ((approve-diffstat-graph-width 30))
        (expect (cdr (approve--diffstat-scale-counts 100 1 101))
                :to-be-greater-than 0))))

  (describe "approve--diffstat-graph-string"
    (it "creates graph with adds and deletes"
      (let ((approve-diffstat-graph-width 30)
            (result (approve--diffstat-graph-string 5 3 8)))
        ;; Check that the result contains + and - characters
        (expect (string-match-p "\\+\\+" result) :to-be-truthy)
        (expect (string-match-p "--" result) :to-be-truthy)))

    (it "creates graph with only additions"
      (let ((approve-diffstat-graph-width 30)
            (result (approve--diffstat-graph-string 10 0 10)))
        (expect (string-match-p "\\+\\+\\+\\+" result) :to-be-truthy)
        (expect (string-match-p "-" result) :not :to-be-truthy)))

    (it "creates graph with only deletions"
      (let ((approve-diffstat-graph-width 30)
            (result (approve--diffstat-graph-string 0 10 10)))
        (expect (string-match-p "-" result) :to-be-truthy)
        (expect (string-match-p "\\+" result) :not :to-be-truthy))))

  (describe "approve--diffstat-file-max-changes"
    (it "returns zero for empty list"
      (expect (approve--diffstat-file-max-changes nil) :to-equal 0))

    (it "returns the max total changes"
      (let ((files '(((additions . 5) (deletions . 3))
                     ((additions . 10) (deletions . 2))
                     ((additions . 1) (deletions . 1)))))
        (expect (approve--diffstat-file-max-changes files) :to-equal 12)))

    (it "handles missing additions/deletions"
      (let ((files '(((path . "file.el"))
                     ((additions . 5) (path . "other.el")))))
        (expect (approve--diffstat-file-max-changes files) :to-equal 5))))

  (describe "approve--diffstat-max-path-width"
    (it "returns zero for empty list"
      (expect (approve--diffstat-max-path-width nil) :to-equal 0))

    (it "returns the longest path width"
      (let ((files '(((path . "short.el"))
                     ((path . "very-long-filename.el"))
                     ((path . "medium.el")))))
        (expect (approve--diffstat-max-path-width files) :to-equal 21))))

  (describe "approve--diffstat-max-count-width"
    (it "returns 1 for empty list"
      (expect (approve--diffstat-max-count-width nil) :to-equal 1))

    (it "returns width for largest count"
      (let ((files '(((additions . 50) (deletions . 50))
                     ((additions . 5) (deletions . 5))
                     ((additions . 500) (deletions . 499)))))
        (expect (approve--diffstat-max-count-width files) :to-equal 3))))

  (describe "approve--format-viewed-indicator"
    (it "returns checkmark for VIEWED state"
      (let ((result (approve--format-viewed-indicator "VIEWED")))
        (expect result :to-match approve-file-viewed-indicator)))

    (it "returns bullet for UNVIEWED state"
      (let ((result (approve--format-viewed-indicator "UNVIEWED")))
        (expect result :to-match approve-file-unviewed-indicator)))

    (it "returns refresh for DISMISSED state"
      (let ((result (approve--format-viewed-indicator "DISMISSED")))
        (expect result :to-match approve-file-dismissed-indicator)))

    (it "returns bullet for nil state"
      (let ((result (approve--format-viewed-indicator nil)))
        (expect result :to-match approve-file-unviewed-indicator))))

  (describe "approve--format-comment-indicator"
    (it "returns empty string for zero comments"
      (expect (approve--format-comment-indicator 0) :to-equal ""))

    (it "returns empty string for nil comments"
      (expect (approve--format-comment-indicator nil) :to-equal ""))

    (it "returns indicator with count for positive comments"
      (let ((result (approve--format-comment-indicator 3)))
        (expect (string-match-p approve-file-comment-indicator result) :to-be-truthy)
        (expect (string-match-p "3" result) :to-be-truthy)))

    (it "applies the comment indicator face"
      (let ((result (approve--format-comment-indicator 5)))
        ;; Result starts with a space, so check position 1
        (expect (get-text-property 1 'face result)
                :to-equal 'approve-file-comment-indicator-face))))

  (describe "approve--format-diffstat-line"
    (it "formats a file line correctly"
      (let* ((file '((path . "lisp/test.el")
                     (additions . 10)
                     (deletions . 5)))
             (result (approve--format-diffstat-line file 20 3 15)))
        ;; Should contain the path
        (expect (string-match-p "lisp/test.el" result) :to-be-truthy)
        ;; Should contain the count (15)
        (expect (string-match-p "15" result) :to-be-truthy)
        ;; Should contain a pipe separator
        (expect (string-match-p "|" result) :to-be-truthy)))

    (it "includes viewed indicator for viewed files"
      (let* ((file '((path . "viewed.el")
                     (additions . 5)
                     (deletions . 2)
                     (viewerViewedState . "VIEWED")))
             (result (approve--format-diffstat-line file 20 3 10)))
        (expect (string-match-p approve-file-viewed-indicator result) :to-be-truthy)))

    (it "includes unviewed indicator for unviewed files"
      (let* ((file '((path . "unviewed.el")
                     (additions . 5)
                     (deletions . 2)
                     (viewerViewedState . "UNVIEWED")))
             (result (approve--format-diffstat-line file 20 3 10)))
        (expect (string-match-p approve-file-unviewed-indicator result) :to-be-truthy)))

    (it "includes dismissed indicator for dismissed files"
      (let* ((file '((path . "dismissed.el")
                     (additions . 5)
                     (deletions . 2)
                     (viewerViewedState . "DISMISSED")))
             (result (approve--format-diffstat-line file 20 3 10)))
        (expect (string-match-p approve-file-dismissed-indicator result) :to-be-truthy)))

    (it "includes comment indicator when comment count is provided"
      (let* ((file '((path . "file.el")
                     (additions . 5)
                     (deletions . 2)))
             (result (approve--format-diffstat-line file 20 3 10 3)))
        (expect (string-match-p approve-file-comment-indicator result) :to-be-truthy)
        (expect (string-match-p "3" result) :to-be-truthy)))

    (it "does not include comment indicator when comment count is zero"
      (let* ((file '((path . "file.el")
                     (additions . 5)
                     (deletions . 2)))
             (result (approve--format-diffstat-line file 20 3 10 0)))
        (expect (string-match-p approve-file-comment-indicator result) :not :to-be-truthy))))

  (describe "approve-insert-files-section"
    (it "does nothing when no files data"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load '((__typename . "PullRequest")
                                    (id . "PR_123"))
                                  t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :to-equal ""))
          (kill-buffer buffer))))

    (it "inserts summary line"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 13)
                 (deletions . 7)
                 (files . ((nodes . [((path . "file1.el")
                                      (additions . 10)
                                      (deletions . 5))
                                     ((path . "file2.el")
                                      (additions . 3)
                                      (deletions . 2))])
                           (totalCount . 2))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :to-match "2 files changed"))
          (kill-buffer buffer))))

    (it "inserts file paths"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 30)
                 (deletions . 8)
                 (files . ((nodes . [((path . "lisp/approve.el")
                                      (additions . 10)
                                      (deletions . 5))
                                     ((path . "test/test-approve.el")
                                      (additions . 20)
                                      (deletions . 3))])
                           (totalCount . 2))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :to-match "lisp/approve.el")
              (expect (buffer-string) :to-match "test/test-approve.el"))
          (kill-buffer buffer))))

    (it "shows truncation info when data is truncated"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 100)
                 (deletions . 50)
                 (files . ((nodes . [((path . "file1.el")
                                      (additions . 10)
                                      (deletions . 5))])
                           (totalCount . 5)
                           (pageInfo . ((hasNextPage . t))))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :to-match "(showing 1 of 5 files)"))
          (kill-buffer buffer))))

    (it "creates magit sections for files"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 5)
                 (deletions . 2)
                 (files . ((nodes . [((path . "file1.el")
                                      (additions . 5)
                                      (deletions . 2))])
                           (totalCount . 1))))
               t)
              (let ((inhibit-read-only t))
                (magit-insert-section (root)
                  (approve-insert-files-section)))
              (goto-char (point-min))
              ;; Find the file section by checking section types
              (let ((section (magit-current-section)))
                (while (and section (not (eq (oref section type) 'file)))
                  (magit-section-forward)
                  (setq section (magit-current-section)))
                (expect section :to-be-truthy)
                (expect (oref section type) :to-equal 'file)))
          (kill-buffer buffer))))

    (it "shows viewed indicators for files with different viewed states"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 15)
                 (deletions . 6)
                 (files . ((nodes . [((path . "viewed.el")
                                      (additions . 5)
                                      (deletions . 2)
                                      (viewerViewedState . "VIEWED"))
                                     ((path . "unviewed.el")
                                      (additions . 5)
                                      (deletions . 2)
                                      (viewerViewedState . "UNVIEWED"))
                                     ((path . "dismissed.el")
                                      (additions . 5)
                                      (deletions . 2)
                                      (viewerViewedState . "DISMISSED"))])
                           (totalCount . 3))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              ;; Check that all three indicators are present
              (expect (buffer-string) :to-match approve-file-viewed-indicator)
              (expect (buffer-string) :to-match approve-file-unviewed-indicator)
              (expect (buffer-string) :to-match approve-file-dismissed-indicator))
          (kill-buffer buffer))))

    (it "shows comment indicator for files with FILE type review threads"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 10)
                 (deletions . 5)
                 (files . ((nodes . [((path . "file-with-comments.el")
                                      (additions . 10)
                                      (deletions . 5))])
                           (totalCount . 1)))
                 (reviewThreads . ((nodes . [((__typename . "PullRequestReviewThread")
                                              (id . "RT_1")
                                              (path . "file-with-comments.el")
                                              (line . 1)
                                              (subjectType . "FILE")
                                              (comments . ((nodes . [((__typename . "PullRequestReviewComment")
                                                                      (id . "C_1"))
                                                                     ((__typename . "PullRequestReviewComment")
                                                                      (id . "C_2"))])
                                                           (totalCount . 2))))])
                                   (totalCount . 1))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :to-match approve-file-comment-indicator))
          (kill-buffer buffer))))

    (it "does not show comment indicator for files with only LINE type review threads"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 10)
                 (deletions . 5)
                 (files . ((nodes . [((path . "file-with-line-comments.el")
                                      (additions . 10)
                                      (deletions . 5))])
                           (totalCount . 1)))
                 (reviewThreads . ((nodes . [((__typename . "PullRequestReviewThread")
                                              (id . "RT_1")
                                              (path . "file-with-line-comments.el")
                                              (line . 10)
                                              (subjectType . "LINE")
                                              (comments . ((nodes . [((__typename . "PullRequestReviewComment")
                                                                      (id . "C_1"))])
                                                           (totalCount . 1))))])
                                   (totalCount . 1))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :not :to-match approve-file-comment-indicator))
          (kill-buffer buffer))))

    (it "does not show comment indicator for files without review threads"
      (let ((buffer (generate-new-buffer " *test-changes*")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-mode)
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (additions . 10)
                 (deletions . 5)
                 (files . ((nodes . [((path . "file-no-comments.el")
                                      (additions . 10)
                                      (deletions . 5))])
                           (totalCount . 1)))
                 (reviewThreads . ((nodes . [])
                                   (totalCount . 0))))
               t)
              (let ((inhibit-read-only t))
                (approve-insert-files-section))
              (expect (buffer-string) :not :to-match approve-file-comment-indicator))
          (kill-buffer buffer)))))

  (describe "approve--build-review-threads-by-path"
    (it "returns empty hash table when no review threads"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-model-init)
              (approve-model-load
               '((__typename . "PullRequest")
                 (id . "PR_123")
                 (reviewThreads . ((nodes . [])
                                   (totalCount . 0))))
               t)
              (let ((result (approve--build-review-threads-by-path)))
                (expect (hash-table-count result) :to-equal 0)))
          (kill-buffer buffer))))

    (it "groups threads by file path"
      (let ((buffer (generate-new-buffer " *test*")))
        (unwind-protect
            (with-current-buffer buffer
              (approve-model-init)
              (approve-model-load
               `((__typename . "PullRequest")
                 (id . "PR_123")
                 (reviewThreads . ((nodes . [((__typename . "PullRequestReviewThread")
                                              (id . "RT_1")
                                              (path . "file1.el")
                                              (line . 10))
                                             ((__typename . "PullRequestReviewThread")
                                              (id . "RT_2")
                                              (path . "file2.el")
                                              (line . 20))
                                             ((__typename . "PullRequestReviewThread")
                                              (id . "RT_3")
                                              (path . "file1.el")
                                              (line . 30))])
                                   (totalCount . 3))))
               t)
              (let ((result (approve--build-review-threads-by-path)))
                (expect (hash-table-count result) :to-equal 2)
                (expect (length (gethash "file1.el" result)) :to-equal 2)
                (expect (length (gethash "file2.el" result)) :to-equal 1)))
          (kill-buffer buffer)))))

  (describe "approve--count-thread-comments"
    (it "returns zero for thread with no comments"
      (let ((thread '((id . "RT_1")
                      (comments . ((nodes . ())
                                   (totalCount . 0))))))
        (expect (approve--count-thread-comments thread) :to-equal 0)))

    (it "counts comments in thread"
      (let ((thread '((id . "RT_1")
                      (comments . ((nodes . (((id . "C_1"))
                                             ((id . "C_2"))
                                             ((id . "C_3"))))
                                   (totalCount . 3))))))
        (expect (approve--count-thread-comments thread) :to-equal 3))))

  (describe "approve--count-file-comments"
    (it "returns zero for empty threads list"
      (expect (approve--count-file-comments nil) :to-equal 0))

    (it "returns zero for threads with only LINE subject type"
      (let ((threads '(((id . "RT_1")
                        (subjectType . "LINE")
                        (comments . ((nodes . (((id . "C_1"))
                                               ((id . "C_2"))))
                                     (totalCount . 2))))
                       ((id . "RT_2")
                        (subjectType . "LINE")
                        (comments . ((nodes . (((id . "C_3"))))
                                     (totalCount . 1)))))))
        (expect (approve--count-file-comments threads) :to-equal 0)))

    (it "sums comments across FILE subject type threads only"
      (let ((threads '(((id . "RT_1")
                        (subjectType . "FILE")
                        (comments . ((nodes . (((id . "C_1"))
                                               ((id . "C_2"))))
                                     (totalCount . 2))))
                       ((id . "RT_2")
                        (subjectType . "LINE")
                        (comments . ((nodes . (((id . "C_3"))))
                                     (totalCount . 1))))
                       ((id . "RT_3")
                        (subjectType . "FILE")
                        (comments . ((nodes . (((id . "C_4"))))
                                     (totalCount . 1)))))))
        (expect (approve--count-file-comments threads) :to-equal 3))))

  (describe "approve--thread-file-comment-p"
    (it "returns t for FILE subject type"
      (let ((thread '((id . "RT_1")
                      (subjectType . "FILE"))))
        (expect (approve--thread-file-comment-p thread) :to-be-truthy)))

    (it "returns nil for LINE subject type"
      (let ((thread '((id . "RT_1")
                      (subjectType . "LINE"))))
        (expect (approve--thread-file-comment-p thread) :to-be nil)))

    (it "returns nil when subjectType is missing"
      (let ((thread '((id . "RT_1"))))
        (expect (approve--thread-file-comment-p thread) :to-be nil)))))

(provide 'test-approve-ui-files)
;;; test-approve-ui-files.el ends here
