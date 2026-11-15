;;; test-approve-thread.el --- Tests for review thread rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Threads drawn as boxes: their heading, comments, states (resolved,
;; outdated, pending) and the sections they make.  The threads come
;; from the medium fixture's internal/parser/parser.go, which has one
;; of each kind.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-test-helpers "helpers")

(defvar test-thread--threads nil
  "The threads of parser.go in the medium fixture, resolved.")

(defun test-thread--find (predicate)
  "Return the first thread of parser.go that PREDICATE accepts."
  (or (seq-find predicate test-thread--threads)
      (error "No such thread")))

(defmacro test-thread--with-inserted (thread &rest body)
  "Insert THREAD in a temporary section buffer and run BODY there."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (magit-section-mode)
     (let ((inhibit-read-only t)
           (approve-render-now approve-test-now))
       (magit-insert-section (root)
         (approve-thread-insert ,thread)))
     (goto-char (point-min))
     ,@body))

(defun test-thread--lines ()
  "Return the buffer's lines as they show, indentation included."
  (split-string (string-remove-suffix
                 "\n" (buffer-substring-no-properties (point-min) (point-max)))
                "\n"))

(describe "approve-thread-insert"
  (before-all
    (let ((approve-use-nerd-icons nil))
      (approve-icon-clear-cache)
      (approve-test-with-fixture "medium"
        (let ((buffer (approve-test-open-pr "medium")))
          (with-current-buffer buffer
            (setq test-thread--threads
                  (approve-pr-threads-for-file "internal/parser/parser.go")))
          (kill-buffer buffer)))))
  (before-each
    (setq approve-use-nerd-icons nil)
    (approve-icon-clear-cache))
  (after-all
    (setq approve-use-nerd-icons t)
    (approve-icon-clear-cache))

  (it "draws a box: the first author in the heading, a bar left of the comments"
    (test-thread--with-inserted (car test-thread--threads)
      (let ((lines (test-thread--lines)))
        (expect (car lines) :to-match "\\`╭─ @carol-example · 2 days ago ─+\\'")
        (expect (string-width (car lines)) :to-equal (approve-thread-width))
        (expect (nth 1 lines) :to-equal "│ This allocates on every call; hoist the buffer out of the loop.")
        (expect (nth 2 lines) :to-equal "│")
        (expect (nth 3 lines) :to-match "\\`│ @alice-example · 2 days ago")
        ;; A plain section buffer binds no key to reply or resolve, so
        ;; the rule names none (approve-render-key-hints).
        (expect (car (last lines)) :to-match "\\`╰─+\\'"))))

  (it "makes a section for the thread and one for each comment"
    (let ((thread (car test-thread--threads)))
      (test-thread--with-inserted thread
        (let ((section (car (oref magit-root-section children))))
          (expect (oref section type) :to-be 'approve-thread-section)
          (expect (oref section value) :to-equal (alist-get 'id thread))
          (expect (mapcar (lambda (child) (oref child value)) (oref section children))
                  :to-equal (mapcar (lambda (comment) (alist-get 'id comment))
                                    (approve-model-nodes (alist-get 'comments thread))))))))

  (it "says who resolved a thread, which does not fold by itself"
    (test-thread--with-inserted (test-thread--find (lambda (thread)
                                                     (alist-get 'isResolved thread)))
      (let ((section (car (oref magit-root-section children))))
        ;; Its line or group folds it, leaving no line of it behind.
        (expect (oref section hidden) :to-be nil)
        (expect (oref section content) :to-be nil)
        (expect (car (test-thread--lines)) :to-match "resolved by")
        (expect (ensure-list (get-text-property (point-min) 'face))
                :to-contain 'approve-resolved))))

  (it "marks an outdated thread"
    (test-thread--with-inserted (test-thread--find (lambda (thread)
                                                     (alist-get 'isOutdated thread)))
      (expect (car (test-thread--lines)) :to-match "· outdated ─")))

  (it "marks a pending thread with a coloured label, not a background"
    (test-thread--with-inserted (test-thread--find
                                 (lambda (thread)
                                   (approve-thread-pending-p
                                    (car (approve-model-nodes (alist-get 'comments thread))))))
      (expect (car (test-thread--lines)) :to-match "\\`╭─ ✎ Pending · @vera-example ─")
      (search-forward "Pending")
      (expect (ensure-list (get-text-property (1- (point)) 'face)) :to-contain 'approve-pending)
      (search-forward "branch reachable")
      (expect (ensure-list (get-text-property (point) 'face)) :not :to-contain 'approve-pending)))

  (it "shows a suggestion inside the box"
    (test-thread--with-inserted (test-thread--find (lambda (thread)
                                                     (equal (alist-get 'startLine thread) 129)))
      (expect (test-thread--lines) :to-contain "│ ┌ Suggested change ─────────────────────────────────────────────────────────────")))

  (it "names the thread's first author and words in its summary"
    (expect (substring-no-properties (approve-thread-summary (car test-thread--threads)))
            :to-match "\\`@carol-example: This allocates on every call; .*…\\'")))

;;; test-approve-thread.el ends here
