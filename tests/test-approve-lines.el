;;; test-approve-lines.el --- Tests for GitHub's lines in a buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; The map of GitHub's lines onto a buffer's and back, and the watch
;; that makes it from a diff of the texts, again after edits (decision
;; D45).  The diffs run `diff-command' on temporary files; the tests
;; are skipped where it is missing.  Idle timers don't run in batch, so
;; the tests run the timer's function themselves.

;;; Code:

(require 'buttercup)
(require 'approve-lines)
(require 'approve-test-helpers "helpers")

(defun test-lines--map (&rest headers)
  "Return the map of a diff with the hunk HEADERS, given as strings."
  (with-temp-buffer
    (dolist (header headers)
      (insert header "\n"))
    (approve-lines-parse)))

(defun test-lines--text (&rest lines)
  "Return LINES as a text, each ending in a newline."
  (mapconcat (lambda (line) (concat line "\n")) lines ""))

(defun test-lines--wait (predicate)
  "Wait for PREDICATE to return non-nil, five seconds at most."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))))

(defun test-lines--idle (buffer)
  "Run the idle timer of BUFFER, as Emacs would once idle."
  (let ((timer (buffer-local-value 'approve-lines--timer buffer)))
    (expect (timerp timer) :to-be t)
    (cancel-timer timer)
    (approve-lines--idle buffer)))

(defvar test-lines--maps nil
  "The maps the watch of a test called back with, newest first.")

(defmacro test-lines--with-buffer (text &rest body)
  "Run BODY in a temporary buffer holding TEXT.
`test-lines--maps' collects the maps of its watches."
  (declare (indent 1) (debug t))
  `(progn
     (assume (executable-find diff-command) "diff is not installed")
     (let ((test-lines--maps nil))
       (with-temp-buffer
         (insert ,text)
         (unwind-protect
             (progn ,@body)
           (approve-lines-unwatch))))))

(defun test-lines--watch (key text)
  "Watch TEXT as KEY in the current buffer, and wait for its first map."
  (let ((count (length test-lines--maps)))
    (approve-lines-watch key text :callback (lambda (map) (push map test-lines--maps)))
    (test-lines--wait (lambda () (> (length test-lines--maps) count)))
    (car test-lines--maps)))

(describe "approve-lines-parse"
  (it "reads the hunk headers, a missing count being 1"
    (expect (approve-lines-map-changes
             (test-lines--map "@@ -3,2 +3,3 @@" "@@ -8 +9 @@"))
            :to-equal [[3 2 3 3] [8 1 9 1]]))

  (it "names the line after a change for a side with no lines"
    (expect (approve-lines-map-changes
             (test-lines--map "@@ -0,0 +1,2 @@" "@@ -4 +5,0 @@" "@@ -9,0 +11 @@"))
            :to-equal [[1 0 1 2] [4 1 6 0] [10 0 11 1]]))

  (it "reads BSD's diff, which names the first line for no lines at the top"
    ;; GNU's diff says -0,0 and +0,0 there.
    (expect (approve-lines-map-changes (test-lines--map "@@ -1,0 +1,2 @@" "@@ -1,0 +4 @@"))
            :to-equal [[1 0 1 2] [2 0 4 1]])
    (expect (approve-lines-map-changes (test-lines--map "@@ -1 +1,0 @@"))
            :to-equal [[1 1 1 0]]))

  (it "reads no change from no diff, and skips the lines of the hunks"
    (expect (approve-lines-map-changes (test-lines--map)) :to-equal [])
    (expect (approve-lines-map-changes
             (test-lines--map "@@ -2 +2 @@" "-@@ -9 +9 @@" "+@@ -9 +9 @@"))
            :to-equal [[2 1 2 1]])))

(describe "approve-lines-to-buffer"
  (it "maps every line to itself when nothing changed"
    (let ((map (test-lines--map)))
      (expect (approve-lines-to-buffer map 1) :to-equal '(1 . same))
      (expect (approve-lines-to-buffer map 500) :to-equal '(500 . same))))

  (it "moves the lines after lines added or deleted here"
    ;; Two lines added before GitHub's line 3; GitHub's line 6 deleted.
    (let ((map (test-lines--map "@@ -2,0 +3,2 @@" "@@ -6 +7,0 @@")))
      (expect (approve-lines-to-buffer map 2) :to-equal '(2 . same))
      (expect (approve-lines-to-buffer map 3) :to-equal '(5 . same))
      (expect (approve-lines-to-buffer map 5) :to-equal '(7 . same))
      (expect (approve-lines-to-buffer map 7) :to-equal '(8 . same))))

  (it "puts a line deleted here after the line before it, 0 at the top"
    (let ((map (test-lines--map "@@ -1,2 +0,0 @@" "@@ -6,2 +3,0 @@")))
      (expect (approve-lines-to-buffer map 1) :to-equal '(0 . deleted))
      (expect (approve-lines-to-buffer map 2) :to-equal '(0 . deleted))
      (expect (approve-lines-to-buffer map 3) :to-equal '(1 . same))
      (expect (approve-lines-to-buffer map 6) :to-equal '(3 . deleted))
      (expect (approve-lines-to-buffer map 7) :to-equal '(3 . deleted))
      (expect (approve-lines-to-buffer map 8) :to-equal '(4 . same))))

  (it "puts a line edited here on the line in its place, in order, the rest on the last"
    ;; GitHub's lines 3 to 5 became the buffer's lines 3 and 4.
    (let ((map (test-lines--map "@@ -3,3 +3,2 @@")))
      (expect (approve-lines-to-buffer map 3) :to-equal '(3 . edited))
      (expect (approve-lines-to-buffer map 4) :to-equal '(4 . edited))
      (expect (approve-lines-to-buffer map 5) :to-equal '(4 . edited))
      (expect (approve-lines-to-buffer map 6) :to-equal '(5 . same)))))

(describe "approve-lines-to-github"
  (it "says GitHub's line of an unchanged line, also past changes"
    (let ((map (test-lines--map "@@ -2,0 +3,2 @@" "@@ -6 +7,0 @@")))
      (expect (approve-lines-to-github map 1) :to-equal '(1 . same))
      (expect (approve-lines-to-github map 5) :to-equal '(3 . same))
      (expect (approve-lines-to-github map 7) :to-equal '(5 . same))
      (expect (approve-lines-to-github map 8) :to-equal '(7 . same))))

  (it "says a line added here comes after GitHub's line before it, 0 at the top"
    (let ((map (test-lines--map "@@ -0,0 +1,2 @@" "@@ -2,0 +5,2 @@")))
      (expect (approve-lines-to-github map 1) :to-equal '(0 . added))
      (expect (approve-lines-to-github map 2) :to-equal '(0 . added))
      (expect (approve-lines-to-github map 3) :to-equal '(1 . same))
      (expect (approve-lines-to-github map 5) :to-equal '(2 . added))
      (expect (approve-lines-to-github map 6) :to-equal '(2 . added))
      (expect (approve-lines-to-github map 7) :to-equal '(3 . same))))

  (it "says which of GitHub's lines a line edited here replaced"
    ;; GitHub's lines 3 and 4 became the buffer's lines 3 to 5.
    (let ((map (test-lines--map "@@ -3,2 +3,3 @@")))
      (expect (approve-lines-to-github map 3) :to-equal '(3 . edited))
      (expect (approve-lines-to-github map 4) :to-equal '(4 . edited))
      (expect (approve-lines-to-github map 5) :to-equal '(4 . edited))
      (expect (approve-lines-to-github map 6) :to-equal '(5 . same))))

  (it "finds the change of a line among many"
    (let ((map (apply #'test-lines--map
                      (cl-loop for line from 10 to 1000 by 10
                               collect (format "@@ -%d,0 +%d @@" line (+ line (/ line 10)))))))
      ;; One line added after each tenth of GitHub's lines.
      (expect (approve-lines-to-github map 548) :to-equal '(499 . same))
      (expect (approve-lines-to-github map 549) :to-equal '(500 . same))
      (expect (approve-lines-to-github map 550) :to-equal '(500 . added))
      (expect (approve-lines-to-buffer map 495) :to-equal '(544 . same))
      (expect (approve-lines-to-buffer map 1000) :to-equal '(1099 . same)))))

(describe "approve-lines-watch"
  (it "compares the buffer's text with GitHub's and calls back with the map"
    (test-lines--with-buffer (test-lines--text "one" "new" "two" "three" "five")
      (let ((map (test-lines--watch "head" (test-lines--text "one" "two" "three" "four" "five"))))
        (expect (approve-lines-current-p map) :to-be-truthy)
        (expect (approve-lines-watched-map "head") :to-be map)
        (expect (approve-lines-to-github map 2) :to-equal '(1 . added))
        (expect (approve-lines-to-buffer map 3) :to-equal '(4 . same))
        (expect (approve-lines-to-buffer map 4) :to-equal '(4 . deleted)))))

  (it "compares the whole text, unsaved and narrowed, ignoring carriage returns and a last newline"
    (test-lines--with-buffer "one\ntwo\nthree"
      (narrow-to-region (point-min) (line-end-position -1))
      (let ((map (test-lines--watch "head" "one\r\ntwo\r\nthree\r\n")))
        (expect (approve-lines-map-changes map) :to-equal []))))

  (it "ignores the buffer's carriage returns, there at first or typed later"
    (test-lines--with-buffer "one\r\ntwo\r\nthree\n"
      (let ((map (test-lines--watch "head" (test-lines--text "one" "two" "three"))))
        (expect (approve-lines-map-changes map) :to-equal [])))
    (test-lines--with-buffer (test-lines--text "one" "two" "three")
      (test-lines--watch "head" (test-lines--text "one" "two" "three"))
      (goto-char (point-min))
      (end-of-line)
      (insert "\r")
      (test-lines--idle (current-buffer))
      (test-lines--wait (lambda () (= (length test-lines--maps) 2)))
      (expect (approve-lines-map-changes (car test-lines--maps)) :to-equal [])
      (insert "\r")
      (test-lines--idle (current-buffer))
      (test-lines--wait (lambda () (= (length test-lines--maps) 3)))
      (expect (approve-lines-to-github (car test-lines--maps) 1) :to-equal '(1 . edited))))

  (it "compares again once Emacs is idle after an edit, and drops an answer for older text"
    (test-lines--with-buffer (test-lines--text "one" "two" "three")
      (test-lines--watch "head" (test-lines--text "one" "two" "three"))
      (expect (length test-lines--maps) :to-equal 1)
      (goto-char (point-min))
      (insert "zero\n")
      (test-lines--idle (current-buffer))
      ;; The answer to come is for text edited since: it is dropped.
      (insert "again\n")
      (test-lines--wait (lambda () (null (approve-lines--watch-process
                                          (approve-lines--find "head")))))
      (expect (length test-lines--maps) :to-equal 1)
      (test-lines--idle (current-buffer))
      (test-lines--wait (lambda () (= (length test-lines--maps) 2)))
      (expect (approve-lines-to-github (car test-lines--maps) 3) :to-equal '(1 . same))
      (expect (approve-lines-current-p (car test-lines--maps)) :to-be-truthy)
      (expect (approve-lines-current-p (cadr test-lines--maps)) :not :to-be-truthy)))

  (it "writes the buffer once for every text, and compares only the texts that need it"
    (test-lines--with-buffer (test-lines--text "one" "two" "three")
      (spy-on 'approve-lines--write :and-call-through)
      (test-lines--watch "head" (test-lines--text "one" "two"))
      (test-lines--watch "base" (test-lines--text "two" "three"))
      ;; Each text, and the buffer once.
      (expect (spy-calls-count 'approve-lines--write) :to-equal 3)
      (approve-lines-update)
      (expect (spy-calls-count 'approve-lines--write) :to-equal 3)
      (goto-char (point-max))
      (insert "four\n")
      (test-lines--idle (current-buffer))
      (test-lines--wait (lambda () (= (length test-lines--maps) 4)))
      (expect (spy-calls-count 'approve-lines--write) :to-equal 4)
      (expect (approve-lines-to-github (approve-lines-watched-map "base") 4)
              :to-equal '(2 . added))
      (expect (approve-lines-to-github (approve-lines-watched-map "head") 4)
              :to-equal '(2 . added))))

  (it "replaces the text of a key watched again"
    (test-lines--with-buffer (test-lines--text "one" "two")
      (test-lines--watch "head" (test-lines--text "one"))
      (let ((map (test-lines--watch "head" (test-lines--text "one" "two"))))
        (expect (approve-lines-map-changes map) :to-equal [])
        (expect (length approve-lines--watches) :to-equal 1))))

  (it "calls the errorback with what diff said when it fails"
    (test-lines--with-buffer (test-lines--text "one")
      (let ((failure nil))
        (approve-lines-watch "head" (test-lines--text "one")
                             :callback #'ignore
                             :errorback (lambda (err) (setq failure err)))
        (test-lines--wait (lambda () (null (approve-lines--watch-process
                                            (approve-lines--find "head")))))
        (delete-file (approve-lines--watch-file (approve-lines--find "head")))
        (insert "two\n")
        (approve-lines-update)
        (test-lines--wait (lambda () failure))
        (expect (car failure) :to-be 'approve-lines-error)
        (expect (cadr failure) :to-match "exited abnormally with code 2"))))

  (it "calls the errorback when there is no diff program"
    (test-lines--with-buffer (test-lines--text "one")
      (let* ((failure nil)
             (diff-command "approve-no-such-diff"))
        (approve-lines-watch "head" (test-lines--text "one")
                             :errorback (lambda (err) (setq failure err)))
        (expect (car failure) :to-be 'approve-lines-error)
        (expect (cadr failure) :to-match "approve-no-such-diff")))))

(describe "approve-lines-current-map"
  (it "waits for the map of the text as it is now, after an edit"
    (test-lines--with-buffer (test-lines--text "one" "two")
      (let ((first (test-lines--watch "head" (test-lines--text "one" "two"))))
        (expect (approve-lines-current-map "head") :to-be first)
        (goto-char (point-min))
        (insert "zero\n")
        (let ((map (approve-lines-current-map "head")))
          (expect (approve-lines-current-p map) :to-be t)
          (expect (approve-lines-to-github map 1) :to-equal '(0 . added))
          (expect (approve-lines-to-github map 3) :to-equal '(2 . same))
          ;; The watcher heard of it too, and the idle timer is gone.
          (expect (car test-lines--maps) :to-be map)
          (expect approve-lines--timer :to-be nil)))))

  (it "is nil for a text not watched, or when diff fails"
    (test-lines--with-buffer (test-lines--text "one")
      (expect (approve-lines-current-map "head") :to-be nil)
      (test-lines--watch "head" (test-lines--text "one"))
      (delete-file (approve-lines--watch-file (approve-lines--find "head")))
      (insert "two\n")
      (expect (approve-lines-current-map "head") :to-be nil))))

(describe "approve-lines-unwatch"
  (it "stops the diffs, deletes the temporary files and leaves the buffer as it was"
    (test-lines--with-buffer (test-lines--text "one")
      (let ((hooks after-change-functions))
        (test-lines--watch "head" (test-lines--text "one"))
        (test-lines--watch "base" (test-lines--text "two"))
        (let ((head (approve-lines--watch-file (approve-lines--find "head")))
              (base (approve-lines--watch-file (approve-lines--find "base")))
              (own approve-lines--file))
          (insert "two\n")
          (approve-lines-update)
          (approve-lines-unwatch "head")
          (expect (file-exists-p head) :not :to-be-truthy)
          (expect (file-exists-p own) :to-be-truthy)
          (approve-lines-unwatch)
          (expect (file-exists-p base) :not :to-be-truthy)
          (expect (file-exists-p own) :not :to-be-truthy)
          (expect approve-lines--files :not :to-contain own)
          (expect approve-lines--timer :to-be nil)
          (expect after-change-functions :to-equal hooks)
          (expect (seq-filter (lambda (process)
                                (equal (process-name process) "approve-lines"))
                              (process-list))
                  :to-equal nil)))))

  (it "happens when the buffer is killed or changes its major mode"
    (assume (executable-find diff-command) "diff is not installed")
    (let ((test-lines--maps nil)
          (buffer (generate-new-buffer "approve-lines-test")))
      (with-current-buffer buffer
        (insert "one\n")
        (test-lines--watch "head" "one\n")
        (let ((file approve-lines--file))
          (fundamental-mode)
          (expect approve-lines--watches :to-be nil)
          (expect (file-exists-p file) :not :to-be-truthy))
        (test-lines--watch "head" "one\n"))
      (let ((file (buffer-local-value 'approve-lines--file buffer)))
        (kill-buffer buffer)
        (expect (file-exists-p file) :not :to-be-truthy)))))

;;; test-approve-lines.el ends here
