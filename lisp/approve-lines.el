;;; approve-lines.el --- GitHub's lines in a buffer of the clone -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; URL: https://github.com/fuzzycode/approve.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; A buffer of a file in a clone is not GitHub's version of the file:
;; commits not pushed, edits not saved, a branch behind GitHub's or
;; rebased.  This module maps GitHub's lines onto the buffer's and
;; back, from a diff of the two texts (decision D45), so the threads
;; of a pull request go under their lines there (docs/flows.org, F21):
;;
;; - `approve-lines-watch' compares GitHub's text of the buffer's file
;;   (at the pull request's head, or at an outdated thread's commit)
;;   with the buffer's text, unsaved edits included, and calls back
;;   with a map.  It does so at once, and again after the buffer
;;   changes, once Emacs has been idle for `approve-lines-idle-delay'.
;;   Both texts go to temporary files, and `diff-command' compares
;;   them in an asynchronous process: Emacs is busy only writing the
;;   buffer, once for all the texts it is compared with.  Edits are
;;   coalesced into the next comparison, and an answer for older text
;;   is dropped.
;; - `approve-lines-to-buffer' says where one of GitHub's lines is in
;;   the buffer: the same line, the line now in place of one edited
;;   here, or where lines deleted here were.
;; - `approve-lines-to-github' says which of GitHub's lines a line of
;;   the buffer is: GitHub's own, the only kind GitHub takes comments
;;   on, one edited here, or one added here, which GitHub hasn't.
;; - `approve-lines-unwatch' stops, and deletes the temporary files;
;;   killing the buffer or changing its major mode does too, and the
;;   files left are deleted when Emacs exits.
;;
;; Carriage returns at the ends of lines and a missing newline at the
;; end are no change, on either side.
;;
;; Layer rules: this is a module for a local clone's buffers (D36),
;; which the core never requires.  It reads a buffer's text and never
;; changes it, and knows nothing of pull requests, the store or git:
;; its caller gives it GitHub's text (`approve-clone-file-text' reads
;; it from the clone).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'diff)
(require 'approve-log)

(defcustom approve-lines-idle-delay 0.5
  "Seconds of idleness after an edit before the lines are compared again.
Threads move with the text while it is edited; then Approve compares
the buffer with GitHub's file again, and says anew which lines were
edited here.  To compare sooner:

  (setq approve-lines-idle-delay 0.2)"
  :type 'number
  :group 'approve)

(define-error 'approve-lines-error "Could not compare the lines with GitHub's"
              'approve-error)

;;; The map

(cl-defstruct (approve-lines-map (:constructor approve-lines--make-map)
                                 (:copier nil))
  "How the lines of GitHub's text and of a buffer's map onto each other.
CHANGES is a vector of the changes from GitHub's text to the buffer's,
in order, each [OLD OLD-COUNT NEW NEW-COUNT]: OLD-COUNT of GitHub's
lines from line OLD became NEW-COUNT of the buffer's lines from line
NEW.  A side with no lines names the line after the change.  Between
changes, the lines are the same on both sides.  TICK is the buffer's
`buffer-chars-modified-tick' when its text was compared, or nil."
  changes tick)

(defconst approve-lines--hunk-regexp
  (rx bol "@@ -" (group (+ digit)) (? "," (group (+ digit)))
      " +" (group (+ digit)) (? "," (group (+ digit))) " @@")
  "Regexp matching a hunk header of a unified diff.
Its groups are the first line and the count of each side.")

(defun approve-lines-parse ()
  "Return the map of the diff in the current buffer.
The diff is unified and without context (diff -U0), from GitHub's
text to the buffer's; only its hunk headers are read, as GNU's and
BSD's diff write them.

Example:
  (with-temp-buffer
    (insert \"@@ -2 +1,0 @@\\n-two\\n\")
    (approve-lines-to-buffer (approve-lines-parse) 3))
  => (2 . same)"
  (save-excursion
    (goto-char (point-min))
    (let ((changes nil) (delta 0))
      (while (re-search-forward approve-lines--hunk-regexp nil t)
        (let ((old (string-to-number (match-string 1)))
              (old-count (if (match-beginning 2) (string-to-number (match-string 2)) 1))
              (new (string-to-number (match-string 3)))
              (new-count (if (match-beginning 4) (string-to-number (match-string 4)) 1)))
          ;; Where a side with no lines is, diffs don't say alike: GNU
          ;; names the line before, BSD the line after at the top of
          ;; the text.  The lines before are the same on both sides,
          ;; so the other side says.
          (cond ((zerop old-count) (setq old (- new delta)))
                ((zerop new-count) (setq new (+ old delta))))
          (push (vector old old-count new new-count) changes)
          (setq delta (+ delta (- new-count old-count)))))
      (approve-lines--make-map :changes (vconcat (nreverse changes))))))

(defun approve-lines--change-before (changes line side)
  "Return the last of CHANGES that starts at or before LINE on SIDE, or nil.
SIDE is 0 for GitHub's lines, 2 for the buffer's.  CHANGES are in
order on both sides, so a binary search finds it."
  (let ((low 0) (high (1- (length changes))) (found nil))
    (while (<= low high)
      (let ((middle (/ (+ low high) 2)))
        (if (<= (aref (aref changes middle) side) line)
            (setq found (aref changes middle) low (1+ middle))
          (setq high (1- middle)))))
    found))

(defun approve-lines--translate (map line from to alone)
  "Return where LINE on side FROM of MAP is on side TO, as (LINE . STATE).
FROM and TO are 0 for GitHub's lines and 2 for the buffer's.  STATE
is `same', `edited', or ALONE when LINE's change has lines on side
FROM only; LINE is then the line before the change on side TO."
  (let ((change (approve-lines--change-before (approve-lines-map-changes map) line from)))
    (if (null change)
        (cons line 'same)
      (let ((start (aref change from))
            (count (aref change (1+ from)))
            (other (aref change to))
            (other-count (aref change (1+ to))))
        (cond
         ((>= line (+ start count))
          (cons (+ line (- (+ other other-count) (+ start count))) 'same))
         ((zerop other-count)
          (cons (1- other) alone))
         (t
          (cons (+ other (min (- line start) (1- other-count))) 'edited)))))))

(defun approve-lines-to-buffer (map line)
  "Return where GitHub's LINE is in the buffer, by MAP, as (LINE . STATE).
STATE is `same' when the buffer has the line as GitHub has it;
`edited' when it was changed here, the value's LINE being the line
in its place (GitHub's lines of a change go to the buffer's in order,
those past the last to the last); `deleted' when it was deleted here,
the value's LINE being the line before where it was, 0 at the top.
A line past the end of GitHub's text maps past the end of the
buffer's.

Example, when the buffer's line 2 is new and GitHub's line 4 was
deleted:
  (approve-lines-to-buffer map 3) => (4 . same)
  (approve-lines-to-buffer map 4) => (4 . deleted)"
  (approve-lines--translate map line 0 2 'deleted))

(defun approve-lines-to-github (map line)
  "Return which of GitHub's lines the buffer's LINE is, by MAP, as (LINE . STATE).
STATE is `same' when it is GitHub's line, unchanged: the only kind
GitHub takes a comment on (decision D45); `edited' when it was changed
here, the value's LINE being GitHub's line it replaced (paired as
`approve-lines-to-buffer' pairs them); `added' when it was added here,
so GitHub hasn't it, the value's LINE being GitHub's line before it,
0 at the top.

Example, when the buffer's line 2 is new:
  (approve-lines-to-github map 2) => (1 . added)
  (approve-lines-to-github map 3) => (2 . same)"
  (approve-lines--translate map line 2 0 'added))

(defun approve-lines-current-p (map)
  "Return non-nil if MAP is for the current buffer's text as it is now.
A map made before an edit maps the lines of the text before it."
  (and map (eql (approve-lines-map-tick map) (buffer-chars-modified-tick))))

;;; Temporary files

(defvar approve-lines--files nil
  "The temporary files of the texts compared, deleted when Emacs exits.")

(defun approve-lines--temp-file (prefix)
  "Return a new temporary file whose name starts with PREFIX."
  (let ((file (make-temp-file prefix)))
    (push file approve-lines--files)
    file))

(defun approve-lines--delete-file (file)
  "Delete the temporary FILE, if it is one; nothing when nil."
  (when file
    (setq approve-lines--files (delete file approve-lines--files))
    (ignore-errors (delete-file file))))

(defun approve-lines--delete-files ()
  "Delete the temporary files left: Emacs exits."
  (mapc #'approve-lines--delete-file (copy-sequence approve-lines--files)))

(add-hook 'kill-emacs-hook #'approve-lines--delete-files)

(defvar-local approve-lines--carriage-returns nil
  "Non-nil when this buffer's text may hold a carriage return.
Looked for once, then in each change: when there is none, the buffer
is written as it is, with no copy.")

(defun approve-lines--write (text file)
  "Write TEXT to FILE, or the current buffer's whole text when TEXT is nil.
Carriage returns at the ends of lines are left out, and a newline is
added at the end when there is none: neither is a change.  The text
is written as Emacs holds it (`no-conversion'), which is UTF-8 for
any text, and ten times faster to write than encoding it.  Nothing is
said in the echo area."
  (when (and (null text) approve-lines--carriage-returns)
    (setq text (save-restriction
                 (widen)
                 (buffer-substring-no-properties (point-min) (point-max)))))
  (when (and text (string-search "\r" text))
    (setq text (string-remove-suffix "\r" (string-replace "\r\n" "\n" text))))
  (let ((coding-system-for-write 'no-conversion)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil)
        (write-region-inhibit-fsync t)
        (create-lockfiles nil)
        (last (if text
                  (and (not (string-empty-p text)) (aref text (1- (length text))))
                (save-restriction (widen) (char-before (point-max))))))
    (write-region text nil file nil 'silent)
    (when (and last (/= last ?\n))
      (write-region "\n" nil file t 'silent))))

;;; Watching a buffer

(cl-defstruct (approve-lines--watch (:constructor approve-lines--make-watch)
                                    (:copier nil))
  "One of GitHub's texts of a buffer's file, compared with the buffer's text.
KEY names it among the buffer's; FILE is the temporary file of the
text; CALLBACK and ERRORBACK are the watcher's; PROCESS is the diff
running, or nil; MAP is the last answer, or nil."
  key file callback errorback process map)

(defvar-local approve-lines--watches nil
  "GitHub's texts this buffer's text is compared with, newest first.")

(defvar-local approve-lines--file nil
  "The temporary file of this buffer's text, or nil.")

(defvar-local approve-lines--written nil
  "The `buffer-chars-modified-tick' of the text in `approve-lines--file'.")

(defvar-local approve-lines--timer nil
  "The idle timer of the next comparison, or nil.")

(defun approve-lines--find (key)
  "Return the watch of GitHub's text KEY in the current buffer, or nil."
  (seq-find (lambda (watch) (equal (approve-lines--watch-key watch) key))
            approve-lines--watches))

(defun approve-lines--call (function argument)
  "Call FUNCTION, a watcher's, with ARGUMENT; log its error, if any."
  (when function
    (condition-case-unless-debug err
        (funcall function argument)
      (error (approve-log-condition "Comparing the lines with GitHub's" err)))))

(defun approve-lines--fail (watch message)
  "Log MESSAGE and call the errorback of WATCH with it as an error."
  (approve-log 'error "Comparing the lines with GitHub's: %s" message)
  (approve-lines--call (approve-lines--watch-errorback watch)
                       (list 'approve-lines-error message)))

(defun approve-lines--stop (watch)
  "Stop the diff WATCH runs, if any, and drop its answer."
  (when-let* ((process (approve-lines--watch-process watch)))
    (setf (approve-lines--watch-process watch) nil)
    (let ((output (process-buffer process)))
      (delete-process process)
      (when (buffer-live-p output)
        (kill-buffer output)))))

(defun approve-lines--schedule ()
  "Compare the lines again once Emacs has been idle a while.
The while is `approve-lines-idle-delay', from now when Emacs is idle
already (a timer or a process changed the text).  Changes until then
wait for the same comparison."
  (unless (timerp approve-lines--timer)
    (setq approve-lines--timer
          (run-with-idle-timer (time-add (or (current-idle-time) 0) approve-lines-idle-delay)
                               nil #'approve-lines--idle (current-buffer)))))

(defun approve-lines--carriage-return-p (start end)
  "Return non-nil if the text from START to END holds a carriage return."
  (save-excursion
    (goto-char start)
    (skip-chars-forward "^\r" end)
    (< (point) end)))

(defun approve-lines--changed (start end _length)
  "Compare the lines again soon: the text from START to END changed.
For `after-change-functions'; the comparison waits for Emacs to be
idle (`approve-lines--schedule')."
  (when approve-lines--watches
    (unless approve-lines--carriage-returns
      (setq approve-lines--carriage-returns (approve-lines--carriage-return-p start end)))
    (approve-lines--schedule)))

(defun approve-lines--idle (buffer)
  "Compare the text of BUFFER with GitHub's texts, from an idle timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq approve-lines--timer nil)
      (condition-case-unless-debug err
          (approve-lines-update)
        (error (approve-log-condition "Comparing the lines with GitHub's" err))))))

(defun approve-lines--answer (process buffer watch event)
  "Take the answer of PROCESS, the diff of WATCH in BUFFER, once it ended.
EVENT is what the sentinel was told.  An answer for text older than
the buffer's is dropped, and another comparison asked for; one that
was stopped is ignored."
  (unless (process-live-p process)
    (let ((output (process-buffer process)))
      (unwind-protect
          (when (and (buffer-live-p buffer) (buffer-live-p output)
                     (eq process (approve-lines--watch-process watch)))
            (with-current-buffer buffer
              (setf (approve-lines--watch-process watch) nil)
              (cond
               ;; diff exits with 0 when the texts are the same, 1 when not.
               ((not (and (eq (process-status process) 'exit)
                          (<= (process-exit-status process) 1)))
                (approve-lines--fail
                 watch (format "%s %s: %s" diff-command (string-trim event)
                               (with-current-buffer output
                                 (string-trim (buffer-string))))))
               ((eql (process-get process 'approve-lines-tick)
                     (buffer-chars-modified-tick))
                (let ((map (with-current-buffer output (approve-lines-parse))))
                  (setf (approve-lines-map-tick map) (process-get process 'approve-lines-tick)
                        (approve-lines--watch-map watch) map)
                  (approve-lines--call (approve-lines--watch-callback watch) map)))
               (t
                (approve-lines--schedule)))))
        (when (buffer-live-p output)
          (kill-buffer output))))))

(defun approve-lines--diff (watch tick)
  "Compare GitHub's text of WATCH with the buffer's, written at TICK.
The diff runs in an asynchronous process on the temporary files."
  (approve-lines--stop watch)
  (let ((buffer (current-buffer))
        (output (generate-new-buffer " *approve-lines*" t))
        ;; The process runs here, and a buffer's directory may be remote.
        (default-directory temporary-file-directory))
    (condition-case err
        (let ((process
               (make-process
                :name "approve-lines"
                :buffer output
                ;; diff's own --strip-trailing-cr takes a second on a
                ;; 200 000-line file with macOS's diff: the texts are
                ;; written without them (`approve-lines--write').
                :command (list diff-command "-a" "-U0"
                               (approve-lines--watch-file watch) approve-lines--file)
                :coding 'binary
                :connection-type 'pipe
                :noquery t
                :sentinel (lambda (process event)
                            (approve-lines--answer process buffer watch event)))))
          (process-put process 'approve-lines-tick tick)
          (setf (approve-lines--watch-process watch) process))
      (error
       (kill-buffer output)
       (approve-lines--fail watch (format "%s: %s" diff-command
                                          (error-message-string err)))))))

(defun approve-lines-update ()
  "Compare the current buffer's text with GitHub's texts it watches, now.
Only the texts with neither an answer nor a comparison running for
the buffer's text as it is now are compared.  The buffer is written
to its temporary file once for all of them, which is all the time
Emacs spends; the answers come later (`approve-lines-watch')."
  (when (timerp approve-lines--timer)
    (cancel-timer approve-lines--timer))
  (setq approve-lines--timer nil)
  (let* ((tick (buffer-chars-modified-tick))
         (stale (seq-remove
                 (lambda (watch)
                   (or (eql tick (when-let* ((map (approve-lines--watch-map watch)))
                                   (approve-lines-map-tick map)))
                       (eql tick (when-let* ((process (approve-lines--watch-process watch)))
                                   (process-get process 'approve-lines-tick)))))
                 approve-lines--watches)))
    (when stale
      (unless (eql tick approve-lines--written)
        ;; Every diff running compares older text, and reads the file
        ;; about to be written: its answer would be dropped anyway.
        (mapc #'approve-lines--stop approve-lines--watches)
        (unless approve-lines--file
          (setq approve-lines--file (approve-lines--temp-file "approve-buffer-")))
        (approve-lines--write nil approve-lines--file)
        (setq approve-lines--written tick))
      (dolist (watch stale)
        (approve-lines--diff watch tick)))))

(cl-defun approve-lines-watch (key text &key callback errorback)
  "Compare TEXT, GitHub's version of the current buffer's file, with the buffer.
KEY names TEXT among the texts the buffer is compared with, such as
the commit it is at; watching KEY again replaces its text.  The
buffer's whole text is compared, unsaved edits included, at once and
again after it changes, once Emacs has been idle for
`approve-lines-idle-delay' (decision D45).

CALLBACK is called in the buffer with the map of each answer, an
`approve-lines-map' for the buffer's text as it is then (see
`approve-lines-to-buffer' and `approve-lines-to-github').  ERRORBACK
is called there with the error, (approve-lines-error MESSAGE), when
`diff-command' fails; it is logged either way.  Neither is called
once the buffer is killed or the text unwatched.

Example:
  (approve-lines-watch \"6ffac57\" (approve-clone-file-text clone oid path)
                       :callback (lambda (map)
                                   (message \"Line 42 is here %S\"
                                            (approve-lines-to-buffer map 42))))"
  (let ((watch (approve-lines--find key)))
    (unless approve-lines--watches
      (setq approve-lines--carriage-returns
            (save-restriction
              (widen)
              (approve-lines--carriage-return-p (point-min) (point-max)))))
    (if watch
        (progn
          (approve-lines--stop watch)
          (setf (approve-lines--watch-map watch) nil))
      (setq watch (approve-lines--make-watch
                   :key key :file (approve-lines--temp-file "approve-github-")))
      (push watch approve-lines--watches))
    (setf (approve-lines--watch-callback watch) callback
          (approve-lines--watch-errorback watch) errorback)
    (approve-lines--write text (approve-lines--watch-file watch))
    (add-hook 'after-change-functions #'approve-lines--changed nil t)
    (add-hook 'kill-buffer-hook #'approve-lines-unwatch nil t)
    (add-hook 'change-major-mode-hook #'approve-lines-unwatch nil t)
    (approve-lines-update)
    nil))

(defun approve-lines-unwatch (&optional key)
  "Stop comparing GitHub's text KEY with the current buffer's, or every one.
Its diff is stopped and its temporary file deleted.  With the last
one, the buffer's temporary file goes too, and the buffer is left as
it was before `approve-lines-watch'."
  (dolist (watch (if key
                     (ensure-list (approve-lines--find key))
                   (copy-sequence approve-lines--watches)))
    (approve-lines--stop watch)
    (approve-lines--delete-file (approve-lines--watch-file watch))
    (setq approve-lines--watches (delq watch approve-lines--watches)))
  (unless approve-lines--watches
    (when (timerp approve-lines--timer)
      (cancel-timer approve-lines--timer))
    (approve-lines--delete-file approve-lines--file)
    (setq approve-lines--timer nil
          approve-lines--file nil
          approve-lines--written nil
          approve-lines--carriage-returns nil)
    (remove-hook 'after-change-functions #'approve-lines--changed t)
    (remove-hook 'kill-buffer-hook #'approve-lines-unwatch t)
    (remove-hook 'change-major-mode-hook #'approve-lines-unwatch t)))

(defun approve-lines-watched-map (key)
  "Return the last map of GitHub's text KEY in the current buffer, or nil.
It is nil before the first answer, and may be for older text than the
buffer's (`approve-lines-current-p')."
  (when-let* ((watch (approve-lines--find key)))
    (approve-lines--watch-map watch)))

(provide 'approve-lines)

;;; approve-lines.el ends here
