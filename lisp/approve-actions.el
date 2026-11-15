;;; approve-actions.el --- The pipeline every change on GitHub runs -*- lexical-binding: t; -*-

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

;; Every command that changes something on GitHub runs one pipeline,
;; the action pipeline (docs/flows.org, F00):
;;
;;   context at point → does the command apply? → may the viewer? →
;;   (text in an input buffer) → mark the target in flight → mutation →
;;   patch the store → list edits → drop the draft → redraw
;;
;; A command supplies only its own parts:
;;
;;   (defun approve-resolve-preflight ()
;;     (let ((context (approve-action-context '(thread) "Put point on a thread")))
;;       (unless (plist-get context :can-resolve)
;;         (user-error "%s" "You can't resolve this thread"))
;;       (plist-get context :thread-id)))
;;
;;   (put 'approve-resolve 'approve-preflight #'approve-resolve-preflight)
;;
;;   (defun approve-resolve ()
;;     (interactive)
;;     (let ((id (approve-resolve-preflight)))
;;       (approve-action-run
;;        :what "resolve the thread"
;;        :marks `((section approve-thread-section ,id))
;;        :send (lambda (_text callback errorback)
;;                (approve-mutation-resolve-thread
;;                 id :callback callback :errorback errorback)))))
;;
;; - `approve-action-context' is the context at point when the command
;;   applies to it, else a `user-error' saying what it needs.
;; - A command's preflight finds what it would act on, or signals a
;;   `user-error' saying why it can't (nothing it applies to at point,
;;   no `viewerCan…'), and changes nothing.  The command calls it
;;   first; its `approve-preflight' property names it, so the menus
;;   dim what can't run here (`approve-action-why-not', decision D06).
;; - `approve-action-run' says why not when :why-not says the viewer
;;   may not act (a reason found only once the command has started),
;;   asks for the text when there is :input, then marks the target in
;;   flight and calls :send, which starts the mutations
;;   (approve-mutations.el: they patch the store with what GitHub
;;   returns and make the list edits, decision D35).  Once GitHub
;;   confirms, :done does what else the command needs, the draft goes,
;;   the mark goes, and every buffer of the PR is redrawn, point kept.
;;   When GitHub refuses, the mark goes, the echo area gives GitHub's
;;   message, and the text stays a draft for a retry.  Nothing changes
;;   on screen before GitHub confirms (decision D14).
;; - A mark says what to show as waiting for GitHub: the heading of a
;;   section, (section TYPE VALUE), or diff lines, (lines RANGE).  It
;;   is dimmed (`approve-inflight') with a loading icon after it, by
;;   overlays put back after each redraw, so renderers know nothing of
;;   it.
;;
;; Layer rules: this is the actions layer, the only place where the
;; context, input, network and redraw meet.  It reaches the network
;; through the functions commands give it, never ghub.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'approve-api)
(require 'approve-model)
(require 'approve-session)
(require 'approve-context)
(require 'approve-faces)
(require 'approve-icons)
(require 'approve-diff)
(require 'approve-input)
(require 'approve-pr)

;;; Context

(defun approve-action-context (&optional kinds needs)
  "Return the context at point, if a command for KINDS applies to it.
KINDS is a list of context kinds, such as (thread review-comment), a
function that takes the context and returns non-nil when it applies,
or nil for any context.  When the command does not apply to what is
at point (the author of a comment, a link in it), it applies to the
section around point if it can (the comment).  Else signal a
`user-error' with NEEDS, which says what the command needs, such as
\"Put point on a thread\".  Signal one too when no pull request has
loaded here.

Example:
  (approve-action-context \\='(file) \"Put point on a file\")
  => (:kind file :path \"a.el\" :viewed \"UNVIEWED\" :url \"…\")"
  (unless (approve-model-root-ref)
    (user-error (if (derived-mode-p 'approve-pr-mode)
                    "Wait until the pull request has loaded"
                  "Not in an Approve pull request buffer")))
  (let ((applies (lambda (context)
                   (cond ((null kinds) t)
                         ((functionp kinds) (funcall kinds context))
                         (t (memq (plist-get context :kind) kinds))))))
    (or (seq-find applies (delq nil (list (approve-context-at-point)
                                          (approve-context-of-sections))))
        (user-error "%s" (or needs "That command does not apply here")))))

;;; Whether a command can run here

(defun approve-action-why-not (command)
  "Return why COMMAND can't run at point, or nil when it can.
COMMAND's `approve-preflight' property is a function of no arguments
that returns what the command would act on, or signals a
`user-error' saying why it can't; it must change nothing.  The
command calls it first, and menus call this to dim what can't run
here (decision D06).  A command without one can run anywhere.

Example:
  (put \='approve-resolve \='approve-preflight
       #\='approve-commands--resolve-preflight)
  (approve-action-why-not \='approve-resolve) => \"Put point on a thread\""
  (when-let* ((preflight (get command 'approve-preflight)))
    (condition-case err
        (progn (funcall preflight) nil)
      (user-error (error-message-string err)))))

;;; Marks of the actions in flight

(defvar approve-action--count 0
  "The number of actions started, to tell their entries in :inflight apart.")

(defvar-local approve-action--overlays nil
  "The overlays marking what waits for GitHub in this buffer.")

(defun approve-action--buffers ()
  "Return the live buffers that share the store of the current buffer.
They are its owner (the PR buffer) and the buffers that use its store."
  (let ((owner (approve-model-owner-buffer)))
    (seq-filter (lambda (buffer)
                  (or (eq buffer owner)
                      (eq (buffer-local-value 'approve-model-store-buffer buffer) owner)))
                (buffer-list))))

(defun approve-action--sections (type value)
  "Return the sections of TYPE whose value is VALUE, in buffer order."
  (let ((found nil))
    (when magit-root-section
      (magit-map-sections (lambda (section)
                            (when (and (eq (oref section type) type)
                                       (equal (oref section value) value))
                              (push section found)))
                          magit-root-section))
    (nreverse found)))

(defun approve-action--mark-groups (mark)
  "Return the lines MARK covers in the current buffer, in groups.
Each group is a list of the positions where its lines start: the
heading of one section, or the lines of a range.  MARK is
\(section TYPE VALUE) or (lines RANGE); see `approve-action-run'."
  (pcase mark
    (`(section ,type ,value)
     (mapcar (lambda (section) (list (oref section start)))
             (approve-action--sections type value)))
    (`(lines ,range)
     (and-let* ((starts (approve-diff-range-positions range)))
       (list starts)))))

(defun approve-action--mark-line (start icon)
  "Mark the line at START as waiting for GitHub; return its overlay.
With ICON, the loading icon follows the line."
  (let ((overlay (make-overlay start (save-excursion
                                       (goto-char start)
                                       (line-end-position)))))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'approve-inflight)
    (when icon
      (overlay-put overlay 'after-string
                   (concat " " (approve-icon 'loading 'approve-inflight))))
    overlay))

(defun approve-action--decorate ()
  "Mark what waits for GitHub in the current buffer, anew.
A redraw draws the text anew, so it runs after each one."
  (mapc #'delete-overlay approve-action--overlays)
  (setq approve-action--overlays nil)
  (dolist (entry (approve-session-get :inflight))
    (dolist (mark (cdr entry))
      (dolist (group (approve-action--mark-groups mark))
        (let ((last (car (last group))))
          (dolist (start group)
            (push (approve-action--mark-line start (eql start last))
                  approve-action--overlays)))))))

(defun approve-action--decorate-all ()
  "Mark what waits for GitHub in every buffer of the current PR."
  (dolist (buffer (approve-action--buffers))
    (with-current-buffer buffer
      (approve-action--decorate))))

(defun approve-action--redraw-all ()
  "Redraw every buffer of the current PR, keeping point in each."
  (dolist (buffer (approve-action--buffers))
    (with-current-buffer buffer
      (if (derived-mode-p 'approve-pr-mode)
          (approve-pr-redraw)
        (approve-action--decorate)))))

(add-hook 'approve-pr-redraw-hook #'approve-action--decorate)

;;; The pipeline

(defun approve-action--failure-message (what err draft)
  "Return the message saying that WHAT failed with ERR.
ERR is an error from approve-api; DRAFT is non-nil when the text the
user wrote was kept."
  (concat (format "Approve: could not %s: %s" what (approve-api-error-message err))
          (when-let* ((reset (approve-api-error-reset err)))
            (format-time-string " (the limit resets at %H:%M)" reset))
          (and draft "; your text is kept as a draft")))

(defun approve-action--succeed (entry result done target)
  "End the action of ENTRY, which GitHub confirmed with RESULT.
Drop the draft of TARGET, call DONE with RESULT, then drop the mark
and redraw, even when DONE fails."
  (unwind-protect
      (progn
        (when target
          (approve-input-clear-draft target))
        (when done
          (funcall done result)))
    (approve-session-remove :inflight entry)
    (approve-action--redraw-all)))

(defun approve-action--fail (entry what err target)
  "End the action of ENTRY, which failed with ERR; WHAT says what it was.
Drop the mark and say why, keeping the draft of TARGET."
  (approve-session-remove :inflight entry)
  (approve-action--decorate-all)
  (message "%s" (approve-action--failure-message
                 what err (and target (approve-input-draft target)))))

(defun approve-action--start (what marks send done text target)
  "Mark MARKS in flight and call SEND with TEXT; see `approve-action-run'.
WHAT and DONE are as there; TARGET is the target of the input TEXT
was written in, or nil."
  (with-current-buffer (approve-model-owner-buffer)
    (let ((buffer (current-buffer))
          (entry (cons (cl-incf approve-action--count) marks))
          (settled nil)
          (sent nil))
      (approve-session-add :inflight entry)
      (approve-action--decorate-all)
      (unwind-protect
          (progn
            (funcall send text
                     (lambda (result)
                       (when (and (not settled) (buffer-live-p buffer))
                         (setq settled t)
                         (with-current-buffer buffer
                           (approve-action--succeed entry result done target))))
                     (lambda (err)
                       (when (and (not settled) (buffer-live-p buffer))
                         (setq settled t)
                         (with-current-buffer buffer
                           (approve-action--fail entry what err target)))))
            (setq sent t))
        ;; SEND signalled before it could start anything.
        (unless (or sent settled)
          (approve-session-remove :inflight entry)
          (approve-action--decorate-all))))))

(cl-defun approve-action-run (&key (what "change that on GitHub") why-not input marks
                                   send done)
  "Run the action pipeline (flow F00) for a change on GitHub.
Get the context first, with `approve-action-context'; the arguments
are the parts of the command.

WHY-NOT, when non-nil, says why the viewer may not act (no
`viewerCan…', a locked conversation): it is signalled as a
`user-error' before anything else.

INPUT, when non-nil, asks for text first: it is a plist of
arguments for `approve-input-read' (:prompt, :target, :initial,
:lines…).  The input's draft fills it; aborting keeps the text and
stops.  Sending it goes on.

MARKS say what to show as waiting for GitHub until it answers, a
list of:
  (section TYPE VALUE)   the heading of each section of TYPE with
                         VALUE, such as (section approve-thread-section
                         \"PRRT_1\")
  (lines RANGE)          the diff lines of RANGE, a context of kind
                         `line' or `lines'

SEND starts the change: it is called with the text (nil without
INPUT), a CALLBACK and an ERRORBACK, in the PR buffer.  It queues
the mutations (approve-mutations.el), which patch the store with
GitHub's answers and make the list edits, then calls CALLBACK with a
result once GitHub has confirmed them all, or ERRORBACK with the
error (a condition from approve-api).

On success, the draft of the input goes, DONE (if non-nil) is called
with the result for what else the command needs (the store is
up to date by then), the marks go and every buffer of the PR is
redrawn, point kept.  On
failure, the marks go and the echo area says WHAT could not be done
and why, such as \"Approve: could not resolve the thread: …\"; the
text stays a draft.

Return the input buffer when there is INPUT, else nil."
  (when why-not
    (user-error "%s" why-not))
  (unless (functionp send)
    (error "An action needs a SEND function"))
  (if input
      (apply #'approve-input-read
             :on-commit (lambda (text)
                          (approve-action--start what marks send done text
                                                 (plist-get input :target)))
             input)
    (approve-action--start what marks send done nil nil)
    nil))

(provide 'approve-actions)

;;; approve-actions.el ends here
