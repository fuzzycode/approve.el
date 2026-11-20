;;; test-approve-menus.el --- Tests for the dispatch menu -*- lexical-binding: t; -*-

;;; Commentary:

;; The dispatch menu (?, decision D06) in the medium PR: it lists the
;; commands that exist and reaches every command a PR buffer binds, and
;; it dims a command exactly where the command itself would refuse to
;; run (its preflight).  The menu's suffixes are read from its layout,
;; whatever transient's version.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-menus)
(require 'approve-test-helpers "helpers")

(defconst test-menus--path "internal/parser/parser.go"
  "A file of the medium PR, with threads.")

(defconst test-menus--thread "PRRT_kwDO85-qIzFQYiTYBM"
  "A published, unresolved thread on line 15 of parser.go.")

(defconst test-menus--dispatch-only '(approve-toggle-lock approve-fetch-pull-request)
  "The commands with a key in the dispatch menu alone, none in the buffer.")

(defmacro test-menus--with-medium (&rest body)
  "Run BODY in a freshly loaded medium PR buffer."
  (declare (indent 0) (debug t))
  `(approve-test-with-fixture "medium"
     (let ((buffer (approve-test-open-pr "medium")))
       (unwind-protect
           (with-current-buffer buffer
             (goto-char (point-min))
             ,@body)
         (kill-buffer buffer)))))

(defun test-menus--suffixes (prefix)
  "Return the suffixes of the transient PREFIX, as plists, in order.
Each has :key and :command, and :inapt-if when it can be dimmed.
They are found in PREFIX's layout, which transient's versions shape
differently: any list that starts with a keyword and has :command."
  (let ((found nil))
    (cl-labels ((walk (x)
                  (cond ((and (consp x) (keywordp (car x)) (plist-member x :command))
                         (push x found))
                        ((consp x) (walk (car x)) (walk (cdr x)))
                        ((vectorp x) (mapc #'walk x)))))
      (walk (get prefix 'transient--layout)))
    (nreverse found)))

(defun test-menus--commands (prefix)
  "Return the commands of the transient PREFIX."
  (mapcar (lambda (suffix) (plist-get suffix :command)) (test-menus--suffixes prefix)))

(defun test-menus--dimmed ()
  "Return the keys of the dispatch menu dimmed at point, in order."
  (delq nil (mapcar (lambda (suffix)
                      (when-let* ((inapt (plist-get suffix :inapt-if)))
                        (and (funcall inapt) (plist-get suffix :key))))
                    (test-menus--suffixes 'approve-dispatch))))

(defun test-menus--goto-section (type value)
  "Move point to the heading of the section of TYPE with VALUE.
The diff of `test-menus--path' is expanded first, for its threads."
  (approve-nav-goto-file test-menus--path)
  (goto-char (oref (or (approve-test-find-section type value)
                       (error "No section %s %s" type value))
                   start)))

(defun test-menus--goto-line (number)
  "Move point to the added or context line NUMBER of `test-menus--path'."
  (approve-nav-goto-file test-menus--path)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((line (get-text-property (point) 'approve-line)))
        (if (and line (equal (plist-get line :path) test-menus--path)
                 (equal (plist-get line :side) "RIGHT") (eql (plist-get line :new) number))
            (setq found (point))
          (forward-line 1))))
    (or found (error "No line %s of %s" number test-menus--path))))

(describe "The dispatch menu"
  (it "opens on ?, in Emacs and in evil's normal and visual states"
    (expect (keymap-lookup approve-pr-mode-map "?") :to-be 'approve-dispatch)
    (expect (get 'approve-dispatch 'transient--prefix) :to-be-truthy)
    (let ((bound nil))
      (cl-letf (((symbol-function 'evil-define-key*)
                 (lambda (state _map key def) (push (list state (key-description key) def) bound)))
                ((symbol-function 'evil-set-initial-state) #'ignore)
                ((symbol-function 'evil-add-command-properties) #'ignore))
        (load "approve-evil" nil t))
      (expect (member '(normal "?" approve-dispatch) bound) :to-be-truthy)
      (expect (member '(visual "?" approve-dispatch) bound) :to-be-truthy)))

  (it "lists only commands that exist, with the keys of the PR buffer"
    (dolist (suffix (test-menus--suffixes 'approve-dispatch))
      (let ((command (plist-get suffix :command)))
        (expect (commandp command) :to-be-truthy)
        (expect (keymap-lookup approve-pr-mode-map (plist-get suffix :key))
                :to-be (unless (memq command test-menus--dispatch-only) command)))))

  (it "reaches every command a PR buffer binds, itself or through j and S"
    (let ((reachable (append '(approve-dispatch)
                             (test-menus--commands 'approve-dispatch)
                             (test-menus--commands 'approve-jump)
                             (test-menus--commands 'approve-submit-review)))
          (bound nil))
      (cl-labels ((collect (map)
                    (map-keymap (lambda (event binding)
                                  (cond ((eq event 'remap))
                                        ((keymapp binding) (collect binding))
                                        ((and (symbolp binding)
                                              (string-prefix-p "approve-" (symbol-name binding)))
                                         (push binding bound))))
                                map)))
        (collect approve-pr-mode-map))
      (expect bound :to-contain 'approve-resolve)
      (expect (seq-difference (delete-dups bound) reachable) :to-equal nil)))

  (it "dims a command where the command would refuse to run"
    (test-menus--with-medium
      ;; On a file heading, as in the mockup: the file's commands and
      ;; the PR's apply, those of threads and comments don't.
      (test-menus--goto-section 'approve-file-section test-menus--path)
      (expect (test-menus--dimmed) :to-equal '("s" "x" "b" "r" "R" "e" "k" "+" "H"))
      (expect (approve-action-why-not 'approve-resolve) :to-equal "Put point on a thread")
      ;; On a thread: replying and resolving apply.
      (test-menus--goto-section 'approve-thread-section test-menus--thread)
      (expect (test-menus--dimmed) :to-equal '("s" "x" "b" "e" "k" "+" "H"))
      ;; On the description, which can be edited.
      (goto-char (oref (approve-test-find-section 'approve-description-section) start))
      (expect (test-menus--dimmed) :to-equal '("s" "x" "b" "v" "r" "R" "k" "H" "F" "f"))))

  (it "follows a selection of diff lines, for the command picked in it"
    (test-menus--with-medium
      (let ((transient-mark-mode t))
        (test-menus--goto-line 13)
        (push-mark (point) t t)
        (test-menus--goto-line 14)
        (end-of-line)
        (expect (plist-get (approve-context-at-point) :kind) :to-be 'lines)
        (expect (test-menus--dimmed) :to-equal '("x" "b" "r" "R" "e" "k" "+" "H"))
        (expect (approve-commands--suggest-preflight)
                :to-equal (approve-context-at-point)))))

  (it "dims what the viewer may not do, and what there is nothing for"
    (test-menus--with-medium
      (test-menus--goto-section 'approve-thread-section test-menus--thread)
      (approve-model-patch `((__typename . "PullRequestReviewThread") (id . ,test-menus--thread)
                             (viewerCanResolve) (viewerCanReply)))
      (approve-model-edit-list (approve-model-root-ref) 'pendingReviews #'ignore)
      (expect (test-menus--dimmed) :to-equal '("s" "x" "b" "X" "r" "R" "e" "k" "+" "H"))
      (expect (approve-action-why-not 'approve-resolve) :to-equal "You can't resolve this thread")
      (expect (approve-action-why-not 'approve-discard-review)
              :to-equal "You have no pending review")))

  (it "dims every command that changes the PR until it has loaded"
    (approve-test-with-fixture "medium"
      (let ((buffer (approve-pr-setup-buffer (approve-test-pr-plist "medium"))))
        (unwind-protect
            (with-current-buffer buffer
              (expect (test-menus--dimmed)
                      :to-equal '("c" "s" "x" "b" "v" "S" "X" "r" "R" "e" "k" "+" "H" "C" "t" "d" "l" "a" "A" "M" "D" "w" "#" "m" "F" "f"))
              (expect (approve-action-why-not 'approve-comment)
                      :to-equal "Wait until the pull request has loaded"))
          (kill-buffer buffer)))))

  (it "names the pull request, and says what T will do"
    (test-menus--with-medium
      (expect (substring-no-properties (approve-menus--dispatch-heading))
              :to-equal "Approve  #202 Rewrite the parser and engine scheduler")
      (expect (approve-menus--threads-description) :to-equal "Fold all threads")
      (approve-session-put :threads 'folded)
      (expect (approve-menus--threads-description) :to-equal "Unfold all threads"))))

;;; Evil's keys

(defvar evil-local-mode)
(defvar evil-state)

(defconst test-menus--evil-menus
  '((approve-dispatch . approve-evil-pr-bindings)
    (approve-file-dispatch . approve-evil-file-bindings)
    (approve-dashboard-dispatch . approve-evil-dashboard-bindings))
  "The dispatch menus, each with its buffer's keys in evil's normal state.")

(defun test-menus--load-evil ()
  "Load approve-evil.el without evil, its bindings going nowhere."
  (cl-letf (((symbol-function 'evil-define-key*) #'ignore)
            ((symbol-function 'evil-set-initial-state) #'ignore)
            ((symbol-function 'evil-add-command-properties) #'ignore))
    (load "approve-evil" nil t)))

(defmacro test-menus--in-evil-state (state &rest body)
  "Run BODY in a buffer in evil's STATE, then give the menus their own keys."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (with-temp-buffer
         (setq-local evil-local-mode t)
         (setq-local evil-state ,state)
         ,@body)
     (with-temp-buffer
       (dolist (menu test-menus--evil-menus)
         (approve-menus--set-keys (car menu))))))

(defun test-menus--key (prefix command)
  "Return the key of COMMAND in the transient PREFIX, as `kbd' reads it."
  (key-description (kbd (plist-get (seq-find (lambda (suffix)
                                               (eq (plist-get suffix :command) command))
                                             (test-menus--suffixes prefix))
                                   :key))))

(describe "The dispatch menus in evil's states"
  (before-all (test-menus--load-evil))

  (it "show normal state's keys, so a key runs the same command in the buffer"
    (test-menus--in-evil-state 'normal
      (dolist (menu test-menus--evil-menus)
        (approve-menus--set-keys (car menu))
        (let* ((suffixes (test-menus--suffixes (car menu)))
               (keys (mapcar (lambda (suffix) (key-description (kbd (plist-get suffix :key))))
                             suffixes)))
          ;; One command per key, and no key starts another.
          (expect (length (delete-dups (copy-sequence keys))) :to-equal (length keys))
          (dolist (key keys)
            (expect (seq-find (lambda (other) (string-prefix-p (concat key " ") other)) keys)
                    :to-be nil))
          ;; In the buffer, the key runs the command, or none of
          ;; Approve's: the menu alone reaches it.
          (dolist (suffix suffixes)
            (let ((bound (cdr (seq-find (lambda (binding)
                                          (equal (key-description (kbd (car binding)))
                                                 (key-description (kbd (plist-get suffix :key)))))
                                        (symbol-value (cdr menu))))))
              (expect (cons (plist-get suffix :key) (or bound (plist-get suffix :command)))
                      :to-equal (cons (plist-get suffix :key) (plist-get suffix :command)))))))))

  (it "give the PR buffer's dispatch the keys of its table, and new ones where they clash"
    (test-menus--in-evil-state 'normal
      (approve-menus--set-keys 'approve-dispatch)
      (expect (mapcar (lambda (command) (test-menus--key 'approve-dispatch command))
                      '(approve-toggle-viewed approve-commit-suggestion approve-batch-suggestion
                        approve-edit approve-delete approve-browse approve-fold-others
                        approve-pr-refresh approve-jump approve-visit-file approve-merge
                        approve-set-assignees approve-request-reviewers approve-toggle-draft
                        approve-comment approve-toggle-lock))
              :to-equal '("x" "a" "A" "i" "D" "o" "O" "g r" "J" "g f" "g m"
                          "@" "W" "P" "c" "#"))
      (approve-menus--set-keys 'approve-file-dispatch)
      (expect (mapcar (lambda (command) (test-menus--key 'approve-file-dispatch command))
                      '(approve-file-next-change approve-file-previous-change
                        approve-toggle-viewed approve-file-toggle-version approve-browse))
              :to-equal '("] c" "[ c" "x" "g b" "o"))))

  (it "open with the keys of the buffer's state, and their own outside evil's"
    (spy-on 'transient-setup)
    (test-menus--in-evil-state 'visual
      (call-interactively #'approve-dispatch)
      (expect 'transient-setup :to-have-been-called-with 'approve-dispatch)
      (expect (test-menus--key 'approve-dispatch 'approve-toggle-viewed) :to-equal "x")
      (setq-local evil-state 'emacs)
      (call-interactively #'approve-dispatch)
      (expect (test-menus--key 'approve-dispatch 'approve-toggle-viewed) :to-equal "v")
      (setq-local evil-state 'normal)
      (call-interactively #'approve-dashboard-dispatch)
      (expect (test-menus--key 'approve-dashboard-dispatch 'approve-dashboard-refresh)
              :to-equal "g r"))
    ;; Without evil, the keys the menus define.
    (expect (mapcar (lambda (command) (test-menus--key 'approve-dispatch command))
                    '(approve-toggle-viewed approve-commit-suggestion approve-set-assignees
                      approve-pr-refresh))
            :to-equal '("v" "x" "a" "g"))
    (expect (test-menus--key 'approve-dashboard-dispatch 'approve-dashboard-refresh)
            :to-equal "g")))

;;; test-approve-menus.el ends here
