;;; test-approve-input.el --- Tests for input buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing text in an input buffer: what C-c C-c and C-c C-k hand back,
;; the drafts kept for a target, the header line and the windows.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-input)
(require 'approve-test-helpers "helpers")

(defvar test-input--source nil
  "The buffer inputs are opened from.")

(defun test-input--open (&rest args)
  "Open an input from `test-input--source' with ARGS; return its buffer."
  (with-current-buffer test-input--source
    (apply #'approve-input-read args)))

(defun test-input--header (buffer)
  "Return the header line of BUFFER as text.
In batch, `format-mode-line' returns nothing, so this evaluates the
header line's form."
  (with-current-buffer buffer
    (substring-no-properties (eval (cadr header-line-format) t))))

(defun test-input--draft (target)
  "Return the draft of TARGET in `test-input--source'."
  (with-current-buffer test-input--source
    (approve-input-draft target)))

(defun test-input--kill-inputs ()
  "Kill every input buffer, keeping no draft."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*approve-input" (buffer-name buffer))
      (with-current-buffer buffer
        (setq approve-input--done t))
      (kill-buffer buffer))))

(describe "Input buffers"
  (before-each
    (setq test-input--source (generate-new-buffer "*test-input source*"))
    (switch-to-buffer test-input--source)
    (delete-other-windows))
  (after-each
    (test-input--kill-inputs)
    (kill-buffer test-input--source))

  (it "open below, in markdown, named after NAME, with INITIAL and point at its end"
    (let ((buffer (test-input--open :name "comment a.el:42" :initial "Hello")))
      (expect (buffer-name buffer) :to-equal "*approve-input: comment a.el:42*")
      (with-current-buffer buffer
        (expect (derived-mode-p 'gfm-mode) :to-be-truthy)
        (expect (buffer-string) :to-equal "Hello")
        (expect (point) :to-equal (point-max)))
      (expect (window-buffer (selected-window)) :to-be buffer)
      (expect (> (window-top-line (get-buffer-window buffer))
                 (window-top-line (get-buffer-window test-input--source)))
              :to-be-truthy)))

  (it "say in the header line what the text is for, where it goes, and the keys"
    (let ((header (test-input--header
                   (test-input--open :prompt "Comment on a.el:42"
                                     :context "pending review"))))
      (expect header :to-match "\\` Comment on a\\.el:42 · pending review ")
      (expect header :to-match "C-c C-c send · C-c C-k abort · C-c C-p preview\\'"))
    (expect (test-input--header (test-input--open :prompt "Review body"
                                                  :commit-label "submit"))
            :to-match "C-c C-c submit")
    (expect (test-input--header (test-input--open :prompt "New title" :single-line t))
            :to-match "C-c C-k abort\\'"))

  (it "hand the text to ON-COMMIT in the source buffer, and close"
    (let* ((sent nil)
           (buffer (test-input--open
                    :on-commit (lambda (text) (setq sent (list text (current-buffer)))))))
      (with-current-buffer buffer
        (insert "\n\nLooks good.\n\n  ")
        (approve-input-commit))
      (expect sent :to-equal (list "Looks good." test-input--source))
      (expect (buffer-live-p buffer) :to-be nil)
      (expect (window-list) :to-equal (list (selected-window)))
      (expect (window-buffer (selected-window)) :to-be test-input--source)))

  (it "refuse to send nothing, unless ALLOW-EMPTY"
    (let ((sent 'none))
      (with-current-buffer (test-input--open :on-commit (lambda (text) (setq sent text)))
        (insert "  \n")
        (expect (approve-input-commit) :to-throw 'user-error))
      (expect sent :to-be 'none)
      (with-current-buffer (test-input--open :allow-empty t
                                             :on-commit (lambda (text) (setq sent text)))
        (approve-input-commit))
      (expect sent :to-equal "")))

  (it "keep the text sent as the draft of its target until the caller drops it"
    (with-current-buffer (test-input--open :target '(reply "PRRT_1") :on-commit #'ignore)
      (insert "Done.")
      (approve-input-commit))
    (expect (test-input--draft '(reply "PRRT_1")) :to-equal "Done.")
    (with-current-buffer test-input--source
      (approve-input-clear-draft '(reply "PRRT_1")))
    (expect (test-input--draft '(reply "PRRT_1")) :to-be nil))

  (it "keep aborted text as a draft, and start from it next time"
    (let ((aborted nil))
      (with-current-buffer (test-input--open
                            :target '(reply "PRRT_1")
                            :on-abort (lambda () (setq aborted (current-buffer))))
        (insert "Half a thought")
        (approve-input-abort))
      (expect aborted :to-be test-input--source)
      (expect (window-list) :to-equal (list (selected-window)))
      (expect (test-input--draft '(reply "PRRT_1")) :to-equal "Half a thought")
      (with-current-buffer (test-input--open :target '(reply "PRRT_1") :initial "Other")
        (expect (buffer-string) :to-equal "Half a thought"))
      (with-current-buffer (test-input--open :target '(reply "PRRT_2") :initial "Other")
        (expect (buffer-string) :to-equal "Other"))))

  (it "drop the draft when the text is blank or what it started with"
    (with-current-buffer test-input--source
      (approve-session-map-put :drafts '(edit "IC_1") "Old draft"))
    (with-current-buffer (test-input--open :target '(edit "IC_1") :initial "Body")
      (erase-buffer)
      (insert "Body")
      (approve-input-abort))
    (expect (test-input--draft '(edit "IC_1")) :to-be nil)
    (with-current-buffer (test-input--open :target '(edit "IC_1") :initial "Body")
      (erase-buffer)
      (approve-input-abort))
    (expect (test-input--draft '(edit "IC_1")) :to-be nil))

  (it "keep the text of an input buffer killed without sending it"
    (with-current-buffer (test-input--open :target '(conversation))
      (insert "Unsent")
      (kill-buffer))
    (expect (test-input--draft '(conversation)) :to-equal "Unsent"))

  (it "keep no draft without a target"
    (with-current-buffer (test-input--open)
      (insert "Text")
      (approve-input-abort))
    (with-current-buffer test-input--source
      (expect (approve-session-get :drafts) :to-be nil)))

  (it "show the input already open for a target instead of a second one"
    (let ((first (test-input--open :target '(reply "PRRT_1"))))
      (with-current-buffer first
        (insert "Typing"))
      (select-window (get-buffer-window test-input--source))
      (expect (test-input--open :target '(reply "PRRT_1")) :to-be first)
      (expect (window-buffer (selected-window)) :to-be first)
      (with-current-buffer first
        (expect (buffer-string) :to-equal "Typing"))))

  (it "send the first line of a single-line input with RET, trimmed"
    (let* ((sent nil)
           (buffer (test-input--open :single-line t :initial "  New title  "
                                     :on-commit (lambda (text) (setq sent text)))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "\nignored")
        (expect (key-binding (kbd "RET")) :to-be #'approve-input-commit)
        (approve-input-commit))
      (expect sent :to-equal "New title")))

  (it "stay open when the buffer the text was for is gone"
    (let ((buffer (test-input--open :on-commit #'ignore)))
      (kill-buffer test-input--source)
      (with-current-buffer buffer
        (insert "Precious")
        (expect (approve-input-commit) :to-throw 'user-error))
      (expect (buffer-live-p buffer) :to-be-truthy)
      (setq test-input--source (generate-new-buffer "*test-input source*")))))

(defun test-input--highlighted (buffer)
  "Return the text of the lines highlighted as a target in BUFFER."
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (string-trim (approve-render-strip-decorations
                            (buffer-substring (overlay-start overlay)
                                              (overlay-end overlay)))))
            (sort (seq-filter (lambda (overlay)
                                (eq (overlay-get overlay 'face) 'approve-target))
                              (overlays-in (point-min) (point-max)))
                  (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defconst test-input--range
  '(:kind lines :path "internal/parser/parser.go"
          :start-side "RIGHT" :start-line 13 :side "RIGHT" :line 14)
  "Two added lines of parser.go in the medium PR.")

(describe "The lines a comment is on"
  :var (pr)
  (before-all
    (approve-test-with-fixture "medium"
      (setq pr (approve-test-open-pr "medium"))
      (with-current-buffer pr
        (magit-section-show (approve-test-find-section 'approve-file-section
                                                       "internal/parser/parser.go")))))
  (after-all
    (kill-buffer pr))
  (before-each
    (switch-to-buffer pr)
    (delete-other-windows))
  (after-each
    (test-input--kill-inputs))

  (it "stay highlighted in the PR buffer, across redraws, until the input closes"
    (let ((buffer (with-current-buffer pr
                    (approve-input-read :lines test-input--range)))
          (lines '("if err != nil {" "return nil, fmt.Errorf(\"lookup 14: %w\", err)")))
      (expect (test-input--highlighted pr) :to-equal lines)
      (with-current-buffer pr
        (approve-pr-redraw))
      (expect (test-input--highlighted pr) :to-equal lines)
      (with-current-buffer buffer
        (approve-input-abort))
      (expect (test-input--highlighted pr) :to-be nil)))

  (it "suggest a change of their new side with C-c C-s, point in the block"
    (with-current-buffer (with-current-buffer pr
                           (approve-input-read :lines test-input--range
                                               :initial "Let callers override it:"))
      (expect (test-input--header (current-buffer)) :to-match "C-c C-s suggest\\'")
      (expect (key-binding (kbd "C-c C-s")) :to-be #'approve-input-insert-suggestion)
      (approve-input-insert-suggestion)
      (expect (buffer-string)
              :to-equal (concat "Let callers override it:\n```suggestion\n"
                                "\tif err != nil {\n"
                                "\t\treturn nil, fmt.Errorf(\"lookup 14: %w\", err)\n"
                                "```\n"))
      (expect (buffer-substring (line-beginning-position) (point))
              :to-equal "\t\treturn nil, fmt.Errorf(\"lookup 14: %w\", err)")
      (expect (test-input--header (current-buffer)) :not :to-match "suggest")))

  (it "suggest only the new side of a range over removed and added lines"
    (with-current-buffer (with-current-buffer pr
                           (approve-input-read
                            :suggest t
                            :lines (append '(:start-side "LEFT" :start-line 13)
                                           test-input--range)))
      (expect (buffer-string) :to-match "\\````suggestion\n\tif err != nil {\n")
      (expect (buffer-string) :not :to-match "oldStep")))

  (it "refuse to suggest without lines, or on removed lines only"
    (with-current-buffer (with-current-buffer pr (approve-input-read))
      (expect (approve-input-insert-suggestion) :to-throw 'user-error))
    (let ((removed '(:kind lines :path "internal/parser/parser.go"
                           :start-side "LEFT" :start-line 13 :side "LEFT" :line 14)))
      (with-current-buffer (with-current-buffer pr (approve-input-read :lines removed))
        (expect (test-input--header (current-buffer)) :not :to-match "suggest")
        (expect (approve-input-insert-suggestion) :to-throw 'user-error))
      (expect (with-current-buffer pr (approve-input-read :lines removed :suggest t))
              :to-throw 'user-error)))

  (it "keep no draft of a suggestion left as it was opened"
    (with-current-buffer (with-current-buffer pr
                           (approve-input-read :suggest t :lines test-input--range
                                               :target '(comment "suggest")))
      (approve-input-abort))
    (expect (with-current-buffer pr (approve-input-draft '(comment "suggest")))
            :to-be nil)))

(describe "The preview of an input"
  :var (pr sent)
  (before-all
    (approve-test-with-fixture "medium"
      (setq pr (approve-test-open-pr "medium"))))
  (after-all
    (kill-buffer pr))
  (before-each
    (switch-to-buffer pr)
    (delete-other-windows)
    (setq sent nil))
  (after-each
    (test-input--kill-inputs))

  (it "shows the text as GitHub renders it, beside the input, until it closes"
    (let ((approve-api-transport
           (lambda (request)
             (push request sent)
             (run-at-time 0 nil (plist-get request :on-success)
                          "<p>Ask <strong>@bob-example</strong></p>" nil)
             nil))
          (approve-api-timeout nil)
          (inhibit-message t)
          (input (with-current-buffer pr
                   (approve-input-read :name "comment a.el:42"))))
      (with-current-buffer input
        (insert "Ask **@bob-example**")
        (expect (key-binding (kbd "C-c C-p")) :to-be #'approve-input-preview)
        (approve-input-preview))
      (expect (plist-get (car sent) :resource) :to-equal "/markdown")
      (expect (plist-get (car sent) :reader) :to-be 'text)
      (expect (plist-get (car sent) :payload)
              :to-equal '((text . "Ask **@bob-example**") (mode . "gfm")
                          (context . "acme-example/rocket")))
      (let ((preview (approve-test-wait-for
                      (lambda () (get-buffer "*approve-preview: comment a.el:42*")))))
        (approve-test-wait-for (lambda () (get-buffer-window preview)))
        (with-current-buffer preview
          (expect (buffer-string) :to-equal "Ask @bob-example\n")
          (expect (get-text-property 5 'face) :to-equal 'bold))
        (with-current-buffer input
          (approve-input-abort))
        (expect (buffer-live-p preview) :to-be nil))))

  (it "has nothing to show for an empty text"
    (with-current-buffer (with-current-buffer pr (approve-input-read))
      (expect (approve-input-preview) :to-throw 'user-error))))

;;; test-approve-input.el ends here
