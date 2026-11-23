;;; test-approve-diff.el --- Tests for the diff -*- lexical-binding: t; -*-

;;; Commentary:

;; Parsing patches, placing threads, and drawing files, hunks, lines
;; and threads: lazily, with line numbers that copying leaves out,
;; changed words, and the binary and too-large cases.

;;; Code:

(require 'buttercup)
(require 'approve)
(require 'approve-test-helpers "helpers")

(defface test-diff--red-line
  '((t :background "#ffeeee" :foreground "red"))
  "A face of a line with a background and a colour of its own, as a removed line has."
  :group 'approve)

(describe "approve-diff-parse"
  (it "reads hunks with old and new line numbers"
    (let ((hunks (approve-diff-parse
                  (concat "@@ -10,4 +10,5 @@ func f() {\n"
                          " a\n-b\n+c\n+d\n e\n"
                          "@@ -40 +41,2 @@\n x\n+y\n\\ No newline at end of file"))))
      (expect (length hunks) :to-equal 2)
      (expect (approve-diff-hunk-header (car hunks)) :to-equal "@@ -10,4 +10,5 @@ func f() {")
      (expect (approve-diff-hunk-lines (car hunks))
              :to-equal '((ctx 10 10 "a") (del 11 nil "b") (add nil 11 "c")
                          (add nil 12 "d") (ctx 12 13 "e")))
      (expect (approve-diff-hunk-lines (cadr hunks))
              :to-equal '((ctx 40 41 "x") (add nil 42 "y")
                          (nonl nil nil "\\ No newline at end of file")))))

  (it "keeps empty context lines and drops the patch's last newline"
    (expect (approve-diff-hunk-lines (car (approve-diff-parse "@@ -1,2 +1,2 @@\n \n-a\n+b\n")))
            :to-equal '((ctx 1 1 "") (del 2 nil "a") (add nil 2 "b"))))

  (it "returns nil for no patch, and caches what it parsed"
    (expect (approve-diff-parse nil) :to-be nil)
    (expect (approve-diff-parse "") :to-be nil)
    (let ((patch (concat "@@ -1 +1 @@\n" "-a\n+b")))
      (expect (approve-diff-parse patch) :to-be (approve-diff-parse patch)))))

(describe "approve-diff--moved"
  (it "writes the directories both paths share once, as git does"
    (expect (approve-diff--moved "internal/engine/queue.go" "internal/engine/scheduler.go")
            :to-equal "internal/engine/{queue.go → scheduler.go}")
    (expect (approve-diff--moved "a/x.go" "b/x.go") :to-equal "a/x.go → b/x.go")
    (expect (approve-diff--moved "src/aa.go" "src/ab/c.go") :to-equal "src/{aa.go → ab/c.go}")))

(describe "Syntax highlighting in hunks"
  (it "colours both sides of a hunk in the file's major mode, once per patch"
    (let* ((approve-diff-fontify t)
           (patch "@@ -1,2 +1,2 @@\n (defun f ()\n-  \"old\")\n+  \"new\")")
           (hunks (approve-diff-parse patch))
           (table (approve-diff--fontify "lisp/x.el" patch hunks))
           (lines (approve-diff-hunk-lines (car hunks))))
      (expect (ensure-list (get-text-property 1 'face (gethash (nth 0 lines) table)))
              :to-contain 'font-lock-keyword-face)
      (expect (ensure-list (get-text-property 3 'face (gethash (nth 1 lines) table)))
              :to-contain 'font-lock-string-face)
      (expect (ensure-list (get-text-property 3 'face (gethash (nth 2 lines) table)))
              :to-contain 'font-lock-doc-face)
      (expect (approve-diff--fontify "lisp/x.el" patch hunks) :to-be table)))

  (it "keeps the diff's background under the syntax colours"
    (let* ((approve-diff-fontify t)
           (patch "@@ -1 +1 @@\n-(defun f ())\n+(defun g ())")
           (hunks (approve-diff-parse patch))
           (table (approve-diff--fontify "lisp/x.el" patch hunks))
           (line (nth 1 (approve-diff-hunk-lines (car hunks))))
           (text (approve-diff--line-text "lisp/x.el" line 3 nil (gethash line table)))
           (faces (ensure-list (get-text-property (string-search "defun" text) 'face text))))
      (expect faces :to-contain 'font-lock-keyword-face)
      (expect faces :to-contain 'approve-diff-added)))

  (it "does nothing when turned off or for files without a mode"
    (let ((hunks (approve-diff-parse "@@ -1 +1 @@\n-a\n+b")))
      (let ((approve-diff-fontify nil))
        (expect (approve-diff--fontify "x.el" "p" hunks) :to-be nil))
      (let ((approve-diff-fontify t))
        (expect (approve-diff--fontify "no-mode-for-this.zzz9" "p" hunks) :to-be nil)))))

(describe "approve-diff-thread-index"
  (it "puts threads under their line, on the file, or with the outdated"
    (let* ((lines (make-hash-table :test #'equal))
           (_ (progn (puthash '("RIGHT" . 15) t lines) (puthash '("LEFT" . 52) t lines)))
           (threads '(((id . "a") (subjectType . "LINE") (diffSide . "RIGHT") (line . 15))
                      ((id . "b") (subjectType . "LINE") (diffSide . "LEFT") (line . 52))
                      ((id . "c") (subjectType . "FILE") (diffSide . "RIGHT"))
                      ((id . "d") (subjectType . "LINE") (diffSide . "RIGHT") (line))
                      ((id . "e") (subjectType . "LINE") (diffSide . "RIGHT") (line . 99))
                      ((id . "f") (subjectType . "LINE") (diffSide . "RIGHT") (line . 15))))
           (index (approve-diff-thread-index threads lines))
           (ids (lambda (threads) (mapcar (lambda (thread) (alist-get 'id thread)) threads))))
      (expect (funcall ids (gethash '("RIGHT" . 15) (plist-get index :lines)))
              :to-equal '("a" "f"))
      (expect (funcall ids (gethash '("LEFT" . 52) (plist-get index :lines))) :to-equal '("b"))
      (expect (funcall ids (plist-get index :file)) :to-equal '("c"))
      (expect (funcall ids (plist-get index :outdated)) :to-equal '("d" "e")))))

(defvar test-diff--buffer nil
  "The loaded medium PR buffer.")

(defun test-diff--file-section (path)
  "Return the diff section of PATH in the medium buffer."
  (approve-test-find-section 'approve-file-section path))

(defun test-diff--show (path)
  "Expand the diff of PATH and return its section."
  (let ((section (test-diff--file-section path)))
    (magit-section-show section)
    section))

(defun test-diff--line-at (text)
  "Move to the line holding TEXT in the buffer and return its `approve-line'."
  (goto-char (point-min))
  (search-forward text)
  (get-text-property (point) 'approve-line))

(describe "The diff of the medium PR"
  (before-all
    (setq approve-use-nerd-icons nil)
    (approve-icon-clear-cache)
    (approve-test-with-fixture "medium"
      (setq test-diff--buffer (approve-test-open-pr "medium"))))
  (after-all
    (kill-buffer test-diff--buffer)
    (setq approve-use-nerd-icons t)
    (approve-icon-clear-cache))

  (it "has a collapsed section per file, with its body not drawn yet"
    (with-current-buffer test-diff--buffer
      (let ((diff (approve-test-find-section 'approve-diff-section)))
        (expect (length (oref diff children)) :to-equal 30)
        (dolist (file (oref diff children))
          (expect (oref file hidden) :to-be t))
        (expect (oref (test-diff--file-section "internal/parser/parser.go") children)
                :to-be nil))))

  (it "heads each file with its viewed mark, its change and its path"
    (with-current-buffer test-diff--buffer
      (expect (approve-test-section-text (test-diff--file-section "internal/engine/scheduler.go"))
              :to-match "\\`  · → internal/engine/{queue\\.go → scheduler\\.go} +2 new  💬 1\n")
      (expect (approve-test-section-text (test-diff--file-section "internal/engine/engine.go"))
              :to-match "\\`  · ~ internal/engine/engine\\.go +6 new  💬 2\n")
      (expect (approve-test-section-text (test-diff--file-section "internal/engine/legacy.go"))
              :to-match "\\`  ✓ − internal/engine/legacy\\.go +💬 1\n")
      (expect (approve-test-section-text (test-diff--file-section "CHANGELOG.md"))
              :to-match "\\`  ↻ ~ CHANGELOG\\.md\n")))

  (it "counts the threads on lines at the right edge, lined up"
    (with-current-buffer test-diff--buffer
      ;; parser.go has four threads on lines and one on the file, which
      ;; the file list counts; six comments in them came after the
      ;; viewer's last review.
      (expect (approve-test-section-text (test-diff--file-section "internal/parser/parser.go"))
              :to-match "\\`  · ~ internal/parser/parser\\.go +6 new  💬 4\n")
      (let ((ends nil) (columns nil))
        (dolist (section (oref (approve-test-find-section 'approve-diff-section) children))
          (save-excursion
            (goto-char (oref section start))
            (when-let* ((eol (line-end-position))
                        (match (text-property-search-forward
                                'display nil (lambda (_ value) (eq (car-safe value) 'space))
                                t))
                        ((< (prop-match-beginning match) eol))
                        (column (plist-get (cdr (prop-match-value match)) :align-to)))
              ;; The counts are padded to one width, so they line up;
              ;; "N new" goes before them.
              (unless (string-match-p "new" (buffer-substring (prop-match-end match) eol))
                (push column columns))
              (push (+ column (string-width (buffer-substring (prop-match-end match) eol)))
                    ends))))
        ;; Eight files have threads on lines; one more has new comments
        ;; only on the file.
        (expect (length ends) :to-equal 9)
        (expect (length columns) :to-equal 2)
        (expect (seq-uniq columns) :to-have-same-items-as (list (car columns)))
        (expect (seq-uniq ends) :to-equal (list approve-page-width)))))

  (it "says in its heading how many files are viewed, threads unresolved and comments new"
    (with-current-buffer test-diff--buffer
      (expect (car (split-string (approve-test-section-text
                                  (approve-test-find-section 'approve-diff-section))
                                 "\n"))
              :to-match (concat "\\`Diff (30) +8/30 viewed · 13 unresolved threads"
                                " · 20 new comments\\'"))))

  (it "draws the change as an icon in its face"
    (dolist (case '(("ADDED" "+" approve-file-added)
                    ("MODIFIED" "~" approve-file-modified)
                    ("DELETED" "−" approve-file-deleted)
                    ("RENAMED" "→" approve-file-moved)
                    ("COPIED" "⧉" approve-file-moved)
                    ("CHANGED" "⚙" approve-file-modified)))
      (let ((icon (approve-diff--change-icon (car case))))
        (expect (substring-no-properties icon) :to-equal (nth 1 case))
        (expect (memq (nth 2 case) (ensure-list (get-text-property 0 'face icon)))
                :to-be-truthy))))

  (describe "an expanded file"
    (before-all
      (with-current-buffer test-diff--buffer
        (test-diff--show "internal/parser/parser.go")))

    (it "has its hunks, file-level threads and outdated threads"
      (with-current-buffer test-diff--buffer
        (let ((children (oref (test-diff--file-section "internal/parser/parser.go") children)))
          (expect (mapcar (lambda (s) (oref s type)) children)
                  :to-equal '(approve-file-threads-section
                              approve-hunk-section approve-hunk-section
                              approve-hunk-section approve-hunk-section
                              approve-file-threads-section))
          (expect (oref (car children) value) :to-equal 'file)
          (expect (oref (car (last children)) value) :to-equal 'outdated)
          (expect (oref (car (last children)) hidden) :to-be t)
          (expect (oref (cadr children) value)
                  :to-equal '("internal/parser/parser.go"
                              . "@@ -10,8 +10,12 @@ func step10(ctx context.Context) error {")))))

    (it "draws lines with numbers and markers that copying leaves out"
      (with-current-buffer test-diff--buffer
        (test-diff--line-at "if err != nil {")
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (expect (substring-no-properties line) :to-equal "         13 +\tif err != nil {")
          (expect (filter-buffer-substring (line-beginning-position) (line-end-position))
                  :to-equal "\tif err != nil {")
          (expect (get-text-property (line-beginning-position) 'approve-line)
                  :to-equal (get-text-property (line-end-position) 'approve-line)))))

    (it "tells every line where it is"
      (with-current-buffer test-diff--buffer
        (expect (test-diff--line-at "if err != nil {")
                :to-equal '(:path "internal/parser/parser.go" :side "RIGHT"
                            :old nil :new 13 :kind add))
        (expect (test-diff--line-at "return nil, err")
                :to-equal '(:path "internal/parser/parser.go" :side "LEFT"
                            :old 14 :new nil :kind del))
        (expect (test-diff--line-at "count += 11")
                :to-equal '(:path "internal/parser/parser.go" :side "RIGHT"
                            :old 11 :new 11 :kind ctx))))

    (it "puts each thread under its line, the line becoming a section"
      (with-current-buffer test-diff--buffer
        (let ((lines nil))
          (magit-map-sections (lambda (section)
                                (when (eq (oref section type) 'approve-line-section)
                                  (push (oref section value) lines))))
          (expect (nreverse lines)
                  :to-equal '(("internal/parser/parser.go" "RIGHT" 15)
                              ("internal/parser/parser.go" "LEFT" 52)
                              ("internal/parser/parser.go" "RIGHT" 131)
                              ("internal/parser/parser.go" "RIGHT" 134))))
        (let ((section (approve-test-find-section
                        'approve-line-section '("internal/parser/parser.go" "RIGHT" 15))))
          (expect (approve-test-section-text section)
                  :to-match "\\`.*\t}\n +╭─ @carol-example")
          (expect (oref (car (oref section children)) type) :to-be 'approve-thread-section))))

    (it "marks a line with threads with a bubble at the right edge"
      (with-current-buffer test-diff--buffer
        (let* ((line (approve-test-find-section
                      'approve-line-section '("internal/parser/parser.go" "RIGHT" 15)))
               (eol (save-excursion (goto-char (oref line start)) (line-end-position)))
               (overlays (seq-filter (lambda (overlay) (overlay-get overlay 'approve-bubble))
                                     (overlays-in eol (1+ eol))))
               (bubble (overlay-get (car overlays) 'before-string))
               (stretch (get-text-property 1 'display bubble)))
          (expect (length overlays) :to-be 1)
          (expect (substring-no-properties bubble 2) :to-equal "💬 1")
          (expect (+ (plist-get (cdr stretch) :align-to) (string-width (substring bubble 2)))
                  :to-equal approve-page-width)
          ;; It has the line's background only (`approve-face-background',
          ;; which batch Emacs's faces don't have), and a colour of its own.
          (expect (get-text-property eol 'face) :not :to-be nil)
          (expect (get-text-property 0 'face bubble)
                  :to-equal (if-let* ((background (approve-face-background
                                                   (get-text-property eol 'face))))
                                (list background 'approve-diff-bubble)
                              'approve-diff-bubble))
          ;; It is no text: moving up and down never stops in it, and
          ;; copying leaves it out.  The cursor at the end of the line
          ;; is drawn where it starts.
          (expect (buffer-substring-no-properties (oref line start) (1+ eol))
                  :not :to-match "💬")
          (expect (filter-buffer-substring (oref line start) eol) :to-equal "\t}")
          (expect (get-text-property 0 'cursor bubble) :to-be t))))

    (it "puts a bubble one column short of the window's edge by default"
      (let* ((approve-page-width nil)
             (bubble (approve-diff-thread-bubble
                      (list '((id . "T1") (isResolved))) nil))
             (count (substring bubble 2)))
        ;; It follows the window as it is resized, so a line as long
        ;; as the window allows leaves it room (decision D55).
        (expect (get-text-property 1 'display bubble)
                :to-equal `(space :align-to (- right ,(1+ (string-width count)))))))

    (it "gives a bubble a row of its own when the line leaves it no room on the page"
      (with-temp-buffer
        (let* ((approve-page-width 110)
               (threads (list '((id . "T1") (isResolved))))
               (short (approve-diff-thread-bubble threads 'approve-diff-added nil 100))
               (long (approve-diff-thread-bubble threads 'approve-diff-added nil 106)))
          (expect short :not :to-match "\n")
          (expect (substring-no-properties long 0 2) :to-equal " \n")
          ;; The row has the line's face, so its background reaches the
          ;; window's edge, and the bubble ends it as on a line.
          (expect (get-text-property 1 'face long) :to-be 'approve-diff-added)
          (expect (equal-including-properties (substring long 2) (substring short 1))
                  :to-be t)
          ;; The cursor at the end of the line stays after its text.
          (expect (get-text-property 0 'cursor long) :to-be t))))

    (it "gives a bubble the line's background, not its colour"
      ;; An overlay string takes the colour of the text under it, so the
      ;; bubble's own face gives it one.
      (let* ((threads (list '((isResolved))))
             (bubble (approve-diff-thread-bubble threads 'test-diff--red-line))
             (resolved (approve-diff-thread-bubble (list '((isResolved . t)))
                                             'test-diff--red-line)))
        (expect (get-text-property 0 'face bubble)
                :to-equal '((:background "#ffeeee") approve-diff-bubble))
        (expect (get-text-property (1- (length bubble)) 'face bubble)
                :to-equal '((:background "#ffeeee") approve-diff-bubble))
        (expect (get-text-property (1- (length resolved)) 'face resolved)
                :to-equal '(approve-dim (:background "#ffeeee") approve-diff-bubble))
        (expect (get-text-property 0 'face (approve-diff-thread-bubble threads 'approve-dim))
                :to-be 'approve-diff-bubble)))

    (it "makes a bubble red when one of its threads asks for changes (D34)"
      (let* ((threads (list '((id . "a") (isResolved)) '((id . "b") (isResolved))))
             (bubble (approve-diff-thread-bubble threads 'test-diff--red-line '("b"))))
        (expect (get-text-property (1- (length bubble)) 'face bubble)
                :to-equal '(approve-thread-mark-changes-requested (:background "#ffeeee")
                                                                  approve-diff-bubble))
        (expect (get-text-property 0 'face bubble)
                :to-equal '((:background "#ffeeee") approve-diff-bubble))
        (setq bubble (approve-diff-thread-bubble threads 'test-diff--red-line '("c")))
        (expect (get-text-property (1- (length bubble)) 'face bubble)
                :to-equal '((:background "#ffeeee") approve-diff-bubble))))

    (it "indexes the threads that ask for changes"
      ;; In the medium PR, carol's standing request has one thread: on
      ;; parser.go as a whole.
      (with-current-buffer test-diff--buffer
        (let ((index (approve-diff-thread-index
                      (approve-pr-threads-for-file "internal/parser/parser.go")
                      (make-hash-table :test #'equal))))
          (expect (length (plist-get index :file)) :to-equal 1)
          (expect (plist-get index :asking)
                  :to-equal (list (alist-get 'id (car (plist-get index :file))))))))

    (it "starts lines folded whose threads are all resolved"
      (with-current-buffer test-diff--buffer
        (let ((folded 0) (open 0))
          (magit-map-sections
           (lambda (line)
             (when (eq (oref line type) 'approve-line-section)
               (let ((settled (seq-every-p
                               (lambda (thread)
                                 (approve-model-get "PullRequestReviewThread"
                                                    (oref thread value) 'isResolved))
                               (oref line children))))
                 (expect (oref line hidden) :to-be settled)
                 (if settled (cl-incf folded) (cl-incf open))))))
          (expect folded :to-be-greater-than 0)
          (expect open :to-be-greater-than 0))))

    (it "folds a line's threads away, from the line or from inside them"
      (with-current-buffer test-diff--buffer
        (let* ((line (approve-test-find-section
                      'approve-line-section '("internal/parser/parser.go" "RIGHT" 15)))
               (thread (car (oref line children)))
               (comment (car (oref thread children))))
          (expect (oref comment type) :to-be 'approve-comment-section)
          ;; From a comment: the threads fold, the line and its bubble stay.
          (goto-char (1+ (oref comment start)))
          (approve-section-toggle)
          (expect (oref line hidden) :to-be t)
          (expect (invisible-p (oref thread start)) :to-be-truthy)
          (expect (invisible-p (oref line start)) :to-be nil)
          (expect (point) :to-equal (marker-position (oref line start)))
          ;; From the line: they come back.
          (approve-section-toggle)
          (expect (oref line hidden) :to-be nil)
          (expect (invisible-p (oref thread start)) :to-be nil))))

    (it "folds the hunk from a line without threads"
      (with-current-buffer test-diff--buffer
        (let* ((line (approve-test-find-section
                      'approve-line-section '("internal/parser/parser.go" "RIGHT" 15)))
               (hunk (oref line parent)))
          (goto-char (oref line start))
          (forward-line -1)
          (expect (oref (magit-current-section) type) :to-be 'approve-hunk-section)
          (approve-section-toggle)
          (expect (oref hunk hidden) :to-be t)
          (approve-section-toggle)
          (expect (oref hunk hidden) :to-be nil))))

    (it "binds the keys that toggle sections to the toggle that knows lines"
      (with-current-buffer test-diff--buffer
        (expect (key-binding (kbd "TAB")) :to-be 'approve-section-toggle)))

    (it "indents threads under the code, and copying leaves the indentation out"
      (with-current-buffer test-diff--buffer
        (goto-char (point-min))
        (search-forward "This allocates on every call")
        (goto-char (match-beginning 0))
        (let ((indentation (concat "    " (make-string 9 ?\s) "│ ")))
          (expect (buffer-substring-no-properties (line-beginning-position) (point))
                  :to-equal indentation)
          (expect (filter-buffer-substring (line-beginning-position) (point))
                  :to-equal "")
          (expect (oref (magit-section-at (line-beginning-position)) type)
                  :to-be 'approve-comment-section))))

    (it "marks the words that changed between removed and added lines"
      (with-current-buffer test-diff--buffer
        (test-diff--line-at "if err := oldStep")
        (goto-char (line-beginning-position))
        (search-forward "oldStep")
        (expect (ensure-list (get-text-property (1- (point)) 'face))
                :to-contain 'approve-diff-refine-removed))))

  (it "says a binary file is not shown"
    (with-current-buffer test-diff--buffer
      (expect (approve-test-section-text (test-diff--show "assets/logo.png"))
              :to-match "Binary file not shown.")
      (expect (approve-test-section-text (test-diff--file-section "assets/logo.png"))
              :to-match "\\`  · \\+ assets/logo\\.png\n")))

  (it "omits line numbers when asked"
    (with-current-buffer test-diff--buffer
      (let ((approve-diff-line-numbers nil))
        (approve-pr-redraw)
        (test-diff--line-at "if err != nil {")
        (expect (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                :to-equal "    +\tif err != nil {"))
      (approve-pr-redraw))))

(defconst test-diff--range
  '(:kind lines :path "internal/parser/parser.go"
          :start-side "LEFT" :start-line 13 :side "RIGHT" :line 16)
  "Two removed and four added lines of parser.go; a thread is on the 15th.")

(defun test-diff--folds (path)
  "Return whether each section folding threads in the diff of PATH is hidden.
The value is a list of (TYPE . HIDDEN), in buffer order."
  (let ((folds nil))
    (magit-map-sections
     (lambda (section)
       (when (memq (oref section type) '(approve-line-section approve-file-threads-section))
         (push (cons (oref section type) (and (oref section hidden) t)) folds)))
     (approve-test-find-section 'approve-file-section path))
    (nreverse folds)))

(describe "Folding every thread in the diff"
  :var (buffer)
  (before-each
    (approve-test-with-fixture "medium"
      (setq buffer (approve-test-open-pr "medium")))
    (with-current-buffer buffer
      (magit-section-show (approve-test-find-section 'approve-file-section
                                                     "internal/parser/parser.go"))))
  (after-each
    (kill-buffer buffer))

  (it "folds every line's threads and the file's groups; T again unfolds them all"
    (with-current-buffer buffer
      (let ((inhibit-message t))
        (expect (key-binding (kbd "T")) :to-be #'approve-toggle-threads)
        (expect (mapcar #'cdr (test-diff--folds "internal/parser/parser.go"))
                :to-contain nil)
        (approve-toggle-threads)
        (expect (seq-uniq (mapcar #'cdr (test-diff--folds "internal/parser/parser.go")))
                :to-equal '(t))
        ;; Resolved and outdated threads, folded at first, unfold too.
        (approve-toggle-threads)
        (expect (seq-uniq (mapcar #'cdr (test-diff--folds "internal/parser/parser.go")))
                :to-equal '(nil)))))

  (it "holds for files expanded later, and across redraws"
    (with-current-buffer buffer
      (let ((inhibit-message t))
        (approve-fold-threads)
        (magit-section-show (approve-test-find-section 'approve-file-section
                                                       "internal/engine/engine.go"))
        (expect (seq-uniq (mapcar #'cdr (test-diff--folds "internal/engine/engine.go")))
                :to-equal '(t))
        (approve-pr-redraw)
        (expect (seq-uniq (mapcar #'cdr (test-diff--folds "internal/parser/parser.go")))
                :to-equal '(t))
        (approve-unfold-threads)
        (magit-section-show (approve-test-find-section 'approve-file-section
                                                       "pkg/api/client.go"))
        (expect (seq-uniq (mapcar #'cdr (test-diff--folds "pkg/api/client.go")))
                :to-equal '(nil)))))

  (it "moves point out of a thread it folds, onto the thread's line"
    (with-current-buffer buffer
      (let ((inhibit-message t))
        (goto-char (point-min))
        (search-forward "Done, it is reused now.")
        (approve-fold-threads)
        (expect (get-text-property (point) 'approve-line) :not :to-be nil)
        (expect (oref (magit-current-section) type) :to-be 'approve-line-section)))))

(describe "Ranges of lines in the medium PR"
  :var (buffer)
  (before-all
    (approve-test-with-fixture "medium"
      (setq buffer (approve-test-open-pr "medium"))))
  (after-all
    (kill-buffer buffer))

  (it "come from the patch, from the first end to the last"
    (with-current-buffer buffer
      (expect (approve-diff-range-lines test-diff--range)
              :to-equal '((del 13 nil "\tif err := oldStep(ctx, 13); err != nil {")
                          (del 14 nil "\t\treturn nil, err")
                          (add nil 13 "\tif err != nil {")
                          (add nil 14 "\t\treturn nil, fmt.Errorf(\"lookup 14: %w\", err)")
                          (add nil 15 "\t}")
                          (add nil 16 "\tmetrics.Inc(\"step_16\")")))
      (expect (approve-diff-range-lines '(:path "internal/parser/parser.go"
                                                :side "LEFT" :line 11))
              :to-equal '((ctx 11 11 "\tcount += 11")))))

  (it "are nil when they are not in one hunk, or run backwards"
    (with-current-buffer buffer
      (expect (approve-diff-range-lines '(:path "internal/parser/parser.go"
                                                :start-side "RIGHT" :start-line 13
                                                :side "RIGHT" :line 60))
              :to-be nil)
      (expect (approve-diff-range-lines '(:path "internal/parser/parser.go"
                                                :start-side "RIGHT" :start-line 16
                                                :side "RIGHT" :line 13))
              :to-be nil)))

  (it "are found where they are drawn, without the threads under them"
    (with-current-buffer buffer
      (expect (approve-diff-range-positions test-diff--range) :to-be nil)
      (magit-section-show (approve-test-find-section 'approve-file-section
                                                     "internal/parser/parser.go"))
      (let ((starts (approve-diff-range-positions test-diff--range)))
        (expect (length starts) :to-equal 6)
        (expect (mapcar (lambda (start)
                          (plist-get (get-text-property start 'approve-line) :kind))
                        starts)
                :to-equal '(del del add add add add))
        (expect (string-trim (approve-render-strip-decorations
                              (buffer-substring (car (last starts))
                                                (save-excursion
                                                  (goto-char (car (last starts)))
                                                  (line-end-position)))))
                :to-equal "metrics.Inc(\"step_16\")")))))

(describe "The diff's files, however many"
  :var (buffer)
  (before-each
    (approve-test-with-fixture "medium"
      (setq buffer (approve-test-open-pr "medium"))))
  (after-each
    (kill-buffer buffer))

  (it "are in the PR's order, each section where its heading is"
    (with-current-buffer buffer
      ;; An expanded file's body is drawn at once, among the others.
      (magit-section-show (test-diff--file-section "internal/parser/parser.go"))
      (approve-pr-redraw)
      (let ((files (oref (approve-test-find-section 'approve-diff-section) children)))
        (expect (mapcar (lambda (section) (oref section value)) files)
                :to-equal (mapcar (lambda (file) (alist-get 'path file)) (approve-pr-files)))
        (dolist (section files)
          (expect (magit-section-at (oref section start)) :to-be section)
          (expect (magit-section-at (1- (oref section content))) :to-be section)))))

  (it "stay where they are when one before them unfolds"
    (with-current-buffer buffer
      (let* ((files (oref (approve-test-find-section 'approve-diff-section) children))
             (last (car (last files)))
             (heading (buffer-substring-no-properties (oref last start)
                                                      (oref last content))))
        (magit-section-show (test-diff--file-section "internal/parser/parser.go"))
        (expect (buffer-substring-no-properties (oref last start) (oref last content))
                :to-equal heading)
        (magit-section-show last)
        (expect (magit-section-at (1- (oref last end))) :not :to-be nil)
        (expect (approve-test-section-text last) :to-match "Binary file not shown")))))

(defun test-diff--blob-transport (replay fetched)
  "Return a transport that answers blob queries, and others with REPLAY.
The base version of a file is {\"a\": 1}, the head's {\"a\": 2}; FETCHED,
a symbol, gets each blob's expression pushed onto its value."
  (lambda (request)
    (if (equal (plist-get request :name) "queries/blob.graphql")
        (let* ((expression (alist-get 'expression (plist-get request :variables)))
               (text (if (string-prefix-p (approve-pr-field 'baseRefOid) expression)
                         "{\n  \"a\": 1\n}\n"
                       "{\n  \"a\": 2\n}\n")))
          (set fetched (cons expression (symbol-value fetched)))
          (run-at-time 0 nil (plist-get request :on-success)
                       `((data (repository (id . "R")
                                           (object (__typename . "Blob")
                                                   (id . "B") (text . ,text)))))
                       nil)
          nil)
      (funcall replay request))))

(defvar test-diff--blobs nil
  "The blobs `test-diff--blob-transport' was asked for.")

(describe "The diff of the large PR"
  (it "says when GitHub sent no patch because the file is too large"
    (approve-test-with-fixture "large"
      (let ((buffer (approve-test-open-pr "large")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((section (approve-test-find-section 'approve-file-section
                                                        "pkg/mike/schema_10.json")))
                (magit-section-show section)
                (expect (approve-test-section-text section)
                        :to-match "Diff not shown by GitHub (too large) · RET load it anyway")))
          (kill-buffer buffer)))))

  (it "computes such a diff from both versions of the file on RET"
    (approve-test-with-fixture "large"
      (setq test-diff--blobs nil)
      (let* ((approve-api-transport (test-diff--blob-transport approve-api-transport
                                                               'test-diff--blobs))
             (buffer (approve-test-open-pr "large")))
        (unwind-protect
            (with-current-buffer buffer
              (magit-section-show (approve-test-find-section 'approve-file-section
                                                             "pkg/mike/schema_10.json"))
              (goto-char (point-min))
              (search-forward "load it anyway")
              (approve-visit)
              (approve-test-wait-for (lambda () (= (length test-diff--blobs) 2)))
              (approve-test-wait-for (lambda () (not (approve-pr-loading-p
                                                      '(diff . "pkg/mike/schema_10.json")))))
              (approve-pr-redraw)
              (let ((section (approve-test-find-section 'approve-file-section
                                                        "pkg/mike/schema_10.json")))
                (expect (approve-test-section-text section) :to-match "@@ -1,3 \\+1,3 @@")
                (expect (approve-test-section-text section) :to-match "-  \"a\": 1")
                (expect (approve-test-section-text section) :to-match "\\+  \"a\": 2")))
          (kill-buffer buffer))))))

(describe "A file past the 3000 GitHub lists with their patches"
  (it "offers to compute its diff from both versions of the file on RET"
    (approve-test-with-fixture "large"
      (setq test-diff--blobs nil)
      (let* ((approve-api-transport (test-diff--blob-transport approve-api-transport
                                                               'test-diff--blobs))
             (buffer (approve-test-open-pr "large")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((path (alist-get 'path (car (approve-pr-files)))))
                ;; As in a pull request of more files than GitHub lists.
                (remhash path (approve-model-extra :patches))
                (approve-model-patch `((__typename . "PullRequest")
                                       (id . ,(approve-model-ref-id (approve-model-root-ref)))
                                       (changedFiles . 4202)))
                (approve-pr-redraw)
                (magit-section-show (approve-test-find-section 'approve-file-section path))
                (expect (approve-test-section-text
                         (approve-test-find-section 'approve-file-section path))
                        :to-match (concat "Diff not listed by GitHub (3000 files at most)"
                                          " · RET load it anyway"))
                (goto-char (point-min))
                (search-forward "load it anyway")
                (approve-visit)
                (approve-test-wait-for (lambda () (= (length test-diff--blobs) 2)))
                (approve-test-wait-for (lambda () (not (approve-pr-loading-p (cons 'diff path)))))
                (approve-pr-redraw)
                (expect (approve-test-section-text
                         (approve-test-find-section 'approve-file-section path))
                        :to-match "\\+  \"a\": 2")))
          (kill-buffer buffer)))))

  (it "only offers GitHub for a renamed one, whose old path it doesn't say"
    (approve-test-with-fixture "medium"
      (let ((buffer (approve-test-open-pr "medium")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((path "internal/engine/scheduler.go"))
                (remhash path (approve-model-extra :patches))
                (approve-pr-redraw)
                (magit-section-show (approve-test-find-section 'approve-file-section path))
                (let ((text (approve-test-section-text
                             (approve-test-find-section 'approve-file-section path))))
                  (expect text :to-match "Diff not listed by GitHub · o open it on GitHub")
                  (expect text :not :to-match "load it anyway"))))
          (kill-buffer buffer))))))

;;; test-approve-diff.el ends here
