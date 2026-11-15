;;; test-approve-render.el --- Tests for approve-render -*- lexical-binding: t; -*-

;;; Commentary:

;; Indentation drawn as text: `approve-render-prefix'.  Reactions,
;; paths shortened to fit, ages for lists, and key hints that name the
;; keys bound.

;;; Code:

(require 'buttercup)
(require 'approve-render)
(require 'approve-pr)
(require 'magit-section)

(defun approve-test-render--reply ()
  "A command for the key hint tests."
  (interactive))

(defun approve-test-render--resolve ()
  "A command for the key hint tests."
  (interactive))

(defclass test-render--section (magit-section) ()
  "A section for the tests.")

(defmacro test-render--with-buffer (&rest body)
  "Run BODY in a temporary buffer that copies like an Approve buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq-local filter-buffer-substring-function
                 (lambda (beg end delete)
                   (approve-render-strip-decorations
                    (buffer-substring--filter beg end delete))))
     ,@body))

(describe "approve-render-prefix"
  (it "indents each line with text that copying leaves out"
    (test-render--with-buffer
      (insert "one\ntwo\n")
      (approve-render-prefix (point-min) (point) "  ")
      (expect (buffer-string) :to-equal "  one\n  two\n")
      (expect (filter-buffer-substring (point-min) (point-max)) :to-equal "one\ntwo\n")))

  (it "nests: an outer prefix goes in front of an inner one"
    (test-render--with-buffer
      (insert "text\n")
      (approve-render-prefix (point-min) (point) "│ ")
      (approve-render-prefix (point-min) (point) "    ")
      (expect (buffer-string) :to-equal "    │ text\n")
      (expect (get-text-property (1+ (string-search "text" (buffer-string))) 'wrap-prefix)
              :to-equal "    │ ")))

  (it "puts no trailing spaces on empty lines"
    (test-render--with-buffer
      (insert "one\n\ntwo\n")
      (approve-render-prefix (point-min) (point) "  │   ")
      (approve-render-prefix (point-min) (point) "  ")
      (expect (buffer-string) :to-equal "    │   one\n    │\n    │   two\n")
      (erase-buffer)
      (insert "one\n\n")
      (approve-render-prefix (point-min) (point) "  ")
      (expect (buffer-string) :to-equal "  one\n\n")))

  (it "keeps the prefix's faces and takes its line's diff line and thing"
    (test-render--with-buffer
      (insert (propertize "code\n" 'approve-line '(:new 3) 'approve-thing '(:kind user)
                          'face 'bold))
      (approve-render-prefix (point-min) (point) (approve-face "│ " 'approve-dim))
      (expect (get-text-property 1 'approve-line) :to-equal '(:new 3))
      (expect (get-text-property 1 'approve-thing) :to-equal '(:kind user))
      (expect (get-text-property 1 'face) :to-be 'approve-dim)))

  (it "moves text after the region, point included"
    (test-render--with-buffer
      (insert "one\ntwo\n")
      (let ((end (point)))
        (insert "after")
        (approve-render-prefix (point-min) end "  ")
        (expect (point) :to-equal (point-max))
        (expect (buffer-string) :to-equal "  one\n  two\nafter"))))

  (it "puts the prefix inside a finished section that starts on the line"
    (test-render--with-buffer
      (let ((magit-insert-section--parent nil) child)
        (magit-insert-section (test-render--section 'root)
          (let ((start (point)))
            (setq child (magit-insert-section (test-render--section 'child)
                          (magit-insert-heading "heading")
                          (insert "body\n")))
            (approve-render-prefix start (point) "  ")))
        (expect (buffer-substring (oref child start) (oref child content))
                :to-equal "  heading\n")
        (expect (buffer-substring (oref child content) (oref child end))
                :to-equal "  body\n")
        (expect (magit-section-at 1) :to-be child)))))

(describe "People"
  (it "are written @login, carrying who they are"
    (let ((text (approve-render-user '((__typename . "User") (id . "U_1")
                                       (login . "octocat") (name . "Mona")))))
      (expect (substring-no-properties text) :to-equal "@octocat")
      (expect (get-text-property 0 'approve-thing text)
              :to-equal '(:kind user :login "octocat" :name "Mona" :type "User"
                                :id "U_1" :url nil))
      (expect (substring-no-properties (approve-render-user nil)) :to-equal "@ghost")))

  (it "who wrote a commit are its account, else Git's name without an @"
    (expect (substring-no-properties
             (approve-render-git-actor '((name . "Mona Lisa") (email . "mona@example.com")
                                         (user (login . "octocat")))))
            :to-equal "@octocat")
    (let ((text (approve-render-git-actor '((name . "Mona Lisa")
                                            (email . "mona@example.com")))))
      (expect (substring-no-properties text) :to-equal "Mona Lisa")
      (expect (get-text-property 0 'approve-thing text)
              :to-equal '(:kind git-actor :name "Mona Lisa" :email "mona@example.com")))))

(describe "approve-render-reactions-text"
  :var (saved)
  (before-each
    (setq saved approve-use-nerd-icons)
    (approve-icon-clear-cache))
  (after-each
    (setq approve-use-nerd-icons saved)
    (approve-icon-clear-cache))

  (it "draws each reaction as a flat icon with its count"
    (setq approve-use-nerd-icons t)
    (let ((text (approve-render-reactions-text
                 '(((content . "THUMBS_UP") (viewerHasReacted . t)
                    (reactors (totalCount . 3)))
                   ((content . "HOORAY") (reactors (totalCount . 1)))
                   ((content . "ROCKET") (reactors (totalCount . 0)))))))
      (expect (substring-no-properties text)
              :to-equal (concat (nerd-icons-mdicon "nf-md-thumb_up_outline") " 3  "
                                (nerd-icons-mdicon "nf-md-party_popper") " 1"))
      (expect (memq 'approve-reaction-viewer (ensure-list (get-text-property 0 'face text)))
              :not :to-be nil)
      (expect (plist-get (get-text-property 0 'approve-thing text) :kind)
              :to-be 'reactions)))

  (it "falls back to GitHub's emoji without nerd-icons"
    (setq approve-use-nerd-icons nil)
    (expect (substring-no-properties
             (approve-render-reactions-text
              '(((content . "HEART") (reactors (totalCount . 2)))
                ((content . "EYES") (reactors (totalCount . 1))))))
            :to-equal "❤️ 2  👀 1")))

(describe "approve-render-shorten-path"
  (it "leaves a path that fits"
    (expect (approve-render-shorten-path "internal/x.go" 13) :to-equal "internal/x.go"))

  (it "keeps the first directory and the end, cut between directories"
    (expect (approve-render-shorten-path "internal/render/templates/page.tmpl" 24)
            :to-equal "internal/…/page.tmpl")
    (expect (approve-render-shorten-path "internal/render/templates/{base.tmpl → page.tmpl}" 40)
            :to-equal "internal/…/{base.tmpl → page.tmpl}"))

  (it "cuts inside a name when no directory fits"
    (expect (approve-render-shorten-path "src/x/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.el" 20)
            :to-equal "src/…aaaaaaaaaaaa.el")
    (expect (approve-render-shorten-path "averyveryverylongfilename-without-dirs.txt" 20)
            :to-equal "…me-without-dirs.txt"))

  (it "never returns more than WIDTH columns"
    (dolist (width '(2 5 10 20 30))
      (expect (string-width (approve-render-shorten-path
                             "internal/render/templates/{base.tmpl → page.tmpl}" width))
              :not :to-be-greater-than width))))

(describe "approve-render-age"
  (it "says how long ago in a few columns: minutes, hours, days, weeks, months, years"
    (let ((approve-render-now (encode-time (list 0 0 12 4 10 2026 nil nil t))))
      (expect (mapcar #'approve-render-age
                      '("2026-10-04T11:59:30Z" "2026-10-04T11:55:00Z" "2026-10-04T10:00:00Z"
                        "2026-10-01T12:00:00Z" "2026-09-20T12:00:00Z" "2025-10-20T12:00:00Z"
                        "2024-10-01T12:00:00Z"))
              :to-equal '("now" "5m" "2h" "3d" "2w" "11mo" "2y"))))

  (it "says now for a time to come, and nothing without a time"
    (let ((approve-render-now (encode-time (list 0 0 12 4 10 2026 nil nil t))))
      (expect (approve-render-age "2026-10-04T12:05:00Z") :to-equal "now")
      (expect (approve-render-age nil) :to-equal ""))))

(describe "Key hints"
  (it "name the key that runs each command here, leaving out the unbound"
    (with-temp-buffer
      (use-local-map (define-keymap "r" #'approve-test-render--reply))
      (expect (substring-no-properties
               (approve-render-hints '(approve-test-render--reply "reply")
                                     nil
                                     '(approve-test-render--resolve "resolve")))
              :to-equal "r reply")
      (local-set-key (kbd "R") #'approve-test-render--resolve)
      (local-set-key (kbd "r") nil)
      (local-set-key (kbd "C-c r") #'approve-test-render--reply)
      (expect (substring-no-properties
               (approve-render-hints '(approve-test-render--reply "reply")
                                     '(approve-test-render--resolve "resolve")))
              :to-equal "C-c r reply · R resolve")))

  (it "are hidden with approve-show-hints, but for the input keys and retry"
    (with-temp-buffer
      (use-local-map (define-keymap "r" #'approve-test-render--reply
                                    "g" #'approve-pr-refresh))
      (let ((approve-show-hints nil))
        (expect (approve-render-hints '(approve-test-render--reply "reply")) :to-be nil)
        (expect (substring-no-properties
                 (approve-render-key-hints '((approve-test-render--reply "reply"))))
                :to-equal "r reply")
        (expect (substring-no-properties (approve-render-retry-hint))
                :to-equal "g to retry")))))

;;; test-approve-render.el ends here
