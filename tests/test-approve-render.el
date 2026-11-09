;;; test-approve-render.el --- Tests for approve-render -*- lexical-binding: t; -*-

;;; Commentary:

;; Indentation drawn as text: `approve-render-prefix'.

;;; Code:

(require 'buttercup)
(require 'approve-render)
(require 'magit-section)

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

;;; test-approve-render.el ends here
