;;; test-approve-faces.el --- Tests for approve-faces and approve-icons -*- lexical-binding: t; -*-

;;; Commentary:

;; Faces inherit, labels get readable colours, and icons come from
;; nerd-icons or fall back to Unicode.

;;; Code:

(require 'buttercup)
(require 'approve-faces)
(require 'approve-icons)

(defun test-faces--approve-faces ()
  "Return every face whose name starts with approve-."
  (seq-filter (lambda (face) (string-prefix-p "approve-" (symbol-name face)))
              (face-list)))

(describe "approve-faces"
  (it "defines every face in the approve-faces group, inheriting or plain"
    (let ((faces (test-faces--approve-faces))
          (members (mapcar #'car (get 'approve-faces 'custom-group))))
      (expect (length faces) :to-be-greater-than 40)
      (dolist (face faces)
        (expect (memq face members) :not :to-be nil)
        (expect (face-documentation face) :not :to-be nil))))

  (it "inherits only from faces that exist once it is loaded"
    ;; A face inheriting from one not defined yet has no colour; magit's
    ;; are in magit.el and magit-log.el, not only in magit-diff.el.
    (dolist (face (test-faces--approve-faces))
      (dolist (parent (ensure-list (face-attribute face :inherit nil nil)))
        (unless (eq parent 'unspecified)
          (expect (cons face (and (facep parent) t)) :to-equal (cons face t))))))

  (it "sets face and font-lock-face"
    (let ((text (approve-face "x" 'approve-dim)))
      (expect (get-text-property 0 'face text) :to-be 'approve-dim)
      (expect (get-text-property 0 'font-lock-face text) :to-be 'approve-dim)
      (expect (approve-face "x" nil) :to-equal "x")))

  (it "adds a face under the faces a string has"
    (let ((text (approve-face-add (concat (approve-face "a" 'bold) "b") 'italic)))
      (expect (get-text-property 0 'face text) :to-equal '(bold italic))
      (expect (get-text-property 1 'face text) :to-be 'italic)
      (expect (get-text-property 0 'font-lock-face text) :to-equal '(bold italic))))

  (it "gives labels their GitHub colour with a readable foreground"
    (expect (approve-label-face "a2eeef")
            :to-equal '(:background "#a2eeef" :foreground "#000000"
                        :inherit approve-label))
    (expect (plist-get (approve-label-face "#5319E7") :foreground)
            :to-equal "#ffffff")
    (expect (approve-label-face "nonsense") :to-be 'approve-label)
    (expect (approve-label-face nil) :to-be 'approve-label)))

(describe "approve-icon"
  :var (saved)
  (before-each
    (setq saved approve-use-nerd-icons)
    (approve-icon-clear-cache))
  (after-each
    (setq approve-use-nerd-icons saved)
    (approve-icon-clear-cache))

  (it "falls back to Unicode when nerd-icons are off"
    (setq approve-use-nerd-icons nil)
    (expect (approve-icon 'approved) :to-equal "✓")
    (expect (get-text-property 0 'face (approve-icon 'approved 'approve-approved))
            :to-be 'approve-approved))

  (it "uses the Material Design glyph when nerd-icons are on in a terminal"
    (setq approve-use-nerd-icons t)
    (let ((icon (approve-icon 'approved 'approve-approved)))
      (expect icon :to-equal (nerd-icons-mdicon "nf-md-check_decagram"))
      (expect (memq 'approve-approved
                    (ensure-list (get-text-property 0 'face icon)))
              :not :to-be nil)))

  (it "draws drafts with Codicons' pull request draft, as gh-dash does"
    (setq approve-use-nerd-icons t)
    (approve-icon-clear-cache)
    (expect (approve-icon 'draft)
            :to-equal (nerd-icons-codicon "nf-cod-git_pull_request_draft")))

  (it "draws an icon larger or smaller when given a height"
    (setq approve-use-nerd-icons t)
    (let ((icon (approve-icon 'comment nil 1.2)))
      (expect icon :to-equal (nerd-icons-mdicon "nf-md-comment_outline"))
      (expect (plist-get (get-text-property 0 'face icon) :height) :to-equal 1.2)
      (expect (approve-icon 'comment) :not :to-be icon))
    (setq approve-use-nerd-icons nil)
    (approve-icon-clear-cache)
    (expect (format "%S" (get-text-property 0 'face (approve-icon 'comment nil 1.2)))
            :to-match ":height 1.2"))

  (it "has a glyph and a fallback for every icon"
    (dolist (entry approve-icons)
      (expect (assq (car entry) approve-icon-fallbacks) :not :to-be nil)
      (expect (approve-icon--glyph (cdr entry)) :to-be-truthy)))

  (it "takes a glyph from any nerd-icons set, by the prefix of its name"
    (setq approve-use-nerd-icons t)
    (let ((approve-icons '((approved . "nf-cod-pass") (merged . "nf-oct-git_merge"))))
      (approve-icon-clear-cache)
      (expect (approve-icon 'approved) :to-equal (nerd-icons-codicon "nf-cod-pass"))
      (expect (approve-icon 'merged) :to-equal (nerd-icons-octicon "nf-oct-git_merge"))))

  (it "falls back for an unknown glyph, and says ? for an unknown name"
    (setq approve-use-nerd-icons t)
    (let ((approve-icons '((approved . "nf-md-no-such-icon") (merged . "no-such-set"))))
      (approve-icon-clear-cache)
      (expect (approve-icon 'approved) :to-equal "✓")
      (expect (approve-icon 'merged) :to-equal "⮌"))
    (expect (approve-icon 'no-such-icon) :to-equal "?"))

  (it "caches icons"
    (setq approve-use-nerd-icons nil)
    (expect (approve-icon 'merged) :to-be (approve-icon 'merged))))

;;; test-approve-faces.el ends here
