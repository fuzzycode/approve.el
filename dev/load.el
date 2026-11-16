;;; load.el --- Load Approve from this checkout into a running Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; To try Approve in the Emacs you work in, from this checkout:
;;
;;   M-x load-file RET ~/workspace/approve/dev/load.el RET
;;
;; Loading it again reloads every module (compiled ones when `make
;; build' ran) and redraws the open PR buffers and the dashboard, so a
;; fix shows at once; options left at their default take a changed default.
;; Dependencies the Emacs lacks (often ghub and treepy) come from the
;; project's Cask (`make install').  It turns on `approve-dev-offline-mode' with
;; `approve-dev-offline-fallback': the fixture PRs open without a token,
;; any other URL goes to GitHub.  Then: M-x approve RET URL RET.
;;
;; Not part of the package.

;;; Code:

(defvar approve-dev-load-order
  '(approve-log approve-graphql approve-api approve-api-paging approve-api-queue
    approve-api-url approve-model approve-session approve-queries approve-metadata approve-clone
    approve-faces approve-icons approve-html approve-render approve-pr approve-changes approve-thread
    approve-sections approve-conversation approve-diff approve-context approve-nav
    approve-eldoc approve-input approve-mutations approve-actions approve-review approve-commands
    approve-suggestions approve-manage approve-merge approve-list approve-dashboard approve-menus approve approve-dev)
  "Modules in the order they are reloaded; others follow, by name.")

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
       (lisp (expand-file-name "lisp" root))
       (elpa (expand-file-name (format ".cask/%s/elpa" emacs-version) root)))
  (add-to-list 'load-path lisp)
  ;; Dependencies this Emacs lacks come from the project's Cask.
  (dolist (dir (and (file-directory-p elpa)
                    (directory-files elpa t "\\`[^.]")))
    (let ((package (replace-regexp-in-string "-[0-9.]+\\'" "" (file-name-nondirectory dir))))
      (when (and (file-directory-p dir) (not (locate-library package)))
        (add-to-list 'load-path dir t))))
  (unless (locate-library "ghub")
    (user-error "Approve needs its dependencies: run `make install' in %s" root))
  (let* ((names (mapcar #'file-name-base (directory-files lisp nil "\\`approve.*\\.el\\'")))
         (order (append (mapcar #'symbol-name approve-dev-load-order)
                        (seq-difference names (mapcar #'symbol-name approve-dev-load-order))))
         ;; Options still at their default follow a changed default; a
         ;; reload alone keeps the old value.
         (defaults nil))
    (mapatoms (lambda (symbol)
                (when (and (string-prefix-p "approve-" (symbol-name symbol))
                           (boundp symbol)
                           (get symbol 'standard-value)
                           (ignore-errors
                             (equal (symbol-value symbol)
                                    (eval (car (get symbol 'standard-value)) t))))
                  (push symbol defaults))))
    ;; Lists the modules fill as they load start empty again, so what a
    ;; module no longer adds goes away.
    (dolist (registry '(approve-pr-header-line-functions approve-pr-lazy-loaders
                        approve-pr-load-hook approve-pr-clear-cache-functions
                        approve-visit-commit-functions))
      (when (boundp registry)
        (set registry nil)))
    ;; `defface' leaves a face it has defined before as it was; without
    ;; its old spec, the reload gives it the new one.  Faces the user
    ;; customized keep theirs.
    (dolist (face (face-list))
      (when (and (string-prefix-p "approve-" (symbol-name face))
                 (not (get face 'saved-face))
                 (not (get face 'customized-face)))
        (put face 'face-defface-spec nil)))
    ;; A stale .elc, left by a build that did not finish, never wins over
    ;; its newer source.
    (let ((load-prefer-newer t))
      (dolist (name order)
        (when (and (member name names)
                   (or (not (equal name "approve-evil")) (featurep 'evil)))
          (load (expand-file-name name lisp) nil t))))
    (dolist (symbol defaults)
      (let ((value (eval (car (get symbol 'standard-value)) t)))
        (unless (equal (symbol-value symbol) value)
          (funcall (or (get symbol 'custom-set) #'set-default) symbol value)))))
  (approve-icon-clear-cache)
  (approve-html-clear-cache)
  (setq approve-dev-offline-fallback t)
  (approve-dev-offline-mode 1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((derived-mode-p 'approve-pr-mode) (approve-pr-redraw))
            ((derived-mode-p 'approve-dashboard-mode) (approve-dashboard-redraw)))))
  (message "Approve loaded from %s; fixture PRs replay, other URLs go to GitHub" root))

;;; load.el ends here
