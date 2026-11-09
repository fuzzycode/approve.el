;;; load.el --- Load Approve from this checkout into a running Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; To try Approve in the Emacs you work in, from this checkout:
;;
;;   M-x load-file RET ~/workspace/approve/dev/load.el RET
;;
;; Loading it again reloads every module (compiled ones when `make
;; build' ran) and redraws the open PR buffers, so a fix shows at once.
;; Dependencies the Emacs lacks (often ghub and treepy) come from the
;; project's Cask (`make install').  It turns on `approve-dev-offline-mode' with
;; `approve-dev-offline-fallback': the fixture PRs open without a token,
;; any other URL goes to GitHub.  Then: M-x approve RET URL RET.
;;
;; Not part of the package.

;;; Code:

(defvar approve-dev-load-order
  '(approve-log approve-graphql approve-api approve-api-paging approve-api-queue
    approve-api-url approve-model approve-session approve-queries approve-faces
    approve-icons approve-html approve-render approve-pr approve-thread
    approve-sections approve-conversation approve-diff approve-context approve-nav
    approve-menus approve-eldoc approve approve-dev)
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
                        (seq-difference names (mapcar #'symbol-name approve-dev-load-order)))))
    (dolist (name order)
      (when (and (member name names)
                 (or (not (equal name "approve-evil")) (featurep 'evil)))
        (load (expand-file-name name lisp) nil t))))
  (approve-icon-clear-cache)
  (approve-html-clear-cache)
  (setq approve-dev-offline-fallback t)
  (approve-dev-offline-mode 1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'approve-pr-mode)
        (approve-pr-redraw))))
  (message "Approve loaded from %s; fixture PRs replay, other URLs go to GitHub" root))

;;; load.el ends here
