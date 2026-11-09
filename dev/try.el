;;; try.el --- A fresh Emacs to try Approve on the fixtures -*- lexical-binding: t; -*-

;;; Commentary:

;; `make try' starts `emacs -Q' with this file: Approve from lisp/, its
;; dependencies from Cask, nothing from your own configuration.  It
;; picks a Nerd Font if one is installed (so icons are glyphs, else
;; their Unicode fallbacks), a dark built-in theme, and opens the start
;; page of `approve-dev-try', which replays the fixtures.
;;
;; Not part of the package.

;;; Code:

(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      make-backup-files nil)

(defconst approve-try-fonts
  '("JetBrainsMono Nerd Font Mono" "JetBrainsMono Nerd Font"
    "Iosevka Nerd Font Mono" "Iosevka Nerd Font"
    "Inconsolata Nerd Font Mono" "Symbols Nerd Font Mono")
  "Nerd Fonts to use, the first one installed wins.")

(defvar approve-try-font
  (and (display-graphic-p)
       (seq-find (lambda (family) (find-font (font-spec :family family)))
                 approve-try-fonts))
  "The Nerd Font in use, or nil.")

(when (display-graphic-p)
  (when approve-try-font
    (set-face-attribute 'default nil :family approve-try-font :height 150))
  (set-frame-size nil 130 50)
  (load-theme 'modus-vivendi-tinted t))

(require 'approve)
(require 'approve-dev)

(when approve-try-font
  (setq nerd-icons-font-family approve-try-font)
  (approve-icon-clear-cache))

(approve-dev-try)

;;; try.el ends here
