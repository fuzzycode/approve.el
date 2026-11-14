;;; approve-evil.el --- Evil bindings of Approve -*- lexical-binding: t; -*-

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

;; Bindings for evil's normal state in Approve buffers (decision D17;
;; docs/ui.org, "Keys").  They never shadow motions, search, visual
;; selection or yank: Approve's single-letter keys move to g-, y- and
;; capital-letter keys instead.
;;
;; j, k and the arrow keys move by screen line, as magit's own motions do
;; (`line-move-visual'): moving by logical line from a collapsed
;; heading stops at the start of its hidden body, and magit's
;; intangible text then puts point back at the end of the heading.
;;
;; Input buffers start in insert state, to write at once.
;;
;; The modes require this file once evil is loaded; nothing here
;; requires evil.

;;; Code:

(require 'magit-section)
(require 'approve-pr)

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defconst approve-evil-pr-bindings
  '(("j" . evil-next-visual-line)
    ("k" . evil-previous-visual-line)
    ("<down>" . evil-next-visual-line)
    ("<up>" . evil-previous-visual-line)
    ("RET" . approve-visit)
    ("TAB" . approve-section-toggle)
    ("za" . approve-section-toggle)
    ("zM" . approve-fold-threads)
    ("zm" . approve-fold-threads)
    ("zR" . approve-unfold-threads)
    ("zr" . approve-unfold-threads)
    ("gr" . approve-pr-refresh)
    ("go" . approve-browse)
    ("gy" . approve-copy)
    ("J" . approve-jump)
    ("K" . approve-show-details)
    ("gj" . magit-section-forward)
    ("gk" . magit-section-backward)
    ("C-j" . magit-section-forward)
    ("C-k" . magit-section-backward)
    ("]u" . approve-next-unviewed-file)
    ("[u" . approve-previous-unviewed-file)
    ("]t" . approve-next-thread)
    ("[t" . approve-previous-thread)
    ("q" . quit-window))
  "Keys of `approve-pr-mode' in evil's normal state: (KEY . COMMAND).")

(defun approve-evil-setup ()
  "Bind `approve-evil-pr-bindings' in evil's normal state.
Input buffers start in insert state."
  (dolist (binding approve-evil-pr-bindings)
    (evil-define-key* 'normal approve-pr-mode-map (kbd (car binding)) (cdr binding)))
  (evil-set-initial-state 'approve-input-mode 'insert))

(approve-evil-setup)

(provide 'approve-evil)

;;; approve-evil.el ends here
