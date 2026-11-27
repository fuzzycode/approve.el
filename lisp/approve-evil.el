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
;; capital-letter keys instead.  The exceptions are ?, evil's search
;; backward, which opens the dispatch menu as in evil-collection's
;; Magit, in visual state too, so its commands act on the selection
;; (/ still searches), and +, a line motion that j and RET do too,
;; which reacts, and in the dashboard shows more of a list.  Keys
;; that would edit text in a file buffer (c s r R x D C S X i a A)
;; are free in a read-only buffer, so the
;; commands that change the pull request take them; in visual state,
;; c s r C act on the selection, and ? opens the dispatch menu on it.
;; So are o and O, which open a line: they are Emacs's o and O there,
;; open in the browser and fold all but this (go and zp too).  And =,
;; the operator that indents, opens the filter menu as in Emacs
;; (decision D54).
;;
;; The dispatch menus show these keys in evil's normal and visual
;; states (`approve-evil-menu-keys'): what a menu shows works in the
;; buffer too, and a letter means the same command in both.  The
;; commands a menu alone reaches keep their letters, but for those
;; that normal state gives to another command (decision D17).
;;
;; j, k and the arrow keys move by screen line, as magit's own motions do
;; (`line-move-visual'): moving by logical line from a collapsed
;; heading stops at the start of its hidden body, and magit's
;; intangible text then puts point back at the end of the heading.
;;
;; Input buffers start in insert state, to write at once.
;;
;; In a file's buffer in a clone, the feedback mode binds ]t and [t,
;; and the fold keys za, zo, zO, zc, zC, zR and zM, which fold its
;; threads as the innermost fold of their line and pass on to the
;; buffer's own folds (`approve-evil-setup-feedback', decision D51).
;;
;; The modes require this file once evil is loaded; nothing here
;; requires evil.

;;; Code:

(require 'magit-section)
(require 'approve-pr)
(require 'approve-dashboard)
(require 'approve-file)

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-define-minor-mode-key "evil-core" (state mode key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-add-command-properties "evil-common" (command &rest properties))

(defconst approve-evil-pr-bindings
  '(("j" . evil-next-visual-line)
    ("k" . evil-previous-visual-line)
    ("<down>" . evil-next-visual-line)
    ("<up>" . evil-previous-visual-line)
    ("RET" . approve-visit)
    ("TAB" . approve-section-toggle)
    ("za" . approve-section-toggle)
    ("zo" . approve-section-show)
    ("zc" . approve-section-hide)
    ("O" . approve-fold-others)
    ("zp" . approve-fold-others)
    ("zM" . approve-fold-threads)
    ("zm" . approve-fold-threads)
    ("zR" . approve-unfold-threads)
    ("zr" . approve-unfold-threads)
    ("gr" . approve-pr-refresh)
    ("o" . approve-browse)
    ("go" . approve-browse)
    ("gy" . approve-copy)
    ("J" . approve-jump)
    ("=" . approve-filter-menu)
    ("K" . approve-show-details)
    ("gj" . magit-section-forward)
    ("gk" . magit-section-backward)
    ("C-j" . magit-section-forward)
    ("C-k" . magit-section-backward)
    ("]u" . approve-next-unviewed-file)
    ("[u" . approve-previous-unviewed-file)
    ("]t" . approve-next-thread)
    ("[t" . approve-previous-thread)
    ("x" . approve-toggle-viewed)
    ("c" . approve-comment)
    ("s" . approve-suggest)
    ("r" . approve-reply)
    ("R" . approve-resolve)
    ("i" . approve-edit)
    ("D" . approve-delete)
    ("C" . approve-comment-on-pr)
    ("+" . approve-react)
    ("a" . approve-commit-suggestion)
    ("A" . approve-batch-suggestion)
    ("S" . approve-submit-review)
    ("X" . approve-discard-review)
    ("gm" . approve-merge)
    ("gf" . approve-visit-file)
    ("gF" . approve-feedback-visit-file)
    ("?" . approve-dispatch)
    ("q" . quit-window))
  "Keys of `approve-pr-mode' in evil's normal state: (KEY . COMMAND).
The first key of a command is the one its dispatch menu shows.")

(defconst approve-evil-visual-bindings
  '(("c" . approve-comment)
    ("s" . approve-suggest)
    ("r" . approve-reply)
    ("C" . approve-comment-on-pr)
    ("?" . approve-dispatch))
  "Keys of `approve-pr-mode' in evil's visual state: (KEY . COMMAND).
They act on the selection: a comment or suggestion on the selected
lines of the diff (\\`V' picks them), or a reply or conversation
comment quoting the selected text.  The dispatch menu keeps the
selection for the command picked in it.")

(defconst approve-evil-file-bindings
  '(("]c" . approve-file-next-change)
    ("[c" . approve-file-previous-change)
    ("]t" . approve-file-next-thread)
    ("[t" . approve-file-previous-thread)
    ("RET" . approve-visit)
    ("TAB" . approve-file-toggle-threads)
    ("za" . approve-file-toggle-threads)
    ("gb" . approve-file-toggle-version)
    ("gF" . approve-feedback-visit-file)
    ("gr" . approve-file-refresh)
    ("o" . approve-browse)
    ("go" . approve-browse)
    ("gy" . approve-copy)
    ("x" . approve-toggle-viewed)
    ("c" . approve-comment)
    ("s" . approve-suggest)
    ("r" . approve-reply)
    ("R" . approve-resolve)
    ("i" . approve-edit)
    ("D" . approve-delete)
    ("+" . approve-react)
    ("?" . approve-file-dispatch)
    ("q" . quit-window))
  "Keys of `approve-file-mode' in evil's normal state: (KEY . COMMAND).
The PR buffer's keys for the same commands.  The changes are ]c and
[c, as in vim's diff mode, since n and p are search and paste; b is
a motion, so gb shows the other version.")

(defconst approve-evil-dashboard-bindings
  '(("j" . evil-next-visual-line)
    ("k" . evil-previous-visual-line)
    ("<down>" . evil-next-visual-line)
    ("<up>" . evil-previous-visual-line)
    ("RET" . approve-visit)
    ("TAB" . approve-section-toggle)
    ("za" . approve-section-toggle)
    ("zo" . approve-section-show)
    ("zc" . approve-section-hide)
    ("zO" . approve-section-show)
    ("zC" . approve-section-hide)
    ("O" . approve-fold-others)
    ("zp" . approve-fold-others)
    ("zM" . approve-dashboard-fold-lists)
    ("zm" . approve-dashboard-fold-lists)
    ("zR" . approve-dashboard-unfold-lists)
    ("zr" . approve-dashboard-unfold-lists)
    ("gr" . approve-dashboard-refresh)
    ("gR" . approve-dashboard-refresh-section)
    ("+" . approve-dashboard-load-more)
    ("o" . approve-browse)
    ("go" . approve-browse)
    ("gy" . approve-copy)
    ("K" . approve-show-details)
    ("gj" . magit-section-forward)
    ("gk" . magit-section-backward)
    ("C-j" . magit-section-forward)
    ("C-k" . magit-section-backward)
    ("?" . approve-dashboard-dispatch)
    ("q" . quit-window))
  "Keys of `approve-dashboard-mode' in evil's normal state: (KEY . COMMAND).
The PR buffer's keys for the same commands; G, which refreshes the
list at point in Emacs, is evil's motion, so gR does it.  + shows
more of the list at point, as in Emacs.  The z keys
fold as in vim: za, zo and zc the list at point (zO and zC too, as
lists hold no folds), zM and zm every list; zR and zr unfold them.
O and zp fold every list but the one at point, as in a PR buffer.")

(defconst approve-evil-menus
  '((approve-dispatch approve-evil-pr-bindings
                      (approve-set-assignees . "@")
                      (approve-request-reviewers . "W")
                      (approve-toggle-draft . "P"))
    (approve-file-dispatch approve-evil-file-bindings)
    (approve-dashboard-dispatch approve-evil-dashboard-bindings))
  "The dispatch menus' keys in evil's states: (PREFIX BINDINGS . KEYS).
In the transient PREFIX, a command has its first key in the variable
BINDINGS, the buffer's keys in normal state.  KEYS, an alist of
\(COMMAND . KEY), gives other keys to commands the menu alone reaches,
whose letters normal state gives to other commands: a, A and D are
committing a suggestion, the batch and deleting there, so assignees,
reviewers and draft are @, W and P in evil's dispatch.")

(defun approve-evil-menu-keys (prefix)
  "Return the keys of the transient PREFIX in the current buffer.
They are an alist of (COMMAND . KEY), from `approve-evil-menus', when
the buffer is in evil's normal or visual state, and nil in another
state or without evil.  A command not in it keeps the key PREFIX
gives it.  The dispatch menus take these keys as they open
\(`approve-menus--set-keys'), so the keys they show run the same
commands in the buffer."
  (when-let* (((bound-and-true-p evil-local-mode))
              ((memq (bound-and-true-p evil-state) '(normal visual)))
              (menu (assq prefix approve-evil-menus)))
    (let ((keys (copy-sequence (cddr menu))))
      (pcase-dolist (`(,key . ,command) (symbol-value (cadr menu)))
        (unless (assq command keys)
          (push (cons command key) keys)))
      keys)))

(defconst approve-evil-feedback-bindings
  '(("]t" . approve-feedback-next-thread)
    ("[t" . approve-feedback-previous-thread)
    ("za" . approve-feedback-toggle-fold)
    ("zo" . approve-feedback-open-fold)
    ("zO" . approve-feedback-open-fold-recursively)
    ("zc" . approve-feedback-close-fold)
    ("zC" . approve-feedback-close-fold-recursively)
    ("zR" . approve-feedback-open-all-folds)
    ("zM" . approve-feedback-close-all-folds))
  "Keys of `approve-feedback-mode' in evil's normal state: (KEY . COMMAND).
The next and previous unresolved thread, as in Approve's buffers,
and the fold keys (decision D51).  A line's threads are its
innermost fold: za, zo and zc fold or unfold them, and do what they
do without the mode when the line has none, or when they are as the
key wants them; zO and zC do both.  zR and zM unfold and fold every
thread of the file, and leave the code's folds to zr and zm, which
the mode doesn't bind.  The other commands are under
`approve-feedback-prefix-key'.")

(defun approve-evil-setup-feedback ()
  "Bind `approve-evil-feedback-bindings' for the feedback mode, in normal state.
approve-feedback.el, a module for a local clone that the core never
requires (decision D36), calls this as the mode turns on.  They are
the mode's minor-mode keys (`evil-define-minor-mode-key'), which go
before those of the modes that defined theirs earlier, such as a
folding mode's z keys: the feedback mode passes those on itself."
  (dolist (binding approve-evil-feedback-bindings)
    (evil-define-minor-mode-key 'normal 'approve-feedback-mode
      (kbd (car binding)) (cdr binding))))

(defun approve-evil-setup ()
  "Bind `approve-evil-pr-bindings' in evil's normal state.
Also `approve-evil-visual-bindings' in its visual state,
`approve-evil-dashboard-bindings' in the dashboard's normal state, and
`approve-evil-file-bindings' in a file view's, where visual state
comments or suggests on the selection.  Input buffers start in insert
state."
  (dolist (binding approve-evil-pr-bindings)
    (evil-define-key* 'normal approve-pr-mode-map (kbd (car binding)) (cdr binding)))
  (dolist (binding approve-evil-visual-bindings)
    (evil-define-key* 'visual approve-pr-mode-map (kbd (car binding)) (cdr binding)))
  (dolist (binding approve-evil-dashboard-bindings)
    (evil-define-key* 'normal approve-dashboard-mode-map (kbd (car binding)) (cdr binding)))
  (dolist (binding approve-evil-file-bindings)
    (evil-define-key* 'normal approve-file-mode-map (kbd (car binding)) (cdr binding)))
  (dolist (binding '(("c" . approve-comment) ("s" . approve-suggest)
                     ("?" . approve-file-dispatch)))
    (evil-define-key* 'visual approve-file-mode-map (kbd (car binding)) (cdr binding)))
  (evil-set-initial-state 'approve-input-mode 'insert)
  ;; A thread picked anywhere is a jump: C-o comes back (decision D52).
  (evil-add-command-properties #'approve-jump-thread :jump t))

(approve-evil-setup)

(provide 'approve-evil)

;;; approve-evil.el ends here
