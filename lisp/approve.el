;;; approve.el --- GitHub Pull Request review interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghub "5.0") (magit-section "4.0") (magit "4.0") (markdown-mode "2.5") (dash "2.19"))
;; Homepage: https://github.com/fuzzycode/Approve.el
;; Keywords: tools, vc

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Approve is a GitHub Pull Request review interface for Emacs.
;;
;; It provides a streamlined workflow for reviewing pull requests directly
;; from Emacs, using familiar tools like Magit sections for navigation and
;; Transient menus for actions.
;;
;; Key features:
;; - Browse and review GitHub pull requests
;; - View diffs and file changes
;; - Add comments and suggestions
;; - Approve, request changes, or comment on PRs
;;
;; All state is stored on GitHub, so you can seamlessly switch between
;; Emacs and the GitHub web interface.

;;; Code:

(require 'approve-ui)
(require 'approve-dashboard)
(require 'approve-search)

(defgroup approve nil
  "Settings for Approve PR review tool."
  :group 'tools)

;;; Keybindings

(defcustom approve-prefix-key "C-c"
  "Prefix key for Approve commands.
This prefix is used in `approve-review-mode' for various actions.
Common choices are \"C-c\" or \"C-c a\"."
  :group 'approve
  :type 'key-sequence)

;;; Key Binding Helpers

(defun approve-kbd (key)
  "Return a key sequence string for KEY with the Approve prefix.
KEY should be a string like \"e\" or \"RET\"."
  (kbd (concat approve-prefix-key " " key)))

(defun approve-define-key (keymap key command)
  "Define KEY to run COMMAND in KEYMAP using the Approve prefix.
KEY should be a string like \"e\" or \"RET\"."
  (define-key keymap (approve-kbd key) command))

;;; URL Parsing

(defconst approve--github-pr-url-regexp
  (rx string-start
      (or "https://" "http://")
      (group (or "github.com"
                 ;; GitHub Enterprise: subdomain.github.domain or custom domain
                 (seq (one-or-more (not (any "/")))))) ; any host
      "/"
      (group (one-or-more (not (any "/"))))   ; owner
      "/"
      (group (one-or-more (not (any "/"))))   ; repo
      "/pull/"
      (group (one-or-more digit))             ; PR number
      (optional "/" (zero-or-more anything))  ; optional trailing path
      string-end)
  "Regexp matching GitHub PR URLs.
Groups: 1=host, 2=owner, 3=repo, 4=number.")

(defun approve--parse-pr-url (url)
  "Parse a GitHub PR URL and return (owner repo number) or nil.
Supports both github.com and GitHub Enterprise URLs."
  (when (string-match approve--github-pr-url-regexp url)
    (let ((host (match-string 1 url)))
      ;; Only accept github.com or hosts containing "github"
      (when (or (string= host "github.com")
                (string-match-p "github" host))
        (list (match-string 2 url)
              (match-string 3 url)
              (string-to-number (match-string 4 url)))))))

;;; Public API

;;;###autoload
(defalias 'approve-add-section-hook #'magit-add-section-hook)

;;;###autoload
(defun approve-view-pr (url)
  "Start a review session for a GitHub PR at URL.
URL should be a GitHub pull request URL like:
  https://github.com/owner/repo/pull/123

For GitHub Enterprise, URLs like:
  https://github.mycompany.com/owner/repo/pull/123
are also supported."
  (interactive "sGitHub PR URL: ")
  (let ((parsed (approve--parse-pr-url url)))
    (unless parsed
      (user-error "Invalid GitHub PR URL: %s" url))
    (apply #'approve-ui-view-pr parsed)))

(provide 'approve)
;;; approve.el ends here
