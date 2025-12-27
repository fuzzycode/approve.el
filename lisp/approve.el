;;; approve.el --- GitHub Pull Request review interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Larsson

;; Author: Björn Larsson
;; Maintainer: Björn Larsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghub "3.5") (magit-section "4.0") (magit "4.0"))
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

(require 'approve-api)
(require 'approve-graphql)

(provide 'approve)
;;; approve.el ends here
