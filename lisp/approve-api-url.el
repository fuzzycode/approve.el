;;; approve-api-url.el --- Fetch plain URLs, such as images -*- lexical-binding: t; -*-

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

;; `approve-api-fetch-url' gets the bytes at a URL that is not part of
;; the API: the images in comments, which GitHub serves through its
;; camo proxy or as signed links (private-user-images).  It goes
;; through `url-queue-retrieve', so only a few run at once and each has
;; a timeout, and it needs no token.
;;
;; Like the API requests, the continuations run in the buffer that
;; asked and are skipped when it has been killed.  Tests replace
;; `approve-api-url-transport'.
;;
;; Layer rules: this is the transport layer, next to approve-api.el.
;; It knows nothing about the store or the content of buffers.

;;; Code:

(require 'cl-lib)
(require 'url-queue)
(require 'approve-log)

(defvar approve-api-url-transport #'approve-api-url-retrieve
  "Function that fetches one URL.
It is called with the URL, a function of the body (a unibyte string)
for success, and a function of an error message for failure, and
calls exactly one of them once, after it has returned.")

(defun approve-api-url--body ()
  "Return the body of the HTTP response in the current buffer, unibyte."
  (set-buffer-multibyte nil)
  (goto-char (point-min))
  (if (re-search-forward "\r?\n\r?\n" nil t)
      (buffer-substring-no-properties (point) (point-max))
    ""))

(defun approve-api-url-retrieve (url on-success on-failure)
  "Fetch URL with `url-queue-retrieve'; the default transport.
Call ON-SUCCESS with the body or ON-FAILURE with a message."
  (url-queue-retrieve
   url
   (lambda (status)
     (let ((buffer (current-buffer)))
       (unwind-protect
           (if-let* ((err (plist-get status :error)))
               (funcall on-failure (format "%s" (cdr-safe err)))
             (funcall on-success (approve-api-url--body)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))
   nil t t))

(cl-defun approve-api-fetch-url (url &key callback errorback (buffer (current-buffer)))
  "Fetch the bytes at URL asynchronously.
CALLBACK is called with the body, a unibyte string; ERRORBACK with
a message.  Both run in BUFFER, the current buffer by default, and
are skipped if it has been killed.  Failures are logged.

Example:
  (approve-api-fetch-url \"https://camo.githubusercontent.com/…\"
                         :callback (lambda (data) (message \"%d bytes\"
                                                          (length data))))"
  (cl-flet ((deliver (function value)
              (when (and function (buffer-live-p buffer))
                (with-current-buffer buffer
                  (condition-case-unless-debug err
                      (funcall function value)
                    (error (approve-log-condition
                            (format "Continuation of fetching %s" url) err)))))))
    (approve-log 'debug "Fetching %s" url)
    (funcall approve-api-url-transport url
             (lambda (body) (deliver callback body))
             (lambda (message)
               (approve-log 'warn "Fetching %s failed: %s" url message)
               (deliver errorback message)))))

(provide 'approve-api-url)

;;; approve-api-url.el ends here
