;;; approve-api-paging.el --- Paged GitHub requests for Approve -*- lexical-binding: t; -*-

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

;; Paged lists come in several requests: `approve-api-graphql-all'
;; follows a GraphQL connection through its pageInfo.endCursor, and
;; `approve-api-rest-all' asks a REST list page after page.  Each page
;; is handed to the caller as it arrives, so long lists render
;; progressively (docs/architecture.org, "Loading a PR").
;;
;; This file belongs to the transport layer with approve-api.el, whose
;; requests it makes; like it, it knows nothing about the store, the
;; content of buffers or rendering.

;;; Code:

(require 'approve-api)

;;; Paging

(defun approve-api-paging--connection (data path)
  "Return the paged connection at PATH in DATA.
PATH is a list of keys; an integer key is a position in a list.
Signal `approve-api-error' unless PATH ends at an object with
pageInfo, so that a wrong path never looks like the last page."
  (let ((value data))
    (dolist (key path)
      (setq value (if (integerp key) (nth key value) (alist-get key value))))
    (unless (and (consp value) (assq 'pageInfo value))
      (signal 'approve-api-error
              (list (format "No paged connection (with pageInfo) at %S" path))))
    value))

(defun approve-api-paging--stop (label errorback err)
  "End the paging of LABEL after ERR, an error in its page handling.
Log ERR and give it to ERRORBACK, or to `approve-api-default-errorback'
when there is none, as an `approve-api-error'."
  (approve-log-condition (format "Paging %s" label) err)
  (funcall (or errorback #'approve-api-default-errorback)
           (approve-api--condition 'approve-api-error
                                   (approve-api-error-message err))))

(cl-defun approve-api-graphql-all (query variables
                                         &key path on-page callback errorback
                                         (buffer (current-buffer))
                                         (timeout approve-api-timeout)
                                         (progress approve-api-default-progress)
                                         host max-pages)
  "Run the paged GraphQL QUERY with VARIABLES, page after page.
PATH lists the keys from the data to the paged connection, such as
\(repository pullRequest files); an integer is a position in a list,
as in (repository pullRequest commits nodes 0 commit ...).  The
connection selects pageInfo { hasNextPage endCursor }.  QUERY takes
the cursor in its $after variable, which is left out for the first
page.

ON-PAGE is called with each page's data and its number, from 1, as
it arrives.  CALLBACK is called with no arguments after the last
page: when hasNextPage is false or after MAX-PAGES pages (nil for no
limit).  Otherwise ERRORBACK is called once, with the error: a failed
request, a PATH that does not end at a connection with pageInfo, or
an error signalled by ON-PAGE (which is logged too).  So the paging
ends with exactly one of CALLBACK and ERRORBACK.  BUFFER, TIMEOUT,
PROGRESS and HOST are as for `approve-api-graphql'.

Example:
  (approve-api-graphql-all \"queries/pull-request-files.graphql\"
                           \\='((owner . \"o\") (name . \"r\") (number . 1))
                           :path \\='(repository pullRequest files)
                           :on-page (lambda (data page) (message \"%d\" page)))"
  (let ((variables (assq-delete-all 'after (copy-sequence variables))))
    (letrec ((fetch
              (lambda (page cursor)
                (approve-api-graphql
                 query (if cursor `((after . ,cursor) ,@variables) variables)
                 :errorback errorback :buffer buffer :timeout timeout
                 :progress progress :host host
                 :callback
                 (lambda (data)
                   (let ((next
                          (condition-case-unless-debug err
                              (progn
                                (when on-page
                                  (funcall on-page data page))
                                (let-alist (approve-api-paging--connection
                                            data path)
                                  (if (and .pageInfo.hasNextPage
                                           .pageInfo.endCursor
                                           (not (and max-pages
                                                     (>= page max-pages))))
                                      .pageInfo.endCursor
                                    'last)))
                            (error
                             (approve-api-paging--stop query errorback err)
                             nil))))
                     (cond ((eq next 'last)
                            (when callback
                              (funcall callback)))
                           (next
                            (funcall fetch (1+ page) next)))))))))
      (funcall fetch 1 nil))))

(cl-defun approve-api-rest-all (resource
                                &key params (per-page 100) (max-pages 30)
                                on-page callback errorback
                                (buffer (current-buffer))
                                (timeout approve-api-timeout)
                                (progress approve-api-default-progress)
                                host)
  "GET the paged REST list RESOURCE, page after page.
PARAMS is an alist of query parameters, to which per_page (PER-PAGE)
and page (from 1) are added.  ON-PAGE is called with each page's
list and its number as it arrives.  CALLBACK is called with no
arguments after the last page: one shorter than PER-PAGE, or page
MAX-PAGES.  The defaults fit the pull request files list: 100 per
page and at most 30 pages (3000 files).  Otherwise ERRORBACK is
called once, with the error of a failed request or one signalled by
ON-PAGE (which is logged too).  BUFFER, TIMEOUT, PROGRESS and HOST
are as for `approve-api-graphql'.

Example:
  (approve-api-rest-all \"/repos/magit/magit/pulls/5000/files\"
                        :on-page (lambda (files page) (message \"%d\" page)))"
  (letrec ((fetch
            (lambda (page)
              (approve-api-rest
               "GET" resource
               :params `((per_page . ,per-page) (page . ,page) ,@params)
               :errorback errorback :buffer buffer :timeout timeout
               :progress progress :host host
               :callback
               (lambda (value)
                 (let ((more
                        (condition-case-unless-debug err
                            (progn
                              (when on-page
                                (funcall on-page value page))
                              (if (and (>= (length value) per-page)
                                       (not (and max-pages (>= page max-pages))))
                                  'more
                                'last))
                          (error
                           (approve-api-paging--stop resource errorback err)
                           nil))))
                   (cond ((eq more 'more) (funcall fetch (1+ page)))
                         ((eq more 'last)
                          (when callback
                            (funcall callback))))))))))
    (funcall fetch 1)))

(provide 'approve-api-paging)

;;; approve-api-paging.el ends here
