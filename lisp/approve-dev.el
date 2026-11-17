;;; approve-dev.el --- Development tools for Approve -*- lexical-binding: t; -*-

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

;; Tools for developing Approve; not part of the package.
;;
;; Fixtures (tests/fixtures/README.org describes the format): a
;; scenario is one JSON file holding a pull request's identity and the
;; exchanges that load it, each a request key and GitHub's response.
;;
;; - `approve-dev-read-scenario' and `approve-dev-write-scenario' read
;;   and write scenario files.
;; - `approve-dev-make-replay-transport' returns an
;;   `approve-api-transport' that answers from scenarios, so tests and
;;   `approve-dev-offline-mode' run without GitHub.
;; - `approve-dev-record' records a real pull request as a scenario,
;;   through `approve-dev-make-recording-transport', scrubbed of
;;   tokens and (by default) of personal data.
;;
;; Debugging: `approve-dev-inspect-query' shows a query with its
;; includes expanded; `approve-dev-model-dump' shows the store of an
;; Approve buffer.
;;
;; Layer rules: this file stands beside the layers, for developers
;; only.  It swaps `approve-api-transport' and reads the model's
;; buffer-local state to show it, but no package module may require
;; it, and it never calls ghub itself: recording goes through the
;; transport that was installed.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pp)
(require 'approve)
(require 'approve-log)
(require 'approve-graphql)
(require 'approve-api)
(require 'approve-api-paging)
(require 'approve-model)
(require 'approve-session)
(require 'approve-lines)

(defgroup approve-dev nil
  "Development tools for Approve."
  :group 'approve
  :prefix "approve-dev-")

(define-error 'approve-dev-error "Approve development tool error" 'approve-error)

(defvar approve-dev-fixtures-directory
  (expand-file-name "../tests/fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Directory that holds the scenario files, tests/fixtures/ of the source.")

(defcustom approve-dev-offline-latency 0.2
  "Seconds `approve-dev-offline-mode' waits before it answers a request.
A little latency shows loading states as they would be with GitHub.
For instant answers:

  (setq approve-dev-offline-latency 0)"
  :type 'number
  :group 'approve-dev)

;;; JSON

(defconst approve-dev-fixture-format 1
  "Version of the scenario file format this file reads and writes.")

(defun approve-dev--json-object-p (value)
  "Return non-nil if VALUE is written as a JSON object.
That is an alist whose first key is a symbol other than a keyword,
the shape ghub decodes objects to.  Other lists are arrays."
  (and (consp value)
       (consp (car value))
       (let ((key (caar value)))
         (and key (symbolp key) (not (keywordp key))))))

(defconst approve-dev--json-escape-regexp
  (rx (in "\"\\" (0 . 31)))
  "Regexp matching the characters a JSON string escapes.")

(defun approve-dev--json-string (string)
  "Return STRING as a JSON string literal."
  (concat "\""
          (replace-regexp-in-string
           approve-dev--json-escape-regexp
           (lambda (match)
             (pcase (aref match 0)
               (?\" "\\\"")
               (?\\ "\\\\")
               (?\n "\\n")
               (?\t "\\t")
               (?\r "\\r")
               (?\b "\\b")
               (?\f "\\f")
               (char (format "\\u%04x" char))))
           string t t)
          "\""))

(defun approve-dev--json-insert (value indent)
  "Insert VALUE as JSON at point, nested INDENT spaces deep.
Objects are alists with symbol keys; arrays are lists or vectors.
nil and :null are written as null, :false as false, t as true; an
empty array must be a vector ([]), an empty list is null.  Objects
and arrays put one member per line."
  (cond
   ((eq value t) (insert "true"))
   ((memq value '(nil :null)) (insert "null"))
   ((eq value :false) (insert "false"))
   ((stringp value) (insert (approve-dev--json-string value)))
   ((numberp value) (insert (number-to-string value)))
   ((approve-dev--json-object-p value)
    (approve-dev--json-insert-members
     "{" "}" value indent
     (lambda (field inner)
       (insert (approve-dev--json-string (symbol-name (car field))) ": ")
       (approve-dev--json-insert (cdr field) inner))))
   ((or (consp value) (vectorp value))
    (approve-dev--json-insert-members "[" "]" (append value nil) indent
                                      #'approve-dev--json-insert))
   (t (signal 'approve-dev-error (list "Cannot write as JSON" value)))))

(defun approve-dev--json-insert-members (open close members indent insert)
  "Insert MEMBERS between OPEN and CLOSE, one per line, INDENT deep.
INSERT is called with each member and the indentation inside."
  (if (null members)
      (insert open close)
    (let* ((inner (+ indent 2))
           (prefix (concat "\n" (make-string inner ?\s))))
      (insert open)
      (while members
        (insert prefix)
        (funcall insert (car members) inner)
        (setq members (cdr members))
        (when members
          (insert ",")))
      (insert "\n" (make-string indent ?\s) close))))

(defun approve-dev--read-json-file (file)
  "Return the JSON in FILE, decoded as ghub decodes responses.
Objects become alists with symbol keys, arrays lists, and null and
false nil."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

;;; Scenarios

(defun approve-dev-scenario-file (name &optional directory)
  "Return the file of the scenario NAME in DIRECTORY.
DIRECTORY defaults to `approve-dev-fixtures-directory'.  NAME may be
a file name already, absolute or ending in \".json\".

Example: (approve-dev-scenario-file \"medium\") => \".../medium.json\""
  (expand-file-name (if (string-suffix-p ".json" name) name (concat name ".json"))
                    (or directory approve-dev-fixtures-directory)))

(defun approve-dev-scenario-files (&optional directory)
  "Return the scenario files in DIRECTORY, sorted.
DIRECTORY defaults to `approve-dev-fixtures-directory'."
  (directory-files (or directory approve-dev-fixtures-directory) t
                   (rx ".json" eos)))

(defun approve-dev--request-from-json (request)
  "Return the request key in REQUEST, a decoded fixture request."
  (let-alist request
    (if (equal .kind "rest")
        (list :kind 'rest :method .method :resource .resource :params .params)
      (list :kind 'graphql :name .name :variables .variables))))

(defun approve-dev--request-to-json (key)
  "Return the request KEY as an alist to write."
  (if (eq (plist-get key :kind) 'rest)
      `((kind . "rest")
        (method . ,(plist-get key :method))
        (resource . ,(plist-get key :resource))
        (params . ,(plist-get key :params)))
    `((kind . "graphql")
      (name . ,(plist-get key :name))
      (variables . ,(plist-get key :variables)))))

(defun approve-dev--exchange-from-json (exchange)
  "Return the exchange plist for EXCHANGE, decoded from a fixture."
  (let-alist exchange
    (list :request (approve-dev--request-from-json .request)
          :status .response.status
          :headers (mapcar (lambda (header)
                             (cons (symbol-name (car header)) (cdr header)))
                           .response.headers)
          :body .response.body)))

(defun approve-dev--exchange-to-json (exchange)
  "Return EXCHANGE, an exchange plist, as an alist to write."
  `((request . ,(approve-dev--request-to-json (plist-get exchange :request)))
    (response
     (status . ,(plist-get exchange :status))
     (headers . ,(mapcar (lambda (header)
                           (cons (intern (car header)) (cdr header)))
                         (plist-get exchange :headers)))
     (body . ,(plist-get exchange :body)))))

(defun approve-dev-read-scenario (file)
  "Read the scenario in FILE and return it as a plist.
FILE is a file name or a scenario name (see
`approve-dev-scenario-file').  The plist has :name, :description,
:host, :owner, :repo, :number and :viewer (the pull request and who
looked at it), :file, and :exchanges, a list of plists:

  :request  the request key, as `approve-dev-request-key' makes it
  :status   the HTTP status, 200 for a success
  :headers  alist of (NAME . VALUE) strings
  :body     the decoded body, as ghub gives it: objects are alists
            with symbol keys, arrays lists, null and false nil

Example:
  (plist-get (approve-dev-read-scenario \"medium\") :number) => 202"
  (let* ((file (approve-dev-scenario-file file))
         (json (approve-dev--read-json-file file)))
    (let-alist json
      (unless (equal .approveFixture approve-dev-fixture-format)
        (signal 'approve-dev-error
                (list "Not a scenario file of format" approve-dev-fixture-format
                      file)))
      (list :name .name :description .description
            :host .pullRequest.host :owner .pullRequest.owner
            :repo .pullRequest.repo :number .pullRequest.number
            :viewer .pullRequest.viewer
            :file file
            :exchanges (mapcar #'approve-dev--exchange-from-json .exchanges)))))

(defun approve-dev-write-scenario (scenario file)
  "Write SCENARIO, a plist as `approve-dev-read-scenario' returns, to FILE.
The JSON has one member per line, so fixtures diff well.  Bodies
are written as `approve-dev--json-insert' describes: nil becomes
null, so a recording turns false and [] into null too, which reads
back as the same nil.  Return FILE."
  (let ((json `((approveFixture . ,approve-dev-fixture-format)
                (name . ,(plist-get scenario :name))
                (description . ,(plist-get scenario :description))
                (pullRequest
                 (host . ,(plist-get scenario :host))
                 (owner . ,(plist-get scenario :owner))
                 (repo . ,(plist-get scenario :repo))
                 (number . ,(plist-get scenario :number))
                 (viewer . ,(plist-get scenario :viewer)))
                (exchanges . ,(vconcat (mapcar #'approve-dev--exchange-to-json
                                               (plist-get scenario :exchanges)))))))
    (make-directory (file-name-directory (expand-file-name file)) t)
    (with-temp-buffer
      (approve-dev--json-insert json 0)
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil file nil 'silent)))
    file))

(defun approve-dev-scenario-meta (scenario)
  "Return the model meta plist of SCENARIO, for `approve-model-init'."
  (list :host (plist-get scenario :host) :owner (plist-get scenario :owner)
        :repo (plist-get scenario :repo) :number (plist-get scenario :number)
        :viewer (plist-get scenario :viewer)))

;;; Request keys

(defun approve-dev-request-key (request)
  "Return the key that identifies REQUEST, a transport request plist.
A GraphQL request is identified by its query file name and its
variables, (:kind graphql :name NAME :variables ALIST); a REST
request by (:kind rest :method METHOD :resource RESOURCE :params
ALIST).  Host, auth and payload are left out."
  (if (eq (plist-get request :kind) 'rest)
      (list :kind 'rest :method (plist-get request :method)
            :resource (plist-get request :resource)
            :params (plist-get request :params))
    (list :kind 'graphql :name (plist-get request :name)
          :variables (plist-get request :variables))))

(defun approve-dev--canonical-alist (alist)
  "Return ALIST as sorted (KEY . VALUE) strings, for comparing.
So the order of variables or parameters does not matter, nor
whether a number was given as a string."
  (sort (mapcar (lambda (pair)
                  (cons (format "%s" (car pair)) (format "%s" (cdr pair))))
                alist)
        (lambda (a b) (string< (car a) (car b)))))

(defun approve-dev--canonical-key (key)
  "Return request KEY in a form that is `equal' for equal requests."
  (if (eq (plist-get key :kind) 'rest)
      (list 'rest (upcase (or (plist-get key :method) "GET"))
            (plist-get key :resource)
            (approve-dev--canonical-alist (plist-get key :params)))
    (list 'graphql (plist-get key :name)
          (approve-dev--canonical-alist (plist-get key :variables)))))

(defun approve-dev--describe-key (key)
  "Return a short text naming the request KEY, for messages."
  (if (eq (plist-get key :kind) 'rest)
      (format "%s %s %S" (plist-get key :method) (plist-get key :resource)
              (plist-get key :params))
    (format "graphql %s %S" (or (plist-get key :name) "(query text)")
            (plist-get key :variables))))

;;; Replay

(defun approve-dev--replay-answer (request key exchange)
  "Answer REQUEST, whose key is KEY, from EXCHANGE, or fail if it is nil."
  (cond
   ((null exchange)
    (let ((text (format "approve-dev: no fixture for %s"
                        (approve-dev--describe-key key))))
      (approve-log 'warn "%s" text)
      (funcall (plist-get request :on-failure)
               (list :status 404 :payload `((message . ,text)) :headers nil))))
   ((<= 200 (plist-get exchange :status) 299)
    (funcall (plist-get request :on-success)
             (copy-tree (plist-get exchange :body))
             (copy-tree (plist-get exchange :headers))))
   (t
    (funcall (plist-get request :on-failure)
             (list :status (plist-get exchange :status)
                   :payload (copy-tree (plist-get exchange :body))
                   :headers (copy-tree (plist-get exchange :headers)))))))

(defun approve-dev-make-replay-transport (scenarios &optional latency fallback)
  "Return an `approve-api-transport' that answers from SCENARIOS.
SCENARIOS is a list of plists from `approve-dev-read-scenario'.  A
request is answered with the first exchange whose key matches its
own (`approve-dev-request-key'): the same query file or REST
resource, and the same variables or parameters in any order.  Since
those name the pull request, its owner, name and number pick the
scenario.  The same request can be answered any number of times.

The answer comes from a timer after LATENCY seconds (default 0),
never before the transport returns.  A request no exchange matches
goes to FALLBACK, another transport, when it is non-nil; else it
fails with status 404, and the log says which request it was.  The
returned abort function cancels the answer.

Example:
  (let ((approve-api-transport
         (approve-dev-make-replay-transport
          (list (approve-dev-read-scenario \"small\")))))
    ...)"
  (let ((table (make-hash-table :test #'equal)))
    (dolist (scenario scenarios)
      (dolist (exchange (plist-get scenario :exchanges))
        (let ((key (approve-dev--canonical-key (plist-get exchange :request))))
          (unless (gethash key table)
            (puthash key exchange table)))))
    (lambda (request)
      (let* ((key (approve-dev-request-key request))
             (exchange (gethash (approve-dev--canonical-key key) table)))
        (if (and fallback (null exchange))
            (funcall fallback request)
          (let ((timer (run-at-time (or latency 0) nil #'approve-dev--replay-answer
                                    request key exchange)))
            (lambda () (cancel-timer timer))))))))

;;; Offline mode

(defcustom approve-dev-offline-fallback nil
  "Whether `approve-dev-offline-mode' sends unknown requests to GitHub.
When nil, a request no fixture answers fails as not found.  When
non-nil, it goes to the transport offline mode replaced, so the
fixture PRs and real ones open side by side.  Set it before turning
the mode on:

  (setq approve-dev-offline-fallback t)"
  :type 'boolean
  :group 'approve-dev)

(defvar approve-dev--offline-transport nil
  "The replay transport `approve-dev-offline-mode' installed, or nil.")

(defvar approve-dev--transport-before-offline nil
  "The transport `approve-dev-offline-mode' replaced.")

;;;###autoload
(define-minor-mode approve-dev-offline-mode
  "Serve Approve's requests from the fixtures instead of GitHub.
When on, `approve-api-transport' answers from every scenario in
`approve-dev-fixtures-directory', after `approve-dev-offline-latency'
seconds; the owner, name and number of a request pick the scenario.
A request no fixture answers fails as not found, or goes to GitHub
when `approve-dev-offline-fallback' is non-nil.  Turning the mode
off restores the transport it replaced."
  :global t
  :group 'approve-dev
  :lighter " Approve-Offline"
  (if approve-dev-offline-mode
      (let ((scenarios (mapcar #'approve-dev-read-scenario
                               (approve-dev-scenario-files))))
        (unless (and approve-dev--offline-transport
                     (eq approve-api-transport approve-dev--offline-transport))
          (setq approve-dev--transport-before-offline approve-api-transport))
        (setq approve-dev--offline-transport
              (approve-dev-make-replay-transport
               scenarios approve-dev-offline-latency
               (and approve-dev-offline-fallback
                    (or approve-dev--transport-before-offline
                        #'approve-api-ghub-transport)))
              approve-api-transport approve-dev--offline-transport)
        (message "Approve offline: %s"
                 (mapconcat (lambda (scenario)
                              (format "%s (%s/%s#%s)"
                                      (plist-get scenario :name)
                                      (plist-get scenario :owner)
                                      (plist-get scenario :repo)
                                      (plist-get scenario :number)))
                            scenarios ", ")))
    (when (eq approve-api-transport approve-dev--offline-transport)
      (setq approve-api-transport (or approve-dev--transport-before-offline
                                      #'approve-api-ghub-transport)))
    (setq approve-dev--offline-transport nil
          approve-dev--transport-before-offline nil)))

;;; Trying Approve on the fixtures

(defconst approve-dev-try-buffer-name "*Approve fixtures*"
  "Name of the start page of `approve-dev-try'.")

(defun approve-dev-scenario-url (scenario)
  "Return the pull request URL of SCENARIO."
  (format "https://%s/%s/%s/pull/%s" (or (plist-get scenario :host) "github.com")
          (plist-get scenario :owner) (plist-get scenario :repo)
          (plist-get scenario :number)))

(defun approve-dev--try-open (button)
  "Open the pull request of BUTTON, a start page entry."
  (approve (button-get button 'approve-url)))

;;;###autoload
(defun approve-dev-try ()
  "Try Approve on the fixtures: replay them and list their pull requests.
Turn `approve-dev-offline-mode' on and show a start page with a
button for each scenario in `approve-dev-fixtures-directory';
\\`RET' on one opens it.  Return the start page."
  (interactive)
  (approve-dev-offline-mode 1)
  (let ((buffer (get-buffer-create approve-dev-try-buffer-name)))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Approve on the fixtures" 'face 'approve-title) "\n\n")
        (insert "Requests are answered from tests/fixtures/, so no token or\n"
                "network is needed (images in comments still load from the web).\n"
                "RET on a pull request opens it.\n\n")
        (dolist (scenario (mapcar #'approve-dev-read-scenario
                                  (approve-dev-scenario-files)))
          (let ((url (approve-dev-scenario-url scenario)))
            (insert "  ")
            (insert-text-button (format "%-8s %s/%s#%s" (plist-get scenario :name)
                                        (plist-get scenario :owner)
                                        (plist-get scenario :repo)
                                        (plist-get scenario :number))
                                'action #'approve-dev--try-open
                                'approve-url url
                                'help-echo url)
            (insert "\n    " (propertize (or (plist-get scenario :description) "")
                                       'face 'approve-dim)
                    "\n\n")))
        (insert (propertize "In a pull request" 'face 'approve-heading) "\n"
                "  TAB    expand or collapse        RET  follow the thing at point\n"
                "  ] u    next unviewed file        ] t  next unresolved thread\n"
                "  j      jump menu                 K    details (eldoc buffer)\n"
                "  o      open on GitHub            y    copy path:line, sha, login\n"
                "  g      refresh                   q    bury\n"))
      (goto-char (point-min))
      (forward-button 1))
    (pop-to-buffer-same-window buffer)
    buffer))

;;; Recording

(defconst approve-dev--recorded-header-regexp
  (rx bos (or "x-ratelimit-" "link" "retry-after"))
  "Regexp matching the names of the response headers a recording keeps.")

(defun approve-dev--recorded-headers (headers)
  "Return the HEADERS a recording keeps: rate limits, Link and Retry-After.
Everything else, Authorization and cookies included, is dropped, and
the values kept are redacted with `approve-log-redact'."
  (let ((case-fold-search t))
    (cl-loop for (name . value) in headers
             when (and (stringp name) (stringp value)
                       (string-match-p approve-dev--recorded-header-regexp name))
             collect (cons name (approve-log-redact value)))))

(defun approve-dev-make-recording-transport (inner record)
  "Return an `approve-api-transport' that records what INNER does.
INNER is the transport that does the work, such as
`approve-api-ghub-transport'.  After each response, RECORD is called
with the exchange, a plist as in `approve-dev-read-scenario', before
the request's own continuation runs.  Headers are filtered with
`approve-dev--recorded-headers'.  Network errors, which have no
response, are not recorded.  An error in RECORD is logged."
  (lambda (request)
    (let ((key (approve-dev-request-key request))
          (on-success (plist-get request :on-success))
          (on-failure (plist-get request :on-failure)))
      (funcall inner
               (append
                (list :on-success
                      (lambda (value headers)
                        (approve-dev--record record key 200 headers value)
                        (funcall on-success value headers))
                      :on-failure
                      (lambda (failure)
                        (when-let* ((status (plist-get failure :status)))
                          (approve-dev--record record key status
                                               (plist-get failure :headers)
                                               (plist-get failure :payload)))
                        (funcall on-failure failure)))
                request)))))

(defun approve-dev--record (record key status headers body)
  "Call RECORD with the exchange of KEY, STATUS, HEADERS and BODY."
  (condition-case err
      (funcall record (list :request key :status status
                            :headers (approve-dev--recorded-headers headers)
                            :body body))
    (error (approve-log-condition "Recording an exchange" err))))

(defun approve-dev--plist-with (plist key value)
  "Return a copy of PLIST with KEY set to VALUE."
  (plist-put (copy-sequence plist) key value))

(defun approve-dev--scrubber ()
  "Return a function that maps a login or email to a stable fake.
It is called with KIND (`login' or `email') and the real value and
returns the fake's number: the same value gets the same number."
  (let ((seen (make-hash-table :test #'equal))
        (counts (list (cons 'login 0) (cons 'email 0))))
    (lambda (kind value)
      (or (gethash (cons kind value) seen)
          (puthash (cons kind value)
                   (setf (alist-get kind counts) (1+ (alist-get kind counts)))
                   seen)))))

(defun approve-dev--scrub-object (object fake keep)
  "Return OBJECT with the personal data of the person it is replaced.
A User or Mannequin (it has a `login' not in KEEP) gets the login
user-N, the name \"User N\" and a URL to match; a Git author (it has
an `email') gets person-N, \"Person N\" and person-N@example.com.
FAKE is a function from `approve-dev--scrubber'."
  (let ((type (alist-get '__typename object))
        (login (alist-get 'login object))
        (email (alist-get 'email object)))
    (cond
     ((and (stringp login) (member type '("User" "Mannequin"))
           (not (member login keep)))
      (let* ((n (funcall fake 'login login))
             (new (format "user-%d" n)))
        (mapcar (lambda (field)
                  (pcase (car field)
                    ('login (cons 'login new))
                    ('name (cons 'name (and (cdr field) (format "User %d" n))))
                    ('url (cons 'url
                                (and (stringp (cdr field))
                                     (replace-regexp-in-string
                                      (concat "/" (regexp-quote login) "\\'")
                                      (concat "/" new) (cdr field) t t))))
                    (_ field)))
                object)))
     ((stringp email)
      (let ((n (funcall fake 'email email)))
        (mapcar (lambda (field)
                  (pcase (car field)
                    ('email (cons 'email (format "person-%d@example.com" n)))
                    ('name (cons 'name (format "Person %d" n)))
                    (_ field)))
                object)))
     (t object))))

(defun approve-dev--scrub (value fake keep)
  "Return VALUE with personal data replaced; FAKE and KEEP as for objects."
  (cond
   ((approve-dev--json-object-p value)
    (approve-dev--scrub-object
     (mapcar (lambda (field)
               (cons (car field) (approve-dev--scrub (cdr field) fake keep)))
             value)
     fake keep))
   ((consp value)
    (mapcar (lambda (item) (approve-dev--scrub item fake keep)) value))
   (t value)))

(defun approve-dev-scrub-people (scenario)
  "Return SCENARIO with logins, names and emails of people replaced.
Users and mannequins become user-N (\"User N\"), Git authors
person-N (\"Person N\", person-N@example.com), numbered in the order
they first appear, so one person keeps one fake.  The viewer is
renamed the same way.  The repository owner keeps its login, so
names such as owner/repo stay whole.  Bots and organizations are not
people and stay.  Text that mentions a login (a body with @login) is
not changed."
  (let* ((fake (approve-dev--scrubber))
         (keep (list (plist-get scenario :owner)))
         (viewer (plist-get scenario :viewer))
         (scenario (approve-dev--plist-with
                    scenario :viewer
                    (and viewer (format "user-%d" (funcall fake 'login viewer))))))
    (approve-dev--plist-with
     scenario :exchanges
     (mapcar (lambda (exchange)
               (approve-dev--plist-with
                exchange :body
                (approve-dev--scrub (plist-get exchange :body) fake keep)))
             (plist-get scenario :exchanges)))))

(defconst approve-dev--loading-plan
  '(("queries/pull-request.graphql")
    ("queries/pull-request-files.graphql" repository pullRequest files)
    ("queries/pull-request-threads.graphql" repository pullRequest reviewThreads)
    ("queries/pull-request-timeline.graphql" repository pullRequest timelineItems)
    ("queries/pull-request-commits.graphql" repository pullRequest commits)
    ("queries/pull-request-checks.graphql"
     repository pullRequest statusCheckRollup contexts))
  "The queries `approve-dev-record' runs, in the order it writes them.
Each entry is (QUERY . PATH): PATH leads from the data to the list the
query pages, nil for the core query.  The REST files list comes after
them.")

(defun approve-dev--plan-rank (exchange)
  "Return the position of EXCHANGE's request in the loading plan."
  (let ((key (plist-get exchange :request)))
    (or (and (eq (plist-get key :kind) 'graphql)
             (seq-position (mapcar #'car approve-dev--loading-plan)
                           (plist-get key :name)))
        (length approve-dev--loading-plan))))

;;;###autoload
(cl-defun approve-dev-record (url name &key (scrub #'approve-dev-scrub-people)
                                  directory callback)
  "Record the pull request at URL as the scenario NAME.
Run the loading plan through the installed transport, wrapped by
`approve-dev-make-recording-transport': the queries in
`approve-dev--loading-plan', each list with all its pages, and the
REST files list with all its pages.  When every request has ended,
write the scenario to NAME in DIRECTORY, by default
`approve-dev-fixtures-directory', and call CALLBACK, if non-nil, with
the file name.

The file never holds tokens: request keys have no headers and only
rate-limit, Link and Retry-After response headers are kept.  SCRUB
is a function of the scenario that returns it cleaned of personal
data, `approve-dev-scrub-people' by default; nil keeps it as it is.
Interactively, a prefix argument keeps personal data.

While recording, `approve-api-transport' is the recording one, so
other Approve requests made meanwhile are recorded too.

Example:
  (approve-dev-record \"https://github.com/magit/magit/pull/5000\" \"magit\")"
  (interactive
   (list (read-string "Pull request URL: ")
         (read-string "Scenario name: ")
         :scrub (unless current-prefix-arg #'approve-dev-scrub-people)))
  (let* ((pr (or (approve-parse-url url)
                 (user-error "Not a pull request URL: %s" url)))
         (owner (plist-get pr :owner))
         (repo (plist-get pr :repo))
         (number (plist-get pr :number))
         (host (approve-api-host-from-web-host (plist-get pr :host)))
         (file (approve-dev-scenario-file name directory))
         (variables `((owner . ,owner) (name . ,repo) (number . ,number)))
         (inner approve-api-transport)
         (buffer (generate-new-buffer " *approve-dev-record*"))
         (exchanges nil)
         (failures nil)
         (viewer nil)
         (pending (1+ (length approve-dev--loading-plan))))
    (cl-labels
        ((finish ()
           (setq pending (1- pending))
           (when (zerop pending)
             (setq approve-api-transport inner)
             (kill-buffer buffer)
             (let ((scenario
                    (list :name name
                          :description (format "Recorded from %s on %s" url
                                               (format-time-string "%F"))
                          :host (plist-get pr :host) :owner owner :repo repo
                          :number number :viewer viewer
                          :exchanges (sort (nreverse exchanges)
                                           (lambda (a b)
                                             (< (approve-dev--plan-rank a)
                                                (approve-dev--plan-rank b)))))))
               (approve-dev-write-scenario (if scrub (funcall scrub scenario) scenario)
                                           file)
               (message "Approve: recorded %s to %s%s" url file
                        (if failures
                            (format " (%d requests failed, see %s)"
                                    (length failures) approve-log-buffer-name)
                          ""))
               (when callback
                 (funcall callback file)))))
         (fail (err)
           (push err failures)
           (approve-log 'warn "Recording %s: %s" url (approve-api-error-message err))
           (finish)))
      (setq approve-api-transport
            (approve-dev-make-recording-transport
             inner (lambda (exchange) (push exchange exchanges))))
      (with-current-buffer buffer
        (approve-api-graphql (caar approve-dev--loading-plan) variables
                             :host host :errorback #'fail
                             :callback (lambda (data)
                                         (setq viewer (let-alist data .viewer.login))
                                         (finish)))
        (pcase-dolist (`(,query . ,path) (cdr approve-dev--loading-plan))
          (approve-api-graphql-all query variables :path path
                                   :host host :errorback #'fail
                                   :callback #'finish))
        (approve-api-rest-all (format "/repos/%s/%s/pulls/%d/files" owner repo number)
                              :host host :errorback #'fail
                              :callback #'finish)))
    file))

;;; Benchmarks

(defconst approve-dev-speed-budgets
  '((redraw-medium . 30) (expand-file . 30) (redraw-large . 100)
    (lines-busy . 10) (lines-map . 50))
  "Speed budgets in milliseconds (docs/architecture.org, \"Speed\").")

(defun approve-dev--open-fixture (name)
  "Return a PR buffer of the scenario NAME, loaded from the fixtures.
The fixtures answer without latency; the buffer is hidden."
  (let* ((scenario (approve-dev-read-scenario name))
         (approve-api-transport (approve-dev-make-replay-transport (list scenario)))
         (approve-api-timeout nil)
         (inhibit-message t)
         (buffer (approve-pr-setup-buffer
                  (list :host "github.com" :owner (plist-get scenario :owner)
                        :repo (plist-get scenario :repo)
                        :number (plist-get scenario :number))))
         (deadline (+ (float-time) 30)))
    (with-current-buffer buffer
      (approve-pr-load)
      (while (and (approve-pr-loading-p) (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (approve-pr-redraw))
    buffer))

(defun approve-dev--time (runs function)
  "Return the mean milliseconds of RUNS calls of FUNCTION, after one warm-up.
Garbage collection is included, as a user would feel it."
  (funcall function)
  (let ((start (float-time)))
    (dotimes (_ runs)
      (funcall function))
    (/ (* 1000 (- (float-time) start)) runs)))

(defun approve-dev--synthetic-patch (lines)
  "Return a patch of about LINES lines in hunks of 50, and thread lines.
The value is (PATCH . LINES-WITH-THREADS): new-side line numbers of
added lines, one per hunk of the first five."
  (let ((hunks nil) (threads nil) (old 1) (new 1))
    (dotimes (h (/ lines 50))
      (let ((body nil) (start-old old) (start-new new) (old-count 0) (new-count 0))
        (dotimes (i 50)
          (pcase (% i 5)
            (0 (push (format "-\told line %d of the hunk %d" i h) body)
               (cl-incf old) (cl-incf old-count))
            (1 (push (format "+\tnew line %d of the hunk %d" i h) body)
               (when (and (= i 21) (< h 5)) (push new threads))
               (cl-incf new) (cl-incf new-count))
            (_ (push (format " \tcontext line %d of the hunk %d" i h) body)
               (cl-incf old) (cl-incf new) (cl-incf old-count) (cl-incf new-count))))
        (push (concat (format "@@ -%d,%d +%d,%d @@ func hunk%d() {\n"
                              start-old old-count start-new new-count h)
                      (string-join (nreverse body) "\n"))
              hunks)
        (cl-incf old 20) (cl-incf new 20)))
    (cons (string-join (nreverse hunks) "\n") (nreverse threads))))

(defun approve-dev--file-section (path)
  "Return the diff section of PATH in the current PR buffer."
  (seq-find (lambda (section) (equal (oref section value) path))
            (oref (seq-find (lambda (section)
                              (eq (oref section type) 'approve-diff-section))
                            (oref magit-root-section children))
                  children)))

(defconst approve-dev--expand-path "internal/parser/parser.go"
  "The medium PR's file the expand benchmark draws.")

(defun approve-dev--prepare-expand (buffer)
  "Give the medium PR in BUFFER a 500-line file with 5 threads.
Its parser.go gets a synthetic 500-line patch, and five of its
threads move onto lines of it."
  (with-current-buffer buffer
    (let* ((path approve-dev--expand-path)
           (synthetic (approve-dev--synthetic-patch 500))
           (table (approve-model-extra :patches))
           (threads (seq-take (seq-filter (lambda (thread)
                                            (equal (alist-get 'path thread) path))
                                          (approve-pr-list-fields
                                           'reviewThreads '(id path)))
                              5)))
      (puthash path (cons (cons 'patch (car synthetic))
                          (assq-delete-all 'patch (copy-alist (gethash path table))))
               table)
      (cl-loop for thread in threads
               for line in (cdr synthetic)
               do (approve-model-patch
                   `((__typename . "PullRequestReviewThread") (id . ,(alist-get 'id thread))
                     (line . ,line) (diffSide . "RIGHT") (subjectType . "LINE")
                     (isOutdated)))))))

(defun approve-dev--expand-once (buffer)
  "Return the seconds it takes to expand the prepared file in BUFFER.
The file is collapsed and the buffer redrawn first, so its body is
drawn anew."
  (with-current-buffer buffer
    (magit-section-hide (approve-dev--file-section approve-dev--expand-path))
    (approve-pr-redraw)
    (let ((section (approve-dev--file-section approve-dev--expand-path))
          (start (float-time)))
      (magit-section-show section)
      (- (float-time) start))))

(defun approve-dev--source-text (lines)
  "Return a text of LINES lines, each as long as a line of code."
  (with-temp-buffer
    (dotimes (i lines)
      (insert (format "    total_%d = combine(total_%d, values[%d]); // step %d\n"
                      (1+ i) i (* 7 i) i)))
    (buffer-string)))

(defun approve-dev--wait-until (predicate)
  "Wait for PREDICATE to return non-nil; signal an error after 30 seconds."
  (let ((deadline (+ (float-time) 30)))
    (while (not (funcall predicate))
      (when (> (float-time) deadline)
        (error "The benchmark waited too long"))
      (accept-process-output nil 0.001))))

(defun approve-dev--time-lines (runs)
  "Return the milliseconds a 200 000-line buffer of a clone takes after an edit.
The value is (BUSY MAP), the means of RUNS edits: how long Emacs is
busy comparing the buffer with GitHub's text, which has three lines
edited (`approve-lines-update'), and how long until the map arrives."
  (let* ((text (approve-dev--source-text 200000))
         (github (with-temp-buffer
                   (insert text)
                   (dolist (line '(50000 100000 150000))
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (insert "    // edited on GitHub\n"))
                   (buffer-string)))
         (maps 0) (busy 0) (answered 0))
    (with-temp-buffer
      (insert text)
      (unwind-protect
          (progn
            (approve-lines-watch 'benchmark github :callback (lambda (_) (cl-incf maps)))
            (approve-dev--wait-until (lambda () (= maps 1)))
            (dotimes (run runs)
              (goto-char (point-min))
              (forward-line (* (1+ run) 1000))
              (insert "    // edited here\n")
              (let ((start (float-time)))
                (approve-lines-update)
                (setq busy (+ busy (- (float-time) start)))
                (approve-dev--wait-until (lambda () (= maps (+ run 2))))
                (setq answered (+ answered (- (float-time) start))))))
        (approve-lines-unwatch)))
    (list (/ (* 1000 busy) runs) (/ (* 1000 answered) runs))))

;;;###autoload
(defun approve-dev-benchmark (&optional runs)
  "Measure Approve against its speed budgets with the fixtures.
Each case is timed RUNS times (default 10) after a warm-up:
redrawing the medium PR with the diff collapsed, expanding a
500-line file with 5 threads, redrawing the large PR, and comparing
a 200 000-line buffer with GitHub's text after an edit (Emacs busy,
and the map).  Return an alist of (CASE MILLISECONDS BUDGET), and
show it."
  (interactive)
  (let* ((runs (or runs 10))
         (medium (approve-dev--open-fixture "medium"))
         (large (approve-dev--open-fixture "large"))
         (results
          (unwind-protect
              (list (list 'redraw-medium
                          (with-current-buffer medium
                            (approve-dev--time runs #'approve-pr-redraw)))
                    (list 'redraw-large
                          (with-current-buffer large
                            (approve-dev--time runs #'approve-pr-redraw)))
                    (list 'expand-file
                          (progn
                            (approve-dev--prepare-expand medium)
                            (approve-dev--expand-once medium)
                            (let ((total 0))
                              (dotimes (_ runs)
                                (setq total (+ total (approve-dev--expand-once medium))))
                              (/ (* 1000 total) runs)))))
            (kill-buffer medium)
            (kill-buffer large)))
         (lines (approve-dev--time-lines runs)))
    (setq results (append results (list (list 'lines-busy (nth 0 lines))
                                        (list 'lines-map (nth 1 lines)))))
    (setq results (mapcar (lambda (result)
                            (append result
                                    (list (alist-get (car result)
                                                     approve-dev-speed-budgets))))
                          results))
    (message "%s" (mapconcat (lambda (result)
                               (format "%s %.1f ms (budget %d)" (nth 0 result)
                                       (nth 1 result) (nth 2 result)))
                             results ", "))
    results))

;;; Inspecting

(defun approve-dev--graphql-files ()
  "Return the .graphql files under `approve-graphql-directory', relative."
  (sort (mapcar (lambda (file) (file-relative-name file approve-graphql-directory))
                (directory-files-recursively approve-graphql-directory
                                             (rx ".graphql" eos)))
        #'string<))

(defun approve-dev--show (name text)
  "Show TEXT in the read-only buffer NAME and return the buffer."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun approve-dev-inspect-query (name)
  "Show the GraphQL file NAME with its includes expanded.
NAME is relative to `approve-graphql-directory', such as
\"queries/pull-request.graphql\".  This is the text Approve sends;
paste it into a GraphQL client to try it.  Return the buffer."
  (interactive
   (list (completing-read "GraphQL file: " (approve-dev--graphql-files) nil t)))
  (approve-dev--show (format "*approve-dev %s*" name) (approve-graphql-load name)))

(defun approve-dev--extra-summary (extra)
  "Return the plist EXTRA with large values replaced by their size."
  (cl-loop for (key value) on extra by #'cddr
           append (list key (cond ((hash-table-p value)
                                   (format "<%d entries>" (hash-table-count value)))
                                  ((and (consp value) (> (length value) 20))
                                   (format "<%d entries>" (length value)))
                                  (t value)))))

;;;###autoload
(defun approve-dev-model-dump (&optional buffer)
  "Show the store of BUFFER, the current buffer by default.
The dump lists the root, meta data, extra data (large values as
their size), session state, and every entity by type and id, with
refs as they are stored.  Return the dump buffer."
  (interactive)
  (let* ((source (or buffer (current-buffer)))
         (owner (or (buffer-local-value 'approve-model-store-buffer source) source))
         (store (buffer-local-value 'approve-model--store owner)))
    (unless (hash-table-p store)
      (user-error "No Approve store in %s" (buffer-name source)))
    (let ((types (sort (hash-table-keys store) #'string<)))
      (approve-dev--show
       (format "*approve-dev store %s*" (buffer-name owner))
       (with-temp-buffer
         (insert (format ";; Store of %s\n" (buffer-name owner))
                 (format ";; Root: %S\n" (buffer-local-value 'approve-model--root owner))
                 (format ";; Meta: %S\n" (buffer-local-value 'approve-model--meta owner))
                 (format ";; Extra: %S\n"
                         (approve-dev--extra-summary
                          (buffer-local-value 'approve-model--extra owner)))
                 (format ";; Session: %S\n" (buffer-local-value 'approve-session owner))
                 (format ";; %d entities of %d types\n"
                         (apply #'+ (mapcar (lambda (type)
                                              (hash-table-count (gethash type store)))
                                            types))
                         (length types)))
         (dolist (type types)
           (let ((table (gethash type store)))
             (insert (format "\n;;; %s (%d)\n" type (hash-table-count table)))
             (dolist (id (sort (hash-table-keys table) #'string<))
               (insert (format "\n;; %s\n" id) (pp-to-string (gethash id table))))))
         (buffer-string))))))

(provide 'approve-dev)

;;; approve-dev.el ends here
