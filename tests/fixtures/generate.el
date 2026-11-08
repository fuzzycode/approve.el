;;; generate.el --- Build the synthetic fixture scenarios -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds the scenarios small, medium and large as data and writes
;; them in the recorder's format (README.org), so recordings of real
;; pull requests can replace them without touching the tests.  Run it
;; from the repository root:
;;
;;   cask exec emacs -Q --batch -L lisp -l tests/fixtures/generate.el \
;;     -f approve-fixtures-generate
;;
;; The output depends on nothing but this file: ids, oids and times are
;; derived from fixed seeds, so running it again gives identical files.
;;
;; Bodies mirror GitHub's responses to the queries in graphql/queries/
;; field by field, in selection order.  Values: nil is null, :false is
;; false, t is true, [] an empty array (see `approve-dev--json-insert').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'approve-dev)

(defconst approve-fixtures-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory the scenarios are written to.")

;;; Scenario state

(defvar approve-fixtures--scenario nil
  "Plist of the scenario being built: :name :owner :repo :number :viewer
:author :maintainer (non-nil when the viewer can write to the repository).")

(defvar approve-fixtures--counter 0
  "Counter for database ids and ordering inside the scenario being built.")

(defvar approve-fixtures--reviews nil
  "Reviews of the scenario being built, `approve-fixtures--review' structs.")

(defvar approve-fixtures--files nil
  "Files of the scenario being built, `approve-fixtures--file' structs.")

(defvar approve-fixtures--rate-remaining 5000
  "The rate limit left, counted down by each exchange.")

(defun approve-fixtures--get (key)
  "Return KEY of the scenario being built."
  (plist-get approve-fixtures--scenario key))

(defun approve-fixtures--next ()
  "Return the next number of the scenario being built."
  (setq approve-fixtures--counter (1+ approve-fixtures--counter)))

;;; Values

(defun approve-fixtures--bool (value)
  "Return VALUE as a JSON boolean: t or :false."
  (if value t :false))

(defun approve-fixtures--array (list)
  "Return LIST as a JSON array: LIST itself, or [] when it is empty."
  (or list []))

(defun approve-fixtures--hash (seed)
  "Return the SHA-1 of SEED in hex: an oid."
  (secure-hash 'sha1 seed))

(defun approve-fixtures--id (prefix seed &optional kind)
  "Return a node id with PREFIX for SEED, shaped like GitHub's.
KIND is the part after the underscore, \"kwDO\" by default (users
have \"kgDO\")."
  (concat prefix "_" (or kind "kwDO")
          (substring (base64url-encode-string
                      (secure-hash 'sha1 seed nil nil t) t)
                     0 14)))

(defun approve-fixtures--node-id (prefix seed)
  "Return a node id with PREFIX for SEED within the current scenario."
  (approve-fixtures--id prefix (format "%s/%s" (approve-fixtures--get :name) seed)))

(defconst approve-fixtures--epoch (encode-time (list 0 0 9 1 10 2026 nil nil t))
  "Time zero of the scenarios: 2026-10-01 09:00 UTC.")

(defun approve-fixtures--time (minutes &optional git)
  "Return the time MINUTES after the epoch, as GitHub writes it.
With GIT, write it as a Git author date (with a zone offset)."
  (format-time-string (if git "%Y-%m-%dT%H:%M:%S+00:00" "%Y-%m-%dT%H:%M:%SZ")
                      (time-add approve-fixtures--epoch (round (* 60 minutes)))
                      t))

(defun approve-fixtures--cursor (index)
  "Return a GraphQL cursor for the item at INDEX."
  (base64-encode-string (format "cursor:v2:%d" index) t))

(defun approve-fixtures--pr-url (&rest more)
  "Return the URL of the pull request with MORE strings appended."
  (apply #'concat
         (format "https://github.com/%s/%s/pull/%d" (approve-fixtures--get :owner)
                 (approve-fixtures--get :repo) (approve-fixtures--get :number))
         more))

;;; People

(defconst approve-fixtures--people
  '((alice "alice-example" "Alice Example" "User" "MEMBER")
    (bob "bob-example" "Bob Example" "User" "COLLABORATOR")
    (carol "carol-example" "Carol Example" "User" "CONTRIBUTOR")
    (dave "dave-example" "Dave Example" "User" "MEMBER")
    (vera "vera-example" "Vera Example" "User" "MEMBER")
    (ci "fixture-ci" nil "Bot" "NONE"))
  "People of the scenarios: (KEY LOGIN NAME TYPENAME ASSOCIATION).
vera is the viewer.")

(defun approve-fixtures--person (who)
  "Return the entry of WHO in `approve-fixtures--people'."
  (or (assq who approve-fixtures--people) (error "Unknown person %s" who)))

(defun approve-fixtures--login (who)
  "Return the login of WHO."
  (nth 1 (approve-fixtures--person who)))

(defun approve-fixtures--association (who)
  "Return the author association of WHO with the repository."
  (nth 4 (approve-fixtures--person who)))

(defun approve-fixtures--user-id (who)
  "Return the node id of WHO, the same in every scenario."
  (if (equal (nth 3 (approve-fixtures--person who)) "Bot")
      (approve-fixtures--id "BOT" (symbol-name who) "kgDO")
    (approve-fixtures--id "U" (symbol-name who) "kgDO")))

(defun approve-fixtures--user-url (who)
  "Return the profile URL of WHO."
  (if (equal (nth 3 (approve-fixtures--person who)) "Bot")
      (concat "https://github.com/apps/" (approve-fixtures--login who))
    (concat "https://github.com/" (approve-fixtures--login who))))

(defun approve-fixtures--actor (who)
  "Return WHO as ActorFields select it."
  (pcase-let ((`(,_ ,login ,name ,type ,_) (approve-fixtures--person who)))
    `((__typename . ,type)
      (id . ,(approve-fixtures--user-id who))
      (login . ,login)
      (url . ,(approve-fixtures--user-url who))
      ,@(and (equal type "User") `((name . ,name))))))

(defun approve-fixtures--user (who)
  "Return WHO as UserFields select it."
  (pcase-let ((`(,_ ,login ,name ,_ ,_) (approve-fixtures--person who)))
    `((__typename . "User")
      (id . ,(approve-fixtures--user-id who))
      (login . ,login)
      (name . ,name)
      (url . ,(approve-fixtures--user-url who)))))

(defun approve-fixtures--viewer-p (who)
  "Return non-nil if WHO is the viewer."
  (eq who (approve-fixtures--get :viewer)))

(defun approve-fixtures--organization ()
  "Return the repository owner, an organization, as ActorFields select it."
  `((__typename . "Organization")
    (id . ,(approve-fixtures--id "O" (approve-fixtures--get :owner) "kgDO"))
    (login . ,(approve-fixtures--get :owner))
    (url . ,(concat "https://github.com/" (approve-fixtures--get :owner)))))

(defconst approve-fixtures--reaction-contents
  '("THUMBS_UP" "THUMBS_DOWN" "LAUGH" "HOORAY" "CONFUSED" "HEART" "ROCKET" "EYES")
  "Reaction groups in the order GitHub returns them.")

(defun approve-fixtures--reactions (&optional counts)
  "Return reactionGroups.  COUNTS is an alist (CONTENT COUNT VIEWER-REACTED)."
  (mapcar (lambda (content)
            (let ((entry (cdr (assoc content counts))))
              `((content . ,content)
                (viewerHasReacted . ,(approve-fixtures--bool (nth 1 entry)))
                (reactors (totalCount . ,(or (car entry) 0))))))
          approve-fixtures--reaction-contents))

;;; Markdown and HTML

(defun approve-fixtures--escape (text)
  "Return TEXT with &, < and > escaped for HTML."
  (replace-regexp-in-string
   "[&<>]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (_ "&gt;")))
   text t t))

(defun approve-fixtures--paragraph-html (paragraph)
  "Return PARAGRAPH, markdown with inline code only, as GitHub's HTML."
  (concat "<p dir=\"auto\">"
          (replace-regexp-in-string
           "`\\([^`]+\\)`" "<code class=\"notranslate\">\\1</code>"
           (approve-fixtures--escape paragraph) t)
          "</p>"))

(defun approve-fixtures--suggestion-row (kind line text)
  "Return a row of a suggested change: KIND is deletion or addition."
  (format "          <tr class=\"border-0\">
            <td class=\"blob-num blob-num-%s text-right border-0 px-2 py-1 lh-default\" data-line-number=\"%d\"></td>
            <td class=\"border-0 px-2 py-1 blob-code-inner blob-code-%s js-blob-code-%s blob-code-marker-%s\">%s</td>
          </tr>\n"
          kind line kind kind kind (approve-fixtures--escape text)))

(defun approve-fixtures--suggestion-html (start old new)
  "Return GitHub's HTML for a suggestion replacing OLD with NEW.
OLD and NEW are lists of line texts; START is the line of the first."
  (let ((rows (concat
               (cl-loop for text in old for line from start
                        concat (approve-fixtures--suggestion-row "deletion" line text))
               (cl-loop for text in new for line from start
                        concat (approve-fixtures--suggestion-row "addition" line text)))))
    (concat
     "  <div class=\"my-2 border rounded-2 js-suggested-changes-blob diff-view js-check-hidden-unicode\" id=\"\">
    <div class=\"f6 p-2 lh-condensed border-bottom d-flex\">
      <div class=\"flex-auto flex-items-center color-fg-muted\">
        Suggested change
      </div>
    </div>
    <div itemprop=\"text\" class=\"blob-wrapper data file rounded-0\" style=\"margin: 0; border: none; overflow-y: visible; overflow-x: auto;\">
      <table class=\"d-table tab-size mb-0 width-full\" data-paste-markdown-skip=\"\">
          <tbody>"
     (string-trim-left rows)
     "      </tbody></table>
    </div>
    <div class=\"js-apply-changes\"></div>
  </div>\n")))

(defun approve-fixtures--body (text &optional suggestion)
  "Return (BODY . BODY-HTML) for the markdown TEXT.
TEXT is paragraphs separated by blank lines, with inline code.
SUGGESTION, if non-nil, is (START OLD NEW) as for
`approve-fixtures--suggestion-html'; it is appended as a ```suggestion
block."
  (let ((html (mapconcat #'approve-fixtures--paragraph-html
                         (split-string text "\n\n" t) "\n")))
    (if suggestion
        (pcase-let ((`(,start ,old ,new) suggestion))
          (cons (concat text "\n```suggestion\n" (string-join new "\n") "\n```")
                (concat html "\n" (approve-fixtures--suggestion-html start old new))))
      (cons text html))))

;;; Files and patches

(cl-defstruct (approve-fixtures--file (:constructor approve-fixtures--file-create)
                                      (:copier nil))
  "A changed file: its GraphQL and REST data and its diff lines.
HUNKS is a list of hunk specs (OLD-START CONTEXT-BEFORE DELETED ADDED
CONTEXT-AFTER).  LINES is a vector with, per hunk, a list of (KIND
OLD NEW TEXT), KIND being ctx, del or add.  TEXTS maps (SIDE . LINE)
to the line's text."
  path change viewed previous binary no-patch hunks patch additions
  deletions lines texts)

(defconst approve-fixtures--code
  '(("go"
     ("\tvalue := legacyLookup(table, %d)" "\tif err := oldStep(ctx, %d); err != nil {"
      "\t\treturn nil, err" "\t}" "\t// TODO(%d): drop this fallback" "\tlog.Printf(\"step %d\")")
     ("\tvalue, err := lookup(ctx, table, %d)" "\tif err != nil {"
      "\t\treturn nil, fmt.Errorf(\"lookup %d: %%w\", err)" "\t}"
      "\tmetrics.Inc(\"step_%d\")" "\tresult = append(result, value)")
     ("// helper%d keeps the caller small" "func helper%d(ctx context.Context) error {"
      "\treturn nil" "}" "" "\tcount += %d"))
    ("py"
     ("    return \"Hello, \" + name  # %d" "    print(\"greeting %d\")" "    pass")
     ("    return f\"Hello, {name}!\"  # %d" "    logger.info(\"greeting %d\")"
      "    return greeting")
     ("def helper_%d(name):" "    \"\"\"Line %d stays.\"\"\"" ""))
    ("md"
     ("Old text of paragraph %d." "- an old list item (%d)")
     ("New text of paragraph %d, rewritten." "- a new list item (%d)")
     ("Paragraph %d stays the same." ""))
    ("yml"
     ("      - run: make old-step-%d" "        timeout-minutes: %d")
     ("      - run: make step-%d" "        timeout-minutes: %d" "        continue-on-error: false")
     ("    steps: # %d" "      - uses: actions/checkout@v4"))
    (nil
     ("old line %d" "removed setting %d")
     ("new line %d" "added setting %d")
     ("unchanged line %d" "")))
  "Line templates by file extension: (EXTENSION OLD NEW CONTEXT).
Each template takes the line number.")

(defun approve-fixtures--code-line (path kind number)
  "Return the text of line NUMBER of PATH, of KIND ctx, del or add."
  (let* ((templates (or (assoc (file-name-extension path) approve-fixtures--code)
                        (assoc nil approve-fixtures--code)))
         (choices (pcase kind ('del (nth 1 templates)) ('add (nth 2 templates))
                         (_ (nth 3 templates)))))
    (format (nth (mod number (length choices)) choices) number)))

(defun approve-fixtures--range (start count)
  "Return a hunk header range: START,COUNT, or START when COUNT is 1."
  (if (= count 1) (format "%d" start) (format "%d,%d" start count)))

(defun approve-fixtures--build-patch (file)
  "Fill in the patch, line counts, LINES and TEXTS of FILE from its hunks."
  (let ((path (approve-fixtures--file-path file))
        (offset 0) (additions 0) (deletions 0)
        (texts (make-hash-table :test #'equal))
        (parts nil) (lines nil))
    (dolist (hunk (approve-fixtures--file-hunks file))
      (pcase-let* ((`(,start ,before ,del ,add ,after) hunk)
                   (old-count (+ before del after))
                   (new-count (+ before add after))
                   (new-start (cond ((zerop old-count) (+ start offset 1))
                                    ((zerop new-count) (+ start offset -1))
                                    (t (+ start offset))))
                   (old (max start 1)) (new new-start) (hunk-lines nil))
        (push (format "@@ -%s +%s @@%s"
                      (approve-fixtures--range start old-count)
                      (approve-fixtures--range new-start new-count)
                      (if (and (> start 1) (equal (file-name-extension path) "go"))
                          (format " func step%d(ctx context.Context) error {" start)
                        ""))
              parts)
        (dolist (kind (append (make-list before 'ctx) (make-list del 'del)
                              (make-list add 'add) (make-list after 'ctx)))
          (let ((text (approve-fixtures--code-line path kind (if (eq kind 'add) new old))))
            (push (concat (pcase kind ('ctx " ") ('del "-") ('add "+")) text) parts)
            (push (list kind (and (not (eq kind 'add)) old) (and (not (eq kind 'del)) new)
                        text)
                  hunk-lines)
            (unless (eq kind 'add) (puthash (cons "LEFT" old) text texts) (cl-incf old))
            (unless (eq kind 'del) (puthash (cons "RIGHT" new) text texts) (cl-incf new))))
        (push (nreverse hunk-lines) lines)
        (setq offset (+ offset (- add del))
              additions (+ additions add)
              deletions (+ deletions del))))
    (setf (approve-fixtures--file-patch file) (string-join (nreverse parts) "\n")
          (approve-fixtures--file-lines file) (vconcat (nreverse lines))
          (approve-fixtures--file-texts file) texts)
    (unless (approve-fixtures--file-no-patch file)
      (setf (approve-fixtures--file-additions file) additions
            (approve-fixtures--file-deletions file) deletions))
    file))

(cl-defun approve-fixtures--add-file (path change &key (viewed "UNVIEWED") previous
                                           binary hunks too-large)
  "Add the changed file PATH, of CHANGE (a PatchStatus), to the scenario.
VIEWED is its viewerViewedState, PREVIOUS the old path of a renamed
or copied file.  A BINARY file has no patch and no line counts.
HUNKS are the hunk specs of its patch (see `approve-fixtures--file').
TOO-LARGE is the number of added lines of a file whose patch GitHub
leaves out.  Return the file."
  (let ((file (approve-fixtures--file-create
               :path path :change change :viewed viewed :previous previous
               :binary binary :no-patch (or binary too-large (null hunks))
               :hunks hunks :additions (or too-large 0) :deletions 0)))
    (approve-fixtures--build-patch file)
    (setq approve-fixtures--files (append approve-fixtures--files (list file)))
    file))

(defun approve-fixtures--file (path)
  "Return the file PATH of the scenario."
  (or (seq-find (lambda (file) (equal (approve-fixtures--file-path file) path))
                approve-fixtures--files)
      (error "No file %s" path)))

(defun approve-fixtures--line (path side hunk k)
  "Return the line number of the K-th changed line on SIDE of HUNK in PATH.
SIDE is \"RIGHT\" (added lines) or \"LEFT\" (deleted lines); HUNK and
K count from 0."
  (let* ((kind (if (equal side "RIGHT") 'add 'del))
         (lines (seq-filter (lambda (line) (eq (car line) kind))
                            (aref (approve-fixtures--file-lines
                                   (approve-fixtures--file path))
                                  hunk)))
         (line (or (nth k lines) (error "No line %d on %s of hunk %d in %s"
                                        k side hunk path))))
    (if (eq kind 'add) (nth 2 line) (nth 1 line))))

(defun approve-fixtures--line-text (path side line)
  "Return the text of LINE on SIDE of PATH."
  (or (gethash (cons side line)
               (approve-fixtures--file-texts (approve-fixtures--file path)))
      (error "Line %s %d is not in the diff of %s" side line path)))

(defun approve-fixtures--hunk-pattern (seed count size)
  "Return COUNT hunk specs of SIZE changed lines each, varied by SEED."
  (cl-loop for i below count
           for del = (mod (* (+ seed i) 7) (1+ (/ size 3)))
           collect (list (+ 4 (* i (+ size 40)) (mod seed 5)) 3 del (- size del) 3)))

(defun approve-fixtures--sorted-files ()
  "Return the scenario's files sorted by path, as GitHub lists them."
  (sort (copy-sequence approve-fixtures--files)
        (lambda (a b) (string< (approve-fixtures--file-path a)
                               (approve-fixtures--file-path b)))))

(defun approve-fixtures--file-node (file)
  "Return FILE as the files query selects it."
  `((path . ,(approve-fixtures--file-path file))
    (additions . ,(approve-fixtures--file-additions file))
    (deletions . ,(approve-fixtures--file-deletions file))
    (changeType . ,(approve-fixtures--file-change file))
    (viewerViewedState . ,(approve-fixtures--file-viewed file))))

(defun approve-fixtures--rest-file (file head)
  "Return FILE as REST's pull request files list has it, at HEAD."
  (let* ((path (approve-fixtures--file-path file))
         (encoded (url-hexify-string path))
         (repo (format "%s/%s" (approve-fixtures--get :owner) (approve-fixtures--get :repo))))
    `((sha . ,(approve-fixtures--hash (concat "blob:" path)))
      (filename . ,path)
      (status . ,(pcase (approve-fixtures--file-change file)
                   ("ADDED" "added") ("DELETED" "removed") ("MODIFIED" "modified")
                   ("RENAMED" "renamed") ("COPIED" "copied") ("CHANGED" "changed")))
      (additions . ,(approve-fixtures--file-additions file))
      (deletions . ,(approve-fixtures--file-deletions file))
      (changes . ,(+ (approve-fixtures--file-additions file)
                     (approve-fixtures--file-deletions file)))
      (blob_url . ,(format "https://github.com/%s/blob/%s/%s" repo head encoded))
      (raw_url . ,(format "https://github.com/%s/raw/%s/%s" repo head encoded))
      (contents_url . ,(format "https://api.github.com/repos/%s/contents/%s?ref=%s"
                               repo encoded head))
      ,@(and (not (approve-fixtures--file-no-patch file))
             `((patch . ,(approve-fixtures--file-patch file))))
      ,@(and (approve-fixtures--file-previous file)
             `((previous_filename . ,(approve-fixtures--file-previous file)))))))

;;; Reviews and threads

(cl-defstruct (approve-fixtures--review (:constructor approve-fixtures--review-create)
                                        (:copier nil))
  "A review: KEY names it; COMMENTS are its comment objects, in order."
  key id who state body at comments database-id)

(defun approve-fixtures--add-review (key who state at &optional body)
  "Add the review KEY by WHO in STATE at minute AT with BODY; return it."
  (let ((review (approve-fixtures--review-create
                 :key key :who who :state state :at at :body (or body "")
                 :id (approve-fixtures--node-id "PRR" (format "review/%s" key))
                 :database-id (+ 3300000000 (approve-fixtures--next)))))
    (setq approve-fixtures--reviews (append approve-fixtures--reviews (list review)))
    review))

(defun approve-fixtures--review (key)
  "Return the review KEY."
  (or (seq-find (lambda (review) (eq (approve-fixtures--review-key review) key))
                approve-fixtures--reviews)
      (error "No review %s" key)))

(defun approve-fixtures--review-object (review)
  "Return REVIEW as PullRequestReviewFields select it."
  (let* ((who (approve-fixtures--review-who review))
         (mine (approve-fixtures--viewer-p who))
         (pending (equal (approve-fixtures--review-state review) "PENDING"))
         (body (approve-fixtures--body (approve-fixtures--review-body review))))
    `((__typename . "PullRequestReview")
      (id . ,(approve-fixtures--review-id review))
      (url . ,(approve-fixtures--pr-url
               (format "#pullrequestreview-%d" (approve-fixtures--review-database-id review))))
      (author . ,(approve-fixtures--actor who))
      (authorAssociation . ,(approve-fixtures--association who))
      (state . ,(approve-fixtures--review-state review))
      (body . ,(car body))
      (bodyHTML . ,(if (string-empty-p (car body)) "" (cdr body)))
      (createdAt . ,(approve-fixtures--time (approve-fixtures--review-at review)))
      (submittedAt . ,(and (not pending)
                           (approve-fixtures--time (approve-fixtures--review-at review))))
      (lastEditedAt)
      (includesCreatedEdit . :false)
      (isMinimized . :false)
      (minimizedReason)
      (commentCount (totalCount . ,(length (approve-fixtures--review-comments review))))
      (viewerDidAuthor . ,(approve-fixtures--bool mine))
      (viewerCanUpdate . ,(approve-fixtures--bool mine))
      (viewerCanDelete . ,(approve-fixtures--bool (and mine pending)))
      (viewerCanReact . ,(approve-fixtures--bool (not pending)))
      (viewerCanMinimize . :false)
      (viewerCanUnminimize . :false)
      (reactionGroups . ,(approve-fixtures--reactions
                          (and (equal (approve-fixtures--review-state review) "APPROVED")
                               '(("HOORAY" 1 nil))))))))

(defvar approve-fixtures--threads nil
  "Thread objects of the scenario being built, in order.")

(cl-defun approve-fixtures--add-thread (path &key (side "RIGHT") line start-line
                                             start-side file-level resolved-by
                                             outdated original-line
                                             original-start-line comments)
  "Add a review thread on PATH to the scenario and return it.
SIDE is its diffSide.  LINE and START-LINE place it (START-LINE for a
range, on START-SIDE, SIDE by default).  A FILE-LEVEL thread has no
lines.  An OUTDATED thread has no LINE; ORIGINAL-LINE and
ORIGINAL-START-LINE say where it was.  RESOLVED-BY is who resolved
it.  COMMENTS are plists (:who :at :text :review :suggestion): the
first comment names its REVIEW (a key); a reply without one gets a
review of its own, as GitHub makes for replies.  A thread whose first
review is pending is pending."
  (let* ((n (approve-fixtures--next))
         (thread-id (approve-fixtures--node-id "PRRT" (format "thread/%d" n)))
         (first-review (approve-fixtures--review (plist-get (car comments) :review)))
         (pending (equal (approve-fixtures--review-state first-review) "PENDING"))
         (current-line (and (not outdated) (not file-level) line))
         (current-start (and (not outdated) (not file-level) start-line))
         (original-line (and (not file-level) (or original-line line)))
         (original-start (and (not file-level) (or original-start-line start-line)))
         (start-diff-side (and (or current-start original-start) (or start-side side)))
         (subject (if file-level "FILE" "LINE"))
         (maintainer (approve-fixtures--get :maintainer))
         (first-id nil)
         (objects
          (cl-loop
           for comment in comments
           for index from 0
           collect
           (let* ((who (plist-get comment :who))
                  (at (plist-get comment :at))
                  (review (if (plist-get comment :review)
                              (approve-fixtures--review (plist-get comment :review))
                            (approve-fixtures--add-review
                             (intern (format "reply-%d" (approve-fixtures--next)))
                             who (if pending "PENDING" "COMMENTED") at)))
                  (database-id (+ 3100000000 (approve-fixtures--next)))
                  (id (approve-fixtures--node-id "PRRC" (format "comment/%d" database-id)))
                  (body (approve-fixtures--body (plist-get comment :text)
                                                (plist-get comment :suggestion)))
                  (mine (approve-fixtures--viewer-p who))
                  (object
                   `((__typename . "PullRequestReviewComment")
                     (id . ,id)
                     (url . ,(approve-fixtures--pr-url (format "#discussion_r%d" database-id)))
                     (author . ,(approve-fixtures--actor who))
                     (authorAssociation . ,(approve-fixtures--association who))
                     (body . ,(car body))
                     (bodyHTML . ,(cdr body))
                     (createdAt . ,(approve-fixtures--time at))
                     (lastEditedAt)
                     (includesCreatedEdit . :false)
                     (isMinimized . :false)
                     (minimizedReason)
                     (state . ,(if pending "PENDING" "SUBMITTED"))
                     (path . ,path)
                     (line . ,current-line)
                     (startLine . ,current-start)
                     (originalLine . ,original-line)
                     (originalStartLine . ,original-start)
                     (outdated . ,(approve-fixtures--bool outdated))
                     (subjectType . ,subject)
                     (replyTo . ,(and first-id `((__typename . "PullRequestReviewComment")
                                                 (id . ,first-id))))
                     (pullRequestReview (__typename . "PullRequestReview")
                                        (id . ,(approve-fixtures--review-id review)))
                     (viewerDidAuthor . ,(approve-fixtures--bool mine))
                     (viewerCanUpdate . ,(approve-fixtures--bool mine))
                     (viewerCanDelete . ,(approve-fixtures--bool (or mine maintainer)))
                     (viewerCanReact . ,(approve-fixtures--bool (not pending)))
                     (viewerCanMinimize . ,(approve-fixtures--bool (and maintainer (not mine))))
                     (viewerCanUnminimize . :false)
                     (reactionGroups . ,(approve-fixtures--reactions
                                         (and (= index 0) (not pending)
                                              (cl-evenp n)
                                              '(("THUMBS_UP" 2 t) ("EYES" 1 nil))))))))
             (setf (approve-fixtures--review-comments review)
                   (append (approve-fixtures--review-comments review) (list object)))
             (unless first-id (setq first-id id))
             object)))
         (thread
          `((__typename . "PullRequestReviewThread")
            (id . ,thread-id)
            (path . ,path)
            (line . ,current-line)
            (startLine . ,current-start)
            (originalLine . ,original-line)
            (originalStartLine . ,original-start)
            (diffSide . ,side)
            (startDiffSide . ,start-diff-side)
            (subjectType . ,subject)
            (isResolved . ,(approve-fixtures--bool resolved-by))
            (isOutdated . ,(approve-fixtures--bool outdated))
            (resolvedBy . ,(and resolved-by (approve-fixtures--user resolved-by)))
            (viewerCanReply . t)
            (viewerCanResolve . ,(approve-fixtures--bool
                                  (and maintainer (not resolved-by) (not pending))))
            (viewerCanUnresolve . ,(approve-fixtures--bool (and maintainer resolved-by)))
            (comments
             (totalCount . ,(length objects))
             (pageInfo (hasNextPage . :false)
                       (endCursor . ,(approve-fixtures--cursor (length objects))))
             (nodes . ,objects)))))
    (setq approve-fixtures--threads (append approve-fixtures--threads (list thread)))
    thread))

(defun approve-fixtures--suggestion (path start end new)
  "Return a suggestion replacing lines START..END of PATH by NEW lines."
  (list start
        (cl-loop for line from start to end
                 collect (approve-fixtures--line-text path "RIGHT" line))
        new))

;;; Commits, checks and events

(defun approve-fixtures--rollup (seed state)
  "Return the statusCheckRollup with SEED and STATE, or nil without STATE."
  (and state
       `((__typename . "StatusCheckRollup")
         (id . ,(approve-fixtures--node-id "SCR" seed))
         (state . ,state))))

(defun approve-fixtures--commit (seed headline who at &optional state)
  "Return a commit as CommitFields select it.
SEED makes its oid; HEADLINE is its subject; WHO wrote it at minute
AT.  STATE is its check rollup state, or nil for no checks."
  (let ((oid (approve-fixtures--hash (format "%s/commit/%s"
                                             (approve-fixtures--get :name) seed))))
    `((__typename . "Commit")
      (id . ,(approve-fixtures--node-id "C" (concat "commit/" seed)))
      (oid . ,oid)
      (abbreviatedOid . ,(substring oid 0 7))
      (messageHeadline . ,headline)
      (committedDate . ,(approve-fixtures--time at))
      (url . ,(format "https://github.com/%s/%s/commit/%s"
                      (approve-fixtures--get :owner) (approve-fixtures--get :repo) oid))
      (author (name . ,(nth 2 (approve-fixtures--person who)))
              (email . ,(format "%s@example.com" who))
              (date . ,(approve-fixtures--time at t))
              (user . ,(approve-fixtures--user who)))
      (statusCheckRollup . ,(approve-fixtures--rollup (concat "rollup/" seed) state)))))

(defun approve-fixtures--commit-ref (commit)
  "Return COMMIT as events select it: typename, id and oids."
  `((__typename . "Commit")
    (id . ,(alist-get 'id commit))
    (oid . ,(alist-get 'oid commit))
    (abbreviatedOid . ,(alist-get 'abbreviatedOid commit))))

(defun approve-fixtures--pr-commit (commit)
  "Return the PullRequestCommit node of COMMIT."
  `((__typename . "PullRequestCommit")
    (id . ,(approve-fixtures--node-id "PURC" (alist-get 'oid commit)))
    (commit . ,commit)))

(defun approve-fixtures--check-run (name status conclusion at &optional workflow required)
  "Return a CheckRun NAME with STATUS and CONCLUSION, started at minute AT.
WORKFLOW names its Actions workflow; REQUIRED is its isRequired."
  (let ((database-id (+ 52000000000 (approve-fixtures--next)))
        (workflow (or workflow "CI")))
    `((__typename . "CheckRun")
      (id . ,(approve-fixtures--node-id "CR" (concat "check/" name)))
      (name . ,name)
      (status . ,status)
      (conclusion . ,conclusion)
      (title . ,(and conclusion (format "%s: %s" name (downcase conclusion))))
      (isRequired . ,(approve-fixtures--bool required))
      (detailsUrl . ,(format "https://github.com/%s/%s/actions/runs/%d/job/%d"
                             (approve-fixtures--get :owner) (approve-fixtures--get :repo)
                             (+ 17000000000 (approve-fixtures--get :number)) database-id))
      (url . ,(format "https://github.com/%s/%s/runs/%d"
                      (approve-fixtures--get :owner) (approve-fixtures--get :repo)
                      database-id))
      (startedAt . ,(approve-fixtures--time at))
      (completedAt . ,(and conclusion (approve-fixtures--time (+ at 4))))
      (checkSuite (__typename . "CheckSuite")
                  (id . ,(approve-fixtures--node-id "CS" (concat "suite/" workflow)))
                  (app (__typename . "App")
                       (id . ,(approve-fixtures--id "A" "github-actions"))
                       (name . "GitHub Actions"))
                  (workflowRun (__typename . "WorkflowRun")
                               (id . ,(approve-fixtures--node-id "WFR" (concat "run/" workflow)))
                               (workflow (__typename . "Workflow")
                                         (id . ,(approve-fixtures--node-id "W" workflow))
                                         (name . ,workflow)))))))

(defun approve-fixtures--status-context (context state description at &optional required)
  "Return a StatusContext CONTEXT in STATE with DESCRIPTION at minute AT.
REQUIRED is its isRequired."
  `((__typename . "StatusContext")
    (id . ,(approve-fixtures--node-id "SC" (concat "status/" context)))
    (context . ,context)
    (state . ,state)
    (description . ,description)
    (isRequired . ,(approve-fixtures--bool required))
    (targetUrl . ,(format "https://coverage.example.com/%s/%s/pull/%d"
                          (approve-fixtures--get :owner) (approve-fixtures--get :repo)
                          (approve-fixtures--get :number)))
    (createdAt . ,(approve-fixtures--time at))))

(defconst approve-fixtures--check-run-states
  '("ACTION_REQUIRED" "CANCELLED" "COMPLETED" "FAILURE" "IN_PROGRESS" "NEUTRAL"
    "PENDING" "QUEUED" "SKIPPED" "STALE" "STARTUP_FAILURE" "SUCCESS" "TIMED_OUT"
    "WAITING")
  "Check run states in the order contextCounts lists them.")

(defconst approve-fixtures--status-states
  '("EXPECTED" "ERROR" "FAILURE" "PENDING" "SUCCESS")
  "Status context states in the order contextCounts lists them.")

(defun approve-fixtures--context-counts (contexts)
  "Return the core query's contextCounts for CONTEXTS."
  (let* ((runs (seq-filter (lambda (c) (equal (alist-get '__typename c) "CheckRun"))
                           contexts))
         (statuses (seq-difference contexts runs))
         (run-state (lambda (run) (let ((conclusion (alist-get 'conclusion run)))
                                    (if (stringp conclusion) conclusion
                                      (alist-get 'status run))))))
    `((checkRunCount . ,(length runs))
      (checkRunCountsByState
       . ,(mapcar (lambda (state)
                    `((state . ,state)
                      (count . ,(seq-count (lambda (run) (equal (funcall run-state run) state))
                                           runs))))
                  approve-fixtures--check-run-states))
      (statusContextCount . ,(length statuses))
      (statusContextCountsByState
       . ,(mapcar (lambda (state)
                    `((state . ,state)
                      (count . ,(seq-count (lambda (s) (equal (alist-get 'state s) state))
                                           statuses))))
                  approve-fixtures--status-states)))))

(defun approve-fixtures--event (type prefix who at &rest fields)
  "Return a timeline event of TYPE (id PREFIX) by WHO at minute AT with FIELDS.
FIELDS is an alist of the event's own fields, after actor and createdAt."
  `((__typename . ,type)
    (id . ,(approve-fixtures--node-id prefix (format "event/%d" (approve-fixtures--next))))
    (actor . ,(approve-fixtures--actor who))
    (createdAt . ,(approve-fixtures--time at))
    ,@fields))

(defun approve-fixtures--label (name color description)
  "Return the label NAME as LabelFields select it, with COLOR and DESCRIPTION."
  `((__typename . "Label")
    (id . ,(approve-fixtures--node-id "LA" (concat "label/" name)))
    (name . ,name)
    (color . ,color)
    (description . ,description)
    (url . ,(format "https://github.com/%s/%s/labels/%s" (approve-fixtures--get :owner)
                    (approve-fixtures--get :repo) (url-hexify-string name)))))

(defun approve-fixtures--issue-comment (who at text &optional reactions)
  "Return a conversation comment by WHO at minute AT saying TEXT.
REACTIONS is as for `approve-fixtures--reactions'."
  (let* ((database-id (+ 2700000000 (approve-fixtures--next)))
         (body (approve-fixtures--body text))
         (mine (approve-fixtures--viewer-p who))
         (maintainer (approve-fixtures--get :maintainer)))
    `((__typename . "IssueComment")
      (id . ,(approve-fixtures--node-id "IC" (format "comment/%d" database-id)))
      (url . ,(approve-fixtures--pr-url (format "#issuecomment-%d" database-id)))
      (author . ,(approve-fixtures--actor who))
      (authorAssociation . ,(approve-fixtures--association who))
      (body . ,(car body))
      (bodyHTML . ,(cdr body))
      (createdAt . ,(approve-fixtures--time at))
      (lastEditedAt)
      (includesCreatedEdit . :false)
      (isMinimized . :false)
      (minimizedReason)
      (viewerDidAuthor . ,(approve-fixtures--bool mine))
      (viewerCanUpdate . ,(approve-fixtures--bool (or mine maintainer)))
      (viewerCanDelete . ,(approve-fixtures--bool (or mine maintainer)))
      (viewerCanReact . t)
      (viewerCanMinimize . ,(approve-fixtures--bool (and maintainer (not mine))))
      (viewerCanUnminimize . :false)
      (reactionGroups . ,(approve-fixtures--reactions reactions)))))

;;; Exchanges

(defconst approve-fixtures--reset-time
  (time-add approve-fixtures--epoch 3600)
  "When the rate limit of every exchange resets.")

(defun approve-fixtures--headers (resource &optional link)
  "Return the response headers of an exchange with RESOURCE and LINK.
RESOURCE is \"graphql\" or \"core\"; each exchange uses one point."
  (setq approve-fixtures--rate-remaining (1- approve-fixtures--rate-remaining))
  `(("X-RateLimit-Limit" . "5000")
    ("X-RateLimit-Remaining" . ,(number-to-string approve-fixtures--rate-remaining))
    ("X-RateLimit-Reset" . ,(format-time-string "%s" approve-fixtures--reset-time))
    ("X-RateLimit-Used" . ,(number-to-string (- 5000 approve-fixtures--rate-remaining)))
    ("X-RateLimit-Resource" . ,resource)
    ,@(and link `(("Link" . ,link)))))

(defun approve-fixtures--variables (&optional after)
  "Return the query variables of the pull request, with AFTER if non-nil."
  `((owner . ,(approve-fixtures--get :owner))
    (name . ,(approve-fixtures--get :repo))
    (number . ,(approve-fixtures--get :number))
    ,@(and after `((after . ,after)))))

(defun approve-fixtures--graphql (query data &optional after)
  "Return the exchange of QUERY (a file in queries/) answered with DATA.
DATA is the alist under data before rateLimit; AFTER is the cursor
of a later page."
  (let ((headers (approve-fixtures--headers "graphql")))
    (list :request (list :kind 'graphql :name (concat "queries/" query)
                         :variables (approve-fixtures--variables after))
          :status 200
          :headers headers
          :body `((data ,@data
                        (rateLimit (cost . 1)
                                   (remaining . ,approve-fixtures--rate-remaining)
                                   (resetAt . ,(format-time-string
                                                "%Y-%m-%dT%H:%M:%SZ"
                                                approve-fixtures--reset-time t))))))))

(defun approve-fixtures--pr-id ()
  "Return the node id of the scenario's pull request."
  (approve-fixtures--node-id "PR" "pull-request"))

(defun approve-fixtures--paged (query field nodes &optional files-cursor)
  "Return the exchanges of QUERY paging the list FIELD of NODES by 100.
With FILES-CURSOR, cursors are the item count in base 64, as the files
list has them; otherwise opaque."
  (let* ((pages (or (seq-partition nodes 100) (list nil)))
         (total (length nodes))
         (seen 0) (after nil))
    (cl-loop
     for page in pages
     for last = (eq page (car (last pages)))
     collect
     (let* ((count (+ seen (length page)))
            (cursor (and page (if files-cursor
                                  (base64-encode-string (number-to-string count))
                                (approve-fixtures--cursor count))))
            (exchange
             (approve-fixtures--graphql
              query
              `((repository
                 (pullRequest (__typename . "PullRequest")
                              (id . ,(approve-fixtures--pr-id))
                              (,field (totalCount . ,total)
                                      (pageInfo (hasNextPage . ,(approve-fixtures--bool (not last)))
                                                (endCursor . ,cursor))
                                      (nodes . ,(approve-fixtures--array page))))))
              after)))
       (setq seen count after cursor)
       exchange))))

(defun approve-fixtures--rest-files (head)
  "Return the exchanges of the REST files list at HEAD, 100 per page.
A list of exactly a multiple of 100 ends with an empty page, as
GitHub's does."
  (let* ((entries (mapcar (lambda (file) (approve-fixtures--rest-file file head))
                          (approve-fixtures--sorted-files)))
         (pages (seq-partition entries 100))
         (pages (if (or (null pages) (= (length (car (last pages))) 100))
                    (append pages (list nil))
                  pages))
         (resource (format "/repos/%s/%s/pulls/%d/files" (approve-fixtures--get :owner)
                           (approve-fixtures--get :repo) (approve-fixtures--get :number)))
         (url (format "https://api.github.com/repositories/%d/pulls/%d/files?per_page=100&page="
                      (string-to-number (substring (approve-fixtures--hash
                                                    (approve-fixtures--get :repo))
                                                   0 6)
                                        16)
                      (approve-fixtures--get :number)))
         (count (length pages)))
    (cl-loop
     for page in pages
     for number from 1
     collect
     (list :request (list :kind 'rest :method "GET" :resource resource
                          :params `((per_page . 100) (page . ,number)))
           :status 200
           :headers (approve-fixtures--headers
                     "core"
                     (and (> count 1)
                          (string-join
                           (append
                            (and (< number count)
                                 (list (format "<%s%d>; rel=\"next\"" url (1+ number))
                                       (format "<%s%d>; rel=\"last\"" url count)))
                            (and (> number 1)
                                 (list (format "<%s%d>; rel=\"prev\"" url (1- number))
                                       (format "<%s1>; rel=\"first\"" url))))
                           ", ")))
           :body (approve-fixtures--array page)))))

;;; The pull request

(cl-defun approve-fixtures--pull-request (&key title state body at updated draft labels
                                               assignees milestone review-requests
                                               opinionated decision mergeable
                                               merge-state rollup contexts closing
                                               base-ref head-ref commits pending
                                               viewer-latest)
  "Return the core query's pull request.
TITLE, STATE, BODY, DRAFT; AT and UPDATED are minutes.  LABELS,
ASSIGNEES (people), MILESTONE, REVIEW-REQUESTS (people or a team
alist), OPINIONATED (review keys), DECISION, MERGEABLE, MERGE-STATE,
ROLLUP (the head commit's rollup), CONTEXTS (its checks), CLOSING
(issues), BASE-REF and HEAD-REF names, COMMITS (to count and take the
head from), PENDING (the pending review) and VIEWER-LATEST (a review
key and the commit it was made on)."
  (let* ((files approve-fixtures--files)
         (head (alist-get 'commit (car (last commits))))
         (author (approve-fixtures--get :author))
         (maintainer (approve-fixtures--get :maintainer))
         (repo-id (approve-fixtures--node-id "R" "repository"))
         (repo-name (format "%s/%s" (approve-fixtures--get :owner)
                            (approve-fixtures--get :repo)))
         (repo-url (concat "https://github.com/" repo-name))
         (ref (lambda (name)
                `((__typename . "Ref")
                  (id . ,(approve-fixtures--node-id "REF" (concat "ref/" name)))
                  (name . ,name)
                  (prefix . "refs/heads/")
                  (repository (__typename . "Repository")
                              (id . ,repo-id)
                              (nameWithOwner . ,repo-name)
                              (url . ,repo-url)
                              (isFork . :false)
                              (owner . ,(approve-fixtures--organization))))))
         (text (approve-fixtures--body body)))
    `((__typename . "PullRequest")
      (id . ,(approve-fixtures--pr-id))
      (number . ,(approve-fixtures--get :number))
      (url . ,(approve-fixtures--pr-url))
      (title . ,title)
      (state . ,state)
      (isDraft . ,(approve-fixtures--bool draft))
      (locked . :false)
      (activeLockReason)
      (createdAt . ,(approve-fixtures--time at))
      (updatedAt . ,(approve-fixtures--time updated))
      (closed . :false)
      (closedAt)
      (merged . :false)
      (mergedAt)
      (mergedBy)
      (author . ,(approve-fixtures--actor author))
      (authorAssociation . ,(approve-fixtures--association author))
      (body . ,(car text))
      (bodyHTML . ,(cdr text))
      (lastEditedAt)
      (includesCreatedEdit . :false)
      (baseRefName . ,base-ref)
      (baseRefOid . ,(approve-fixtures--hash (concat (approve-fixtures--get :name) "/base")))
      (headRefName . ,head-ref)
      (headRefOid . ,(alist-get 'oid head))
      (isCrossRepository . :false)
      (baseRef . ,(funcall ref base-ref))
      (headRef . ,(funcall ref head-ref))
      (headRepositoryOwner . ,(approve-fixtures--organization))
      (headRepository (__typename . "Repository")
                      (id . ,repo-id)
                      (nameWithOwner . ,repo-name)
                      (url . ,repo-url))
      (additions . ,(apply #'+ (mapcar #'approve-fixtures--file-additions files)))
      (deletions . ,(apply #'+ (mapcar #'approve-fixtures--file-deletions files)))
      (changedFiles . ,(length files))
      (commitCount (totalCount . ,(length commits)))
      (threadCount (totalCount . ,(length approve-fixtures--threads)))
      (labels (nodes . ,(approve-fixtures--array labels)))
      (assignees (nodes . ,(approve-fixtures--array
                            (mapcar #'approve-fixtures--user assignees))))
      (milestone . ,milestone)
      (reviewRequests
       (nodes . ,(approve-fixtures--array
                  (mapcar (lambda (reviewer)
                            `((__typename . "ReviewRequest")
                              (id . ,(approve-fixtures--node-id
                                      "RR" (format "request/%S" (if (symbolp reviewer)
                                                                    reviewer
                                                                  (alist-get 'slug reviewer)))))
                              (asCodeOwner . ,(approve-fixtures--bool (consp reviewer)))
                              (requestedReviewer
                               . ,(if (symbolp reviewer)
                                      (approve-fixtures--actor reviewer)
                                    reviewer))))
                          review-requests))))
      (latestOpinionatedReviews
       (nodes . ,(approve-fixtures--array
                  (mapcar (lambda (key)
                            (let ((review (approve-fixtures--review key)))
                              `((__typename . "PullRequestReview")
                                (id . ,(approve-fixtures--review-id review))
                                (state . ,(approve-fixtures--review-state review))
                                (submittedAt . ,(approve-fixtures--time
                                                 (approve-fixtures--review-at review)))
                                (author . ,(approve-fixtures--actor
                                            (approve-fixtures--review-who review))))))
                          opinionated))))
      (reviewDecision . ,decision)
      (mergeable . ,mergeable)
      (mergeStateStatus . ,merge-state)
      (statusCheckRollup . ,(and rollup
                                 (append rollup
                                         `((contextCounts
                                            . ,(approve-fixtures--context-counts contexts))))))
      (closingIssuesReferences (nodes . ,(approve-fixtures--array closing)))
      (reactionGroups . ,(approve-fixtures--reactions '(("THUMBS_UP" 3 t) ("ROCKET" 1 nil))))
      (viewerDidAuthor . ,(approve-fixtures--bool (approve-fixtures--viewer-p author)))
      (viewerSubscription . ,(if maintainer "SUBSCRIBED" "UNSUBSCRIBED"))
      (viewerCanUpdate . ,(approve-fixtures--bool maintainer))
      (viewerCanReact . t)
      (viewerCanSubscribe . t)
      (viewerCanLabel . ,(approve-fixtures--bool maintainer))
      (viewerCanAssign . ,(approve-fixtures--bool maintainer))
      (viewerCanClose . ,(approve-fixtures--bool maintainer))
      (viewerCanReopen . :false)
      (viewerCanApplySuggestion . ,(approve-fixtures--bool maintainer))
      (viewerCanEditFiles . ,(approve-fixtures--bool maintainer))
      (viewerCanUpdateBranch . ,(approve-fixtures--bool maintainer))
      (viewerCanDeleteHeadRef . :false)
      (viewerCanEnableAutoMerge . ,(approve-fixtures--bool maintainer))
      (viewerCanDisableAutoMerge . :false)
      (viewerCanMergeAsAdmin . :false)
      (viewerLatestReview
       . ,(and viewer-latest
               (let ((review (approve-fixtures--review (car viewer-latest)))
                     (commit (cdr viewer-latest)))
                 `((__typename . "PullRequestReview")
                   (id . ,(approve-fixtures--review-id review))
                   (state . ,(approve-fixtures--review-state review))
                   (submittedAt . ,(approve-fixtures--time
                                    (approve-fixtures--review-at review)))
                   (commit . ,(approve-fixtures--commit-ref commit))))))
      (pendingReviews
       (nodes . ,(approve-fixtures--array
                  (and pending
                       (let ((comments (approve-fixtures--review-comments pending)))
                         (list (append
                                (approve-fixtures--review-object pending)
                                `((comments
                                   (totalCount . ,(length comments))
                                   (pageInfo (hasNextPage . :false)
                                             (endCursor . ,(approve-fixtures--cursor
                                                            (length comments))))
                                   (nodes . ,(approve-fixtures--array comments))))))))))))))

(cl-defun approve-fixtures--scenario-exchanges (&key pull-request commits timeline
                                                     checks-rollup contexts
                                                     (thread-pages t))
  "Return every exchange of the scenario being built, in loading-plan order.
PULL-REQUEST is the core object, COMMITS the PullRequestCommit nodes,
TIMELINE the (MINUTE . NODE) items, CHECKS-ROLLUP and CONTEXTS the head
commit's checks.  THREAD-PAGES nil leaves the threads query out."
  (let* ((head (car (last commits)))
         (head-commit (alist-get 'commit head)))
    (append
     (list (approve-fixtures--graphql
            "pull-request.graphql"
            `((repository (pullRequest . ,pull-request))
              (viewer . ,(approve-fixtures--user (approve-fixtures--get :viewer))))))
     (approve-fixtures--paged "pull-request-files.graphql" 'files
                              (mapcar #'approve-fixtures--file-node
                                      (approve-fixtures--sorted-files))
                              t)
     (and thread-pages
          (approve-fixtures--paged "pull-request-threads.graphql" 'reviewThreads
                                   approve-fixtures--threads))
     (approve-fixtures--paged "pull-request-timeline.graphql" 'timelineItems
                              (mapcar #'cdr (sort (copy-sequence timeline)
                                                  (lambda (a b) (< (car a) (car b))))))
     (approve-fixtures--paged "pull-request-commits.graphql" 'commits commits)
     (list (approve-fixtures--graphql
            "pull-request-checks.graphql"
            `((repository
               (pullRequest
                (__typename . "PullRequest")
                (id . ,(approve-fixtures--pr-id))
                (statusCheckRollup
                 ,@checks-rollup
                 (contexts
                  (totalCount . ,(length contexts))
                  (pageInfo (hasNextPage . :false)
                            (endCursor . ,(base64-encode-string
                                           (number-to-string (length contexts)))))
                  (nodes . ,(approve-fixtures--array contexts)))))))))
     (approve-fixtures--rest-files (alist-get 'oid head-commit)))))

(defun approve-fixtures--review-items ()
  "Return the submitted reviews as timeline items (MINUTE . NODE)."
  (cl-loop for review in approve-fixtures--reviews
           unless (equal (approve-fixtures--review-state review) "PENDING")
           collect (cons (approve-fixtures--review-at review)
                         (approve-fixtures--review-object review))))

(defmacro approve-fixtures--with-scenario (plist &rest body)
  "Run BODY building the scenario described by PLIST, from scratch."
  (declare (indent 1))
  `(let ((approve-fixtures--scenario ,plist)
         (approve-fixtures--counter 0)
         (approve-fixtures--reviews nil)
         (approve-fixtures--files nil)
         (approve-fixtures--threads nil)
         (approve-fixtures--rate-remaining 5000))
     ,@body))

(defun approve-fixtures--write (description exchanges)
  "Write the scenario being built with DESCRIPTION and EXCHANGES."
  (let ((file (approve-dev-scenario-file (approve-fixtures--get :name)
                                         approve-fixtures-directory)))
    (approve-dev-write-scenario
     (list :name (approve-fixtures--get :name) :description description
           :host "github.com" :owner (approve-fixtures--get :owner)
           :repo (approve-fixtures--get :repo) :number (approve-fixtures--get :number)
           :viewer (approve-fixtures--login (approve-fixtures--get :viewer))
           :exchanges exchanges)
     file)
    (message "Wrote %s: %d files, %d diff lines, %d threads, %d comments, %d exchanges"
             (file-relative-name file)
             (length approve-fixtures--files)
             (apply #'+ (mapcar (lambda (file)
                                  (if (approve-fixtures--file-no-patch file) 0
                                    (length (split-string
                                             (approve-fixtures--file-patch file) "\n"))))
                                approve-fixtures--files))
             (length approve-fixtures--threads)
             (apply #'+ (mapcar (lambda (thread)
                                  (let-alist thread .comments.totalCount))
                                approve-fixtures--threads))
             (length exchanges))))

;;; Small

(defun approve-fixtures--small ()
  "Build and write the small scenario."
  (approve-fixtures--with-scenario
      (list :name "small" :owner "acme-example" :repo "greeter" :number 7
            :viewer 'vera :author 'alice :maintainer nil)
    (approve-fixtures--add-file "README.md" "MODIFIED" :viewed "VIEWED"
                                :hunks '((1 0 1 2 3)))
    (approve-fixtures--add-file "src/greeter/greet.py" "MODIFIED"
                                :hunks '((1 0 2 4 3) (20 3 1 3 3)))
    (approve-fixtures--add-file "tests/test_greet.py" "ADDED"
                                :hunks '((0 0 0 18 0)))
    (approve-fixtures--add-review 'bob-1 'bob "COMMENTED" 60 "A couple of small things.")
    (approve-fixtures--add-thread
     "src/greeter/greet.py" :line (approve-fixtures--line "src/greeter/greet.py" "RIGHT" 0 1)
     :comments
     '((:who bob :at 60 :review bob-1
             :text "Should this handle an empty `name`?")
       (:who alice :at 75 :text "Good point, it falls back to `World` now.")))
    (approve-fixtures--add-thread
     "tests/test_greet.py" :line (approve-fixtures--line "tests/test_greet.py" "RIGHT" 0 4)
     :comments
     '((:who bob :at 61 :review bob-1
             :text "Nit: the test name could say what it checks.")))
    (let* ((commits (list (approve-fixtures--pr-commit
                           (approve-fixtures--commit "1" "Greet by name" 'alice -30 "SUCCESS"))
                          (approve-fixtures--pr-commit
                           (approve-fixtures--commit "2" "Fall back to World" 'alice 80
                                                     "SUCCESS"))))
           (rollup (approve-fixtures--rollup "rollup/2" "SUCCESS"))
           (contexts (list (approve-fixtures--check-run "test (3.12)" "COMPLETED" "SUCCESS" 81
                                                        "Tests" t)
                           (approve-fixtures--check-run "lint" "COMPLETED" "SUCCESS" 81
                                                        "Lint")))
           (pull-request
            (approve-fixtures--pull-request
             :title "Greet people by name" :state "OPEN" :at 0 :updated 90
             :body "Greets by name instead of `World`.\n\nCloses #3."
             :base-ref "main" :head-ref "greet-by-name"
             :decision "REVIEW_REQUIRED" :mergeable "MERGEABLE" :merge-state "BLOCKED"
             :review-requests '(bob) :rollup rollup :contexts contexts
             :closing (list `((__typename . "Issue")
                              (id . ,(approve-fixtures--node-id "I" "issue/3"))
                              (number . 3)
                              (title . "Say hello to the user")
                              (state . "OPEN")
                              (url . ,(format "https://github.com/acme-example/greeter/issues/3"))))
             :commits commits))
           (timeline
            (append
             (list (cons -30 (nth 0 commits))
                   (cons 80 (nth 1 commits))
                   (cons 85 (approve-fixtures--issue-comment
                             'carol 85 "Nice, this closes #3." '(("HEART" 1 nil)))))
             (approve-fixtures--review-items))))
      (approve-fixtures--write
       "A small pull request: three files, two threads (one with a reply), one conversation comment, no pending review, passing checks."
       (approve-fixtures--scenario-exchanges
        :pull-request pull-request :commits commits :timeline timeline
        :checks-rollup rollup :contexts contexts)))))

;;; Medium

(defun approve-fixtures--medium-files ()
  "Add the medium scenario's 30 files."
  (approve-fixtures--add-file ".github/workflows/ci.yml" "MODIFIED" :viewed "VIEWED"
                              :hunks (approve-fixtures--hunk-pattern 1 2 8))
  (approve-fixtures--add-file "CHANGELOG.md" "MODIFIED" :viewed "DISMISSED"
                              :hunks '((1 0 0 6 3)))
  (approve-fixtures--add-file "Makefile" "MODIFIED" :viewed "VIEWED"
                              :hunks (approve-fixtures--hunk-pattern 2 1 6))
  (approve-fixtures--add-file "README.md" "MODIFIED" :viewed "UNVIEWED"
                              :hunks '((1 0 0 4 3) (30 3 2 2 3)))
  (approve-fixtures--add-file "assets/logo.png" "ADDED" :binary t)
  (approve-fixtures--add-file "cmd/rocket/main.go" "MODIFIED" :viewed "VIEWED"
                              :hunks (approve-fixtures--hunk-pattern 3 2 12))
  (approve-fixtures--add-file "docs/usage.md" "ADDED" :hunks '((0 0 0 90 0)))
  (approve-fixtures--add-file "go.mod" "MODIFIED" :viewed "VIEWED"
                              :hunks '((3 3 1 2 3)))
  (approve-fixtures--add-file "go.sum" "MODIFIED" :viewed "VIEWED"
                              :hunks (approve-fixtures--hunk-pattern 4 1 8))
  (approve-fixtures--add-file "internal/config/config.go" "MODIFIED"
                              :hunks '((12 3 2 4 3) (60 3 3 3 3)))
  (approve-fixtures--add-file "internal/config/config_test.go" "MODIFIED"
                              :hunks (approve-fixtures--hunk-pattern 5 2 20))
  (approve-fixtures--add-file "internal/engine/engine.go" "MODIFIED"
                              :hunks '((20 3 10 40 3) (150 3 0 30 3) (300 3 20 25 3)
                                       (480 3 6 6 3)))
  (approve-fixtures--add-file "internal/engine/engine_test.go" "ADDED"
                              :hunks '((0 0 0 150 0)))
  (approve-fixtures--add-file "internal/engine/legacy.go" "DELETED" :viewed "VIEWED"
                              :hunks '((1 0 38 0 0)))
  (approve-fixtures--add-file "internal/engine/scheduler.go" "RENAMED"
                              :previous "internal/engine/queue.go"
                              :hunks '((1 0 2 5 3) (40 3 1 1 3)))
  (approve-fixtures--add-file "internal/parser/lexer.go" "MODIFIED"
                              :hunks '((5 3 3 2 3) (90 3 2 8 3)))
  (approve-fixtures--add-file "internal/parser/parser.go" "MODIFIED"
                              :hunks '((10 3 2 6 3) (48 3 4 3 3) (120 3 0 12 3)
                                       (200 3 5 0 3)))
  (approve-fixtures--add-file "internal/parser/parser_test.go" "MODIFIED"
                              :viewed "DISMISSED"
                              :hunks (approve-fixtures--hunk-pattern 6 3 30))
  (approve-fixtures--add-file "internal/parser/testdata/basic.golden" "ADDED"
                              :hunks '((0 0 0 60 0)))
  (approve-fixtures--add-file "internal/render/render.go" "MODIFIED"
                              :hunks (approve-fixtures--hunk-pattern 7 3 22))
  (approve-fixtures--add-file "internal/render/render_test.go" "MODIFIED"
                              :hunks (approve-fixtures--hunk-pattern 8 2 22))
  (approve-fixtures--add-file "internal/render/templates/page.tmpl" "COPIED"
                              :previous "internal/render/templates/base.tmpl"
                              :hunks '((4 3 2 3 3)))
  (approve-fixtures--add-file "internal/store/migrations.go" "ADDED"
                              :hunks '((0 0 0 45 0)))
  (approve-fixtures--add-file "internal/store/store.go" "MODIFIED"
                              :hunks (approve-fixtures--hunk-pattern 9 3 20))
  (approve-fixtures--add-file "internal/store/store_test.go" "MODIFIED"
                              :hunks (approve-fixtures--hunk-pattern 10 2 30))
  (approve-fixtures--add-file "internal/util/strings.go" "DELETED" :viewed "VIEWED"
                              :hunks '((1 0 24 0 0)))
  (approve-fixtures--add-file "pkg/api/client.go" "MODIFIED"
                              :hunks '((25 3 3 6 3) (110 3 0 4 3)))
  (approve-fixtures--add-file "pkg/api/client_test.go" "MODIFIED"
                              :hunks (approve-fixtures--hunk-pattern 11 3 26))
  (approve-fixtures--add-file "pkg/api/types.go" "MODIFIED" :viewed "VIEWED"
                              :hunks (approve-fixtures--hunk-pattern 12 2 16))
  (approve-fixtures--add-file "scripts/release.sh" "CHANGED"))

(defun approve-fixtures--medium-threads ()
  "Add the medium scenario's reviews and threads."
  (let ((parser "internal/parser/parser.go")
        (lexer "internal/parser/lexer.go")
        (engine "internal/engine/engine.go")
        (config "internal/config/config.go")
        (line #'approve-fixtures--line))
    (approve-fixtures--add-review 'dave-1 'dave "DISMISSED" 200
                                  "Needs tests for the error paths.")
    (approve-fixtures--add-review 'carol-1 'carol "COMMENTED" 300)
    (approve-fixtures--add-review 'vera-1 'vera "COMMENTED" 400 "A few questions.")
    (approve-fixtures--add-review 'bob-1 'bob "APPROVED" 1700 "Looks good to me.")
    (approve-fixtures--add-review 'carol-2 'carol "CHANGES_REQUESTED" 1800
                                  "The file-level comment on `parser.go` still needs an answer.")
    (approve-fixtures--add-review 'pending 'vera "PENDING" 3100)
    ;; parser.go: every kind of thread.
    (approve-fixtures--add-thread
     parser :line (funcall line parser "RIGHT" 0 2)
     :comments '((:who carol :at 301 :review carol-1
                       :text "This allocates on every call; hoist the buffer out of the loop.")
                 (:who alice :at 500 :text "Done, it is reused now.")
                 (:who carol :at 520 :text "Thanks!")))
    (let ((start (funcall line parser "RIGHT" 2 3))
          (end (funcall line parser "RIGHT" 2 5)))
      (approve-fixtures--add-thread
       parser :start-line start :line end
       :comments `((:who vera :at 401 :review vera-1
                         :text "These three lines can be one call:"
                         :suggestion ,(approve-fixtures--suggestion
                                       parser start end
                                       '("\tresult = append(result, parseAll(ctx, table)...)")))
                   (:who alice :at 510 :text "Applied in the next commit."))))
    (approve-fixtures--add-thread
     parser :side "LEFT" :line (funcall line parser "LEFT" 1 1) :resolved-by 'alice
     :comments '((:who carol :at 302 :review carol-1
                       :text "Was the old fallback here used anywhere?")
                 (:who alice :at 530 :text "No, `legacyLookup` had no other callers.")))
    (approve-fixtures--add-thread
     parser :outdated t :original-line 61
     :comments '((:who dave :at 201 :review dave-1
                       :text "What happens when `table` is nil?")
                 (:who alice :at 540 :text "It returns `ErrNoTable`; there is a test now.")))
    (approve-fixtures--add-thread
     parser :file-level t
     :comments '((:who carol :at 1801 :review carol-2
                       :text "This file is getting long; could the error helpers move to `errors.go`?")))
    (approve-fixtures--add-thread
     parser :line (funcall line parser "RIGHT" 2 8)
     :comments '((:who vera :at 3101 :review pending
                       :text "Is this branch reachable after the lexer change?")))
    ;; lexer.go: a range on the left side, and a resolved outdated thread.
    (approve-fixtures--add-thread
     lexer :side "LEFT" :start-line (funcall line lexer "LEFT" 0 0)
     :line (funcall line lexer "LEFT" 0 2)
     :comments '((:who bob :at 1701 :review bob-1
                       :text "Nice to see these three special cases go.")))
    (approve-fixtures--add-thread
     lexer :outdated t :original-line 77 :resolved-by 'carol
     :comments '((:who carol :at 303 :review carol-1 :text "Off by one at the end of input?")
                 (:who alice :at 550 :text "Yes, fixed.")))
    ;; engine.go
    (approve-fixtures--add-thread
     engine :start-line (funcall line engine "RIGHT" 0 5) :line (funcall line engine "RIGHT" 0 9)
     :resolved-by 'alice
     :comments '((:who dave :at 202 :review dave-1 :text "This block needs a test.")
                 (:who alice :at 600 :text "Added `TestScheduleRetry`.")
                 (:who dave :at 620 :text "Great.")))
    (approve-fixtures--add-thread
     engine :line (funcall line engine "RIGHT" 1 10)
     :comments '((:who dave :at 203 :review dave-1
                       :text "Should the retry limit be configurable?")
                 (:who alice :at 610 :text "I would keep it fixed for now.")
                 (:who bob :at 640 :text "Agreed, we can add an option later.")
                 (:who dave :at 650 :text "Fine by me.")))
    (approve-fixtures--add-thread
     engine :start-line (funcall line engine "RIGHT" 2 2) :line (funcall line engine "RIGHT" 2 4)
     :comments '((:who vera :at 3105 :review pending
                       :text "Could this use the scheduler's clock instead of `time.Now`?")))
    (let ((target (funcall line engine "RIGHT" 3 1)))
      (approve-fixtures--add-thread
       engine :line target :resolved-by 'alice
       :comments `((:who carol :at 304 :review carol-1 :text "Typo in the error message."
                         :suggestion ,(approve-fixtures--suggestion
                                       engine target target
                                       '("\t\treturn nil, fmt.Errorf(\"schedule: %w\", err)"
                                         "\t}")))
                   (:who alice :at 560 :text "Fixed, thanks."))))
    ;; config.go
    (approve-fixtures--add-thread
     config :line (funcall line config "RIGHT" 0 1)
     :comments '((:who vera :at 402 :review vera-1 :text "Is the default documented?")
                 (:who alice :at 570 :text "It is in `docs/usage.md`.")))
    (approve-fixtures--add-thread
     config :side "LEFT" :line (funcall line config "LEFT" 1 2)
     :comments '((:who vera :at 3110 :review pending
                       :text "Removing this breaks old config files; worth a note in the changelog.")))
    ;; The rest.
    (approve-fixtures--add-thread
     "internal/engine/scheduler.go"
     :line (funcall line "internal/engine/scheduler.go" "RIGHT" 0 3)
     :comments '((:who carol :at 305 :review carol-1 :text "Why the rename?")
                 (:who alice :at 580 :text "It schedules now; it no longer only queues.")
                 (:who carol :at 590 :text "Makes sense.")))
    (approve-fixtures--add-thread
     "pkg/api/client.go" :line (funcall line "pkg/api/client.go" "RIGHT" 1 2)
     :resolved-by 'bob
     :comments '((:who bob :at 1702 :review bob-1 :text "Close the response body here.")
                 (:who alice :at 1750 :text "Done.")))
    (approve-fixtures--add-thread
     "pkg/api/client.go" :outdated t :original-start-line 27 :original-line 30
     :comments '((:who dave :at 204 :review dave-1 :text "This retry loop never backs off.")))
    (approve-fixtures--add-thread
     "README.md" :line (funcall line "README.md" "RIGHT" 0 1)
     :comments '((:who carol :at 306 :review carol-1 :text "Link to the usage docs?")))
    (approve-fixtures--add-thread
     "internal/store/migrations.go" :file-level t
     :comments '((:who carol :at 307 :review carol-1
                       :text "Are migrations run in a transaction?")
                 (:who alice :at 590 :text "Each one is, yes.")))
    (approve-fixtures--add-thread
     "internal/engine/legacy.go" :side "LEFT"
     :line (funcall line "internal/engine/legacy.go" "LEFT" 0 10)
     :comments '((:who dave :at 205 :review dave-1
                       :text "Is anything still importing this?")))))

(defun approve-fixtures--medium ()
  "Build and write the medium scenario."
  (approve-fixtures--with-scenario
      (list :name "medium" :owner "acme-example" :repo "rocket" :number 202
            :viewer 'vera :author 'alice :maintainer t)
    (approve-fixtures--medium-files)
    (approve-fixtures--medium-threads)
    (let* ((c1 (approve-fixtures--commit "1" "Add the new parser" 'alice -120 "SUCCESS"))
           (c2 (approve-fixtures--commit "2" "Rename the queue to scheduler" 'alice -30
                                         "FAILURE"))
           (c2-old (approve-fixtures--commit "2-old" "Rename the queue" 'alice -40))
           (c3 (approve-fixtures--commit "3" "Address review comments" 'alice 1500
                                         "SUCCESS"))
           (c4 (approve-fixtures--commit "4" "Fix lexer edge cases" 'alice 2900 "FAILURE"))
           (commits (mapcar #'approve-fixtures--pr-commit (list c1 c2 c3 c4)))
           (rollup (approve-fixtures--rollup "rollup/4" "FAILURE"))
           (contexts
            (list (approve-fixtures--check-run "build (ubuntu-latest)" "COMPLETED" "SUCCESS"
                                               2901 "CI" t)
                  (approve-fixtures--check-run "build (macos-latest)" "COMPLETED" "SUCCESS"
                                               2901 "CI")
                  (approve-fixtures--check-run "test" "COMPLETED" "FAILURE" 2902 "CI" t)
                  (approve-fixtures--check-run "lint" "IN_PROGRESS" nil 2903 "Lint")
                  (approve-fixtures--check-run "e2e" "QUEUED" nil 2904 "E2E")
                  (approve-fixtures--status-context "coverage/patch" "PENDING"
                                                    "Waiting for the coverage report" 2905)))
           (enhancement (approve-fixtures--label "enhancement" "a2eeef" "New feature or request"))
           (parser-label (approve-fixtures--label "area: parser" "0e8a16" "The parser and lexer"))
           (team `((__typename . "Team")
                   (id . ,(approve-fixtures--id "T" "acme-example/core" "kwDO"))
                   (slug . "core")
                   (combinedSlug . "acme-example/core")
                   (url . "https://github.com/orgs/acme-example/teams/core")))
           (milestone `((__typename . "Milestone")
                        (id . ,(approve-fixtures--node-id "MI" "milestone/1.2"))
                        (number . 4)
                        (title . "v1.2")
                        (state . "OPEN")
                        (dueOn . "2026-11-30T00:00:00Z")
                        (progressPercentage . 62.5)
                        (description . "Parser rewrite and scheduling.")
                        (url . "https://github.com/acme-example/rocket/milestone/4")))
           (issue `((__typename . "Issue")
                    (id . ,(approve-fixtures--node-id "I" "issue/12"))
                    (number . 12)
                    (title . "The parser is slow on large inputs")
                    (state . "OPEN")
                    (url . "https://github.com/acme-example/rocket/issues/12")))
           (pull-request
            (approve-fixtures--pull-request
             :title "Rewrite the parser and engine scheduler" :state "OPEN"
             :at 0 :updated 3110
             :body "Rewrites the parser for speed and renames the queue to a scheduler.\n\nThe lexer fixes come in a follow-up commit.\n\nFixes #12."
             :base-ref "main" :head-ref "parser-rewrite"
             :labels (list enhancement parser-label) :assignees '(alice)
             :milestone milestone :review-requests (list 'dave team)
             :opinionated '(bob-1 carol-2) :decision "CHANGES_REQUESTED"
             :mergeable "MERGEABLE" :merge-state "BLOCKED"
             :rollup rollup :contexts contexts
             :closing (list issue) :commits commits
             :pending (approve-fixtures--review 'pending)
             :viewer-latest (cons 'vera-1 c2)))
           (timeline
            (append
             (list
              (cons -120 (nth 0 commits))
              (cons -30 (nth 1 commits))
              (cons 2 (approve-fixtures--event "LabeledEvent" "LE" 'alice 2
                                               `(label . ,enhancement)))
              (cons 2.5 (approve-fixtures--event "LabeledEvent" "LE" 'alice 2.5
                                                 `(label . ,parser-label)))
              (cons 3 (approve-fixtures--event "ReviewRequestedEvent" "RRE" 'alice 3
                                               `(requestedReviewer
                                                 . ,(approve-fixtures--actor 'bob))))
              (cons 4 (approve-fixtures--event "ReviewRequestedEvent" "RRE" 'alice 4
                                               `(requestedReviewer . ,team)))
              (cons 5 (approve-fixtures--event "AssignedEvent" "AE" 'alice 5
                                               `(assignee . ,(approve-fixtures--actor 'alice))))
              (cons 30 (approve-fixtures--issue-comment
                        'bob 30 "Thanks! I'll take a look today." '(("THUMBS_UP" 1 nil))))
              (cons 1450 (approve-fixtures--event
                          "HeadRefForcePushedEvent" "HRFPE" 'alice 1450
                          `(beforeCommit . ,(approve-fixtures--commit-ref c2-old))
                          `(afterCommit . ,(approve-fixtures--commit-ref c2))))
              (cons 1500 (nth 2 commits))
              (cons 1510 (approve-fixtures--event
                          "RenamedTitleEvent" "RTE" 'alice 1510
                          '(previousTitle . "Parser rewrite")
                          '(currentTitle . "Rewrite the parser and engine scheduler")))
              (cons 1600 (approve-fixtures--event
                          "ReviewDismissedEvent" "RDE" 'alice 1600
                          '(dismissalMessage . "Addressed in the latest commits.")
                          '(previousReviewState . "CHANGES_REQUESTED")
                          `(review (__typename . "PullRequestReview")
                                   (id . ,(approve-fixtures--review-id
                                           (approve-fixtures--review 'dave-1))))))
              (cons 1900 (approve-fixtures--issue-comment
                          'alice 1900
                          "Addressed the comments; the lexer fix follows.\n\n@carol-example, does the `parser.go` split look right now?"))
              (cons 2000 (approve-fixtures--event
                          "CrossReferencedEvent" "CRE" 'carol 2000
                          '(isCrossRepository . :false)
                          '(willCloseTarget . :false)
                          `(source (__typename . "Issue")
                                   (id . ,(alist-get 'id issue))
                                   (number . 12)
                                   (title . ,(alist-get 'title issue))
                                   (url . ,(alist-get 'url issue))
                                   (closed . :false)
                                   (repository
                                    (__typename . "Repository")
                                    (id . ,(approve-fixtures--node-id "R" "repository"))
                                    (nameWithOwner . "acme-example/rocket")))))
              (cons 2100 (approve-fixtures--event "MilestonedEvent" "MIE" 'alice 2100
                                                  '(milestoneTitle . "v1.2")))
              (cons 2900 (nth 3 commits))
              (cons 2950 (approve-fixtures--issue-comment
                          'ci 2950 "Coverage: 81.4% (-0.3%) on `parser-rewrite`."))
              (cons 3000 (approve-fixtures--issue-comment
                          'carol 3000 "CI fails in `test`; it looks related to the scheduler rename.")))
             (approve-fixtures--review-items))))
      (approve-fixtures--write
       "A medium pull request (the speed budget's): 30 files, threads of every kind (lines, ranges, both sides, file level, suggestions, resolved, outdated, replies), the viewer's pending review, reviews of every verdict, several timeline events, failing and pending checks."
       (approve-fixtures--scenario-exchanges
        :pull-request pull-request :commits commits :timeline timeline
        :checks-rollup rollup :contexts contexts)))))

;;; Large

(defconst approve-fixtures--modules
  '("alpha" "bravo" "charlie" "delta" "echo" "foxtrot" "golf" "hotel" "india"
    "juliet" "kilo" "lima" "mike" "november" "oscar")
  "Directories of the large scenario, 20 files each.")

(defun approve-fixtures--large-files ()
  "Add the large scenario's 300 files."
  (dotimes (i 300)
    (let ((directory (format "pkg/%s/" (nth (/ i 20) approve-fixtures--modules)))
          (n (mod i 20)))
      (cond
       ((= i 150)
        (approve-fixtures--add-file (format "%sicon_%02d.png" directory n) "ADDED" :binary t))
       ((= i 250)
        (approve-fixtures--add-file (format "%sschema_%02d.json" directory n) "ADDED"
                                    :too-large 3500))
       ((= (mod i 25) 7)
        (approve-fixtures--add-file (format "%sfile_%02d.go" directory n) "ADDED"
                                    :hunks '((0 0 0 60 0))))
       ((= (mod i 37) 11)
        (approve-fixtures--add-file (format "%sfile_%02d.go" directory n) "DELETED"
                                    :viewed "VIEWED" :hunks '((1 0 40 0 0))))
       (t
        (approve-fixtures--add-file (format "%sfile_%02d.go" directory n) "MODIFIED"
                                    :viewed (cond ((< i 40) "VIEWED")
                                                  ((= (mod i 50) 45) "DISMISSED")
                                                  (t "UNVIEWED"))
                                    :hunks (approve-fixtures--hunk-pattern
                                            i 3 (+ 13 (mod i 7)))))))))

(defun approve-fixtures--large ()
  "Build and write the large scenario."
  (approve-fixtures--with-scenario
      (list :name "large" :owner "acme-example" :repo "monorepo" :number 3003
            :viewer 'vera :author 'dave :maintainer t)
    (approve-fixtures--large-files)
    (approve-fixtures--add-review 'bob-1 'bob "COMMENTED" 600 "Some notes from a first pass.")
    (approve-fixtures--add-review 'carol-1 'carol "APPROVED" 900)
    (let ((line #'approve-fixtures--line))
      (approve-fixtures--add-thread
       "pkg/alpha/file_05.go" :line (funcall line "pkg/alpha/file_05.go" "RIGHT" 0 2)
       :comments '((:who bob :at 601 :review bob-1 :text "Can this be shared with `bravo`?")
                   (:who dave :at 700 :text "Later, in a separate pull request.")))
      (approve-fixtures--add-thread
       "pkg/foxtrot/file_00.go" :side "LEFT"
       :line (funcall line "pkg/foxtrot/file_00.go" "LEFT" 1 0)
       :comments '((:who bob :at 602 :review bob-1 :text "Was this check redundant?")))
      (approve-fixtures--add-thread
       "pkg/foxtrot/file_01.go" :start-line (funcall line "pkg/foxtrot/file_01.go" "RIGHT" 2 1)
       :line (funcall line "pkg/foxtrot/file_01.go" "RIGHT" 2 3) :resolved-by 'dave
       :comments '((:who bob :at 603 :review bob-1 :text "Use the shared helper here.")
                   (:who dave :at 710 :text "Done.")))
      (approve-fixtures--add-thread
       "pkg/mike/file_19.go" :outdated t :original-line 12
       :comments '((:who bob :at 604 :review bob-1 :text "Is this still needed?")))
      (approve-fixtures--add-thread
       "pkg/oscar/file_19.go" :line (funcall line "pkg/oscar/file_19.go" "RIGHT" 1 4)
       :comments '((:who carol :at 901 :review carol-1 :text "Nice cleanup."))))
    (let* ((commits (list (approve-fixtures--pr-commit
                           (approve-fixtures--commit "1" "Split the services into packages"
                                                     'dave -300 "SUCCESS"))
                          (approve-fixtures--pr-commit
                           (approve-fixtures--commit "2" "Move the handlers" 'dave -200
                                                     "SUCCESS"))
                          (approve-fixtures--pr-commit
                           (approve-fixtures--commit "3" "Add the generated schemas" 'dave 800
                                                     "SUCCESS"))))
           (rollup (approve-fixtures--rollup "rollup/3" "SUCCESS"))
           (contexts (list (approve-fixtures--check-run "build" "COMPLETED" "SUCCESS" 801
                                                        "CI" t)))
           (pull-request
            (approve-fixtures--pull-request
             :title "Split the monolith into packages" :state "OPEN" :at 0 :updated 901
             :body "Moves every service into its own package.  Mostly mechanical."
             :base-ref "main" :head-ref "split-packages"
             :labels (list (approve-fixtures--label "refactor" "d4c5f9" "Code changes without new behaviour"))
             :opinionated '(carol-1) :decision "APPROVED"
             :mergeable "MERGEABLE" :merge-state "CLEAN"
             :rollup rollup :contexts contexts :commits commits))
           (timeline
            (append
             (list (cons -300 (nth 0 commits))
                   (cons -200 (nth 1 commits))
                   (cons 800 (nth 2 commits))
                   (cons 10 (approve-fixtures--issue-comment
                             'dave 10 "The diff is big but each file changes the same way.")))
             (approve-fixtures--review-items))))
      (approve-fixtures--write
       "A large pull request: 300 files over three GraphQL pages and four REST pages (the last empty), one file too large for a patch, one binary, a handful of threads."
       (approve-fixtures--scenario-exchanges
        :pull-request pull-request :commits commits :timeline timeline
        :checks-rollup rollup :contexts contexts)))))

;;; Entry point

(defun approve-fixtures-generate ()
  "Write the small, medium and large scenarios."
  (approve-fixtures--small)
  (approve-fixtures--medium)
  (approve-fixtures--large))

(provide 'approve-fixtures-generate)

;;; generate.el ends here
