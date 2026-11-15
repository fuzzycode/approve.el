;;; approve-manage.el --- Manage a pull request: its labels, people and milestone -*- lexical-binding: t; -*-

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

;; The commands of a PR buffer that change what GitHub shows at the top
;; and in the sidebar of a pull request (docs/flows.org, F08 and F12;
;; docs/ui.org, "Metadata pickers"), each a run of the action pipeline
;; (F00):
;;
;;   t   `approve-edit-title'          the title, on one line
;;   d   `approve-edit-description'    the description (e on it too)
;;   l   `approve-set-labels'          the labels
;;   a   `approve-set-assignees'       who is assigned
;;   A   `approve-request-reviewers'   who is asked for a review
;;   M   `approve-set-milestone'       the milestone, or none
;;   D   `approve-toggle-draft'        a draft, or ready for review
;;   w   `approve-set-subscription'    how you hear of it
;;   #   `approve-toggle-lock'         lock or unlock the conversation
;;                                     (in the dispatch menu only)
;;
;; They work anywhere in the buffer, and RET on the Labels, Assignees,
;; Reviewers and Milestone rows runs the one of the row
;; (`approve-visit-row-commands').  The title and the description are
;; written in an input buffer (approve-input.el), starting from what
;; they are.  A picker offers the repository's
;; labels, people, teams or milestones (approve-metadata.el, decision
;; D24), fetched the first time and waited for.  The ones the pull
;; request has are chosen already, and every candidate says what it
;; is: a label's colour and description, a person's name, why GitHub
;; suggests a reviewer.  Choosing the same as before sends nothing.
;; Evil users reach them through the dispatch menu (?).
;;
;; Layer rules: this is the actions layer.  The commands read the
;; store and the repository's metadata, ask, and leave the rest to the
;; pipeline and the mutations, which keep the store true (decision
;; D35).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'approve-model)
(require 'approve-faces)
(require 'approve-input)
(require 'approve-metadata)
(require 'approve-mutations)
(require 'approve-actions)
(require 'approve-pr)
(require 'approve-nav)

;;; Pickers

(defun approve-manage--annotator (candidates notes)
  "Return an annotation function giving the NOTES of CANDIDATES.
NOTES is an alist (CANDIDATE . TEXT); the texts start in one column,
after the longest of CANDIDATES."
  (let ((width (apply #'max 0 (mapcar #'string-width candidates))))
    (lambda (candidate)
      (when-let* ((note (cdr (assoc candidate notes))))
        (concat (make-string (max 2 (- (+ width 2) (string-width candidate))) ?\s)
                note)))))

(defun approve-manage-table (candidates notes category)
  "Return a completion table of CANDIDATES, kept in their order.
NOTES annotate them (see `approve-manage--annotator'); CATEGORY is the
completion category, such as `approve-label'.  The pickers of this
file and the other commands that offer a choice use it.

Example:
  (completing-read \"Label: \" (approve-manage-table
                               \\='(\"bug\") \\='((\"bug\" . \"Broken\")) \\='approve-label))"
  (let ((annotate (approve-manage--annotator candidates notes)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata (category . ,category)
                     (annotation-function . ,annotate)
                     (display-sort-function . identity)
                     (cycle-sort-function . identity))
        (complete-with-action action candidates string predicate)))))

(cl-defun approve-manage--read-many (prompt candidates current &key notes category)
  "Read some of CANDIDATES, strings, with PROMPT; return those chosen.
CURRENT, the ones chosen to start with, are in the input already,
separated by commas; removing one there leaves it out.  NOTES, an
alist (CANDIDATE . TEXT), says what each is; CATEGORY is the
completion category.  Each chosen one is returned once, in the order
chosen.

Example:
  (approve-manage--read-many \"Labels\" \\='(\"bug\" \"docs\") \\='(\"bug\"))"
  (delete-dups
   (approve-input-with-submit-key
     (completing-read-multiple
      (format "%s: " prompt)
      (approve-manage-table candidates notes category)
      nil t (and current (concat (string-join current ",") ","))))))

(cl-defun approve-manage-read-one (prompt candidates &key notes category default)
  "Read one of CANDIDATES, strings, with PROMPT, the whole prompt.
NOTES and CATEGORY are as for `approve-manage-table'; DEFAULT is what
\\`RET' alone chooses.  \\<approve-input-minibuffer-map>As in every prompt of Approve,
\\[approve-input-minibuffer-submit] chooses too (`approve-input-with-submit-key').

Example:
  (approve-manage-read-one \"Notifications: \" \\='(\"Subscribed\" \"Ignored\"))"
  (approve-input-with-submit-key
    (completing-read prompt (approve-manage-table candidates notes category)
                     nil t nil nil default)))

(defun approve-manage-note (&rest parts)
  "Return PARTS, strings or nil, joined by two spaces, as a dim note."
  (approve-face (string-join (delq nil parts) "  ") 'approve-dim))

(defun approve-manage-mark (current)
  "Return the mark of a candidate the PR has, when CURRENT, else a space."
  (if current (approve-face "✓" 'approve-approved) " "))

(defun approve-manage--login (actor)
  "Return the candidate of ACTOR, a user or a bot: @login."
  (concat "@" (alist-get 'login actor)))

(defun approve-manage--unchanged (what)
  "Say that nothing changes in WHAT, such as \"the labels\"."
  (message "Approve: no change to %s" what))

(defun approve-manage--ids (chosen pairs)
  "Return the ids of CHOSEN, candidates, from PAIRS, an alist (CANDIDATE . ID)."
  (delq nil (mapcar (lambda (candidate) (cdr (assoc candidate pairs))) chosen)))

;;; Title and description (t, d)

(defun approve-manage--update-preflight ()
  "Return the context where the title or description can be edited.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not edit it."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'update)
      (user-error "%s" "You can't edit this pull request"))))

(put 'approve-edit-title 'approve-preflight #'approve-manage--update-preflight)
(put 'approve-edit-description 'approve-preflight #'approve-manage--update-preflight)

(defun approve-manage--markdown (text)
  "Return TEXT, markdown from GitHub, with Unix line ends and no trailing space."
  (string-trim-right (replace-regexp-in-string "\r\n" "\n" (or text ""))))

(defun approve-edit-title ()
  "Edit the title of the pull request, on one line (flow F08).
It starts as it is; sending it unchanged sends nothing."
  (interactive)
  (approve-manage--update-preflight)
  (let ((title (approve-pr-field 'title))
        (number (approve-pr-field 'number)))
    (approve-action-run
     :what "save the title"
     :input (list :prompt "New title" :context (format "#%s" number)
                  :target '(title) :name (format "title #%s" number)
                  :initial title :single-line t :commit-label "save")
     :send (lambda (text callback errorback)
             (if (equal text title)
                 (progn (approve-manage--unchanged "the title")
                        (funcall callback nil))
               (approve-mutation-set-title text :callback callback :errorback errorback))))))

(defun approve-edit-description ()
  "Edit the description of the pull request (flow F08).
It starts from its markdown; sending it unchanged sends nothing, and
sending it empty empties it."
  (interactive)
  (approve-manage--update-preflight)
  (let ((body (approve-manage--markdown (approve-pr-field 'body)))
        (number (approve-pr-field 'number)))
    (approve-action-run
     :what "save the description"
     :input (list :prompt "Edit the description" :context (format "#%s" number)
                  :target '(description) :name (format "description #%s" number)
                  :initial body :allow-empty t :commit-label "save")
     :marks '((section approve-description-section nil))
     :send (lambda (text callback errorback)
             (if (equal text body)
                 (progn (approve-manage--unchanged "the description")
                        (funcall callback nil))
               (approve-mutation-set-description text
                                                 :callback callback :errorback errorback))))))

;;; Labels (l)

(defun approve-manage--labels-preflight ()
  "Return the context where `approve-set-labels' runs.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not label it."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'label)
      (user-error "%s" "You can't change the labels of this pull request"))))

(put 'approve-set-labels 'approve-preflight #'approve-manage--labels-preflight)

(defun approve-manage--label-note (label current)
  "Return the note of LABEL in the picker: mark, colour and description.
CURRENT is non-nil when the PR has LABEL."
  (let-alist label
    (concat (approve-manage-mark current) " "
            (approve-face "    " (approve-label-face .color)) " "
            (approve-manage-note .description))))

(defun approve-set-labels ()
  "Choose the labels of the pull request (flow F12).
The repository's labels are offered, with the pull request's already
chosen; remove one from the input to take it away."
  (interactive)
  (approve-manage--labels-preflight)
  (let* ((current (approve-pr-field 'labels))
         (had (mapcar (lambda (label) (alist-get 'name label)) current))
         (labels (sort (append current
                               (seq-remove (lambda (label)
                                             (member (alist-get 'name label) had))
                                           (approve-metadata-wait 'labels)))
                       (lambda (a b) (string-collate-lessp (alist-get 'name a)
                                                           (alist-get 'name b) nil t))))
         (names (mapcar (lambda (label) (alist-get 'name label)) labels))
         (chosen (approve-manage--read-many
                  "Labels" names had
                  :category 'approve-label
                  :notes (mapcar (lambda (label)
                                   (cons (alist-get 'name label)
                                         (approve-manage--label-note
                                          label (member (alist-get 'name label) had))))
                                 labels)))
         (ids (mapcar (lambda (label) (cons (alist-get 'name label) (alist-get 'id label)))
                      labels))
         (add (approve-manage--ids (seq-difference chosen had) ids))
         (remove (approve-manage--ids (seq-difference had chosen) ids)))
    (if (not (or add remove))
        (approve-manage--unchanged "the labels")
      (approve-action-run
       :what "change the labels"
       :marks '((section approve-meta-section labels))
       :send (lambda (_text callback errorback)
               (approve-mutation-set-labels add remove
                                            :callback callback :errorback errorback))))))

;;; Assignees (a)

(defun approve-manage--assignees-preflight ()
  "Return the context where `approve-set-assignees' runs.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not assign people to it."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'assign)
      (user-error "%s" "You can't assign people to this pull request"))))

(put 'approve-set-assignees 'approve-preflight #'approve-manage--assignees-preflight)

(defun approve-manage--people (current more)
  "Return the people of CURRENT, then those of MORE not in it, each once.
Both are lists of user alists with `login'."
  (let ((seen (mapcar (lambda (user) (alist-get 'login user)) current)))
    (append current
            (seq-remove (lambda (user) (member (alist-get 'login user) seen)) more))))

(defun approve-set-assignees ()
  "Choose who is assigned to the pull request (flow F12).
The people who can be assigned in the repository are offered, with
the pull request's assignees already chosen."
  (interactive)
  (approve-manage--assignees-preflight)
  (let* ((current (approve-pr-field 'assignees))
         (people (approve-manage--people current (approve-metadata-wait 'assignable-users)))
         (had (mapcar #'approve-manage--login current))
         (chosen (approve-manage--read-many
                  "Assignees" (mapcar #'approve-manage--login people) had
                  :category 'approve-user
                  :notes (mapcar (lambda (user)
                                   (let ((login (approve-manage--login user)))
                                     (cons login
                                           (concat (approve-manage-mark
                                                    (member login had))
                                                   " "
                                                   (approve-manage-note
                                                    (alist-get 'name user))))))
                                 people)))
         (ids (mapcar (lambda (user) (cons (approve-manage--login user) (alist-get 'id user)))
                      people))
         (add (approve-manage--ids (seq-difference chosen had) ids))
         (remove (approve-manage--ids (seq-difference had chosen) ids)))
    (if (not (or add remove))
        (approve-manage--unchanged "the assignees")
      (approve-action-run
       :what "change the assignees"
       :marks '((section approve-meta-section assignees))
       :send (lambda (_text callback errorback)
               (approve-mutation-set-assignees add remove
                                               :callback callback :errorback errorback))))))

;;; Reviewers (A)

(defun approve-manage--reviewers-preflight ()
  "Return the context where `approve-request-reviewers' runs.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not change it.  GitHub says no more of who may ask for a
review; it refuses those who may not."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'update)
      (user-error "%s" "You can't ask for reviews of this pull request"))))

(put 'approve-request-reviewers 'approve-preflight #'approve-manage--reviewers-preflight)

(defun approve-manage--requested ()
  "Return who is asked for a review now, as (CANDIDATE KIND . ID).
KIND is `user', `team' or `bot'."
  (delq nil
        (mapcar (lambda (request)
                  (let-alist (alist-get 'requestedReviewer request)
                    (pcase .__typename
                      ("User" (cons (concat "@" .login) (cons 'user .id)))
                      ("Bot" (cons (concat "@" .login) (cons 'bot .id)))
                      ("Team" (cons (concat "@" .combinedSlug) (cons 'team .id))))))
                (approve-pr-field 'reviewRequests))))

(defun approve-manage--verdicts ()
  "Return each reviewer's latest verdict, an alist (LOGIN . STATE)."
  (mapcar (lambda (review)
            (cons (let-alist review .author.login) (alist-get 'state review)))
          (approve-pr-field 'latestOpinionatedReviews)))

(defun approve-manage--verdict-note (state)
  "Return what the latest review STATE of a reviewer says, or nil."
  (pcase state
    ("APPROVED" "approved")
    ("CHANGES_REQUESTED" "requested changes")
    ("COMMENTED" "commented")
    ("DISMISSED" "dismissed")))

(defun approve-manage--reviewer-candidates ()
  "Return the reviewers to offer, as (CANDIDATE KIND ID NOTE), in order.
GitHub's suggestions come first, then who is asked now, then the
people who can be assigned in the repository, then its owner's
teams.  The PR's author is never one: GitHub refuses."
  (let* ((author (alist-get 'login (approve-pr-field 'author)))
         (requested (approve-manage--requested))
         (verdicts (approve-manage--verdicts))
         (suggested (seq-remove (lambda (suggestion) (alist-get 'isAuthor suggestion))
                                (approve-pr-field 'suggestedReviewers)))
         (found nil))
    (cl-flet ((add (candidate kind id &rest notes)
                (unless (or (null id) (assoc candidate found)
                            (equal candidate (concat "@" author)))
                  (push (list candidate kind id
                              (concat (approve-manage-mark
                                       (assoc candidate requested))
                                      " "
                                      (apply #'approve-manage-note notes)))
                        found))))
      (dolist (suggestion suggested)
        (let-alist suggestion
          (add (concat "@" .reviewer.login) 'user .reviewer.id .reviewer.name
               (if .isCommenter "suggested, commented" "suggested")
               (approve-manage--verdict-note (cdr (assoc .reviewer.login verdicts))))))
      (dolist (request requested)
        (add (car request) (cadr request) (cddr request)))
      (dolist (user (approve-metadata-wait 'assignable-users))
        (let-alist user
          (add (concat "@" .login) 'user .id .name
               (approve-manage--verdict-note (cdr (assoc .login verdicts))))))
      (dolist (team (approve-metadata-wait 'teams))
        (let-alist team
          (add (concat "@" .combinedSlug) 'team .id .name))))
    (nreverse found)))

(defun approve-request-reviewers ()
  "Choose who is asked to review the pull request (flow F12).
GitHub's suggestions are offered first, then the people who can be
assigned in the repository and the teams of its organization, with
those asked now already chosen.  Remove one from the input to ask
them no more; choosing someone who reviewed asks them again."
  (interactive)
  (approve-manage--reviewers-preflight)
  ;; The teams load while the people are waited for.
  (approve-metadata-fetch 'teams)
  (let* ((candidates (approve-manage--reviewer-candidates))
         (requested (approve-manage--requested))
         (had (mapcar #'car requested))
         (chosen (approve-manage--read-many
                  "Reviewers" (mapcar #'car candidates) had
                  :category 'approve-user
                  :notes (mapcar (lambda (candidate)
                                   (cons (car candidate) (nth 3 candidate)))
                                 candidates)))
         (picked (delq nil (mapcar (lambda (candidate) (assoc candidate candidates))
                                   chosen)))
         (ids (lambda (kind)
                (delq nil (mapcar (lambda (entry) (and (eq (nth 1 entry) kind) (nth 2 entry)))
                                  picked)))))
    (if (and (null (seq-difference chosen had)) (null (seq-difference had chosen)))
        (approve-manage--unchanged "the review requests")
      (approve-action-run
       :what "ask for reviews"
       :marks '((section approve-meta-section reviewers))
       :send (lambda (_text callback errorback)
               (approve-mutation-request-reviews
                :users (funcall ids 'user) :teams (funcall ids 'team)
                :bots (funcall ids 'bot)
                :callback callback :errorback errorback))))))

;;; Milestone (M)

(defun approve-manage--milestone-preflight ()
  "Return the context where `approve-set-milestone' runs.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not change it."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'update)
      (user-error "%s" "You can't change the milestone of this pull request"))))

(put 'approve-set-milestone 'approve-preflight #'approve-manage--milestone-preflight)

(defconst approve-manage--no-milestone "No milestone"
  "The candidate that takes the milestone away.")

(defun approve-manage--milestones-in-order (milestones)
  "Return MILESTONES with a due date first, the one due first first.
The others follow in the order they came."
  (let ((dated (seq-filter (lambda (milestone) (alist-get 'dueOn milestone)) milestones)))
    (append (sort (copy-sequence dated)
                  (lambda (a b) (string< (alist-get 'dueOn a) (alist-get 'dueOn b))))
            (seq-remove (lambda (milestone) (alist-get 'dueOn milestone)) milestones))))

(defun approve-manage--milestone-note (milestone current)
  "Return the note of MILESTONE in the picker: due date and progress.
CURRENT is non-nil when the PR is in MILESTONE."
  (let-alist milestone
    (concat (approve-manage-mark current) " "
            (approve-manage-note
             (if .dueOn (format "due %s" (substring .dueOn 0 10)) "no due date")
             (and .progressPercentage (format "%d%% done" (round .progressPercentage)))
             (and (equal .state "CLOSED") "closed")
             .description))))

(defun approve-set-milestone ()
  "Choose the milestone of the pull request, or none (flow F12).
The repository's open milestones are offered, those due first first."
  (interactive)
  (approve-manage--milestone-preflight)
  (let* ((current (approve-pr-field 'milestone))
         (title (alist-get 'title current))
         (milestones (approve-manage--milestones-in-order
                      (approve-metadata-wait 'milestones)))
         (milestones (if (and current (not (seq-find (lambda (milestone)
                                                        (equal (alist-get 'title milestone)
                                                               title))
                                                      milestones)))
                         (cons current milestones)
                       milestones))
         (titles (append (and current (list approve-manage--no-milestone))
                         (mapcar (lambda (milestone) (alist-get 'title milestone))
                                 milestones)))
         (choice (approve-manage-read-one
                  (format-prompt "Milestone" title) titles
                  :notes (mapcar (lambda (milestone)
                                   (cons (alist-get 'title milestone)
                                         (approve-manage--milestone-note
                                          milestone
                                          (equal (alist-get 'title milestone) title))))
                                 milestones)
                  :category 'approve-milestone :default title))
         (milestone (seq-find (lambda (milestone) (equal (alist-get 'title milestone) choice))
                              milestones)))
    (cond
     ((or (equal choice title) (and (null current) (string-empty-p choice)))
      (approve-manage--unchanged "the milestone"))
     (t
      (approve-action-run
       :what (if milestone "set the milestone" "take the milestone away")
       :marks '((section approve-meta-section milestone))
       :send (lambda (_text callback errorback)
               (approve-mutation-set-milestone (alist-get 'id milestone)
                                               :callback callback :errorback errorback)))))))

;;; Draft or ready (D)

(defun approve-manage--draft-preflight ()
  "Return the context where `approve-toggle-draft' runs.
Signal a `user-error' when the pull request has not loaded, is not
open, or the viewer may not change it."
  (prog1 (approve-action-context)
    (unless (equal (approve-pr-field 'state) "OPEN")
      (user-error "%s" "Only an open pull request can be a draft"))
    (unless (approve-pr-viewer-can 'update)
      (user-error "%s" "You can't change this pull request"))))

(put 'approve-toggle-draft 'approve-preflight #'approve-manage--draft-preflight)

(defun approve-toggle-draft ()
  "Mark the draft pull request ready for review, or make it a draft (flow F12).
It asks first: marking it ready tells its reviewers."
  (interactive)
  (approve-manage--draft-preflight)
  (let ((draft (approve-pr-field 'isDraft))
        (number (approve-pr-field 'number)))
    (when (y-or-n-p (if draft
                        (format "Mark #%s ready for review? " number)
                      (format "Convert #%s to a draft? " number)))
      (approve-action-run
       :what (if draft "mark it ready for review" "convert it to a draft")
       :send (lambda (_text callback errorback)
               (approve-mutation-set-draft (not draft)
                                           :callback callback :errorback errorback))
       :done (lambda (_pr)
               (message "Approve: #%s is %s" number
                        (if draft "ready for review" "a draft")))))))

;;; Subscription (w)

(defun approve-manage--subscription-preflight ()
  "Return the context where `approve-set-subscription' runs.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not subscribe to it."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'subscribe)
      (user-error "%s" "You can't subscribe to this pull request"))))

(put 'approve-set-subscription 'approve-preflight #'approve-manage--subscription-preflight)

(defconst approve-manage--subscriptions
  '(("SUBSCRIBED" "Subscribed" "all of its activity")
    ("UNSUBSCRIBED" "Participating and @mentions" "when you take part or are mentioned")
    ("IGNORED" "Ignored" "never"))
  "How one can hear of a pull request: (STATE NAME WHEN).")

(defun approve-set-subscription ()
  "Choose how you hear of the pull request (flow F12).
Subscribed: all of its activity; participating and @mentions: when
you take part or are mentioned; ignored: never.  GitHub wants the
token to have the notifications scope for it."
  (interactive)
  (approve-manage--subscription-preflight)
  (let* ((current (approve-pr-field 'viewerSubscription))
         (name (nth 1 (assoc current approve-manage--subscriptions)))
         (choice (approve-manage-read-one
                  (format-prompt "Notifications" name)
                  (mapcar #'cadr approve-manage--subscriptions)
                  :notes (mapcar (lambda (entry)
                                   (cons (nth 1 entry)
                                         (concat (approve-manage-mark
                                                  (equal (car entry) current))
                                                 " " (approve-manage-note (nth 2 entry)))))
                                 approve-manage--subscriptions)
                  :category 'approve-subscription :default name))
         (state (car (seq-find (lambda (entry) (equal (nth 1 entry) choice))
                               approve-manage--subscriptions))))
    (if (or (null state) (equal state current))
        (approve-manage--unchanged "the subscription")
      (approve-action-run
       :what "change the subscription"
       :send (lambda (_text callback errorback)
               (approve-mutation-set-subscription state
                                                  :callback callback :errorback errorback))
       :done (lambda (_pr)
               (message "Approve: notifications of #%s: %s" (approve-pr-field 'number)
                        (downcase choice)))))))

;;; Lock (#, in the dispatch menu)

(defun approve-manage--lock-preflight ()
  "Return the context where `approve-toggle-lock' runs.
Signal a `user-error' when the pull request has not loaded, or the
viewer may not change it.  GitHub says no more of who may lock it;
it refuses those who may not."
  (prog1 (approve-action-context)
    (unless (approve-pr-viewer-can 'update)
      (user-error "%s" "You can't lock this conversation"))))

(put 'approve-toggle-lock 'approve-preflight #'approve-manage--lock-preflight)

(defconst approve-manage--lock-reasons
  '(("No reason") ("Off topic" . "OFF_TOPIC") ("Too heated" . "TOO_HEATED")
    ("Resolved" . "RESOLVED") ("Spam" . "SPAM"))
  "The reasons one can give for locking a conversation: (NAME . REASON).")

(defun approve-toggle-lock ()
  "Lock the conversation of the pull request, or unlock it.
Locking asks for a reason, which may be none; once it is locked,
only collaborators can comment.  Unlocking asks first."
  (interactive)
  (approve-manage--lock-preflight)
  (let ((locked (approve-pr-field 'locked))
        (number (approve-pr-field 'number)))
    (when (or (not locked)
              (y-or-n-p (format "Unlock the conversation of #%s? " number)))
      (let ((reason (unless locked
                      (cdr (assoc (approve-manage-read-one
                                   "Lock the conversation, because: "
                                   (mapcar #'car approve-manage--lock-reasons)
                                   :default "No reason")
                                  approve-manage--lock-reasons)))))
        (approve-action-run
         :what (if locked "unlock the conversation" "lock the conversation")
         :marks '((section approve-conversation-section nil))
         :send (lambda (_text callback errorback)
                 (approve-mutation-set-locked (not locked) :reason reason
                                              :callback callback :errorback errorback))
         :done (lambda (_pr)
                 (message "Approve: the conversation of #%s is %s" number
                          (if locked "unlocked" "locked"))))))))

;;; Keys

;; RET on these metadata rows changes them, as their keys do anywhere.
(setf (alist-get 'labels approve-visit-row-commands) #'approve-set-labels
      (alist-get 'assignees approve-visit-row-commands) #'approve-set-assignees
      (alist-get 'reviewers approve-visit-row-commands) #'approve-request-reviewers
      (alist-get 'milestone approve-visit-row-commands) #'approve-set-milestone)

(keymap-set approve-pr-mode-map "t" #'approve-edit-title)
(keymap-set approve-pr-mode-map "d" #'approve-edit-description)
(keymap-set approve-pr-mode-map "l" #'approve-set-labels)
(keymap-set approve-pr-mode-map "a" #'approve-set-assignees)
(keymap-set approve-pr-mode-map "A" #'approve-request-reviewers)
(keymap-set approve-pr-mode-map "M" #'approve-set-milestone)
(keymap-set approve-pr-mode-map "D" #'approve-toggle-draft)
(keymap-set approve-pr-mode-map "w" #'approve-set-subscription)

(provide 'approve-manage)

;;; approve-manage.el ends here
