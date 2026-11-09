;;; approve-html.el --- Render GitHub's bodyHTML -*- lexical-binding: t; -*-

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

;; Descriptions and comments are shown from GitHub's `bodyHTML'
;; (docs/decisions.org, D11), which already resolves mentions, issue
;; links, emoji and suggestions.  This file renders it with `shr' and
;; tag handlers of its own:
;;
;; - code blocks, coloured from GitHub's syntax classes (pl-*);
;; - suggestions, drawn as a small diff in a box;
;; - tables with box borders, lists, task lists, quotes and alerts;
;; - <details>, which become collapsible sections;
;; - images, from a cache that the buffer fills (see below);
;; - links, which carry their URL, and for mentions, issue links and
;;   commit links what they point to (`approve-thing').
;;
;;   (approve-html-insert "<p>Fixes <code>x</code></p>")
;;
;; Rendering goes to a string, at column 0 and at most WIDTH columns
;; wide; callers indent it.  Strings are cached by their HTML and
;; width, so a redraw costs only an insertion.  `approve-html-insert'
;; turns the <details> of the string into magit sections when it runs
;; inside one.
;;
;; Images: a renderer never calls the network.  An image that is not
;; in the cache is drawn as a placeholder, and its URL is added to
;; `approve-html-wanted-images'; the buffer fetches it, gives it to
;; `approve-html-put-image' and redraws.
;;
;; Layer rules: this is the renderer layer.  It reads its own image
;; cache, never the store, and never calls the network.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'shr)
(require 'subr-x)
(require 'magit-section)
(require 'approve-faces)

;;; Options

(defcustom approve-fill-column 80
  "Column at which comments and descriptions are wrapped.
Indentation comes on top of it, so a line of a comment in a thread
is at most this wide plus the thread's indentation.  For example:

  (setq approve-fill-column 100)"
  :type 'natnum
  :group 'approve)

(defcustom approve-load-images t
  "Whether images in comments and descriptions are shown.
When nil, or in a terminal, each image is a link with its alt text
that `o' opens.  GitHub serves images through its camo proxy, so
loading them reveals nothing but that someone looked.  To turn
them off:

  (setq approve-load-images nil)"
  :type 'boolean
  :group 'approve)

(defcustom approve-html-quote-bar "▌ "
  "Text drawn left of each line of quoted text."
  :type 'string
  :group 'approve)

;;; Caches

(defvar approve-html--cache (make-hash-table :test #'equal)
  "Rendered strings by (WIDTH . HTML).")

(defconst approve-html--cache-limit 4000
  "Number of rendered strings kept before the cache is emptied.")

(defvar approve-html--images (make-hash-table :test #'equal)
  "Images by URL: an image descriptor, `loading' or `failed'.")

(defvar approve-html-wanted-images nil
  "URLs of images rendered while not in the cache, newest first.
The buffer that renders takes them with
`approve-html-take-wanted-images', fetches them and gives each to
`approve-html-put-image'.")

(defun approve-html-clear-cache ()
  "Forget rendered strings and images."
  (clrhash approve-html--cache)
  (clrhash approve-html--images))

(defun approve-html-take-wanted-images ()
  "Return the URLs of images wanted since the last call, and forget them.
They count as loading from now on: the caller fetches them and gives
each to `approve-html-put-image'."
  (let ((urls (delete-dups (nreverse approve-html-wanted-images))))
    (setq approve-html-wanted-images nil)
    (dolist (url urls)
      (puthash url 'loading approve-html--images))
    urls))

(defun approve-html-put-image (url data)
  "Remember DATA, the bytes of the image at URL, or that it failed.
DATA nil means the image could not be loaded; it is not wanted again
until `approve-html-clear-cache'.  Return non-nil if an image was
made."
  (let ((image (and data
                    (display-images-p)
                    (ignore-errors
                      (create-image data nil t
                                    :max-width (* approve-fill-column
                                                  (frame-char-width))
                                    :ascent 'center)))))
    (puthash url (or image 'failed) approve-html--images)
    (and image t)))

;;; DOM helpers

(defun approve-html--classes (dom)
  "Return the CSS classes of DOM, a list of strings."
  (let ((class (dom-attr dom 'class)))
    (and (stringp class) (split-string class nil t))))

(defun approve-html--class-p (dom class)
  "Return non-nil if DOM has the CSS CLASS."
  (and (member class (approve-html--classes dom)) t))

(defun approve-html--text (dom)
  "Return the text of DOM and its children, as one string."
  (cond ((stringp dom) dom)
        ((null dom) "")
        (t (mapconcat #'approve-html--text (dom-children dom) ""))))

(defun approve-html--add-face (start end face)
  "Add FACE under the faces between START and END."
  (when (< start end)
    (add-face-text-property start end face t)))

(defmacro approve-html--with-face (face &rest body)
  "Run BODY, then add FACE to what it inserted."
  (declare (indent 1) (debug t))
  (let ((start (gensym "start")))
    `(let ((,start (point)))
       ,@body
       (approve-html--add-face ,start (point) ,face))))

;;; Rendering into a string

(defvar approve-html--width 80
  "Width the current rendering fills to.")

(defvar approve-html--in-details nil
  "Non-nil while rendering the body of a <details>.")

(defvar approve-html--details-count 0
  "Number of <details> rendered so far in the current string.")

(defconst approve-html--handlers
  '((a . approve-html--tag-a)
    (blockquote . approve-html--tag-blockquote)
    (code . approve-html--tag-code)
    (del . approve-html--tag-del)
    (s . approve-html--tag-del)
    (strike . approve-html--tag-del)
    (details . approve-html--tag-details)
    (div . approve-html--tag-div)
    (h1 . approve-html--tag-heading)
    (h2 . approve-html--tag-heading)
    (h3 . approve-html--tag-heading)
    (h4 . approve-html--tag-heading)
    (h5 . approve-html--tag-heading)
    (h6 . approve-html--tag-heading)
    (hr . approve-html--tag-hr)
    (img . approve-html--tag-img)
    (input . ignore)
    (li . approve-html--tag-li)
    (ol . approve-html--tag-ol)
    (pre . approve-html--tag-pre)
    (summary . ignore)
    (svg . ignore)
    (table . approve-html--tag-table)
    (ul . approve-html--tag-ul))
  "The tag handlers used instead of shr's own.")

(defun approve-html--parse (html)
  "Return the DOM of HTML."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun approve-html--render-dom (dom width)
  "Render DOM into the current buffer at point, filling to WIDTH."
  (let ((shr-width width)
        (shr-max-width nil)
        (shr-use-fonts nil)
        (shr-use-colors nil)
        (shr-inhibit-images t)
        (shr-indentation 0)
        (shr-bullet "• ")
        (shr-discard-aria-hidden t)
        (shr-external-rendering-functions approve-html--handlers)
        (approve-html--width width))
    (shr-insert-document dom)))

(defun approve-html--subrender (dom width)
  "Return the children of DOM rendered as a document WIDTH wide.
The string has no trailing newline; tags that make sections are
rendered inline, since the string is inserted as it is."
  (with-temp-buffer
    (let ((approve-html--in-details t))
      (approve-html--render-dom `(div nil ,@(dom-children dom))
                                (max 10 width)))
    (approve-html--clean-up)
    (string-trim-right (buffer-string) "\n+")))

(defun approve-html--insert-block (text prefix &optional face)
  "Insert TEXT, a rendered block, one line at a time with PREFIX.
Each line starts at the current indentation (`shr-indentation'),
then PREFIX, then the line.  FACE, if non-nil, goes under the whole
line, from the prefix on.  The lines are complete: shr does not fill
them again."
  (shr-ensure-newline)
  (let ((indent (make-string shr-indentation ?\s)))
    (dolist (line (split-string text "\n"))
      (let ((start (point)))
        (insert indent)
        (let ((line-start (point)))
          (insert prefix line)
          (when face
            (approve-html--add-face line-start (point) face)))
        (insert "\n")
        (put-text-property start (point) 'shr-indentation nil)))))

(defun approve-html--clean-up ()
  "Tidy the rendered buffer: spaces at line ends, runs of blank lines.
Faces are mirrored into `font-lock-face'."
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (unless (get-text-property (match-beginning 0) 'display)
      (replace-match "")))
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n"))
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point))
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'face nil (point-max)))
            (face (get-text-property pos 'face)))
        (when face
          (put-text-property pos next 'font-lock-face face))
        (setq pos next)))))

(defun approve-html-render (html &optional width)
  "Return HTML rendered as text with faces, WIDTH columns wide.
WIDTH defaults to `approve-fill-column'.  The text starts at column
0 and ends with a newline; it is empty when HTML has no content.
<details> are marked with the text properties `approve-html-details'
\(on the summary line) and `approve-html-details-body', which
`approve-html-insert' turns into sections.  Results are cached,
except for HTML with images, which change once loaded.

Example: (approve-html-render \"<p>Hello <b>you</b></p>\")"
  (let* ((width (or width approve-fill-column))
         (key (cons width html)))
    (cond
     ((or (null html) (string-blank-p html)) "")
     ((gethash key approve-html--cache))
     (t
      (let ((text (with-temp-buffer
                    (let ((approve-html--details-count 0))
                      (approve-html--render-dom (approve-html--parse html) width))
                    (approve-html--clean-up)
                    (goto-char (point-max))
                    (skip-chars-backward "\n")
                    (delete-region (point) (point-max))
                    (unless (bobp)
                      (insert "\n"))
                    (buffer-string))))
        (unless (string-match-p "<img" html)
          (when (> (hash-table-count approve-html--cache)
                   approve-html--cache-limit)
            (clrhash approve-html--cache))
          (puthash key text approve-html--cache))
        text)))))

;;; Inserting

(defun approve-html--next-details (text start)
  "Return the position of the next details summary in TEXT after START."
  (text-property-not-all start (length text) 'approve-html-details nil text))

(defun approve-html--insert-details (summary body details)
  "Insert the <details> with SUMMARY and BODY lines; DETAILS is its plist.
Inside a section being inserted, it becomes an `approve-html-details'
section, collapsed unless the HTML said open; elsewhere it is text."
  (if (bound-and-true-p magit-insert-section--current)
      (magit-insert-section (approve-html-details
                             (cons (plist-get details :index)
                                   (plist-get details :summary))
                             (not (plist-get details :open)))
        (magit-insert-heading summary)
        (insert body))
    (insert summary body)))

(cl-defun approve-html-insert (html &key width)
  "Insert HTML rendered at point, WIDTH columns wide.
WIDTH defaults to `approve-fill-column'.  The text starts at column
0 and ends with a newline; nothing is inserted when HTML has no
content.  Return non-nil if something was inserted.  Inside a magit
section, each <details> becomes a collapsible section.

Example: (approve-html-insert (alist-get \\='bodyHTML comment))"
  (let* ((text (approve-html-render html width))
         (pos 0))
    (while (< pos (length text))
      (let ((summary-start (approve-html--next-details text pos)))
        (if (null summary-start)
            (progn (insert (substring text pos))
                   (setq pos (length text)))
          (insert (substring text pos summary-start))
          (let* ((details (get-text-property summary-start 'approve-html-details text))
                 (summary-end (or (next-single-property-change
                                   summary-start 'approve-html-details text)
                                  (length text)))
                 (body-end (or (next-single-property-change
                                summary-end 'approve-html-details-body text)
                               (length text))))
            (unless (get-text-property summary-end 'approve-html-details-body text)
              (setq body-end summary-end))
            (approve-html--insert-details (substring text summary-start summary-end)
                                          (substring text summary-end body-end)
                                          details)
            (setq pos body-end)))))
    (> (length text) 0)))

;;; Tag handlers

(defun approve-html--link-thing (dom url)
  "Return what the link DOM to URL points to, as a plist, or nil.
Mentions give (:kind user :login L), team mentions (:kind team :slug
S), issue and pull request links (:kind ref :owner O :repo R :number
N), commit links (:kind commit :oid OID); each with :url URL."
  (cond
   ((approve-html--class-p dom "user-mention")
    (list :kind 'user :login (string-remove-prefix "@" (approve-html--text dom))
          :url url))
   ((approve-html--class-p dom "team-mention")
    (list :kind 'team :slug (string-remove-prefix "@" (approve-html--text dom))
          :url url))
   ((and (approve-html--class-p dom "issue-link")
         (string-match (rx "/" (group (+ (not "/"))) "/" (group (+ (not "/")))
                           "/" (or "issues" "pull") "/" (group (+ digit)))
                       url))
    (list :kind 'ref :owner (match-string 1 url) :repo (match-string 2 url)
          :number (string-to-number (match-string 3 url)) :url url))
   ((and (approve-html--class-p dom "commit-link")
         (string-match (rx "/commit/" (group (+ hex-digit))) url))
    (list :kind 'commit :oid (match-string 1 url) :url url))))

(defvar-keymap approve-html-link-map
  :doc "Keymap on links in rendered text: the mouse opens them."
  "<mouse-2>" #'approve-html-browse-url-at-mouse
  "<follow-link>" 'mouse-face)

(defun approve-html-browse-url-at-mouse (event)
  "Open the URL of the link clicked in EVENT."
  (interactive "e")
  (when-let* ((pos (posn-point (event-end event)))
              (url (get-text-property pos 'approve-url
                                      (window-buffer (posn-window (event-end event))))))
    (browse-url url)))

(defun approve-html-link-properties (url &optional thing)
  "Return the text properties of a link to URL.
THING, if non-nil, is what the link points to (`approve-thing')."
  `(approve-url ,url
    ,@(and thing (list 'approve-thing thing))
    mouse-face highlight
    help-echo ,url
    keymap ,approve-html-link-map))

(defun approve-html--tag-a (dom)
  "Render the link DOM: its text, with the URL and what it points to."
  (let ((url (dom-attr dom 'href))
        (start (point)))
    (unless (approve-html--class-p dom "anchor")
      (shr-generic dom)
      (when (and (stringp url) (not (string-empty-p url))
                 (not (string-prefix-p "#" url))
                 (< start (point)))
        (approve-html--add-face start (point) 'approve-link)
        (add-text-properties start (point)
                             (approve-html-link-properties
                              url (approve-html--link-thing dom url)))))))

(defun approve-html--tag-code (dom)
  "Render the inline code DOM."
  (approve-html--with-face 'approve-html-code
    (shr-generic dom)))

(defun approve-html--tag-del (dom)
  "Render the struck-through text DOM."
  (approve-html--with-face 'approve-html-strike
    (shr-generic dom)))

(defun approve-html--tag-heading (dom)
  "Render the heading DOM (h1 to h6) as a bold paragraph."
  (shr-ensure-paragraph)
  (approve-html--with-face 'approve-html-heading
    (shr-generic dom))
  (shr-ensure-paragraph))

(defun approve-html--tag-hr (_dom)
  "Render a horizontal rule as a dim line."
  (shr-ensure-newline)
  (approve-html--insert-block
   (make-string (max 3 (- approve-html--width shr-indentation)) ?─) ""
   'approve-dim)
  (shr-ensure-paragraph))

;;; Quotes and alerts

(defun approve-html--tag-blockquote (dom)
  "Render the quote DOM with a bar left of each line."
  (shr-ensure-paragraph)
  (approve-html--insert-block
   (approve-html--subrender dom (- approve-html--width shr-indentation
                                   (string-width approve-html-quote-bar)))
   (approve-face approve-html-quote-bar 'approve-html-blockquote-border)
   'approve-html-blockquote)
  (shr-ensure-paragraph))

(defun approve-html--tag-alert (dom)
  "Render the GitHub alert DOM ([!NOTE] and friends) as a titled quote."
  (shr-ensure-paragraph)
  (let* ((title-dom (seq-find (lambda (child)
                                (and (consp child)
                                     (approve-html--class-p child "markdown-alert-title")))
                              (dom-children dom)))
         (title (string-trim (approve-html--text title-dom)))
         (rest `(div nil ,@(remq title-dom (dom-children dom))))
         (bar (approve-face approve-html-quote-bar 'approve-html-blockquote-border))
         (text (approve-html--subrender
                rest (- approve-html--width shr-indentation
                        (string-width approve-html-quote-bar)))))
    (approve-html--insert-block (approve-face title 'approve-html-heading) bar)
    (approve-html--insert-block text bar))
  (shr-ensure-paragraph))

;;; Code

(defun approve-html--syntax-face (class)
  "Return the face for GitHub's syntax CLASS, such as \"pl-k\", or nil."
  (pcase (string-remove-prefix "pl-" class)
    ("c" 'font-lock-comment-face)
    ("c1" 'font-lock-constant-face)
    ("k" 'font-lock-keyword-face)
    ((or "s" "pds" "pse" "sr" "sre" "sra") 'font-lock-string-face)
    ((or "e" "en") 'font-lock-function-name-face)
    ("ent" 'font-lock-type-face)
    ((or "v" "smi" "vpf") 'font-lock-variable-name-face)
    ("mi" 'italic)
    ((or "mb" "mh" "ms") 'bold)
    ("mi1" 'approve-diff-added)
    ("md" 'approve-diff-removed)
    ("mdr" 'approve-diff-hunk-heading)
    ((or "ii" "bu" "ba" "mc") 'font-lock-warning-face)))

(defun approve-html--code-text (dom)
  "Return the text of the code DOM, with faces from its syntax classes."
  (cond
   ((stringp dom) dom)
   ((null dom) "")
   (t
    (let ((text (mapconcat #'approve-html--code-text (dom-children dom) ""))
          (face (seq-some #'approve-html--syntax-face
                          (seq-filter (lambda (class) (string-prefix-p "pl-" class))
                                      (approve-html--classes dom)))))
      (when face
        (setq text (copy-sequence text))
        (add-face-text-property 0 (length text) face t text))
      text))))

(defun approve-html--tag-pre (dom)
  "Render the code block DOM, keeping its lines, on a background."
  (shr-ensure-paragraph)
  (let ((text (string-trim-right (approve-html--code-text dom) "\n+")))
    (approve-html--insert-block (concat text) " " 'approve-html-code-block))
  (shr-ensure-paragraph))

;;; Suggestions

(defun approve-html--suggestion-line (cell refine)
  "Return the text of the suggestion CELL, changed words in face REFINE.
GitHub marks the changed words with spans of class x."
  (cond
   ((stringp cell) cell)
   ((null cell) "")
   (t
    (let ((text (mapconcat (lambda (child)
                             (approve-html--suggestion-line child refine))
                           (dom-children cell) "")))
      (when (approve-html--class-p cell "x")
        (setq text (copy-sequence text))
        (add-face-text-property 0 (length text) refine t text))
      text))))

(defun approve-html--tag-suggestion (dom)
  "Render the suggested change DOM as a small diff in a box."
  (shr-ensure-paragraph)
  (let* ((border (lambda (text) (approve-face text 'approve-thread-border)))
         (width (max 20 (- approve-html--width shr-indentation)))
         (title " Suggested change ")
         (rows (dom-by-tag dom 'tr))
         (lines
          (delq nil
                (mapcar
                 (lambda (row)
                   (when-let* ((cell (seq-find
                                      (lambda (td)
                                        (seq-some (lambda (class)
                                                    (string-prefix-p "blob-code" class))
                                                  (approve-html--classes td)))
                                      (dom-by-tag row 'td))))
                     (let* ((kind (cond ((approve-html--class-p cell "blob-code-deletion") 'del)
                                        ((approve-html--class-p cell "blob-code-addition") 'add)
                                        (t 'ctx)))
                            (text (concat (pcase kind ('del "-") ('add "+") (_ " "))
                                          (approve-html--suggestion-line
                                           cell (if (eq kind 'del)
                                                    'approve-diff-refine-removed
                                                  'approve-diff-refine-added))))
                            (face (pcase kind
                                    ('del 'approve-diff-removed)
                                    ('add 'approve-diff-added)
                                    (_ 'approve-diff-context))))
                       (add-face-text-property 0 (length text) face t text)
                       (concat (funcall border "│ ") text))))
                 rows))))
    (approve-html--insert-block
     (funcall border (concat "┌" title
                             (make-string (max 1 (- width (length title) 1)) ?─)))
     "")
    (dolist (line lines)
      (approve-html--insert-block line ""))
    (approve-html--insert-block
     (funcall border (concat "└" (make-string (max 1 (1- width)) ?─))) ""))
  (shr-ensure-paragraph))

(defun approve-html--tag-div (dom)
  "Render the div DOM: suggestions and alerts specially, else as usual."
  (cond
   ((approve-html--class-p dom "js-suggested-changes-blob")
    (approve-html--tag-suggestion dom))
   ((approve-html--class-p dom "markdown-alert")
    (approve-html--tag-alert dom))
   (t (shr-generic dom))))

;;; Lists

(defun approve-html--tag-ul (dom)
  "Render the unordered list DOM, indented by one column."
  (shr-ensure-paragraph)
  (let ((shr-list-mode 'ul)
        (shr-indentation (1+ shr-indentation)))
    (shr-generic dom))
  (shr-ensure-paragraph))

(defun approve-html--tag-ol (dom)
  "Render the ordered list DOM, numbered from its start attribute."
  (shr-ensure-paragraph)
  (let ((shr-list-mode (or (ignore-errors
                             (cl-parse-integer (dom-attr dom 'start)))
                           1))
        (shr-indentation (1+ shr-indentation)))
    (shr-generic dom))
  (shr-ensure-paragraph))

(defun approve-html--insert-item (bullet dom)
  "Insert the list item DOM after BULLET, its lines hanging under it."
  (shr-ensure-newline)
  (let ((start (point)))
    (insert bullet)
    (shr-mark-fill start)
    (let ((shr-indentation (+ shr-indentation (string-width bullet))))
      (put-text-property start (1+ start)
                         'shr-continuation-indentation shr-indentation)
      (put-text-property start (1+ start) 'shr-prefix-length (length bullet))
      (shr-generic dom)))
  (unless (bolp)
    (insert "\n")))

(defun approve-html--tag-li (dom)
  "Render the list item DOM: a task with a box, a number, or a bullet."
  (let ((checkbox (car (dom-by-class dom "task-list-item-checkbox"))))
    (cond
     (checkbox
      (approve-html--insert-item
       (if (dom-attr checkbox 'checked)
           (approve-face "☑ " 'approve-html-task-done)
         (approve-face "☐ " 'approve-html-task-open))
       dom))
     ((numberp shr-list-mode)
      (let ((bullet (format "%d. " shr-list-mode)))
        (setq shr-list-mode (1+ shr-list-mode))
        (approve-html--insert-item bullet dom)))
     (t (approve-html--insert-item (approve-face "• " 'approve-dim) dom)))))

;;; Tables

(defun approve-html--table-rows (dom)
  "Return the rows of the table DOM: lists of (TEXT . HEADER-P)."
  (mapcar (lambda (tr)
            (delq nil
                  (mapcar (lambda (cell)
                            (when (and (consp cell) (memq (dom-tag cell) '(td th)))
                              (cons (string-trim
                                     (replace-regexp-in-string
                                      "\n" " " (approve-html--subrender cell 1000)))
                                    (eq (dom-tag cell) 'th))))
                          (dom-children tr))))
          (dom-by-tag dom 'tr)))

(defun approve-html--tag-table (dom)
  "Render the table DOM with box borders, one line per row."
  (shr-ensure-paragraph)
  (let* ((rows (seq-filter #'identity (approve-html--table-rows dom)))
         (columns (apply #'max 0 (mapcar #'length rows)))
         (widths (cl-loop for i below columns
                          collect (apply #'max 1
                                         (mapcar (lambda (row)
                                                   (string-width (or (car (nth i row)) "")))
                                                 rows))))
         (border (lambda (left middle right)
                   (approve-face (concat left
                                         (mapconcat (lambda (w) (make-string (+ w 2) ?─))
                                                    widths middle)
                                         right)
                                 'approve-html-table-border)))
         (bar (approve-face "│" 'approve-html-table-border)))
    (when rows
      (approve-html--insert-block (funcall border "┌" "┬" "┐") "")
      (cl-loop for (row . more) on rows
               do (approve-html--insert-block
                   (concat bar
                           (mapconcat
                            (lambda (i)
                              (let* ((cell (nth i row))
                                     (text (or (car cell) ""))
                                     (pad (make-string (- (nth i widths) (string-width text))
                                                       ?\s)))
                                (concat " " (if (cdr cell)
                                                (approve-face text 'approve-html-table-header)
                                              text)
                                        pad " ")))
                            (number-sequence 0 (1- columns)) bar)
                           bar)
                   "")
               ;; A rule under the header row only, as GitHub draws it.
               (when (and more (seq-every-p #'cdr row))
                 (approve-html--insert-block (funcall border "├" "┼" "┤") "")))
      (approve-html--insert-block (funcall border "└" "┴" "┘") "")))
  (shr-ensure-paragraph))

;;; Details

(defun approve-html--tag-details (dom)
  "Render the <details> DOM: its summary line, then its body.
At the top level both are marked for `approve-html-insert', which
makes a section of them; nested ones are rendered open, inline."
  (shr-ensure-paragraph)
  (let* ((summary-dom (car (dom-by-tag dom 'summary)))
         (summary (string-trim (if summary-dom
                                   (approve-html--subrender summary-dom 1000)
                                 "Details")))
         (body-dom `(div nil ,@(remq summary-dom (dom-children dom))))
         (body (approve-html--subrender body-dom (- approve-html--width
                                                     shr-indentation 2))))
    (if approve-html--in-details
        (progn
          (approve-html--insert-block (approve-face summary 'approve-html-summary) "")
          (approve-html--insert-block body "  "))
      (let ((start (point))
            (index (cl-incf approve-html--details-count)))
        (approve-html--insert-block (approve-face summary 'approve-html-summary) "")
        (put-text-property start (point) 'approve-html-details
                           (list :index index
                                 :summary (substring-no-properties summary)
                                 ;; GitHub writes open=""; libxml drops
                                 ;; a bare open.
                                 :open (and (dom-attr dom 'open) t)))
        (setq start (point))
        (approve-html--insert-block body "  ")
        (put-text-property start (point) 'approve-html-details-body index))))
  (shr-ensure-paragraph))

;;; Images

(defun approve-html--tag-img (dom)
  "Render the image DOM from the image cache, or a placeholder link.
A missing image is added to `approve-html-wanted-images'."
  (let* ((url (dom-attr dom 'src))
         (alt (string-trim (or (dom-attr dom 'alt) "")))
         (label (format "[image%s]" (if (string-empty-p alt) "" (concat ": " alt))))
         (image (and (stringp url) (gethash url approve-html--images))))
    (when (stringp url)
      (cond
       ((and approve-load-images (display-images-p) image (not (symbolp image)))
        (shr-ensure-newline)
        (insert-image image label)
        (add-text-properties (1- (point)) (point) (approve-html-link-properties url))
        (insert "\n"))
       (t
        (when (and approve-load-images (display-images-p) (null image))
          (push url approve-html-wanted-images))
        (let ((start (point)))
          (shr-insert label)
          (approve-html--add-face start (point) 'approve-link)
          (add-text-properties start (point) (approve-html-link-properties url))))))))

(provide 'approve-html)

;;; approve-html.el ends here
