;;; test-approve-html.el --- Tests for approve-html -*- lexical-binding: t; -*-

;;; Commentary:

;; Rendering GitHub's bodyHTML: the text a reader sees, its faces, and
;; the properties links carry.  The snippets are in the shape GitHub
;; sends them.

;;; Code:

(require 'buttercup)
(require 'approve-html)
(require 'approve-test-helpers "helpers")

(defun test-html--render (html &optional width)
  "Return HTML rendered WIDTH wide, without the cache."
  (approve-html-clear-cache)
  (approve-html-render html width))

(defun test-html--face-at (text needle)
  "Return the face list at the first NEEDLE in TEXT."
  (let ((pos (string-search needle text)))
    (unless pos (error "%S not in %S" needle text))
    (ensure-list (get-text-property pos 'face text))))

(defun test-html--lines (text)
  "Return the lines of TEXT, without the final empty one."
  (split-string (string-trim-right text "\n") "\n"))

(describe "approve-html-render"
  (it "renders paragraphs filled to the width, ending in one newline"
    (let ((text (test-html--render
                 "<p dir=\"auto\">one two three four five six seven eight nine ten</p>\n<p>next</p>"
                 20)))
      (expect (test-html--lines text)
              :to-equal '("one two three four" "five six seven eight" "nine ten" "" "next"))
      (expect text :to-match "next\n\\'")))

  (it "renders nothing for empty HTML"
    (expect (approve-html-render "") :to-equal "")
    (expect (approve-html-render nil) :to-equal "")
    (expect (approve-html-insert "  ") :to-be nil))

  (it "marks inline code, struck text and headings"
    (let ((text (test-html--render
                 "<h2 class=\"heading-element\">Title</h2><p>Use <code class=\"notranslate\">cl-incf</code> not <del>setq</del></p>")))
      (expect (test-html--face-at text "Title") :to-contain 'approve-html-heading)
      (expect (test-html--face-at text "cl-incf") :to-contain 'approve-html-code)
      (expect (test-html--face-at text "setq") :to-contain 'approve-html-strike)
      (expect (get-text-property (string-search "cl-incf" text) 'font-lock-face text)
              :to-equal (get-text-property (string-search "cl-incf" text) 'face text))))

  (it "keeps code blocks as they are, coloured by GitHub's classes"
    (let* ((text (test-html--render
                  "<div class=\"highlight highlight-source-emacs-lisp\"><pre>(<span class=\"pl-k\">defun</span> <span class=\"pl-en\">foo</span> ()\n  <span class=\"pl-s\"><span class=\"pl-pds\">\"</span>doc<span class=\"pl-pds\">\"</span></span>)</pre></div>"
                  10))
           (lines (test-html--lines text)))
      (expect lines :to-equal '(" (defun foo ()" "   \"doc\")"))
      (expect (test-html--face-at text "defun") :to-contain 'font-lock-keyword-face)
      (expect (test-html--face-at text "defun") :to-contain 'approve-html-code-block)
      (expect (test-html--face-at text "foo") :to-contain 'font-lock-function-name-face)
      (expect (test-html--face-at text "doc") :to-contain 'font-lock-string-face)))

  (it "renders lists with bullets, numbers and task boxes, hanging long items"
    (let ((text (test-html--render
                 (concat "<ul><li>short</li><li>a long item that has to wrap</li></ul>"
                         "<ol start=\"3\"><li>three</li><li>four</li></ol>"
                         "<ul class=\"contains-task-list\">"
                         "<li class=\"task-list-item\"><input type=\"checkbox\" class=\"task-list-item-checkbox\" disabled checked> done</li>"
                         "<li class=\"task-list-item\"><input type=\"checkbox\" class=\"task-list-item-checkbox\" disabled> todo</li></ul>")
                 20)))
      (expect (test-html--lines text)
              :to-equal '(" • short" " • a long item that" "   has to wrap" ""
                          " 3. three" " 4. four" ""
                          " ☑ done" " ☐ todo"))
      (expect (test-html--face-at text "☑") :to-contain 'approve-html-task-done)))

  (it "draws a bar left of quoted lines, also between paragraphs"
    (let ((text (test-html--render
                 "<blockquote><p>quoted words that wrap here</p><p>again</p></blockquote>"
                 16)))
      (expect (test-html--lines text)
              :to-equal '("▌ quoted words" "▌ that wrap here" "▌" "▌ again"))
      (expect (test-html--face-at text "quoted") :to-contain 'approve-html-blockquote)))

  (it "renders GitHub alerts as a titled quote"
    (let ((text (test-html--render
                 "<div class=\"markdown-alert markdown-alert-note\"><p class=\"markdown-alert-title\"><svg aria-hidden=\"true\"><path d=\"M0\"></path></svg>Note</p><p>Read this.</p></div>")))
      (expect (test-html--lines text) :to-equal '("▌ Note" "▌ Read this."))))

  (it "draws tables with borders and a rule under the header"
    (let ((text (test-html--render
                 "<markdown-accessiblity-table><table><thead><tr><th>Name</th><th>N</th></tr></thead><tbody><tr><td>a</td><td><code>1</code></td></tr><tr><td>longer</td><td>2</td></tr></tbody></table></markdown-accessiblity-table>")))
      (expect (test-html--lines text)
              :to-equal '("┌────────┬───┐"
                          "│ Name   │ N │"
                          "├────────┼───┤"
                          "│ a      │ 1 │"
                          "│ longer │ 2 │"
                          "└────────┴───┘"))))

  (it "draws a suggested change as a small diff in a box"
    (let* ((comment (car (approve-model-nodes
                          (alist-get 'comments
                                     (seq-find
                                      (lambda (thread)
                                        (equal (alist-get 'startLine thread) 129))
                                      (let-alist (approve-test-fixture-data
                                                  "medium" "queries/pull-request-threads.graphql")
                                        .repository.pullRequest.reviewThreads.nodes))))))
           (body (alist-get 'bodyHTML comment))
           (text (test-html--render body 40))
           (lines (test-html--lines text)))
      (expect (car lines) :to-equal "These three lines can be one call:")
      (expect (nth 2 lines) :to-equal "┌ Suggested change ─────────────────────")
      (expect (nth 3 lines) :to-equal "│ -\t}")
      (expect (nth 6 lines) :to-equal "│ +\tresult = append(result, parseAll(ctx, table)...)")
      (expect (car (last lines)) :to-match "\\`└─+\\'")
      (expect (test-html--face-at text "-\t}") :to-contain 'approve-diff-removed)
      (expect (test-html--face-at text "+\tresult") :to-contain 'approve-diff-added)))

  (it "marks each suggested change with its place among the comment's"
    (let* ((box (concat "<div class=\"js-suggested-changes-blob\"><table><tr>"
                        "<td class=\"blob-code blob-code-addition\">%s</td></tr></table></div>"))
           (text (test-html--render
                  (concat "<p>One</p>" (format box "first")
                          "<details><summary>More</summary>" (format box "second")
                          "</details>"))))
      (expect (get-text-property (string-search "+first" text) 'approve-thing text)
              :to-equal '(:kind suggestion :index 0))
      (expect (get-text-property (string-search "+second" text) 'approve-thing text)
              :to-equal '(:kind suggestion :index 1))
      (expect (get-text-property (string-search "One" text) 'approve-thing text)
              :to-be nil)))

  (it "gives links their URL, and mentions, issues and commits what they point to"
    (let ((text (test-html--render
                 (concat "<p><a class=\"user-mention notranslate\" data-hovercard-type=\"user\" href=\"https://github.com/bob\">@bob</a> "
                         "<a class=\"team-mention js-team-mention notranslate\" href=\"https://github.com/orgs/acme/teams/core\">@acme/core</a> "
                         "<a class=\"issue-link js-issue-link\" data-hovercard-type=\"issue\" href=\"https://github.com/acme/rocket/issues/12\">#12</a> "
                         "<a class=\"commit-link\" data-hovercard-type=\"commit\" href=\"https://github.com/acme/rocket/commit/d4e5f6a0123\"><tt>d4e5f6a</tt></a> "
                         "<a href=\"https://example.com\">site</a></p>"))))
      (cl-flet ((thing (needle) (get-text-property (string-search needle text)
                                                   'approve-thing text))
                (url (needle) (get-text-property (string-search needle text)
                                                 'approve-url text)))
        (expect (thing "@bob") :to-equal '(:kind user :login "bob" :url "https://github.com/bob"))
        (expect (plist-get (thing "@acme") :slug) :to-equal "acme/core")
        (expect (thing "#12") :to-equal '(:kind ref :owner "acme" :repo "rocket" :number 12
                                          :url "https://github.com/acme/rocket/issues/12"))
        (expect (plist-get (thing "d4e5f6a") :oid) :to-equal "d4e5f6a0123")
        (expect (thing "site") :to-be nil)
        (expect (url "site") :to-equal "https://example.com")
        (expect (test-html--face-at text "site") :to-contain 'approve-link))))

  (it "drops heading anchors and their icons"
    (expect (test-html--lines
             (test-html--render
              "<div class=\"markdown-heading\"><h2 class=\"heading-element\">Summary</h2><a id=\"user-content-summary\" class=\"anchor\" href=\"#summary\"><svg class=\"octicon\" aria-hidden=\"true\"><path d=\"m7\"></path></svg></a></div>"))
            :to-equal '("Summary")))

  (it "caches what it rendered"
    (approve-html-clear-cache)
    (let ((html "<p>cached</p>"))
      (expect (approve-html-render html) :to-be (approve-html-render html))))

  (describe "with images"
    (after-each (approve-html-clear-cache)
                (setq approve-html-wanted-images nil))

    (it "draws a link with the alt text when images are off, and wants nothing"
      (let* ((approve-load-images nil)
             (text (test-html--render
                    "<p><a href=\"https://camo.example/x\"><img src=\"https://camo.example/x\" alt=\"shot\"></a></p>")))
        (expect (string-trim text) :to-equal "[image: shot]")
        (expect (get-text-property 0 'approve-url text) :to-equal "https://camo.example/x")
        (expect approve-html-wanted-images :to-be nil)))

    (it "wants a missing image when it can show it, and never caches its text"
      (spy-on 'display-images-p :and-return-value t)
      (let ((approve-load-images t)
            (html "<p><img src=\"https://camo.example/y\" alt=\"\"></p>"))
        (expect (string-trim (test-html--render html)) :to-equal "[image]")
        (expect (approve-html-take-wanted-images) :to-equal '("https://camo.example/y"))
        (expect (approve-html-take-wanted-images) :to-be nil)
        (expect (approve-html-render html) :not :to-be (approve-html-render html))))))

(describe "approve-html-insert"
  (it "makes each <details> a section, collapsed unless open"
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (magit-insert-section (comment "c1")
            (approve-html-insert
             (concat "<p>before</p><details><summary>Logs</summary><p>hidden</p></details>"
                     "<details open=\"\"><summary>Shown</summary><p>visible</p></details><p>after</p>")))))
      (let* ((comment (car (oref magit-root-section children)))
             (details (oref comment children)))
        (expect (length details) :to-equal 2)
        (expect (mapcar (lambda (s) (oref s value)) details)
                :to-equal '((1 . "Logs") (2 . "Shown")))
        (expect (mapcar (lambda (s) (oref s hidden)) details) :to-equal '(t nil))
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "before\n\nLogs\n  hidden\n\nShown\n  visible\n\nafter\n"))))

  (it "inserts <details> as plain text outside sections"
    (with-temp-buffer
      (approve-html-insert "<details><summary>Logs</summary><p>hidden</p></details>")
      (expect (buffer-string) :to-equal "Logs\n  hidden\n"))))

;;; test-approve-html.el ends here
