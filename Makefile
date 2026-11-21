.PHONY: install build test cover lint clean distclean docs try

CASK ?= cask
EMACS ?= emacs

SRCS = $(wildcard lisp/*.el)
OBJS = $(SRCS:.el=.elc)

# A module required while compiling another loads from its newer file,
# never from a .elc older than its source.
BATCH = $(CASK) exec $(EMACS) -Q --batch -L lisp -L tests \
	--eval "(setq load-prefer-newer t)"

# package-lint needs the archive contents that cask install fetched.
PACKAGE_INIT = (progn (require 'package) \
  (setq package-user-dir \"$(shell $(CASK) package-directory 2>/dev/null)\") \
  (setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\") \
                           (\"melpa\" . \"https://melpa.org/packages/\"))) \
  (package-initialize))

install:
	$(CASK) install

build: install $(OBJS)

lisp/%.elc: lisp/%.el
	$(BATCH) -f batch-byte-compile $<

# The tests load each module's .elc: build first, so none is stale.
test: build
	$(CASK) exec buttercup -L lisp -L tests -L . tests

# Tests with coverage; prints a text report.
cover: install
	UNDERCOVER_FORCE=true $(BATCH) \
		--eval "(require 'undercover)" \
		--eval "(undercover \"lisp/*.el\" (:report-format 'text) (:send-report nil))" \
		--eval "(require 'buttercup)" \
		-f buttercup-run-discover $(CURDIR)/tests

# Byte-compile with warnings as errors, then package-lint and checkdoc.
# The .elc go to a temporary directory: the build's stay, so Approve
# reloaded after a lint still runs compiled.  Checkdoc's guess at the
# voice of a doc string is off, as from Emacs 31.1, so every Emacs in
# CI checks the same way.
LINT_INIT = (progn \
  (defvar approve-lint-directory (make-temp-file \"approve-lint\" t)) \
  (setq byte-compile-error-on-warn t) \
  (setq byte-compile-dest-file-function \
        (lambda (file) \
          (expand-file-name (concat (file-name-base file) \".elc\") approve-lint-directory))) \
  (add-hook (quote kill-emacs-hook) \
            (lambda () (delete-directory approve-lint-directory t))))

lint: install
	$(BATCH) --eval "$(LINT_INIT)" -f batch-byte-compile $(SRCS)
	$(BATCH) --eval "$(PACKAGE_INIT)" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"lisp/approve.el\")" \
		-f package-lint-batch-and-exit $(SRCS)
	$(BATCH) --eval "(require 'checkdoc)" \
		--eval "(let* ((failed nil) \
		               (checkdoc-verb-check-experimental-flag nil) \
		               (default checkdoc-create-error-function) \
		               (checkdoc-create-error-function \
		                (lambda (&rest args) (setq failed t) (apply default args)))) \
		          (dolist (file command-line-args-left) (checkdoc-file file)) \
		          (setq command-line-args-left nil) \
		          (kill-emacs (if failed 1 0)))" \
		$(SRCS)

clean:
	rm -f $(OBJS)

docs:
	docs/html/build.sh

distclean: clean
	rm -rf .cask

# A fresh Emacs (-Q, Cask dependencies only) replaying the fixtures.
try: build
	$(CASK) emacs -Q -L lisp -l dev/try.el
