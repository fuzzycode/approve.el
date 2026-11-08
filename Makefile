.PHONY: install build test cover lint clean distclean docs

CASK ?= cask
EMACS ?= emacs

SRCS = $(wildcard lisp/*.el)
OBJS = $(SRCS:.el=.elc)

BATCH = $(CASK) exec $(EMACS) -Q --batch -L lisp -L tests

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

test: install
	$(CASK) exec buttercup -L lisp -L tests -L . tests

# Tests with coverage; prints a text report.
cover: install
	UNDERCOVER_FORCE=true $(BATCH) \
		--eval "(require 'undercover)" \
		--eval "(undercover \"lisp/*.el\" (:report-format 'text) (:send-report nil))" \
		--eval "(require 'buttercup)" \
		-f buttercup-run-discover $(CURDIR)/tests

# Byte-compile with warnings as errors, then package-lint and checkdoc.
lint: install
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SRCS)
	rm -f $(OBJS)
	$(BATCH) --eval "$(PACKAGE_INIT)" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"lisp/approve.el\")" \
		-f package-lint-batch-and-exit $(SRCS)
	$(BATCH) --eval "(require 'checkdoc)" \
		--eval "(let* ((failed nil) \
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
