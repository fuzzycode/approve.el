.PHONY: build test clean distclean install lint docs

CASK ?= cask
EMACS ?= emacs

SRCS = $(wildcard lisp/*.el)
OBJS = $(SRCS:.el=.elc)

install:
	$(CASK) install

build: install $(OBJS)

lisp/%.elc: lisp/%.el
	$(CASK) exec $(EMACS) -Q --batch -L lisp -f batch-byte-compile $<

test: install
	$(CASK) exec buttercup -L lisp -L . tests

lint: install
	$(CASK) exec $(EMACS) -Q --batch -L lisp \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SRCS)

clean:
	rm -f $(OBJS)

docs:
	docs/html/build.sh

distclean: clean
	rm -rf .cask
