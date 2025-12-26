# Makefile for Approve.el -*- mode: makefile-gmake -*-

EMACS ?= emacs
CASK ?= cask

# Directories
LISP_DIR = lisp
TEST_DIR = test

# Source files
SRCS = $(wildcard $(LISP_DIR)/*.el)
TESTS = $(wildcard $(TEST_DIR)/*.el)

# Byte-compiled files
OBJS = $(SRCS:.el=.elc)

.PHONY: all build test test-verbose clean distclean help install lint checkdoc

# Default target
all: build

## help: Show this help message
help:
	@echo "Approve.el - GitHub Pull Request review interface"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@grep -E '^## ' $(MAKEFILE_LIST) | sed 's/## /  /'

## install: Install package dependencies via Cask
install:
	$(CASK) install

## build: Byte-compile all source files
build: install
	$(CASK) emacs --batch \
		-L $(LISP_DIR) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SRCS)

## test: Run all tests
test: install
	$(CASK) exec buttercup -L . -L $(LISP_DIR) $(TEST_DIR)

## test-verbose: Run all tests with verbose output
test-verbose: install
	$(CASK) exec buttercup -L . -L $(LISP_DIR) $(TEST_DIR) --reporter spec

## test-coverage: Run tests with coverage reporting
test-coverage: install
	$(CASK) emacs --batch \
		-L $(LISP_DIR) \
		-L $(TEST_DIR) \
		--eval "(require 'undercover)" \
		--eval "(undercover \"$(LISP_DIR)/*.el\" (:report-format 'text))" \
		-f package-initialize \
		--eval "(require 'buttercup)" \
		--eval "(buttercup-run-discover \"$(TEST_DIR)\")"

## lint: Run package-lint on source files
lint: install
	$(CASK) emacs --batch \
		-L $(LISP_DIR) \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(SRCS)

## checkdoc: Check documentation style
checkdoc:
	$(CASK) emacs --batch \
		-L $(LISP_DIR) \
		--eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-verb-check-experimental-flag nil)" \
		--eval "(let ((files '($(SRCS)))) \
		         (dolist (file files) \
		           (with-current-buffer (find-file-noselect file) \
		             (checkdoc-current-buffer t))))"

## clean: Remove byte-compiled files
clean:
	rm -f $(OBJS)
	rm -f $(TEST_DIR)/*.elc

## distclean: Remove all generated files including Cask dependencies
distclean: clean
	rm -rf .cask

## watch: Run tests continuously (requires watchexec)
watch:
	watchexec -e el -- make test
