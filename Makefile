emacs ?= emacs
all: test

test: clean
	cask exec $(emacs) -Q -batch -L . -l zephir-mode-test.el -l zephir-mode.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile d-mode.el

clean:
	rm -f zephir-mode.elc

.PHONY: all clean test
