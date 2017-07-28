EMACS ?= emacs
ELS = zephir-mode.el zephir-mode-test.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: $(ELCS)

clean:
	rm -f $(ELCS)

test:
	make clean
	make all
	$(EMACS) -Q -batch -L . -l zephir-mode-test.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test
