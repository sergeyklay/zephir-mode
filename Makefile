EMACS=emacs
CASK=cask
PACKAGE-NAME=zephir-mode.el

all: checkdoc test

checkdoc:
	$(EMACS) -Q -L . -batch --eval "(checkdoc-file \"${PACKAGE-NAME}\")"

package-lint:
	${CASK} exec $(EMACS) -Q -L . --batch -l "package-lint.el" \
	-f "package-lint-batch-and-exit" ${PACKAGE-NAME}

build: package-lint
	${CASK} exec  $(EMACS) -Q -L . --batch \
	--eval "(progn \
		(setq byte-compile-error-on-warn t)  \
		(batch-byte-compile))" ${PACKAGE-NAME}

test: build
	${CASK} exec ert-runner

clean:
	@rm -f *.elc
	@rm -rf .cask

.PHONY: all checkdoc package-lint test build clean
