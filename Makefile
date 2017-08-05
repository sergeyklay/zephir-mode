EMACS       = emacs
CASK        = cask

EMACSFLAGS ?=
TESTFLAGS  ?=

PKGDIR      := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PACKAGE_NAME = zephir-mode.el
COMPILED_OBJ = $(PACKAGE_NAME:.el=.elc)

all: build test

checkdoc:
	${CASK} exec $(EMACS) -Q -L . --batch --eval "(checkdoc-file \"${PACKAGE_NAME}\")"

package-lint:
	${CASK} exec $(EMACS) -Q -L . --batch -l "package-lint.el" -f "package-lint-batch-and-exit" ${PACKAGE_NAME}

build: $(COMPILED_OBJ)

test: $(PKGDIR) package-lint
	$(CASK) exec ert-runner $(TESTFLAGS)

clean:
	${CASK} clean-elc

%.elc : %.el $(PKGDIR)
	${CASK} exec $(EMACS) -Q -L . --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

.PHONY: all checkdoc package-lint build test clean
