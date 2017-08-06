# Copyright (C) 2017 Serghei Iakovlev
#
# This file is not part of GNU Emacs.
#
# License
#
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

SHELL       := $(shell which bash)
ROOT_DIR    := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS        = emacs
CASK         = cask

EMACSFLAGS  ?=
TESTFLAGS   ?=

PKGDIR      := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PACKAGE_NAME = zephir-mode.el
COMPILED_OBJ = $(PACKAGE_NAME:.el=.elc)

.SILENT: ;               # no need for @
.ONESHELL: ;             # recipes execute in same shell
.NOTPARALLEL: ;          # wait for this target to finish
.EXPORT_ALL_VARIABLES: ; # send all vars to shell
default: help-default;   # default target
Makefile: ;              # skip prerequisite discovery

.title:
	$(info Zepphir Mode version: $(shell cat $(ROOT_DIR)/$(PACKAGE_NAME) | grep ";; Version:" | awk -F': ' '{print $$2}'))
	$(info )

help: .title
	@echo "                          ====================================================================="
	@echo "                    help: Show this help and exit"
	@echo "                ckeckdoc: Checks Zephir Mode code for errors in documentation"
	@echo "                pkg-lint: Run package linter for the Zephir Mode metadata"
	@echo "                   build: Byte compile Zephir Mode package"
	@echo "                    test: Run the non-interactive unit test suite"
	@echo "                   clean: Remove all byte compiled Elisp files"
	@echo "                          ====================================================================="
	@echo ""

all: build test

checkdoc:
	${CASK} exec $(EMACS) -Q -L . --batch --eval "(checkdoc-file \"${PACKAGE_NAME}\")"

pkg-lint:
	${CASK} exec $(EMACS) -Q -L . --batch -l "package-lint.el" -f "package-lint-batch-and-exit" ${PACKAGE_NAME}

build: $(COMPILED_OBJ)

test: $(PKGDIR) pkg-lint
	$(CASK) exec ert-runner $(TESTFLAGS)

clean:
	${CASK} clean-elc

%.elc : %.el $(PKGDIR)
	${CASK} exec $(EMACS) -Q -L . --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

.PHONY: .title help all checkdoc pkg-lint build test clean
