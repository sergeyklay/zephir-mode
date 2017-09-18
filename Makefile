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

SHELL := $(shell which bash)
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
EMACS = emacs
CASK = cask
EMACSFLAGS ?=
TESTFLAGS ?=
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# File lists
SRCS = zephir-mode.el
OBJS = $(SRCS:.el=.elc)

.SILENT: ;               # no need for @
.ONESHELL: ;             # recipes execute in same shell
.NOTPARALLEL: ;          # wait for this target to finish
.EXPORT_ALL_VARIABLES: ; # send all vars to shell
Makefile: ;              # skip prerequisite discovery

# Run make help by default
.DEFAULT_GOAL = help

# Internal variables
EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS =

# Program availability
ifdef CASK
RUNEMACS = $(CASK) exec $(EMACSBATCH)
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
endif
else
RUNEMACS = $(EMACSBATCH)
endif

%.elc: %.el $(PKGDIR)
	$(RUNEMACS) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

# Public targets

.PHONY: .title
.title:
	$(info Zepphir Mode $(shell cat $(ROOT_DIR)/$(SRCS) | grep ";; Version:" | awk -F': ' '{print $$2}'))
	$(info )

.PHONY: init
init: $(PKGDIR)

.PHONY: checkdoc
checkdoc:
	$(RUNEMACS) --eval '(checkdoc-file "$(SRCS)")'

.PHONY: build
build: $(OBJS)

.PHONY: test
test: build
	$(CASK) exec ert-runner $(TESTFLAGS)

.PHONY: clean
clean:
	$(CASK) clean-elc

.PHONY: help
help: .title
	echo 'Run `make init` first to install and update all local dependencies.'
	echo ''
	echo 'Available targets:'
	echo '  help:     Show this help and exit'
	echo '  init:     Initialise the project (has to be launched first)'
	echo '  checkdoc: Checks Zephir Mode code for errors in documentation'
	echo '  build:    Byte compile Zephir Mode package'
	echo '  test:     Run the non-interactive unit test suite'
	echo '  clean:    Remove all byte compiled Elisp files'
	echo ''
	echo 'Available programs:'
	echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	echo ''
	echo 'You need $(CASK) to develop Zephir Mode.  See http://cask.readthedocs.io/ for more.'
	echo ''
