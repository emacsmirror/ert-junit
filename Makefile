# Copyright (C) 2015-2017 Ola Nilsson <ola.nilsson@gmail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

EFLAGS = -Q -L .
EFLAGS += -L $(PACKAGEDIR)

ELS = ert-junit.el
elcs = $(ELS:.el=.elc)
EMACS ?= emacs
ifeq ($(EMACS),t)
# Called from within Emacs
EMACS = emacs
endif
loadfiles = $(addprefix -l ,$(packagefiles) $(wildcard test/*.el)) $(EXTRA_LOADFILES)

override ever:=$(word 3,$(shell $(EMACS) --version | head -1))
override ever_major=$(firstword $(subst ., ,$(ever)))

packages_23 = https://raw.githubusercontent.com/ohler/ert/fb3c278d/lisp/emacs-lisp/ert.el
packages = $(packages_$(ever_major)) $(EXTRA_PACKAGES)

PACKAGEDIR = packages/$(ever)

CURL = curl -fsSkL --create-dirs --retry 9 --retry-delay 9

JUNIT_OUT ?= shippable/testresults/tests.xml
COBERTURA_OUT ?= shippable/codecoverage/coverage.xml

all: lisp

lisp: packages $(elcs)

%.elc: %.el
	$(EMACS) --batch $(EFLAGS) -f batch-byte-compile $^

$(foreach p,$(packages),\
	$(eval $(PACKAGEDIR)/$(notdir $(p)): ; $(CURL) $p -o $$@))

packagefiles = $(addprefix $(PACKAGEDIR)/,$(notdir $(packages)))

packages: $(packagefiles)

#run tests
#test-no-results: lisp packages
#	$(EMACS) --batch $(EFLAGS) $(loadfiles) -f ert-run-tests-batch-and-exit

#run tests
check test: lisp packages
	mkdir -p $(dir $(JUNIT_OUT))
	$(EMACS) --batch $(EFLAGS) $(loadfiles) -f ert-junit-run-tests-batch-and-exit $(JUNIT_OUT)

cichecks = $(addprefix cicheck_,24_3 24_4 24_5 25_1 25_2 25_3 26_0_91)
cicheck: $(cichecks)

$(cichecks): lisp-clean
	circleci build --job=$(patsubst cicheck_%,emacs_%,$@) | tee $@.cilog
	grep --binary-file=text -q 'Success!' $@.cilog

#coverage: lisp-clean packages
#	mkdir -p $(dir $(COBERTURA_OUT))
#	$(EMACS) --batch $(EFLAGS) $(loadfiles) -f ert-run-cobertura-tests-batch-and-exit $(COBERTURA_OUT) $(ELS)

lisp-clean:
	-rm -rf *.elc test/*.elc

clean: lisp-clean
	-rm -rf  $(PACKAGEDIR)

.PHONY: all test lisp clean packages test-no-results coverage cicheck $(cichecks)

