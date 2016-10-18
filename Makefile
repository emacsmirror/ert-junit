# Copyright (C) 2015-2016 Ola Nilsson <ola.nilsson@gmail.com>

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
EFLAGS += -L packages

ELS = ert-junit.el
elcs = $(ELS:.el=.elc)
EMACS ?= emacs
ifeq ($(EMACS),t)
# Called from within Emacs
EMACS = emacs
endif
loadfiles = $(addprefix -l ,$(packagefiles) $(wildcard test/*.el)) $(EXTRA_LOADFILES)

ever:=$(word 3,$(shell $(EMACS) --version | head -1))
ever_major=$(firstword $(subst ., ,$(ever)))

packages_23 = https://raw.githubusercontent.com/ohler/ert/fb3c278d/lisp/emacs-lisp/ert.el
packages = $(packages_$(ever_major)) $(EXTRA_PACKAGES)
#packages += https://raw.githubusercontent.com/philjackson/xmlgen/master/xmlgen.el
#packages += https://bitbucket.org/olanilsson/ert-junit/raw/master/ert-junit.el

CURL = curl -fsSkL --create-dirs --retry 9 --retry-delay 9

JUNIT_OUT ?= shippable/testresults/tests.xml
COBERTURA_OUT ?= shippable/codecoverage/coverage.xml

all: lisp

lisp: packages $(elcs)

%.elc: %.el
	$(EMACS) --batch $(EFLAGS) -f batch-byte-compile $^

$(foreach p,$(packages),\
	$(eval packages/$(notdir $(p)): ; $(CURL) $p -o $$@))

packagefiles = $(addprefix packages/,$(notdir $(packages)))

packages: $(packagefiles)

#run tests
#test-no-results: lisp packages
#	$(EMACS) --batch $(EFLAGS) $(loadfiles) -f ert-run-tests-batch-and-exit

#run tests
#test: lisp packages
#	mkdir -p $(dir $(JUNIT_OUT))
#	$(EMACS) --batch $(EFLAGS) $(loadfiles) -f ert-junit-run-tests-batch-and-exit $(JUNIT_OUT)

#coverage: lisp-clean packages
#	mkdir -p $(dir $(COBERTURA_OUT))
#	$(EMACS) --batch $(EFLAGS) $(loadfiles) -f ert-run-cobertura-tests-batch-and-exit $(COBERTURA_OUT) $(ELS)

lisp-clean:
	-rm -rf *.elc test/*.elc

clean: lisp-clean
	-rm -rf  packages

.PHONY: all test lisp clean packages test-no-results coverage

