# -*- Mode: Makefile; tab-width: 3; indent-tabs-mode: t -*-
#
# Makefile --- Make targets for various tasks.
#
# Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

# This way you can easily run the tests for different versions
# of each lisp with, e.g. ALLEGRO=/path/to/some/lisp make test-allegro
CMUCL    ?= lisp
OPENMCL  ?= openmcl
SBCL     ?= sbcl
CLISP    ?= clisp
ALLEGRO  ?= alisp
SCL      ?= scl
ECL      ?= ecl

shlibs:
	@$(MAKE) -wC tests shlibs

clean:
	@$(MAKE) -wC tests clean
	find . -name ".fasls" | xargs rm -rf
	find . \( -name "*.dfsl" -o -name "*.fasl" -o -name "*.fas" -o -name "*.lib" -o -name "*.x86f" -o -name "*.amd64f" -o -name "*.sparcf" -o -name "*.sparc64f" -o -name "*.hpf" -o -name "*.hp64f" -o -name "*.ppcf" -o -name "*.nfasl" -o -name "*.ufsl" -o -name "*.fsl" -o -name "*.lx64fsl" \) -exec rm {} \;

test-openmcl:
	@-$(OPENMCL) --load tests/run-tests.lisp

test-sbcl:
	@-$(SBCL) --noinform --load tests/run-tests.lisp

test-cmucl:
	@-$(CMUCL) -load tests/run-tests.lisp

test-scl:
	@-$(SCL) -load tests/run-tests.lisp

test-clisp:
	@-$(CLISP) -q -x '(load "tests/run-tests.lisp")'

test-clisp-modern:
	@-$(CLISP) -modern -q -x '(load "tests/run-tests.lisp")'

test-allegro:
	@-$(ALLEGRO) -L tests/run-tests.lisp

test-ecl:
	@-$(ECL) --quiet --load tests/run-tests.lisp

test: test-openmcl test-sbcl test-cmucl test-clisp test-ecl

# vim: ft=make ts=3 noet
