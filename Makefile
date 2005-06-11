# -*- Mode: Makefile; tab-width: 3; indent-tabs-mode: t -*-
#
# Makefile --- Make targets for various tasks.
#
# Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
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

shlibs:
	@$(MAKE) -wC tests shlibs

clean:
	@$(MAKE) -wC tests clean
	find . \( -name "*.dfsl" -o -name "*.fasl" -o -name "*.fas" -o -name "*.lib" -o -name "*.x86f" -o -name "*.ppcf" \) -exec rm {} \;

test: shlibs
	@-test -x `which openmcl` && echo "-------- Running unit tests in OpenMCL: --------" && openmcl --load tests/run-tests.lisp
	@-test -x `which sbcl` && echo "-------- Running unit tests in SBCL: --------" && sbcl --noinform --load tests/run-tests.lisp
	@-test -x `which lisp` && echo "-------- Running unit tests in CMU CL: --------" && lisp -load tests/run-tests.lisp
	@-test -x `which clisp` && echo "-------- Running unit tests in CLISP: --------" && clisp tests/run-tests.lisp

# vim: ft=make ts=3 noet
