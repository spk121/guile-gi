# Makefile.am - test directory
# Copyright (C) 2018,2019 Michael L. Gran

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

################################################################
# Test driver

.PHONY: test
test: check

AM_TESTS_ENVIRONMENT = \
  GUILE_AUTO_COMPILE=0; \
  export GUILE_AUTO_COMPILE;

TEST_EXTENSIONS = .scm

noinst_DATA = lib.scm automake-test-lib.scm

TESTS = \
 memchk_1.scm \
 memchk_2.scm \
 byte_array_new.scm \
 byte_array_new_take.scm \
 bytes_new_null_size.scm \
 bytes_new_null_contents.scm \
 gapplication_1.scm \
 gapplication_2.scm \
 softerror_create.scm 

SCM_LOG_COMPILER = \
  ${top_builddir}/libtool \
  --dlopen=${top_builddir}/src/libguile-gi.la \
  --mode=execute $(GUILE)

AM_SCM_LOG_FLAGS = --no-auto-compile -L $(abs_top_srcdir)/src -L $(abs_top_srcdir) -s

EXTRA_DIST = \
  $(TESTS) \
  lib.scm \
  automake-test-lib.scm \
  example-1.scm \
  browser.scm

exampledir = $(datadir)/doc/guile-gi
dist_example_DATA = \
  example-1.scm \
  example-1b.scm \
  example-1c.scm \
  browser.scm
