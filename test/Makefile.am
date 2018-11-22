# Makefile.am - test directory
# Copyright (C) 2018 Michael L. Gran

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
  export GUILE_AUTO_COMPILE; \
  LTDL_LIBRARY_PATH=${srcdir}; \
  export LTDL_LIBRARY_PATH;

TEST_EXTENSIONS = .scm

noinst_DATA = lib.scm

TESTS = \
 memchk_1.scm \
 memchk_2.scm \
 mainloop.scm

SCM_LOG_COMPILER = ${top_builddir}/libtool --mode=execute --dlopen=${top_builddir}/src/libguile-gi.la guile

AM_SCM_LOG_FLAGS = --no-auto-compile -L ${top_srcdir}/src -L ${top_srcdir}/test -s

EXTRA_DIST = \
  $(TESTS) \
  lib.scm
