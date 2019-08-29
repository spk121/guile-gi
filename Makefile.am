# Makefile.am - top level
# Copyright (C) 2018, 2019 Michael L. Gran

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

ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AM_DISTCHECK_CONFIGURE_FLAGS = --with-gnu-filesystem-hierarchy --enable-introspection
SUBDIRS = src doc docs m4 module build-aux tools test examples

# Build ChangeLog from GIT history
EXTRA_DIST = README.md
dist: ChangeLog README
ChangeLog:
	$(AM_V_GEN) if test -d $(top_srcdir)/.git; then \
		GIT_DIR="$(top_srcdir)/.git" git log --stat > $@; \
	fi

README: README.md
	$(LN_S) $< $@

CLEANFILES = ChangeLog README
