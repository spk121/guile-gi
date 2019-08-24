# Makefile.am - module directory
# Copyright (C) 2019 Michael L. Gran

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

pkgguilesitedir=$(guilesitedir)/gi
scriptguilesitedir=$(guilesitedir)/scripts
pkgguileobjectdir=$(guileobjectdir)/gi
scriptguileobjectdir=$(guileobjectdir)/scripts

dist_guilesite_DATA = \
  gi.scm

nodist_pkgguilesite_DATA = \
  gi/config.scm

dist_pkgguilesite_DATA = \
  gi/core-generics.scm \
  gi/documentation.scm \
  gi/oop.scm \
  gi/repository.scm \
  gi/types.scm \
  gi/util.scm

dist_scriptguilesite_DATA = \
  scripts/gi-gtkdoc.scm

guileobject_DATA = \
  $(dist_guilesite_DATA:%.scm=%.go)

pkgguileobject_DATA = \
  $(dist_pkgguilesite_DATA:%.scm=%.go) \
  $(nodist_pkgguilesite_DATA:%.scm=%.go)

scriptguileobject_DATA = \
  $(dist_scriptguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = \
 -O2 \
 -Warity-mismatch \
 -Wformat \
 -Wmacro-use-before-definition \
 -Wunbound-variable \
 --load-path=$(abs_srcdir) \
 --load-path=$(abs_builddir)

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_top_builddir)/src/.libs \
	$(GUILE_TOOLS) compile --target="$(host)" $(GUILEC_FLAGS) \
	-o "$@" "$<"

CLEANFILES = $(guileobject_DATA) $(pkgguileobject_DATA) $(scriptguileobject_DATA)
