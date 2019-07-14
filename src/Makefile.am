# Makefile.am - src directory
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

guileextension_LTLIBRARIES = libguile-gi.la

libguile_gi_la_SOURCES = \
 gi_function_info.c \
 gi_function_info.h \
 gi_giargument.c \
 gi_giargument.h \
 gig_object.c \
 gig_object.h \
 gig_object_private.c \
 gig_object_private.h \
 gi_gvalue.c \
 gi_gvalue.h \
 gi_signal_closure.c \
 gi_signal_closure.h \
 gi_type_tag.c \
 gi_type_tag.h \
 gir.c \
 gir_arg_map.c \
 gir_arg_map.h \
 gir_callback.c \
 gir_callback.h \
 gir_function.c \
 gir_function.h \
 gir_constant.c \
 gir_constant.h \
 gir_flag.c \
 gir_flag.h \
 gir_typelib.c \
 gir_typelib.h \
 gir_type.c \
 gir_type.h \
 gi_util.c \
 gi_util.h

libguile_gi_la_CPPFLAGS = -DG_LOG_DOMAIN=\"GuileGI\"
if BUILDING_DLL
libguile_gi_la_CPPFLAGS += -DGIR_DLL -DGIR_DLL_EXPORTS
endif
if COVERAGE
libguile_gi_la_CPPFLAGS += -DENABLE_GCOV --coverage
endif
if MTRACE
libguile_gi_la_CPPFLAGS += -DMTRACE
endif

libguile_gi_la_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS) \
  $(GOBJECT_CFLAGS) \
  $(GOBJECT_INTROSPECTION_CFLAGS) \
  $(FFI_CFLAGS)

libguile_gi_la_LDFLAGS = \
 -no-undefined \
 -version-info $(LIBGUILE_GI_INTERFACE) \
 -export-symbols-regex '^gi[rg]?_' \
 $(GUILE_LDFLAGS)

if !DLL_VERSION_INFO
libguile_gi_la_LDFLAGS += -avoid-version
endif

if COVERAGE
libguile_gi_la_LDFLAGS += --coverage -Wl,--dynamic-list-data
endif

libguile_gi_la_LIBADD = \
 $(GUILE_LIBS) \
 $(GOBJECT_INTROSPECTION_LIBS) \
 $(GLIB_LIBS) \
 $(GOBJECT_LIBS) \
 $(FFI_LIBS)

################################################################

pkgguilesitedir=$(guilesitedir)/gi
pkgguileobjectdir=$(guileobjectdir)/gi

dist_guilesite_DATA = \
  gi.scm

dist_pkgguilesite_DATA = gi/oop.scm gi/core-generics.scm gi/util.scm

pkgguileobject_DATA = $(dist_guilesite_DATA:%.scm=%.go) $(dist_pkgguilesite_DATA:%.scm=%.go)

$(pkgguileobject_DATA): libguile-gi.la

GUILEC_FLAGS = -Warity-mismatch -Wformat --load-path=$(abs_srcdir)

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILE_TOOLS) compile --target="$(host)" $(GUILEC_FLAGS) \
	-o "$@" "$<"

CLEANFILES = $(pkgguileobject_DATA) $(BUILT_SOURCES)

################################################################
# FLYMAKE

.PHONY: check-syntax
check-syntax:
	$(CC) -std=c11 $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GOBJECT_CFLAGS) \
	 $(GOBJECT_INTROSPECTION_CFLAGS) $(FFI_CFLAGS) \
	 -DG_LOG_DOMAIN=\"GuileGI\" $(CFLAGS) -fsyntax-only $(CHK_SOURCES)

indent:
	VERSION_CONTROL=none indent $(libguile_gi_la_SOURCES)
