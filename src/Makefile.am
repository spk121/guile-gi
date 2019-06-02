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

noinst_PROGRAMS = fo_gen$(EXEEXT)

fo_gen_SOURCES = \
 fo_gen.c

fo_gen_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS)

fo_gen_LDADD = $(GUILE_LIBS) $(GLIB_LIBS)

EXTRA_DIST = foreign-object-types.ini

BUILT_SOURCES = \
 __gi_gboxed.h __gi_gboxed.c \
 __gi_genum.h __gi_genum.c \
 __gi_gflags.h __gi_gflags.c \
 __gi_gobject.h __gi_gobject.c \
 __gi_gvalue.h __gi_gvalue.c \
 __gi_gparamspec.h __gi_gparamspec.c

__gi_gboxed.c: foreign-object-types.ini $(builddir)/fo_gen$(EXEEXT) Makefile
	$(builddir)/fo_gen$(EXEEXT) $< .
__gi_gboxed.h: foreign-object-types.ini $(builddir)/fo_gen$(EXEEXT) Makefile
	$(builddir)/fo_gen$(EXEEXT) $< .
__gi_genum.c: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_genum.h: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gflags.h: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gflags.c: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gobject.h: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gobject.c: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gvalue.h: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gvalue.c: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gparamspec.h: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .
__gi_gparamspec.c: $(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini Makefile
	$(builddir)/fo_gen$(EXEEXT) foreign-object-types.ini .

nodist_libguile_gi_la_SOURCES = \
 __gi_gboxed.c \
 __gi_gboxed.h \
 __gi_giargument.c \
 __gi_giargument.h \
 __gi_ginterface.h \
 __gi_gobject.c \
 __gi_gobject.h \
 __gi_gparamspec.c \
 __gi_gparamspec.h \
 __gi_gvalue.c \
 __gi_gvalue.h

libguile_gi_la_SOURCES = \
 gi_basictype.c \
 gi_basictype.h \
 gi_gboxed.c \
 gi_gboxed.h \
 gi_gflags.h \
 gi_giargument.c \
 gi_giargument.h \
 gi_ginterface.h \
 gi_gobject.c \
 gi_gobject.h \
 gi_gparamspec.c \
 gi_gparamspec.h \
 gi_gsignal.c \
 gi_gsignal.h \
 gi_gvalue.c \
 gi_gvalue.h \
 gi_signal_closure.c \
 gi_signal_closure.h \
 gir.c \
 gir_callback.c \
 gir_callback.h \
 gir_function.c \
 gir_function.h \
 gir_method.c \
 gir_method.h \
 gir_constant.c \
 gir_constant.h \
 gir_flag.c \
 gir_flag.h \
 gir_typelib.c \
 gir_typelib.h \
 gir_type.c \
 gir_type.h \
 gir_xguile.c \
 gir_xguile.h

libguile_gi_la_CPPFLAGS = -DG_LOG_DOMAIN=\"GuileGI\"
if BUILDING_DLL
libguile_gi_la_CPPFLAGS += -DGIR_DLL -DGIR_DLL_EXPORTS
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
 $(GUILE_LDFLAGS)

if !DLL_VERSION_INFO
libguile_gi_la_LDFLAGS += -avoid-version
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

dist_pkgguilesite_DATA = gi/glib-2.scm gi/gtk-3.scm gi/gio-2.scm gi/webkit2-4.scm

pkgguileobject_DATA = $(dist_guilesite_DATA:%.scm=%.go) $(dist_pkgguilesite_DATA:%.scm=%.go)

$(pkgguileobject_DATA): libguile-gi.la

GUILEC_FLAGS = -Warity-mismatch -Wformat --load-path=$(PWD)

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
