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
AUTOMAKE_OPTIONS = subdir-objects
AM_DISTCHECK_CONFIGURE_FLAGS = --with-gnu-filesystem-hierarchy --enable-introspection

#################
#  Directories  #
#################

cleanpkgdatadir = $(pkgdatadir)
exampledir = $(datadir)/doc/guile-gi
emptywindowdir = $(datadir)/doc/guile-gi/empty-window
builderdir = $(datadir)/doc/guile-gi/builder
resourcesdir = $(datadir)/doc/guile-gi/resources
pkgguilesitedir = $(guilesitedir)/gi
scriptguilesitedir = $(guilesitedir)/scripts
pkgguileobjectdir = $(guileobjectdir)/gi
scriptguileobjectdir = $(guileobjectdir)/scripts

#################
#   Top Level   #
#################

# Build ChangeLog from GIT history
dist: ChangeLog README
ChangeLog:
	$(AM_V_GEN) if test -d $(top_srcdir)/.git; then \
		GIT_DIR="$(top_srcdir)/.git" git log --stat > $@; \
	fi

README: README.md
	$(LN_S) $< $@

#################
# Documentation #
#################

info_TEXINFOS = doc/guile-gi.texi

docs:
	makeinfo $(info_TEXINFOS) --css-ref=docs/document-1.0.1.css --html --output=$(abs_srcdir)/docs

-distclean-docs:
	-rm -rf docs/*.html

#################
#    Examples   #
#################

dist_example_DATA = \
  examples/browser.scm \
  examples/editor.scm

dist_emptywindow_DATA = \
  examples/empty-window/main.scm \
  examples/empty-window/empty-app.scm \
  examples/empty-window/empty-app-window.scm

dist_builder_DATA = \
  examples/builder/main.scm \
  examples/builder/builder.ui

dist_resources_DATA = \
  examples/resources/main.scm \
  examples/resources/builder.ui \
  examples/resources/test.gresource.xml

nodist_resources_DATA = \
  examples/resources/test.gresource

examples/resources/test.gresource: AM_GRESOURCE_FLAGS = --sourcedir=$(abs_builddir)/examples/resources

.gresource.xml.gresource:
	$(GLIB_COMPILE_RESOURCES) $(AM_GRESOURCE_FLAGS) $(GRESOURCE_FLAGS) --target=$@ $<

#################
#  libguile-gi  #
#################

guileextension_LTLIBRARIES = libguile-gi.la

libguile_gi_la_c_sources = \
  src/gig_argument.c \
  src/gig_closure.c \
  src/gig_data_type.c \
  src/gig_object.c \
  src/gig_value.c \
  src/gig_signal.c \
  src/gig.c \
  src/gig_arg_map.c \
  src/gig_callback.c \
  src/gig_function.c \
  src/gig_constant.c \
  src/gig_flag.c \
  src/gig_repository.c \
  src/gig_document.c \
  src/gig_type.c \
  src/gig_type_private.c \
  src/gig_util.c

libguile_gi_la_internal_headers = \
  src/gig_argument.h \
  src/gig_closure.h \
  src/gig_data_type.h \
  src/gig_object.h \
  src/gig_value.h \
  src/gig_signal.h \
  src/gig_arg_map.h \
  src/gig_callback.h \
  src/gig_function.h \
  src/gig_function_private.h \
  src/gig_constant.h \
  src/gig_flag.h \
  src/gig_repository.h \
  src/gig_type.h \
  src/gig_type_private.h \
  src/gig_util.h

libguile_gi_la_SOURCES = \
  $(libguile_gi_la_internal_headers) \
  $(libguile_gi_la_c_sources)

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
 -export-symbols-regex '^gig_' \
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

#################
# Guile Modules #
#################

dist_guilesite_DATA = \
  module/gi.scm

nodist_pkgguilesite_DATA = \
  module/gi/config.scm

dist_pkgguilesite_DATA = \
  module/gi/core-generics.scm \
  module/gi/documentation.scm \
  module/gi/oop.scm \
  module/gi/repository.scm \
  module/gi/types.scm \
  module/gi/util.scm

dist_scriptguilesite_DATA = \
  module/scripts/gi-gtkdoc.scm

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
 --load-path=$(abs_srcdir)/module \
 --load-path=$(abs_builddir)/module

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILE_TOOLS) compile --target="$(host)" $(GUILEC_FLAGS) \
	-o "$@" "$<"

$(guileobject_DATA) $(pkgguileobject_DATA) $(scriptguileobject_DATA): $(guileextension_LTLIBRARIES)

#################
#     Tests     #
#################

TEST_EXTENSIONS = .scm

TYPELIB_TESTS = \
  test/typelib/document.scm \
  test/typelib/to-module.scm

IO_CHANNELS_TESTS = \
  test/io/io_channel_pipe.scm \
  test/io/io_channel_new_file.scm \
  test/io/io_channel_unichar.scm

MESSAGE_OUTPUT_TESTS = \
  test/io/log_writer_is_journald.scm

FILE_TESTS = \
  test/file/new_for_path.scm \
  test/file/get_parent.scm \
  test/file/has_parent.scm \
  test/file/get_child.scm

VALUE_TESTS = \
  test/value/getset.scm \
  test/value/transform.scm \
  test/closure/simple.scm

-include $(INTROSPECTION_MAKEFILE)
if HAVE_INTROSPECTION
pkglib_LTLIBRARIES = test/libeverything-1.0.la test/libmarshall-1.0.la

test/everything.c: $(GI_DATA_DIR)/tests/everything.c
	cp $< $@
test/everything.h: $(GI_DATA_DIR)/tests/everything.h
	cp $< $@
test/gitestmacros.h : $(GI_DATA_DIR)/tests/gitestmacros.h
	cp $< $@

GI_MARSHAL_SED = sed \
  -e "s/gimarshallingtests/marshall/" \
  -e "s/gi_marshalling_tests/marshall/g" \
  -e "s/GI_MARSHALLING_TESTS/MARSHALL/g" \
  -e "s/GIMarshallingTests/Marshall/g"

test/marshall.c: $(GI_DATA_DIR)/tests/gimarshallingtests.c
	$(GI_MARSHAL_SED) $< > $@
test/marshall.h: $(GI_DATA_DIR)/tests/gimarshallingtests.h
	$(GI_MARSHAL_SED) $< > $@

BUILT_SOURCES = \
  test/everything.c test/everything.h \
  test/marshall.c test/marshall.h \
  test/gitestmacros.h

INTROSPECTION_GIRS = test/Everything-1.0.gir test/Marshall-1.0.gir
INTROSPECTION_SCANNER_ARGS = \
  -I $(abs_srcdir)/test \
  -I $(abs_builddir)/test \
  --warn-all
INTROSPECTION_COMPILER_ARGS =
nodist_test_libeverything_1_0_la_SOURCES = test/everything.c test/everything.h
test_libeverything_1_0_la_CFLAGS = $(GOBJECT_CFLAGS)
test_libeverything_1_0_la_LDFLAGS =
test_libeverything_1_0_la_LIBADD = $(GOBJECT_LIBS)
if BUILDING_DLL
test_libeverything_1_0_la_LDFLAGS += -no-undefined
endif

nodist_test_libmarshall_1_0_la_SOURCES = \
  test/marshall.c test/marshall.h test/gitestmacros.h
test_libmarshall_1_0_la_CFLAGS = $(GOBJECT_CFLAGS) -I $(abs_builddir)/test
test_libmarshall_1_0_la_LDFLAGS =
test_libmarshall_1_0_la_LIBADD = $(GOBJECT_LIBS)
if BUILDING_DLL
test_libmarshall_1_0_la_LDFLAGS += -no-undefined
endif

test/Everything-1.0.gir: test/libeverything-1.0.la
test_Everything_1_0_gir_LIBS = test/libeverything-1.0.la
test_Everything_1_0_gir_INCLUDES = GObject-2.0
test_Everything_1_0_gir_FILES = test/everything.c test/everything.h

test/Marshall-1.0.gir: test/libmarshall-1.0.la
test_Marshall_1_0_gir_NAMESPACE = Marshall
test_Marshall_1_0_gir_LIBS = test/libmarshall-1.0.la
test_Marshall_1_0_gir_INCLUDES = GObject-2.0
test_Marshall_1_0_gir_FILES = test/marshall.c test/marshall.h

cleanpkgdata_DATA = \
  test/Everything-1.0.gir test/Marshall-1.0.gir \
  test/Everything-1.0.typelib test/Marshall-1.0.typelib

EVERYTHING_TESTS = \
  test/everything.scm \
  test/marshall.scm

TESTS_LTDLOPEN = \
  -dlopen ${top_builddir}/libguile-gi.la \
  -dlopen ${top_builddir}/test/libeverything-1.0.la \
  -dlopen ${top_builddir}/test/libmarshall-1.0.la
else
cleanpkgdata_DATA =
EVERYTHING_TESTS =
TESTS_LTDLOPEN = -dlopen ${top_builddir}/libguile-gi.la
endif

TESTS = \
  $(TYPELIB_TESTS) \
  test/constants.scm \
  test/mem.scm \
  $(MESSAGE_OUTPUT_TESTS) \
  $(IO_CHANNELS_TESTS) \
  $(FILE_TESTS) \
  test/string.scm \
  test/bash-the-hash.scm \
  test/datetime.scm \
  test/byte-array.scm \
  test/application.scm \
  test/gtk.scm \
  test/grilo.scm \
  test/oop.scm \
  $(VALUE_TESTS) \
  test/flags.scm \
  test/misc/use-typelibs.scm \
  test/misc/interfaces.scm \
  $(EVERYTHING_TESTS)

XFAIL_TESTS =

SCM_LOG_DRIVER = \
  $(top_builddir)/tools/uninstalled-test-env \
  $(top_builddir)/libtool --mode=execute $(TESTS_LTDLOPEN) \
  $(GUILE) --no-auto-compile $(top_srcdir)/build-aux/test-driver.scm

_OLD_SCHOOL_DRIVER = $(SHELL) $(top_srcdir)/build-aux/test-driver
_OLD_SCHOOL_COMPILER = ${builddir}/tools/run-test
_OLD_SCHOOL_TESTS = \
  $(TYPELIB_TESTS) \
  $(MESSAGE_OUTPUT_TESTS) \
  $(IO_CHANNELS_TESTS) \
  $(FILE_TESTS) \
  $(VALUE_TESTS) \
  test/misc/use-typelibs.scm \
  test/misc/interfaces.scm 

$(_OLD_SCHOOL_TESTS:%.scm=%.log): SCM_LOG_DRIVER = $(_OLD_SCHOOL_DRIVER)
$(_OLD_SCHOOL_TESTS:%.scm=%.log): SCM_LOG_COMPILER = $(_OLD_SCHOOL_COMPILER)

test: check

#################
#     Tools     #
#################

bin_SCRIPTS = tools/guile-gi

#################
#  Local rules  #
#################

distclean-local: -distclean-docs
html-local: docs

check-syntax:
	$(CC) -std=c11 $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GOBJECT_CFLAGS) \
	 $(GOBJECT_INTROSPECTION_CFLAGS) $(FFI_CFLAGS) \
	 -DG_LOG_DOMAIN=\"GuileGI\" $(CFLAGS) -fsyntax-only $(libguile_gi_la_c_sources)

indent:
	VERSION_CONTROL=none indent $(libguile_gi_la_SOURCES)

.PHONY: check-syntax indent test

#################
#   Variables   #
#################

noinst_DATA = \
  test/automake-test-lib.scm

CLEANFILES = \
  ChangeLog README \
  doc/guile-gi.dvi \
  examples/resources/test.gresource \
  $(guileobject_DATA) $(pkgguileobject_DATA) $(scriptguileobject_DATA) \
  tmp.txt \
  $(BUILT_SOURCES) \
  $(cleanpkgdata_DATA)

EXTRA_DIST = \
  README.md \
  build-aux/config.rpath \
  doc/example-1.scm doc/gtk-3.scm doc/fdl-1.3.texi \
  docs/document-1.0.1.css docs/README \
  examples/resources/test.gresource \
  m4/attributes.m4 \
  tools/uninstalled-env.in \
  tools/uninstalled-test-env.in \
  tools/run-guile.in \
  tools/run-test.in \
  tools/gdb-guile.in \
  tools/gdb-test.in \
  tools/guile-gi.in \
  tools/lcov.sh.in \
  tools/valgrind.sh.in \
  tools/valgrind.supp \
  $(TESTS) \
  $(noinst_DATA)

SUFFIXES = .gresource .gresource.xml
