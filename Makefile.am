#        +         +         +         +         +         +         +         |
ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
DISTCHECK_CONFIGURE_FLAGS = --enable-introspection
CLEANFILES=

BUILT_SOURCES = docs/index.html

info_TEXINFOS = doc/guile-gi.texi

guileextension_LTLIBRARIES = libguile-gi.la

noinst_PROGRAMS = guile-gi fo_gen

fo_gen_SOURCES = \
 src/gi/fo_gen.c

fo_gen_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS) \
  $(GOBJECT_CFLAGS) \
  $(GOBJECT_INTROSPECTION_CFLAGS) \
  -DGUILE_GI_CORE \
  -DG_LOG_DOMAIN=\"GuileGI\"
fo_gen_LDADD = $(GUILE_LIBS) $(GOBJECT_INTROSPECTION_LIBS) $(GLIB_LIBS) $(GOBJECT_LIBS)

BUILT_SOURCES += \
 src/gi/__gi_gboxed.h src/gi/__gi_gboxed.c \
 src/gi/__gi_genum.h src/gi/__gi_genum.c \
 src/gi/__gi_genumcollection.h src/gi/__gi_genumcollection.c \
 src/gi/__gi_gflags.h src/gi/__gi_gflags.c \
 src/gi/__gi_gflagscollection.h src/gi/__gi_gflagscollection.c \
 src/gi/__gi_gobject.h src/gi/__gi_gobject.c \
 src/gi/__gi_gtype.h src/gi/__gi_gtype.c \
 src/gi/__gi_gvalue.h src/gi/__gi_gvalue.c \
 src/gi/__gi_gparamspec.h src/gi/__gi_gparamspec.c

src/gi/__gi_gboxed.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gboxed.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_genum.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_genum.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_genumcollection.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_genumcollection.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gflags.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gflags.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gflagscollection.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gflagscollection.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gobject.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gobject.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gtype.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gtype.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gvalue.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gvalue.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gparamspec.h: $(builddir)/fo_gen src/gi/foreign-object-types.ini
src/gi/__gi_gparamspec.c: $(builddir)/fo_gen src/gi/foreign-object-types.ini
	$(builddir)/fo_gen $(top_srcdir)/src/gi/foreign-object-types.ini $(top_srcdir)/src/gi

guile_gi_SOURCES = \
 src/gi/__gi_gobject.h src/gi/__gi_gobject.c \
 src/gi/gi_gobject.h src/gi/gi_gobject.c \
 src/gi/__gi_gvalue.h src/gi/__gi_gvalue.c \
 src/gi/gi_gvalue.h src/gi/gi_gvalue.c \
 src/gi/__gi_gtype.h src/gi/__gi_gtype.c \
 src/gi/gi_gtype.h src/gi/gi_gtype.c \
 src/gi/__gi_gparamspec.h src/gi/__gi_gparamspec.c \
 src/gi/gi_gparamspec.h src/gi/gi_gparamspec.c \
 src/gi/gi_gsignal.h src/gi/gi_gsignal.c \
 src/gi/gi_giargument.h src/gi/gi_giargument.c \
 src/gi/gi_basictype.h src/gi/gi_basictype.c

# src/gi/__gi_gboxed.h src/gi/__gi_gboxed.c \
 # src/gi/gi_gboxed.h src/gi/gi_gboxed.c \
 # src/gi/__gi_genum.h src/gi/__gi_genum.c \
 # src/gi/gi_genum.h src/gi/gi_genum.c \
 # scr/gi/__gi_genumcollection.h src/gi/__gi_genumcollection.c \
 # src/gi/__gi_gflags.h src/gi/__gi_gflags.c \
 # src/gi/gi_gflags.h src/gi/gi_gflags.c \
 # src/gi/__gi_gflagscollection.h src/gi/__gi_gflagscollection.c \
 # src/gi/gi_gvalue.h src/gi/gi_gvalue.c \
 # src/gi/stubs.c \
 # src/gi/pycompat.c \
 # src/gi/gir_xguile.c src/gi/gir_xguile.h \
 # src/gi/gir_g_type.c \
 # src/gi/gir_g_value.c \
 # src/gi/gir-object.c src/gi/gir-object.h \
 # src/gi/gir_ginterface.c src/gi/gir_ginterface.h \
 # src/gi/gimodule.c src/gi/gimodule.h \
 # src/gi/gi_util.c src/gi/gi_util.h

# src/gi/gimodule.c
# src/gi/gugobject.c

# src/gi/gugobject-object.c \
# src/gi/gugi-object.c

guile_gi_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS) \
  $(GOBJECT_CFLAGS) \
  $(GOBJECT_INTROSPECTION_CFLAGS) \
  -DGUILE_GI_CORE \
  -DSTANDALONE \
  -DG_LOG_DOMAIN=\"GuileGI\"

guile_gi_LDFLAGS = -no-undefined
guile_gi_LDADD = $(GUILE_LIBS) $(GOBJECT_INTROSPECTION_LIBS) $(GLIB_LIBS) $(GOBJECT_LIBS)

libguile_gi_la_SOURCES = \
 src/gi/__gi_gobject.h src/gi/__gi_gobject.c \
 src/gi/gi_gobject.h src/gi/gi_gobject.c \
 src/gi/__gi_gvalue.h src/gi/__gi_gvalue.c \
 src/gi/gi_gvalue.h src/gi/gi_gvalue.c \
 src/gi/__gi_gtype.h src/gi/__gi_gtype.c \
 src/gi/gi_gtype.h src/gi/gi_gtype.c \
 src/gi/__gi_gparamspec.h src/gi/__gi_gparamspec.c \
 src/gi/gi_gparamspec.h src/gi/gi_gparamspec.c \
 src/gi/gi_gsignal.h src/gi/gi_gsignal.c

libguile_gi_la_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS) \
  $(GOBJECT_CFLAGS) \
  $(GOBJECT_INTROSPECTION_CFLAGS) \
  -DG_LOG_DOMAIN=\"GuileGI\"

libguile_gi_la_LDFLAGS = -no-undefined -version-info 0:0:0
libguile_gi_la_LIBADD = $(GUILE_LIBS) $(GOBJECT_INTROSPECTION_LIBS) $(GLIB_LIBS) $(GOBJECT_LIBS)

# Build ChangeLog from GIT  history
dist: ChangeLog
.PHONY: ChangeLog
ChangeLog:
	$(AM_V_GEN) if test -d $(top_srcdir)/.git; then \
		GIT_DIR="$(top_srcdir)/.git" git log --stat > $@; \
	fi

docs/index.html: doc/guile-gi.texi
	texi2any $< --css-ref=document-1.0.1.css --html -o docs

dist_guilesite_DATA = \
  src/gi.scm

################################################################
# Set up the test libraries that we will use for testing

check_LTLIBRARIES = libgirtest.la

libgirtest_la_SOURCES = \
 test/girtest.h \
 test/GirtestTriv.c test/GirtestTriv.h

libgirtest_la_CFLAGS = \
 $(AM_CFLAGS) \
 $(GLIB_CFLAGS)

#libgirtest_la_LDFLAGS = -no-undefined Wl,--no-undefined
libgirtest_la_LDFLAGS = -Wl,--no-as-needed -no-undefined -no-install

libgirtest_la_LIBADD = $(GLIB_LIBS)


################################################################
# GObject Introspection of test libraries

-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = --add-include-path=$(top_srcdir)/test --warn-all
INTROSPECTION_COMPILER_ARGS = --includedir=$(top_srcdir)/test

if HAVE_INTROSPECTION
introspection_sources = $(libgirtest_la_SOURCES)

Girtest-1.0.gir: libgirtest.la
Girtest_1_0_gir_INCLUDES = GObject-2.0
Girtest_1_0_gir_CFLAGS = $(INCLUDES)
Girtest_1_0_gir_LIBS = libgirtest.la
Girtest_1_0_gir_FILES = $(introspection_sources)
INTROSPECTION_GIRS += Girtest-1.0.gir
check_DATA = $(INTROSPECTION_GIRS) $(INTROSPECTION_GIRS:.gir=.typelib)
CLEANFILES += $(check_DATA)
endif

################################################################
# FLYMAKE

.PHONY: check-syntax
check-syntax:
	$(CC)  -std=c11 $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GOBJECT_CFLAGS) \
	 $(GOBJECT_INTROSPECTION_CFLAGS) \
	 -DGUILE_GI_CORE -DG_LOG_DOMAIN=\"GuileGI\" $(CFLAGS) -fsyntax-only $(CHK_SOURCES)


################################################################
# Test driver

# TEST_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' GUILE_AUTO_COMPILE=0 $(SHELL) \
#                   $(top_srcdir)/build-aux/tap-driver.sh
AM_TESTS_ENVIRONMENT = LD_LIBRARY_PATH="$(top_builddir)/.libs"; export LD_LIBRARY_PATH;
TESTS = \
 test/test-setup.scm \
 test/test-gobject.scm \
 test/test-test.scm

TEST_EXTENSIONS = .scm
#SCM_LOG_COMPILER = ${top_builddir}/libtool \
#    -dlopen ${top_builddir}/src/gi/libguile-gi.la \
#    --mode=execute guile
AM_SCM_LOG_FLAGS = --no-auto-compile -L ${top_srcdir}/src -L ${top_srcdir}/test/. -s
SCM_LOG_COMPILER = $(GUILE)
# AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir)/src

EXTRA_DIST = \
  $(TESTS) \
  COPYING \
  bootstrap.sh \
  build-aux/config.rpath \
  m4/attributes.m4 \
  m4/ax_c___attribute__.m4 \
  docs/document-1.0.1.css

MAINTAINERCLEANFILES = \
  Makefile.in aclocal.m4 build-aux/compile		\
  build-aux/config.guess build-aux/config.sub			\
  build-aux/depcomp build-aux/install-sh			\
  build-aux/ltmain.sh build-aux/missing		\
  configure m4/libtool.m4 m4/ltoptions.m4	\
  m4/ltsugar.m4 m4/ltversion.m4 m4/lt~obsolete.m4 \
  src/gi/config.h.in src/gi/config.h.in~ \
  docs/*.html
