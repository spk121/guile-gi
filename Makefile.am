#        +         +         +         +         +         +         +        |
ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
DISTCHECK_CONFIGURE_FLAGS =
CLEANFILES=

BUILT_SOURCES = docs/index.html

info_TEXINFOS = doc/guile-gi.texi

lib_LTLIBRARIES = libguile-gi.la

noinst_PROGRAMS = fo_gen$(EXEEXT)

fo_gen_SOURCES = \
 src/gi/fo_gen.c

fo_gen_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS) \
  $(GOBJECT_CFLAGS) \
  $(GOBJECT_INTROSPECTION_CFLAGS) \
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

src/gi/__gi_gboxed.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gboxed.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_genum.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_genum.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_genumcollection.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_genumcollection.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gflags.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gflags.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gflagscollection.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gflagscollection.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gobject.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gobject.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gtype.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gtype.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gvalue.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gvalue.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gparamspec.h: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
src/gi/__gi_gparamspec.c: $(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini
	$(builddir)/fo_gen$(EXEEXT) $(srcdir)/src/gi/foreign-object-types.ini $(srcdir)/src/gi


libguile_gi_la_SOURCES = \
 src/gi/__gi_gboxed.c \
 src/gi/__gi_gboxed.h \
 src/gi/__gi_giargument.c \
 src/gi/__gi_giargument.h \
 src/gi/__gi_ginterface.h \
 src/gi/__gi_gobject.c \
 src/gi/__gi_gobject.h \
 src/gi/__gi_gparamspec.c \
 src/gi/__gi_gparamspec.h \
 src/gi/__gi_gtype.c \
 src/gi/__gi_gtype.h \
 src/gi/__gi_gvalue.c \
 src/gi/__gi_gvalue.h \
 src/gi/gi_basictype.c \
 src/gi/gi_basictype.h \
 src/gi/gi_gboxed.c \
 src/gi/gi_gboxed.h \
 src/gi/gi_gflags.h \
 src/gi/gi_giargument.c \
 src/gi/gi_giargument.h \
 src/gi/gi_ginterface.h \
 src/gi/gi_gobject.c \
 src/gi/gi_gobject.h \
 src/gi/gi_gparamspec.c \
 src/gi/gi_gparamspec.h \
 src/gi/gi_gsignal.c \
 src/gi/gi_gsignal.h \
 src/gi/gi_gstruct.c \
 src/gi/gi_gstruct.h \
 src/gi/gi_gtype.c \
 src/gi/gi_gtype.h \
 src/gi/gi_gvalue.c \
 src/gi/gi_gvalue.h \
 src/gi/gi_signal_closure.c \
 src/gi/gi_signal_closure.h \
 src/gi/gir.c \
 src/gi/gir_callback.c \
 src/gi/gir_callback.h \
 src/gi/gir_func.c \
 src/gi/gir_func.h \
 src/gi/gir_func2.c \
 src/gi/gir_func2.h \
 src/gi/gir_type.c \
 src/gi/gir_type.h \
 src/gi/gir_xguile.c \
 src/gi/gir_xguile.h

libguile_gi_la_CFLAGS = \
  -std=c11 \
  $(GUILE_CFLAGS) \
  $(GLIB_CFLAGS) \
  $(GOBJECT_CFLAGS) \
  $(GOBJECT_INTROSPECTION_CFLAGS) \
  $(FFI_CFLAGS) \
  -DG_LOG_DOMAIN=\"GuileGI\"

libguile_gi_la_LDFLAGS = -no-undefined -version-info 0:0:0
libguile_gi_la_LIBADD = \
 $(GUILE_LIBS) \
 $(GOBJECT_INTROSPECTION_LIBS) \
 $(GLIB_LIBS) \
 $(GOBJECT_LIBS) \
 $(FFI_LIBS)

# Build ChangeLog from GIT history
dist: ChangeLog
.PHONY: ChangeLog
ChangeLog:
	$(AM_V_GEN) if test -d $(top_srcdir)/.git; then \
		GIT_DIR="$(top_srcdir)/.git" git log --stat > $@; \
	fi

docs/index.html: doc/guile-gi.texi
	makeinfo $< --css-ref=document-1.0.1.css --html -o docs

pkgdata_DATA = \
  src/gi.scm

################################################################
# FLYMAKE

.PHONY: check-syntax
check-syntax:
	$(CC) -std=c11 $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GOBJECT_CFLAGS) \
	 $(GOBJECT_INTROSPECTION_CFLAGS) $(FFI_CFLAGS) \
	 DG_LOG_DOMAIN=\"GuileGI\" $(CFLAGS) -fsyntax-only $(CHK_SOURCES)


################################################################
# Test driver

AM_TESTS_ENVIRONMENT = LD_LIBRARY_PATH="$(builddir)/.libs"; export LD_LIBRARY_PATH;
TESTS = \
 test/memchk_1.scm \
 test/memchk_2.scm \
 test/mainloop.scm

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = ${builddir}/libtool \
    -dlopen ${builddir}/libguile-gi.la \
    --mode=execute guile
AM_SCM_LOG_FLAGS = --no-auto-compile -L ${srcdir}/src -L ${srcdir}/test/. -s

EXTRA_DIST = \
  $(TESTS) \
  COPYING \
  src/gi/foreign-object-types.ini \
  doc/fdl-1.3.texi \
  test/lib.scm \
  build-aux/config.rpath \
  m4/attributes.m4 \
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
