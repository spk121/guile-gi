CLEANFILES=

noinst_LTLIBRARIES = libgirtest.la

libgirtest_la_SOURCES = \
 girtest.h \
 GirtestTriv.c GirtestTriv.h

libgirtest_la_CFLAGS = \
 $(AM_CFLAGS) \
 $(GLIB_CFLAGS)

#libgirtest_la_LDFLAGS = -no-undefined Wl,--no-undefined
libgirtest_la_LDFLAGS = -Wl,--no-as-needed

libgirtest_la_LIBADD = $(GLIB_LIBS)

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
noinst_DATA = $(INTROSPECTION_GIRS) $(INTROSPECTION_GIRS:.gir=.typelib)
CLEANFILES += $(noinst_DATA)
endif

-include $(top_srcdir)/git.mk

