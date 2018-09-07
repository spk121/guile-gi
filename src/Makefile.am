lib_LTLIBRARIES = libguile-gir.la

libguile_gir_la_SOURCES = \
 gir.c

libguile_gir_la_CFLAGS = \
 -std=c11 \
 $(GUILE_CFLAGS) \
 $(GOBJECT_INTROSPECTION_CFLAGS)
libguile_gir_la_LDFLAGS = -no-undefined Wl,--no-undefined
libguile_gir_la_LIBADD = $(GUILE_LIBS) $(GOBJECT_INTROSPECTION)

-include $(top_srcdir)/git.mk
