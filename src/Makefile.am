lib_LTLIBRARIES = libguile-gir.la

libguile_gir_la_SOURCES = \
 gir_module.c \
 gir_error.c gir_error.h \
 gir_foreign.c gir_foreign.h \
 gir_info.c gir_info.h \
 gir_repository.c gir_repository.h \
 gir_type.c gir_type.h

 # gir.c \
 # gir_gtype.c gir_gtype.h \
 # gir_gboxed.c gir_gboxed.h

libguile_gir_la_CFLAGS = \
 -std=c11 \
 $(GUILE_CFLAGS) \
 $(GLIB_CFLAGS) \
 $(GOBJECT_CFLAGS) \
 $(GOBJECT_INTROSPECTION_CFLAGS) \
 -DG_LOG_DOMAIN=\"Gir\"

#  -DG_USE_LOG_STRUCTURED

libguile_gir_la_LDFLAGS = -no-undefined Wl,--no-undefined
libguile_gir_la_LIBADD = $(GUILE_LIBS) $(GOBJECT_INTROSPECTION_LIBS) $(GLIB_LIBS) $(GOBJECT_LIBS)

-include $(top_srcdir)/git.mk
