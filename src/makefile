all: libguile-gi.so libguile-gi.a guile-gi

CC = gcc

PKG_CFLAGS != pkg-config guile-3.0 gobject-introspection-1.0 --cflags

MORE_CFLAGS += \
 -Warith-conversion -Wconversion -Wsign-conversion -Wfloat-conversion \
 -Wunused-macros -Wunused-parameter \
 -Wpointer-arith -Wincompatible-pointer-types \
 -Wmissing-prototypes -Wmissing-declarations \
 -Wswitch-enum  \
 -Wimplicit-fallthrough -Wdouble-promotion \
 -Wcast-align -Wsign-compare \
 -Wpedantic \
 -Wcast-qual \
 -Wformat-nonliteral \
 -Wbad-function-cast -Wunreachable-code -fanalyzer

EVEN_MORE_CFLAGS = -Wpadded

CPPFLAGS = -I .
CFLAGS := $(CPPFLAGS) -g -Og -Wall -fpic $(PKG_CFLAGS)

PKG_LIBS != pkg-config guile-3.0 gobject-introspection-1.0 libffi --libs
LIBS := $(PKG_LIBS)

# OBJS = \
# gig_arg_map.o \
# gig_argument.o \
# gig.o \
# gig_callback.o \
# gig_closure.o \
# gig_constant.o \
# gig_data_type.o \
# gig_document.o \
# gig_flag.o \
# gig_function_args.o \
# gig_function.o \
# gig_keyval.o \
# gig_logging.o \
# gig_mem_list.o \
# gig_object.o \
# gig_repository.o \
# gig_signal.o \
# gig_type.o \
# gig_type_private.o \
# gig_util.o \
# gig_value.o

OBJS = \
x/keyval.o \
x/logging.o \
x/mem.o \
x/slist.o \
x/string.o \
x/strval.o \
y/arg.o \
y/arg_map.o \
y/argument.o \
y/callback.o \
y/closure.o \
y/constant.o \
y/flag.o \
y/function_args.o \
y/function.o \
y/guile.o \
y/meta_type.o \
y/object.o \
y/repository.o \
y/signal.o \
y/type.o \
y/type_private.o \
y/value.o \
y/util.o

libguile-gi.so: $(OBJS)
	$(CC) -shared -o $@ $(OBJS) $(LIBS)

libguile-gi.a: $(OBJS)
	ar cru libguile-gi.a $(OBJS)
	ranlib libguile-gi.a

guile-gi: libguile-gi.a main.o
	$(CC) -o $@ main.o libguile-gi.a $(LIBS)

clean:
	-rm $(OBJS) libguile-gi.so libguile-gi.a guile-gi
