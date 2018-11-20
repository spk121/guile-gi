# Makefile - a GNU-style makefile
# Copyright (C) 2018 Michael L. Gran
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!/usr/bin/make -f

#SHELL = /bin/sh

################################################################
#HOST=WIN32
HOST=LINUX
#HOST=CYGWIN
#HOST=DARWIN

#PROC=IA32
PROC=AMD64
#PROC=ARM

#GCOV=YES
GCOV=NO

################################################################
# Direcories

PACKAGE_TARNAME=guile-gi
prefix = /usr/local
exec_prefix = ${prefix}
datarootdir = ${prefix}/share
datadir = ${datarootdir}
infodir = ${datarootdir}/info
guileextensiondir = ${exec_prefix}/lib/guile/
guileobjectdir = ${exec_prefix}/lib/guile/site-ccache
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
pdfdir = ${docdir}
guilesitedir = $(datadir)/guile/site/
pkgdatadir = $(datadir)/guile-gi


################################################################
# External Programs

CC = gcc
LD = ld
GUILD = guild
GUILE = guile
GUILE_EFFECTIVE_VERSION = 2.2

################################################################
# Building the library

EXEEXT=
ifeq ($(HOST),CYGWIN)
EXEEXT=.exe
endif

LIBEXT=so
ifeq ($(HOST),CYGWIN)
LIBEXT=dll
endif

all: libguile-gi.$(LIBEXT) fo_gen$(EXEEXT) info pdf html

DEFS = \
  -DG_LOG_DOMAIN=\"GuileGI\" \
  -D_FORTIFY_SOURCE=2 \
  -DSCM_DEBUG_TYPING_STRICTNESS=2

INCLUDES = \
 -I src/gi

CPPFLAGS = \
  $(DEFS) \
  $(INCLUDES) \
  $(shell pkg-config --cflags-only-I guile-2.2 glib-2.0 gobject-2.0 gobject-introspection-1.0 libffi)

CFLAGS = \
 -fasynchronous-unwind-tables -fexceptions \
 -fstack-protector-strong \
 -g -grecord-gcc-switches \
 -O0 -pipe \
 -Wall -Werror=format-security -Werror=implicit-function-declaration -Wshadow \
 -fno-omit-frame-pointer -fdiagnostics-color=auto \
 $(shell pkg-config --cflags-only-other guile-2.2 glib-2.0 gobject-2.0 gobject-introspection-1.0 libffi)

ifeq ($(CC),gcc)
CFLAGS += \
 -fvar-tracking
endif

ifeq ($(GCOV),YES)
CFLAGS += --coverage
DEFS += -DENABLE_GCOV
endif

ifeq ($(HOST),LINUX)
CFLAGS += \
 -fPIC \
 -fstack-clash-protection \
 -fcf-protection
ifeq ($(CC),gcc)
CFLAGS += \
 -fplugin=annobin
endif
endif

LDFLAGS = \
 -Wl,--dynamic-list-data \
 $(shell pkg-config --libs-only-L guile-2.2 glib-2.0 gobject-2.0 gobject-introspection-1.0 libffi)

ifeq ($(HOST),LINUX)
LDFLAGS += \
 -Wl,-z,defs \
 -Wl,-z,now \
 -Wl,-z,relro
endif

ifeq ($(LD),gold)
LDFLAGS += -fuse-ld=gold
else
ifeq ($(LD),lld)
LDFLAGS += -fuse-ld=lld
endif
endif

LIBS = \
 $(shell pkg-config --libs-only-l --libs-only-other guile-2.2 glib-2.0 gobject-2.0 gobject-introspection-1.0 libffi)

# Build helper program fo_gen

fo_gen$(EXEEXT): src/gi/fo_gen.c
	$(CC) $(CFLAGS) $(LDFLAGS) -fpie -pie -o $@ $< `pkg-config --cflags --libs glib-2.0`

C_HEADERS = \
 src/gi/__gi_gobject.h \
 src/gi/gi_gobject.h \
 src/gi/__gi_gvalue.h \
 src/gi/gi_gvalue.h \
 src/gi/__gi_gtype.h \
 src/gi/gi_gtype.h \
 src/gi/__gi_gparamspec.h \
 src/gi/gi_gparamspec.h \
 src/gi/gi_gsignal.h \
 src/gi/gi_giargument.h \
 src/gi/gi_basictype.h \
 src/gi/gir_func.h \
 src/gi/gir_func2.h \
 src/gi/gir_type.h \
 src/gi/gi_signal_closure.h \
 src/gi/__gi_giargument.h \
 src/gi/gi_ginterface.h \
 src/gi/__gi_ginterface.h \
 src/gi/gi_gstruct.h \
 src/gi/gi_gflags.h \
 src/gi/gir_xguile.h \
 src/gi/__gi_gboxed.h \
 src/gi/gi_gboxed.h \
 src/gi/gir_callback.h

C_SOURCES = \
 src/gi/__gi_gobject.c \
 src/gi/gi_gobject.c \
 src/gi/__gi_gvalue.c \
 src/gi/gi_gvalue.c \
 src/gi/__gi_gtype.c \
 src/gi/gi_gtype.c \
 src/gi/__gi_gparamspec.c \
 src/gi/gi_gparamspec.c \
 src/gi/gi_gsignal.c \
 src/gi/gi_giargument.c \
 src/gi/gi_basictype.c \
 src/gi/gir_func.c \
 src/gi/gir_func2.c \
 src/gi/gir_type.c \
 src/gi/gi_signal_closure.c \
 src/gi/__gi_giargument.c \
 src/gi/gi_gstruct.c \
 src/gi/gir_callback.c \
 src/gi/gir.c \
 src/gi/gir_xguile.c \
 src/gi/__gi_gboxed.c \
 src/gi/gi_gboxed.c \
 src/gi/gir_callback.c

SCM_SOURCES = \
 src/gi.scm

C_OBJECTS = $(C_SOURCES:.c=.o)

src/gi/__gi_gstruct.c: src/gi/foreign-object-types.ini fo_gen$(EXEEXT)
	./fo_gen$(EXEEXT) src/gi/foreign-object-types.ini src/gi

src/gi/__gi_gobject.c: src/gi/foreign-object-types.ini fo_gen$(EXEEXT)
	./fo_gen$(EXEEXT) src/gi/foreign-object-types.ini src/gi

src/gi/gi_gstruct.c: src/gi/__gi_gstruct.c

src/gi/gi_gobject.c: src/gi/__gi_gobject.c

libguile-gi.$(LIBEXT): $(C_OBJECTS)
	@rm -f libguile-gi.$(LIBEXT)
	$(CC) $(CFLAGS) -shared $(LDFLAGS) -o $@ $^ $(LIBS)

################################################################
# Documentation

.PHONY: info pdf html

info: doc/guile-gi.info

doc/guile-gi.info: doc/guile-gi.texi
	makeinfo -I doc -o $@ $<

pdf: doc/guile-gi.pdf

doc/guile-gi.pdf: doc/guile-gi.texi
	makeinfo -I doc --pdf -o $@ $<

html: docs/index.html

docs/index.html: doc/guile-gi.texi
	@rm -f docs/*.html 
	makeinfo $< --css-ref=document-1.0.1.css --html -o docs

################################################################
# Tarballs

DISTFILES = $(C_SOURCES) $(C_HEADERS) $(SCM_SOURCES) \
  src/gi/fo_gen.c \
  doc/guile-gi.texi docs/document-1.0.1.css \
  README.md AUTHORS NEWS COPYING \
  Makefile Makefile.mingw ChangeLog

.PHONY: dist
dist: guile-gi-0.0.1.tar.gz ChangeLog

.PHONY: ChangeLog
ChangeLog:
	if test -d .git; then \
		git log --stat > $@; \
	fi

guile-gi-0.0.1.tar.gz: guile-gi-0.0.1.tar
	gzip guile-gi-0.0.1.tar

guile-gi-0.0.1.tar: $(DISTFILES)
	tar --create --format=ustar --verbose --dereference \
	  --transform 's,^,guile-gi-0.0.1/,' --file=$@ $^

################################################################
# Check

.PHONY: check

TESTS = test/memchk_1.scm \
 test/mainloop.scm

check:
	@for t in $(TESTS); do \
	  echo test $$t; \
	  LD_LIBRARY_PATH=. $(GUILE) --no-auto-compile -L src -L test $$t; \
	  if [ $$? ]; then \
	    echo " PASS"; \
	  else \
	    echo " FAIL"; \
	  fi \
	done


################################################################
# Misc

.PHONY: install clean

install: libguile-gi.$(LIBEXT) doc/guile-gi.info doc/guile-gi.pdf
	install -D -v -m 644 libguile-gi.$(LIBEXT) $(guileextensiondir)
	install -D -v -m 644 $(SCM_SOURCES) $(guilesitedir)
	install -D -v -m 644 doc/guile-gi.info $(infodir)
	install -D -v -m 644 doc/guile-gi.pdf $(pdfdir)

CLEAN_FILES= $(C_OBJECTS) libguile-gi.$(LIBEXT)

clean:
	@-rm $(CLEAN_FILES)


################################################################
# Flymake

.PHONY: check-syntax
check-syntax:
	$(CC) $(CPPFLAGS) $(CFLAGS) -fsyntax-only $(CHK_SOURCES)

