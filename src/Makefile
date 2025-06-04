# Makefile for Modernized GIRepository Extension
# Copyright (C) 2025 Michael L. Gran

# ============================================================================
# Configuration
# ============================================================================

# Package information
PACKAGE = gig-girepository
VERSION = 2.0.0
TARNAME = $(PACKAGE)-$(VERSION)

# Build configuration
CC = gcc
CFLAGS = -Wall -Wextra -O2 -g -fPIC -std=c11 -fno-diagnostics-color
CPPFLAGS = -D_GNU_SOURCE
LDFLAGS = -shared
LIBS = -L/ucrt64/lib `pkg-config guile-3.0 --libs`

# Installation directories
PREFIX = /usr/local
LIBDIR = $(PREFIX)/lib
INCLUDEDIR = $(PREFIX)/include
PKGCONFIGDIR = $(LIBDIR)/pkgconfig
GUILE_SITEDIR = $(shell pkg-config --variable=sitedir guile-3.0)
GUILE_EXTENSIONDIR = $(shell pkg-config --variable=extensiondir guile-3.0)

# Dependency detection
PKG_CONFIG = pkg-config
GUILE_CFLAGS = $(shell $(PKG_CONFIG) --cflags guile-3.0)
GUILE_LIBS = $(shell $(PKG_CONFIG) --libs guile-3.0)
GI_CFLAGS = $(shell $(PKG_CONFIG) --cflags gobject-introspection-1.0)
GI_LIBS = $(shell $(PKG_CONFIG) --libs gobject-introspection-1.0)
GLIB_CFLAGS = $(shell $(PKG_CONFIG) --cflags glib-2.0)
GLIB_LIBS = $(shell $(PKG_CONFIG) --libs glib-2.0)

# Combine flags
ALL_CFLAGS = $(CFLAGS) $(CPPFLAGS) $(GUILE_CFLAGS) $(GI_CFLAGS) $(GLIB_CFLAGS)
ALL_LIBS = $(LIBS) $(GUILE_LIBS) $(GI_LIBS) $(GLIB_LIBS)

# ============================================================================
# Source Files
# ============================================================================

# Core library sources
CORE_SOURCES = \
	core/mem.c \
	core/logging.c \
	core/str.c \
	core/slist.c \
	core/keyval.c \
	core/scheme.c \
	core/oop.c

# GIRepository sources
GI_SOURCES = \
	gi/gig_girepository.c

# All sources
SOURCES = $(CORE_SOURCES) $(GI_SOURCES)

# Object files
CORE_OBJECTS = $(CORE_SOURCES:.c=.o)
GI_OBJECTS = $(GI_SOURCES:.c=.o)
OBJECTS = $(SOURCES:.c=.o)

# Headers
CORE_HEADERS = \
	core/visibility.h \
	core/mem.h \
	core/logging.h \
	core/str.h \
	core/slist.h \
	core/keyval.h \
	core/scheme.h \
	core/oop.h

GI_HEADERS = \
	gi/gig_girepository.h

HEADERS = $(CORE_HEADERS) $(GI_HEADERS)

# Scheme files
SCHEME_FILES = \
	scheme/gi/repository.scm

# Example files
EXAMPLE_FILES = \
	examples/gi-example.scm

# Documentation
DOC_FILES = \
	README.md \
	CHANGELOG.md \
	LICENSE

# ============================================================================
# Build Targets
# ============================================================================

# Default target
all: libgig-core.so libgig-girepository.so

# Core library
libgig-core.so: $(CORE_OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^ $(ALL_LIBS)

# GIRepository extension
libgig-girepository.so: $(GI_OBJECTS) libgig-core.so
	$(CC) $(LDFLAGS) -o $@ $(GI_OBJECTS) -L. -lgig-core $(ALL_LIBS)

# Object file compilation
.c.o: $(HEADERS)
	$(CC) $(ALL_CFLAGS) -Icore -Igi -c $< -o $@

# ============================================================================
# Installation
# ============================================================================

install: all install-libs install-headers install-scheme install-examples
	@echo "Installation completed to $(PREFIX)"

install-libs: libgig-core.so libgig-girepository.so
	install -d $(DESTDIR)$(LIBDIR)
	install -d $(DESTDIR)$(GUILE_EXTENSIONDIR)/gi
	install -m 755 libgig-core.so $(DESTDIR)$(LIBDIR)/
	install -m 755 libgig-girepository.so $(DESTDIR)$(GUILE_EXTENSIONDIR)/gi/

install-headers: $(HEADERS)
	install -d $(DESTDIR)$(INCLUDEDIR)/gig/core
	install -d $(DESTDIR)$(INCLUDEDIR)/gig/gi
	install -m 644 $(CORE_HEADERS) $(DESTDIR)$(INCLUDEDIR)/gig/core/
	install -m 644 $(GI_HEADERS) $(DESTDIR)$(INCLUDEDIR)/gig/gi/

install-scheme: $(SCHEME_FILES)
	install -d $(DESTDIR)$(GUILE_SITEDIR)/gi
	install -m 644 $(SCHEME_FILES) $(DESTDIR)$(GUILE_SITEDIR)/gi/

install-examples: $(EXAMPLE_FILES)
	install -d $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)/examples
	install -m 644 $(EXAMPLE_FILES) $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)/examples/
	install -m 644 $(DOC_FILES) $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)/

# Create pkg-config file
install-pkgconfig:
	install -d $(DESTDIR)$(PKGCONFIGDIR)
	sed -e 's|@PREFIX@|$(PREFIX)|g' \
	    -e 's|@VERSION@|$(VERSION)|g' \
	    -e 's|@LIBDIR@|$(LIBDIR)|g' \
	    -e 's|@INCLUDEDIR@|$(INCLUDEDIR)|g' \
	    gig-girepository.pc.in > $(DESTDIR)$(PKGCONFIGDIR)/gig-girepository.pc

# ============================================================================
# Uninstallation
# ============================================================================

uninstall:
	rm -f $(DESTDIR)$(LIBDIR)/libgig-core.so
	rm -f $(DESTDIR)$(GUILE_EXTENSIONDIR)/gi/libgig-girepository.so
	rm -rf $(DESTDIR)$(INCLUDEDIR)/gig
	rm -rf $(DESTDIR)$(GUILE_SITEDIR)/gi
	rm -rf $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)
	rm -f $(DESTDIR)$(PKGCONFIGDIR)/gig-girepository.pc

# ============================================================================
# Testing
# ============================================================================

check: all
	@echo "Running tests..."
	guile -L scheme -c "(use-modules (gi repository)) (display \"Basic load test: PASSED\n\")"
	@echo "Running example..."
	guile -L scheme examples/gi-example.scm

# Valgrind testing for memory leaks
check-valgrind: all
	@echo "Running Valgrind tests..."
	valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all \
		guile -L scheme -c "(use-modules (gi repository)) (require-namespace \"GLib\" \"2.0\")"

# Performance benchmarking
benchmark: all
	@echo "Running performance benchmarks..."
	time guile -L scheme -c "(use-modules (gi repository)) \
		(require-namespace \"GLib\" \"2.0\") \
		(preload-namespace \"GLib\") \
		(do ((i 0 (+ i 1))) ((= i 1000)) \
			(find-by-name \"GLib\" \"MainLoop\"))"

# ============================================================================
# Development
# ============================================================================

# Debug build
debug: CFLAGS += -DDEBUG -ggdb3 -O0 -fsanitize=address -fsanitize=undefined
debug: LDFLAGS += -fsanitize=address -fsanitize=undefined
debug: all

# Static analysis
static-analysis:
	@echo "Running static analysis..."
	cppcheck --enable=all --std=c11 $(SOURCES)
	scan-build $(MAKE) clean all

# Code formatting
format:
	@echo "Formatting code..."
	find . -name '*.c' -o -name '*.h' | xargs clang-format -i

# Generate tags for development
tags:
	ctags -R core gi

# ============================================================================
# Documentation
# ============================================================================

docs: docs/api.html docs/manual.html

docs/api.html: $(HEADERS)
	@echo "Generating API documentation..."
	doxygen Doxyfile

docs/manual.html: docs/manual.md
	@echo "Generating manual..."
	pandoc -s docs/manual.md -o docs/manual.html

# ============================================================================
# Packaging
# ============================================================================

dist: $(TARNAME).tar.gz

$(TARNAME).tar.gz: $(SOURCES) $(HEADERS) $(SCHEME_FILES) $(EXAMPLE_FILES) Makefile
	@echo "Creating distribution tarball..."
	mkdir -p $(TARNAME)
	cp -r core gi scheme examples docs $(TARNAME)/
	cp Makefile README.md CHANGELOG.md LICENSE $(TARNAME)/
	tar -czf $(TARNAME).tar.gz $(TARNAME)
	rm -rf $(TARNAME)

# RPM packaging
rpm: $(TARNAME).tar.gz
	rpmbuild -ta $(TARNAME).tar.gz

# Debian packaging
deb:
	dpkg-buildpackage -rfakeroot -uc -us

# ============================================================================
# Cleaning
# ============================================================================

clean:
	rm -f $(OBJECTS)
	rm -f libgig-core.so libgig-girepository.so
	rm -f core.h.gch gi.h.gch
	rm -f tags

distclean: clean
	rm -f $(TARNAME).tar.gz
	rm -rf $(TARNAME)
	rm -rf docs/html docs/latex
	rm -f docs/api.html docs/manual.html

maintainer-clean: distclean
	rm -f configure config.status config.log
	rm -rf autom4te.cache

# ============================================================================
# Special Targets
# ============================================================================

.PHONY: all install install-libs install-headers install-scheme install-examples \
        install-pkgconfig uninstall check check-valgrind benchmark debug \
        static-analysis format tags docs dist rpm deb clean distclean \
        maintainer-clean

# Dependency tracking
-include $(OBJECTS:.o=.d)

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -MM $(ALL_CFLAGS) -Icore -Igi $< > $@.$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$ > $@; \
	rm -f $@.$$

# ============================================================================
# Platform-specific configurations
# ============================================================================

# macOS specific settings
ifeq ($(shell uname -s),Darwin)
    LDFLAGS += -undefined dynamic_lookup
    CFLAGS += -Wno-deprecated-declarations
endif

# Windows/MinGW specific settings
ifeq ($(shell uname -o 2>/dev/null),Msys)
    LDFLAGS += -Wl,--export-all-symbols
    CFLAGS += -DGIR_DLL -DBUILDING_GIG
endif

# ============================================================================
# Help Target
# ============================================================================

help:
	@echo "Available targets:"
	@echo "  all              - Build all libraries"
	@echo "  debug            - Build with debug symbols and sanitizers"
	@echo "  install          - Install everything"
	@echo "  uninstall        - Remove installed files"
	@echo "  check            - Run basic tests"
	@echo "  check-valgrind   - Run tests with Valgrind"
	@echo "  benchmark        - Run performance benchmarks"
	@echo "  static-analysis  - Run static code analysis"
	@echo "  format           - Format source code"
	@echo "  tags             - Generate ctags"
	@echo "  docs             - Generate documentation"
	@echo "  dist             - Create distribution tarball"
	@echo "  rpm              - Create RPM package"
	@echo "  deb              - Create Debian package"
	@echo "  clean            - Remove build artifacts"
	@echo "  distclean        - Remove all generated files"
	@echo "  help             - Show this help"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  CC=$(CC)"
	@echo "  CFLAGS=$(CFLAGS)"

# ============================================================================
# Configuration Check
# ============================================================================

check-deps:
	@echo "Checking dependencies..."
	@$(PKG_CONFIG) --exists guile-3.0 || (echo "ERROR: guile-3.0 not found" && exit 1)
	@$(PKG_CONFIG) --exists gobject-introspection-1.0 || (echo "ERROR: gobject-introspection-1.0 not found" && exit 1)
	@$(PKG_CONFIG) --exists glib-2.0 || (echo "ERROR: glib-2.0 not found" && exit 1)
	@echo "All dependencies found!"
	@echo "  Guile: $(shell $(PKG_CONFIG) --modversion guile-3.0)"
	@echo "  GI: $(shell $(PKG_CONFIG) --modversion gobject-introspection-1.0)"
	@echo "  GLib: $(shell $(PKG_CONFIG) --modversion glib-2.0)"

# ============================================================================
# Install Dependencies (for development)
# ============================================================================

install-deps-debian:
	sudo apt-get update
	sudo apt-get install -y \
		build-essential \
		pkg-config \
		guile-3.0-dev \
		libgirepository1.0-dev \
		libglib2.0-dev \
		valgrind \
		cppcheck \
		clang \
		doxygen \
		pandoc

install-deps-fedora:
	sudo dnf install -y \
		gcc \
		make \
		pkg-config \
		guile30-devel \
		gobject-introspection-devel \
		glib2-devel \
		valgrind \
		cppcheck \
		clang \
		doxygen \
		pandoc

install-deps-macos:
	brew install \
		pkg-config \
		guile \
		gobject-introspection \
		glib \
		cppcheck \
		doxygen \
		pandoc

# ============================================================================
# Advanced Features
# ============================================================================

# Profile-guided optimization
pgo: clean
	$(MAKE) CFLAGS="$(CFLAGS) -fprofile-generate" all
	$(MAKE) check
	$(MAKE) clean
	$(MAKE) CFLAGS="$(CFLAGS) -fprofile-use" all

# Link-time optimization
lto:
	$(MAKE) CFLAGS="$(CFLAGS) -flto" LDFLAGS="$(LDFLAGS) -flto" all

# Generate coverage report
coverage: CFLAGS += --coverage
coverage: LDFLAGS += --coverage
coverage: all check
	gcov $(SOURCES)
	lcov --capture --directory . --output-file coverage.info
	genhtml coverage.info --output-directory coverage-html

# Cross-compilation support
CROSS_COMPILE ?=
ifneq ($(CROSS_COMPILE),)
    CC = $(CROSS_COMPILE)gcc
    PKG_CONFIG = $(CROSS_COMPILE)pkg-config
endif
