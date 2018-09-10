ACLOCAL_AMFLAGS = --install -I m4 ${ACLOCAL_FLAGS}

EXTRA_DIST = \
 COPYING \
 bootstrap.sh

SUBDIRS = \
 src \
 test \
 doc \
 m4 \
 build-aux

DISTCHECK_CONFIGURE_FLAGS = --enable-introspection

# Build ChangeLog from GIT  history
ChangeLog:
	$(AM_V_GEN) if test -d $(top_srcdir)/.git; then \
		GIT_DIR="$(top_srcdir)/.git" git log --stat > $@; \
	fi

dist: ChangeLog

.PHONY: ChangeLog

-include $(top_srcdir)/git.mk

