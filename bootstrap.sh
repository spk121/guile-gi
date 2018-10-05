#!/bin/sh
set -e

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.
cd "$srcdir"
mkdir -p m4 >/dev/null 2>&1 || true
# gtkdocize --copy --flavour notmpl || exit 1
autoreconf --verbose --force --install
cd -

