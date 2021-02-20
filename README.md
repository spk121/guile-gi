# Guile GI

[![Build Status](https://travis-ci.com/spk121/guile-gi.svg?branch=master)](https://travis-ci.com/spk121/guile-gi)

This is a library for [GNU Guile](https://gnu.org/software/guile) to
create language bindings via [GObject Introspection](https://gi.readthedocs.io).

GNU Guile is an implementation of Scheme, which is a Lisp-like language.
This library allows Guile to use GObject-based libraries -- such as GTK+3,
GLib, and WebKit2 -- by generating a Scheme API from the GObject Introspection
information provided by those libraries.

This is beta code.  It is only partially documented.  The API is
stabilizing, but may still be subject to change.

Guile GI has two primary components.

* The `gi` scheme modules: guile modules that provide functionality to
  dynamically generate Scheme API from GObject typelib files

* `libguile-gi.so` or `libguile-gi.dll`: a compiled module that
  contains glue code to interface with GObject

To create bindings, use the `use-typelibs` syntax found in the `(gi)`
library.

To build `guile-gi`, one needs a standard build environment plus the
development files for GObject Introspection and for Guile.  If
building straight from a clone of the repository and not from an
official release, Texinfo and its many dependencies are required to
build the documentation.  To run the full test suite, one also needs
the Gtk and its many dependencies installed, including Cairo.

For the moment, the docs are at
[spk121.github.io/guile-gi](https://spk121.github.io/guile-gi/)

If you're using GNOME on Guix, try:

    $ guix environment --ad-hoc -l guix.scm guile webkitgtk -- guile examples/browser.scm

If you're using another desktop environment or prefer pure environments, try:

    $ guix environment [--pure -E XAUTHORITY -E 'XDG_.*'] --ad-hoc -l guix.scm guile gtk+ webkitgtk -- guile examples/browser.scm

Alternatively, to use `guix environment` for developing:

    $ guix environment -l guix.scm [--ad-hoc gtk+ webkitgtk]
    $ ./bootstrap && ./configure && make
    $ tools/uninstalled-env tools/run-guile examples/browser.scm

For other examples on how to use guile-gi, check out the examples directory.
