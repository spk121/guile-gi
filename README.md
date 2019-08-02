# Guile GI

[![Build Status](https://travis-ci.com/spk121/guile-gi.svg?branch=master)](https://travis-ci.com/spk121/guile-gi)

This is a library for [GNU Guile](https://gnu.org/software/guile) to
create language bindings via [GObject Introspection](https://gi.readthedocs.io).

GNU Guile is an implementation of Scheme, which is a Lisp-like language.
This library allows Guile to use GObject-based libraries -- such as GTK+3,
GLib, and WebKit2 -- by generating a Scheme API from the GObject Introspection
information provided by those libraries.

This is alpha code.  It is barely documented.  The API may not be stable.

Guile GI has two primary components.

* The `gi` scheme modules: guile modules that provide functionality to
  dynamically generate Scheme API from GObject typelib files

* `libguile-gi.so` or `libguile-gi.dll`: a compiled module that
  contains glue code to interface with GObject

To create bindings, use the `use-typelibs` syntax found in the `(gi)`
library.

For the moment, the docs are at
[spk121.github.io/guile-gi](https://spk121.github.io/guile-gi/)

Try:

    guix environment --ad-hoc -l guix.scm guile
    guile-gi examples/browser.scm

Or, create and run in a development environment

    guix environment -l guix.scm
    ./bootstrap && ./configure && make
    tools/uninstalled-env tools/guile-gi examples/browser.scm
