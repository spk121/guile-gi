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

## Requirements

To build `guile-gi`, one needs a fairly standard build environment,
plus the development files for GObject Introspection and Guile.
On Debian-based systems, this translates to

    build-essential libgirepository1.0-dev guile-3.0-dev

It is also possible to build Guile-GI for Guile 2.2, in which
`guile-3.0-dev` is to be replaced by `guile-2.2-dev`.

When building from a git clone rather than a release tarball, Texinfo
is required as well in order to build documentation.

Users of GNU Guix can set up development environments through the provided `guix.scm`.

## Running from source

To run one of the examples or your own code with Guile-GI built directly from source,
use `tools/uninstalled-env`.  For example

    $ tools/uninstalled-env tools/run-guile examples/browser.scm

## Testing

In order to test Guile-GI itself, only the requirements above are necessary.
Some tests further rely on the GObject Introspection files of certain libraries.
To run the full suite, you need
- GLib, GObject, Gio
- Marshall, Everything (should come with gobject-instrospection)
- Gtk
- Grl (grilo)

Users of GNU Guix will have to pass the `--with-gir-hacks` option to `./configure`;
it will cause Guile-GI to build typelibs for the versions of its dependencies, that are
actually found in the build environment.

## Documentation

Further documentation is located at
[spk121.github.io/guile-gi](https://spk121.github.io/guile-gi/).
