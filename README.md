# Guile GI

This is a library for [GNU Guile](https://gnu.org/software/guile) to
create language bindings via [GObject Introspection](https://gi.readthedocs.io).

This is pre-alpha code.  It barely works.  The API is in flux.

Guile GI has two primary components.

* `(gi)` aka `gi.scm`: a guile module that provides functions to parse
  GObject typelib files

* `libguile-gi.so` or `libguile-gi.dll`: a compiled module that
  contains glue code to interface with GObject

To create bindings and import bindings, use the `use-typelibs` syntax
or the `typelib-load` procedure. The former is preferred, as it does not
affect the exports of the current module.

For the moment, the docs are at
[spk121.github.io/guile-gi](https://spk121.github.io/guile-gi/)

Try:

    guix environment --ad-hoc -l guix.scm guile
    guile-gi examples/browser.scm

Or, create and run in a development environment

    guix environment -l guix.scm
    ./bootstrap && ./configure && make
    tools/uninstalled-env tools/guile-gi test/browser.scm
    tools/uninstalled-env tools/guile-gi test/editor.scm
