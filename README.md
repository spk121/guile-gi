# Guile GI

This is a library for GNU Guile.  GNU Guile is an implementation of Scheme,
which is a Lisp-like language.

This library allows Guile to use GObject-based libraries, such as GTK+3.  GObject libraries are shared
object libraries that have been written in a standardized way to make them easy to use
from other languages.  GObject libraries come with metadata that describes the
functions and procedures in the library.

Guile GI has two primary components
- (gi): which makes GObject-based libraries available from Guile
- (gobject): which allows one to write and manipulate GObject-based types in Guile

Guile GI is heavily based on PyGObject, and most of the code is a transliteration
of the code in PyGObject.

For the moment, the docs are at https://spk121.github.io/
