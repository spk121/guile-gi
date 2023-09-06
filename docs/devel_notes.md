# Guile Modules

# Aliases
These are typedefs, either to simple types, like guint8, or to _opaque_ types.
When they are opaque, the type name is `none`.

Since aliases don't have any typological meaning, let's just keep
an alist of alises and use the more fundamental type whenever the alias
appears, during parse time.

What to do with opaque type aliases? They could be pointers, if they're
used like pointers.

When using GIRepository, can you even get alias data? Or is that
just a GIR XML thing?

# Constants
Define directly at parse time assuming they are simple types: `gint`

```Scheme
(define BINARY_AGE 1301)
```
# Structs/Unions and Fields

Subclasses of `<GStruct>` may have a list of fields for use with
getters and setters.  When that it true,
a class derived from `<GStruct>` has a class slot containing
information required to get/set fields.  A `<GStruct>` subclass needs to
define its fieldinfo class slot at parse time, since it is not introspectable.
That information is used by the `get-field` and `set-field!` methods.

Remember not to add field info for private fields.

In GLib, there are unions with fields that are anonymous
structs.  Don't even try to support fields of anonymous structs

```Scheme
(describe-fields (instance <GStruct>))
(get-field (instance <GStruct>) (field-name <symbol>))
(set-field! (instance <GStruct>) (field-name <symbol>) value)
```



|               |                |            |
|-----------|-----------|---------|----------|
| utf8 | gchar* | writable |  | GArray.data | provides unsafe direct write access to pointed-to bytes |
| guint | guint | writable | | GArray.len | can modify this int |

# Arguments and Return Values

| array name | array c:type | type name | type c:type |
|--|--|--|--|
| return | GLib.Array | GArray* | gpointer | gpointer |
|return | gpointer | gpointer | | | pointer | 

## Special Types

`GArray`: all `GArray` methods are marked as non-introspectable.  The binding
is suppose to provide native ways to handle `GArray`



## Enums and Bitfields

Stick with the term `bitfields` over `flags` to match GTK online docs.

Despite my goal of only using the `name` as given in the typelib,
with enums and bitfields, it gets messy.

| XML            | GType Enum                | Untyped Enum      |   |
|----------------|---------------------------|-------------------|---|
| `name`         | `baseline_fill`           |                   |   |
| `glib:nick`    | `baseline-fill`           |                   |   |
| `glib:name`    | `GTK_ALIGN_BASELINE_FILL` |                   |   | 
| `name`         |                           | `unexp_eof`       |   |
| `c:identifier` |                           | `G_ERR_UNEXP_EOF` |   | 

GTK's online docs give `glib:name` or `c:identifier` type names.

So what to do? If I hold the line on only using `name`, it is out of
sync with the online docs.

I could use the `glib:name` or `c:identifier` as a define of an enum
instance.

```Scheme
(define GTK_ALIGN_BASELINE_FILL
  (nick->value Align 'baseline-fill))
```

For typed enums, at runtime we can use `enum_get_value_by_name` and `enum_get_value_by_nick`.

```Scheme
(name->value (class <GEnum>) (name <symbol>))
(nick->value (class <GEnum>) (nick <symbol>))
(value->name (class <GEnum>) (x <integer>))
(value->nick (class <GEnum>) (x <integer>))
```

## Functions, Methods, Constructors

Something like this
```Scheme
(define gtk_color_dialog_choose_rgba_finish
  (foreign-library-function "libgtk-4" "gtk_color_dialog_choose_rgba_finish"
                            #:return-type '*
                            #:arg-types (list '* '* '*)))

(define-method (choose_rgba_finish (self GtkColorDialog) (result GAsyncResult))
  (gi-func-marshall gtk_color_dialog_choose_rgba_finish self result
    #:return-type '((type . "Gdk.RGBA")
                    (nullable . #t)
                    (transfer . 'full))
    #:param-type '(((type . "GtkColorDialog")
                    (name . "self"))
                   ((type . 'Gio.AsyncResult")
                    (name . "result")))
    ;; Indicate that there is a GError at the end
    #:throws #t))
```

Note the types from other modules need to be handled are
are imported with a dot notation, so the `#:use-module`
will need a `#:prefix Gio.` etc.

Also this
probably all needs to be lazy evaluation since we can't call
foreign-library-function until top-level library is loaded (Gtk4,
WebKit) since that one links to the version of GObject/GLib we
need to handle.

## String Argument Conversions

String argument conversions are always a source of friction.

1. input, utf8 `gchar *` w/ length arg, transfer-ownership: none

Example: `ascii_dtostr`

Caller is providing a UTF-8 string buffer than can be modified by
callee.

2. output, utf8 `gchar *`, transfer-ownership: full

C


## Argument Conversion

*Input Conversions*

| GObject Type | Scheme Type                 | Notes |
|---------------|-----------------------------|--------|
| int8, uint8   | exact integer, Latin-1 char |         |
| int16, uint16 | exact integer              |         |
| int32, uint32 | exact integer              |         |
| int64, uint64 | exact integer              |         |
| float, double | real                       |         |
| gboolean      | #t, #f                     |         |
| gunichar      | exact integer, char        |         |
| utf8          | string                     | Note 6 |
| filename      | string                     | Note 6  |
| GArray        | GArray, lists, vectors     | Lists & vectors are converted to GArray. See Note 1 |
| GByteArray    | GByteArray, bytevectors    | Bytevectors are converted into GByteArray, See Note 2 |
| GPtrArray     | GPtrArray, vector of pointers | See Note 3 |
| GList, GSList | GList, GSList, lists       | Note 4 |
| GHash         | GHash, hashtable           | Note 5 |
| C arrays of char |  | |

*Note 1*: Lists and vectors are by default converted to GArray by copying contents.
Modifying the GArray won't modify the contents of the list or vector.
To share contents, when possible, there will be a procedure like `(array->GArray/shared)` or such.
Lists can not share contents.

*Note 2*: Vectors and bytevectors are converted into GByteArray by copying contents.
Modifying the GArray won't modify the contents of the list or vector.
To share contents, when possible, there will be a procedure like `(bytevector->GByteArray/shared)` or something.

*Note 4*: Lists are converted to GList or GSList by copying contents.
Modifying the GList or GSList won't modify the contents of the list.

*Note 5*: Hash tables are converted to GHash by copying contents.
There is no practical way to share contents.

*Note 6*: Guile strings and GLib strings do not share a common representation
and the contents of Guile strings may be read-only anyway.
The strings are converted by translating the contents from one representation
to another, so it is not possible to share contents. GObject/GLib functions that
modify string contents will be modifying their copy of C string.
Some functions exist to modify the contents of C strings.  To keep these
useful, 
we allow the GString type to be used in any context where plain C strings
are expected.

*Output Conversions*

| GObject Type | Scheme Type                 | Notes      |
|---------------|----------------------------|------------|
| int8, uint8   | exact integer              | Note 1     |
| int16, uint16 | exact integer              |            |
| int32, uint32 | exact integer              |            |
| int64, uint64 | exact integer              |            |
| float, double | real                       |            |
| gboolean      | #t, #f                     |            |
| gunichar      | char                       |            |
| utf8          | TBD                     | Note 3     |
| filename      | TBD                    | Note 3     |
| GArray        | GArray                     | Note 2     |
| GByteArray    | GByteArray                 | Note 2     |
| GPtrArray     | GPtrArray                  | Note 2     |
| GList, GSList | GList, GSList              | Note 2     |
| GHash         | GHash                      | Note 2     |

*Note 1*: For functions that return an 8-bit integer that is meant to represent
an 8-bit character, it is going to return the integer form.

*Note 2*: For functions that returns an array, list, or hash
the return is always a GObject/GLib type,
even if the array or list that was passed in was a Scheme list or vector.
The programmer will have to convert them back to a Scheme type, if desired.

*Note 3*: Returned strings are C arrays in either UTF-8 or locale
encoding.  They may be `const char *`, implying no ownership
transfer, or may be `char *`

### Output Args

In 0.3.0, guile-gi was clever enough to elide output arrguments
from a bound function's input args, and then add them as a multiple
return values.  To go with new philosopy of making Guile-GI
a worse Scheme binding so it looks more like GTK docs, we're going
back to requiring output arguments when calling
a function.

In 0.0.x, for a moment, we used SRFI-111 boxes for this.

For bound types, is probably better to
have the caller pass in a `<GFundamental>`, and then have the
function binding call `change-class` to upgrade the class instance
to the new class.  This works fine, so long as `<GFundamental>`
has `<redefinable-class>` as its metaclass.

```Scheme
(use-modules (oop goops) (oop goops describe) (system foreign))

(define-class <GFundamental> ()
  (value #:class <scm-slot>
         #:init-keyword #:value
         #:init-value %null-pointer)
  #:metaclass <redefinable-class>)
(define-class <GBravo> (<GFundamental>)
  bravo)
(define-method (set-bravo! (x <GFundamental>))
  (change-class x <GBravo>)
  (slot-set! x 'bravo 2))

(define x (make <GFundamental> #:value 1))
(describe x)
(set-bravo! x)
(describe x)
```

If output is a simple value, like an integer, you can still
use a `<GFundamental>`, or a SRFI-111 box is good, too.

(ugh)

### Length Args

In 0.3.0, guile-gi was clever enough to elide array length
arguments, instead calculating them from the length of the
array or bytevector used as input.  To go with the new plan
of being worse scheme experience but matching GTK docs better, length
arguments are again required. Passing in a non-number
will compute the array size from the array.

What non-number? Maybe `#t` or perhaps `_` or `:`.
