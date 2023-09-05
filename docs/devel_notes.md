# Guile Modules

## Constants
Define directly at parse time.

```Scheme
(define BINARY_AGE 1301)
```

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

Note the types from other modules need to be handled.  Also this
probably all needs to be lazy evaluation since we can't call
foreign-library-function until top-level library is loaded (Gtk4,
WebKit) since that one links to the version of GObject/GLib we
need to handle.

