# Guile Modules

# Constants
Define directly at parse time.

```Scheme
(define BINARY_AGE 1301)
```

## Enums and Bitfields

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


Or maybe just `nick->value` and then use the `c:identifier`
name as a top-level define.

```Scheme
(define GTK_ALIGN_BASELINE_FILL
  (nick->value Align 'baseline-fill))
```
