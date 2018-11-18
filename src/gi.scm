(define-module (gi)
  #:use-module (ice-9 eval-string)
  #:export (
	    ;;
	    get-typelib-search-path
	    prepend-typelib-search-path
	    load-typelib
	    import-typelib
	    export-typelib
	    gi-constant-value
	    gi-flag-value
	    gi-enum-value
	    gi-struct-ref
	    gi-struct-set
	    gi-function-invoke
	    gi-method-prepare
	    gi-method-send
	    gi-lookup-type
	    gi-lookup-callback-info

	    <GObject>
	    <GBox>
	    gbox-printer
	    gbox-set-finalizer!

	    ;; gi_gtype.c
	    ->gtype
	    integer->gtype-unsafe
	    string->gtype
	    gtype-get-name
	    gtype-get-parent
	    gtype-get-fundamental
	    gtype-get-childnre
	    gtype-get-interfaces
	    gtype-get-depth
	    gtype-is-interface?
	    gtype-is-classed?
	    gtype-is-instantiatable?
	    gtype-is-derivable?
	    gtype-is-deep-derivable?
	    gtype-is-abstract?
	    gtype-is-value-abstract?
	    gtype-has-value-table?
	    gtype-is-a-?
	    gtype-get-bases
	    gtype-is-value?
	    gtype-is-param?
	    gtype=?
	    %gtypes
	    G_TYPE_NONE
	    G_TYPE_INTERFACE
	    G_TYPE_CHAR
	    G_TYPE_UCHAR
	    G_TYPE_BOOLEAN
	    G_TYPE_INT
	    G_TYPE_UINT
	    G_TYPE_LONG
	    G_TYPE_ULONG
	    G_TYPE_INT64
	    G_TYPE_UINT64
	    G_TYPE_ENUM
	    G_TYPE_FLAGS
	    G_TYPE_FLOAT
	    G_TYPE_DOUBLE
	    G_TYPE_STRING
	    G_TYPE_BOXED
	    G_TYPE_PARAM
	    G_TYPE_OBJECT
	    G_TYPE_GTYPE
	    G_TYPE_VARIANT
	    G_TYPE_CHECKSUM
	    G_TYPE_POINTER
	    ))

(define (import-typelib namespace version)
  (load-typelib namespace version)
  (eval-string (%import-typelib namespace version)))

(load-extension "libguile-gi" "gir_init")

(when (defined? 'gcov-reset)
  (export gcov-reset))
(when (defined? 'gcov-dump)
  (export gcov-dump))
