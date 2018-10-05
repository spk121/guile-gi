(define-module (gi gobject)
  #:export (
	    ;; from gi_gobject.c
	    gobject?
	    register-type
	    make-gobject
	    gobject-is-object?
	    gobject-type
	    gobject-type-name
	    gobject-is-floating?
	    gobject-set-property!
	    gobject-get-property
	    ;; from gi_gsignal.c
	    G_SIGNAL_RUN_FIRST
	    G_SIGNAL_RUN_LAST
	    G_SIGNAL_RUN_CLEANUP
	    G_SIGNAL_NO_RECURSE
	    G_SIGNAL_DETAILED
	    G_SIGNAL_ACTION
	    G_SIGNAL_NO_HOOKS
	    G_SIGNAL_MUST_COLLECT
	    G_SIGNAL_DEPRECATED
	    ;; from gi_gtype.c
	    gtype?
	    ->gtype
	    integer->gtype-unsafe
	    string->gtype
	    gtype-get-name
	    gtype-get-parent
	    gtype-get-fundamental
	    gtype-get-children
	    gtype-get-interfaces
	    gtype-get-depth
	    gtype-is-interface?
	    gtype-is-classed?
	    gtype-is-instantiatable?
	    gtype-is-derivable?
	    gtype-is-deep-derivable?
	    gtype-is-abstract?
	    gtype-is-value-abstract?
	    gtype-is-value-type?
	    gtype-have-value-table?
	    gtype-is-a?
	    gtype-get-bases
	    gtype-is-value?
	    gtype-is-param?
	    gtype=?
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
	    G_TYPE_PARAM
	    G_TYPE_BOXED
	    G_TYPE_OBJECT
	    G_TYPE_GTYPE
	    G_TYPE_VARIANT
	    G_TYPE_CHECKSUM
	    G_TYPE_POINTER
	    ;; from gi_gparamspec.c
	    G_PARAM_READABLE
	    G_PARAM_WRITABLE
	    G_PARAM_READWRITE
	    G_PARAM_CONSTRUCT
	    G_PARAM_CONSTRUCT_ONLY
	    ))

(load-extension "libguile-gi" "gi_init_gobject")
