(use-modules (gi typelib)
	     (gi girepository))

(irepository-require "GLib" "2.0" 0)

(define n (irepository-get-n-infos "GLib"))

;; If a type is a GObject, push up the type chain to make sure
;; the parents are defined.  If a parent is in another
;; namespace, assume the type is prefixed like this
;; Namespace.Name
;; GdkRGBA => Gdk.RGBA

;; For functions and methods, also chain up types.



;; At top-level, there should only be
;; classes
;; interfaces
;; structs
;; unions
;; enums
;; bitfields
;; error domains
;; functions
;; constants
;; callbacks - ignore
;; aliases - has not info
;; function-macros - skip


(define (do-registered-type-info info)
  

(define (do-class-info info)
  (do-class-info-type info)
  (do-class-info-consants info)
  (do-class-info-fields info)
  (do-class-info-interfaces info)
  (do-class-info-properties info)
  (do-class-info-signals))

(define (do-top-level-info info)
  (cond
   ((= (base-info-get-type info) INFO_TYPE_OBJECT)
    (do-top-level-class info))
   ((= (base-info-get-type-info) INFO_TYPE_INTERFACE)
    (do-top-level-interface info))
   ((= (base-info-get-type info) INFO_TYPE_STRUCT)
    (do-top-level-struct info))
   ((= (base-info-get-type info) INFO_TYPE_UNION)
    (do-top-level-union info))
   ((and (= (base-info-get-type info) INFO_TYPE_ENUM)
	 (not (enum-info-get-error-domain info)))
    (do-top-level-enumeration info))
   ((= (base-info-get-type info) INFO_TYPE_FLAGS)
    (do-top-level-bitfield info))
   ((and (= (base-info-get-type info) INFO_TYPE_ENUM)
	 (enum-info-get-error-domain info))
    (do-top-level-error-domain info))
   ((= (base-info-get-type) INFO_TYPE_FUNCTION)
    (do-top-level-function info))
   ((= (base-info-get-type info) INFO_TYPE_CONSTANT)
    (do-top-level-constant info))
   (else
    #f)))
   
(let loop ((i 0))
  (when (< i n)
    (let* ((info (irepository-get-info "GLib" i))
	   (infotype (base-info-get-type info))
	   (infostring
	    (if (and (= infotype INFO_TYPE_ENUM)
		     (enum-info-get-error-domain info))
		"error_domain"
		(info-type-to-string infotype))))
      
      (format #t "namespace ~S name ~S infotype ~S~%"
	      (base-info-get-namespace info)
	      (base-info-get-name info)
	      infostring)

      (cond
       ((= (base-info-get-type info) INFO_TYPE_STRUCT)
	(write
	 (list '^struct
	       (base-info-get-namespace info)
	       (base-info-get-name info)
	       (struct-info-get-size info)
	       (registered-type-info-get-type-name info)
	       (registered-type-info-get-type-init info)))
	(newline)
	(let ((n (struct-info-get-n-fields info)))
	  (let loop ((i 0))
	    (let ((field (struct-info-get-field info i)))
	      (when (and (logtest (field-info-get-flags field) FIELD_IS_READABLE)
			 (logtest (field-info-get-flags field) FIELD_IS_WRITABLE))
		(let ((typ (field-info-get-type field)))
		  (when (or (and (not (type-info-is-pointer typ))
				 (member (type-info-get-tag typ)
					 (list TYPE_TAG_BOOLEAN
					       TYPE_TAG_INT8
					       TYPE_TAG_UINT8
					       TYPE_TAG_INT16
					       TYPE_TAG_UINT16
					       TYPE_TAG_INT32
					       TYPE_TAG_UINT32
					       TYPE_TAG_INT64
					       TYPE_TAG_UINT64
					       TYPE_TAG_FLOAT
					       TYPE_TAG_DOUBLE)))
			    (and (type-info-is-pointer typ)
				 (= (type-info-get-tag typ) TYPE_TAG_UTF8)))

		    (write
		     (list '^field
			   (base-info-get-namespace info)
			   (base-info-get-name info)
			   (base-info-get-name field)
			   (field-info-get-size field)
			   (field-info-get-offset field)))
		    (newline)))))
	    (if (< i n)
		(loop (1+ i))))))
       (else
	#f))
	       
      ;; check if enum, interface, object, struct, union has gtype
      (when (is-registered-type-info info)
	(if (registered-type-info-has-g-type info)
	    (format #t "typed ~A of registered-type ~S~%"
		    (info-type-to-string (base-info-get-type info))
		    (registered-type-info-get-type-name info))
	    (format #t "untyped ~A~%"
		    (info-type-to-string (base-info-get-type info)))))
      (flush-all-ports)
      (when (= (base-info-get-type info) INFO_TYPE_STRUCT)
	(if (struct-info-is-gtype-struct info)
	    (format #t "ignoring because is gtype struct\n"))
	(if (struct-info-is-foreign info)
	    (format #t "struct is foreign~%"))
	(format #t "struct has alignment ~a, size ~s, n_fields ~a, n_methods ~a~%"
		(struct-info-get-alignment info)
		(struct-info-get-size info)
		(struct-info-get-n-fields info)
		(struct-info-get-n-methods info))
	(let loop ((i 0))
	  (if (< i (struct-info-get-n-fields info))
	      (let ((meth (struct-info-get-field info i)))
		(format #t "  :~a~%" (base-info-get-name meth))
		(loop (1+ i)))))
	(let loop ((i 0))
	  (if (< i (struct-info-get-n-methods info))
	      (let ((meth (struct-info-get-method info i)))
		(format #t "  ~A~A~a~%"
			(if (logtest FUNCTION_IS_CONSTRUCTOR (function-info-get-flags meth))
			    "$" "")
			(if (logtest FUNCTION_IS_METHOD (function-info-get-flags meth))
			    "." "")
			(base-info-get-name meth))
		(loop (1+ i)))))))
      
	
      (loop (1+ i))))

#|
Top-level

classes
interfaces
structs
unions
enums
bitfields
error domains
functions
constants

IL is based on what information is available with GObject available
but libgirepository unavailable.

^enuj, ^bitfield, ^error-domain .  If it has a GType, you can
get values at runtime from GObject.

^untyped-enum etc.  If it has no GType, you need to get the values
at parse type.

^struct, ^union, ^object, ^interface ^boxed have a gtype and can
get some info at runtime (like what?)

^untyped-struct ^untyped-union ^untyped-box have no gtype and need to have
all info (what info?) at parse type

struct fields are punted to Taylan bytestructures

^function includes top-level function, constructor, and type function
and method.  They differ at parse time by how the names are constructed,
but they are all GOOPS methods. Could split into ^function, ^constructor,
^method just to be fancy.

^signal and ^function currently differ in the handling of output argument.
Moving forward with plan to align with Gtk docs, give up on fancy handling
of inout and out params and just set! them instead.

If an inout or out param is not writable, just skip.

New GError handling: set! output param to a pair (num . string)

^properties are handled like methods: GOOPS methods that
provide accessors to objects. They use the ':' prefix

^fields are like properties: GOOPS methods that provide
accessors to struct/union.  They use the '.' prefix.

INOUT args and writability
Not all types can be set!. It depends if the arg is passing by value or
by reference, in C terms.
strings, vectors, and bytevectors will be set!
other types will be added to the output values.

OUT args
you can pass a srfi-111 <box>, it will be set!
If #f they'll be added to output values.

ARRAY SIZES
Some functions have you pass in an array with a size parameter.  If
you provide that size parameter as an integer, it will try to use that
size.  If you pass in #f or -1, it will compute it from the array.


#|
(define info (irepository-get-info "GLib" 0))

(write (info->IL-constant info))
(newline)

(set! info (irepository-get-info "GLib" 1))
(format #t "namespace ~S name ~S~%" (base-info-get-namespace info)
	(base-info-get-name info))

(write (info->IL-constant info))
(newline)

(let loop ((i 2))
  (set! info (irepository-get-info "GLib" i))
  (format #t "namespace ~S name ~S~%" (base-info-get-namespace info)
	  (base-info-get-name info))
  (loop (1+ i)))


|#
