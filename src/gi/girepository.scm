(define-module (girepository)
  #:export (
	    callback-info?
	    const-info?
	    constant-info?
	    enum-info?
	    field-info?
	    function-info?
	    interface-info?
	    object-info?
	    property-info?
	    signal-info?
	    struct-info?
	    union-info?
	    value-info?
	    vfunc-info?

	    ;; BASE
	    arg-info-get-name
	    callback-info-get-name
	    constant-info-get-name
	    enum-info-get-name
	    field-info-get-name
	    function-info-get-name
	    interface-info-get-name
	    object-info-get-name
	    property-info-get-name
	    signal-info-get-name
	    struct-info-get-name
	    type-info-get-name
	    union-info-get-name
	    vfunc-info-get-name

	    ;; ARG
	    GI_DIRECTION_IN
	    GI_DIRECTION_OUT
	    GI_DIRECTION_INOUT
	    
	    ;; CALLABLE
	    callback-info-may-return-null?
	    function-info-may-return-null?
	    vfunc-info-may-return-null?
	    callback-info-get-return-type
	    function-info-get-return-type
	    vfunc-info-get-return-type
	    callback-info-get-args
	    function-info-get-args
	    vfunc-info-get-args
	    
	    ;; CONSTANT
	    constant-info-get-type
	    constant-info-get-value

	    ;; ENUM
	    enum-info-get-values
	    
	    ;; FUNCTION
	    function-info-is-deprecated?
	    function-info-is-method?
	    function-info-is-constructor?
	    function-info-is-getter?
	    function-info-is-setter?
	    function-info-wraps-vfunc?
	    function-info-throws?
	    function-info-get-property
	    function-info-get-symbol
	    
	    %function-info-get-flags
	    irepository-get-n-infos
	    irepository-get-infos
	    irepository-require

	    ;; REGISTERED TYPE
	    enum-info-get-g-type
	    interface-info-get-g-type
	    object-info-get-g-type
	    struct-info-get-g-type
	    union-info-get-g-type
	    enum-info-get-type-name
	    interface-info-get-type-name
	    object-info-get-type-name
	    struct-info-get-type-name
	    union-info-get-type-name
	    
	    ;; STRUCT
	    struct-info-get-alignment
	    struct-info-get-size
	    struct-info-is-gtype-struct?
	    struct-info-get-fields
	    struct-info-get-methods
	    ))

(defmacro function-info-is (__TYPE __FLAG)
  `(define (,(string->symbol (string-append "function-info-is-" __TYPE "?")) arg)
     (logtest
      (%function-info-get-flags arg)
      ,__FLAG )))

(function-info-is "method" GI_FUNCTION_IS_METHOD)
(function-info-is "constructor" GI_FUNCTION_IS_CONSTRUCTOR)
(function-info-is "getter" GI_FUNCTION_IS_GETTER)
(function-info-is "setter" GI_FUNCTION_IS_SETTER)

(define (function-info-wraps-vfunc? fi)
  "Returns #t if the function info wraps a virtual function"
  (logtest
   (%function-info-get-flags fi)
   GI_FUNCTION_WRAPS_VFUNC))

(define (function-info-throws? fi)
  "Returns #t if the function info can throw an error"
  (logtest
   (%function-info-get-flags fi)
   GI_FUNCTION_THROWS))

(define (irepository-get-n-infos namespace)
  "Given a string containing a GObject namespace, such as
'GLib' or 'Gtk', return a count of the number of interfaces
in that namespace.  The namespace must first have been loaded
with 'irepository-require'"
  (%irepository-get-n-infos namespace))

(define (irepository-get-infos namespace)
  "Given a string containing a GObject namespace, such as 'GLib' or
'Gtk', return a list if interface objects for that namespace.  The
namespace must first have been loaded with 'irepository-require'"
  (%irepository-get-infos namespace))

(load-extension "libguile-girepository.so" "gir_init")
