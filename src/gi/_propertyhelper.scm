(define-module (gi _propertyhelper)
  #:use-module (gi _none)
  #:use-module (gi _constants)
  #:use-module (ice-9 optargs))

(define _type_from_class_lookup
  '((<integer> . TYPE_LONG)
    (<real> . TYPE_DOUBLE)
    (<object> . TYPE_OBJECT)
    (<string> . TYPE_STRING)))

(define _min_value_lookup
  '((TYPE_UINT . 0)
    (TYPE_ULONG . 0)
    (TYPE_UINT64 . 0)
    (TYPE_FLOAT . -G_MAXFLOAT)
    (TYPE_DOUBLE . -G_MAXDOUBLE)
    (TYPE_INT . G_MININT)
    (TYPE_LONG . G_MINLONG)
    (TYPE_INT64 . (- (expt 2 63)))))

(define _max_falue_lookup
  '((TYPE_UINT . G_MAXUINT)
    (TYPE_ULONG . G_MAXULONG)
    (TYPE_INT64 . (1- (expt 2 63)))
    (TYPE_UINT64 . (1- (expt 2 64)))
    (TYPE_FLOAT . G_MAXFLOAT)
    (TYPE_DOUBLE . G_MAXDOUBLE)
    (TYPE_INT . G_MAXINT)
    (TYPE_LONG . G_MAXLONG)))

(define _default_lookup
  '((TYPE_INT . 0)
    (TYPE_UINT . 0)
    (TYPE_LONG . 0)
    (TYPE_ULONG . 0)
    (TYPE_INT64 . 0)
    (TYPE_UINT64 . 0)
    (TYPE_STRING . "")
    (TYPE_FLOAT . 0.0)
    (TYPE_DOUBLE . 0.0)))

(define-class <Property> (<GObject>)
  (getter #:getter get-getter #:init-value $NONE #:init-keyword #:getter)
  (setter #:getter get-setter #:init-value $NONE #:init-keyword #:setter)
  (type #:getter get-type #:init-value $NONE #:init-keyword #:type)
  (default #:getter get-default #:init-value $NONE #:init-keyword #:default)
  (nick #:getter get-nick #:init-value "" #:init-keyword #:nick)
  (blurb #:getter get-blurb #:init-value "" #:init-keyword #:name)
  (flags #:getter get-flags #:init-value PARAM_READWRITE #:init-keyword #:flags)
  (minimum #:getter get-minimum #:init-value $NONE #:init-keyword #:minimum)
  (maximum #:getter get-maximum #:init-value $NONE #:init-keyword #:maximum)
  (name #:getter get-name #:init-value $NONE #:init-keyword #:name)
  (__doc__ #:init-keyword #:__doc__)
  (fset #:init-keyword #:fset)
  (_exc #:init-keyword #:_exc))

(define (_type_from_guile type_)
  (cond
   ((assoc type_ _type_from_class_lookup)
    (assoc-ref _type_from_class_lookup type_))
   ((and (class? type_)
	 (or-map (lambda (parent)
		   (class-descends-from? type_ parent))
		 <GObject>
		 <GEnum>
		 <GFlags>
		 <GBoxed>
		 <GInterface>))
    (slot-ref type '__gtype__))
   ((member type '(TYPE_NONE TYPE_INTERFACE TYPE_CHAR TYPE_UCHAR
			     TYPE_INT TYPE_UINT TYPE_BOOLEAN TYPE_LONG
			     TYPE_ULONG TYPE_INT64 TYPE_UINT64
			     TYPE_FLOAT TYPE_DOUBLE TYPE_POINTER
			     TYPE_BOXED TYPE_PARAM TYPE_OBJECT TYPE_STRING
			     TYPE_GOBJECT TYPE_GTYPE TYPE_STRV TYPE_VARIANT))
    type_)
   (else
    (scm-error 'wrong-type-arg "_type_from_guile" "Unsupported type: ~S" (list type_) #f))))

(define (_get_default type default)
  (cond
   ((not (none? default))
    default)
   (else
    (let ((type-default (assoc type  _default_lookup)))
      (if type-default
	  (cdr type-default)
	  ;; else
	  $NONE)))))

(define (_check_default type default)
  (let ((ptype type))
    (cond
     ((and (eqv? ptype TYPE_BOOLEAN)
	   (or (eq? default #t) (eq? default #f)))
      (scm-error 'wrong-type-arg "_check_default"
		 "default must be #t or #f, not ~S" (list default) #f))
     ((and (eqv? ptype TYPE_OBJECT) (not (none? default)))
      (scm-error 'wrong-type-arg "_check_default"
		 "object types do not have default values" '() #f))
     ((and (eqv? ptype TYPE_GTYPE) (not (none? default)))
      (scm-error 'wrong-type-arg "_check_default"
		 "GType types do not have default values" '() #f))
     ((and (eqv? ptype TYPE_ENUM) (none? default))
      (scm-error 'wrong-type-arg "_check_default"
		 "enum properties need a default value" '() #f))
     ((and (eqv? ptype TYPE_ENUM) (not (is-a? (GType default) ptype)))
      (scm-error 'wrong-type-arg "_check_default"
		 "enum value ~A must be an instance of ~A" (list default ptype) #f))
     ((and (eqv? ptype TYPE_FLAGS) (not (is-a? (GType default) ptype)))
      (scm-error 'wrong-type-args "_check_default"
		 "flags value ~A must be an instance of ~A" (list default ptype)))
     ((and (eqv? ptype TYPE_STRV) (not (none? default)))
      (if (not (list? default))
	  (scm-error 'wrong-type-args "_check_default"
		     "Strv value ~A must be a list" (list default) #f)
	  ;; else
	  (if (or-map (lambda (x)
			(not (string? x)))
		      default)
	      (scm-error 'wrong-type-args "_check_default"
			 "Strv value ~S must contain only strings" (list default) #f))))
     ((and (eqv? type TYPE_VARIANT) (not (none? default)))
      (if (or (not (hasattr? default '__gtype__))
	      (not (is-a? (GType default) TYPE_VARIANT)))
	  (scm-error 'wrong-type-args "_check_default"
		     "variant value ~S must be an instance of ~S" (list default ptype) #f)))

(define* (make-Property name #:key (getter $NONE) (setter $NONE) (type $NONE) (default $NONE)
			(nick "") (blurb "") (flags PARAM_READWRITE) (minimum $NONE)
			(maximum $NONE))
  (let ((__doc__ $NONE)
	(fset $NONE)
	(_exc $NONE))
    (when (none? type)
      (set! type <GObject>))
    (set! type (_type_from_python type))
    (set! default (_get_default type default))
    (_check_default gtype default)
    (unless (string? nick)
      (scm-error 'wrong-type-error "make-Property"
		 "nick must be a string: ~S" (list nick) #f))
    (unless (string? blurb)
      (scm-error 'wrong-type-error "make-Property"
		 "blurb must be a string"))
    
    (if (none? minimum)
	(set! minimum (_get_minimum type))
	;; else 
	(if (< minimum (_get_minimum type))
	    (scm-error 'out-of-range "make-Property"
		       "Minimum for type ~S cannot be lower than ~S"
		       (list minimum (_get_minimum type)))))
    
    (if (none? maximum)
	(set! maximum (_get_maximum type))
	;; else
	(if (> maximum (_get_maximum type))
	    (scm-error 'out-of-range "make_Property"
		       "Maximum for type ~S cannot be greater than ~S"
		       (list maximum (_get_maximum type)))))
    
    (set! __doc__ blurb)
    (cond
     ((and (not (none? getter))
	   (none? setter))
      (set! setter _readonly_setter))
     ((and (not (none? setter))
	   (none? getter))
      (set! getter _writeonly_getter))
     ((and (none? setter) (none? getter))
      (set! getter _default_getter)
      (set! setter _default_getter)))
    
    (set! fset setter)
    (set! _exc $NONE)
    (make <Property> #:name name #:getter getter #:setter setter #:type type
	  #:default default #:nick nick #:blurb blurb #:flags flags #:minimum minimum
	  #:maximum maximum #:__doc__ __doc__ #:fset fset #:_exc _exc)))

(define-method (__repr__ (property <Property>))
  (let ((name (get-name property)))
    (format #f "<GObject Property ~S (~S)>"
	    (if (none? name) "(unitialized)" name)
	    (type->name (get-type property)))))

(define-method (__get__ (property <Property>) instance klass)
  "MLG- Appears to call a generic getter and maybe raise an error on failure?"
  (cond
   ((none? instance)
    property)
   (else
    (set-_exc! property $NONE)
    (let ((value ((get-fget property) instance)))
      (unless (none? (get-_exc property))
	(let ((exc (get-_exc property)))
	  (set-_exc! $NONE)
	  (raise exc)))
      value))))

(define-method (__set__ (property <Property>) instance value)
  (cond
   ((none? instance)
    (scm-error 'wrong-type-error "__set__"
	       "the instance '~S' cannot be NONE"
	       (list instance) #f))
   (else
    (set-_exc! property $NONE)
    ;; MLG - this is meta.  Look in the instance for a set_property
    ;; procedure and then set a property there using this property's
    ;; name?
    ((get-set_property instance)
     property
     (get-name property)
     value)
    (unless (none? (get-_exc property))
      (let ((exc (get-_exc property)))
	(set-_exc! property $NONE)
	(raise exc))))))

(define-method (__call__ (property <Property>) fget)
  "MLG - 'apply the getter along with init arguments'?"
  ((get-getter property) fget))

(define-method (getter (property <Property>) fget)
  "Set the getter function to FGET."
  (when (none? (get-name property))
    (set-name! property (procedure-name fget)))
  (set-getter! property fget)
  property)

(define-method (setter (property <Property>) fget)
  (set-setter! property fget)
  property)

(define-method (_default_setter (property <Property>) instance value)
(define-method (add-property (obj <GObject>) (property <Property>))
  (
	  
