(define-module (gi object)
  #:use-module (gi _lib)
  #:use-module (gi _gobject)
  #:export (
	    make-gobject-class
	    ))

(define $type-class-hash (make-hash-table))

(define (append-<GObject>-if-missing list-of-classes)
  "If none of LIST-OF-CLASSES descends from <GObject>,
return LIST-OF-CLASSES with <GObject> appended"
  (if (list-of-classes-descends-from? list-of-classes <GObject>)
		     list-of-classes
		     ;; else
		     (append list-of-classes (list <GObject>))))

(define (slot-spec->name slot-spec)
  (match slot-spec
    (((? symbol? name) . args) name)
    ((? slot? slot) (%slot-definition-name slot))))

(define (slot-specification->parameter-specification spec)
  (unless (or (symbol? spec) (list? spec))
    (scm-error 'wrong-type-arg "slot-specification->list-specification"
	       "Not a symbol or list: ~S" (list spec) #f))
  (if (symbol? spec)
      #f
      ;; else
      (let* ((name (car spec))
	     (kwd (cdr spec))
	     (gtype (list-get-keyword #:gtype kwd #f))
	     (nick (list-get-keyword #:nick kwd (string-title-case (symbol->string name))))
	     (blurb (list-get-keyword #:blurb kwd (string-title-case (symbol->string name))))
	     (minimum (list-get-keyword #:minimum kwd #f))
	     (maximum (list-get-keyword #:maximum kwd #f))
	     (default (list-get-keyword #:init-value kwd #f))
	     (flags (list-get-keyword #:flags kwd 0)))
	(cond
	 ((eq? gtype G_TYPE_BOOLEAN)
	  (unless default (set! default #f))
	  (list gtype name nick blurb default flags))
	 ((eq? gtype G_TYPE_CHAR)
	  (unless default (set! default 0))
	  (unless minimum (set! minimum -128))
	  (unless maximum (set! maximum 127))
	  (list gtype name nick blurb minimum maximum default flags))
	 ((eq? gtype G_TYPE_UCHAR)
	  (unless default (set! default 0))
	  (unless minimum (set! minimum 0))
	  (unless maximum (set! maximum 255))
	  (list gtype name nick blurb minimum maximum default flags))
	 ((or (and (eq? gtype G_TYPE_INT) (sizeof
	       (eq? gtype G_TYPE_LONG)
	      (eq? gtype G_TYPE_INT64))
	  (unless default (set! default 0))
	  (unless minimum (set! minimum -9223372036854775808))
	  (unless maximum (set! maximum 9223372036854775807))
	  (list gtype name nick blurb minimum maximum defaults flags))
	 ((eq? gtype G_TYPE_UINT)
	  (eq? gtype G_TYPE_LONG)
	  (eq? gtype 
	  (unless default (set! default 0))
	  (unless minimum (set! minimum 0))
	  (unless maximum (set! maximum 18446744073709551615))
	  (list gtype name nick blurb minimum maximum defaults flags))
	 ((eq? gtype G_TYPE_LONG)
	  (unless default (set! default 0))
	  (unless minimum (set! minimum -9223372036854775808))
	  (unless maximum (set! maximum 9223372036854775807))
	  (list gtype name nick blurb minimum maximum defaults flags))
	 ((eq? gtype G_TYPE_UINT)
	  (unless default (set! default 0))
	  (unless minimum (set! minimum 0))
	  (unless maximum (set! maximum 18446744073709551615))
	  (list gtype name nick blurb minimum maximum defaults flags))
	 ((eq? gtype
	 
  (cond
   ((symbol? spec)
    ;; A standard slot
    #f)
   
   ((not (list-get-keyword #:gtype (cdr spec) #f))
    ;; A standard slot.
    #f)
   (else
    ;; A parameter
    (let ((gtype (list-get-keyword #:gtype spec #f)))
      

(define (make-gobject-class name
			    supers
			    slots
			    signals)
  "Create a new GOOPS class with GObject functionality.  NAME is a
symbol that names the class.  SUPERS are the superclasses of this
class.  In the SUPERS list, then one (and only one) superclass must be
a GObject-based class.  The rest must be non-GObject based classes.
SLOTS are a list of GOOPS slots, but, if the slots have the special
#:gtype key, the slots will also be GObject properties for this class.
SIGNALS is a list of events to which callback procedures can be
attached.

On top of the regular slot options, if a slot has a #:gtype, it will be a
GObject property for this, in which case, there are additionally
#:type	  - a GType of a GObject-class that maps to a GType
#:flags   - GObject.ParamFlags property configuration flags
#:minimum - minimum value, depends on type
#:maximum - maximum value, depends on type

The signals options are
#:flags   - GObject.Signal flags  (default RUN_FIRST)
#:return-type - a GType or a GObject-class that maps to a GType  (default the NONE type)
#:arg-types - A list of GTypes, or '() or #f if the signal takes no arguments
(#:accumulator)
(#:accu_data)
"
  (unless (symbol? name)
    (scm-error 'wrong-type-arg "make-gobject-class"
	       "Not a symbol: ~S" (list name) #f))
  (unless (list? supers)
    (scm-error 'wrong-type-arg "make-gobject-class"
	       "Not a list: ~S" (list supers) #f))
  (unless (list? slots)
    (scm-error 'wrong-type-arg "make-gobject-class"
	       "Not a list: ~S" (list s) #f))
  (unless (list? signals)
    (scm-error 'wrong-type-arg "make-gobject-class"
	       "Not a list: ~S" (list supers) #f))
  
  (let* ((supers (append-<GObject>-if-missing supers))
	 (metaclass (ensure-metaclass supers)))

    ;; Verify that all direct slots and properties are different and that we
    ;; don't inherit several times from the same class.
    (let ((test1 (list-find-duplicate supers))
	  (test2 (list-find-duplicate (map slot-spec->name slots))))
      (when test1
	(goops-error "make-gobject-class: super class ~S is duplicate in class ~S"
		     test1 name))
      (when test2
	(goops-error "make-gobject-class: slot or property ~S is duplicate in class ~S"
		     test2 name)))

    (let* ((parameters (slots->parameters slots))
	   (parent (find-gobject-class supers))
	   (new-type (type-register name parent parameters signals)))
      
      ;; Build the class
      (let ((the-class (apply make metaclass
			      #:dsupers (append (list gobject-super) supers)
			      #:slots (append parameter-slots slots)
			      #:name name)))
	(hash-set! $type-class-hash new-type the-class)
	the-class))))

;; An association-list of namespaces and versions
;; e.g. (("Gtk" . "3.0"))
(define _versions '())

(define (require-version namespace version)
  "Ensures the correct versions are loaded when importing 'gi' modules.
NAMESPACE - a string - The name of the module to require
VERSION - a string - The version number of the module to require

If the module/version is already loaded, already required or unavailable,
it will throw an error.

Example:
  (use-module gi)
  (require-version \"Gtk\" \"3.0\")
"
  (assert-string namespace)
  (assert-string version)
  (let ((repository (get-default Repository)))
    (when (member namespace (get-loaded-namespaces repository))
      (let ((loaded-version (get-version namespace)))
	(unless (equal? loaded-version version)
	  (scm-error 'misc-error "require-version"
		     "Namespace ~A is already loaded with version ~A"
		     (list namespace loaded-version)
		     #f)))))

  (let ((required-version (assoc-ref _versions namespace)))
    (when (and required-verion
	       (not (equal? verion required-version)))
      (scm-error 'misc-error "require-version"
		 "Namespace ~A already requirs ~A"
		 (list namespace require-version)
		 #f)))

  (let ((available-versions (enumerate-versions namespace)))
    (unless available-versions
      (scm-error 'misc-error "require-versions"
		 "Namespace ~A not available"
		 (list namespace)
		 #f))
    (unless (member version available-versions)
      (scm-error 'misc-error "require-versions"
		 "Namespace ~A not available for versions ~A"
		 (list namespace version))))
  (set! _versions
    (assoc-set! _versions namespace version)))

(define* (require-foreign namespace #:optional symbol)
  "Ensure the given foreign marshaling module is available a loaded.
NAMESPACE - a string - Introspection namespace of the foreign module (e.g. \"cairo\")
SYMBOL - a string - Optional symbol typename to ensure a converter exists

Throws an error on failure.

Example:
  (import gi)
  (import cairo)
  (require-foreign \"cairo\")
"
  ((@ (gi _gi) require-foreign) namespace symbol)
  (import-module "gi.repository" namespace))

(load-extension ".libs/libguile-gi.so" "gir_init")
