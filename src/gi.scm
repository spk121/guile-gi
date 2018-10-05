(define-module (gi)
  #:use-module (gi _gi)
  #:export (
	    require-version
	    ;; require-foreign
	    ;; check-version
	    ;; get-required-version
	    ;; version-info
	    ;; import
	    ))

(define (find-duplicate l)
  (match l
    (() #f)
    ((head . tail)
     (if (memq head tail)
	 head
	 (find-duplicate tail)))))

(define (slot-spec->name slot-spec)
  (match slot-spec
    (((? symbol? name) . args) name)
    ((? slot? slot) (%slot-definition-name slot))))

(define (make-gobject-class name
			    supers
			    slots
			    signals)
  "Create a new GOOPS class with GObject functionality.  NAME is a
symbol that names the class.  SUPERS are the superclasses of this
class.  If SUPERS is not specified, the default GObject class with be
the superclass.  IF SUPERS is specified, then one (and only one)
superclass must be a GObject-based class.  The rest must be
non-GObject based classes.  SLOTS are a list of GOOPS slots, but, if
the slots have the special #:type key, the slots will also be GObject
properties for this class.  SIGNALS is a list of events to which
callback procedures can be attached.

On top of the regular slot options, if a slot has a #:type, it will be a
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
  (let* ((supers (if (not (or-map (lambda (class)
				    (memq <object>
					  (class-precedence-list class)))
				  supers))
		     (append supers (list <object>))
		     supers))
	 (metaclass (ensure-metaclass supers)))

    ;; Verify that all direct slots and properties are different and that we
    ;; don't inherit several times from the same class.
    (let ((tmp1 (find-duplicate (append (list gobject-super) other-supers)))
	  (tmp2 (find-duplicate (append (map slot-spec->name slots)
					(map parameters-spec->name parameters)))))
      (when tmp1
	(goops-error "make-gobject-class: super class ~S is duplicate in class ~S"
		     tmp1 name))
      (when tmp2
	(goops-error "make-gobject-class: slot or property ~S is duplicate in class ~S"
		     tmp2 name)))

    (let ((new-type (type-register name gobject-super parameters signals))
	  (parameter-slots (map parameter->slot parameters)))
      
      ;; Build the class
      (apply make metaclass
             #:dsupers (append (list gobject-super) supers)
             #:slots (append parameter-slots slots)
             #:name name))))
	  
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
