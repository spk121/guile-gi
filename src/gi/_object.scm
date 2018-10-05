(define-module (gi _object)
  #:use-module (oop goops)
  #:use-module (ice-9 weak-vector)
  #:use-module (rnrs hashtables)
  #:use-module (gi _none)
  #:export (<GObject>
	    incref
	    decref))

(define-class <GObject> ()
  (ob_type   #:init-value #f
	     #:getter get-ob-type
	     #:setter set-ob-type!
	     #:init-keyword #:type)
  (ob_refcnt #:init-value 1
	     #:getter get-ob-refcnt
	     #:setter set-ob-refcnt!
	     #:init-keyword #:refcnt)
  (obj       #:init-value $NONE
	     #:getter get-obj
	     #:setter set-obj!
	     #:init-keyword #:obj)
  (finalizer #:init-value #f
	     #:getter get-finalizer
	     #:setter set-finalizer!
	     #:init-keyword #:finalizer)
  (inst_dict #:init-thunk (lambda () (make-hash-table))
	     #:getter get-inst-dict)
  (weakreflist #:init-thunk (lambda ()(make-doubly-weak-hash-table 5))))

(define-method (incref (x <GObject>))
  "Increase a reference count on the OBJ slot"
  (let ((refcnt (get-ob-refcnt x)))
    (unless (= refcnt 0)
      (set-ob-refcnt! x (1+ refcnt))
      *unspecified*)))

(define-method (decref (x <GObject>))
  "Decreate a reference count on the OBJ slot, calling the
FINALIZER if the reference count hits zero"
  (let ((refcnt (get-ob-refcnt x)))
    (unless (= refcnt 0)
      (set-ob-refcnt! x (1- refcnt))
      (when (= refcnt 1)
	(let ((finalize (get-finalizer x))
	      (obj (get-obj x)))
	  (when (and finalize (not (none? obj)))
	    (finalize obj)))
	(set-obj! x $NONE)
	*unspecified*))))

#|
(define (object->concrete-gtype type)
  (let ((gtype (object->gtype type)))
    (if (g-type-is-abstract? type)
	(scm-error 'misc-error "object->concrete-gtype"
		   "cannot create a concrete GType from an abstract (non-instantiable) type '~A'"
		   (list type) #f)
	;; else
	gtype)))

(define (make-gobject type args)
  (let ((gtype (object->concrete-gtype type))
|#	

	


	     
