(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign)
	     (srfi srfi-9 gnu)
	     (ice-9 eval-string))

(set-record-type-printer! <GObject> gobject-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "gobject"
    
   (pass-if "load Gtk 3.0 repository"
	    (if (false-if-exception (eval-string (gi-load-repository "Gtk" "3.0")))
		#t
		#f))

   (pass-if "subclass <Application>"
	    (let ((<ExampleApp> (register-type "ExampleApp" <Application> #f #f #f)))
	      (write <ExampleApp>)
	      (newline)
	      #t))))

   #|
(define TYPE_ALPHA #f)
(define OBJ_ALPHA #f)
(define TYPE_BETA #f)
(define OBJ_BETA #f)

(define (register-type-zero-props-zero-signals n)
  (format #t "# Register type TYPE_ALPHA with zero properties and zero signals~%")
  (set! TYPE_ALPHA (register-type "alpha" G_TYPE_OBJECT #f #f #f))
  (let ((test1 TYPE_ALPHA)
	(test2 (string=? "alpha" (gtype-get-name TYPE_ALPHA)))
	(test3 (gtype=? (gtype-get-parent TYPE_ALPHA) G_TYPE_OBJECT)))
    (format #t "# type: ~S, name: ~S, parent: ~S, checks ~s ~s ~s ~%"
	    TYPE_ALPHA (gtype-get-name TYPE_ALPHA) (gtype-get-parent TYPE_ALPHA)
	    test1 test2 test3)
    (if (and test1 test2 test3)
	(format #t "ok ~A - register custom type with zero properties and zero signals ~%" n)
	(format #t "not ok ~A - register custom type with zero properties and zero signals ~%" n))))

(define (instantiate-type-zero-props-zero-signals n)
  (format #t "# Make an instance of type TYPE_ALPHA~%")
  (set! OBJ_ALPHA (make-gobject TYPE_ALPHA))
  (format #t "# OBJ_ALPHA: ~S~%" OBJ_ALPHA)
  (if OBJ_ALPHA
      (format #t "ok ~A - instantiate custom type with zero properties and zero signals ~%" n)
      (format #t "not ok ~A - instantiate custom type with zero properties and zero signals ~%" n)))

(define (register-type-int-prop-zero-signals n)
  (format #t "# Register type TYPE_BETA with one integer property and zero signals~%")
  (set! TYPE_BETA (register-type "beta" G_TYPE_OBJECT
				 `(("u" ,G_TYPE_INT "u" "u" -100 100 1 ,(logior G_PARAM_READWRITE G_PARAM_CONSTRUCT)))
				 #f #f))
  (let ((test1 TYPE_BETA)
	(test2 (string=? "beta" (gtype-get-name TYPE_BETA)))
	(test3 (gtype=? (gtype-get-parent TYPE_BETA) G_TYPE_OBJECT)))
    (format #t "# type: ~S, name: ~S, parent: ~S, checks ~s ~s ~s ~%"
	    TYPE_BETA (gtype-get-name TYPE_BETA) (gtype-get-parent TYPE_BETA)
	    test1 test2 test3)
    (if (and test1 test2 test3)
	(format #t "ok ~A - register custom type with an int property and zero signals ~%" n)
	(format #t "not ok ~A - register custom type with an int property and zero signals ~%" n))))

(define (instantiate-type-int-prop-zero-signals n)
  (format #t "# Make an instance of type TYPE_BETA~%")
  (set! OBJ_BETA (make-gobject TYPE_BETA '(("u" . 50))))
  (format #t "# OBJ_BETA: ~S~%" OBJ_BETA)
  (let ((test1 OBJ_BETA)
	(test2 (equal? 50 (gobject-get-property OBJ_BETA "u"))))
    (if (and test1 test2)
	(format #t "ok ~A - instantiate custom type with int property and zero signals ~%" n)
	(format #t "not ok ~A - instantiate custom type with int property and zero signals ~%" n))))

(define TESTS
  (list
   register-type-zero-props-zero-signals
   instantiate-type-zero-props-zero-signals
   register-type-int-prop-zero-signals
   instantiate-type-int-prop-zero-signals))

(define (main . args)
  (let ((n-tests (length TESTS)))
    (format #t "1..~A~%" n-tests)
    (do ((i 1 (1+ i)))
	((> i n-tests))
      (let ((test (list-ref TESTS (1- i))))
	(test i)))))

(main)
;; Local Variables:
;; mode: scheme
;; End:
|#
