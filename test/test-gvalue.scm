(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign)
	     (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "initialization"
   (pass-if "load GLib 2.0 repository"
	    (gi-load-repository "GLib" "2.0")
	    #t))

  (with-test-prefix
      "integer type"
      (pass-if "G_TYPE_INT is a gtype"
	       (gtype? G_TYPE_INT))

      (pass-if "a gvalue of type G_TYPE_INT is a gvalue"
	       (let ((val (make-gvalue G_TYPE_INT)))
		 (gvalue? val)))

      ;; (pass-if "make GValue of G_TYPE_INT holding zero"
      ;; 	       (let ((val (make-gvalue G_TYPE_INT)))
      ;; 		 (gvalue-set! val 0)
      ;; 		 (equal? 0 (gvalue-get val))))
      )
  
  (print-counts (results-proc))
  (exit-value (results-proc)))
