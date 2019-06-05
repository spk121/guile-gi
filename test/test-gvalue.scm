(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign)
	     (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)

(define (float=? a b)
  (let ((epsilon (max 1.0e-10 (* 1.0e-5 (max (abs a) (abs b))))))
    (< (abs (- a b)) epsilon)))

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "make-gvalue"
   
   (pass-if-exception "call make-gvalue with wrong-type '#t'"
		      exception:wrong-type-arg
		      (make-gvalue #t))
   
   (pass-if "(make-gvalue G_TYPE_INT) returns a gvalue"
	    (let ((val (make-gvalue G_TYPE_INT)))
	      (gvalue? val)))
   )
  
  (with-test-prefix
      "gvalue-set! boolean types"

      (pass-if "make GValue of G_TYPE_BOOLEAN holding #t"
      	       (let ((val (make-gvalue G_TYPE_BOOLEAN)))
      		 (gvalue-set! val #t)
       		 (eq? #t (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_BOOLEAN holding #f"
      	       (let ((val (make-gvalue G_TYPE_BOOLEAN)))
      		 (gvalue-set! val #f)
       		 (eq? #f (gvalue-get val))))

      (pass-if-exception "make GValue of G_TYPE_BOOLEAN with wrong-type '0'"
			 exception:wrong-type-arg
      	       (let ((val (make-gvalue G_TYPE_BOOLEAN)))
      		 (gvalue-set! val 0)))

      )
  
  (with-test-prefix
      "gvalue-set! integer types"

      (pass-if "make GValue of G_TYPE_CHAR holding #x7F"
      	       (let ((val (make-gvalue G_TYPE_CHAR)))
      		 (gvalue-set! val #x7F)
       		 (equal? #x7F (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_CHAR holding #x-80"
      	       (let ((val (make-gvalue G_TYPE_CHAR)))
      		 (gvalue-set! val #x-80)
       		 (equal? #x-80 (gvalue-get val))))

      (pass-if-exception "make GValue of G_TYPE_CHAR with out-of-range #x80"
			 exception:out-of-range
      	       (let ((val (make-gvalue G_TYPE_CHAR)))
      		 (gvalue-set! val #x80)))

      (pass-if-exception "make GValue of G_TYPE_CHAR with out-of-range #x-81"
			 exception:out-of-range
      			 (let ((val (make-gvalue G_TYPE_CHAR)))
      			   (gvalue-set! val #x-81)))

      (pass-if-exception "make GValue of G_TYPE_CHAR with wrong type '#t'"
			 exception:wrong-type-arg
      	       (let ((val (make-gvalue G_TYPE_CHAR)))
      		 (gvalue-set! val #t)))

      (pass-if "make GValue of G_TYPE_INT holding zero"
      	       (let ((val (make-gvalue G_TYPE_INT)))
      		 (gvalue-set! val 0)
       		 (equal? 0 (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_INT holding #x7FFF"
      	       (let ((val (make-gvalue G_TYPE_INT)))
      		 (gvalue-set! val #x7FFF)
       		 (equal? #x7FFF (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_INT holding #x-8000"
      	       (let ((val (make-gvalue G_TYPE_INT)))
      		 (gvalue-set! val #x-8000)
       		 (equal? #x-8000 (gvalue-get val))))

      (pass-if-exception "make GValue of G_TYPE_INT with out-of-range #x7FFFFFFFFFFFFFFFF"
			 exception:out-of-range
      	       (let ((val (make-gvalue G_TYPE_INT)))
      		 (gvalue-set! val #x7FFFFFFFFFFFFFFFF)))

      (pass-if-exception "make GValue of G_TYPE_INT with wrong type '0.1'"
			 exception:wrong-type-arg
      	       (let ((val (make-gvalue G_TYPE_INT)))
      		 (gvalue-set! val 0.1)))
      )
  
  (with-test-prefix
      "gvalue-set! real types"
      
      (pass-if "make GValue of G_TYPE_FLOAT holding 0.0"
      	       (let ((val (make-gvalue G_TYPE_FLOAT)))
      		 (gvalue-set! val 0.0)
       		 (float=? 0.0 (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_FLOAT holding 1.0"
      	       (let ((val (make-gvalue G_TYPE_FLOAT)))
      		 (gvalue-set! val 1.0)
		 (float=? 1.0 (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_FLOAT holding integer 1"
      	       (let ((val (make-gvalue G_TYPE_FLOAT)))
      		 (gvalue-set! val 1)
		 (float=? 1.0 (gvalue-get val))))

      (pass-if-exception "make GValue of G_TYPE_FLOAT with out-of-range 1.0e50"
			 exception:out-of-range
      	       (let ((val (make-gvalue G_TYPE_FLOAT)))
      		 (gvalue-set! val 1.0e50)))
      )
  
  (with-test-prefix
   "gvalue-set! strings"
   
   (pass-if "make GValue of G_TYPE_STRING holding \"\""
      	    (let ((val (make-gvalue G_TYPE_STRING)))
      	      (gvalue-set! val "")
       	      (string= "" (gvalue-get val))))
   
   (pass-if "make GValue of G_TYPE_STRING holding \"foo\""
      	    (let ((val (make-gvalue G_TYPE_STRING)))
      	      (gvalue-set! val "foo")
       	      (string= "foo" (gvalue-get val))))
   
   (pass-if "make GValue of G_TYPE_STRING holding U+2B77C U+2B77D"
	    (let ((str (string (integer->char #x2b77c)
			       (integer->char #x2b22d)))
		  (val (make-gvalue G_TYPE_STRING)))
	      (gvalue-set! val str)
	      (string= str (gvalue-get val))))
   
   (pass-if-exception "make GValue of G_TYPE_STRING with wrong-type (symbol)"
		      exception:wrong-type-arg
      		      (let ((val (make-gvalue G_TYPE_STRING)))
      			(gvalue-set! val 'foo)))
   
   )
  
  (with-test-prefix
      "gvalue-set! raw pointers"
      
      (pass-if "make GValue of G_TYPE_POINTER holding NULL"
      	       (let ((val (make-gvalue G_TYPE_POINTER)))
      		 (gvalue-set! val %null-pointer)
       		 (equal? %null-pointer (gvalue-get val))))
      
      (pass-if "make GValue of G_TYPE_POINTER holding #xDEADBEEF"
      	       (let ((val (make-gvalue G_TYPE_POINTER))
		     (ptr (make-pointer #xDEADBEEF)))
      		 (gvalue-set! val ptr)
       		 (equal? ptr (gvalue-get val))))

      (pass-if "make GValue of G_TYPE_POINTER holding bytevetor"
	       (let ((val (make-gvalue G_TYPE_POINTER))
		     (bv (make-bytevector 16 0)))
		 (gvalue-set! val (bytevector->pointer bv))
		 (equal? (bytevector->pointer bv)
			 (gvalue-get val))))

      (pass-if-exception "make GValue of G_TYPE_POINTER with wrong-type \"foo\""
			 exception:wrong-type-arg
			 (let ((val (make-gvalue G_TYPE_POINTER)))
			   (gvalue-set! val "foo")))

      )

    (with-test-prefix
      "gvalue->gtype"

      (pass-if-exception "call with wrong-type integer 0"
			 exception:wrong-type-arg
			 (gvalue->gtype 0))
      
      (pass-if "G_TYPE_POINTER"
      	       (let ((val (make-gvalue G_TYPE_POINTER)))
		 (equal? G_TYPE_POINTER (gvalue->gtype val))))

      (pass-if "G_TYPE_INT"
      	       (let ((val (make-gvalue G_TYPE_INT)))
		 (equal? G_TYPE_INT (gvalue->gtype val))))
      )

    (with-test-prefix
      "gvalue-holds?"

      (pass-if-exception "call with wrong-types (0, 0)"
			 exception:wrong-type-arg
			 (gvalue-holds? 0 0))
      
      (pass-if "G_TYPE_POINTER holds G_TYPE_POINTER"
      	       (let ((val (make-gvalue G_TYPE_POINTER)))
		 (gvalue-holds? val G_TYPE_POINTER)))

      (pass-if "G_TYPE_STRING holds G_TYPE_STRING"
      	       (let ((val (make-gvalue G_TYPE_STRING)))
		 (gvalue-holds? val G_TYPE_STRING)))

      (pass-if "G_TYPE_LONG holds G_TYPE_LONG"
      	       (let ((val (make-gvalue G_TYPE_LONG)))
		 (gvalue-holds? val G_TYPE_LONG)))
      )

    (with-test-prefix
      "gvalue-type-name"

      (pass-if-exception "call with wrong-type integer zero"
			 exception:wrong-type-arg
			 (gvalue-type-name 0))
      
      (pass-if "type name of GValue of G_TYPE_LONG is a string"
      	       (let ((val (make-gvalue G_TYPE_LONG)))
		 (string? (gvalue-type-name val))))
      )
    
    (print-counts (results-proc))
  (exit-value (results-proc)))
