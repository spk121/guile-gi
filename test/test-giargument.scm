(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign)
	     (srfi srfi-9 gnu))

;; (set-record-type-printer! <GObject> gobject-printer)

(define (float=? a b)
  (let ((epsilon (max 1.0e-10 (* 1.0e-5 (max (abs a) (abs b))))))
    (< (abs (- a b)) epsilon)))

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "const void pointer"
   (pass-if "pointer is preserved"
	    (let* ((bv (make-bytevector 10))
		   (ptr (bytevector->pointer bv))
		   (arg (pointer->giargument ptr))
		   (ptr2 (giargument->pointer arg)))
	      (equal? ptr ptr2)))
   )

  (with-test-prefix
   "integer"
   (pass-if "int8 1 is preserved"
	    (let* ((arg (basic-type-object->giargument 1 GI_TYPE_TAG_INT8))
		   (obj (basic-type-giargument->object arg GI_TYPE_TAG_INT8)))
	      (write arg) (newline)
	      (write obj) (newline)
	      (equal? obj 1))))

  (with-test-prefix
   "string"
   (pass-if "string 'hello' is preserved"
	    (let* ((arg (basic-type-object->giargument "hello" GI_TYPE_TAG_UTF8))
		   (obj (basic-type-giargument->object arg GI_TYPE_TAG_UTF8)))
	      (string=? "hello" obj))))
  
  (print-counts (results-proc))
  (exit-value (results-proc)))
