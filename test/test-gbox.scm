(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign)
	     (ice-9 eval-string)
	     (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "make-pointer-gbox"
   (pass-if "for G_TYPE_NONE, pointer is preserved"
	    (let* ((bv (string->utf8 "hello"))
		   (ptr (bytevector->pointer bv))
		   (box (make-pointer-gbox G_TYPE_NONE ptr #f)))
	      (equal? ptr (gbox-peek-pointer box))))

   (pass-if "for G_TYPE_NONE, type is preserved"
	    (let* ((bv (string->utf8 "hello"))
		   (ptr (bytevector->pointer bv))
		   (box (make-pointer-gbox G_TYPE_NONE ptr #f)))
	      (equal? G_TYPE_NONE (gbox-get-gtype box))))

   (pass-if "box is created with refcount = 1"
	    (let* ((bv (string->utf8 "hello"))
		   (ptr (bytevector->pointer bv))
		   (box (make-pointer-gbox G_TYPE_NONE ptr #f)))
	      (equal? 1 (%gbox-get-refcount box)))))
   
  (with-test-prefix
   "make-const-gbox"
   (pass-if "load GLib 2.0"
	    (eval-string (gi-load-repository "GLib" "2.0"))
	    #t)
   
   (pass-if "make a GBytes from a bytevector"
	    (let* ((vec (vector 1 2 3 4 5))
		   (gbytes (bytes-new vec 5)))
	      (write gbytes) (newline)
	      (write (gbox-get-gtype gbytes)) (newline)
	      #f)))
   
  
  (print-counts (results-proc))
  (exit-value (results-proc)))
