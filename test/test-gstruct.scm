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
   "glib"

   (pass-if "load GLib 2.0 repository"
	    (eval-string (gi-load-repository "GLib" "2.0"))
	    #t))
  
  (with-test-prefix
   "make-gstruct"
   (pass-if "make-gstruct <TimeZone> NULL"
	    (let ((val (make-gstruct <TimeZone> %null-pointer)))
	      (write val) (newline)
        (gtype-get-name (gstruct->gtype val)) (newline)
	      #f)))
    
    (print-counts (results-proc))
  (exit-value (results-proc)))
