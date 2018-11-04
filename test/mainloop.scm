(use-modules (lib)
	     (ice-9 eval-string)
	     (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)
(set-record-type-printer! <GBox> gbox-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)
  
  (with-test-prefix
   "load typelib"
   
   (pass-if "load GLib 2.0 repository"
	    (eval-string (gi-load-repository "GLib" "2.0"))
	    #t))

  (with-test-prefix
   "main context"
   (let ((ctx (main-context-new))
	 (source #f)
	 (id 0))
		    
     (pass-if "main context is not pending"
	      (not (main-context-pending? ctx)))))
  
  (with-text-prefix
   "unload typelib"
   (pass-if "unload repositories"
	    (gi-unload-repositories)
	    #t))
  
  (print-counts (results-proc))
  (exit-value (results-proc)))
