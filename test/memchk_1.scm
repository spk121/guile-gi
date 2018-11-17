(use-modules (gi)
	     (lib))

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))

  (register-reporter counter-proc)
  (register-reporter full-reporter)
  
  (with-test-prefix
   "memchk1"
   (pass-if "gc"
	    (gc)
	    #t))
  
  (print-counts (results-proc))
  (exit-value (results-proc)))

