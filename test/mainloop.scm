(use-modules (gi)
	     (lib)
             (rnrs bytevectors)
             (system foreign)
             (ice-9 eval-string)
	     (ice-9 rdelim)
             (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)
(set-record-type-printer! <GBox> gbox-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt))
       (ctx #f)
       (source #f)
       (id 0))
  (register-reporter counter-proc)
  (register-reporter full-reporter)
  
  (with-test-prefix
   "load typelib"
   
   (pass-if "load G 2.0 repository"
            (import-typelib "GLib" "2.0")
            #t))

  (with-test-prefix
   "main context"

   (pass-if "GMainContext-new returns a context"
            (set! ctx (GMainContext-new))
            (gbox? ctx))
   
   (pass-if "main context is not pending"
            (not (GMainContext-pending? ctx)))

   (pass-if "an iteration of the main loop doesn't dispatch events"
            (not (GMainContext-iteration? ctx #f)))

   (pass-if "g_idle_remove_by_data works"
	    (let ((callback (lambda (user-data)
			      (format #t "In callback.  Received ~s~%" user-data)
			      #f))
		  (user-data (make-bytevector 8 0)))
	      (g-idle-add PRIORITY_DEFAULT_IDLE
			callback
			(bytevector->pointer user-data) #f)
   	      (g-idle-remove-by-data (bytevector->pointer user-data)))))

  (flush-all-ports)
  (usleep 100)
  
  (with-test-prefix
   "mainloop basics"
   (let ((loop #f)
         (ctx #f))
     
     (pass-if "new main loop is not running"
              (set! loop (GMainLoop-new #f #f))
              (not (GMainLoop-is-running? loop)))


     (flush-all-ports)
     (usleep 100)
     (pass-if "main loop context is default context"
              (set! ctx (GMainLoop-get-context loop))
              (format #t "main loop context: ~S. default context: ~S.~%" ctx (GMainContext-default))
              (equal? ctx (GMainContext-default)))

     (flush-all-ports)
     (usleep 100)
     (pass-if "main depth is zero"
              (= (g-main-depth) 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "garbage collection doesn't crash"
	      (GMainLoop-quit loop)
              (set! loop #f)
              (gc)
              #t)
     ))

  (flush-all-ports)
  (usleep 100)

  (with-test-prefix
   "test timeouts"
   (let ((ctx #f)
         (loop #f)
         (source #f)
         (a 0)
         (b 0)
         (c 0))
     (define (a++ data)
       (set! a (1+ a)))
     (define (b++ data)
       (set! b (1+ b)))
     (define (c++ data)
       (set! c (1+ c)))
     (define (stopit data)
       (GMainLoop-quit loop))

     (pass-if "setup"
	      (let ((sourceA (g-timeout-source-new 100))
		    (sourceB (g-timeout-source-new 250))
		    (sourceC (g-timeout-source-new 330))
		    (sourceD (g-timeout-source-new 1050)))
		(set! ctx (GMainContext-new))
		(set! loop (GMainLoop-new ctx #f))
		(GSource-set-callback sourceA a++ #f #f)
		(GSource-set-callback sourceB b++ #f #f)
		(GSource-set-callback sourceC c++ #f #f)
		(GSource-set-callback sourceD stopit #f #f)
		(GSource-attach sourceA ctx)
		(GSource-attach sourceB ctx)
		(GSource-attach sourceC ctx)
		(GSource-attach sourceD ctx))
	      #t)

     (pass-if "timeouts can run"
	      (GMainLoop-run loop)
	      #t)

     (flush-all-ports)
     (usleep 100)
     (pass-if "fast source fired"
              (> a 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "fast source fired more often than medium source"
              (>= a b))
     
     (flush-all-ports)
     (usleep 100)
     (pass-if "medium source fired more often than slow source"
              (>= b c))
     
     (flush-all-ports)
     (usleep 100)
     (pass-if "fast source fired <= 10 times"
              (<= a 10))
     
     (flush-all-ports)
     (usleep 100)
     (pass-if "medium source fired <= 4 times"
              (<= b 4))
     
     (flush-all-ports)
     (usleep 100)
     (pass-if "slow source fired <= 3 times"
              (<= c 3))

     (flush-all-ports)
     (usleep 100)
     (pass-if "gc doesn't crash"
              (set! ctx #f)
              (set! loop #f)
              (usleep 1) (gc)
              (usleep 1) (gc)
              (usleep 1) (gc)
              #t)

     ))
         
  (with-test-prefix
   "test priorities"
   (let ((ctx #f)
         (sourceA #f)
         (sourceB #f)
         (a 0)
         (b 0)
         (c 0))

     (pass-if "setup test, part 1"
              (set! ctx (GMainContext-new))
              (set! sourceA (g-idle-source-new))
              (set! sourceB (g-idle-source-new))
	      #t)

     (flush-all-ports)
     (usleep 100)
     (pass-if "setup test, part 2"
              (GSource-set-callback sourceA
                                   (lambda (x)
                                     (set! a (1+ a)))
                                   #f #f)
              (GSource-set-priority sourceA 1)
              (GSource-attach sourceA ctx)

              (GSource-set-callback sourceB
                                   (lambda (x)
                                     (set! b (1+ b)))
                                   #f #f)
              (GSource-set-priority sourceB 0)
              (GSource-attach sourceB ctx)

              (GMainContext-pending? ctx))
     
     (flush-all-ports)
     (usleep 100)
     (pass-if "iterate once"
              (GMainContext-iteration? ctx #f))

     (flush-all-ports)
     (usleep 100)
     (pass-if "low priority callback didn't run"
              (= a 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "high priority callback did run"
              (= b 1))

     (flush-all-ports)
     (usleep 100)
     (pass-if "iterate once"
              (GMainContext-iteration? ctx #f))

     (flush-all-ports)
     (usleep 100)
     (pass-if "low priority callback didn't run"
              (= a 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "high priority callback did run"
              (= b 2))
     
     (flush-all-ports)
     (usleep 100)
     (pass-if "GC doesn't crash"
              (set! ctx #f)
              (usleep 1) (gc)
              (set! sourceA #f)
              (usleep 1) (gc)
              (set! sourceB #f)
              (usleep 1) (gc)
              #t)
     ))

  (flush-all-ports)
  (usleep 100)
  
  (with-test-prefix
   "test invoke"
   (let ((ctx #f)
         (thread #f)
         (count 0))
     
     (define (func data)
       (set! count (1+ count))
       SOURCE_REMOVE)

     (define (call-func data)
       ;; (func (g-thread-self))
       (func #f)
       SOURCE_REMOVE)
     
     (pass-if "invoking a func from idle"
	      (set! count 0)
              (g-idle-add PRIORITY_DEFAULT_IDLE
			call-func
                        #f
                        #f)
              (GMainContext-iteration? (GMainContext-default) #f)
              (= count 1))
     
     (flush-all-ports)
     (usleep 100)
     ))

  (print-counts (results-proc))
  (exit-value (results-proc))
  )
