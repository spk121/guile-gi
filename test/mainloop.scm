(use-modules (lib)
             (rnrs bytevectors)
             (system foreign)
             (ice-9 eval-string)
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
   
   (pass-if "load GLib 2.0 repository"
            (eval-string (gi-load-repository "GLib" "2.0"))
            #t))

  (with-test-prefix
   "main context"

   (pass-if "main-context-new returns a context"
            (set! ctx (main-context-new))
            (gbox? ctx))
   
   (pass-if "main context is not pending"
            (not (main-context-pending? ctx)))

   (pass-if "an iteration of the main loop doesn't dispatch events"
            (not (main-context-iteration? ctx #f)))

   (pass-if "g_idle_remove_by_data works"
	    (let ((callback (lambda (user-data)
			      (format #t "In callback.  Received ~s~%" user-data)
			      #f))
		  (user-data (make-bytevector 8 0)))
	      (idle-add PRIORITY_DEFAULT_IDLE
			callback
			(bytevector->pointer user-data) #f)
	      (idle-remove-by-data? (bytevector->pointer user-data)))))

  (with-test-prefix
   "mainloop basics"
   (let ((loop #f)
         (ctx #f))
     
     (pass-if "new main loop is not running"
              (set! loop (main-loop-new #f #f))
              (not (main-loop-is-running? loop)))

     (pass-if "main loop context is default context"
              (set! ctx (main-loop-get-context loop))
              (format #t "main loop context: ~S. default context: ~S.~%" ctx (main-context-default))
              (equal? ctx (main-context-default)))

     (pass-if "main depth is zero"
              (= (main-depth) 0))

     (pass-if "garbage collection doesn't crash"
              (set! loop #f)
              (gc)
              #t)
     ))

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

     (pass-if "timeouts can run"
	      (let ((sourceA (timeout-source-new 100))
		    (sourceB (timeout-source-new 250))
		    (sourceC (timeout-source-new 330))
		    (sourceD (timeout-source-new 1050)))
		(set! ctx (main-context-new))
		(set! loop (main-loop-new ctx #f))
		(source-set-callback sourceA a++ #f #f)
		(source-set-callback sourceB b++ #f #f)
		(source-set-callback sourceC c++ #f #f)
		(source-set-callback sourceD main-loop-quit loop #f)
		(source-attach sourceA ctx)
		(source-attach sourceB ctx)
		(source-attach sourceC ctx)
		(source-attach sourceD ctx)
		(main-loop-run loop)
		#t))

     (pass-if "fast source fired"
              (> a 0))
     (pass-if "fast source fired more often than medium source"
              (>= a b))
     (pass-if "medium source fired more often than slow source"
              (>= b c))
     (pass-if "fast source fired <= 10 times"
              (<= a 10))
     (pass-if "medium source fired <= 4 times"
              (<= b 4))
     (pass-if "slow source fired <= 3 times"
              (<= c 3))

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

     (pass-if "setup test"
              (set! ctx (main-context-new))
              (set! sourceA (idle-source-new))
              (source-set-callback sourceA
                                   (gir-callback-new SourceFunc
                                                     (lambda (x)
                                                       (set! a (1+ a))))
                                   #f #f)
              (source-set-priority sourceA 1)
              (source-attach sourceA ctx)

              (set! sourceB (idle-source-new))
              (source-set-callback sourceB
                                   (gir-callback-new SourceFunc
                                                     (lambda (x)
                                                       (set! b (1+ b))))
                                   #f #f)
              (source-set-priority sourceB 0)
              (source-attach sourceB ctx)

              (main-context-pending? ctx))
     
     (pass-if "iterate once"
              (main-context-iteration? ctx #f))

     (pass-if "low priority callback didn't run"
              (= a 0))

     (pass-if "high priority callback did run"
              (= b 1))

     (pass-if "iterate once"
              (main-context-iteration? ctx #f))

     (pass-if "low priority callback didn't run"
              (= a 0))

     (pass-if "high priority callback did run"
              (= b 2))
     
     (pass-if "GC doesn't crash"
              (set! ctx #f)
              (set! sourceA #f)
              (set! sourceB #f)
              (usleep 1) (gc)
              (usleep 1) (gc)
              (usleep 1) (gc)
              #t)
     ))

  (with-test-prefix
   "test invoke"
   (let ((ctx #f)
         (thread #f)
         (count 0))
     
     (define (func data)
       (when (and data
                  (not (equal? data (thread-self))))
         (misc-error "not running from self thread"))
       (set! count (1+ count))
       #f)

     (define (call-func data)
       (func (thread-self))
       SOURCE_REMOVE)
     
     (pass-if "directly invoking callback in this thread"
              (main-context-invoke-full
               #f                       ; default primary thread context
               0                        ; priority
               (gir-callback-new SourceFunc func)
               (thread-self)            ; data
               #f                       ; destroy notification func
               )
              (= count 1))

     (pass-if "invoking a func from idle"
              (idle-add PRIORITY_DEFAULT_IDLE
                        (gir-callback-new SourceFunc call-func)
                        #f
                        #f)
              (main-context-iteration? (main-context-default) #f)
              (= count 2))))

  (with-test-prefix
   "unload typelib"
   
   (pass-if "unload repositories"
            (gi-unload-repositories)
            #t))
  
  (print-counts (results-proc))
  ;;(exit-value (results-proc))
  )
