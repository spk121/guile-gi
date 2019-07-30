(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define my-signal
  (make-signal
   #:name "my-signal"
   #:return-type G_TYPE_NONE))

(define <TestSignal>
  ((@ (gi) register-type)
   "TestSignal"
   <GObject>
   '()
   (list my-signal)))

(automake-test
 (let ((object (make <TestSignal>))
       (handled #f))
   (let ((handler-id (connect object my-signal
                              (lambda (obj) (set! handled #t)))))
     (and (begin (signal-handler-block object handler-id)
                 (my-signal object)
                 (not handled))
          (begin (signal-handler-unblock object handler-id)
                 (my-signal object)
                 handled)))))
