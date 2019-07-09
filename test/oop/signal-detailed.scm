(use-modules (gi)
             (gi oop)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define my-signal
  (make <signal>
    #:name "my-signal"
    #:flags G_SIGNAL_DETAILED
    #:return-type G_TYPE_NONE))

(define <TestParam>
  (register-type
   "TestParam"
   <GObject>
   '()
   (list my-signal)))

(automake-test
 (let ((object (make-gobject <TestParam>))
       (success #f)
       (fail #f))
   (connect object my-signal 'wrong-detail (lambda (obj) (set! fail #t)))
   (connect object my-signal 'detail (lambda (obj) (set! success #t)))
   (my-signal object 'detail)
   (and success (not fail))))
