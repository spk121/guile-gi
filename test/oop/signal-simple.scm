(use-modules (gi)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define my-signal
  (make <signal>
    #:name "my-signal"
    #:return-type G_TYPE_NONE))

(define <TestParam>
  (register-type
   "TestParam"
   <GObject>
   '()
   (list my-signal)))

(automake-test
 (let ((object (make-gobject <TestParam>))
       (success #f))
   (connect object my-signal (lambda (obj) (set! success #t)))
   (my-signal object)
   success))
