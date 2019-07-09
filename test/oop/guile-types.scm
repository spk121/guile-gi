(use-modules (gi)
             (gi oop)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define my-param
  (make <number-property>
    #:name "my-param"
    #:blurb "This is a test parameter"
    #:type G_TYPE_INT
    #:min -200
    #:max 200))

(define <TestParam>
  (register-type
   "TestParam"
   <GObject>
   (list my-param)))

(automake-test
 (let ((object (make-gobject <TestParam>)))
   (and (= (gobject-get-property object "my-param") (my-param object) 0)
        (begin
          (gobject-set-property! object "my-param" 100)
          (= (gobject-get-property object "my-param")
             (my-param object)
             100))
        (begin
          (set! (my-param object) 200)
          (= (gobject-get-property object "my-param")
             (my-param object)
             200)))))
