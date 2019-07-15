(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define my-param
  (param-spec-int
   "my-param"
   "my-param"
   "This is a test parameter"
   -200 200 0
   PARAM_READWRITE))

(define <TestParam>
  ((@ (gi) register-type)
   "TestParam"
   <GObject>
   (list my-param)))

(automake-test
 (let ((object (make <TestParam>)))
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
