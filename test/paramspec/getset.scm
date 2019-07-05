(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define param
  (list->gparamspec
   `("my-param"
     ,G_TYPE_INT
     "my-param"
     "This is a test parameter"
     -200 400 0
     ,(logior G_PARAM_READABLE
              G_PARAM_WRITABLE))))

(define <TestParam>
  (register-type
   "TestParam"
   <GObject>
   (list param)))

(automake-test
 (let ((object (make-gobject <TestParam>)))
   (and (= (gobject-get-property object "my-param") 0)
        (begin
          (gobject-set-property! object "my-param" 200)
          (= (gobject-get-property object "my-param") 200)))))
