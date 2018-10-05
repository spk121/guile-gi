(use-modules (lib)
	     (gi gobject))

(define TYPE_ALPHA #f)
(define OBJ_ALPHA #f)
(define TYPE_BETA #f)
(define OBJ_BETA #f)

(with-test-prefix
 "gobject"

 (with-test-prefix "custom type with zero properties"
   (pass-if "register type"
     (set! TYPE_ALPHA (register-type "alpha" G_TYPE_OBJECT #f #f #f))
     (gtype? TYPE_ALPHA))

   (pass-if "registered gtype's name equals registration name"
     (string=? "alpha" (gtype-get-name TYPE_ALPHA)))
   
   (pass-if "registered gtype's parent equals registration parent"
     (gtype=? (gtype-get-parent TYPE_ALPHA) G_TYPE_OBJECT))

   (pass-if "make instance"
     (set! OBJ_ALPHA (make-gobject TYPE_ALPHA))
     (gobject? OBJ_ALPHA)))

 (with-test-prefix "custom type with int property"

   (pass-if "register type"
     (set! TYPE_BETA (register-type "beta" G_TYPE_OBJECT
				    `(("u" ,G_TYPE_INT "u" "u" -100 100 1 ,(logior G_PARAM_READWRITE G_PARAM_CONSTRUCT)))
				    #f #f))
     (gtype? TYPE_BETA))

   (pass-if "registered gtype's name equals registration name"
     (string=? "beta" (gtype-get-name TYPE_BETA)))
   
   (pass-if "registered gtype's parent equals registration parent"
     (gtype=? (gtype-get-parent TYPE_BETA) G_TYPE_OBJECT))

   (pass-if "make instance w/ default property"
     (set! OBJ_BETA (make-gobject TYPE_BETA))
     (gobject? OBJ_BETA))

   (pass-if "instance's property has default value"
     (equal? (gobject-get-property OBJ_BETA "u")
	     1))

   (pass-if "make instance w/ set property"
     (set! OBJ_BETA (make-gobject TYPE_BETA '(("u" . 50))))
     (gobject? OBJ_BETA))

   (pass-if "instance's property has set value"
     (equal? (gobject-get-property OBJ_BETA "u")
	     50))

   (pass-if "instance's property can be set"
     (gobject-set-property! OBJ_BETA "u" 49)
     (write (gobject-get-property OBJ_BETA "u"))
     (equal? 49 (gobject-get-property OBJ_BETA "u")))))
