(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((str-bv (string->utf8 CONSTANT_UTF8)))
   (write str-bv)
   (utf8-as-uint8array-in str-bv)
   #t))
