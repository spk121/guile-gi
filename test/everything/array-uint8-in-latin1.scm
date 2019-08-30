(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (string->utf8 "abcd")))
   (format #t "Input: ~S~%" x)
   (array-uint8-in x)
   #t))
