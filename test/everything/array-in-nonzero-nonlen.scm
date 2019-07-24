(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (string->utf8 "abcd")))
   (format #t "Input: ~S~%" x)
   (array-in-nonzero-nonlen 1 x)
   #t))
