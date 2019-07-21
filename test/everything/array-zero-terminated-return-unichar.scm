(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (array-zero-terminated-return-unichar)))
   (format #t "Output: ~S~%" x)
   (string=? x CONSTANT_UTF8)))
