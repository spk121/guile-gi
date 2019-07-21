(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors)
             (srfi srfi-43))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (array-zero-terminated-return-null)))
   (format #t "Output: ~S~%" x)
   (vector-empty? x)))
