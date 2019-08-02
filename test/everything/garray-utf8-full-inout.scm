(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x #("0" "1" "2")))
   (format #t "Input Before: ~S~%" x)
   (let ((y (garray-utf8-full-inout x)))
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" y)
     (list= string=? '("-2" "-1" "0" "1") (vector->list y)))))

