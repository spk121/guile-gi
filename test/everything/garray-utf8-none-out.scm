(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (garray-utf8-none-out)))
   (format #t "Output: ~S~%" x)
   (list= string=? '("0" "1" "2") (vector->list x))))

