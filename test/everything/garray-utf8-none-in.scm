(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x #("0" "1" "2")))
   (format #t "Input: ~S~%" x)
   (garray-utf8-none-in x)
   #t))
