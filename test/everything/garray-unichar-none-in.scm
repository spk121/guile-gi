(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x CONSTANT_UTF8))
   (format #t "Input: ~S~%" x)
   (garray-unichar-none-in x)
   #t))
