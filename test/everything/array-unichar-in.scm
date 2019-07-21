(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (begin
   (format #t "Input: ~S~%" CONSTANT_UTF8)
   (array-unichar-in CONSTANT_UTF8)
   #t))


