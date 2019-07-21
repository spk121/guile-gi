(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x #("foo" "bar")))
   (format #t "Input: ~S~%" x)
   (array-string-in x)
   #t))
