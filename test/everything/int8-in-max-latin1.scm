(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x #\delete))
   (format #t "Input: ~S~%" x)
   (int8-in-max x)
   #t))

