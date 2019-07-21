(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((flag #f))
   (format #t "Input before: ~S~%" flag)
   (let ((ret (boolean-inout-false-true flag)))
     (format #t "Input after: ~S~%" flag)
     (format #t "Output: ~S~%" ret)
     ret)))
