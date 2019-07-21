(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((ret (init-function)))
   (format #t "Output: ~S~%" ret)
   ret))
