(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (utf8-dangling-out)))
   (format #t "Output: ~S~%" x)
   #t))
