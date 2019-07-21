(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x CONSTANT_UTF8))
   (format #t "Input Before: ~S~%" x)
   (let ((y (utf8-full-inout x)))
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" y)
     (string-null? y))))

