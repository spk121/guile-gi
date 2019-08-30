(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (utf8-none-return)))
   (format #t "Output: ~S~%" x)
   (string=? x CONSTANT_UTF8)))
