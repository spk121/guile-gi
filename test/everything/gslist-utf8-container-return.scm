(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (gslist-utf8-container-return)))
   (format #t "Output: ~S~%" x)
   (list= string=? x '("0" "1" "2"))))

