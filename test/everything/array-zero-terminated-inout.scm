(use-modules (gi)
             (srfi srfi-43)
             (test automake-test-lib))

(use-typelibs ("Marshall" "1.0"))

(automake-test
 (let ((x #("0" "1" "2")))
   (format #t "Input Before: ~S~%" x)
   (let ((y (array-zero-terminated-inout x)))
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" y)
     (vector= string=? #("-1" "0" "1" "2") y))))
 
