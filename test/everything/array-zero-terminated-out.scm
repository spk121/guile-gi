(use-modules (gi)
             (srfi srfi-43)
             (test automake-test-lib))

(use-typelibs ("Marshall" "1.0"))

(automake-test
 (let ((x (array-zero-terminated-out)))
   (format #t "Output: ~S~%" x)
   (vector= string=? #("0" "1" "2") x)))
