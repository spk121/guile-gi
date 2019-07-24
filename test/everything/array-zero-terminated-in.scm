(use-modules (gi)
             (test automake-test-lib))

(use-typelibs ("Marshall" "1.0"))

(automake-test
 (let ((x #("0" "1" "2")))
   (format #t "Input: ~S~%" x)
   (array-zero-terminated-in x)
   #t))
