(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->short-vector '(-1 0 1 2))))
   (format #t "Input: ~S~%" x)
   (array-fixed-short-in x)
   #t))
