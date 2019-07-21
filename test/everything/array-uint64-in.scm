(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->u64vector '(#xFFFFFFFFFFFFFFFF 0 1 2))))
   (format #t "Input: ~S~%" x)
   (array-uint64-in x)
   #t))

