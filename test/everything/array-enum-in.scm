(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->int-vector `(,ENUM_VALUE1 ,ENUM_VALUE2 ,ENUM_VALUE3))))
   (format #t "Input: ~S~%" x)
   (array-enum-in x)
   #t))
