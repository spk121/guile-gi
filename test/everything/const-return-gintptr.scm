(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 ((lambda (x)
   (write x)
   (newline)
   (= 0 x))
  (const-return-gintptr)))
