(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (begin
   (eqv? #t (boolean-return-true?))))
