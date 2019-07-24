(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

;; The test case returns a (GType) 0.

(automake-test
 (let ((x (one-outparam-gtype)))
   (write x) (newline)
   (= 0 x)))
