(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let* ((x (passthrough-one-gintptr 1)))
   (write x) (newline)
   (= x 1)))
