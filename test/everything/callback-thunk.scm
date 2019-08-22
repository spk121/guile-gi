(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((cb (new-callback-return-value-only)))
   (= (cb) 42)))
