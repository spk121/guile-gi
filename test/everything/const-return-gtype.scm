(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let ((x (const-return-gtype)))
   (write x)
   (newline)
   (eq? x <GObject>)))
