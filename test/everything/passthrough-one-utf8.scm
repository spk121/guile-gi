(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let* ((x (passthrough-one-utf8 "Việt Nam")))
   (write x) (newline)
   (string=? x "Việt Nam")))
