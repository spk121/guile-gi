(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let* ((x (passthrough-one-filename "temp.txt")))
   (write x) (newline)
   (string=? x "temp.txt")))
