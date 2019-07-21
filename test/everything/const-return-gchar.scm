(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

;; Looks like gchar is returned as int8.

(automake-test
 (let ((x (const-return-gchar)))
   (write x)
   (newline)
   (eq? x 0)))
