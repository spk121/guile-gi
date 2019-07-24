(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (begin
   (oneparam-gchar 1)
   #t))
