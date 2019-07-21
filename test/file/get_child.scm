(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (equal? "/tmp"
         (file:get-path
          (file:get-child (file:new-for-path "/") "tmp"))))
