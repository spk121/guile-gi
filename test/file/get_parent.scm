(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (equal? "/"
         (file:get-path
          (file:get-parent (file:new-for-path "/tmp")))))
