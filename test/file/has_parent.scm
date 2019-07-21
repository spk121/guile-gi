(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (file:has-parent? (file:new-for-path "/tmp")))
