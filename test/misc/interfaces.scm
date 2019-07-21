(use-modules (gi)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (memq <GActionMap> (class-precedence-list <GApplication>)))
