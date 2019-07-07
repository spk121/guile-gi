(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let ((doc (typelib-document "GLib" "2.0")))
   (display doc)
   (string? doc)))
