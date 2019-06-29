(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let ((doc (typelib-document "GLib" "2.0")))
   (display doc)
   (string? doc)))
