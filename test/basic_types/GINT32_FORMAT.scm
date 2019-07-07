(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (begin
   (write GINT32_FORMAT) (newline)
   ;; A string.  Usually "i".
   (string? GINT32_FORMAT)))
