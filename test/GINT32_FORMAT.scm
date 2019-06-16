(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (begin
   (write GINT32_FORMAT) (newline)
   ;; A string.  Usually "i".
   (string? GINT32_FORMAT)))
