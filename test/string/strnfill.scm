(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (begin
   (let* ((str (strnfill 10 (char->integer #\A))))
     (write str) (newline)
     (string=? str "AAAAAAAAAA"))))
