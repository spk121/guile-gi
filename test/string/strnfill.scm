(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (begin
   (let* ((str (strnfill 10 (char->integer #\A))))
     (write str) (newline)
     (string=? str "AAAAAAAAAA"))))
