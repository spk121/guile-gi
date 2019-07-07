(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (begin
   (let* ((str1 "hello")
          (str2 (strndup "hello" 4)))
     (write (%string-dump str1)) (newline)
     (write (%string-dump str2)) (newline)
     (and
      (equal? str1 "hello")
      (equal? str2 "hell"))))))
