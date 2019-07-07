(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let* ([date (date:new-dmy 25 12 1990)]
        [day (with-object date (get-day))]
        [month (with-object date (get-month))])
   (write date) (newline)
   (write day) (newline)
   (write month) (newline)
   (and (equal? day 25)
        (equal? month 12))))
