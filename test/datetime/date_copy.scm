(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let* ([date (date:new-dmy 25 12 1990)]
        [day (with-object date (get-day))]
        [month (with-object date (get-month))]
        [date2 (with-object date (copy))]
        [day2 (with-object date2 (get-day))]
        [month2 (with-object date2 (get-month))])
   (write date) (newline)
   (write day) (newline)
   (write month) (newline)
   (write date2) (newline)
   (write day2) (newline)
   (write month2) (newline)
   (and (equal? day day2)
        (equal? month month2))))
