(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let* ([date (Date-new-dmy 25 12 1990)]
        [day (send date (get-day))]
        [month (send date (get-month))])
   (write date) (newline)
   (write day) (newline)
   (write month) (newline)
   (and (equal? day 25)
        (equal? month 12))))
