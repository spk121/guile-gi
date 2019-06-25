(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let* ([date (Date-new-dmy 25 12 1990)]
        [day (send date (get-day))]
        [month (send date (get-month))]
        [date2 (send date (copy))]
        [day2 (send date2 (get-day))]
        [month2 (send date2 (get-month))])
   (write date) (newline)
   (write day) (newline)
   (write month) (newline)
   (write date2) (newline)
   (write day2) (newline)
   (write month2) (newline)
   (and (equal? day day2)
        (equal? month month2))))
