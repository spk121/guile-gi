(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let* ([date (date:new-dmy 25 (make <%GDateMonth> 'december) 1990)]
        [day (get-day date)]
        [month (get-month date)]
        [date2 (copy date)]
        [day2 (get-day date2)]
        [month2 (get-month date2)])
   (write date) (newline)
   (write day) (newline)
   (write month) (newline)
   (write date2) (newline)
   (write day2) (newline)
   (write month2) (newline)
   (and (equal? day day2)
        (equal? month month2))))
