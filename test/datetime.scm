(use-modules (gi) (gi util)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal
                                     test-assert
                                     test-skip))))

(define years-since-1970
  (compose
   (lambda (year) (- year 1970))
   (@@ (srfi srfi-19) date-year)
   (@@ (srfi srfi-19) current-date)))

(test-begin "date")

(let ((date (date:new-dmy 25 (number->date-month 12) 1990)))
  (test-assert "new-dmy"
    (and (equal? (get-day date) 25)
         (= (get-month date) 12)
         (equal? (get-year date) 1990)))

  (let ((date2 (copy date)))
    (test-equal "copy-dates-equal"
      (get-day date)
      (get-day date2))
    (test-equal "copy-months-equal"
      (get-month date)
      (get-month date2))
    (test-equal "copy-years-equal"
      (get-year date)
      (get-year date2))

    (clear date2 1)
    (test-assert "clear-invalidates"
      (not (valid? date2)))
    
    (test-assert "clear-original-unaffected"
      (valid? date))))

(test-end "date")

(test-begin "time")

(let* ((then (get-monotonic-time))
       (now (begin
              (usleep 1)
              (get-monotonic-time)))
       (years-since-1970 (years-since-1970)))
  (test-assert "monotonic-time"
    (> now then))

  (test-assert "real-time"
    (> (get-real-time)
       (* years-since-1970 365 24 60 60 1000000))))

(test-end "time")
