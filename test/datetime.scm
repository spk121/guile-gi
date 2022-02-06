(use-modules (gi) (gi util)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip))))

(define years-since-1970
  (compose
   (lambda (year) (- year 1970))
   (@@ (srfi srfi-19) date-year)
   (@@ (srfi srfi-19) current-date)))

(test-begin "date")

(let ((date (date:new-dmy 25 (number->date-month 12) 1990)))
  (test-assert "new-dmy"
    (and (equal? (date:get-day date) 25)
         (= (date:get-month date) 12)
         (equal? (date:get-year date) 1990)))

  ;; g_date_copy requires GLib 2.56 or greater
  (when (or (> MAJOR_VERSION 2)
            (and (= MAJOR_VERSION 2) (>= MINOR_VERSION 56)))
    (let ((date2 (date:copy date)))
      (test-equal "copy-dates-equal"
        (date:get-day date)
        (date:get-day date2))
      (test-equal "copy-months-equal"
        (date:get-month date)
        (date:get-month date2))
      (test-equal "copy-years-equal"
        (date:get-year date)
        (date:get-year date2))

      (date:clear date2 1)
      (test-assert "clear-invalidates"
        (not (date:valid? date2)))

      (test-assert "clear-original-unaffected"
        (date:valid? date)))))

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
