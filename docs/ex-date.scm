(use-modules (gi))
(use-typelibs ("GLib" "2.0"))
;; Create a new empty <GDate> using 'make'
(write (make <GDate>))
(newline)
;; Create a new <GDate> using a constructor procedure
(define dt (date:new-dmy 1 (symbol->date-month 'january) 2000))
;; Modify the contents of <GDate>
(add-years dt 1)
;; Compute the resulting year
(write (get-year dt))
(newline)

