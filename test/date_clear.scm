(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let* ([date (Date-new-dmy 25 12 1990)]
        [date2 (send date (copy))])

   ;; Clear one GDate starting at the memory location in date2.  This
   ;; API is not great, since creating more than one contiguous
   ;; <GDate> isn't trivial using the introspected functions.
   (send date2 (clear 1))

   (write date) (newline)
   (write date2) (newline)

   (not (send date2 (valid?)))))
