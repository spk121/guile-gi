(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let* ([date (date:new-dmy 25 12 1990)]
        [date2 (with-object date (copy))])

   ;; Clear one GDate starting at the memory location in date2.  This
   ;; API is not great, since creating more than one contiguous
   ;; <GDate> isn't trivial using the introspected functions.
   (with-object date2 (clear 1))

   (write date) (newline)
   (write date2) (newline)

   (not (with-object date2 (valid?)))))
