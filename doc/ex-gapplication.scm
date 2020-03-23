(use-modules (gi))
(use-typelibs ("Gio" "2.0"))
;; Create a new empty <GApplication> using 'make' setting the
;; application-id parameter
(write (make <GApplication> #:application-id "org.test.test"))
(newline)
;; Create a new <GApplication> using a constructor procedure
(define ga (application:new "org.test.test2"
                            (list->application-flags '(handles-open))))
;; Modify the flags parameter of <GApplication> via the set-flags
;; method
(set-flags ga (list->application-flags '(non-unique)))
;; Query if it is running
(write (get-is-busy? ga))
(newline)

