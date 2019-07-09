(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(format #t "Will try to run timed callback 5 times...\n")

(automake-test
 (let ((mainloop (main-loop:new #f #t))
       (n 1))
   (let ((source-id (timeout-add PRIORITY_DEFAULT
                                 100                    ; milliseconds

                                 ;; This callback should run 5 times.
                                 (lambda (x)
                                   (format #t "Iteration ~a~%" n)
                                   (if (= n 5)
                                       ;; FALSE deletes this callback.
                                       #f
                                       ;; else
                                       (begin
                                         (set! n (1+ n))
                                         ;; TRUE continues this callback.
                                         #t)))

                                 #f

                                 ;; This callback should run after the
                                 ;; 5th time.
                                 (lambda (x)
                                   (display "Complete")
                                   (newline)
                                   (with-object mainloop (quit))
                                   #f))))
     (with-object mainloop (run))
     (format #t "Callback ran ~a times~%" n)
     (= n 5))))
