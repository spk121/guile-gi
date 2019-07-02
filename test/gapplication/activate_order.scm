(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (letrec ((app (create <GApplication>))
            (fired-signals '())
            (fire-signal (lambda (arg)
                           (lambda _
                             (set! fired-signals (cons arg fired-signals))))))
     (modify-signals app
       (connect activate
         (fire-signal 1))
       (connect-after activate
         (fire-signal 2))
       (connect activate
         (fire-signal 3))
       (connect shutdown
         (lambda _ (set! fired-signals (reverse fired-signals)))))

     (with-object app (run 0 '()))
     (equal? fired-signals '(1 3 2)))))
