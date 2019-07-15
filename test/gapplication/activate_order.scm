(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (begin
   (letrec ((app (make <GApplication>))
            (fired-signals '())
            (fire-signal (lambda (arg)
                           (lambda _
                             (set! fired-signals (cons arg fired-signals))))))
     (with-object app
       (connect! activate
         (fire-signal 1))
       (connect-after! activate
         (fire-signal 2))
       (connect! activate
         (fire-signal 3))
       (connect! shutdown
         (lambda _ (set! fired-signals (reverse fired-signals))))

       (run '()))
     (equal? fired-signals '(1 3 2)))))
