(use-modules (gi) (gi repository)
             (test automake-test-lib))

(unless (false-if-exception (require "Gio" "2.0"))
  (exit EXIT_SKIPPED))

(load-by-name "Gio" "Application")

(automake-test
 (begin
   (letrec ((app (make <GApplication>))
            (fired-signals '())
            (fire-signal (lambda (arg)
                           (lambda _
                             (set! fired-signals (cons arg fired-signals))))))
     (for-each (lambda (fn i) (fn app activate (fire-signal i)))
               (list connect connect-after connect)
               '(1 2 3))
     (connect app shutdown (lambda _ (set! fired-signals (reverse fired-signals))))
     (run app '())
     (equal? fired-signals '(1 3 2)))))
