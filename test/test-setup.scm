(define (show-environment n)
  (format #t "PWD = ~S~%" (system "pwd"))
  (format #t "LD_LIBRARY_PATH = ~S~%" (getenv "LD_LIBRARY_PATH"))
  (format #t "GUILE_AUTO_COMPILE = ~S~%" (getenv "GUILE_AUTO_COMPILE"))
  (format #t "ok ~A - show-environment ~%" n))

(define TESTS
  (list
   show-environment))

(define (main . args)
  (let ((n-tests (length TESTS)))
    (format #t "1..~A~%" n-tests)
    (do ((i 1 (1+ i)))
	((> i n-tests))
      (let ((test (list-ref TESTS (1- i))))
	(test i)))))

(main)
;; Local Variables:
;; mode: scheme
;; End:
