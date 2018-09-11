#!/usr/bin/env sh
exec guile --no-auto-compile -l test-setup.scm -e '(@ (test-setup) main)' -s "$0" "$@"
!#
(define-module (test-setup)
  #:use-module (gi)
  #:export (main))

(define (t-require-version n)
  (require-version "Girtest" "1.0")
  (format #t "ok ~A - dummy test~%" n))

(define TESTS
  (list
   dummy))

(define (main . args)
  (let ((n-tests (length TESTS)))
    (format #t "1..~A~%" n-tests)
    (do ((i 1 (1+ i)))
	((> i n-tests))
      (let ((test (list-ref TESTS (1- i))))
	(test i)))))

;; Local Variables:
;; mode: scheme
;; End:
