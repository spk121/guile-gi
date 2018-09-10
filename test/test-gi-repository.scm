#!/usr/bin/env sh
exec guile --no-auto-compile -l test-gi-repository.scm -e '(@ (test-gi-repository) main)' -s "$0" "$@"
!#
(define-module (test-gi-repository)
  #:use-module (gi _repository)
  #:export (main))

(define (dummy n)
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
