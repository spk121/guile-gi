(use-modules (srfi srfi-64))

(define (my-simple-runner)
  (let ((runner (test-runner-simple))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (+ num-passed 1)))
          ((fail xfail) (set! num-failed (+ num-failed 1)))
          (else #t))))
    (test-runner-on-final! runner
       (lambda (runner)
          (format #t "Passing tests: ~a.~%Failing tests: ~a.~%"
                  num-passed num-failed)
	  (if (> num-failed 0)
	      (exit 1)
	      (exit 0))))
    runner))

(test-runner-factory
 my-simple-runner)
