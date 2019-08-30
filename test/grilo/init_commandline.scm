(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))

(typelib-require ("Grl" "0.3"))

(automake-test
 (let ((cmdline #("command" "--xxx")))
   (format #t "Input Before: ~S~%" cmdline)

   ;; Grilo's init will intercept some arguments like "-h"
   ;; but passes most through unchanged.
   (let ((ret (init cmdline)))
     (format #t "Input after: ~S~%" cmdline)
     (format #t "Output: ~S~%" ret)
     (vector= string=? cmdline ret))))
