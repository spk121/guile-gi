(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

;; 01Aug19 - This doesn't work.  The unit test modifies the GArray
;; in place, but, the GArray here is just a temp variable that
;; gets created from the vector. For caller-allocates, out GArray
;; arguments, there needs to be an additional step that modifies
;; the input vector.

(automake-test
 (let ((x #("A" "B" "C")))
   (format #t "Input before: ~S~%" x)
   (garray-utf8-full-out-caller-allocated x)
     (format #t "Input after: ~S~%" x)
     (list= string=? '("0" "1" "2") (vector->list x)))))


