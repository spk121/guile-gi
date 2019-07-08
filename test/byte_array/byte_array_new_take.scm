(use-modules (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

;; The 'new-take' is supposed to own the data
;; passed in to it.  That functionality probably
;; won't work.

(automake-test
 (begin
   (let* ((bv (make-bytevector 4 32))
          (bv2 (byte-array:new-take bv)))
     (format #t "Input bytevector: ~S~%" bv)
     (format #t "Output: ~S~%" bv2)
     (equal? bv bv2))))
