

(use-modules (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(setlocale LC_ALL "")

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
