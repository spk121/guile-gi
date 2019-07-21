(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

;; Returns a nullalble NULL pointer, thus #f.

(automake-test
 (let ((ptr (const-return-gpointer)))
   (write ptr) (newline)
   (not ptr)))
