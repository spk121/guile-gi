(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

;; This won't work.  It violates our general understanding
;; that C char * return values marked as TRANSFER_EVERYTHING
;; should be
;; 1. used to create new Guile strings
;; 2. freed

;; They can't be freed because the pointer points into the middle of a
;; C string.

(automake-test
 ;; (let ((out (strstr-len "hello, world" -1
 ;;                       "l")))
 ;;  (write out) (newline)
   'skipped))

