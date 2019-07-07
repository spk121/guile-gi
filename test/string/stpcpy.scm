(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; stpcpy is unlikely to work.

;; stpcpy returns a char * denoted as TRANSFER_EVERYTHING, and
;; guile-gi converts it to a string and returns it.  However, guile-gi
;; frees returned strings denoted TRANSFER_EVERYTHING, which is
;; usually a good idea.  But in this case, the pointer returned by
;; stpcpy points inside of the input string, and not to the start of
;; an allocated buffer, so trying to free it will fail.

(automake-test
 (begin
   'skipped))
