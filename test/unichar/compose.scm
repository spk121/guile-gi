(use-modules (gi)
             (rnrs bytevectors)
             (system foreign)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; This is very clunky. There ought to be a better way to handle
;; output unichar.

;; Also, it doesn't work because the 3rd arg of g_unichar_compose
;; isn't registered as OUT.  See
;; https://gitlab.gnome.org/GNOME/glib/issues/1811

(automake-test
 (let* ((bv (make-bytevector 8 0))
        ;; (success (unichar-compose? #\A #\◌́ bv))
        ;; (ch (integer->char (bytevector-u64-ref bv 0)))
        )
   (write bv) (newline)
   ;; (write success) (newline)
   ;; (write ch) (newline)
   ;; (and success
   ;;      (char=? ch Á)
   'skipped))
