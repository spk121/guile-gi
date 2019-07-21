(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; The introspection info for g_unicode_canonical_ordering is incorrect.
;; See https://gitlab.gnome.org/GNOME/glib/issues/1840

(automake-test
 ;; (let ((x (apply string (list #\A #\◌́ #\◌̱))))
 ;;   (format #t "Input Before: ~S~%" x)
 ;;   (let ((y (unicode-canonical-ordering x)))
 ;;     (format #t "Input Before: ~S~%" x)
 ;;     (format #t "Output: ~S~%" y)))
 'skipped)

