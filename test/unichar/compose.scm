(use-modules (gi)
             (ice-9 receive)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; This doesn't work because the 3rd arg of g_unichar_compose
;; isn't registered as OUT.  See
;; https://gitlab.gnome.org/GNOME/glib/issues/1811

(automake-test
 ;; (receive (success out)
 ;;     (unichar-compose #\A #\◌́)
 ;;   (format #t "Output: ~S~%" x)
 ;;   (format #t "Success: ~S~%" success)
 ;;   (and success
 ;;        (char=? ch Á)))
 'skipped)
