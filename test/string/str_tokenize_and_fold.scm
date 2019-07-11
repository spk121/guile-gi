(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib)
             (ice-9 receive)
             (srfi srfi-43))

(typelib-require ("GLib" "2.0"))

;; FIXME: the ascii-alternates output parameter
;; is from a gchar*** output parameter.  This is
;; probably not properly handled.

;; gchar **
;; g_str_tokenize_and_fold (const gchar *string,
;;                         const gchar *translit_locale,
;;                         gchar ***ascii_alternates);

(automake-test
 (let ((str "Les pâtes")
       (locale "fr_FR"))
   (receive (tokens ascii-alternates)
       (str-tokenize-and-fold str locale)
     (format #t "String: ~S~%" str)
     (format #t "Locale: ~S~%" locale)
     (format #t "Tokens: ~S~%" tokens)
     (format #t "ASCII alternates: ~S~%" ascii-alternates)
     ;; take case-folding into account
     (vector= string-ci=? #("Les" "pâtes") tokens))))
