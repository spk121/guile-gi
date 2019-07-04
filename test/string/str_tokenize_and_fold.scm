(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib)
             (ice-9 receive)
             (srfi srfi-43))

;; FIXME: the ascii-alternates output parameter
;; is from a gchar*** output parameter.  This is
;; probably not properly handled.

;; gchar **
;; g_str_tokenize_and_fold (const gchar *string,
;;                         const gchar *translit_locale,
;;                         gchar ***ascii_alternates);

(automake-test
 (receive (tokens ascii-alternates)
     (str-tokenize-and-fold "Les pâtes" "fr_FR")
   (write tokens) (newline)
     (write ascii-alternates) (newline)
     ;; take case-folding into account
     (vector= string-ci=? #("Les" "pâtes") tokens)))
