(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

;; FIXME: the ascii-alternates output parameter
;; is from a gchar*** output parameter.  This is
;; probably not properly handled.

;; gchar **
;; g_str_tokenize_and_fold (const gchar *string,
;;                         const gchar *translit_locale,
;;                         gchar ***ascii_alternates);

(automake-test
 (let ((out (str-tokenize-and-fold "Les pâtes" "fr_FR")))
   (let ((tokens (car out))
         (ascii-alternates (cadr out)))
     (write tokens) (newline)
     (write ascii-alternates) (newline)
     (equal? tokens (list "Les" "pâtes")))))

