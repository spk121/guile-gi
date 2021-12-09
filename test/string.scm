(use-modules (gi) (gi util)
             (ice-9 receive)
             (srfi srfi-43)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip))))
(test-begin "strings")

(let* ((str1 "hello")
       (str2 (strdup str1))
       (str3 (strndup str1 4)))
  (test-equal "strdup-equal" str1 str2)
  (test-assert "strdup-not-eq" (not (eq? str1 str2)))
  (test-equal "strndup" "hell" str3))

(test-skip "stpcpy")
(test-assert "stpcpy" #f)
;; This won't work.  It violates our general understanding
;; that C char * return values marked as TRANSFER_EVERYTHING
;; should be
;; 1. used to create new Guile strings
;; 2. freed

;; They can't be freed because the pointer points into the middle of a
;; C string.
(test-skip "strstr-len")
(test-equal "strstr-len"
  "llo, world"
  (strstr-len "hello, world" -1 "l"))

(test-assert "has-prefix"
  (str-has-prefix? "ABCDEFG" "ABC"))

(receive (tokens ascii-alternates)
    (str-tokenize-and-fold "Les pâtes françaises" "fr_FR")
  (test-assert
      "tokenize-and-fold tokens"
    (vector= string-ci=? #("Les" "pâtes" "françaises") tokens))
  (test-assert
      "tokenize-and-fold ascii-alternates"
    (vector= string-ci=? #("pates" "francaises") ascii-alternates)))

(test-end "strings")

(test-begin "unichar")

(test-eqv "unichar-combine-none"
  0
  (unichar-combining-class #\と))

(test-eqv "unichar-combine-above"
  230
  (unichar-combining-class #\x0300)) ;; ◌̀

;; This doesn't work because the 3rd arg of g_unichar_compose
;; isn't registered as OUT.  See
;; https://gitlab.gnome.org/GNOME/glib/issues/1811
(test-skip "unichar-compose")
(test-assert "unichar-compose"
  ;; (receive (success out)
  ;;     (unichar-compose #\A #\◌́)
  ;;   (format #t "Output: ~S~%" x)
  ;;   (format #t "Success: ~S~%" success)
  ;;   (and success
  ;;        (char=? ch Á)))
  #f)

(test-assert "unichar-isalpha"
  (unichar-isalpha? #\と))

(test-assert "unichar-validate-zero"
  (unichar-validate? #\x0000))

(test-end "unichar")

(unless (< MINOR_VERSION 56)
  (test-begin "conversions")

  (test-equal "convert-latin-1"
    #u8(195 129 195 137)
    (convert #u8(193 201) "UTF-8" "ISO-8859-1"))

  (test-equal "convert-ascii"
    #u8(65 66 67 68 69)
    (convert #u8(65 66 67 68 69) "UTF-8" "US-ASCII"))

  (test-end "conversions"))
