(use-modules (gi) (gi util)
             (ice-9 receive)
             (srfi srfi-43)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal
                                     test-assert
                                     test-skip))))
(test-begin "string.scm")

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

(test-end "string.scm")
