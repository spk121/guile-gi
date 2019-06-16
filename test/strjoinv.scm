(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

;;; Hmmm. The procedure should obviously take a list of strings, but,
;;; the introspection looks like it wants just a single string.

(automake-test
 (string=? (strjoinv ":" '("foo" "bar"))
           "foo:bar"))
