(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;;; Hmmm. The procedure should obviously take a list of strings, but,
;;; the introspection looks like it wants just a single string.

(automake-test
 (string=? (strjoinv ":" '("foo" "bar"))
           "foo:bar"))
