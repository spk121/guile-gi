(use-modules (gi)
             (test automake-test-lib))

;; documentation is unavailable in this build
(exit EXIT_SKIPPED)

(automake-test
 (let ((doc (typelib-document "GLib" "2.0")))
   (display doc)
   (string? doc)))
