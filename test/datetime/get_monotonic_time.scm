(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let ([then (get-monotonic-time)])
   (usleep 1)
   (let ([now (get-monotonic-time)])
     (write then) (newline)
     (write now) (newline)
     (and
      (integer? now)
      (> now then)))))
