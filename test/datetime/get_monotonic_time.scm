(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let ([then (get-monotonic-time)])
   (usleep 1)
   (let ([now (get-monotonic-time)])
     (write then) (newline)
     (write now) (newline)
     (and
      (integer? now)
      (> now then)))))
