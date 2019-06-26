(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let ([then (* (- 2018 1970) 365 24 60 60 1000000)])
   (let ([now (get-real-time)])
     (write then) (newline)
     (write now) (newline)
     (and
      (integer? now)
      (> now then)))))
