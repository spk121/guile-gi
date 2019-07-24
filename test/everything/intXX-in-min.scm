(use-modules (gi) (gi util)
             (test automake-test-lib)
             (srfi srfi-1)
             (system foreign))

(typelib-require (("Marshall" "1.0")
                  #:renamer (protect* '(sizeof short int long size_t))))

(automake-test
 (let ((x (list (- (expt 2 7))
                (- (expt 2 15))
                (- (expt 2 31))
                (- (expt 2 63))
                (- (expt 2 (1- (* 8 (sizeof short)))))
                (- (expt 2 (1- (* 8 (sizeof int)))))
                (- (expt 2 (1- (* 8 (sizeof long)))))))
       (procs (list int8-in-min
                    int16-in-min
                    int32-in-min
                    int64-in-min
                    short-in-min
                    int-in-min
                    long-in-min)))
   (format #t "Input: ~S~%" x)
   (map (lambda (proc val)
          (proc val))
        procs x)))

