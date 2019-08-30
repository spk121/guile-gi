(use-modules (gi) (gi util)
             (test automake-test-lib)
             (srfi srfi-1)
             (system foreign))

(typelib-require (("Marshall" "1.0")
                  #:renamer (protect* '(sizeof short int long size_t))))

(automake-test
 (let ((x (list (1- (expt 2 7))
                (1- (expt 2 15))
                (1- (expt 2 31))
                (1- (expt 2 63))
                (1- (expt 2 (1- (* 8 (sizeof short)))))
                (1- (expt 2 (1- (* 8 (sizeof int)))))
                (1- (expt 2 (1- (* 8 (sizeof long)))))))
       (procs (list int8-in-max
                    int16-in-max
                    int32-in-max
                    int64-in-max
                    short-in-max
                    int-in-max
                    long-in-max)))
   (format #t "Input: ~S~%" x)
   (map (lambda (proc val)
          (proc val))
        procs x)))
