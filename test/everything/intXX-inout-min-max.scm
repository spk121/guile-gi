(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1)
             (system foreign))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list (- (expt 2 7))
                (- (expt 2 15))
                (- (expt 2 31))
                (- (expt 2 63))
                (- (expt 2 (1- (* 8 (sizeof short)))))
                (- (expt 2 (1- (* 8 (sizeof int)))))
                (- (expt 2 (1- (* 8 (sizeof long)))))))
       (expected (list (1- (expt 2 7))
                       (1- (expt 2 15))
                       (1- (expt 2 31))
                       (1- (expt 2 63))
                       (1- (expt 2 (1- (* 8 (sizeof short)))))
                       (1- (expt 2 (1- (* 8 (sizeof int)))))
                       (1- (expt 2 (1- (* 8 (sizeof long)))))))
       (procs (list int8-inout-min-max
                    int16-inout-min-max
                    int32-inout-min-max
                    int64-inout-min-max
                    short-inout-min-max
                    int-inout-min-max
                    long-inout-min-max)))
   (format #t "Input Before: ~S~%" x)
   (let ((y (map (lambda (proc val)
                   (proc val))
                 procs x)))
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" y)
     (format #t "Expected output: ~S~%" expected)
     (list= = y expected))))

