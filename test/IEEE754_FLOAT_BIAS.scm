(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (= IEEE754_FLOAT_BIAS 127))
