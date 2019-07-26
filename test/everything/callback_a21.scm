(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((success #f)
       (error #f))
   (callback-a21 (lambda (a b c d e f g h i j k l m n o p q r s t u)
                   (set! success #t)))

   (callback-a21 (lambda (a b c d e f g h i j k l m n o p q r s t u v)
                   (set! error #t)))
   (and success (not error))))
