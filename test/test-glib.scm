(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign))

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
      "glib"
    
    (pass-if "load GLib 2.0 repository"
      (gi-load-repository "GLib" "2.0")
      #t)
    
    (with-test-prefix "Version Information"
      (pass-if "MAJOR_VERSION is 2"
	(equal? (gi-constant-value "GLib" "MAJOR_VERSION") 2))

      (pass-if "MINOR_VERSION is integer"
	(integer? (gi-constant-value "GLib" "MINOR_VERSION")))

      (pass-if "MICRO_VERSION is integer"
	(integer? (gi-constant-value "GLib" "MICRO_VERSION")))

      (pass-if "VERSION_MIN_REQURED is 2"
	(equal? (gi-constant-value "GLib" "VERSION_MIN_REQUIRED") 2))

      (pass-if "check-version"
	(string-null? (gi-function-invoke "GLib" "check_version"
					  (gi-constant-value "GLib" "MAJOR_VERSION")
					  (gi-constant-value "GLib" "MINOR_VERSION")
					  (gi-constant-value "GLib" "MICRO_VERSION")))))
	
    (with-test-prefix "Basic Types"
      (pass-if "MININT8 is -128"
	(equal? (gi-constant-value "GLib" "MININT8") -128))

      (pass-if "MAXINT8 is 127"
	(equal? (gi-constant-value "GLib" "MAXINT8") 127))

      (pass-if "GINT16_FORMAT is 'hi'"
	(string=? "hi" (gi-constant-value "GLib" "GINT16_FORMAT"))))

    (with-test-prefix "Standard Macros"
      (pass-if "DIR_SEPARATOR is (int) '/'"
	(equal? (char->integer #\/) (gi-constant-value "GLib" "DIR_SEPARATOR")))

      (pass-if "DIR_SEPARATOR_S is \"/\""
	(equal? "/" (gi-constant-value "GLib" "DIR_SEPARATOR_S"))))

    (with-test-prefix "Numerical Definitions"
      (pass-if "PI is about 3.14"
	(and (> (gi-constant-value "GLib" "PI") 3.14)
	     (< (gi-constant-value "GLib" "PI") 3.15))))

    (with-test-prefix "Atomic Operations"
      (pass-if "atomic_int_set"
	(let ((bv (make-bytevector 8 ; bytes
				   0 ; value
				   )))
	  (gi-function-invoke "GLib" "atomic_int_set" bv 1)
	  (equal? (bytevector-s32-native-ref bv 0) 1)))

      (pass-if "atomic_int_get"
	       (let ((bv (make-bytevector 8 ; bytes
					  0 ; value
					  )))
		 (bytevector-s32-native-set! bv 0 1234)
		 (equal? 1234 (gi-function-invoke "GLib" "atomic_int_get" bv))))

      (pass-if "atomic_pointer_set"
	       (let ((bv (make-bytevector 8 ;bytes
					  0 ; value
					  ))
		     (ptr (make-pointer #xF00FF00F)))
		 (gi-function-invoke "GLib" "atomic_pointer_set" bv ptr)
		 (equal? #xF00FF00F (bytevector-u64-native-ref bv 0))))

      (pass-if "atomic_pointer_add"
	       (let ((bv (make-bytevector 8 ;bytes
					  0 ; value
					  ))
		     (ptr (make-pointer #xF00FF00F)))
		 (gi-function-invoke "GLib" "atomic_pointer_set" bv ptr)
		 (gi-function-invoke "GLib" "atomic_pointer_add" bv #x0FF0)
		 (equal? #xF00FFFFF (bytevector-u64-native-ref bv 0)))))

    (with-test-prefix "Main Event Loop"
		      (pass-if "MainLoop-new"
			       (let ((mainloop (gi-function-invoke "GLib" "MainLoop-new" #f #t)))
				 #t)))

    (pass-if "unload repositories"
      (gi-unload-repositories)
      #t))
  
  (print-counts (results-proc))
  (exit-value (results-proc)))
