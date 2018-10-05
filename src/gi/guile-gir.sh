#!/usr/local/bin/guile
!#
;; -*- mode: guile -*-
(use-modules (girepository)
	     (ice-9 textual-ports))
;;(format #t "PRESS RETURN")
;;(get-char (current-input-port))

;;(irepository-require "GIRepository" "2.0")
;;(define infos (irepository-get-infos "GIRepository"))
(irepository-require "Gtk" "3.0")
(define infos (irepository-get-infos "Gtk"))
;; (irepository-require "GLib" "2.0")
;; (define infos (irepository-get-infos "GLib"))
;;(irepository-require "WebKit2" "4.0")
;;(define infos (irepository-get-infos "WebKit2"))

(let loop ((cur (car infos))
	   (rest (cdr infos)))
  (cond
   ((callback-info? cur)
    (format #t "callback ~A~%" (callback-info-get-name cur)))
   ((constant-info? cur)
    (format #t "(define ~A ~S) ; ~A~%" (constant-info-get-name cur) (constant-info-get-value cur) (constant-info-get-type cur)))
   ((enum-info? cur)
    (let ((name (enum-info-get-name cur))
	  (tname (enum-info-get-type-name cur)))
      (for-each (lambda (x)
		  (format #t "(define ~a-~a ~a) ; enum ~A ~%"
			  name (car x) (cdr x) tname))
		(enum-info-get-values cur))))
   ((field-info? cur)
    (format #t "field~%"))
   ((function-info? cur)
    (format #t "function ~a~%" (function-info-get-name cur))
    (when (function-info-is-getter? cur)
      (format #t " ; getter"))
    (when (function-info-is-setter? cur)
      (format #t " ; setter"))
    (when (function-info-is-constructor? cur)
      (format #t " ; constructor"))
    (when (function-info-wraps-vfunc? cur)
      (format #t " ; vfunc-wrapper"))
    (when (function-info-throws? cur)
      (format #t " ; throws"))
    (when (function-info-may-return-null? cur)
      (format #t " may-return-null"))
    (format #t " return-type ~a" (function-info-get-return-type cur))
    (let ((args (function-info-get-args cur)))
      (for-each (lambda (x)
		  (format #t "    ~a ~%" x))
		args))
    (newline))
   ((interface-info? cur)
    (format #t "interface ~A~%" (interface-info-get-name cur)))
   ((object-info? cur)
    (format #t "object ~A~%" (object-info-get-name cur)))
   ((signal-info? cur)
    (format #t "signal~%"))
   ((struct-info? cur)
    (format #t "struct ~A~%" (struct-info-get-name cur))
    (format #t "  alignment ~A~%" (struct-info-get-alignment cur))
    (format #t "  size ~A~%" (struct-info-get-size cur))
    (format #t "  gtype? ~A~%" (struct-info-is-gtype-struct? cur))
    (let ((fields (struct-info-get-fields cur)))
      (for-each (lambda (x)
		  (format #t "   ~a~%" x))
		fields))
    (let ((methods (struct-info-get-methods cur)))
      (for-each (lambda (cur)
		  (format #t "  function ~a~%" (function-info-get-name cur))
		  (when (function-info-is-getter? cur)
		    (format #t " ; getter"))
		  (when (function-info-is-setter? cur)
		    (format #t " ; setter"))
		  (when (function-info-is-constructor? cur)
		    (format #t " ; constructor"))
		  (when (function-info-wraps-vfunc? cur)
		    (format #t " ; vfunc-wrapper"))
		  (when (function-info-throws? cur)
		    (format #t " ; throws"))
		  (when (function-info-may-return-null? cur)
		    (format #t " ; may-return-null"))
		  (format #t " return-type ~a" (function-info-get-return-type cur))
		  (let ((args (function-info-get-args cur)))
		    (for-each (lambda (x)
				(format #t "    ~a ~%" x))
			      args))
		  (newline))
		methods)))
   ((property-info? cur)
    (format #t "property~%"))
   ((union-info? cur)
    (format #t "union ~A~%" (union-info-get-type-name cur)))
   ((value-info? cur)
    (format #t "value~%"))
   ((vfunc-info? cur)
    (format #t "vfunc~%"))
   (else
    (format #t "other ~A~%" cur)))
  (unless (null? rest)
    (loop (car rest) (cdr rest))))
