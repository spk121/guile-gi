(use-modules (lib)
	     (rnrs bytevectors)
	     (system foreign)
	     (ice-9 eval-string)
	     (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt)))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "boolean"
   (pass-if "pack and unpack #t"
	    (let* ((arg (make-giargument GI_TYPE_TAG_BOOLEAN #t))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_BOOLEAN)))
	      (equal? #t val)))
   
   (pass-if "pack and unpack #f"
	    (let* ((arg (make-giargument GI_TYPE_TAG_BOOLEAN #f))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_BOOLEAN)))
	      (equal? #f val))))

  (with-test-prefix
   "integers"
   (pass-if "pack and unpack (int8) 0"
	    (let* ((arg (make-giargument GI_TYPE_TAG_INT8 0))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_INT8)))
	      (equal? val 0)))

   (pass-if "pack and unpack (int8) 127"
	    (let* ((arg (make-giargument GI_TYPE_TAG_INT8 127))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_INT8)))
	      (equal? val 127)))

   (pass-if "pack and unpack (int8) -127"
	    (let* ((arg (make-giargument GI_TYPE_TAG_INT8 -127))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_INT8)))
	      (equal? val -127)))

   (pass-if "pack and unpack (uint8) 0"
	    (let* ((arg (make-giargument GI_TYPE_TAG_UINT8 0))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_UINT8)))
	      (equal? val 0)))
   
   (pass-if "pack and unpack (uint8) 255"
	    (let* ((arg (make-giargument GI_TYPE_TAG_UINT8 255))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_UINT8)))
	      (equal? val 255)))
   )
  
  (with-test-prefix
   "real numbers"

   (pass-if "pack and unpack (float) 0.0"
	    (let* ((arg (make-giargument GI_TYPE_TAG_FLOAT 0.0))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_FLOAT)))
	      (< (abs (- 0.0 val)) 1e-5)))

   (pass-if "pack and unpack (float) 1.0"
	    (let* ((arg (make-giargument GI_TYPE_TAG_FLOAT 1.0))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_FLOAT)))
	      (< (abs (- 1.0 val)) 1e-5)))
   
   )

  (with-test-prefix
   "utf8 strings"
   (pass-if "pack and unpack \"hello\""
	    (let* ((arg (make-giargument GI_TYPE_TAG_UTF8 "hello"))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_UTF8)))
	      (string=? "hello" val)))

   (pass-if "pack-and-unpack '時間英語チャンネル'"
	    (let* ((str "時間英語チャンネル")
		   (arg (make-giargument GI_TYPE_TAG_UTF8 str))
		   (val (convert-giargument-basic-type-to-object
			 arg
			 GI_TYPE_TAG_UTF8)))
	      (string=? str val)))
   )

  (with-test-prefix
   "void pointer"
   (pass-if "pack and unpack void *"
	    (let* ((bv (make-bytevector 10))
		   (ptr (bytevector->pointer bg))
		   (arg (make-giargument GI_TYPE_TAG_VOID
  (print-counts (results-proc))
  (exit-value (results-proc)))
