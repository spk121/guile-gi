
(define TYPE_POOP
  (register-type "poop" TYPE_OBJECT
		 `(("height" ,TYPE_INT "Height" "how tall r u"
		    0 300 185 ,(logior PARAM_READWRITE PARAM_CONSTRUCT)))
		 '() #f))

(define poop (make-gobject TYPE_POOP '(("height" . 200))))
