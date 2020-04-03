(use-modules (gi) (gi repository) (gi util)
             (oop goops)
             (srfi srfi-64))

(test-begin "signals")

(use-typelibs (("GObject" "2.0") #:renamer (protect register-type)))

(test-assert "simple signal with return type NONE"
  (let* ((signalAlpha (make-signal #:name "signal-alpha"
                                   #:return-type G_TYPE_NONE))
         (<ClassAlpha> (register-type "ClassAlpha"
                                      <GObject>
                                      '()
                                      (list signalAlpha)))
         (instance (make <ClassAlpha>))
         (success #f))
    (connect instance signalAlpha
             (lambda (userdata)
               (set! success #t)))
    (signalAlpha instance)
    success))

(test-equal "simple signal with return type INT"
  100
  (let* ((signalBravo (make-signal #:name "signal-bravo"
                                   #:return-type G_TYPE_INT))
         (<ClassBravo> (register-type "ClassBravo"
                                      <GObject>
                                      #f
                                      (list signalBravo)))
         (instance (make <ClassBravo>)))
    (connect instance signalBravo
             (lambda (userdata)
               100))
    (signalBravo instance)))

(test-error "simple signal with return type INT returning wrong type"
  #t
  (let* ((signalCharlie (make-signal #:name "signal-charlie"
                                     #:return-type G_TYPE_INT))
         (<ClassCharlie> (register-type "ClassCharlie"
                                        <GObject>
                                        #f
                                        (list signalCharlie)))
         (instance (make <ClassCharlie>)))
    (connect instance signalCharlie
             (lambda (userdata)
               "hello"))
    (signalCharlie instance)))

(test-error "simple signal with return type INT returning out of range"
  #t
  (let* ((signalDelta (make-signal #:name "signal-delta"
                                   #:return-type G_TYPE_INT))
         (<ClassDelta> (register-type "ClassDelta"
                                      <GObject>
                                      #f
                                      (list signalDelta)))
         (instance (make <ClassDelta>)))
    (connect instance signalDelta
             (lambda (userdata)
               (expt 2 65)))
    (signalDelta instance)))

(test-error "simple signal with return type unspecified should error"
  #t
  (let* ((signalEcho (make-signal #:name "signal-echo"))
         (<ClassEcho> (register-type "ClassEcho"
                                     <GObject>
                                     '()
                                     (list signalEcho)))
         (instance (make <ClassEcho>)))
    (connect instance signalEcho
             (lambda (userdata) 1))
    (signalEcho instance)))

(test-error "simple signal with wrong argument arity"
  #t
  (let* ((signalFoxtrot (make-signal #:name "signal-foxtrot"
                                     #:return-type G_TYPE_INT))
         (<ClassFoxtrot> (register-type "ClassFoxtrot"
                                        <GObject>
                                        #f
                                        (list signalFoxtrot)))
         (instance (make <ClassFoxtrot>)))
    (connect instance signalFoxtrot
             (lambda (userdata dummy)
               100))
    (signalFoxtrot instance)))

(test-equal "simple signal with return type INT with accumulator return value unspecified"
  100
  (let* ((accum-value #f)
         (accumulatorGolf (lambda (seed val)
                            (set! accum-value val)))
         (signalGolf (make-signal #:name "signal-golf"
                                  #:return-type G_TYPE_INT
                                  #:accumulator accumulatorGolf))
         (<ClassGolf> (register-type "ClassGolf"
                                     <GObject>
                                     #f
                                     (list signalGolf)))
         (instance (make <ClassGolf>)))
    (connect instance signalGolf
             (lambda (userdata)
               100))
    (signalGolf instance)
    accum-value))

(test-equal "simple signal with return type INT with accumulator return type INT"
  '(100 101)
  (let* ((accum-value #f)
         (accumulatorHotel (lambda (seed val)
                             (set! accum-value val)
                             (1+ val)))
         (signalHotel (make-signal #:name "signal-hotel"
                                   #:return-type G_TYPE_INT
                                   #:accumulator accumulatorHotel))
         (<ClassHotel> (register-type "ClassHotel"
                                      <GObject>
                                      #f
                                      (list signalHotel)))
         (instance (make <ClassHotel>)))
    (connect instance signalHotel
             (lambda (userdata)
               100))
    (let ((ret (signalHotel instance)))
      (list accum-value ret))))

(test-equal "simple signal with return type INT with accumulator that doesn't abort signal"
  '(3 300 301)
  (let* ((accumulator-value #f)
         (accumulatorIndia (lambda (seed val)
                             (set! accumulator-value val)
                             (values #t ; don't abort signal emission
                                     (1+ val) ; modify signal return value
                                     )))
         (signalIndia (make-signal #:name "signal-india"
                                   #:return-type G_TYPE_INT
                                   #:accumulator accumulatorIndia))
         (<ClassIndia> (register-type "ClassIndia"
                                      <GObject>
                                      #f
                                      (list signalIndia)))
         (instance (make <ClassIndia>))
         (called 0))
    (connect instance signalIndia
             (lambda (userdata)
               (set! called (1+ called))
               100
               ))
    (connect instance signalIndia
             (lambda (userdata)
               (set! called (1+ called))
               200
               ))
    (connect instance signalIndia
             (lambda (userdata)
               (set! called (1+ called))
               300
               ))
    (let ((ret (signalIndia instance)))
      (list
       called
       accumulator-value
       ret))))

(test-equal "simple signal with return type INT with accumulator that aborts signal"
  '(1 100 101)
  (let* ((accumulator-value #f)
         (accumulatorJuliette (lambda (seed val)
                                (set! accumulator-value val)
                                (values #f ; abort signal emission
                                        (1+ val) ; modify signal return value
                                        )))
         (signalJuliette (make-signal #:name "signal-juliette"
                                      #:return-type G_TYPE_INT
                                      #:accumulator accumulatorJuliette))
         (<ClassJuliette> (register-type "ClassJuliette"
                                         <GObject>
                                         #f
                                         (list signalJuliette)))
         (instance (make <ClassJuliette>))
         (called 0))
    (connect instance signalJuliette
             (lambda (userdata)
               (set! called (1+ called))
               100
               ))
    (connect instance signalJuliette
             (lambda (userdata)
               (set! called (1+ called))
               200
               ))
    (connect instance signalJuliette
             (lambda (userdata)
               (set! called (1+ called))
               300
               ))
    (let ((ret (signalJuliette instance)))
      (list
       called
       accumulator-value
       ret))))

(test-error "simple signal with return type INT with accumulator that returns too many values"
  #t
  (let* ((accum-value #f)
         (accumulatorKilo (lambda (seed val)
                            (set! accum-value val)
                            ;; Returning too many values
                            (values #f seed val)))
         (signalKilo (make-signal #:name "signal-kilo"
                                  #:return-type G_TYPE_INT
                                  #:accumulator accumulatorKilo))
         (<ClassKilo> (register-type "ClassKilo"
                                     <GObject>
                                     #f
                                     (list signalKilo)))
         (instance (make <ClassKilo>)))
    (connect instance signalKilo
             (lambda (userdata)
               100))
    (signalKilo instance)
    accum-value))

(test-equal "simple signal with return type INT using 'first-wins accumulator"
  '(1 100)
  (let* ((signalLima (make-signal #:name "signal-lima"
                                  #:return-type G_TYPE_INT
                                  #:accumulator 'first-wins))
         (<ClassLima> (register-type "ClassLima"
                                     <GObject>
                                     #f
                                     (list signalLima)))
         (instance (make <ClassLima>))
         (called 0))
    (connect instance signalLima
             (lambda (userdata)
               (set! called (1+ called))
               100
               ))
    (connect instance signalLima
             (lambda (userdata)
               (set! called (1+ called))
               200
               ))
    (connect instance signalLima
             (lambda (userdata)
               (set! called (1+ called))
               300
               ))
    (let ((ret (signalLima instance)))
      (list
       called
       ret))))

(test-equal "simple signal with return type BOOL using 'true-handled accumulator"
  '(2 #t)
  (let* ((signalMike (make-signal #:name "signal-mike"
                                  #:return-type G_TYPE_BOOLEAN
                                  #:accumulator 'true-handled))
         (<ClassMike> (register-type "ClassMike"
                                     <GObject>
                                     #f
                                     (list signalMike)))
         (instance (make <ClassMike>))
         (called 0))
    (connect instance signalMike
             (lambda (userdata)
               (set! called (1+ called))
               #f
               ))
    (connect instance signalMike
             (lambda (userdata)
               (set! called (1+ called))
               #t
               ))
    (connect instance signalMike
             (lambda (userdata)
               (set! called (1+ called))
               #f
               ))
    (let ((ret (signalMike instance)))
      (list
       called
       ret))))

(test-error "simple signal with return type INT using 'true-handled accumulator which requires return type BOOL"
  #t
  (let* ((signalNovember (make-signal #:name "signal-november"
                                      #:return-type G_TYPE_INT
                                      #:accumulator 'true-handled))
         (<ClassNovember> (register-type "ClassNovember"
                                         <GObject>
                                         #f
                                         (list signalNovember)))
         (instance (make <ClassNovember>))
         (called 0))
    (connect instance signalNovember
             (lambda (userdata)
               (set! called (1+ called))
               0
               ))
    (connect instance signalNovember
             (lambda (userdata)
               (set! called (1+ called))
               1
               ))
    (connect instance signalNovember
             (lambda (userdata)
               (set! called (1+ called))
               0
               ))
    (let ((ret (signalNovember instance)))
      (list
       called
       ret))))

(test-equal "output signal"
  '(1 #t)
  (let* ((signalOscar (make-signal #:name "signal-oscar"
                                   #:return-type G_TYPE_NONE
                                   #:param-types (list G_TYPE_INT G_TYPE_BOOLEAN)
                                   #:output-mask #*011))
         (<ClassOscar> (register-type "ClassOscar"
                                      <GObject>
                                      #f
                                      (list signalOscar)))
         (instance (make <ClassOscar>)))
    (connect-after instance signalOscar
                   (lambda (obj a b)
                     (values 1 #t)))
    (call-with-values (lambda () (signalOscar instance 0 #f)) list)))


(test-end "signals")
