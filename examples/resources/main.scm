(use-modules (gi)
             (ice-9 ftw))

(use-typelibs ("Gio" "2.0")
              (("Gtk" "3.0") #:prefix gtk::))

(define (print-hello button)
  (display "Hello World")
  (newline))

(define find-resource
  (compose resource:load
           (lambda (filename)
             (let ((location
                    (ftw (getcwd)
                         (lambda (pathname statinfo flag)
                           (if (string-suffix? filename pathname)
                               pathname
                               #t)))))
               (unless (string? location)
                 (error "Can't find file ~S" filename))
               location))
           (lambda (resource)
             (string-append resource ".gresource"))))

(define (main)
  (gtk::init 0 #f)

  (resources-register (find-resource "test"))

  (let* ((builder (gtk::builder:new-from-resource "/test/builder.ui"))
         (window (with-object builder (get-object "window")))
         (button1 (with-object builder (get-object "button1")))
         (button2 (with-object builder (get-object "button2")))
         (button3 (with-object builder (get-object "quit"))))
    (with-object window
      (connect! destroy
        (lambda (object)
          (gtk::main-quit))))
    (with-object button1 (connect! clicked print-hello))
    (with-object button2 (connect! clicked print-hello))
    (with-object button3
      (connect! clicked
        (lambda (button)
          (gtk::main-quit)))))

  (gtk::main)
  *unspecified*)

(main)
