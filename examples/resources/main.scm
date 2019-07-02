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
         (window (send builder (get-object "window")))
         (button1 (send builder (get-object "button1")))
         (button2 (send builder (get-object "button2")))
         (button3 (send builder (get-object "quit"))))
    (connect window (destroy
                       (lambda (object)
                         (gtk::main-quit))))
    (connect button1 (clicked print-hello))
    (connect button2 (clicked print-hello))
    (connect button3 (clicked
                      (lambda (button)
                        (gtk::main-quit)))))

  (gtk::main)
  *unspecified*)

(main)
