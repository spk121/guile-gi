(use-modules (gi) (gi util)
             (ice-9 ftw))

(push-duplicate-handler! 'shrug-equals)

(use-typelibs
 ("Gio" "2.0")
 (("Gtk" "3.0")
  #:renamer (protect* (cons* 'init 'main 'main-quit %rnrs-syntax) 'gtk::)))

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
  (gtk::init)

  (resources-register (find-resource "test"))

  (let* ((builder (builder:new-from-resource "/test/builder.ui"))
         (window (get-object builder "window"))
         (button1 (get-object builder "button1"))
         (button2 (get-object builder "button2"))
         (button3 (get-object builder "quit")))
    (connect window destroy
             (lambda (object)
               (gtk::main-quit)))
    (connect button1 clicked print-hello)
    (connect button2 clicked print-hello)
    (connect button3 clicked
             (lambda (button)
               (gtk::main-quit))))

  (gtk::main)
  *unspecified*)

(main)
