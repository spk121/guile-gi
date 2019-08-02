(use-modules (ice-9 rdelim)
             (ice-9 ftw)
             (gi)
             (gi repository))

(require "Gio" "2.0")
(require "Gtk" "3.0")

(load-by-name "Gio" "Resource")
(load-by-name "Gio" "resources_register")

(map (lambda (n) (load-by-name "Gtk" n))
     '("Builder" "Widget" "Button"))

;; avoid name conflicts and confusion by loading functions in another module
(save-module-excursion
 (lambda ()
   (let ((previous-module (set-current-module (make-module))))
     (load-by-name "Gtk" "init")
     (load-by-name "Gtk" "main")
     (load-by-name "Gtk" "main_quit")
     (module-for-each (lambda (sym var)
                        (module-define! previous-module
                                        (symbol-append 'gtk:: sym)
                                        (variable-ref var)))
                      (current-module)))))

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
