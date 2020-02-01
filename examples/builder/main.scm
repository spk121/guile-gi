(use-modules (ice-9 rdelim)
             (ice-9 ftw)
             (gi)
             (gi repository))

(eval-when (compile load eval)
  (require "Gtk" "3.0")

  (for-each (lambda (n) (load-by-name "Gtk" n))
            '("Builder" "Widget" "Button" "init" "main_quit" "main")))

(define (print-hello button)
  (display "Hello World")
  (newline))

(define (find-file filename)
  "Search for a file in the current directory."
  (let ((location
         (ftw (getcwd)
              (lambda (pathname statinfo flag)
                (if (string-suffix? filename pathname)
                    pathname
                    #t)))))
    (unless (string? location)
      (error "Can't find file ~S" filename))
    location))

(define (go)
  ;; Initialize GTK
  (init!)

  ;; Construct a GtkBuilder instance and load our UI description.
  (let ((builder (builder:new-from-file (find-file "builder.ui"))))

    ;; Connect the widgets to their signals and callbacks.
    (let ((window (get-object builder "window"))
          (button1 (get-object builder "button1"))
          (button2 (get-object builder "button2"))
          (button3 (get-object builder "quit")))
      (connect window destroy
               (lambda (object)
                 (main-quit)))
      (connect button1 clicked print-hello)
      (connect button2 clicked print-hello)
      (connect button3 clicked
               (lambda (button)
                 (main-quit)))))

  ;; Let's demonstrate that the signals and callback survive
  ;; garbage collection.
  (gc) (gc) (gc)

  ;; Run the main loop
  (main)
  #t)

(go)
