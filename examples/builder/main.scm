(use-modules (ice-9 rdelim)
             (ice-9 ftw)
             (gi)
             (gi gtk-3))

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
  (init 0 #f)

  ;; Construct a GtkBuilder instance and load our UI description.
  (let ((builder (builder:new-from-file (find-file "builder.ui"))))

    ;; Connect the widgets to their signals and callbacks.
    (let ((window (send builder (get-object "window")))
          (button1 (send builder (get-object "button1")))
          (button2 (send builder (get-object "button2")))
          (button3 (send builder (get-object "quit"))))
      (connect window (destroy
                       (lambda (object)
                         (main-quit))))
      (connect button1 (clicked print-hello))
      (connect button2 (clicked print-hello))
      (connect button3 (clicked
                        (lambda (button)
                          (main-quit))))))

  ;; Let's demonstrate that the signals and callback survive
  ;; garbage collection.
  (gc) (gc) (gc)

  ;; Run the main loop
  (main)
  #t)

(go)
