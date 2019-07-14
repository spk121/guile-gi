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
  (init)

  ;; Construct a GtkBuilder instance and load our UI description.
  (let ((builder (builder:new-from-file (find-file "builder.ui"))))

    ;; Connect the widgets to their signals and callbacks.
    (let ((window (with-object builder (get-object "window")))
          (button1 (with-object builder (get-object "button1")))
          (button2 (with-object builder (get-object "button2")))
          (button3 (with-object builder (get-object "quit"))))
      (with-object window
        (connect! destroy
          (lambda (object)
            (main-quit))))
      (with-object button1 (connect! clicked print-hello))
      (with-object button2 (connect! clicked print-hello))
      (with-object button3
        (connect! clicked
          (lambda (button)
            (main-quit))))))

  ;; Let's demonstrate that the signals and callback survive
  ;; garbage collection.
  (gc) (gc) (gc)

  ;; Run the main loop
  (main)
  #t)

(go)
