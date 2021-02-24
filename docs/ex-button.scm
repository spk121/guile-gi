(use-modules (gi) (gi types) (gi util))

;; We're going to be creating many GOOPS generic functions
;; once we load up the typelibs.  We want the same-named
;; GOOPS generic functions to merge: we don't want the
;; generic functions from one type to overwrite a previous
;; class's generic functions.
(push-duplicate-handler! 'merge-generics)

;; Here we parse the typelib files to dynamically create our
;; types, classes and procedures.
;; Gio's application:new and Gtk's application:new take the
;; same argument types, so their GOOPS generics are easy to
;; confuse. We rename Gio's application:new for safety.
(use-typelibs (("Gio" "2.0") #:renamer (protect 'application:new))
              ("Gtk" "3.0"))

;; This is a callback we install to print a console message on the
;; receipt of the `clicked' signal.
(define (print-hello widget)
  (display "Hello World\n"))

;; This is the callback we install to handle the `activate'
;; signal that a <GApplication> may receive.
(define (activate-callback app)

  ;; Create a <GtkApplicationWindow> for this
  ;; <GtkApplication>, plus a button.
  (let* ((window (application-window:new app))
         (button-box (button-box:new
                      (symbol->orientation 'vertical)))
         (button (button:new-with-label "Hello World")))

    ;; Connect and decorate the window and button.
    (set-title window "Window")
    (set-default-size window 200 200)
    (add window button-box)
    (add button-box button)

    ;; Register a procedure as a signal handler to the
    ;; <GtkButton>'s `clicked' signal.
    (connect button print-hello)

    ;; Register a lambda as the signal handler of the
    ;; <GtkButton>'s `clicked' signal.
    (connect button clicked (lambda args (destroy window)))

    ;; Make everything visible.
    (show-all window))

  ;; Note that the `activate' signal callback requires no
  ;; return value.
  )

(define (main)
  ;; Call Gtk's application:new to create a new
  ;; <GtkApplication>
  (let ((app (application:new
              "org.gtk.example"
              ;; Pick a flag from the set of GApplicationFlags
              (list->application-flags '(flags-none)))))
    ;; Register the procedure `activate-callback' as the
    ;; signal handler for the `activate' signal that the
    ;; <GtkApplication> handles.
    (connect app activate activate-callback)

    ;; Call application:run.  If there are no arguments in
    ;; the command line args, this will send the `activate'
    ;; signal.
    (run app (command-line))))

;; Run the script
(main)
