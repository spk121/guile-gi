;;; Calling and overriding virtual functions.

(use-modules (gi) (gi repository) (gi types) (gi util)
             (oop goops)
             (srfi srfi-64))

(test-begin "vfunc")

(require "GObject" "2.0")
(require "Gio" "2.0")
(load-by-name "GObject" "Object")
(load-by-name "Gio" "ListModel")
(load-by-name "Gio" "Action")
(load-by-name "Gio" "SimpleAction")
(load-by-name "Gio" "ListStore")

(define actions (map (lambda (name) (simple-action:new name #f)) '("a" "b" "c")))

;;; Interfaces

(define <ActionList>
  (register-type "VfuncActionList" <GObject> '() '()
                 #:interfaces (list <GListModel>)
                 #:vfuncs `((get-item-type . ,(lambda (self) (get-gtype <GSimpleAction>)))
                            (get-n-items . ,(lambda (self) (length actions)))
                            (get_item . ,(lambda (self i)
                                           (and (< i (length actions)) (list-ref actions i)))))))

(define action-list (make <ActionList>))

(test-assert "implements the interface"
  (memq <GListModel> (class-precedence-list <ActionList>)))

(test-equal "interface virtual functions, called from C"
  '(3 "b" #f)
  (list (list-model:get-n-items action-list)
        (action:get-name (list-model:get-item action-list 1))
        (list-model:get-item action-list 3)))

(test-equal "returned objects keep their references"
  '("a" "b" "c")
  (begin
    ;; get_item returns a new reference, which C releases.
    (do ((i 0 (1+ i))) ((= i 1000))
      (list-model:get-item action-list 2))
    (gc) (gc)
    (map action:get-name actions)))

(test-equal "invoker of an interface virtual function"
  3
  (list-model:do-get-n-items <ActionList> action-list))

(test-equal "a GListStore's own implementation"
  1
  (let ((store (list-store:new (get-gtype <GSimpleAction>))))
    (list-store:append store (car actions))
    (list-model:do-get-n-items <GListStore> store)))

;;; Classes

(define disposed 0)

(define <Tracked>
  (register-type "VfuncTracked" <GObject>
                 #:vfuncs `((dispose . ,(lambda (self)
                                          (set! disposed (1+ disposed))
                                          ;; Chain up.
                                          (object:do-dispose <GObject> self))))))

(test-equal "class virtual function, with chaining up"
  1
  (begin
    (object:run-dispose (make <Tracked>))
    disposed))

(test-equal "subclasses inherit overrides"
  2
  (begin
    (object:run-dispose (make (register-type "VfuncSubTracked" <Tracked>)))
    disposed))

(test-error "unknown virtual function"
  #t
  (register-type "VfuncBad1" <GObject> #:vfuncs `((no-such-vfunc . ,(lambda (self) #t)))))

(test-error "reserved virtual function"
  #t
  (register-type "VfuncBad2" <GObject> #:vfuncs `((finalize . ,(lambda (self) #t)))))

(test-error "interface the parent already implements"
  #t
  (register-type "VfuncBad3" <ActionList> #:interfaces (list <GListModel>)))

(test-error "not an interface"
  #t
  (register-type "VfuncBad4" <GObject> #:interfaces (list <GSimpleAction>)))

(test-error "invoker with a class that isn't an implementor"
  #t
  (list-model:do-get-n-items <GObject> action-list))

(test-error "invoker with an instance of another class"
  #t
  (list-model:do-get-n-items <ActionList> (make <GObject>)))

;;; GTK, when a display is available

(define gtk?
  (and (false-if-exception (require "Gtk" "4.0"))
       (begin
         (for-each (lambda (name) (load-by-name "Gtk" name))
                   '("init_check" "Widget" "Window" "Snapshot" "Orientation"))
         (load-by-name "Graphene" "Rect")
         (load-by-name "Gdk" "RGBA")
         (load-by-name "GLib" "MainContext")
         (init-check?))))

(unless gtk?
  (test-skip 1))

(test-equal "a GTK widget drawn and sized in Scheme"
  '(#t 200 90)
  (let* ((snapshots 0)
         (red (let ((c (make <GdkRGBA>))) (rgba:parse? c "red") c))
         (<Swatch>
          (register-type
           "VfuncSwatch" <GtkWidget>
           #:vfuncs
           `((snapshot . ,(lambda (self snapshot)
                            (set! snapshots (1+ snapshots))
                            (let ((r (rect:alloc)))
                              (rect:init r 0.0 0.0 1.0 1.0)
                              (snapshot:append-color snapshot red r))))
             (measure . ,(lambda (self orientation for-size)
                           (if (eq? (enum->symbol orientation) 'horizontal)
                               (values 120 200 -1 -1)
                               (values 80 90 -1 -1)))))))
         (window (window:new))
         (swatch (make <Swatch>)))
    (window:set-child window swatch)
    (window:present window)
    (do ((i 0 (1+ i)))
        ((or (> snapshots 0) (= i 1000)))
      (main-context:iteration? (main-context:default) #f))
    (let ((result (list (> snapshots 0) (widget:get-width swatch) (widget:get-height swatch))))
      (window:destroy window)
      result)))

(test-end "vfunc")
