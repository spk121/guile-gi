;;; A GtkTreeModel written in Scheme, for GTK 4: its virtual functions
;;; fill in caller-allocated iters and values, advance iters in place
;;; and return new paths.  Only the last test needs a display.

(use-modules (gi) (gi repository) (gi types) (gi util)
             (oop goops)
             (srfi srfi-64)
             (system foreign))

(test-begin "gtk4-tree-model")

(define rows #("one" "two" "three"))
(define n-values-read 0)

(define (row iter)
  (pointer-address (or (tree-iter:user-data iter) %null-pointer)))

(define (point-at! iter n)
  (tree-iter:set-stamp! iter 77)
  (tree-iter:set-user-data! iter (make-pointer n))
  (< n (vector-length rows)))

(define-syntax-rule (values->list expr)
  (call-with-values (lambda () expr) list))

(define <RowModel> #f)
(define rows-model #f)
(define rows-iter #f)

(define (read-row model iter)
  (let ((value (make <GValue>)))
    (tree-model:get-value! model iter 0 value)
    (value)))

(if (and (false-if-exception (require "Gtk" "4.0"))
         (false-if-exception (require "GObject" "2.0")))
    (test-assert "register a tree model"
      (begin
        (for-each (lambda (name) (load-by-name "Gtk" name))
                  '("TreeModel" "TreeModelFlags" "TreeIter" "TreePath"))
        (load-by-name "GObject" "Object")
        (set! <RowModel>
          (register-type
           "RowModel" <GObject>
           #:interfaces (list <GtkTreeModel>)
           #:vfuncs
           `((get-flags . ,(lambda (self) (list->tree-model-flags '(list-only))))
             (get-n-columns . ,(lambda (self) 1))
             (get-column-type . ,(lambda (self column) G_TYPE_STRING))
             (get-iter . ,(lambda (self iter path)
                            (point-at! iter (s32vector-ref (tree-path:get-indices path) 0))))
             (get-path . ,(lambda (self iter)
                            (tree-path:new-from-indices (s32vector (row iter)))))
             (get-value . ,(lambda (self iter column value)
                             (set! n-values-read (1+ n-values-read))
                             (set! (value G_TYPE_STRING) (vector-ref rows (row iter)))))
             (iter-next . ,(lambda (self iter) (point-at! iter (1+ (row iter)))))
             (iter-children . ,(lambda (self iter parent)
                                 (and (not parent) (point-at! iter 0))))
             (iter-has-child . ,(lambda (self iter) #f))
             (iter-n-children . ,(lambda (self iter)
                                   (if iter 0 (vector-length rows))))
             (iter-nth-child . ,(lambda (self iter parent n)
                                  (and (not parent) (point-at! iter n))))
             (iter-parent . ,(lambda (self iter child) #f)))))
        (set! rows-model (make <RowModel>))
        (set! rows-iter (make <GtkTreeIter>))
        (is-a? rows-model <GtkTreeModel>)))
    (begin
      (test-expect-fail "register a tree model")
      (test-assert "register a tree model" #f)))

(unless (test-passed?)
  (test-skip most-positive-fixnum))

(test-equal "plain virtual functions"
  (list 1 <string> 3)
  (list (tree-model:get-n-columns rows-model)
        (tree-model:get-column-type rows-model 0)
        (tree-model:iter-n-children rows-model #f)))

(test-equal "a virtual function fills in a caller-allocated iter"
  '(#t 77 0)
  (list (car (values->list (tree-model:get-iter-first! rows-model rows-iter)))
        (tree-iter:stamp rows-iter)
        (row rows-iter)))

(test-equal "a virtual function fills in a caller-allocated value"
  "one"
  (read-row rows-model rows-iter))

(test-equal "a virtual function advances an iter in place"
  '("two" "three" #f)
  (let* ((second (and (tree-model:iter-next? rows-model rows-iter)
                      (read-row rows-model rows-iter)))
         (third (and (tree-model:iter-next? rows-model rows-iter)
                     (read-row rows-model rows-iter))))
    (list second third (tree-model:iter-next? rows-model rows-iter))))

(test-equal "a virtual function that returns #f leaves no row"
  #f
  (car (values->list
        (tree-model:get-iter! rows-model rows-iter (tree-path:new-from-indices (s32vector 5))))))

(test-equal "a virtual function returns a new path"
  "2"
  (begin
    (tree-model:iter-nth-child! rows-model rows-iter #f 2)
    (let ((path (tree-model:get-path rows-model rows-iter)))
      (gc)
      (tree-path:to-string path))))

;; With a display, let a GtkTreeView read the model.
(when (and rows-model
           (false-if-exception (load-by-name "Gtk" "init_check"))
           (false-if-exception (init-check?)))
  (for-each (lambda (name) (load-by-name "Gtk" name))
            '("TreeView" "TreeViewColumn" "CellRendererText" "CellLayout"
              "TreeSelection" "Window" "Widget"))
  (require "GLib" "2.0")
  (load-by-name "GLib" "MainContext")
  (test-equal "a tree view shows the model"
    '(#t 1)
    (let ((view (tree-view:new-with-model rows-model))
          (column (tree-view-column:new))
          (cell (cell-renderer-text:new))
          (window (make <GtkWindow>)))
      (cell-layout:pack-start column cell #t)
      (cell-layout:add-attribute column cell "text" 0)
      (tree-view:append-column view column)
      (window:set-child window view)
      (window:present window)
      (set! n-values-read 0)
      (do ((i 0 (1+ i)))
          ((or (= i 200) (not (main-context:iteration? (main-context:default) #f)))))
      (let ((selection (tree-view:get-selection view))
            (selected (make <GtkTreeIter>)))
        (tree-selection:select-path selection (tree-path:new-from-indices (s32vector 1)))
        (tree-selection:get-selected! selection selected)
        (gc)
        (let ((result (list (>= n-values-read (vector-length rows)) (row selected))))
          (window:destroy window)
          result)))))

(test-end "gtk4-tree-model")
