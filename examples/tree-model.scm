;; Copyright (C) 2026 Michael L. Gran

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A GtkTreeModel written in Scheme, shown in a GTK 4 GtkTreeView.
;;
;; GtkTreeStore and GtkListStore hold copies of the data they show.  A
;; model of your own shows data where it already is; this one shows a
;; Scheme expression, with a row for each datum and the elements of a
;; list as the children of its row.
;;
;; A tree model hands out GtkTreeIters, small structs in which the
;; model notes which row an iter refers to.  Here every datum gets a
;; number, and an iter holds the number of its datum in its user_data
;; field.
;;
;; Usage, from the top of the build tree:
;;
;;   tools/uninstalled-env tools/run-guile examples/tree-model.scm

;;; Code:

(use-modules (gi)
             (gi repository)
             (gi types)
             (gi util)
             (oop goops)
             (srfi srfi-9)
             (system foreign))

(use-typelibs
 (("GObject" "2.0") #:select ())
 (("Gio" "2.0") #:renamer (protect* '(application:new)))
 ("Gtk" "4.0"))

;; GTK deprecated its tree widgets in version 4.10, in favor of
;; GtkListView and GtkColumnView, and loading a whole typelib leaves
;; out what is deprecated.  They still work; ask for them by name.
(for-each (lambda (name) (load-by-name "Gtk" name))
          '("TreeModel" "TreeModelFlags" "TreeIter" "TreePath"
            "TreeView" "TreeViewColumn" "TreeSelection"
            "CellLayout" "CellRendererText"))

;;; The data: a Scheme expression, flattened into numbered nodes.

(define expression
  '(define (factorial n)
     "The product of the integers from 1 to N."
     (if (< n 2)
         1
         (* n (factorial (- n 1))))))

(define-record-type <node>
  (make-node datum parent position children)
  node?
  (datum node-datum)
  (parent node-parent)                  ; a node number, or #f at the top
  (position node-position)              ; its index among its siblings
  (children node-children set-node-children!)) ; a vector of node numbers

;; Node numbers start at 1, since an iter whose user_data is NULL reads
;; back as #f.
(define nodes (make-hash-table))
(define n-nodes 0)

(define (add-node! datum parent position)
  (set! n-nodes (1+ n-nodes))
  (let ((id n-nodes)
        (node (make-node datum parent position #())))
    (hashv-set! nodes id node)
    (when (list? datum)
      (set-node-children!
       node
       (list->vector
        (map (lambda (child i) (add-node! child id i))
             datum (iota (length datum))))))
    id))

(define (node id)
  (hashv-ref nodes id))

;; The rows at the top of the tree: just the one expression.
(define roots (vector (add-node! expression #f 0)))

(define (children-of id)
  (if id (node-children (node id)) roots))

(define (datum->label datum)
  (cond ((null? datum) "()")
        ((list? datum) (format #f "(~a …)" (car datum)))
        (else (format #f "~s" datum))))

(define (datum->kind datum)
  (cond ((list? datum) (format #f "list of ~a" (length datum)))
        ((symbol? datum) "symbol")
        ((string? datum) "string")
        ((number? datum) "number")
        (else "other")))

;;; Iters.

(define (iter->id iter)
  (and iter
       (let ((data (tree-iter:user-data iter)))
         (and data (pointer-address data)))))

;; Make ITER refer to the Nth of SIBLINGS and return #t, or return #f if
;; there is no such row.  A virtual function that fills in an iter
;; returns whether it did.
(define (point-at! iter siblings n)
  (cond
   ((< -1 n (vector-length siblings))
    (tree-iter:set-user-data! iter (make-pointer (vector-ref siblings n)))
    #t)
   (else #f)))

;;; The model.  Each virtual function takes the model and then the
;;; arguments of the C virtual function.  The iters and values that C
;;; allocates for a virtual function to fill in are arguments too.

(define <ExpressionModel>
  (register-type
   "ExpressionModel" <GObject>
   #:interfaces (list <GtkTreeModel>)
   #:vfuncs
   `((get-flags . ,(lambda (self) (list->tree-model-flags '(iters-persist))))
     (get-n-columns . ,(lambda (self) 2))
     (get-column-type . ,(lambda (self column) G_TYPE_STRING))

     ;; Fill in ITER to refer to the row at PATH.
     (get-iter
      . ,(lambda (self iter path)
           (let loop ((indices (s32vector->list (tree-path:get-indices path)))
                      (siblings roots))
             (and (pair? indices)
                  (< -1 (car indices) (vector-length siblings))
                  (if (null? (cdr indices))
                      (point-at! iter siblings (car indices))
                      (loop (cdr indices)
                            (children-of (vector-ref siblings (car indices)))))))))

     ;; Return a new path to the row of ITER.
     (get-path
      . ,(lambda (self iter)
           (let loop ((id (iter->id iter))
                      (indices '()))
             (if id
                 (loop (node-parent (node id))
                       (cons (node-position (node id)) indices))
                 (tree-path:new-from-indices (list->s32vector indices))))))

     ;; Fill in VALUE with what the row of ITER shows in COLUMN.
     (get-value
      . ,(lambda (self iter column value)
           (let ((datum (node-datum (node (iter->id iter)))))
             (set! (value G_TYPE_STRING)
                   (if (= column 0) (datum->label datum) (datum->kind datum))))))

     ;; Move ITER to the next row at its level.
     (iter-next
      . ,(lambda (self iter)
           (let ((this (node (iter->id iter))))
             (point-at! iter (children-of (node-parent this))
                        (1+ (node-position this))))))

     (iter-previous
      . ,(lambda (self iter)
           (let ((this (node (iter->id iter))))
             (point-at! iter (children-of (node-parent this))
                        (1- (node-position this))))))

     ;; Fill in ITER to refer to the first child of PARENT, or to the
     ;; first row at the top when PARENT is #f.
     (iter-children
      . ,(lambda (self iter parent)
           (point-at! iter (children-of (iter->id parent)) 0)))

     (iter-has-child
      . ,(lambda (self iter)
           (positive? (vector-length (children-of (iter->id iter))))))

     (iter-n-children
      . ,(lambda (self iter)
           (vector-length (children-of (iter->id iter)))))

     (iter-nth-child
      . ,(lambda (self iter parent n)
           (point-at! iter (children-of (iter->id parent)) n)))

     ;; Fill in ITER to refer to the parent of CHILD.
     (iter-parent
      . ,(lambda (self iter child)
           (let ((this (node (iter->id child))))
             (and (node-parent this)
                  (let ((grandparent (node-parent (node (node-parent this)))))
                    (point-at! iter (children-of grandparent)
                               (node-position (node (node-parent this))))))))))))

;;; The window.

(define (add-text-column! view title column)
  (let ((view-column (tree-view-column:new))
        (cell (cell-renderer-text:new)))
    (tree-view-column:set-title view-column title)
    (tree-view-column:set-resizable view-column #t)
    (cell-layout:pack-start view-column cell #t)
    (cell-layout:add-attribute view-column cell "text" column)
    (tree-view:append-column view view-column)))

(define (show-selection selection)
  (let ((iter (make <GtkTreeIter>)))
    (call-with-values (lambda () (tree-selection:get-selected! selection iter))
      (lambda (selected? model iter)
        (when selected?
          (format #t "~a: ~s~%"
                  (tree-path:to-string (tree-model:get-path model iter))
                  (node-datum (node (iter->id iter)))))))))

(define (activate app)
  (let* ((window (application-window:new app))
         (scrolled (scrolled-window:new))
         (view (tree-view:new-with-model (make <ExpressionModel>))))
    (add-text-column! view "Datum" 0)
    (add-text-column! view "Kind" 1)
    (tree-view:expand-all view)
    (connect (tree-view:get-selection view) changed show-selection)
    (scrolled-window:set-child scrolled view)
    (window:set-title window "A tree model in Scheme")
    (window:set-default-size window 560 460)
    (window:set-child window scrolled)
    (window:present window)))

(define (main)
  (let ((app (application:new "com.example.TreeModel" (number->application-flags 0))))
    (connect app application:activate activate)
    (application:run app (command-line))))

(main)
