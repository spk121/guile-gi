(define-module (gi _lib)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (class?
	    class-descends-from?
	    list-find-duplicate
	    list-get-keyword
	    list-of-classes-descends-from?
	    or-map))

(define (class? x)
  "Return #t if X is a class."
  (if (false-if-exception (class-name x))
      #t
      #f))

(define (class-descends-from? class parent)
  "Return #t if CLASS is a GOOPS class that is a PARENT,
(and not, for example, a <number>)"
  (unless (class? class)
    (scm-error 'wrong-type-arg "class-descends-from?" "Not a class: ~S" (list class) #f))
  (unless (class? parent)
    (scm-error 'wrong-type-arg "class-descends-from?" "Not a class: ~S" (list parent) #f))
  (if (memq <object> (class-precedence-list class))
      #t
      #f))

(define (_list-find-duplicate l)
  "Given a list, returns the first duplicate entry it finds, or #f if
there are no duplicate entries."
  (match l
    (() #f)
    ((head . tail)
     (if (memq head tail)
	 head
	 (_list-find-duplicate tail)))))

;; adapted from (oop goops)
(define (list-find-duplicate l)
  "Given a list, returns the first duplicate entry it finds, or #f if
there are no duplicate entries."
  (unless (list? l)
    (scm-error 'wrong-type-arg "list-find-duplicate" "Not a list: ~S" (list l) #f))
  (_list-find-duplicate l))

;; adapted from (oop goops)
(define* (list-get-keyword key l #:optional default)
  "Determine an associated value for the keyword @var{key} from the list
@var{l}.  The list @var{l} has to consist of an even number of elements,
where, starting with the first, every second element is a keyword,
followed by its associated value.  If @var{l} does not hold a value for
@var{key}, the value @var{default} is returned."
  (unless (list? l)
    (scm-error 'wrong-type-arg "list-get-keyword" "Not a list: ~S" (list l) #f))
  (unless (keyword? key)
    (scm-error 'wrong-type-arg "list-get-keyword" "Not a keyword: ~S" (list key) #f))
  (let loop ((l l))
    (match l
      (() default)
      ((kw arg . l)
       (unless (keyword? kw)
         (scm-error 'wrong-type-arg "list-get-keyword" "Not a keyword: ~S" (list kw) #f))
       (if (eq? kw key)
	   arg
	   ;; else
	   (loop l))))))

(define (list-of-classes-descends-from? list-of-classes parent)
  "Return #t if at least one of the classes in LIST-OF-CLASSES
descends from the PARENT class"
  (unless (list? list-of-classes)
    (scm-error 'wrong-type-arg
	       "list-of-classes-descends-from?"
	       "Not a list: ~S" (list list-of-classes) #f))
  (unless (class? parent)
    (scm-error 'wrong-type-arg "list-of-classes-descends-from?" "Not a class: ~S" (list parent) #f))
  (if (or-map (lambda (C)
		(class-descends-from? C parent))
	      list-of-classes)
      #t
      #f))

;;; (or-map fn lst)
;; is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)

;; adapted from (ice-9 boot-9.scm)
(define (or-map func lst)
  "Apply FUNC to successive elements of LST until exhaustion or while FUNC returns #f.
If returning early, return the return value of FUNC."
  (unless (list? lst)
    (scm-error 'wrong-type-arg "or-map" "Not a list: ~S" (list lst) #f))
  (unless (procedure? func)
    (scm-error 'wrong-type-arg "or-map" "Not a procedure: ~S" (list func) #f))
  (let loop ((result #f)
             (L lst))
    (or result
        (and (not (null? L))
             (loop (func (car L)) (cdr L))))))
