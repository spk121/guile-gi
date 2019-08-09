;; Copyright (C), 2019 Michael L. Gran

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

(define-module (gi documentation)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (sxml ssax)
  #:use-module (sxml fold)
  #:use-module ((sxml xpath) #:prefix xpath:)
  #:use-module (gi config)
  #:use-module (gi types)
  #:use-module ((gi repository) #:select (infos require))
  #:export (parse
            typelib gir ->guile-procedures.txt))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_document"))

(define gi-namespaces
  '((c ."http://www.gtk.org/introspection/c/1.0")
    (core ."http://www.gtk.org/introspection/core/1.0")
    (glib . "http://www.gtk.org/introspection/glib/1.0")))

(define* (parse string-or-port #:optional (preload '()))
  (letrec ((documentation preload)
           (namespaces
            (map (lambda (el)
                   (cons* (car el) (car el) (ssax:uri-string->symbol (cdr el))))
                 gi-namespaces))
           (res-name->sxml
            (lambda (name)
              (cond
               ((symbol? name) name)
               ((eq? (car name) 'core) (cdr name))
               (else (symbol-append (car name) ': (cdr name))))))

           (crumb (lambda (elem-gi name)
                    (cons (res-name->sxml elem-gi) name)))

           (assoc-subcons!
            (lambda (alist key subkey value)
              (let ((handle (assoc-ref alist key)))
                (cond
                 ((null? list)
                  (list (cons key (list (cons subkey value)))))
                 ((not handle)
                  (assoc-set! alist key (list (cons subkey value))))
                 (else
                  (assoc-set! alist key (assoc-set! handle subkey value)))))))

           (parser
            (ssax:make-parser
             NEW-LEVEL-SEED
             (lambda (elem-gi attrs ns content seed)
               (let ((name (assq-ref attrs 'name))
                     (%path (or (assq-ref seed '%path) '())))
                 (cond
                  (name
                   (list
                    (cons '%path
                          (cons (crumb elem-gi name)
                                %path))))
                  ((equal? elem-gi '(core . return-value))
                   (list (cons* '%path 'return-value %path)))
                  (else
                   (list (cons '%path %path))))))

             FINISH-ELEMENT
             (lambda (elem-gi attributes namespaces parent-seed seed)
               (let ((path (assq-ref seed '%path))
                     (end (car seed))
                     (seed (cdr (ssax:reverse-collect-str-drop-ws seed)))
                     (name (res-name->sxml elem-gi))
                     (attrs
                      (attlist-fold (lambda (attr accum)
                                      (cons (list (res-name->sxml (car attr)) (cdr attr))
                                            accum))
                                    '() attributes)))
                 (case (res-name->sxml elem-gi)
                   ((doc core:doc)
                    (let ((real-doc end))
                      (cond
                       ((null? path) #f) ;; no docs for pathless nodes
                       ((equal? (car path) 'return-value)
                        (set! documentation
                              (assoc-subcons! documentation (reverse (cdr path)) '%return real-doc)))
                       (else
                        (set! documentation
                              (assoc-subcons! documentation (reverse path) 'doc real-doc))))))
                   ((scheme)
                    (set! documentation
                          (assoc-subcons! documentation (reverse path) 'scheme seed))))
                 (acons (res-name->sxml elem-gi)
                        (if (null? attrs)
                            seed
                            (cons (cons '@ attrs) seed))
                        parent-seed)))

             ;; the rest is taken from ssax:xml->sxml
             CHAR-DATA-HANDLER
             (lambda (string1 string2 seed)
               (if (string-null? string2) (cons string1 seed)
                   (cons* string2 string1 seed)))

             DOCTYPE
             (lambda (port docname systemid internal-subset? seed)
               (when internal-subset? (ssax:skip-internal-dtd port))
               (values #f '() namespaces seed))

             UNDECL-ROOT
             (lambda (elem-gi seed)
               (values #f '() namespaces seed))

             PI
             ((*DEFAULT* .
                         (lambda (port pi-tag seed)
                           (ssax:read-pi-body-as-string port)
                           seed))))))
    (parser (if (string? string-or-port) (open-input-string string-or-port) string-or-port)
            '())
    documentation))

(define-method (%info (info <GIBaseInfo>))
  (let ((doc (with-output-to-string (lambda () (%document info)))))
    doc))

(define* (typelib lib #:optional version #:key (require? #t))
  (when require? (require lib version))
  (open-input-string
   (format #f "<namespace name=~s>~a</namespace>" lib
           (string-join (map %info (infos lib)) ""))))

(define (find-gir lib version)
  (or
   (find identity (map (lambda (path)
                         (let ((file (format #f "~a/~a-~a.gir" path lib version)))
                           (and (file-exists? file) file)))
                       (gir-search-path)))
   (error "unable to find gir for ~a-~a" (list lib version))))

(define (gir lib version)
  (open-input-file (find-gir lib version)))

(define* (sort+delete-duplicates! list less #:optional (= equal?))
  "Sort LIST according to LESS and delete duplicate entries according to `='."
  (set! list (sort! list less))
  (let loop ((list list))
    (unless (null? list)
      (do ((item (car list))
           (rest (cdr list) (cdr rest)))
          ((or (null? rest)
               (not (= item (car rest)))))
        (set-cdr! list (cdr rest)))
      (loop (cdr list))))
  list)

(define (car? pair)
  (and (pair? pair) (car pair)))

(define %long-name
  (xpath:sxpath `(@ long-name ,cdr)))

(define %name
  (xpath:sxpath `(@ name ,cdr)))

(define %args
  (xpath:sxpath `(argument @ name ,cdr)))

(define %returns
  (xpath:sxpath `(return @ name ,cdr)))

(define %doc
  (cute assq-ref <> 'doc))

(define %procedures
  (xpath:sxpath `(// procedure)))

(define (%procedures-by-name name)
  (xpath:filter
   (compose
    (xpath:node-or
     (xpath:select-kids (xpath:node-equal? `(name ,name)))
     (xpath:select-kids (xpath:node-equal? `(long-name ,name))))
    (xpath:select-kids (xpath:node-typeof? '@)))))

(define (->guile-procedures.txt xml)
  (let* ((^ (compose cdar (xpath:node-parent xml)))
         (procedures (%procedures xml))
         (names (sort+delete-duplicates! (append (%name procedures)
                                                 (%long-name procedures))
                                         string<=? string=?)))
    (for-each
     (lambda (name)
       (format #t "~c~a~%~%" #\page name)
       (for-each (lambda (p)
                   (let ((long-name (car? (%long-name p))))
                     (if long-name
                         (format #t "- Method: ~a ~a => ~a~%" long-name
                                 (%args p) (%returns p))
                         (format #t "- Procedure: ~a ~a => ~a~%" name
                                 (%args p) (%returns p)))
                     (when (or (not long-name)
                               (equal? name long-name))
                       ;; we have a fully qualified name, so display doc if
                       ;; available
                       (let ((doc (%doc (^ p))))
                          (when doc (display doc) (newline))))))
                 ((%procedures-by-name name) procedures)))
    names)))
