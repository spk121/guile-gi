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
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (sxml ssax)
  #:use-module (sxml transform)
  #:use-module ((sxml xpath) #:prefix xpath:)
  #:use-module (gi config)
  #:use-module (gi types)
  #:use-module ((gi repository) #:select (infos require))
  #:export (parse
            typelib gir
            ->guile-procedures.txt
            ->docbook))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_document"))

(define documentation-specials
  (cdr '(%
         namespace
         ;; introspected types
         class record union interface
         function method
         ;; leaves
         doc scheme)))

(define (documentation-special? sym)
  (member sym documentation-specials))

(define gi-namespaces
  '((c ."http://www.gtk.org/introspection/c/1.0")
    (core ."http://www.gtk.org/introspection/core/1.0")
    (glib . "http://www.gtk.org/introspection/glib/1.0")))

(define (car? pair)
  (and (pair? pair) (car pair)))

(define* (parse string-or-port #:optional (documentation '()))
  (letrec ((namespaces
            (map (lambda (el)
                   (cons* (car el) (car el) (ssax:uri-string->symbol (cdr el))))
                 gi-namespaces))
           (res-name->sxml
            (lambda (name)
              (cond
               ((symbol? name) name)
               ((eq? (car name) 'core) (cdr name))
               (else (symbol-append (car name) ': (cdr name))))))

           (%existing
            (case-lambda
             ((elem-gi)
              (xpath:node-or
                (xpath:select-kids
                 (xpath:node-typeof? (res-name->sxml elem-gi)))
                (xpath:node-self
                 (xpath:node-typeof? (res-name->sxml elem-gi)))))
             ((elem-gi name)
              (compose
               (xpath:filter
                (compose
                 (xpath:select-kids (xpath:node-equal? `(name ,name)))
                 (xpath:select-kids (xpath:node-typeof? '@))))
               (%existing elem-gi)))))

           (parser
            (ssax:make-parser
             ;; the real work here lies in detecting existing items
             ;; and making them the new seed
             NEW-LEVEL-SEED
             (lambda (elem-gi attrs ns content seed)
               (let ((name (assq-ref attrs 'name))
                     (%path (or (assq-ref seed '%path) '()))
                     (existing '()))
                 (if name
                     (set! existing ((%existing elem-gi name) seed)))

                 (cons*
                  ;; the current path in tags
                  (cons* '%path (res-name->sxml elem-gi) %path)
                  ;; link to the existing element
                  (cons '%existing (car? existing))
                  (cond
                   ((equal? (res-name->sxml elem-gi) 'namespace)
                    ;; we have to pull documentation directly here instead of having it as
                    ;; seed because of some oddities regarding the <repository> tag
                    (let ((existing-ns ((%existing elem-gi name) documentation)))
                      (if (pair? existing-ns)
                          (cddar existing-ns)
                          '())))
                   (name
                    (if (pair? existing)
                        (cddar existing)
                        '()))
                   (else
                    '())))))

             FINISH-ELEMENT
             (lambda (elem-gi attributes namespaces parent-seed seed)
               (let ((path (assq-ref seed '%path))
                     (existing (assq-ref seed '%existing))
                     (end (car? seed))
                     ;; collect strings and drop helper elements
                     (seed (filter (negate (lambda (elt)
                                             (member (car? elt) '(%path %existing))))
                                   (ssax:reverse-collect-str-drop-ws seed)))
                     (name (assq-ref attributes 'name))
                     (attrs
                      (attlist-fold (lambda (attr accum)
                                      (cons (list (res-name->sxml (car attr)) (cdr attr))
                                            accum))
                                    '() attributes)))
                 (let ((seed (if (or (member 'scheme path)
                                     (member 'doc path))
                                 seed
                                 (filter
                                  (compose documentation-special? car?)
                                  seed))))
                   (cond
                    ;; we've reached the top and are only interested in namespaces
                    ((equal? (res-name->sxml elem-gi) 'repository)
                     (cons (res-name->sxml elem-gi)
                           (filter
                            (lambda (head)
                              (eq? (car? head) 'namespace))
                            seed)))
                    ;; keep the XML structure, but delete the previously existing element
                    (else
                     (acons
                      (res-name->sxml elem-gi)
                      (if (null? attrs) seed (cons (cons '@ attrs) seed))
                      (delq existing parent-seed)))))))

             ;; the rest is taken from ssax:xml->sxml with minor changes
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
                           (cons
                            (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                            seed)))))))
    (parser (if (string? string-or-port) (open-input-string string-or-port) string-or-port)
            '())))

(define-method (%info (info <GIBaseInfo>))
  (let ((doc (with-output-to-string (lambda () (%document info)))))
    doc))

(define* (typelib lib #:optional version #:key (require? #t))
  (when require? (require lib version))
  (open-input-string
   (format #f "<repository><namespace name=~s>~a</namespace></repository>" lib
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

(define %long-name
  (xpath:sxpath `(@ long-name *text*)))

(define %name
  (xpath:sxpath `(@ name *text*)))

(define %args
  (xpath:sxpath `(argument @ name *text*)))

(define %returns
  (xpath:sxpath `(return @ name *text*)))

(define %doc
  (xpath:sxpath `(doc *text*)))

(define %procedures (xpath:sxpath `(// procedure)))

(define (%procedures-by-name name)
  (xpath:filter
   (compose
    (xpath:node-or
     (xpath:select-kids (xpath:node-equal? `(name ,name)))
     (xpath:select-kids (xpath:node-equal? `(long-name ,name))))
    (xpath:select-kids (xpath:node-typeof? '@)))))

(define (->guile-procedures.txt xml)
  (let* ((^ (xpath:node-parent xml))
         (^^ (compose ^ ^))
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
                       (let ((doc (car? (%doc (^^ p)))))
                          (when doc (display doc) (newline))))))
                 ((%procedures-by-name name) procedures))
       (newline))
    names)))

(define (string-whitespace? str)
  (string-fold (lambda (char seed) (and (char-whitespace? char) seed)) #t str))

(define (parse-markdown-1 str chain)
  (let ((function (make-regexp "([A-Za-z0-9_]+\\(\\))"))
        (param (make-regexp "@([A-Za-z0-9_]+)"))
        (symbol (make-regexp "#([A-Za-z0-9_]+)"))
        (propsig (make-regexp "#[A-Za-z0-9_]+::?([a-z0-9-]+)"))
        (constant (make-regexp "%([A-Za-z0-9_]+)"))
        (\n\n (make-regexp "(\n\n)"))

        (para
         (lambda (chain)
           (call-with-values (lambda ()
                               (span
                                (lambda (elt)
                                  (if (pair? elt)
                                      (not (equal? (car elt) 'para))
                                      #t))
                                chain))
             (lambda (para paras)
               (if (null? para)
                   paras
                   (cons `(para ,@(reverse! para)) paras))))))

        (parse-matches
         (lambda (tag regex)
           (fold-matches regex str '()
            (lambda (match rest)
             (cons
              (list (match:start match)
                    (match:end match)
                    tag
                    (match:substring match 1))
              rest)))))
        (string-cons?
         (lambda (str rest)
           (if (string-whitespace? str)
               rest
               (cons str rest)))))

    (let loop ((chain chain)
               (point 0)
               (matches
                (sort
                 (append
                  (parse-matches 'function function)
                  (parse-matches 'parameter param)
                  (parse-matches 'type symbol)
                  (parse-matches 'constant constant)
                  (parse-matches 'literal propsig)
                  (parse-matches 'para \n\n))
                 (lambda (a b) (< (car a) (car b))))))
      (if (null? matches)
          (para (string-cons? (substring str point (string-length str)) chain))
          (let* ((match (car matches))
                 (start (first match))
                 (end (second match))
                 (tag (third match))
                 (match-str (fourth match)))
            (loop
             (case tag
               ((para)
                (para (string-cons? (substring str point start) chain)))
               ((literal)
                (cons `(type ,(string-append "“" match-str "”"))
                      ;; the previous chain element is the type, drop it
                      (cdr chain)))
               ((function parameter type constant)
                (cons
                 `(,tag ,match-str)
                 (string-cons? (substring str point start) chain)))
               (else (error "invalid tag")))
             end
             (cdr matches)))))))

(define (parse-markdown str)
  (reverse!
   (let ((code-block-start (make-regexp "\\|\\["))
         (code-block-end (make-regexp "\\]\\|")))
     (let loop
         ((chain '())
          (point 0)
          (starts (list-matches code-block-start str))
          (ends (list-matches code-block-end str)))
       (cond
        ((null? starts)
         (parse-markdown-1 (substring str point (string-length str))
                           chain))

        ((null? ends)
         (parse-markdown-1 (substring str point (string-length str))
                           chain))

        (else
         (let ((start (car starts))
               (end (car ends)))
           (cond
            ((< (match:start start) point)
             (loop chain point (cdr starts) ends))
            ((< (match:start end) (match:end start))
             (loop chain point starts (cdr ends)))
            (else
             (loop
              (let ((pre (substring str point (match:start start)))
                    (this (substring str (match:end start) (match:start end))))
                (cons* `(para (informalexample (programlisting (,this))))
                       (parse-markdown-1 pre chain)))
              (match:end end)
              (cdr starts)
              (cdr ends)))))))))))

(define ->docbook
  (letrec ((%scheme (xpath:sxpath `(scheme)))
           (%section (xpath:sxpath `(section)))
           (%simplesect (xpath:sxpath `(simplesect)))
           (%top (xpath:sxpath '(namespace *any*)))

           (\n\n (make-regexp "\n\n"))

           (title
            (lambda (tag . kids)
              `(title
                ,@(%name (cons tag kids)))))

           (chapter
            (lambda (tag . kids)
              (let ((simple (%simplesect (cons tag kids)))
                    (sections (%section (cons tag kids))))
                `(chapter
                  (title ,@(%name (cons tag kids)))
                  (section (@ (name "Contents")))
                  ,@sections
                  ,@simple))))

           (section
            (lambda (tag . kids)
              (let ((doc (%doc (cons tag kids)))
                    (scheme (%scheme (cons tag kids)))
                    (sections (%simplesect (cons tag kids))))
                (cond
                 ((and (null? scheme) (null? sections))
                  '())

                 ((null? scheme)
                  `(section ,@sections))

                 (else
                  `(section
                    ,@(cdar scheme)
                    ,@(parse-markdown (string-join doc ""))
                    ,@sections))))))

           (simplesect
            (lambda (tag . kids)
              (let ((doc (%doc (cons tag kids)))
                    (scheme (%scheme (cons tag kids))))
                (cond
                 ((null? scheme) '())
                 ((null? doc)
                  `(simplesect ,@(cadar scheme)
                               (para "Undocumented")))
                 (else `(simplesect ,@(cadar scheme) ,@(parse-markdown (string-join doc "")))))))))
    (compose
     sxml->xml
     (cute
      pre-post-order
      <>
      `((repository . ,(lambda (tag . kids)
                         `(*TOP* (book ,@kids))))
        (namespace . ,chapter)
        (record . ,section)
        (class . ,section)
        (interface . ,section)
        (union . ,section)
        (method . ,simplesect)
        (function . ,simplesect)

        (procedure
         .
         ,(lambda (tag . kids)
            (let ((node (cons tag kids)))
              `(,(apply title tag kids)
                ,(car
                  (map
                   (lambda (ln)
                     `(subtitle
                       (code
                        ,(format
                          #f "~a ;=> ~a"
                          (cons
                           ln
                           (append-map %name
                                       ((xpath:sxpath `(argument)) node)))
                          (append-map %name
                                      ((xpath:sxpath `(return)) node))))))
                   (append (%long-name node)
                           (%name node))))))))
        (type . ,title)

        (*text* . ,(lambda (tag txt)
                     txt))
        (*default* . ,(lambda (tag . kids)
                        (cons tag kids))))))))
