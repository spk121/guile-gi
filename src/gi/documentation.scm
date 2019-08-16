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
  #:use-module (ice-9 peg)
  #:use-module (ice-9 pretty-print)
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

(define-peg-pattern inline-ws body (or " "))
(define-peg-pattern any-ws body (or inline-ws "\n"))
(define-peg-pattern wordsep body
  (or any-ws "-" "_" "/" "+" "*" "<" ">" "=" "." ":" "," ";" "?" "!"))

(define-peg-pattern listing-language none
  (and "<!--"
       (* any-ws)
       "language=\"" (+ (and (not-followed-by "\"") peg-any)) "\""
       (* any-ws)
       "-->"))
(define-peg-pattern listing-body body
  (and  (? "\n")
        (*
         (and
          (* inline-ws)
          (not-followed-by "]|")
          (* (and (not-followed-by "\n") peg-any))
          "\n"))))
(define-peg-pattern listing all
  (and (ignore "|[")
       (? listing-language)
       listing-body
       (ignore (* inline-ws))
       (ignore "]|")))

(define-peg-pattern word body
  (+ (and (not-followed-by wordsep) peg-any)))

(define-peg-pattern token body
  (+ (or (range #\A #\Z)
         (range #\a #\z)
         (range #\0 #\9)
         "_")))
(define-peg-pattern id body
  (+ (or (range #\a #\z)
         (range #\0 #\9)
         "_" "-")))

(define-peg-pattern function all (and token "()"))
(define-peg-pattern parameter all (and (ignore "@") token))
(define-peg-pattern constant all (and (ignore "%") token))
(define-peg-pattern symbol all (and (ignore "#") token (not-followed-by ":")))
(define-peg-pattern property all (and (ignore (and "#" token ":")) id))
(define-peg-pattern signal all (and (ignore (and "#" token "::")) id))

(define-peg-pattern paragraph all
  (+
   (and
    (not-followed-by (or (and (+ "#") inline-ws) "\n"))
    (* inline-ws)
    (or
     listing
     (and
      (*
       (and
        (not-followed-by "\n")
        (or function parameter constant symbol property signal word
            wordsep)))
      (? "\n"))))))

(define-peg-pattern anchor all
  (and (ignore (* "#"))
       (ignore (* inline-ws))
       (ignore "{#")
       (+ (and (not-followed-by "}") peg-any))
       (ignore "}")))

(define-peg-pattern title all
  (and (ignore (+ inline-ws))
       (+ (and (not-followed-by (or anchor "\n")) peg-any))
       (? anchor)))

(define-peg-pattern refsect2 all
  (and (ignore "##") title (ignore "\n")
       (* (or (ignore any-ws) paragraph))))

(define-peg-pattern doc-string body
  (+ (or (ignore any-ws) refsect2 paragraph)))

(define ->docbook
  (letrec ((%scheme (xpath:sxpath `(scheme)))
           (%entry (xpath:sxpath `(refentry)))
           (%functions
            (compose
             (xpath:select-kids identity)
             (xpath:node-or
              (xpath:node-self
               (xpath:node-typeof? 'method))
              (xpath:node-self
               (xpath:node-typeof? 'function)))))

           (fancy-quote
            (lambda (str)
              (string-append "“" str "”")))

           (ppfn
            (lambda (port name args returns)
              (let ((name (string->symbol name))
                    (args (map string->symbol args))
                    (returns (map string->symbol returns)))
               (pretty-print
               `(define-values ,returns (,name ,@args))
               port
               ;; allow one-liners
               #:max-expr-width 79))))

           (markdown-1
            (lambda (str)
              (let ((match (match-pattern doc-string str)))
                (if match
                    (pre-post-order
                     (peg:tree match)
                     `((paragraph . ,(lambda (tag . kids) `(para ,@kids)))
                       (listing . ,(lambda (tag . kids)
                                     `(informalexample (programlisting ,@kids))))
                       (property . ,(lambda (tag property . ignore)
                                      `(type ,(fancy-quote property))))
                       (signal . ,(lambda (tag signal . ignore)
                                    `(type ,(fancy-quote signal))))
                       (symbol . ,(lambda (tag . kids) `(type ,@kids)))
                       (anchor . ,(lambda (tag id . ignore) `(,tag (@ (id ,id)))))
                       (*text* . ,(lambda (tag txt) txt))
                       (*default* . ,(lambda (tag . kids) (cons tag kids)))))
                    (begin
                      (display "WARNING: unparseable docstring will be included literally"
                               (current-error-port))
                      (newline (current-error-port))
                      (list str))))))

           (markdown
            (lambda (str)
              (let ((md (markdown-1 str)))
                (cond
                 ((string? (car md))
                  `((para ,@md)))
                 ((symbol? (car md))
                  (list md))
                 (else md)))))

           (refname
            (lambda (tag . kids)
              `(refname
                ,@(%name (cons tag kids)))))

           (chapter
            (lambda (tag . kids)
              (let ((functions (%functions kids))
                    (entries (%entry (cons tag kids))))
                `(chapter
                  (title ,@(%name (cons tag kids)))
                  ,@entries
                  (refentry
                   (refnamediv (refname "Functions"))
                   (refsect1 (title "Functions")
                    ,@functions))))))

           (entry
            (lambda (tag . kids)
              (let ((doc (%doc (cons tag kids)))
                    (scheme (%scheme (cons tag kids)))
                    (functions (%functions kids)))
                (cond
                 ((null? scheme)
                  '())

                 ((null? doc)
                  `(refentry
                    (refnamediv ,@(cdar scheme))
                    (refsect1
                     (title "Methods")
                     ,@functions)))

                 (else
                  `(refentry
                    (refnamediv ,@(cdar scheme))
                    (refsect1
                     (title "Description")
                     ,@(markdown (string-join doc "")))
                    (refsect1
                     (title "Methods")
                     ,@functions)))))))

           (refsect2
            (lambda (tag . kids)
              (let ((doc (%doc (cons tag kids)))
                    (scheme (%scheme (cons tag kids))))
                (list
                 tag
                 (cond
                  ((null? scheme) '())
                  ((null? doc)
                   `(refsect2 ,@(cadar scheme)
                              (para "Undocumented")))
                  (else `(refsect2 ,@(cadar scheme) ,@(markdown (string-join doc ""))))))))))

    (compose
     sxml->xml
     (cute
      pre-post-order
      <>
      `((repository . ,(lambda (tag . kids)
                         `(*TOP* (book ,@kids))))
        (namespace . ,chapter)
        (record . ,entry)
        (class . ,entry)
        (interface . ,entry)
        (union . ,entry)
        (method . ,refsect2)
        (function . ,refsect2)

        (procedure
         .
         ,(lambda (tag . kids)
            (let ((node (cons tag kids)))
              `((title ,@(%name (cons tag kids)))
                ,(car
                  (map
                   (lambda (ln)
                     `(informalexample
                       (programlisting
                        ,(call-with-output-string
                          (lambda (port)
                            (ppfn port ln
                                  (append-map
                                    %name
                                    ((xpath:sxpath `(argument)) node))
                                  (append-map
                                   %name
                                   ((xpath:sxpath `(return)) node))))))))
                   (append (%long-name node)
                           (%name node))))))))
        (type . ,refname)

        (*text* . ,(lambda (tag txt)
                     txt))
        (*default* . ,(lambda (tag . kids)
                        (cons tag kids))))))))
