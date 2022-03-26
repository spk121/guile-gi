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
  #:use-module ((gi repository) #:select (<baseinfo> infos require))
  #:export (parse
            typelib gir
            ->guile-procedures.txt
            ->docbook))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_document"))

;;; XML Parsing

(define documentation-special-leaves
  '(doc scheme parameters))

(define documentation-special-nodes
  (cdr '(%
         namespace
         ;; introspected types
         class record union interface enumeration bitfield
         function method member property)))

(define (documentation-special? sym)
  (or (member sym documentation-special-nodes)
      (member sym documentation-special-leaves)))

(define (documentation-leaf? path)
  (any (cute member <> path) documentation-special-leaves))

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
                 ;; the *single* link to the already existing element
                 ;; documenting the same data
                 (set! existing
                       (cond
                        ((member (res-name->sxml elem-gi)
                                 '(parameters instance-parameter))
                         ((%existing elem-gi) seed))
                        (name
                         ((%existing elem-gi name) seed))))

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
                          ;; take the children of the element, remove its attrs
                          (cddar existing-ns)
                          '())))
                   (name
                    (if (pair? existing)
                        ;; take the children of the element, remove its attrs
                        (cddar existing)
                        '()))
                   (else
                    (if (pair? existing)
                        ;; take the children of the element
                        (cdar existing)
                        '()))))))

             FINISH-ELEMENT
             (lambda (elem-gi attributes namespaces parent-seed seed)
               (let ((path (assq-ref seed '%path))
                     (existing (assq-ref seed '%existing))
                     ;; collect strings and drop helper elements
                     (seed (filter (negate (lambda (elt)
                                             (member (car? elt) '(%path %existing))))
                                   (ssax:reverse-collect-str-drop-ws seed)))
                     (attrs
                      (attlist-fold (lambda (attr accum)
                                      (cons (list (res-name->sxml (car attr)) (cdr attr))
                                            accum))
                                    '() attributes)))
                 (let ((seed (if (documentation-leaf? path)
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

(define-method (%info (info <baseinfo>))
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

;;; (GTK flavoured) Markdown Parser

(define-peg-pattern inline-ws body (or " "))
(define-peg-pattern any-ws body (or inline-ws "\n"))
(define-peg-pattern wordsep body
  (or any-ws
      "-" "_" "/" "+" "*" "="
      "(" ")" "[" "]" "{" "}" "<" ">"
      "." ":" "," ";" "?" "!"))

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

(define-peg-pattern code all
  (and (ignore "`") (+ (and (not-followed-by "`") peg-any)) (ignore "`")))
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
        (or code
            function parameter constant
            symbol property signal
            word wordsep)))
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

;;; Backends

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

(define (->guile-procedures.txt xml)
  (letrec ((%scheme
            (compose (xpath:select-kids identity)
                     (xpath:node-self (xpath:node-typeof? 'scheme))))
           (procedure
            (lambda (tag . kids)
              (let* ((doc (%doc (cons tag kids)))
                     (scheme (%scheme kids))
                     (args (%args scheme))
                     (returns (%returns scheme))
                     (long-name (%long-name scheme))
                     (name (%name scheme)))
                (cond
                 ((null? scheme) '())
                 ((null? doc) '())
                 ((null? long-name)
                  (cons* 'procedure (car name)
                         (format #f "- Procedure: ~a => ~a"
                                 (cons (car name) args)
                                 returns)
                         doc))
                 (else
                  (cons* 'procedure (car long-name)
                         `(alias ,@name)
                         (format #f "- Method: ~a => ~a"
                                 (cons (car long-name) args)
                                 returns)
                         doc))))))
           (assoc-cons!
            (lambda (alist key val)
              (assoc-set! alist key (cons val (or (assoc-ref alist key) '())))))

           (container
            (lambda (tag . kids)
              (filter (xpath:node-typeof? 'procedure) kids))))
    (pre-post-order
     xml
     `((repository . ,(lambda (tag . procedures)
                        (for-each
                         (lambda (proc)
                           (format #t "~c~a~%~%~a~%~%"
                                   #\page (car proc)
                                   (string-join (cdr proc) "\n")))
                         (sort
                          (apply append procedures)
                          (lambda (a b)
                            (string< (car a) (car b)))))))
       (namespace . ,(compose
                      (lambda (procedures)
                        (fold
                         (lambda (proc seed)
                           (let* ((name (cadr proc))
                                  (%doc (cddr proc))
                                  (alias (and (pair? (car %doc)) (car %doc)))
                                  (stx (if alias (cadr %doc) (car %doc)))
                                  (doc (if alias (caddr %doc) (cadr %doc))))
                             (assoc-cons!
                              (if alias
                                  (assoc-cons! seed (cadr alias) stx)
                                  seed)
                              name (string-join (list stx doc) "\n"))))
                         '() procedures))
                      (lambda (tag . kids)
                        (append-map
                         (lambda (kid)
                           (cond
                            ((null? kid) kid)
                            ((equal? 'procedure (car kid)) (list kid))
                            ((and (pair? (car kid))
                                  (equal? 'procedure (caar kid)))
                             kid)
                            (else '())))
                         kids))))

       (record . ,container)
       (class . ,container)
       (interface . ,container)
       (union . ,container)

       (method . ,procedure)
       (function . ,procedure)

       (*text* . ,(lambda (tag txt) txt))
       (*default* . ,(lambda (tag . kids) (cons tag kids)))))))

(define ->docbook
  (letrec ((%scheme (xpath:sxpath `(scheme)))
           (%parameters (xpath:sxpath `(parameters)))
           (%entry (xpath:sxpath `(refentry)))
           (%members
            (compose
             (xpath:select-kids identity)
             (xpath:node-self
              (xpath:node-typeof? 'member))))
           (%properties
            (compose
             (xpath:select-kids identity)
             (xpath:node-self
              (xpath:node-typeof? 'property))))
           (%functions
            (compose
             (xpath:select-kids identity)
             (xpath:node-or
              (xpath:node-self
               (xpath:node-typeof? 'method))
              (xpath:node-self
               (xpath:node-typeof? 'function)))))
           (%c-id (xpath:sxpath `(@ c:identifier *text*)))

           (fancy-quote
            (lambda (str)
              (string-append "“" str "”")))

           (ppfn
            (lambda (name args returns)
              (let ((name (string->symbol name))
                    (args (map string->symbol args))
                    (returns (map string->symbol returns)))
                (%pp `(define-values ,returns (,name ,@args))))))

           (%pp
            (lambda (obj)
              (call-with-output-string
               (lambda (port)
                 (pretty-print
                  obj
                  port
                  ;; allow one-liners
                  #:max-expr-width 79)))))

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

           (parameter
            (lambda (tag . kids)
              (let* ((node (cons tag kids))
                     (name (%name node))
                     (doc (%doc node))
                     (parent ((xpath:sxpath '(inferred @ parent *text*))
                              node))
                     (argument ((xpath:sxpath '(inferred @ argument *text*))
                                node)))
                `(tr
                  (td (@ (class "parameter_name"))
                      (para ,@name))
                  (td (@ (class "parameter_description"))
                      ,@(markdown (string-join doc " "))
                      ,@(map (lambda (p)
                               `(para "Inferred from " (code ,p)))
                             parent)
                      ,@(map (lambda (p)
                               `(para "Passed as " (code ,p)))
                             argument))))))

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
                    (members (%members kids))
                    (properties (%properties kids))
                    (functions (%functions kids))
                    (section (lambda (title content)
                               (and (not (null? content))
                                    `(refsect1 (title ,title)
                                               ,@content)))))
                (cond
                 ((null? scheme)
                  '())

                 (else
                  (cons
                   'refentry
                   (filter
                    identity
                    `((refnamediv ,@(cdar scheme))
                      ,(and (not (null? doc))
                            `(refsect1 (title "Description")
                                       ,@(markdown (string-join doc ""))))
                      ,(section "Members" members)
                      ,(section "Properties" properties)
                      ,(section "Functions" functions)))))))))

           (refsect2
            (lambda (tag . kids)
              (let ((doc (%doc (cons tag kids)))
                    (scheme (%scheme (cons tag kids)))
                    (params (let ((%params (%parameters (cons tag kids))))
                              (if (null? %params) %params (cdar %params)))))
                (list
                 tag
                 (cond
                  ((null? scheme) '())
                  ((null? doc)
                   `(refsect2 ,@(cadar scheme)
                              (para "Undocumented")
                              ,@params))
                  (else `(refsect2 ,@(cadar scheme)
                                   ,@(markdown (string-join doc ""))
                                   ,@params))))))))

    (compose
     sxml->xml
     (cute
      pre-post-order
      <>
      `((repository . ,(lambda (tag . kids)
                         `(*TOP* (*PI* xml "version=\"1.0\"") (book ,@kids))))
        (namespace . ,chapter)
        (record . ,entry)
        (class . ,entry)
        (interface . ,entry)
        (union . ,entry)
        (enumeration . ,entry)
        (bitfield . ,entry)

        (method . ,refsect2)
        (function . ,refsect2)
        (member . ,refsect2)
        (property . ,refsect2)

        (parameter . ,parameter)
        (instance-parameter . ,parameter)
        (parameters . ,(lambda (tag . kids)
                         (if (null? kids)
                             '()
                             `(parameters
                               (refsect3
                                (title "Parameters")
                                (informaltable ,@kids))))))

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
                        ,(ppfn ln
                               (append-map
                                %name
                                ((xpath:sxpath `(argument)) node))
                               (append-map
                                %name
                                ((xpath:sxpath `(return)) node))))))
                   (append (%long-name node)
                           (%name node))))))))
        (accessor
         .
         ,(lambda (tag . kids)
            (let ((node (cons tag kids))
                  (%readable
                   (xpath:sxpath `(@ readable *text*)))
                  (%writable
                   (xpath:sxpath `(@ writable *text*)))
                  (ones? (cute every (cute string= "1" <>) <>)))
              (filter identity
                      `((title ,@(%name (cons tag kids)))
                        ,(and (ones? (%readable node))
                              `(informalexample
                                (programlisting
                                 ,(%pp
                                   `(,(string->symbol (car (%long-name node)))
                                     self)))))
                        ,(and (ones? (%writable node))
                              `(informalexample
                                (programlisting
                                 ,(%pp
                                   `(set!
                                     (,(string->symbol (car (%long-name node)))
                                      self)
                                     value))))))))))

        (type . ,refname)
        (symbol . ,(lambda (tag . kids)
                     (let ((c-id (car? (%c-id (cons tag kids)))))
                       `((title ,@(%name (cons tag kids)))
                         ,(if c-id
                              `((remark "alias " (code ,c-id)))
                              '())))))

        (*text* . ,(lambda (tag txt)
                     txt))
        (*default* . ,(lambda (tag . kids)
                        (cons tag kids))))))))
