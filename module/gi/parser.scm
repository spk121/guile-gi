;; Copyright (C) 2022 Michael L. Gran

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

(define-module (gi parser)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<GIBaseInfo>
            irepository-search-path
            irepository-prepend-search-path

            namespace-load
            namespace-version
            namespace-shared-library
            namespace-infos
            namespace-info-by-name
            namespace-dependencies
            namespace-immediate-dependencies

            info-name
            info-namespace
            
            new-parser
            parser-add-namespace!
            parser-add-info!
            parser-take-output!

            LOAD_METHODS LOAD_PROPERTIES LOAD_SIGNALS
            LOAD_EVERYTHING LOAD_INFO_ONLY))

(eval-when (expand load eval)
  (load-extension "libguile-giparse" "gig_init_parser"))

(define (irepository-search-path)
  "Returns a list of paths where we search for irepository typelib
files."
  (%irepository-search-path))

(define (irepository-prepend-search-path dir)
  "Returns a list of paths where we search for irepository typelib
files."
  (%irepository-prepend-search-path dir))

(define* (namespace-load namespace #:optional (version ""))
  "Force the namespace to be loaded if it isn't already.
Throws on failure."
  (%namespace-load namespace version))

(define (namespace-version namespace)
  "Returns, as a string, the version of a given loaded namespace.
Throws an error if the namespace hasn't been loaded."
  (%namespace-version namespace))

(define (namespace-shared-library namespace)
  "Returns, as a list of strings, the paths to the shared C libraries
associated with the namespace. The list may be empty. Throws an error
if the namespace hasn't been loaded"
  (%namespace-shared-library namespace))

(define (namespace-infos namespace)
  "Returns, as a list of <GIBaseInfo> objects, all the metadata for a
given loaded namespace.

Throws an error if the namespace isn't loaded."
  (%namespace-infos namespace))

(define (namespace-info-by-name namespace name)
  "Searches for a metadata in the namespace. If found,
returns a <GIBaseInfo>. Otherwise, returns #f."
  (%namespace-info-by-name namespace name))

(define (namespace-dependencies namespace)
  "Returns a list of all versioned dependencies for this namespace."
   (%namespace-dependencies namespace))

(define (namespace-immediate-dependencies namespace)
  "Returns a list of the immediate versioned dependencies for this namespace."
   (%namespace-immediate-dependencies namespace))

(define (info-name info)
  "Returns, as a string, the internal name field of a <GIBaseInfo> object,
which is often the name of an associated C function or a GObject type."
  (%info-name info))

(define (info-namespace info)
  "Returns, as a string, the internal namespace field of a
<GIBaseInfo> object."
  (%info-namespace info))

(define (new-parser)
  "Returns a new parser state object."

  ;; Sometimes you just get very tired of GOOPS nonsense.
  (vector
   'parser
   '()                                  ; list of GType #'s
   '()                                  ; library info
   '()))                                ; type and func info

(define (is-parser? x)
  (and (vector? x)
       (= (vector-length x) 4)
       (eq? 'parser (vector-ref x 0))))

(define* (parser-add-namespace! parser namespace #:optional (version ""))
    "Force the namespace to be loaded if it isn't already. Records
information about the opened namespace in the parser state object.

Returns value is unspecified. Throws on failure."
    (unless (is-parser? parser)
      (scm-error 'wrong-type-arg "parser-add-namespace!" "not a parser state object: ~S"
                 (list parser) (list parser)))
    
    (let ((lst (namespace-load namespace version)))
      (vector-set! parser 2
                   (cons (list '^library
                               namespace
                               (namespace-version namespace)
                               (namespace-shared-library namespace))
                         (vector-ref parser 2)))))

(define* (parser-add-info! parser info #:optional (flags LOAD_EVERYTHING))
  "Adds <GIBaseInfo> to the parser."
    (unless (is-parser? parser)
      (scm-error 'wrong-type-arg "parser-add-info!" "not a parser state object: ~S"
                 (list parser) (list parser)))

    (%parser-add-info! parser info flags))

(define (qwote output)
  (map (lambda (cmd)
         (cons
          (car cmd)
          (map (lambda (entry)
                 (cond
                  ((or (symbol? entry) (list? entry))
                   (list 'quote entry))
                  (else
                   entry)))
               (cdr cmd))))
       output))
            
(define* (parser-take-output! parser #:optional (quote? #f))
  "Returns a list of procedure calls that the runtime can use to
dynamically load types and functions. Removes that information from
the parser state object."

  (unless (is-parser? parser)
    (scm-error 'wrong-type-arg "parser-add-info!" "not a parser state object: ~S"
               (list parser) (list parser)))

  (let ((output (append
                 (reverse (vector-ref parser 2))
                 (list (list '^initialize))
                 (reverse (vector-ref parser 3)))))
    ;; (vector-set! parser 2 '())
    (vector-set! parser 3 '())
    (if quote?
        (qwote output)
        output)))
