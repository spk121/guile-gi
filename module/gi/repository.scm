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

(define-module (gi repository)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gi core-generics)
  #:use-module (gi parser)
  #:use-module (gi runtime)
  #:re-export (<GIBaseInfo>
               LOAD_METHODS LOAD_PROPERTIES LOAD_SIGNALS
               LOAD_EVERYTHING LOAD_INFO_ONLY
               )
  #:export (require
            infos info
            load-by-name typelib->module
            reset-repository

            get-search-path prepend-search-path!
            get-dependencies
            ))

(define *parser* (new-parser))

(define (reset-repository)
  (set! *parser* (new-parser)))

(define-method (load (info <GIBaseInfo>))
  (parser-add-info! *parser* info LOAD_EVERYTHING)
  (for-each runtime-eval (parser-take-output! *parser*)))

(define-method (load (info <GIBaseInfo>) flags)
  (parser-add-info! *parser* info flags)
  (for-each runtime-eval (parser-take-output! *parser*)))

(define* (load-by-name lib name #:optional (flags LOAD_EVERYTHING))
  (parser-add-info! *parser*
                    (namespace-info-by-name lib name))
  (for-each runtime-eval (parser-take-output! *parser*)))

(define* (require namespace #:optional (version ""))
  (parser-add-namespace! *parser* namespace version))

(define (infos lib)
  (namespace-infos lib))

(define get-search-path irepository-search-path)

(define prepend-search-path! irepository-prepend-search-path)

(define get-dependencies namespace-dependencies)

(define* (typelib->module module lib #:optional version)
  (require lib version)
  (set! module (cond
                ((module? module) module)
                ((list? module) (resolve-module module))
                (else (error "not a module: ~A" module))))

  (unless (module-public-interface module)
    (let ((interface (make-module)))
      (set-module-name! interface (module-name module))
      (set-module-version! interface (module-version module))
      (set-module-kind! interface 'interface)
      (set-module-public-interface! module interface)))

  (save-module-excursion
   (lambda ()
     (set-current-module module)
     (for-each (lambda (i)
                 (parser-add-info! *parser* i LOAD_EVERYTHING))
               (namespace-infos lib))
     (let ((cmds (parser-take-output! *parser*)))
       (when #t
         (for-each (lambda (entry)
                     (unless (list? entry)
                       (error "non-list export symbol entry '~A'" entry)))
                   cmds))
       
       (let ((syms (append-map runtime-eval cmds)))
         (module-export! module syms)))))
       
  module)
