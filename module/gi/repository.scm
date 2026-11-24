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

  #:use-module (gi types)
  #:use-module (gi core-generics)
  #:re-export (load)
  #:export (require
            infos info
            load-by-name typelib->module

            get-search-path prepend-search-path!
            get-dependencies

            LOAD_METHODS LOAD_PROPERTIES LOAD_SIGNALS
            LOAD_EVERYTHING LOAD_INFO_ONLY))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_repository"))

(define-method (load (info <GIBaseInfo>))
  (%load-info info LOAD_EVERYTHING))

(define-method (load (info <GIBaseInfo>) flags)
  (%load-info info flags))

(define* (load-by-name lib name #:optional (flags LOAD_EVERYTHING))
  (load (info lib name) flags))

;; What typelib->module has loaded into each module: an alist from a
;; library's name to #t if that included its deprecated infos, and to
;; #f if it did not.
(define %loaded-typelibs (make-weak-key-hash-table))

(define* (typelib->module module lib #:optional version #:key (deprecated? #f))
  ;; require takes a version string or nothing; it doesn't accept #f.
  (if version
      (require lib version)
      (require lib))
  (set! module (cond
                ((module? module) module)
                ((list? module) (resolve-module module))
                (else (scm-error 'wrong-type-arg "typelib->module"
                                 "not a module or module name: ~S"
                                 (list module) (list module)))))

  (unless (module-public-interface module)
    (let ((interface (make-module)))
      (set-module-name! interface (module-name module))
      (set-module-version! interface (module-version module))
      (set-module-kind! interface 'interface)
      (set-module-public-interface! module interface)))

  (let* ((loaded (hashq-ref %loaded-typelibs module '()))
         (previous (assoc lib loaded))
         ;; A module that has the rest of LIB only needs what is
         ;; deprecated added to it.
         (which (cond ((not deprecated?) #f)
                      ((and previous (not (cdr previous))) 'only)
                      (else #t))))
    (save-module-excursion
     (lambda ()
       (set-current-module module)
       (module-export! module (append-map! load (infos lib which)))))
    (hashq-set! %loaded-typelibs module
                (acons lib
                       (and (or deprecated? (and previous (cdr previous))) #t)
                       (if previous (delete previous loaded) loaded))))

  module)
