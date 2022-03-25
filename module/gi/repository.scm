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
            get-dependencies set-il-output-port

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
     (module-export! module (append-map! load (infos lib)))))

  module)
