;; Copyright (C) 2023 Michael L. Gran

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

(define-module (gi parser type)
  #:use-module (gi girepository)
  #:use-module (gi parser type)
  #:export (type-info-il))

(define (type-info-il type-info)
  (assert-gitypeinfo "type-info-il" type-info)
  (let* ((tag (type-info-get-tag type-info))
         (pointer? (type-info-is-pointer? type-info)))
    (cond
     ((and (type-tag-is-basic? tag) (not pointer?))
      tag)
     ((and (type-tag-is-basic? tag) pointer?)
      (cons tag 'pointer))
     ((and (eq? tag 'interface) pointer?)
      (let* ((base-info (type-info-get-interface type-info))
             (base-type (base-info-get-type base-info)))
        (cons
         (if (member base-type '(union boxed enum interface flags object union))
             (or (and=> (registered-type-info-get-type-name base-info) string->symbol)
                 (string->symbol (base-info-get-name base-info)))
             base-type)
         'pointer)))
     ((and (eq? tag 'interface) (not pointer?))
      (let* ((base-info (type-info-get-interface type-info))
             (base-type (base-info-get-type base-info)))
         (if (member base-type '(union boxed enum interface flags object union))
             (or (and=> (registered-type-info-get-type-name base-info) string->symbol)
                 (string->symbol (base-info-get-name base-info)))
             (if (eq? base-type 'callback)
                 ;; FIXME: this base-info is a callable, so the entire
                 ;; signature of the callback function could be
                 ;; extracted.
                 (cons 'callback (base-info-get-name base-info))
                 
                 base-type))))
     (else
      tag))))
