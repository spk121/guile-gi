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

(define-module (gi parser struct)
  #:use-module (gi girepository)
  #:use-module (gi parser entry)
  #:export (struct-info-parent
            parse-struct-info
            struct-info-childnre))

;; Structs have no prerequisites.  They may have a GType.
;; The field information needs to be gathered at parse time.
;; They may have methods.

(define (pre-struct-info info)
  '())

(define (type-info-to-guile-type type-info)
  (assert-gitypeinfo "type-info-to-guile-type" type-info)
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
         (if (member base-type '(struct boxed enum interface flags object union))
             (or (and=> (registered-type-info-get-type-name base-info) string->symbol)
                 (string->symbol (base-info-get-name base-info)))
             base-type)
         'pointer)))
     ((and (eq? tag 'interface) (not pointer?))
      (let* ((base-info (type-info-get-interface type-info))
             (base-type (base-info-get-type base-info)))
         (if (member base-type '(struct boxed enum interface flags object union))
             (or (and=> (registered-type-info-get-type-name base-info) string->symbol)
                 (string->symbol (base-info-get-name base-info)))
             (if (eq? base-type 'callback)
                 (let* ((base-info (type-info-get-interface type-info)))
                   (cons 'kallback (base-info-get-name base-info)))
                 
                 base-type))))
     (else
      tag))))

(define (extract-field-info struct-info)
  (assert-gistructinfo "extract-field-info" struct-info)
  (let ((field-infos (struct-info-get-fields struct-info)))
    (map-in-order
     (lambda (fi)
       (list
        (base-info-get-name fi)
        (field-info-get-flags fi)
        (field-info-get-offset fi)
        (field-info-get-size fi)
        (type-info-to-guile-type (field-info-get-type fi))))
     field-infos)))
  
(define (parse-struct-info struct-info namespace)
  (assert-gistructinfo "parse-struct-info" struct-info)
  (let ((name-symbol (string->symbol (base-info-get-name struct-info)))
        (fields (extract-field-info struct-info)))

    (list
     (make-entry
      ;; Declaration
      (list name-symbol)

      ;; Definition
      `(define-class ,name-symbol (<GStruct>)
         (field-info #:allocation #:each-subclass
                     #:init-value ,fields))
      ))))


(define (post-constant-info info)
  '())
