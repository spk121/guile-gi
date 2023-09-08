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
  #:use-module (gi parser field)
  #:export (struct-info-parents
            parse-struct-info
            struct-info-children))

;; Structs have no prerequisites.  They may have a GType.
;; The field information needs to be gathered at parse time.
;; They may have methods.

(define (struct-info-children info)
  '())

(define (extract-field-info struct-info)
  (assert-gistructinfo "extract-field-info" struct-info)
  (let ((field-infos (struct-info-get-fields struct-info)))
    (map-in-order field-info-il field-infos)))

(define (parse-struct-info struct-info namespace)
  (assert-gistructinfo "parse-struct-info" struct-info)
  (let ((name-symbol (string->symbol (base-info-get-name struct-info)))
        (fields (extract-field-info struct-info))
        (gtype-string (registered-type-info-get-type-name struct-info)))



    (list
     (make-entry
      ;; Declaration
      (list name-symbol)

      ;; Definition
      (cond
       ((and (null? fields) (not gtype-string))
        `(define-class ,name-symbol (<GUntypedOpaqueStruct>)))
       ((and (null? fields) gtype-string)
        `(define-class ,name-symbol (<GOpaqueStruct>)
           (gtype-string #:allocation #:class #:init-value ,gtype-string)))
       ((and (not (null? fields)) (not gtype-string))
        `(define-class ,name-symbol (<GUntypedStruct>)
           (field-info #:allocation #:each-subclass
                       #:init-value ,fields)))
       ((and (not (null? fields)) gtype-string)
        `(define-class ,name-symbol (<GStruct>)
           (gtype-string #:allocation #:class #:init-value ,gtype-string)
           (field-info #:allocation #:each-subclass
                       #:init-value ,fields))))
      ))))


(define (struct-info-children struct-info)
  (assert-gistructinfo "parse-struct-info" struct-info)
  (struct-info-get-methods struct-info))
