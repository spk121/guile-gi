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
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module ((sxml xpath) #:prefix xpath:)
  #:use-module (gi types)
  #:use-module ((gi repository) #:select (infos require))
  #:export (info typelib ->guile-procedures.txt))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_document"))

(define namespaces
  '((c ."http://www.gtk.org/introspection/c/1.0")
    (glib . "http://www.gtk.org/introspection/glib/1.0")))

(define-method (%info (info <GIBaseInfo>))
  (let ((doc (with-output-to-string (lambda () (%document info)))))
    (and (> (string-length doc) 0) doc)))

(define-method (info (info <GIBaseInfo>))
  (and-let* ((i (%info info)))
    (xml->sxml i #:namespaces namespaces)))

(define* (typelib lib #:optional version #:key (require? #t))
  (when require? (require lib version))
  (xml->sxml
   (format #f "<namespace name=~s>~a</namespace>" lib
           (string-join (filter-map %info (infos lib)) ""))
   #:namespaces namespaces))

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

(define (car? pair)
  (and (pair? pair) (car pair)))

(define %long-name
  (xpath:sxpath `(@ long-name ,cdr)))

(define %name
  (xpath:sxpath `(@ name ,cdr)))

(define %args
  (xpath:sxpath `(argument @ name ,cdr)))

(define %returns
  (xpath:sxpath `(return @ name ,cdr)))

(define %procedures
  (xpath:sxpath `(// procedure)))

(define (%procedures-by-name name)
  (xpath:filter
   (compose
    (xpath:select-kids (xpath:node-equal? `(name ,name)))
    (xpath:select-kids (xpath:node-typeof? '@)))))

(define (->guile-procedures.txt xml)
  (let* ((procedures (%procedures xml))
         (names (sort+delete-duplicates! (%name procedures) string<=? string=?)))
    (for-each
     (lambda (name)
       (format #t "~c~a~%~%" #\page name)
       (for-each (lambda (p)
                   (let ((long-name (car? (%long-name p))))
                     (if long-name
                       (begin
                         (format #t "- Method: ~a ~a => ~a~%" long-name
                                 (%args p) (%returns p)))
                       (begin
                         (format #t "- Procedure: ~a ~a => ~a~%" name
                                 (%args p) (%returns p))))))
                 ((%procedures-by-name name) procedures)))
     names)))
