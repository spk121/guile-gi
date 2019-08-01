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
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (sxml simple)
  #:use-module (gi types)
  #:use-module ((gi repository) #:select (infos require))
  #:export (info typelib))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_document"))

(define-method (%info (info <GIBaseInfo>))
  (let ((doc (with-output-to-string (lambda () (%document info)))))
    (and (> (string-length doc) 0) doc)))

(define-method (info (info <GIBaseInfo>))
  (and-let* ((i (%info info)))
    (xml->sxml i)))

(define* (typelib lib #:optional version #:key (require? #t))
  (when require? (require lib version))
  (xml->sxml
   (format #f "<namespace name=~s>~a</namespace>" lib
           (string-join (filter-map %info (infos lib)) ""))))
