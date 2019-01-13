;; Copyright (C), 2018, 2019 Michael L. Gran

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https:;;www.gnu.org/licenses/>.
(define-module (gi)
  #:export (send
            connect))

;; This macro derives from
;; https:;;lists.gnu.org/archive/html/guile-user/2018-12/msg00037.html

(define-syntax send
  (lambda (stx)
    (syntax-case stx ()
      ((_ self (method arg ...))
       (identifier? #'method)
       (with-syntax ((method-str (symbol->string
                                  (syntax->datum #'method))))
         #'(call-method self method-str arg ...))))))

(define-syntax connect
  (lambda (stx)
    (syntax-case stx ()
      ((_ self (method arg ...))
       (identifier? #'method)
       (with-syntax ((method-str (symbol->string
                                  (syntax->datum #'method))))
         #'(signal-connect self method-str arg ...))))))

(load-extension "libguile-gi" "gir_init")

(when (defined? 'gcov-reset)
  (export gcov-reset))
(when (defined? 'gcov-dump)
  (export gcov-dump))
