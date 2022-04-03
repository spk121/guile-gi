;; Copyright (C) 2019, 2022 Michael L. Gran

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

;;; Commentary:

;; Usage: gi-parse -n NAMESPACE -v VERSION [OPTIONS]
;;
;; This program creates modules for Guile-GI bindings to libraries,
;; that use GObject introspection.
;;
;; Example: gi-parse -n GLib -v 2.0
;;
;; Required options are:
;;
;;   -n, --namespace=NAMESPACE   The namespace of the module.
;;   -v, --version=VERSION       The version of the documented namespace.
;;
;; Additional options are:
;;
;;   -f, --force                 Overwrite existing files.
;;   -o, --output=DIRECTORY      Store the generated output in DIRECTORY.
;;                               By default, a new temporary directory
;;                               is created and used, which may not be
;;                               what you want.
;;   -R, --recurse               Also create modules for all the dependencies
;;                               of the given namespace.

;;; Code:

(define-module (scripts gi-parse)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs io ports)
  #:use-module (gi parser)
  #:export (parse))

(define %summary "Create Guile libraries for typelib-described libraries")

(define command-synopsis
  '((namespace (single-char #\n) (value #t) (required? #t))
    (version   (single-char #\v) (value #t))
    (output    (single-char #\o) (value #t))
    (force     (single-char #\f) (value #f))
    (recurse   (single-char #\R) (value #f))
    (help      (single-char #\h))))

(define (display-help)
  (display
   ((lambda (s)
      (substring s 1 (1- (string-length s))))
    (file-commentary (%search-load-path "scripts/gi-parse.scm")
                     "^;;; Commentary:"
                     ";;; Code:"
                     (let ((dirt (make-regexp "^;+ ?")))
                       (lambda (line)
                         (let ((m (regexp-exec dirt line)))
                           (if m (match:suffix m) ""))))))))

(define (safename str)
  (string-map
   (lambda (c)
     (cond
      ((char=? c #\.)
       #\-)
      (else c)))
   str))

(define (header port lib)
  (display "(define-module (gi " port)
  (display (safename (string-append (first lib) "-" (second lib))) port)
  (display ")" port)
  (newline port)
  (display "  #:use-module (gi runtime)" port)
  (newline port)
  (for-each
   (lambda (dep)
     (display "  #:use-module (gi " port)
     (display (safename dep) port)
     (display ")" port)
     (newline port))
   (namespace-immediate-dependencies (first lib))))


;; This is not the way to do this; the IL functions return the symbols
;; that need to be exported.  But I haven't figured out how to do the
;; export at eval/load time using information output by the IL
;; functions.
(define (symbols il)
  (let ((type (car il)))
    (define (U x) (symbol->string (second x)))
    (cond
     ((or (eq? type '^type)
          (eq? type '^sized-type)
          (eq? type '^custom-type))
      (list (U (third il))))
     ((or (eq? type '^function)
          (eq? type '^signal))
      (list (U (fourth il)) (U (fifth il))))
     ((or (eq? type '^constant)
          (eq? type '^untyped-flags)
          (eq? type '^untyped-enum))
      (list (U (second il))))
     ((or (eq? type '^enum-conv)
          (eq? type '^untyped-enum-conv))
      (let ((x (second il)))
        (list (format #f "~A->number" (U x))
              (format #f "~A->symbol" (U x))
              (format #f "number->~A" (U x))
              (format #f "symbol->~A" (U x)))))
     ((or (eq? type '^flags-conv)
          (eq? type '^untyped-flags-conv))
      (let ((x (second il)))
        (list (format #f "~a->number" (U x))
              (format #f "~a->list" (U x))
              (format #f "number->~a" (U x))
              (format #f "list->~a" (U x)))))
     (else
      '()))))

(define (wrap-print lst port)
  (let loop ((cur (car lst))
             (rest (cdr lst))
             (i 10))
    (if (and (> (+ (string-length cur) i) 78)
             (< (string-length cur) 50))
        (begin
          (newline port)
          (display "           " port)
          (loop cur rest 10))
        ;; else
        (begin
          (display cur port)
          (unless (null? rest)
            (display " " port)
            (loop (car rest) (cdr rest) (+ i 1 (string-length cur))))))))

(define (contents port lib)
  (let ((p (new-parser))
        (exports '())
        (str (open-output-string)))
    (catch #t
      (lambda ()
        (parser-add-namespace! p (first lib) (second lib))
        (for-each (lambda (i)
                    (parser-add-info! p i LOAD_EVERYTHING))
                  (namespace-infos (first lib)))
        (let ((IL (parser-take-output! p #t)))
          (for-each
           (lambda (i)
             (set! exports (append (symbols i) exports))
             (pretty-print i str)
             (newline str))
           IL))
        (display "  #:export(" port)
        (wrap-print (reverse exports) port)
        (display "))" port)
        (newline port)
        (newline port)
        (display "(eval-when (expand load eval)" port)
        (newline port)
        (display (get-output-string str) port)
        (newline port)
        (display ")" port)
        (newline port))
      (lambda x
        (display ";;; No typelib info found for this namespace" port)
        (newline port)
        (write x)
        (newline)))))

(define (makelib lib path overwrite)
  (let ((namespace (first lib))
        (version (second lib)))
    (let ((filename (string-append
                     path
                     "/"
                     (safename (string-append namespace "-" version))
                     ".scm")))
      (if (and (not overwrite) (file-exists? filename))
          (begin
            (format #t "~A already exists!\n" filename)
            #f)
          ;; else
          (let ((port (open-file filename "w")))
            (header port lib)
            (contents port lib)
            (close port))))))

(define (parse . args)
  (let ((p (getopt-long (cons "gi-parse" args) command-synopsis)))
    (let ((help-wanted? (option-ref p 'help #f))
          (namespace (option-ref p 'namespace #f))
          (version (option-ref p 'version #f))
          (overwrite? (option-ref p 'force #f))
          (recurse? (option-ref p 'recurse #f)))
      (cond
       (help-wanted?
        (display-help))
       (else
        (if version
            (namespace-load namespace version)
            (namespace-load namespace))
        (let* ((outdir (or
                        (option-ref p 'output #f)
                        (false-if-exception
                         (let ((d (getcwd)))
                           (format (current-error-port)
                                   "No output directory given, using \"~a/\" ~%"
                                   d)
                           d))))
               (dependencies (if recurse?
                                 (map
                                  (lambda (i)
                                    (string-split i #\-))
                                  (namespace-dependencies namespace))
                                 '()))
               (libs (cons (list namespace (namespace-version namespace))
                           dependencies)))
          (for-each (lambda (lib)
                      (makelib lib outdir overwrite?))
                    libs)
          (exit EXIT_SUCCESS)))))))

(define main parse)
