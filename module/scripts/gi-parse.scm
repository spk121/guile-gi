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
;; The simplest way to use this is to create a standard module for a
;; given top-level namespace.  The following creates an import library
;; for Gtk 3.0.  This file can be used with `use-modules`.  It may be
;; very large and take a significant amount of time to compile and
;; run.
;;
;; Example: gi-parse --namespace=Gtk --version=3.0
;;
;; But, since modules have dependencies, you'll have to create the
;; modules for the dependencies as well.  This can be done
;; automatically using the `--recurse` option.
;;
;; Example: gi-parse --namespace=Gtk --version=3.0 --recurse
;;
;; Alternately, this tool can create header-less files (without the
;; `define-module` header) that can be used with 'load' instead of
;; `use-modules`.
;;
;; Example: gi-parse --namespace=Gtk --version=3.0 --no-header
;;
;; Dependencies matter here as well.  If you want to create a full set
;; of headerless files, use the `--recurse` options.
;;
;; To create much smaller libraries, the tool can be passed a list of
;; specified symbols to extract. These symbols are either function
;; names like `gtk_window_new` or type names like `GtkWindow`. The
;; symbols can be passed in on the command-line using the `--symbols`
;; argument or can be taken from a files using the `--symbols-file`
;; argument.  The tool will also extract the minimal set of
;; dependencies required for those functions and type to be imported.
;;
;; Example: gi-parse --namespace=Gtk --version=3.0 --symbols="GtkWindow"
;;
;; To see the list of symbols in a given namespace, you can use the `--dump`
;; option.  The `--dump` option will not create libraries.
;;
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
;;   -H, --no-header             Don't add the 'define-module' header to
;;                               the files.
;;   --symbols="LIST"            A comma or whitespace separated list of
;;                               import symbols. Only import these symbols
;;                               and the necessary predecessors.
;;   -S, --symbol-file=FILENAME  Like --symbols, but read from a file.
;;   -t, --top-level=NAME        For modules with 'define-module' headers, this
;;                               sets the top-level to be NAME instead of `gi`.
;;   -d, --dump                  List the symbols in the given namespace.
;;                               Overrides all other output options.

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
    (no-header (single-char #\H) (value #f))
    (top-level (single-char #\t) (value #t))
    (symbols                     (value #t))
    (symbol-file (single-char #\S) (value #t))
    (dump      (single-char #\d) (value #f))
    (help      (single-char #\h) (value #f))))

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

(define* (header port lib #:key (dependencies? #t) (subs '()) (re-exports '()))
  (display "(define-module (gi " port)
  (display (safename (string-append (first lib) "-" (second lib))) port)
  (display ")" port)
  (newline port)
  (display "  #:use-module (gi runtime)" port)
  (newline port)
  (unless (null? subs)
    (for-each
     (lambda (sub)
       (display "  #:use-module (gi " port)
       (display (safename sub) port)
       (display ")" port)
       (newline port))
     subs))
  (when dependencies?
    (for-each
     (lambda (dep)
       (display "  #:use-module (gi " port)
       (display (safename dep) port)
       (display ")" port)
       (newline port))
     (namespace-immediate-dependencies (first lib))))
  (unless (null? re-exports)
    (lambda (sub)
      (display "  #:re-export(" port)
      (wrap-print subs port)
      (display ")" port)
      (newline port))))


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

(define (dump lib)
  (namespace-load (first lib) (second lib))
  (let ((syms (map info-name (namespace-infos (first lib)))))
    (for-each
     (lambda (sym)
       (display sym) (newline))
     (sort syms string<?))))

(define (contents port lib allow-list header?)
  (let ((p (new-parser))
        (exports '())
        (str (open-output-string)))
    (catch #t
      (lambda ()
        (parser-add-namespace! p (first lib) (second lib))
        (for-each (lambda (i)
                    (if (null? allow-list)
                        (parser-add-info! p i LOAD_EVERYTHING)
                        ;; else
                        (when (member (info-name i) allow-list)
                          ;; FIXME: we could handle regex symbols, perhaps.
                          (parser-add-info! p i LOAD_EVERYTHING))))
                  (namespace-infos (first lib)))
        (let ((IL (parser-take-output! p #t)))
          (for-each
           (lambda (i)
             (set! exports (append (symbols i) exports))
             (pretty-print i str)
             (newline str))
           IL))
        (when header?
          (display "  #:export(" port)
          (wrap-print (reverse exports) port)
          (display "))" port)
          (newline port)
          (newline port)
          (display "(eval-when (expand load eval)" port)
          (newline port))
        (display (get-output-string str) port)
        (newline port)
        (when header?
          (display ")" port)
          (newline port)))
      (lambda x
        (display ")" port)
        (newline port)
        (display ";;; No typelib info found for this namespace" port)
        (newline port)
        (write x)
        (newline)))))

(define (makelib lib path allow-list overwrite no-header?)
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
            (unless no-header?
              (header port lib))
            (contents port lib allow-list (not no-header?))
            (close port))))))

(define (parse . args)
  (let ((p (getopt-long (cons "gi-parse" args) command-synopsis)))
    (let ((help-wanted? (option-ref p 'help #f))
          (namespace (option-ref p 'namespace #f))
          (version (option-ref p 'version #f))
          (overwrite? (option-ref p 'force #f))
          (recurse? (option-ref p 'recurse #f))
          (no-header? (option-ref p 'no-header #f))
          (symbol-str (option-ref p 'symbols #f))
          (symbol-file (option-ref p 'symbol-file #f))
          (dump? (option-ref p 'dump #f))
          (allow-list '()))
      (cond
       (help-wanted?
        (display-help))
       (else
        (if version
            (namespace-load namespace version)
            (namespace-load namespace))
        (when dump?
          (dump (list namespace (namespace-version namespace)))
          (exit EXIT_SUCCESS))
        (when symbol-str
          (set! allow-list
            (string-split symbol-str
                          (->char-set " ,;"))))
        (when symbol-file
          (if (file-exists? symbol-file)
              (begin
                (with-input-from-file symbol-file
                  (while #t
                    (let ((cur (string-trim-both (read-delimited " ,;\t\r\n"))))
                      (if (eof-object? cur)
                          (break)
                          ;; else
                          (unless (string-null? cur)
                            (set! allow-list (cons cur allow-list))))))))
              ;; else
              (begin
                (format (current-error-port)
                        "Cannot open symbol file ~A" symbol-file)
                (exit EXIT_FAILURE))))
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
                      (makelib lib outdir allow-list overwrite? no-header?))
                    libs)
      )))
      #t)
    (exit EXIT_SUCCESS)))

(define main parse)
