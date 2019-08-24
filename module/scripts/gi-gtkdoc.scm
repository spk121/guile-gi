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

;;; Commentary:

;; Usage: gi-gtkdoc -n NAMESPACE -v VERSION [OPTIONS]
;;
;; This program creates documentation for Guile-GI bindings to libraries,
;; that use GObject introspection, via gtkdoc.
;;
;; Example: gi-gtkdoc -n GLib -v 2.0
;;
;; Required Options are:
;;
;;   -n, --namespace=NAMESPACE   The namespace to document.
;;   -v, --version=VERSION       The version of the documented namespace.
;;
;; Additional options are:
;;
;;   -f, --format=FORMAT         Use FORMAT as output format instead of
;;                               the default, which is "html". This works
;;                               by appending FORMAT to "gtkdoc-mk" to form
;;                               a command. Supported formats are (to the
;;                               best of our knowledge): html, man, pdf.
;;   -o, --output=DIRECTORY      Store the generated output in DIRECTORY.
;;                               By default, a new temporary directory
;;                               is created and used, which may not be
;;                               what you want.
;;   -s, --skip-gir              Skip reading of the .gir XML files.
;;                               Doing so will only document functions,
;;                               that can be documented from the typelib
;;                               alone. This is not recommended.

;;; Code:

(define-module (scripts gi-gtkdoc)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 getopt-long)
  #:use-module (rnrs io ports)
  #:use-module (gi documentation)
  #:export (gtkdoc))

(define %summary "Document Guile bindings for GObject-based libraries")

(define (gtkdoc . args)
  (let* ((p (getopt-long (cons 'gtkdoc args)
                         '((namespace (single-char #\n) (value #t)
                                      (required? #t))
                           (version (single-char #\v) (value #t)
                                    (required? #t))
                           (skip-gir (single-char #\s))
                           (output (single-char #\o) (value #t))
                           (format (single-char #\f) (value #t)))))

         (namespace (option-ref p 'namespace #f))
         (version (option-ref p 'version #f))
         (skip-gir? (option-ref p 'skip-gir #f))
         (output (or
                  (option-ref p 'output #f)
                  (false-if-exception
                   (let ((f (tmpnam)))
                     (mkdir f)
                     f))))
         (fmt (option-ref p 'format "html"))
         (documentation '())
         (add-documentation!
          (lambda (port) (set! documentation (parse port documentation))))
         (docfile (string-join `(,namespace "gtkdoc" "xml") ".")))
    (unless output
      (format (current-error-port)
              "ERROR: ~a~%"
              "no output given and temporary output could not be created")
      (exit EXIT_FAILURE))

    (if (file-exists? output)
        (unless (file-is-directory? output)
          (format (current-error-port)
                  "ERROR: ~a~%"
                  "output must be a directory")
          (exit EXIT_FAILURE))
        (unless (= (system* "mkdir" "-p" output) 0)
          (format (current-error-port)
                  "ERROR: ~a~%"
                  "failed to create output directory")
          (exit EXIT_FAILURE)))

    (chdir output)

    (call-with-port (typelib namespace version) add-documentation!)
    (unless skip-gir?
      (call-with-port (gir namespace version) add-documentation!))

    (with-output-to-file docfile
      (lambda ()
        (->docbook documentation)))

    (system* (string-join `("gtkdoc-mk" ,fmt) "")
             (string-join `("Guile" ,namespace) "-")
             docfile)))

(define main gtkdoc)
