(use-modules ((gi girepository) #:prefix gir:)
             (srfi srfi-64))

(test-begin "girepository")

(gir:require "GObject" #:version "2.0")

(define object-info (gir:find-by-name "GObject" "Object"))

(test-assert "enumerate-versions"
  ;; The cleanup handler used to call g_list_free_full with a garbage
  ;; free function.
  (member "2.0" (gir:enumerate-versions "GLib")))

(test-equal "object-info-get-parent of a root class"
  #f
  (gir:object-info-get-parent object-info))

(test-equal "object-info-get-ref-function when there is none"
  ;; Used to pass NULL to scm_from_utf8_string.
  #f
  (gir:object-info-get-ref-function object-info))

(test-equal "callable-info-get-arg"
  ;; Was bound to callable-info-get-n-args.
  "property_name"
  (gir:base-info-get-name
   (gir:callable-info-get-arg
    (gir:object-info-find-method object-info "set_property") 0)))

(if (false-if-exception (gir:require "Gio" #:version "2.0"))
    (begin
      (test-equal "get-object-gtype-interfaces"
        ;; Every other interface used to be skipped.
        '("ActionGroup" "ActionMap")
        (sort (map gir:base-info-get-name
                   (gir:get-object-gtype-interfaces
                    (gir:registered-type-info-get-g-type
                     (gir:find-by-name "Gio" "Application"))))
              string<?))

      (test-assert "get-object-gtype-interfaces results can be collected"
        ;; The interface infos are borrowed; they used to be unreffed
        ;; when their wrappers were collected.
        (let ((gtype (gir:registered-type-info-get-g-type
                      (gir:find-by-name "Gio" "Application"))))
          (do ((i 0 (1+ i))) ((= i 100))
            (gir:get-object-gtype-interfaces gtype))
          (do ((i 0 (1+ i))) ((= i 5)) (gc))
          (= 2 (length (gir:get-object-gtype-interfaces gtype))))))
    (begin
      (test-skip 2)
      (test-assert "get-object-gtype-interfaces" #f)
      (test-assert "get-object-gtype-interfaces results can be collected" #f)))

(test-end "girepository")
