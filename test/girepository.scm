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

;;; Scheme-side conversions

(test-equal "type-info-get-tag"
  ;; Used core list-index with SRFI-1 argument order, and the tag list
  ;; lacked 'boolean.
  'utf8
  (gir:type-info-get-tag
   (gir:arg-info-get-type
    (gir:callable-info-get-arg
     (gir:object-info-find-method object-info "set_property") 0))))

(test-equal "type-tag->string" "gboolean" (gir:type-tag->string 'boolean))
(test-assert "type-tag-is-basic?" (gir:type-tag-is-basic? 'utf8))
(test-assert "type-tag-is-container?" (gir:type-tag-is-container? 'glist))
(test-assert "type-tag-is-numeric?" (gir:type-tag-is-numeric? 'double))
(test-equal "info-type->string" "object" (gir:info-type->string 'object))

(test-equal "enum-info-get-storage-type"
  'uint32
  (gir:enum-info-get-storage-type (gir:find-by-name "GObject" "ParamFlags")))

(test-assert "base-info-equal?"
  ;; Was exported but never defined.
  (gir:base-info-equal? object-info (gir:find-by-name "GObject" "Object")))

(test-equal "minor and micro versions"
  (list (@@ (gi girepository) MINOR_VERSION)
        (@@ (gi girepository) MICRO_VERSION))
  ;; Both used to return the major version.
  (list (gir:get-minor-version) (gir:get-micro-version)))

(test-equal "object-info-get-get-value-function"
  ;; Used to return the set-value function.
  '("g_value_get_param" "g_value_set_param")
  (let ((pspec (gir:find-by-name "GObject" "ParamSpec")))
    (list (gir:object-info-get-get-value-function pspec)
          (gir:object-info-get-set-value-function pspec))))

(test-assert "field-info-get-size is not the offset"
  (let ((data (gir:struct-info-find-field (gir:find-by-name "GObject" "Value") "data")))
    (and (positive? (gir:field-info-get-offset data))
         (eqv? (gir:field-info-get-size data)
               ((@@ (gi girepository) %field-info-get-size) data)))))

(test-equal "namespace names may contain X"
  ;; The character set of valid names spelled the alphabet "...WZYZ".
  #f
  (gir:is-registered? "GdkX11" "3.0"))

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
          (= 2 (length (gir:get-object-gtype-interfaces gtype)))))

      (let ((read-async (gir:interface-info-find-method
                         (gir:find-by-name "Gio" "File") "read_async")))
        (test-equal "arg-info callback accessors"
          ;; These checked the type tag via the broken type-info-get-tag
          ;; inside false-if-exception, so they always raised.
          '(async 3 #f)
          (let ((callback (gir:callable-info-get-arg read-async 2)))
            (list (gir:arg-info-get-scope callback)
                  (gir:arg-info-get-closure callback)
                  (gir:arg-info-get-destroy callback)))))

      (let* ((app (gir:find-by-name "Gio" "Application"))
             (getter (gir:object-info-find-method app "get_application_id")))
        ;; Getter/setter annotations need a recent GIR.
        (if (memq 'getter (gir:function-info-get-flags getter))
            (test-equal "function-info-get-property"
              ;; The flags check used MEMBER on the whole flags list, so
              ;; it always raised.
              "application-id"
              (gir:base-info-get-name (gir:function-info-get-property getter)))
            (begin
              (test-skip 1)
              (test-assert "function-info-get-property" #f)))

        (let ((action-group
               (let loop ((i 0))
                 (and (< i (gir:object-info-get-n-properties app))
                      (let ((p (gir:object-info-get-property app i)))
                        (if (equal? (gir:base-info-get-name p) "action-group")
                            p
                            (loop (1+ i))))))))
          (if (and action-group (gir:property-info-get-setter action-group))
              (test-equal "property-info-get-setter"
                ;; Used to return the getter.
                "set_action_group"
                (gir:base-info-get-name (gir:property-info-get-setter action-group)))
              (begin
                (test-skip 1)
                (test-assert "property-info-get-setter" #f))))))
    (begin
      (test-skip 5)
      (test-assert "get-object-gtype-interfaces" #f)
      (test-assert "get-object-gtype-interfaces results can be collected" #f)
      (test-assert "arg-info callback accessors" #f)
      (test-assert "function-info-get-property" #f)
      (test-assert "property-info-get-setter" #f)))

(test-end "girepository")
