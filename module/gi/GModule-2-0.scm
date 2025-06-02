(define-module (gi GModule-2-0)
  #:use-module (gi runtime)
  #:use-module (gi GLib-2-0)
  #:export(<%GModuleError> symbol->module-error number->module-error 
           module-error->symbol module-error->number <%GModuleFlags> 
           list->module-flags number->module-flags module-flags->list 
           module-flags->number module-error gmodule:module-error 
           module-error-quark gmodule:module-error-quark module-supported? 
           gmodule:module-supported?))

(eval-when (expand load eval)
(^library
  "GModule"
  "2.0"
  '("libgmodule-2.0-0.dll"))

(^initialize)

(^untyped-enum
  '<%GModuleError>
  '%GModuleError
  '((failed . 0) (check-failed . 1)))

(^untyped-enum-conv 'module-error '%GModuleError)

(^untyped-flags
  '<%GModuleFlags>
  '%GModuleFlags
  '((lazy . 1) (local . 2) (mask . 3)))

(^untyped-flags-conv
  'module-flags
  '%GModuleFlags)

(^function
  'GModule
  #f
  'gmodule:module-error
  'module-error
  'g_module_error
  '((name . "module_error")
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GModule
  #f
  'gmodule:module-error-quark
  'module-error-quark
  'g_module_error_quark
  '((name . "module_error_quark")
    (return-val
      (name . "%return")
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GModule
  #f
  'gmodule:module-supported?
  'module-supported?
  'g_module_supported
  '((name . "module_supported")
    (return-val
      (name . "%return")
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))


)
