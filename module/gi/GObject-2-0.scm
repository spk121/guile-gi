(define-module (gi GObject-2-0)
  #:use-module (gi runtime)
  #:use-module (gi GLib-2-0)
  #:export(<GBinding> <GObject> dup-source binding:dup-source dup-target 
           binding:dup-target <GBindingFlags> list->binding-flags 
           number->binding-flags binding-flags->list binding-flags->number 
           get-flags binding:get-flags get-source binding:get-source 
           get-source-property binding:get-source-property get-target 
           binding:get-target get-target-property binding:get-target-property 
           unbind binding:unbind <GBindingGroup> new binding-group:new bind 
           binding-group:bind <GClosure> bind-full binding-group:bind-full 
           dup-source binding-group:dup-source set-source 
           binding-group:set-source new-object closure:new-object new-simple 
           closure:new-simple invalidate closure:invalidate <GValue> invoke! 
           closure:invoke! ref closure:ref sink closure:sink unref 
           closure:unref <%GConnectFlags> list->connect-flags 
           number->connect-flags connect-flags->list connect-flags->number 
           <GInitiallyUnowned> compat-control object:compat-control <GParam> 
           bind-property object:bind-property bind-property-full 
           object:bind-property-full force-floating object:force-floating 
           freeze-notify object:freeze-notify get-data object:get-data 
           get-property object:get-property get-qdata object:get-qdata getv 
           object:getv is-floating? object:is-floating? notify object:notify 
           notify-by-pspec object:notify-by-pspec ref object:ref ref-sink 
           object:ref-sink run-dispose object:run-dispose set-data 
           object:set-data set-property object:set-property steal-data 
           object:steal-data steal-qdata object:steal-qdata thaw-notify 
           object:thaw-notify unref object:unref watch-closure 
           object:watch-closure notify object:notify PARAM_MASK 
           PARAM_STATIC_STRINGS PARAM_USER_SHIFT <%GParamFlags> 
           list->param-flags number->param-flags param-flags->list 
           param-flags->number is-valid-name? param-spec:is-valid-name? 
           get-blurb param-spec:get-blurb get-default-value 
           param-spec:get-default-value get-name param-spec:get-name 
           get-name-quark param-spec:get-name-quark get-nick 
           param-spec:get-nick get-qdata param-spec:get-qdata 
           get-redirect-target param-spec:get-redirect-target set-qdata 
           param-spec:set-qdata sink param-spec:sink steal-qdata 
           param-spec:steal-qdata <GParamBoolean> <GParamBoxed> <GParamChar> 
           <GParamDouble> <GParamEnum> <GParamFlags> <GParamFloat> 
           <GParamGType> <GParamInt> <GParamInt64> <GParamLong> <GParamObject> 
           <GParamOverride> <GParamParam> <GParamPointer> <GParamString> 
           <GParamUChar> <GParamUInt> <GParamUInt64> <GParamULong> 
           <GParamUnichar> <GParamValueArray> <GParamVariant> SIGNAL_FLAGS_MASK 
           SIGNAL_MATCH_MASK <%GSignalFlags> list->signal-flags 
           number->signal-flags signal-flags->list signal-flags->number 
           <GSignalGroup> new signal-group:new block signal-group:block 
           connect-closure signal-group:connect-closure connect-data 
           signal-group:connect-data connect-swapped 
           signal-group:connect-swapped dup-target signal-group:dup-target 
           set-target signal-group:set-target unblock signal-group:unblock bind 
           signal-group:bind unbind signal-group:unbind <%GSignalMatchType> 
           list->signal-match-type number->signal-match-type 
           signal-match-type->list signal-match-type->number 
           TYPE_FLAG_RESERVED_ID_BIT TYPE_FUNDAMENTAL_MAX 
           TYPE_FUNDAMENTAL_SHIFT TYPE_RESERVED_BSE_FIRST 
           TYPE_RESERVED_BSE_LAST TYPE_RESERVED_GLIB_FIRST 
           TYPE_RESERVED_GLIB_LAST TYPE_RESERVED_USER_FIRST <%GTypeFlags> 
           list->type-flags number->type-flags type-flags->list 
           type-flags->number <%GTypeFundamentalFlags> 
           list->type-fundamental-flags number->type-fundamental-flags 
           type-fundamental-flags->list type-fundamental-flags->number 
           <GTypePlugin> <GTypeModule> set-name type-module:set-name unuse 
           type-module:unuse use? type-module:use? unuse type-plugin:unuse use 
           type-plugin:use VALUE_INTERNED_STRING VALUE_NOCOPY_CONTENTS copy 
           value:copy dup-object value:dup-object dup-string value:dup-string 
           <GVariant> dup-variant value:dup-variant fits-pointer? 
           value:fits-pointer? get-boolean? value:get-boolean? get-boxed 
           value:get-boxed get-char value:get-char get-double value:get-double 
           get-enum value:get-enum get-flags value:get-flags get-float 
           value:get-float get-gtype value:get-gtype get-int value:get-int 
           get-int64 value:get-int64 get-long value:get-long get-object 
           value:get-object get-param value:get-param get-pointer 
           value:get-pointer get-schar value:get-schar get-string 
           value:get-string get-uchar value:get-uchar get-uint value:get-uint 
           get-uint64 value:get-uint64 get-ulong value:get-ulong get-variant 
           value:get-variant init value:init peek-pointer value:peek-pointer 
           reset value:reset set-boolean value:set-boolean set-boxed 
           value:set-boxed set-boxed-take-ownership 
           value:set-boxed-take-ownership set-char value:set-char set-double 
           value:set-double set-enum value:set-enum set-flags value:set-flags 
           set-float value:set-float set-gtype value:set-gtype set-instance 
           value:set-instance set-int value:set-int set-int64 value:set-int64 
           set-interned-string value:set-interned-string set-long 
           value:set-long set-object value:set-object set-param value:set-param 
           set-pointer value:set-pointer set-schar value:set-schar 
           set-static-boxed value:set-static-boxed set-static-string 
           value:set-static-string set-string value:set-string 
           set-string-take-ownership value:set-string-take-ownership set-uchar 
           value:set-uchar set-uint value:set-uint set-uint64 value:set-uint64 
           set-ulong value:set-ulong set-variant value:set-variant take-boxed 
           value:take-boxed take-string value:take-string take-variant 
           value:take-variant transform? value:transform? unset value:unset 
           type-compatible? value:type-compatible? type-transformable? 
           value:type-transformable? <GValueArray> new value-array:new append 
           value-array:append copy value-array:copy get-nth value-array:get-nth 
           insert value-array:insert prepend value-array:prepend remove 
           value-array:remove sort value-array:sort boxed-copy 
           gobject:boxed-copy boxed-free gobject:boxed-free 
           cclosure-marshal-boolean--boxed-boxed 
           gobject:cclosure-marshal-boolean--boxed-boxed 
           cclosure-marshal-boolean--flags 
           gobject:cclosure-marshal-boolean--flags 
           cclosure-marshal-string--object-pointer 
           gobject:cclosure-marshal-string--object-pointer 
           cclosure-marshal-void--boolean 
           gobject:cclosure-marshal-void--boolean cclosure-marshal-void--boxed 
           gobject:cclosure-marshal-void--boxed cclosure-marshal-void--char 
           gobject:cclosure-marshal-void--char cclosure-marshal-void--double 
           gobject:cclosure-marshal-void--double cclosure-marshal-void--enum 
           gobject:cclosure-marshal-void--enum cclosure-marshal-void--flags 
           gobject:cclosure-marshal-void--flags cclosure-marshal-void--float 
           gobject:cclosure-marshal-void--float cclosure-marshal-void--int 
           gobject:cclosure-marshal-void--int cclosure-marshal-void--long 
           gobject:cclosure-marshal-void--long cclosure-marshal-void--object 
           gobject:cclosure-marshal-void--object cclosure-marshal-void--param 
           gobject:cclosure-marshal-void--param cclosure-marshal-void--pointer 
           gobject:cclosure-marshal-void--pointer cclosure-marshal-void--string 
           gobject:cclosure-marshal-void--string cclosure-marshal-void--uchar 
           gobject:cclosure-marshal-void--uchar cclosure-marshal-void--uint 
           gobject:cclosure-marshal-void--uint 
           cclosure-marshal-void--uint-pointer 
           gobject:cclosure-marshal-void--uint-pointer 
           cclosure-marshal-void--ulong gobject:cclosure-marshal-void--ulong 
           cclosure-marshal-void--variant 
           gobject:cclosure-marshal-void--variant cclosure-marshal-void--void 
           gobject:cclosure-marshal-void--void cclosure-marshal-generic 
           gobject:cclosure-marshal-generic clear-signal-handler 
           gobject:clear-signal-handler enum-to-string gobject:enum-to-string 
           flags-to-string gobject:flags-to-string gtype-get-type 
           gobject:gtype-get-type param-spec-boolean gobject:param-spec-boolean 
           param-spec-boxed gobject:param-spec-boxed param-spec-char 
           gobject:param-spec-char param-spec-double gobject:param-spec-double 
           param-spec-enum gobject:param-spec-enum param-spec-flags 
           gobject:param-spec-flags param-spec-float gobject:param-spec-float 
           param-spec-gtype gobject:param-spec-gtype param-spec-int 
           gobject:param-spec-int param-spec-int64 gobject:param-spec-int64 
           param-spec-long gobject:param-spec-long param-spec-object 
           gobject:param-spec-object param-spec-param gobject:param-spec-param 
           param-spec-pointer gobject:param-spec-pointer param-spec-string 
           gobject:param-spec-string param-spec-uchar gobject:param-spec-uchar 
           param-spec-uint gobject:param-spec-uint param-spec-uint64 
           gobject:param-spec-uint64 param-spec-ulong gobject:param-spec-ulong 
           param-spec-unichar gobject:param-spec-unichar <GVariantType> 
           param-spec-variant gobject:param-spec-variant param-value-convert? 
           gobject:param-value-convert? param-value-defaults? 
           gobject:param-value-defaults? param-value-is-valid? 
           gobject:param-value-is-valid? param-value-set-default 
           gobject:param-value-set-default param-value-validate? 
           gobject:param-value-validate? param-values-cmp 
           gobject:param-values-cmp pointer-type-register-static 
           gobject:pointer-type-register-static signal-add-emission-hook 
           gobject:signal-add-emission-hook signal-chain-from-overridden 
           gobject:signal-chain-from-overridden signal-connect-closure 
           gobject:signal-connect-closure signal-connect-closure-by-id 
           gobject:signal-connect-closure-by-id signal-emitv! 
           gobject:signal-emitv! signal-handler-block 
           gobject:signal-handler-block signal-handler-disconnect 
           gobject:signal-handler-disconnect signal-handler-find 
           gobject:signal-handler-find signal-handler-is-connected? 
           gobject:signal-handler-is-connected? signal-handler-unblock 
           gobject:signal-handler-unblock signal-handlers-block-matched 
           gobject:signal-handlers-block-matched signal-handlers-destroy 
           gobject:signal-handlers-destroy signal-handlers-disconnect-matched 
           gobject:signal-handlers-disconnect-matched 
           signal-handlers-unblock-matched 
           gobject:signal-handlers-unblock-matched signal-has-handler-pending? 
           gobject:signal-has-handler-pending? signal-is-valid-name? 
           gobject:signal-is-valid-name? signal-list-ids 
           gobject:signal-list-ids signal-lookup gobject:signal-lookup 
           signal-name gobject:signal-name signal-override-class-closure 
           gobject:signal-override-class-closure signal-parse-name 
           gobject:signal-parse-name signal-remove-emission-hook 
           gobject:signal-remove-emission-hook signal-stop-emission 
           gobject:signal-stop-emission signal-stop-emission-by-name 
           gobject:signal-stop-emission-by-name signal-type-cclosure-new 
           gobject:signal-type-cclosure-new <GSource> source-set-closure 
           gobject:source-set-closure source-set-dummy-callback 
           gobject:source-set-dummy-callback strdup-value-contents 
           gobject:strdup-value-contents type-add-class-private 
           gobject:type-add-class-private type-add-instance-private 
           gobject:type-add-instance-private type-add-interface-dynamic 
           gobject:type-add-interface-dynamic type-check-is-value-type? 
           gobject:type-check-is-value-type? type-check-value? 
           gobject:type-check-value? type-check-value-holds? 
           gobject:type-check-value-holds? type-children gobject:type-children 
           type-class-adjust-private-offset 
           gobject:type-class-adjust-private-offset type-depth 
           gobject:type-depth type-ensure gobject:type-ensure type-from-name 
           gobject:type-from-name type-fundamental gobject:type-fundamental 
           type-fundamental-next gobject:type-fundamental-next 
           type-get-instance-count gobject:type-get-instance-count 
           type-get-plugin gobject:type-get-plugin type-get-qdata 
           gobject:type-get-qdata type-get-type-registration-serial 
           gobject:type-get-type-registration-serial 
           type-interface-add-prerequisite 
           gobject:type-interface-add-prerequisite type-interface-get-plugin 
           gobject:type-interface-get-plugin 
           type-interface-instantiatable-prerequisite gobject:type-interface-instantiatable-prerequisite 
           type-interface-prerequisites gobject:type-interface-prerequisites 
           type-interfaces gobject:type-interfaces type-is-a? 
           gobject:type-is-a? type-name gobject:type-name type-next-base 
           gobject:type-next-base type-parent gobject:type-parent type-qname 
           gobject:type-qname type-register-dynamic 
           gobject:type-register-dynamic type-set-qdata gobject:type-set-qdata 
           type-test-flags? gobject:type-test-flags? value-type-compatible? 
           gobject:value-type-compatible? value-type-transformable? 
           gobject:value-type-transformable?))

(eval-when (expand load eval)
(^library
  "GObject"
  "2.0"
  '("libgobject-2.0-0.dll"))

(^initialize)

(^type 'GObject
       '<GBinding>
       "GBinding"
       "g_binding_get_type")

(^type 'GObject
       '<GObject>
       "GObject"
       "g_object_get_type")

(^function
  'GObject
  'GBinding
  'binding:dup-source
  'dup-source
  'g_binding_dup_source
  '((name . "dup_source")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBinding
  'binding:dup-target
  'dup-target
  'g_binding_dup_target
  '((name . "dup_target")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GObject
       '<GBindingFlags>
       "GBindingFlags"
       "g_binding_flags_get_type")

(^flags-conv 'binding-flags 'GBindingFlags)

(^function
  'GObject
  'GBinding
  'binding:get-flags
  'get-flags
  'g_binding_get_flags
  '((name . "get_flags")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GBindingFlags")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBinding
  'binding:get-source
  'get-source
  'g_binding_get_source
  '((name . "get_source")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBinding
  'binding:get-source-property
  'get-source-property
  'g_binding_get_source_property
  '((name . "get_source_property")
    (is-method . #t)
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
  'GObject
  'GBinding
  'binding:get-target
  'get-target
  'g_binding_get_target
  '((name . "get_target")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBinding
  'binding:get-target-property
  'get-target-property
  'g_binding_get_target_property
  '((name . "get_target_property")
    (is-method . #t)
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
  'GObject
  'GBinding
  'binding:unbind
  'unbind
  'g_binding_unbind
  '((name . "unbind")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GBinding
  'binding:flags
  'flags
  'flags)

(^property
  'GBinding
  'binding:source
  'source
  'source)

(^property
  'GBinding
  'binding:source-property
  'source-property
  'source-property)

(^property
  'GBinding
  'binding:target
  'target
  'target)

(^property
  'GBinding
  'binding:target-property
  'target-property
  'target-property)

(^type 'GObject
       '<GBindingGroup>
       "GBindingGroup"
       "g_binding_group_get_type")

(^function
  'GObject
  'GBindingGroup
  'binding-group:new
  'new
  'g_binding_group_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GBindingGroup")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBindingGroup
  'binding-group:bind
  'bind
  'g_binding_group_bind
  '((name . "bind")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "source_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "target")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "target_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "GBindingFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'GObject
  '<GClosure>
  "GClosure"
  "g_closure_get_type"
  64)

(^function
  'GObject
  'GBindingGroup
  'binding-group:bind-full
  'bind-full
  'g_binding_group_bind_with_closures
  '((name . "bind_full")
    (is-method . #t)
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "source_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "target")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "target_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "GBindingFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "transform_to")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "transform_from")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBindingGroup
  'binding-group:dup-source
  'dup-source
  'g_binding_group_dup_source
  '((name . "dup_source")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GBindingGroup
  'binding-group:set-source
  'set-source
  'g_binding_group_set_source
  '((name . "set_source")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "source")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GBindingGroup
  'binding-group:source
  'source
  'source)

(^function
  'GObject
  'GClosure
  'closure:new-object
  'new-object
  'g_closure_new_object
  '((name . "new_object")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "sizeof_closure")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "object")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GClosure")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GClosure
  'closure:new-simple
  'new-simple
  'g_closure_new_simple
  '((name . "new_simple")
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "sizeof_closure")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GClosure")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GClosure
  'closure:invalidate
  'invalidate
  'g_closure_invalidate
  '((name . "invalidate")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'GObject
  '<GValue>
  "GValue"
  "g_value_get_type"
  24)

(^function
  'GObject
  'GClosure
  'closure:invoke!
  'invoke!
  'g_closure_invoke
  '((name . "invoke")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 1)
    (s-output-len . 1)
    (c-input-len . 3)
    (c-output-len . 1)
    (pdata ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags out preallocated optional)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-input-pos . 0))
           ((name . "param_values")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GValue")
                     (item-size . 24)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 2)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GClosure
  'closure:ref
  'ref
  'g_closure_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GClosure")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GClosure
  'closure:sink
  'sink
  'g_closure_sink
  '((name . "sink")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GClosure
  'closure:unref
  'unref
  'g_closure_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^untyped-flags
  '<%GConnectFlags>
  '%GConnectFlags
  '((default . 0) (after . 1) (swapped . 2)))

(^untyped-flags-conv
  'connect-flags
  '%GConnectFlags)

(^type 'GObject
       '<GInitiallyUnowned>
       "GInitiallyUnowned"
       "g_initially_unowned_get_type")

(^function
  'GObject
  'GObject
  'object:compat-control
  'compat-control
  'g_object_compat_control
  '((name . "compat_control")
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "what")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . uint64)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^custom-type
  'GObject
  '<GParam>
  "GParam"
  "intern"
  "g_param_spec_ref_sink"
  "g_param_spec_unref")

(^function
  'GObject
  'GObject
  'object:bind-property
  'bind-property
  'g_object_bind_property
  '((name . "bind_property")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "source_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "target")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "target_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "GBindingFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GBinding")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:bind-property-full
  'bind-property-full
  'g_object_bind_property_with_closures
  '((name . "bind_property_full")
    (is-method . #t)
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "source_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "target")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "target_property")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "GBindingFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "transform_to")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "transform_from")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GBinding")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:force-floating
  'force-floating
  'g_object_force_floating
  '((name . "force_floating")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:freeze-notify
  'freeze-notify
  'g_object_freeze_notify
  '((name . "freeze_notify")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:get-data
  'get-data
  'g_object_get_data
  '((name . "get_data")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "key")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:get-property
  'get-property
  'g_object_get_property
  '((name . "get_property")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "property_name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:get-qdata
  'get-qdata
  'g_object_get_qdata
  '((name . "get_qdata")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:getv
  'getv
  'g_object_getv
  '((name . "getv")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 3)
    (pdata ((name . "n_properties")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-input-pos . 0))
           ((name . "names")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 0))
           ((name . "values")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GValue")
                     (item-size . 24)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:is-floating?
  'is-floating?
  'g_object_is_floating
  '((name . "is_floating")
    (is-method . #t)
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

(^function
  'GObject
  'GObject
  'object:notify
  'notify
  'g_object_notify
  '((name . "notify")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "property_name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:notify-by-pspec
  'notify-by-pspec
  'g_object_notify_by_pspec
  '((name . "notify_by_pspec")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:ref
  'ref
  'g_object_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:ref-sink
  'ref-sink
  'g_object_ref_sink
  '((name . "ref_sink")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:run-dispose
  'run-dispose
  'g_object_run_dispose
  '((name . "run_dispose")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:set-data
  'set-data
  'g_object_set_data
  '((name . "set_data")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "key")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:set-property
  'set-property
  'g_object_set_property
  '((name . "set_property")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "property_name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:steal-data
  'steal-data
  'g_object_steal_data
  '((name . "steal_data")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "key")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:steal-qdata
  'steal-qdata
  'g_object_steal_qdata
  '((name . "steal_qdata")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:thaw-notify
  'thaw-notify
  'g_object_thaw_notify
  '((name . "thaw_notify")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:unref
  'unref
  'g_object_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GObject
  'object:watch-closure
  'watch-closure
  'g_object_watch_closure
  '((name . "watch_closure")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'GObject
  'GObject
  'object:notify
  'notify
  'notify
  '((name . "notify")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^constant 'PARAM_MASK 255)

(^constant 'PARAM_STATIC_STRINGS 224)

(^constant 'PARAM_USER_SHIFT 8)

(^untyped-flags
  '<%GParamFlags>
  '%GParamFlags
  '((readable . 1)
    (writable . 2)
    (readwrite . 3)
    (construct . 4)
    (construct-only . 8)
    (lax-validation . 16)
    (static-name . 32)
    (private . 32)
    (static-nick . 64)
    (static-blurb . 128)
    (explicit-notify . 1073741824)
    (deprecated . 2147483648)))

(^untyped-flags-conv 'param-flags '%GParamFlags)

(^function
  'GObject
  'GParam
  'param-spec:is-valid-name?
  'is-valid-name?
  'g_param_spec_is_valid_name
  '((name . "is_valid_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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

(^function
  'GObject
  'GParam
  'param-spec:get-blurb
  'get-blurb
  'g_param_spec_get_blurb
  '((name . "get_blurb")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GParam
  'param-spec:get-default-value
  'get-default-value
  'g_param_spec_get_default_value
  '((name . "get_default_value")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValue")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GParam
  'param-spec:get-name
  'get-name
  'g_param_spec_get_name
  '((name . "get_name")
    (is-method . #t)
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
  'GObject
  'GParam
  'param-spec:get-name-quark
  'get-name-quark
  'g_param_spec_get_name_quark
  '((name . "get_name_quark")
    (is-method . #t)
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
  'GObject
  'GParam
  'param-spec:get-nick
  'get-nick
  'g_param_spec_get_nick
  '((name . "get_nick")
    (is-method . #t)
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
  'GObject
  'GParam
  'param-spec:get-qdata
  'get-qdata
  'g_param_spec_get_qdata
  '((name . "get_qdata")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GParam
  'param-spec:get-redirect-target
  'get-redirect-target
  'g_param_spec_get_redirect_target
  '((name . "get_redirect_target")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GParam
  'param-spec:set-qdata
  'set-qdata
  'g_param_spec_set_qdata
  '((name . "set_qdata")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GParam
  'param-spec:sink
  'sink
  'g_param_spec_sink
  '((name . "sink")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GParam
  'param-spec:steal-qdata
  'steal-qdata
  'g_param_spec_steal_qdata
  '((name . "steal_qdata")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GObject
       '<GParamBoolean>
       "GParamBoolean"
       "intern")

(^type 'GObject
       '<GParamBoxed>
       "GParamBoxed"
       "intern")

(^type 'GObject
       '<GParamChar>
       "GParamChar"
       "intern")

(^type 'GObject
       '<GParamDouble>
       "GParamDouble"
       "intern")

(^type 'GObject
       '<GParamEnum>
       "GParamEnum"
       "intern")

(^type 'GObject
       '<GParamFlags>
       "GParamFlags"
       "intern")

(^type 'GObject
       '<GParamFloat>
       "GParamFloat"
       "intern")

(^type 'GObject
       '<GParamGType>
       "GParamGType"
       "intern")

(^type 'GObject
       '<GParamInt>
       "GParamInt"
       "intern")

(^type 'GObject
       '<GParamInt64>
       "GParamInt64"
       "intern")

(^type 'GObject
       '<GParamLong>
       "GParamLong"
       "intern")

(^type 'GObject
       '<GParamObject>
       "GParamObject"
       "intern")

(^type 'GObject
       '<GParamOverride>
       "GParamOverride"
       "intern")

(^type 'GObject
       '<GParamParam>
       "GParamParam"
       "intern")

(^type 'GObject
       '<GParamPointer>
       "GParamPointer"
       "intern")

(^type 'GObject
       '<GParamString>
       "GParamString"
       "intern")

(^type 'GObject
       '<GParamUChar>
       "GParamUChar"
       "intern")

(^type 'GObject
       '<GParamUInt>
       "GParamUInt"
       "intern")

(^type 'GObject
       '<GParamUInt64>
       "GParamUInt64"
       "intern")

(^type 'GObject
       '<GParamULong>
       "GParamULong"
       "intern")

(^type 'GObject
       '<GParamUnichar>
       "GParamUnichar"
       "intern")

(^type 'GObject
       '<GParamValueArray>
       "GParamValueArray"
       "intern")

(^type 'GObject
       '<GParamVariant>
       "GParamVariant"
       "intern")

(^constant 'SIGNAL_FLAGS_MASK 511)

(^constant 'SIGNAL_MATCH_MASK 63)

(^untyped-flags
  '<%GSignalFlags>
  '%GSignalFlags
  '((run-first . 1)
    (run-last . 2)
    (run-cleanup . 4)
    (no-recurse . 8)
    (detailed . 16)
    (action . 32)
    (no-hooks . 64)
    (must-collect . 128)
    (deprecated . 256)
    (accumulator-first-run . 131072)))

(^untyped-flags-conv
  'signal-flags
  '%GSignalFlags)

(^type 'GObject
       '<GSignalGroup>
       "GSignalGroup"
       "g_signal_group_get_type")

(^function
  'GObject
  'GSignalGroup
  'signal-group:new
  'new
  'g_signal_group_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "target_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GSignalGroup")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:block
  'block
  'g_signal_group_block
  '((name . "block")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:connect-closure
  'connect-closure
  'g_signal_group_connect_closure
  '((name . "connect_closure")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "detailed_signal")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "after")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:connect-data
  'connect-data
  'g_signal_group_connect_data
  '((name . "connect_data")
    (is-method . #t)
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "detailed_signal")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "c_handler")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "Callback")
                    (return-val
                      (name . "%return")
                      (meta (arg-type . void) (transfer . nothing))
                      (s-direction . void)
                      (tuple . singleton)
                      (presence . required)
                      (i . 0))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "notify")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "ClosureNotify")
                    (s-input-req . 2)
                    (c-input-len . 2)
                    (pdata ((name . "data")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "closure")
                            (meta (arg-type . GBoxed)
                                  (gtype-name . "GClosure")
                                  (flags ptr in)
                                  (item-size . 64)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 1)
                            (c-input-pos . 1)
                            (s-input-pos . 1)))
                    (return-val
                      (name . "%return")
                      (meta (arg-type . void) (transfer . nothing))
                      (s-direction . void)
                      (tuple . singleton)
                      (presence . required)
                      (i . 0))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GConnectFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:connect-swapped
  'connect-swapped
  'g_signal_group_connect_swapped
  '((name . "connect_swapped")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 1)
    (c-input-len . 3)
    (pdata ((name . "detailed_signal")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "c_handler")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "Callback")
                    (return-val
                      (name . "%return")
                      (meta (arg-type . void) (transfer . nothing))
                      (s-direction . void)
                      (tuple . singleton)
                      (presence . required)
                      (i . 0))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:dup-target
  'dup-target
  'g_signal_group_dup_target
  '((name . "dup_target")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:set-target
  'set-target
  'g_signal_group_set_target
  '((name . "set_target")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "target")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GSignalGroup
  'signal-group:unblock
  'unblock
  'g_signal_group_unblock
  '((name . "unblock")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GSignalGroup
  'signal-group:target
  'target
  'target)

(^property
  'GSignalGroup
  'signal-group:target-type
  'target-type
  'target-type)

(^signal
  'GObject
  'GSignalGroup
  'signal-group:bind
  'bind
  'bind
  '((name . "bind")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'GObject
  'GSignalGroup
  'signal-group:unbind
  'unbind
  'unbind
  '((name . "unbind")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^untyped-flags
  '<%GSignalMatchType>
  '%GSignalMatchType
  '((id . 1)
    (detail . 2)
    (closure . 4)
    (func . 8)
    (data . 16)
    (unblocked . 32)))

(^untyped-flags-conv
  'signal-match-type
  '%GSignalMatchType)

(^constant 'TYPE_FLAG_RESERVED_ID_BIT 1)

(^constant 'TYPE_FUNDAMENTAL_MAX 255)

(^constant 'TYPE_FUNDAMENTAL_SHIFT 2)

(^constant 'TYPE_RESERVED_BSE_FIRST 32)

(^constant 'TYPE_RESERVED_BSE_LAST 48)

(^constant 'TYPE_RESERVED_GLIB_FIRST 22)

(^constant 'TYPE_RESERVED_GLIB_LAST 31)

(^constant 'TYPE_RESERVED_USER_FIRST 49)

(^untyped-flags
  '<%GTypeFlags>
  '%GTypeFlags
  '((none . 0)
    (abstract . 16)
    (value-abstract . 32)
    (final . 64)
    (deprecated . 128)))

(^untyped-flags-conv 'type-flags '%GTypeFlags)

(^untyped-flags
  '<%GTypeFundamentalFlags>
  '%GTypeFundamentalFlags
  '((classed . 1)
    (instantiatable . 2)
    (derivable . 4)
    (deep-derivable . 8)))

(^untyped-flags-conv
  'type-fundamental-flags
  '%GTypeFundamentalFlags)

(^type 'GObject
       '<GTypePlugin>
       "GTypePlugin"
       "g_type_plugin_get_type")

(^type 'GObject
       '<GTypeModule>
       "GTypeModule"
       "g_type_module_get_type")

(^function
  'GObject
  'GTypeModule
  'type-module:set-name
  'set-name
  'g_type_module_set_name
  '((name . "set_name")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GTypeModule
  'type-module:unuse
  'unuse
  'g_type_module_unuse
  '((name . "unuse")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GTypeModule
  'type-module:use?
  'use?
  'g_type_module_use
  '((name . "use")
    (is-method . #t)
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

(^function
  'GObject
  'GTypePlugin
  'type-plugin:unuse
  'unuse
  'g_type_plugin_unuse
  '((name . "unuse")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GTypePlugin
  'type-plugin:use
  'use
  'g_type_plugin_use
  '((name . "use")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^constant 'VALUE_INTERNED_STRING 268435456)

(^constant 'VALUE_NOCOPY_CONTENTS 134217728)

(^function
  'GObject
  'GValue
  'value:copy
  'copy
  'g_value_copy
  '((name . "copy")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "dest_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:dup-object
  'dup-object
  'g_value_dup_object
  '((name . "dup_object")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:dup-string
  'dup-string
  'g_value_dup_string
  '((name . "dup_string")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GLib '<GVariant> "GVariant" "intern")

(^function
  'GObject
  'GValue
  'value:dup-variant
  'dup-variant
  'g_value_dup_variant
  '((name . "dup_variant")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GVariant")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:fits-pointer?
  'fits-pointer?
  'g_value_fits_pointer
  '((name . "fits_pointer")
    (is-method . #t)
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

(^function
  'GObject
  'GValue
  'value:get-boolean?
  'get-boolean?
  'g_value_get_boolean
  '((name . "get_boolean")
    (is-method . #t)
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

(^function
  'GObject
  'GValue
  'value:get-boxed
  'get-boxed
  'g_value_get_boxed
  '((name . "get_boxed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-char
  'get-char
  'g_value_get_char
  '((name . "get_char")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . int8)
            (flags out)
            (item-size . 1)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-double
  'get-double
  'g_value_get_double
  '((name . "get_double")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . double)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-enum
  'get-enum
  'g_value_get_enum
  '((name . "get_enum")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-flags
  'get-flags
  'g_value_get_flags
  '((name . "get_flags")
    (is-method . #t)
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
  'GObject
  'GValue
  'value:get-float
  'get-float
  'g_value_get_float
  '((name . "get_float")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-gtype
  'get-gtype
  'g_value_get_gtype
  '((name . "get_gtype")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-int
  'get-int
  'g_value_get_int
  '((name . "get_int")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-int64
  'get-int64
  'g_value_get_int64
  '((name . "get_int64")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . int64)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-long
  'get-long
  'g_value_get_long
  '((name . "get_long")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-object
  'get-object
  'g_value_get_object
  '((name . "get_object")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GObject")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-param
  'get-param
  'g_value_get_param
  '((name . "get_param")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-pointer
  'get-pointer
  'g_value_get_pointer
  '((name . "get_pointer")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-schar
  'get-schar
  'g_value_get_schar
  '((name . "get_schar")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . int8)
            (flags out)
            (item-size . 1)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-string
  'get-string
  'g_value_get_string
  '((name . "get_string")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-uchar
  'get-uchar
  'g_value_get_uchar
  '((name . "get_uchar")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . uint8)
            (flags out)
            (item-size . 1)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-uint
  'get-uint
  'g_value_get_uint
  '((name . "get_uint")
    (is-method . #t)
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
  'GObject
  'GValue
  'value:get-uint64
  'get-uint64
  'g_value_get_uint64
  '((name . "get_uint64")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . uint64)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:get-ulong
  'get-ulong
  'g_value_get_ulong
  '((name . "get_ulong")
    (is-method . #t)
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
  'GObject
  'GValue
  'value:get-variant
  'get-variant
  'g_value_get_variant
  '((name . "get_variant")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GVariant")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:init
  'init
  'g_value_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "g_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValue")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:peek-pointer
  'peek-pointer
  'g_value_peek_pointer
  '((name . "peek_pointer")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:reset
  'reset
  'g_value_reset
  '((name . "reset")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValue")
            (flags ptr out)
            (item-size . 24)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-boolean
  'set-boolean
  'g_value_set_boolean
  '((name . "set_boolean")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_boolean")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-boxed
  'set-boxed
  'g_value_set_boxed
  '((name . "set_boxed")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_boxed")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-boxed-take-ownership
  'set-boxed-take-ownership
  'g_value_set_boxed_take_ownership
  '((name . "set_boxed_take_ownership")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_boxed")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-char
  'set-char
  'g_value_set_char
  '((name . "set_char")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_char")
            (meta (arg-type . int8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-double
  'set-double
  'g_value_set_double
  '((name . "set_double")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_double")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-enum
  'set-enum
  'g_value_set_enum
  '((name . "set_enum")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_enum")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-flags
  'set-flags
  'g_value_set_flags
  '((name . "set_flags")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_flags")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-float
  'set-float
  'g_value_set_float
  '((name . "set_float")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_float")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-gtype
  'set-gtype
  'g_value_set_gtype
  '((name . "set_gtype")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_gtype")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-instance
  'set-instance
  'g_value_set_instance
  '((name . "set_instance")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "instance")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-int
  'set-int
  'g_value_set_int
  '((name . "set_int")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_int")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-int64
  'set-int64
  'g_value_set_int64
  '((name . "set_int64")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_int64")
            (meta (arg-type . int64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-interned-string
  'set-interned-string
  'g_value_set_interned_string
  '((name . "set_interned_string")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_string")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-long
  'set-long
  'g_value_set_long
  '((name . "set_long")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_long")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-object
  'set-object
  'g_value_set_object
  '((name . "set_object")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_object")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-param
  'set-param
  'g_value_set_param
  '((name . "set_param")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "param")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-pointer
  'set-pointer
  'g_value_set_pointer
  '((name . "set_pointer")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_pointer")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-schar
  'set-schar
  'g_value_set_schar
  '((name . "set_schar")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_char")
            (meta (arg-type . int8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-static-boxed
  'set-static-boxed
  'g_value_set_static_boxed
  '((name . "set_static_boxed")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_boxed")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-static-string
  'set-static-string
  'g_value_set_static_string
  '((name . "set_static_string")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_string")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-string
  'set-string
  'g_value_set_string
  '((name . "set_string")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_string")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-string-take-ownership
  'set-string-take-ownership
  'g_value_set_string_take_ownership
  '((name . "set_string_take_ownership")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_string")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-uchar
  'set-uchar
  'g_value_set_uchar
  '((name . "set_uchar")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_uchar")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-uint
  'set-uint
  'g_value_set_uint
  '((name . "set_uint")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_uint")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-uint64
  'set-uint64
  'g_value_set_uint64
  '((name . "set_uint64")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_uint64")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-ulong
  'set-ulong
  'g_value_set_ulong
  '((name . "set_ulong")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v_ulong")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:set-variant
  'set-variant
  'g_value_set_variant
  '((name . "set_variant")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "variant")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GVariant")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:take-boxed
  'take-boxed
  'g_value_take_boxed
  '((name . "take_boxed")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_boxed")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:take-string
  'take-string
  'g_value_take_string
  '((name . "take_string")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "v_string")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:take-variant
  'take-variant
  'g_value_take_variant
  '((name . "take_variant")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "variant")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GVariant")
                  (flags ptr in nullable)
                  (transfer . everything))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:transform?
  'transform?
  'g_value_transform
  '((name . "transform")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "dest_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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

(^function
  'GObject
  'GValue
  'value:unset
  'unset
  'g_value_unset
  '((name . "unset")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValue
  'value:type-compatible?
  'type-compatible?
  'g_value_type_compatible
  '((name . "type_compatible")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "src_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  'GValue
  'value:type-transformable?
  'type-transformable?
  'g_value_type_transformable
  '((name . "type_transformable")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "src_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^sized-type
  'GObject
  '<GValueArray>
  "GValueArray"
  "g_value_array_get_type"
  24)

(^function
  'GObject
  'GValueArray
  'value-array:new
  'new
  'g_value_array_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "n_prealloced")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:append
  'append
  'g_value_array_append
  '((name . "append")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in nullable)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:copy
  'copy
  'g_value_array_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:get-nth
  'get-nth
  'g_value_array_get_nth
  '((name . "get_nth")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "index_")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValue")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:insert
  'insert
  'g_value_array_insert
  '((name . "insert")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "index_")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in nullable)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:prepend
  'prepend
  'g_value_array_prepend
  '((name . "prepend")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in nullable)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:remove
  'remove
  'g_value_array_remove
  '((name . "remove")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "index_")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  'GValueArray
  'value-array:sort
  'sort
  'g_value_array_sort_with_data
  '((name . "sort")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "compare_func")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "CompareDataFunc")
                    (s-input-opt . 3)
                    (c-input-len . 3)
                    (pdata ((name . "a")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . optional)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "b")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . optional)
                            (i . 1)
                            (c-input-pos . 1)
                            (s-input-pos . 1))
                           ((name . "user_data")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . optional)
                            (i . 2)
                            (c-input-pos . 2)
                            (s-input-pos . 2)))
                    (return-val
                      (name . "%return")
                      (meta (arg-type . int32)
                            (flags out)
                            (item-size . 4)
                            (transfer . nothing))
                      (s-direction . output)
                      (tuple . singleton)
                      (presence . required)
                      (i . 0))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GValueArray")
            (flags ptr out)
            (item-size . 24)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:boxed-copy
  'boxed-copy
  'g_boxed_copy
  '((name . "boxed_copy")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "boxed_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "src_boxed")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:boxed-free
  'boxed-free
  'g_boxed_free
  '((name . "boxed_free")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "boxed_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "boxed")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-boolean--boxed-boxed
  'cclosure-marshal-boolean--boxed-boxed
  'g_cclosure_marshal_BOOLEAN__BOXED_BOXED
  '((name . "cclosure_marshal_BOOLEAN__BOXED_BOXED")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-boolean--flags
  'cclosure-marshal-boolean--flags
  'g_cclosure_marshal_BOOLEAN__FLAGS
  '((name . "cclosure_marshal_BOOLEAN__FLAGS")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-string--object-pointer
  'cclosure-marshal-string--object-pointer
  'g_cclosure_marshal_STRING__OBJECT_POINTER
  '((name .
          "cclosure_marshal_STRING__OBJECT_POINTER")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--boolean
  'cclosure-marshal-void--boolean
  'g_cclosure_marshal_VOID__BOOLEAN
  '((name . "cclosure_marshal_VOID__BOOLEAN")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--boxed
  'cclosure-marshal-void--boxed
  'g_cclosure_marshal_VOID__BOXED
  '((name . "cclosure_marshal_VOID__BOXED")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--char
  'cclosure-marshal-void--char
  'g_cclosure_marshal_VOID__CHAR
  '((name . "cclosure_marshal_VOID__CHAR")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--double
  'cclosure-marshal-void--double
  'g_cclosure_marshal_VOID__DOUBLE
  '((name . "cclosure_marshal_VOID__DOUBLE")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--enum
  'cclosure-marshal-void--enum
  'g_cclosure_marshal_VOID__ENUM
  '((name . "cclosure_marshal_VOID__ENUM")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--flags
  'cclosure-marshal-void--flags
  'g_cclosure_marshal_VOID__FLAGS
  '((name . "cclosure_marshal_VOID__FLAGS")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--float
  'cclosure-marshal-void--float
  'g_cclosure_marshal_VOID__FLOAT
  '((name . "cclosure_marshal_VOID__FLOAT")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--int
  'cclosure-marshal-void--int
  'g_cclosure_marshal_VOID__INT
  '((name . "cclosure_marshal_VOID__INT")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--long
  'cclosure-marshal-void--long
  'g_cclosure_marshal_VOID__LONG
  '((name . "cclosure_marshal_VOID__LONG")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--object
  'cclosure-marshal-void--object
  'g_cclosure_marshal_VOID__OBJECT
  '((name . "cclosure_marshal_VOID__OBJECT")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--param
  'cclosure-marshal-void--param
  'g_cclosure_marshal_VOID__PARAM
  '((name . "cclosure_marshal_VOID__PARAM")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--pointer
  'cclosure-marshal-void--pointer
  'g_cclosure_marshal_VOID__POINTER
  '((name . "cclosure_marshal_VOID__POINTER")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--string
  'cclosure-marshal-void--string
  'g_cclosure_marshal_VOID__STRING
  '((name . "cclosure_marshal_VOID__STRING")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--uchar
  'cclosure-marshal-void--uchar
  'g_cclosure_marshal_VOID__UCHAR
  '((name . "cclosure_marshal_VOID__UCHAR")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--uint
  'cclosure-marshal-void--uint
  'g_cclosure_marshal_VOID__UINT
  '((name . "cclosure_marshal_VOID__UINT")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--uint-pointer
  'cclosure-marshal-void--uint-pointer
  'g_cclosure_marshal_VOID__UINT_POINTER
  '((name . "cclosure_marshal_VOID__UINT_POINTER")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--ulong
  'cclosure-marshal-void--ulong
  'g_cclosure_marshal_VOID__ULONG
  '((name . "cclosure_marshal_VOID__ULONG")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--variant
  'cclosure-marshal-void--variant
  'g_cclosure_marshal_VOID__VARIANT
  '((name . "cclosure_marshal_VOID__VARIANT")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-void--void
  'cclosure-marshal-void--void
  'g_cclosure_marshal_VOID__VOID
  '((name . "cclosure_marshal_VOID__VOID")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:cclosure-marshal-generic
  'cclosure-marshal-generic
  'g_cclosure_marshal_generic
  '((name . "cclosure_marshal_generic")
    (s-input-req . 4)
    (s-input-opt . 2)
    (c-input-len . 6)
    (pdata ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_gvalue")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "n_param_values")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "invocation_hint")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "marshal_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:clear-signal-handler
  'clear-signal-handler
  'g_clear_signal_handler
  '((name . "clear_signal_handler")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "handler_id_ptr")
            (meta (arg-type . uint32)
                  (flags ptr in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:enum-to-string
  'enum-to-string
  'g_enum_to_string
  '((name . "enum_to_string")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "g_enum_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:flags-to-string
  'flags-to-string
  'g_flags_to_string
  '((name . "flags_to_string")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "flags_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:gtype-get-type
  'gtype-get-type
  'g_gtype_get_type
  '((name . "gtype_get_type")
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-boolean
  'param-spec-boolean
  'g_param_spec_boolean
  '((name . "param_spec_boolean")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "default_value")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-boxed
  'param-spec-boxed
  'g_param_spec_boxed
  '((name . "param_spec_boxed")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "boxed_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-char
  'param-spec-char
  'g_param_spec_char
  '((name . "param_spec_char")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . int8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . int8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . int8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-double
  'param-spec-double
  'g_param_spec_double
  '((name . "param_spec_double")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-enum
  'param-spec-enum
  'g_param_spec_enum
  '((name . "param_spec_enum")
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "enum_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "default_value")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-flags
  'param-spec-flags
  'g_param_spec_flags
  '((name . "param_spec_flags")
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "default_value")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-float
  'param-spec-float
  'g_param_spec_float
  '((name . "param_spec_float")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-gtype
  'param-spec-gtype
  'g_param_spec_gtype
  '((name . "param_spec_gtype")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "is_a_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-int
  'param-spec-int
  'g_param_spec_int
  '((name . "param_spec_int")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-int64
  'param-spec-int64
  'g_param_spec_int64
  '((name . "param_spec_int64")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . int64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . int64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . int64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-long
  'param-spec-long
  'g_param_spec_long
  '((name . "param_spec_long")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-object
  'param-spec-object
  'g_param_spec_object
  '((name . "param_spec_object")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "object_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-param
  'param-spec-param
  'g_param_spec_param
  '((name . "param_spec_param")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "param_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-pointer
  'param-spec-pointer
  'g_param_spec_pointer
  '((name . "param_spec_pointer")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-string
  'param-spec-string
  'g_param_spec_string
  '((name . "param_spec_string")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "default_value")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-uchar
  'param-spec-uchar
  'g_param_spec_uchar
  '((name . "param_spec_uchar")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-uint
  'param-spec-uint
  'g_param_spec_uint
  '((name . "param_spec_uint")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-uint64
  'param-spec-uint64
  'g_param_spec_uint64
  '((name . "param_spec_uint64")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-ulong
  'param-spec-ulong
  'g_param_spec_ulong
  '((name . "param_spec_ulong")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "minimum")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "maximum")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "default_value")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-spec-unichar
  'param-spec-unichar
  'g_param_spec_unichar
  '((name . "param_spec_unichar")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "default_value")
            (meta (arg-type . unichar)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GLib
       '<GVariantType>
       "GVariantType"
       "g_variant_type_get_gtype")

(^function
  'GObject
  #f
  'gobject:param-spec-variant
  'param-spec-variant
  'g_param_spec_variant
  '((name . "param_spec_variant")
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "nick")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blurb")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "type")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GVariantType")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "default_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GVariant")
                  (flags ptr in nullable)
                  (transfer . everything))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GParamFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GParam")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-value-convert?
  'param-value-convert?
  'g_param_value_convert
  '((name . "param_value_convert")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "src_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "dest_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "strict_validation")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
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

(^function
  'GObject
  #f
  'gobject:param-value-defaults?
  'param-value-defaults?
  'g_param_value_defaults
  '((name . "param_value_defaults")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:param-value-is-valid?
  'param-value-is-valid?
  'g_param_value_is_valid
  '((name . "param_value_is_valid")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:param-value-set-default
  'param-value-set-default
  'g_param_value_set_default
  '((name . "param_value_set_default")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:param-value-validate?
  'param-value-validate?
  'g_param_value_validate
  '((name . "param_value_validate")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:param-values-cmp
  'param-values-cmp
  'g_param_values_cmp
  '((name . "param_values_cmp")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "pspec")
            (meta (arg-type . GObject)
                  (gtype-name . "GParam")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value1")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "value2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:pointer-type-register-static
  'pointer-type-register-static
  'g_pointer_type_register_static
  '((name . "pointer_type_register_static")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-add-emission-hook
  'signal-add-emission-hook
  'g_signal_add_emission_hook
  '((name . "signal_add_emission_hook")
    (s-input-req . 3)
    (s-input-opt . 2)
    (c-input-len . 5)
    (pdata ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "hook_func")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "SignalEmissionHook")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 4)
                    (pdata ((name . "ihint")
                            (meta (arg-type . GBoxed)
                                  (gtype-name . "void")
                                  (flags ptr in)
                                  (item-size . 12)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "n_param_values")
                            (meta (arg-type . uint32)
                                  (flags in)
                                  (item-size . 4)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . array_size)
                            (presence . implicit)
                            (i . 1)
                            (c-input-pos . 1))
                           ((name . "param_values")
                            (meta (arg-type . array)
                                  (flags ptr in sized)
                                  (length-arg . 1)
                                  (transfer . nothing)
                                  (params
                                    ((arg-type . GBoxed)
                                     (gtype-name . "GValue")
                                     (item-size . 24)
                                     (transfer . nothing))))
                            (s-direction . input)
                            (tuple . array)
                            (presence . required)
                            (i . 2)
                            (c-input-pos . 2)
                            (s-input-pos . 1))
                           ((name . "data")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . optional)
                            (i . 3)
                            (c-input-pos . 3)
                            (s-input-pos . 2)))
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
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "hook_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "data_destroy")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "DestroyNotify")
                    (s-input-opt . 1)
                    (c-input-len . 1)
                    (pdata ((name . "data")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . optional)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0)))
                    (return-val
                      (name . "%return")
                      (meta (arg-type . void) (transfer . nothing))
                      (s-direction . void)
                      (tuple . singleton)
                      (presence . required)
                      (i . 0))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
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
  'GObject
  #f
  'gobject:signal-chain-from-overridden
  'signal-chain-from-overridden
  'g_signal_chain_from_overridden
  '((name . "signal_chain_from_overridden")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance_and_params")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GValue")
                     (item-size . 24)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-connect-closure
  'signal-connect-closure
  'g_signal_connect_closure
  '((name . "signal_connect_closure")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "detailed_signal")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "after")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
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
  'GObject
  #f
  'gobject:signal-connect-closure-by-id
  'signal-connect-closure-by-id
  'g_signal_connect_closure_by_id
  '((name . "signal_connect_closure_by_id")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "after")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
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
  'GObject
  #f
  'gobject:signal-emitv!
  'signal-emitv!
  'g_signal_emitv
  '((name . "signal_emitv")
    (s-input-req . 4)
    (s-output-len . 1)
    (c-input-len . 4)
    (c-output-len . 1)
    (pdata ((name . "instance_and_params")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GValue")
                     (item-size . 24)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "return_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags in out optional)
                  (item-size . 24)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (c-output-pos . 0)
            (s-input-pos . 3)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-handler-block
  'signal-handler-block
  'g_signal_handler_block
  '((name . "signal_handler_block")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "handler_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-handler-disconnect
  'signal-handler-disconnect
  'g_signal_handler_disconnect
  '((name . "signal_handler_disconnect")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "handler_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-handler-find
  'signal-handler-find
  'g_signal_handler_find
  '((name . "signal_handler_find")
    (s-input-req . 4)
    (s-input-opt . 3)
    (c-input-len . 7)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mask")
            (meta (arg-type . flags)
                  (qualified-name . "%GSignalMatchType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "func")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
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
  'GObject
  #f
  'gobject:signal-handler-is-connected?
  'signal-handler-is-connected?
  'g_signal_handler_is_connected
  '((name . "signal_handler_is_connected")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "handler_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:signal-handler-unblock
  'signal-handler-unblock
  'g_signal_handler_unblock
  '((name . "signal_handler_unblock")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "handler_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-handlers-block-matched
  'signal-handlers-block-matched
  'g_signal_handlers_block_matched
  '((name . "signal_handlers_block_matched")
    (s-input-req . 4)
    (s-input-opt . 3)
    (c-input-len . 7)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mask")
            (meta (arg-type . flags)
                  (qualified-name . "%GSignalMatchType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "func")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
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
  'GObject
  #f
  'gobject:signal-handlers-destroy
  'signal-handlers-destroy
  'g_signal_handlers_destroy
  '((name . "signal_handlers_destroy")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-handlers-disconnect-matched
  'signal-handlers-disconnect-matched
  'g_signal_handlers_disconnect_matched
  '((name . "signal_handlers_disconnect_matched")
    (s-input-req . 4)
    (s-input-opt . 3)
    (c-input-len . 7)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mask")
            (meta (arg-type . flags)
                  (qualified-name . "%GSignalMatchType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "func")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
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
  'GObject
  #f
  'gobject:signal-handlers-unblock-matched
  'signal-handlers-unblock-matched
  'g_signal_handlers_unblock_matched
  '((name . "signal_handlers_unblock_matched")
    (s-input-req . 4)
    (s-input-opt . 3)
    (c-input-len . 7)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mask")
            (meta (arg-type . flags)
                  (qualified-name . "%GSignalMatchType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "func")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6)))
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
  'GObject
  #f
  'gobject:signal-has-handler-pending?
  'signal-has-handler-pending?
  'g_signal_has_handler_pending
  '((name . "signal_has_handler_pending")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "may_be_blocked")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
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

(^function
  'GObject
  #f
  'gobject:signal-is-valid-name?
  'signal-is-valid-name?
  'g_signal_is_valid_name
  '((name . "signal_is_valid_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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

(^function
  'GObject
  #f
  'gobject:signal-list-ids
  'signal-list-ids
  'g_signal_list_ids
  '((name . "signal_list_ids")
    (s-input-req . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "itype")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_ids")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out sized)
            (length-arg . 1)
            (transfer . everything)
            (params
              ((arg-type . uint32)
               (item-size . 4)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-lookup
  'signal-lookup
  'g_signal_lookup
  '((name . "signal_lookup")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "itype")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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
  'GObject
  #f
  'gobject:signal-name
  'signal-name
  'g_signal_name
  '((name . "signal_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-override-class-closure
  'signal-override-class-closure
  'g_signal_override_class_closure
  '((name . "signal_override_class_closure")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "instance_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "class_closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-parse-name
  'signal-parse-name
  'g_signal_parse_name
  '((name . "signal_parse_name")
    (s-input-req . 3)
    (s-output-len . 2)
    (c-input-len . 3)
    (c-output-len . 2)
    (pdata ((name . "detailed_signal")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "itype")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "signal_id_p")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "detail_p")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "force_detail_quark")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 2)
            (s-input-pos . 2)))
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

(^function
  'GObject
  #f
  'gobject:signal-remove-emission-hook
  'signal-remove-emission-hook
  'g_signal_remove_emission_hook
  '((name . "signal_remove_emission_hook")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "hook_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-stop-emission
  'signal-stop-emission
  'g_signal_stop_emission
  '((name . "signal_stop_emission")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "signal_id")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "detail")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-stop-emission-by-name
  'signal-stop-emission-by-name
  'g_signal_stop_emission_by_name
  '((name . "signal_stop_emission_by_name")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance")
            (meta (arg-type . GObject)
                  (gtype-name . "GObject")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "detailed_signal")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:signal-type-cclosure-new
  'signal-type-cclosure-new
  'g_signal_type_cclosure_new
  '((name . "signal_type_cclosure_new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "itype")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "struct_offset")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GClosure")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'GLib
  '<GSource>
  "GSource"
  "g_source_get_type"
  96)

(^function
  'GObject
  #f
  'gobject:source-set-closure
  'source-set-closure
  'g_source_set_closure
  '((name . "source_set_closure")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "source")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GSource")
                  (flags ptr in)
                  (item-size . 96)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "closure")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GClosure")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:source-set-dummy-callback
  'source-set-dummy-callback
  'g_source_set_dummy_callback
  '((name . "source_set_dummy_callback")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "source")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GSource")
                  (flags ptr in)
                  (item-size . 96)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:strdup-value-contents
  'strdup-value-contents
  'g_strdup_value_contents
  '((name . "strdup_value_contents")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-add-class-private
  'type-add-class-private
  'g_type_add_class_private
  '((name . "type_add_class_private")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "class_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "private_size")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-add-instance-private
  'type-add-instance-private
  'g_type_add_instance_private
  '((name . "type_add_instance_private")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "class_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "private_size")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-add-interface-dynamic
  'type-add-interface-dynamic
  'g_type_add_interface_dynamic
  '((name . "type_add_interface_dynamic")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "instance_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "interface_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "plugin")
            (meta (arg-type . GInterface)
                  (gtype-name . "GTypePlugin")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-check-is-value-type?
  'type-check-is-value-type?
  'g_type_check_is_value_type
  '((name . "type_check_is_value_type")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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

(^function
  'GObject
  #f
  'gobject:type-check-value?
  'type-check-value?
  'g_type_check_value
  '((name . "type_check_value")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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

(^function
  'GObject
  #f
  'gobject:type-check-value-holds?
  'type-check-value-holds?
  'g_type_check_value_holds
  '((name . "type_check_value_holds")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags ptr in)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:type-children
  'type-children
  'g_type_children
  '((name . "type_children")
    (s-input-req . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_children")
            (meta (arg-type . uint32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out sized)
            (length-arg . 1)
            (transfer . everything)
            (params
              ((arg-type . GType)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-class-adjust-private-offset
  'type-class-adjust-private-offset
  'g_type_class_adjust_private_offset
  '((name . "type_class_adjust_private_offset")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "g_class")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "private_size_or_offset")
            (meta (arg-type . int32)
                  (flags ptr in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-depth
  'type-depth
  'g_type_depth
  '((name . "type_depth")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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
  'GObject
  #f
  'gobject:type-ensure
  'type-ensure
  'g_type_ensure
  '((name . "type_ensure")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-from-name
  'type-from-name
  'g_type_from_name
  '((name . "type_from_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-fundamental
  'type-fundamental
  'g_type_fundamental
  '((name . "type_fundamental")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type_id")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-fundamental-next
  'type-fundamental-next
  'g_type_fundamental_next
  '((name . "type_fundamental_next")
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-get-instance-count
  'type-get-instance-count
  'g_type_get_instance_count
  '((name . "type_get_instance_count")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-get-plugin
  'type-get-plugin
  'g_type_get_plugin
  '((name . "type_get_plugin")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GInterface)
            (gtype-name . "GTypePlugin")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-get-qdata
  'type-get-qdata
  'g_type_get_qdata
  '((name . "type_get_qdata")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . pointer)
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-get-type-registration-serial
  'type-get-type-registration-serial
  'g_type_get_type_registration_serial
  '((name . "type_get_type_registration_serial")
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
  'GObject
  #f
  'gobject:type-interface-add-prerequisite
  'type-interface-add-prerequisite
  'g_type_interface_add_prerequisite
  '((name . "type_interface_add_prerequisite")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "interface_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "prerequisite_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-interface-get-plugin
  'type-interface-get-plugin
  'g_type_interface_get_plugin
  '((name . "type_interface_get_plugin")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "instance_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "interface_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GInterface)
            (gtype-name . "GTypePlugin")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-interface-instantiatable-prerequisite
  'type-interface-instantiatable-prerequisite
  'g_type_interface_instantiatable_prerequisite
  '((name .
          "type_interface_instantiatable_prerequisite")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "interface_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-interface-prerequisites
  'type-interface-prerequisites
  'g_type_interface_prerequisites
  '((name . "type_interface_prerequisites")
    (s-input-req . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "interface_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_prerequisites")
            (meta (arg-type . uint32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out sized)
            (length-arg . 1)
            (transfer . everything)
            (params
              ((arg-type . GType)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-interfaces
  'type-interfaces
  'g_type_interfaces
  '((name . "type_interfaces")
    (s-input-req . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_interfaces")
            (meta (arg-type . uint32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out sized)
            (length-arg . 1)
            (transfer . everything)
            (params
              ((arg-type . GType)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-is-a?
  'type-is-a?
  'g_type_is_a
  '((name . "type_is_a")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "is_a_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:type-name
  'type-name
  'g_type_name
  '((name . "type_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-next-base
  'type-next-base
  'g_type_next_base
  '((name . "type_next_base")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "leaf_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "root_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-parent
  'type-parent
  'g_type_parent
  '((name . "type_parent")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-qname
  'type-qname
  'g_type_qname
  '((name . "type_qname")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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
  'GObject
  #f
  'gobject:type-register-dynamic
  'type-register-dynamic
  'g_type_register_dynamic
  '((name . "type_register_dynamic")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "parent_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "type_name")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "plugin")
            (meta (arg-type . GInterface)
                  (gtype-name . "GTypePlugin")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "flags")
            (meta (arg-type . flags)
                  (qualified-name . "%GTypeFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-set-qdata
  'type-set-qdata
  'g_type_set_qdata
  '((name . "type_set_qdata")
    (s-input-req . 2)
    (s-input-opt . 1)
    (c-input-len . 3)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "quark")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GObject
  #f
  'gobject:type-test-flags?
  'type-test-flags?
  'g_type_test_flags
  '((name . "type_test_flags")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "flags")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:value-type-compatible?
  'value-type-compatible?
  'g_value_type_compatible
  '((name . "value_type_compatible")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "src_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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

(^function
  'GObject
  #f
  'gobject:value-type-transformable?
  'value-type-transformable?
  'g_value_type_transformable
  '((name . "value_type_transformable")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "src_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
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
