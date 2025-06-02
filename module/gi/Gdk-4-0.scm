(define-module (gi Gdk-4-0)
  #:use-module (gi runtime)
  #:use-module (gi cairo-1-0)
  #:use-module (gi PangoCairo-1-0)
  #:use-module (gi Pango-1-0)
  #:use-module (gi Gio-2-0)
  #:use-module (gi GdkPixbuf-2-0)
  #:export(ACTION_ALL <GdkAnchorHints> list->anchor-hints number->anchor-hints 
           anchor-hints->list anchor-hints->number <GAppLaunchContext> 
           <GdkAppLaunchContext> <GdkDisplay> get-display 
           app-launch-context:get-display set-desktop 
           app-launch-context:set-desktop <GObject> <GIcon> set-icon 
           app-launch-context:set-icon set-icon-name 
           app-launch-context:set-icon-name set-timestamp 
           app-launch-context:set-timestamp <GdkAxisFlags> list->axis-flags 
           number->axis-flags axis-flags->list axis-flags->number <GdkAxisUse> 
           symbol->axis-use number->axis-use axis-use->symbol axis-use->number 
           BUTTON_MIDDLE BUTTON_PRIMARY BUTTON_SECONDARY <GdkEvent> 
           <GdkButtonEvent> get-button button-event:get-button CURRENT_TIME 
           <GdkDrawContext> <GdkCairoContext> <CairoContext> cairo-create 
           cairo-context:cairo-create <GdkClipboard> <GdkContentProvider> 
           get-content clipboard:get-content get-display clipboard:get-display 
           <GdkContentFormats> get-formats clipboard:get-formats is-local? 
           clipboard:is-local? <GCancellable> read-async clipboard:read-async 
           <GInputStream> <GAsyncResult> read-finish clipboard:read-finish 
           read-text-async clipboard:read-text-async read-text-finish 
           clipboard:read-text-finish read-texture-async 
           clipboard:read-texture-async <GdkPaintable> <GLoadableIcon> 
           <GdkTexture> read-texture-finish clipboard:read-texture-finish 
           read-value-async clipboard:read-value-async <GValue> 
           read-value-finish clipboard:read-value-finish set-content? 
           clipboard:set-content? set clipboard:set store-async 
           clipboard:store-async store-finish? clipboard:store-finish? changed 
           clipboard:changed <GdkContentDeserializer> get-cancellable 
           content-deserializer:get-cancellable get-gtype 
           content-deserializer:get-gtype get-input-stream 
           content-deserializer:get-input-stream get-mime-type 
           content-deserializer:get-mime-type get-priority 
           content-deserializer:get-priority get-task-data 
           content-deserializer:get-task-data get-user-data 
           content-deserializer:get-user-data get-value 
           content-deserializer:get-value return-error 
           content-deserializer:return-error return-success 
           content-deserializer:return-success set-task-data 
           content-deserializer:set-task-data new content-formats:new 
           new-for-gtype content-formats:new-for-gtype contain-gtype? 
           content-formats:contain-gtype? contain-mime-type? 
           content-formats:contain-mime-type? get-gtypes 
           content-formats:get-gtypes get-mime-types 
           content-formats:get-mime-types match? content-formats:match? 
           match-gtype content-formats:match-gtype match-mime-type 
           content-formats:match-mime-type <GString> print 
           content-formats:print ref content-formats:ref to-string 
           content-formats:to-string union content-formats:union 
           union-deserialize-gtypes content-formats:union-deserialize-gtypes 
           union-deserialize-mime-types 
           content-formats:union-deserialize-mime-types union-serialize-gtypes 
           content-formats:union-serialize-gtypes union-serialize-mime-types 
           content-formats:union-serialize-mime-types unref 
           content-formats:unref parse content-formats:parse 
           <GdkContentFormatsBuilder> new content-formats-builder:new 
           add-formats content-formats-builder:add-formats add-gtype 
           content-formats-builder:add-gtype add-mime-type 
           content-formats-builder:add-mime-type ref 
           content-formats-builder:ref to-formats 
           content-formats-builder:to-formats unref 
           content-formats-builder:unref <GBytes> new-for-bytes 
           content-provider:new-for-bytes new-for-value 
           content-provider:new-for-value new-union content-provider:new-union 
           content-changed content-provider:content-changed get-value! 
           content-provider:get-value! ref-formats content-provider:ref-formats 
           ref-storable-formats content-provider:ref-storable-formats 
           <GOutputStream> write-mime-type-async 
           content-provider:write-mime-type-async write-mime-type-finish? 
           content-provider:write-mime-type-finish? content-changed 
           content-provider:content-changed <GdkContentSerializer> 
           get-cancellable content-serializer:get-cancellable get-gtype 
           content-serializer:get-gtype get-mime-type 
           content-serializer:get-mime-type get-output-stream 
           content-serializer:get-output-stream get-priority 
           content-serializer:get-priority get-task-data 
           content-serializer:get-task-data get-user-data 
           content-serializer:get-user-data get-value 
           content-serializer:get-value return-error 
           content-serializer:return-error return-success 
           content-serializer:return-success set-task-data 
           content-serializer:set-task-data <GdkCrossingEvent> <GdkNotifyType> 
           symbol->notify-type number->notify-type notify-type->symbol 
           notify-type->number get-detail crossing-event:get-detail get-focus? 
           crossing-event:get-focus? <GdkCrossingMode> symbol->crossing-mode 
           number->crossing-mode crossing-mode->symbol crossing-mode->number 
           get-mode crossing-event:get-mode <GdkCursor> new-from-name 
           cursor:new-from-name new-from-texture cursor:new-from-texture 
           get-fallback cursor:get-fallback get-hotspot-x cursor:get-hotspot-x 
           get-hotspot-y cursor:get-hotspot-y get-name cursor:get-name 
           get-texture cursor:get-texture <GdkDNDEvent> <GdkDrop> get-drop 
           dndevent:get-drop <GdkDeleteEvent> <GdkDevice> get-caps-lock-state? 
           device:get-caps-lock-state? <GdkDeviceTool> get-device-tool 
           device:get-device-tool <PangoDirection> symbol->direction 
           number->direction direction->symbol direction->number get-direction 
           device:get-direction get-display device:get-display get-has-cursor? 
           device:get-has-cursor? <GdkModifierType> list->modifier-type 
           number->modifier-type modifier-type->list modifier-type->number 
           get-modifier-state device:get-modifier-state get-name 
           device:get-name get-num-lock-state? device:get-num-lock-state? 
           get-num-touches device:get-num-touches get-product-id 
           device:get-product-id get-scroll-lock-state? 
           device:get-scroll-lock-state? <GdkSeat> get-seat device:get-seat 
           <GdkInputSource> symbol->input-source number->input-source 
           input-source->symbol input-source->number get-source 
           device:get-source <GdkSurface> get-surface-at-position 
           device:get-surface-at-position get-timestamp device:get-timestamp 
           get-vendor-id device:get-vendor-id has-bidi-layouts? 
           device:has-bidi-layouts? changed device:changed tool-changed 
           device:tool-changed <GdkDevicePad> <GdkDevicePadFeature> 
           symbol->device-pad-feature number->device-pad-feature 
           device-pad-feature->symbol device-pad-feature->number 
           get-feature-group device-pad:get-feature-group get-group-n-modes 
           device-pad:get-group-n-modes get-n-features 
           device-pad:get-n-features get-n-groups device-pad:get-n-groups 
           get-axes device-tool:get-axes get-hardware-id 
           device-tool:get-hardware-id get-serial device-tool:get-serial 
           <GdkDeviceToolType> symbol->device-tool-type 
           number->device-tool-type device-tool-type->symbol 
           device-tool-type->number get-tool-type device-tool:get-tool-type 
           get-default display:get-default open display:open beep display:beep 
           close display:close <GdkGLContext> create-gl-context 
           display:create-gl-context device-is-grabbed? 
           display:device-is-grabbed? flush display:flush 
           get-app-launch-context display:get-app-launch-context get-clipboard 
           display:get-clipboard get-default-seat display:get-default-seat 
           <GdkMonitor> get-monitor-at-surface display:get-monitor-at-surface 
           <GListModel> get-monitors display:get-monitors get-name 
           display:get-name get-primary-clipboard display:get-primary-clipboard 
           get-setting? display:get-setting? get-startup-notification-id 
           display:get-startup-notification-id is-closed? display:is-closed? 
           is-composited? display:is-composited? is-rgba? display:is-rgba? 
           list-seats display:list-seats notify-startup-complete 
           display:notify-startup-complete prepare-gl? display:prepare-gl? 
           put-event display:put-event supports-input-shapes? 
           display:supports-input-shapes? sync display:sync translate-key 
           display:translate-key closed display:closed opened display:opened 
           seat-added display:seat-added seat-removed display:seat-removed 
           setting-changed display:setting-changed <GdkDisplayManager> get 
           display-manager:get get-default-display 
           display-manager:get-default-display list-displays 
           display-manager:list-displays open-display 
           display-manager:open-display set-default-display 
           display-manager:set-default-display display-opened 
           display-manager:display-opened <GdkDrag> <GdkDragAction> 
           list->drag-action number->drag-action drag-action->list 
           drag-action->number begin drag:begin drop-done drag:drop-done 
           get-actions drag:get-actions get-content drag:get-content get-device 
           drag:get-device get-display drag:get-display get-drag-surface 
           drag:get-drag-surface get-formats drag:get-formats 
           get-selected-action drag:get-selected-action get-surface 
           drag:get-surface set-hotspot drag:set-hotspot <GdkDragCancelReason> 
           symbol->drag-cancel-reason number->drag-cancel-reason 
           drag-cancel-reason->symbol drag-cancel-reason->number cancel 
           drag:cancel dnd-finished drag:dnd-finished drop-performed 
           drag:drop-performed is-unique? drag-action:is-unique? 
           <GdkDragSurface> present? drag-surface:present? <CairoRegion> 
           begin-frame draw-context:begin-frame end-frame 
           draw-context:end-frame get-display draw-context:get-display 
           get-frame-region draw-context:get-frame-region get-surface 
           draw-context:get-surface is-in-frame? draw-context:is-in-frame? 
           finish drop:finish get-actions drop:get-actions get-device 
           drop:get-device get-display drop:get-display get-drag drop:get-drag 
           get-formats drop:get-formats get-surface drop:get-surface read-async 
           drop:read-async read-finish drop:read-finish read-value-async 
           drop:read-value-async read-value-finish drop:read-value-finish 
           status drop:status EVENT_PROPAGATE EVENT_STOP -get-angle 
           event:-get-angle -get-center event:-get-center -get-distance 
           event:-get-distance get-axes event:get-axes get-axis event:get-axis 
           get-device event:get-device get-device-tool event:get-device-tool 
           get-display event:get-display <GdkEventSequence> get-event-sequence 
           event:get-event-sequence <GdkEventType> symbol->event-type 
           number->event-type event-type->symbol event-type->number 
           get-event-type event:get-event-type get-modifier-state 
           event:get-modifier-state get-pointer-emulated? 
           event:get-pointer-emulated? get-position event:get-position get-seat 
           event:get-seat get-surface event:get-surface get-time event:get-time 
           ref event:ref triggers-context-menu? event:triggers-context-menu? 
           unref event:unref <GdkFileList> <GFile> new-from-array 
           file-list:new-from-array new-from-list file-list:new-from-list 
           get-files file-list:get-files <GdkFocusEvent> get-in? 
           focus-event:get-in? <GdkFrameClock> begin-updating 
           frame-clock:begin-updating end-updating frame-clock:end-updating 
           <GdkFrameTimings> get-current-timings 
           frame-clock:get-current-timings get-fps frame-clock:get-fps 
           get-frame-counter frame-clock:get-frame-counter get-frame-time 
           frame-clock:get-frame-time get-history-start 
           frame-clock:get-history-start get-refresh-info 
           frame-clock:get-refresh-info get-timings frame-clock:get-timings 
           <GdkFrameClockPhase> list->frame-clock-phase 
           number->frame-clock-phase frame-clock-phase->list 
           frame-clock-phase->number request-phase frame-clock:request-phase 
           after-paint frame-clock:after-paint before-paint 
           frame-clock:before-paint flush-events frame-clock:flush-events 
           layout frame-clock:layout paint frame-clock:paint resume-events 
           frame-clock:resume-events update frame-clock:update get-complete? 
           frame-timings:get-complete? get-frame-counter 
           frame-timings:get-frame-counter get-frame-time 
           frame-timings:get-frame-time get-predicted-presentation-time 
           frame-timings:get-predicted-presentation-time get-presentation-time 
           frame-timings:get-presentation-time get-refresh-interval 
           frame-timings:get-refresh-interval ref frame-timings:ref unref 
           frame-timings:unref <GdkFullscreenMode> symbol->fullscreen-mode 
           number->fullscreen-mode fullscreen-mode->symbol 
           fullscreen-mode->number <GdkGLAPI> list->glapi number->glapi 
           glapi->list glapi->number clear-current glcontext:clear-current 
           get-current glcontext:get-current get-allowed-apis 
           glcontext:get-allowed-apis get-api glcontext:get-api 
           get-debug-enabled? glcontext:get-debug-enabled? get-display 
           glcontext:get-display get-forward-compatible? 
           glcontext:get-forward-compatible? get-required-version 
           glcontext:get-required-version get-shared-context 
           glcontext:get-shared-context get-surface glcontext:get-surface 
           get-use-es? glcontext:get-use-es? get-version glcontext:get-version 
           is-legacy? glcontext:is-legacy? is-shared? glcontext:is-shared? 
           make-current glcontext:make-current realize? glcontext:realize? 
           set-allowed-apis glcontext:set-allowed-apis set-debug-enabled 
           glcontext:set-debug-enabled set-forward-compatible 
           glcontext:set-forward-compatible set-required-version 
           glcontext:set-required-version set-use-es glcontext:set-use-es 
           <GdkGLError> symbol->glerror number->glerror glerror->symbol 
           glerror->number quark glerror:quark <GdkGLTexture> new gltexture:new 
           release gltexture:release <GdkGrabBrokenEvent> get-grab-surface 
           grab-broken-event:get-grab-surface get-implicit? 
           grab-broken-event:get-implicit? <GdkGravity> symbol->gravity 
           number->gravity gravity->symbol gravity->number KEY_0 KEY_1 KEY_2 
           KEY_3 KEY_3270_AltCursor KEY_3270_Attn KEY_3270_BackTab 
           KEY_3270_ChangeScreen KEY_3270_Copy KEY_3270_CursorBlink 
           KEY_3270_CursorSelect KEY_3270_DeleteWord KEY_3270_Duplicate 
           KEY_3270_Enter KEY_3270_EraseEOF KEY_3270_EraseInput 
           KEY_3270_ExSelect KEY_3270_FieldMark KEY_3270_Ident KEY_3270_Jump 
           KEY_3270_KeyClick KEY_3270_Left2 KEY_3270_PA1 KEY_3270_PA2 
           KEY_3270_PA3 KEY_3270_Play KEY_3270_PrintScreen KEY_3270_Quit 
           KEY_3270_Record KEY_3270_Reset KEY_3270_Right2 KEY_3270_Rule 
           KEY_3270_Setup KEY_3270_Test KEY_4 KEY_5 KEY_6 KEY_7 KEY_8 KEY_9 
           KEY_A KEY_AE KEY_Aacute KEY_Abelowdot KEY_Abreve KEY_Abreveacute 
           KEY_Abrevebelowdot KEY_Abrevegrave KEY_Abrevehook KEY_Abrevetilde 
           KEY_AccessX_Enable KEY_AccessX_Feedback_Enable KEY_Acircumflex 
           KEY_Acircumflexacute KEY_Acircumflexbelowdot KEY_Acircumflexgrave 
           KEY_Acircumflexhook KEY_Acircumflextilde KEY_AddFavorite 
           KEY_Adiaeresis KEY_Agrave KEY_Ahook KEY_Alt_L KEY_Alt_R KEY_Amacron 
           KEY_Aogonek KEY_ApplicationLeft KEY_ApplicationRight KEY_Arabic_0 
           KEY_Arabic_1 KEY_Arabic_2 KEY_Arabic_3 KEY_Arabic_4 KEY_Arabic_5 
           KEY_Arabic_6 KEY_Arabic_7 KEY_Arabic_8 KEY_Arabic_9 KEY_Arabic_ain 
           KEY_Arabic_alef KEY_Arabic_alefmaksura KEY_Arabic_beh 
           KEY_Arabic_comma KEY_Arabic_dad KEY_Arabic_dal KEY_Arabic_damma 
           KEY_Arabic_dammatan KEY_Arabic_ddal KEY_Arabic_farsi_yeh 
           KEY_Arabic_fatha KEY_Arabic_fathatan KEY_Arabic_feh 
           KEY_Arabic_fullstop KEY_Arabic_gaf KEY_Arabic_ghain KEY_Arabic_ha 
           KEY_Arabic_hah KEY_Arabic_hamza KEY_Arabic_hamza_above 
           KEY_Arabic_hamza_below KEY_Arabic_hamzaonalef KEY_Arabic_hamzaonwaw 
           KEY_Arabic_hamzaonyeh KEY_Arabic_hamzaunderalef KEY_Arabic_heh 
           KEY_Arabic_heh_doachashmee KEY_Arabic_heh_goal KEY_Arabic_jeem 
           KEY_Arabic_jeh KEY_Arabic_kaf KEY_Arabic_kasra KEY_Arabic_kasratan 
           KEY_Arabic_keheh KEY_Arabic_khah KEY_Arabic_lam 
           KEY_Arabic_madda_above KEY_Arabic_maddaonalef KEY_Arabic_meem 
           KEY_Arabic_noon KEY_Arabic_noon_ghunna KEY_Arabic_peh 
           KEY_Arabic_percent KEY_Arabic_qaf KEY_Arabic_question_mark 
           KEY_Arabic_ra KEY_Arabic_rreh KEY_Arabic_sad KEY_Arabic_seen 
           KEY_Arabic_semicolon KEY_Arabic_shadda KEY_Arabic_sheen 
           KEY_Arabic_sukun KEY_Arabic_superscript_alef KEY_Arabic_switch 
           KEY_Arabic_tah KEY_Arabic_tatweel KEY_Arabic_tcheh KEY_Arabic_teh 
           KEY_Arabic_tehmarbuta KEY_Arabic_thal KEY_Arabic_theh 
           KEY_Arabic_tteh KEY_Arabic_veh KEY_Arabic_waw KEY_Arabic_yeh 
           KEY_Arabic_yeh_baree KEY_Arabic_zah KEY_Arabic_zain KEY_Aring 
           KEY_Armenian_AT KEY_Armenian_AYB KEY_Armenian_BEN KEY_Armenian_CHA 
           KEY_Armenian_DA KEY_Armenian_DZA KEY_Armenian_E KEY_Armenian_FE 
           KEY_Armenian_GHAT KEY_Armenian_GIM KEY_Armenian_HI KEY_Armenian_HO 
           KEY_Armenian_INI KEY_Armenian_JE KEY_Armenian_KE KEY_Armenian_KEN 
           KEY_Armenian_KHE KEY_Armenian_LYUN KEY_Armenian_MEN KEY_Armenian_NU 
           KEY_Armenian_O KEY_Armenian_PE KEY_Armenian_PYUR KEY_Armenian_RA 
           KEY_Armenian_RE KEY_Armenian_SE KEY_Armenian_SHA KEY_Armenian_TCHE 
           KEY_Armenian_TO KEY_Armenian_TSA KEY_Armenian_TSO KEY_Armenian_TYUN 
           KEY_Armenian_VEV KEY_Armenian_VO KEY_Armenian_VYUN KEY_Armenian_YECH 
           KEY_Armenian_ZA KEY_Armenian_ZHE KEY_Armenian_accent 
           KEY_Armenian_amanak KEY_Armenian_apostrophe KEY_Armenian_at 
           KEY_Armenian_ayb KEY_Armenian_ben KEY_Armenian_but KEY_Armenian_cha 
           KEY_Armenian_da KEY_Armenian_dza KEY_Armenian_e KEY_Armenian_exclam 
           KEY_Armenian_fe KEY_Armenian_full_stop KEY_Armenian_ghat 
           KEY_Armenian_gim KEY_Armenian_hi KEY_Armenian_ho KEY_Armenian_hyphen 
           KEY_Armenian_ini KEY_Armenian_je KEY_Armenian_ke KEY_Armenian_ken 
           KEY_Armenian_khe KEY_Armenian_ligature_ew KEY_Armenian_lyun 
           KEY_Armenian_men KEY_Armenian_nu KEY_Armenian_o KEY_Armenian_paruyk 
           KEY_Armenian_pe KEY_Armenian_pyur KEY_Armenian_question 
           KEY_Armenian_ra KEY_Armenian_re KEY_Armenian_se 
           KEY_Armenian_separation_mark KEY_Armenian_sha KEY_Armenian_shesht 
           KEY_Armenian_tche KEY_Armenian_to KEY_Armenian_tsa KEY_Armenian_tso 
           KEY_Armenian_tyun KEY_Armenian_verjaket KEY_Armenian_vev 
           KEY_Armenian_vo KEY_Armenian_vyun KEY_Armenian_yech 
           KEY_Armenian_yentamna KEY_Armenian_za KEY_Armenian_zhe KEY_Atilde 
           KEY_AudibleBell_Enable KEY_AudioCycleTrack KEY_AudioForward 
           KEY_AudioLowerVolume KEY_AudioMedia KEY_AudioMicMute KEY_AudioMute 
           KEY_AudioNext KEY_AudioPause KEY_AudioPlay KEY_AudioPreset 
           KEY_AudioPrev KEY_AudioRaiseVolume KEY_AudioRandomPlay 
           KEY_AudioRecord KEY_AudioRepeat KEY_AudioRewind KEY_AudioStop 
           KEY_Away KEY_B KEY_Babovedot KEY_Back KEY_BackForward KEY_BackSpace 
           KEY_Battery KEY_Begin KEY_Blue KEY_Bluetooth KEY_Book 
           KEY_BounceKeys_Enable KEY_Break KEY_BrightnessAdjust 
           KEY_Byelorussian_SHORTU KEY_Byelorussian_shortu KEY_C KEY_CD KEY_CH 
           KEY_C_H KEY_C_h KEY_Cabovedot KEY_Cacute KEY_Calculator KEY_Calendar 
           KEY_Cancel KEY_Caps_Lock KEY_Ccaron KEY_Ccedilla KEY_Ccircumflex 
           KEY_Ch KEY_Clear KEY_ClearGrab KEY_Close KEY_Codeinput KEY_ColonSign 
           KEY_Community KEY_ContrastAdjust KEY_Control_L KEY_Control_R 
           KEY_Copy KEY_CruzeiroSign KEY_Cut KEY_CycleAngle KEY_Cyrillic_A 
           KEY_Cyrillic_BE KEY_Cyrillic_CHE KEY_Cyrillic_CHE_descender 
           KEY_Cyrillic_CHE_vertstroke KEY_Cyrillic_DE KEY_Cyrillic_DZHE 
           KEY_Cyrillic_E KEY_Cyrillic_EF KEY_Cyrillic_EL KEY_Cyrillic_EM 
           KEY_Cyrillic_EN KEY_Cyrillic_EN_descender KEY_Cyrillic_ER 
           KEY_Cyrillic_ES KEY_Cyrillic_GHE KEY_Cyrillic_GHE_bar 
           KEY_Cyrillic_HA KEY_Cyrillic_HARDSIGN KEY_Cyrillic_HA_descender 
           KEY_Cyrillic_I KEY_Cyrillic_IE KEY_Cyrillic_IO KEY_Cyrillic_I_macron 
           KEY_Cyrillic_JE KEY_Cyrillic_KA KEY_Cyrillic_KA_descender 
           KEY_Cyrillic_KA_vertstroke KEY_Cyrillic_LJE KEY_Cyrillic_NJE 
           KEY_Cyrillic_O KEY_Cyrillic_O_bar KEY_Cyrillic_PE KEY_Cyrillic_SCHWA 
           KEY_Cyrillic_SHA KEY_Cyrillic_SHCHA KEY_Cyrillic_SHHA 
           KEY_Cyrillic_SHORTI KEY_Cyrillic_SOFTSIGN KEY_Cyrillic_TE 
           KEY_Cyrillic_TSE KEY_Cyrillic_U KEY_Cyrillic_U_macron 
           KEY_Cyrillic_U_straight KEY_Cyrillic_U_straight_bar KEY_Cyrillic_VE 
           KEY_Cyrillic_YA KEY_Cyrillic_YERU KEY_Cyrillic_YU KEY_Cyrillic_ZE 
           KEY_Cyrillic_ZHE KEY_Cyrillic_ZHE_descender KEY_Cyrillic_a 
           KEY_Cyrillic_be KEY_Cyrillic_che KEY_Cyrillic_che_descender 
           KEY_Cyrillic_che_vertstroke KEY_Cyrillic_de KEY_Cyrillic_dzhe 
           KEY_Cyrillic_e KEY_Cyrillic_ef KEY_Cyrillic_el KEY_Cyrillic_em 
           KEY_Cyrillic_en KEY_Cyrillic_en_descender KEY_Cyrillic_er 
           KEY_Cyrillic_es KEY_Cyrillic_ghe KEY_Cyrillic_ghe_bar 
           KEY_Cyrillic_ha KEY_Cyrillic_ha_descender KEY_Cyrillic_hardsign 
           KEY_Cyrillic_i KEY_Cyrillic_i_macron KEY_Cyrillic_ie KEY_Cyrillic_io 
           KEY_Cyrillic_je KEY_Cyrillic_ka KEY_Cyrillic_ka_descender 
           KEY_Cyrillic_ka_vertstroke KEY_Cyrillic_lje KEY_Cyrillic_nje 
           KEY_Cyrillic_o KEY_Cyrillic_o_bar KEY_Cyrillic_pe KEY_Cyrillic_schwa 
           KEY_Cyrillic_sha KEY_Cyrillic_shcha KEY_Cyrillic_shha 
           KEY_Cyrillic_shorti KEY_Cyrillic_softsign KEY_Cyrillic_te 
           KEY_Cyrillic_tse KEY_Cyrillic_u KEY_Cyrillic_u_macron 
           KEY_Cyrillic_u_straight KEY_Cyrillic_u_straight_bar KEY_Cyrillic_ve 
           KEY_Cyrillic_ya KEY_Cyrillic_yeru KEY_Cyrillic_yu KEY_Cyrillic_ze 
           KEY_Cyrillic_zhe KEY_Cyrillic_zhe_descender KEY_D KEY_DOS 
           KEY_Dabovedot KEY_Dcaron KEY_Delete KEY_Display KEY_Documents 
           KEY_DongSign KEY_Down KEY_Dstroke KEY_E KEY_ENG KEY_ETH KEY_EZH 
           KEY_Eabovedot KEY_Eacute KEY_Ebelowdot KEY_Ecaron KEY_Ecircumflex 
           KEY_Ecircumflexacute KEY_Ecircumflexbelowdot KEY_Ecircumflexgrave 
           KEY_Ecircumflexhook KEY_Ecircumflextilde KEY_EcuSign KEY_Ediaeresis 
           KEY_Egrave KEY_Ehook KEY_Eisu_Shift KEY_Eisu_toggle KEY_Eject 
           KEY_Emacron KEY_End KEY_Eogonek KEY_Escape KEY_Eth KEY_Etilde 
           KEY_EuroSign KEY_Excel KEY_Execute KEY_Explorer KEY_F KEY_F1 KEY_F10 
           KEY_F11 KEY_F12 KEY_F13 KEY_F14 KEY_F15 KEY_F16 KEY_F17 KEY_F18 
           KEY_F19 KEY_F2 KEY_F20 KEY_F21 KEY_F22 KEY_F23 KEY_F24 KEY_F25 
           KEY_F26 KEY_F27 KEY_F28 KEY_F29 KEY_F3 KEY_F30 KEY_F31 KEY_F32 
           KEY_F33 KEY_F34 KEY_F35 KEY_F4 KEY_F5 KEY_F6 KEY_F7 KEY_F8 KEY_F9 
           KEY_FFrancSign KEY_Fabovedot KEY_Farsi_0 KEY_Farsi_1 KEY_Farsi_2 
           KEY_Farsi_3 KEY_Farsi_4 KEY_Farsi_5 KEY_Farsi_6 KEY_Farsi_7 
           KEY_Farsi_8 KEY_Farsi_9 KEY_Farsi_yeh KEY_Favorites KEY_Finance 
           KEY_Find KEY_First_Virtual_Screen KEY_Forward KEY_FrameBack 
           KEY_FrameForward KEY_G KEY_Gabovedot KEY_Game KEY_Gbreve KEY_Gcaron 
           KEY_Gcedilla KEY_Gcircumflex KEY_Georgian_an KEY_Georgian_ban 
           KEY_Georgian_can KEY_Georgian_char KEY_Georgian_chin 
           KEY_Georgian_cil KEY_Georgian_don KEY_Georgian_en KEY_Georgian_fi 
           KEY_Georgian_gan KEY_Georgian_ghan KEY_Georgian_hae KEY_Georgian_har 
           KEY_Georgian_he KEY_Georgian_hie KEY_Georgian_hoe KEY_Georgian_in 
           KEY_Georgian_jhan KEY_Georgian_jil KEY_Georgian_kan 
           KEY_Georgian_khar KEY_Georgian_las KEY_Georgian_man KEY_Georgian_nar 
           KEY_Georgian_on KEY_Georgian_par KEY_Georgian_phar KEY_Georgian_qar 
           KEY_Georgian_rae KEY_Georgian_san KEY_Georgian_shin KEY_Georgian_tan 
           KEY_Georgian_tar KEY_Georgian_un KEY_Georgian_vin KEY_Georgian_we 
           KEY_Georgian_xan KEY_Georgian_zen KEY_Georgian_zhar KEY_Go 
           KEY_Greek_ALPHA KEY_Greek_ALPHAaccent KEY_Greek_BETA KEY_Greek_CHI 
           KEY_Greek_DELTA KEY_Greek_EPSILON KEY_Greek_EPSILONaccent 
           KEY_Greek_ETA KEY_Greek_ETAaccent KEY_Greek_GAMMA KEY_Greek_IOTA 
           KEY_Greek_IOTAaccent KEY_Greek_IOTAdiaeresis KEY_Greek_IOTAdieresis 
           KEY_Greek_KAPPA KEY_Greek_LAMBDA KEY_Greek_LAMDA KEY_Greek_MU 
           KEY_Greek_NU KEY_Greek_OMEGA KEY_Greek_OMEGAaccent KEY_Greek_OMICRON 
           KEY_Greek_OMICRONaccent KEY_Greek_PHI KEY_Greek_PI KEY_Greek_PSI 
           KEY_Greek_RHO KEY_Greek_SIGMA KEY_Greek_TAU KEY_Greek_THETA 
           KEY_Greek_UPSILON KEY_Greek_UPSILONaccent KEY_Greek_UPSILONdieresis 
           KEY_Greek_XI KEY_Greek_ZETA KEY_Greek_accentdieresis KEY_Greek_alpha 
           KEY_Greek_alphaaccent KEY_Greek_beta KEY_Greek_chi KEY_Greek_delta 
           KEY_Greek_epsilon KEY_Greek_epsilonaccent KEY_Greek_eta 
           KEY_Greek_etaaccent KEY_Greek_finalsmallsigma KEY_Greek_gamma 
           KEY_Greek_horizbar KEY_Greek_iota KEY_Greek_iotaaccent 
           KEY_Greek_iotaaccentdieresis KEY_Greek_iotadieresis KEY_Greek_kappa 
           KEY_Greek_lambda KEY_Greek_lamda KEY_Greek_mu KEY_Greek_nu 
           KEY_Greek_omega KEY_Greek_omegaaccent KEY_Greek_omicron 
           KEY_Greek_omicronaccent KEY_Greek_phi KEY_Greek_pi KEY_Greek_psi 
           KEY_Greek_rho KEY_Greek_sigma KEY_Greek_switch KEY_Greek_tau 
           KEY_Greek_theta KEY_Greek_upsilon KEY_Greek_upsilonaccent 
           KEY_Greek_upsilonaccentdieresis KEY_Greek_upsilondieresis 
           KEY_Greek_xi KEY_Greek_zeta KEY_Green KEY_H KEY_Hangul KEY_Hangul_A 
           KEY_Hangul_AE KEY_Hangul_AraeA KEY_Hangul_AraeAE KEY_Hangul_Banja 
           KEY_Hangul_Cieuc KEY_Hangul_Codeinput KEY_Hangul_Dikeud KEY_Hangul_E 
           KEY_Hangul_EO KEY_Hangul_EU KEY_Hangul_End KEY_Hangul_Hanja 
           KEY_Hangul_Hieuh KEY_Hangul_I KEY_Hangul_Ieung KEY_Hangul_J_Cieuc 
           KEY_Hangul_J_Dikeud KEY_Hangul_J_Hieuh KEY_Hangul_J_Ieung 
           KEY_Hangul_J_Jieuj KEY_Hangul_J_Khieuq KEY_Hangul_J_Kiyeog 
           KEY_Hangul_J_KiyeogSios KEY_Hangul_J_KkogjiDalrinIeung 
           KEY_Hangul_J_Mieum KEY_Hangul_J_Nieun KEY_Hangul_J_NieunHieuh 
           KEY_Hangul_J_NieunJieuj KEY_Hangul_J_PanSios KEY_Hangul_J_Phieuf 
           KEY_Hangul_J_Pieub KEY_Hangul_J_PieubSios KEY_Hangul_J_Rieul 
           KEY_Hangul_J_RieulHieuh KEY_Hangul_J_RieulKiyeog 
           KEY_Hangul_J_RieulMieum KEY_Hangul_J_RieulPhieuf 
           KEY_Hangul_J_RieulPieub KEY_Hangul_J_RieulSios 
           KEY_Hangul_J_RieulTieut KEY_Hangul_J_Sios KEY_Hangul_J_SsangKiyeog 
           KEY_Hangul_J_SsangSios KEY_Hangul_J_Tieut KEY_Hangul_J_YeorinHieuh 
           KEY_Hangul_Jamo KEY_Hangul_Jeonja KEY_Hangul_Jieuj KEY_Hangul_Khieuq 
           KEY_Hangul_Kiyeog KEY_Hangul_KiyeogSios KEY_Hangul_KkogjiDalrinIeung 
           KEY_Hangul_Mieum KEY_Hangul_MultipleCandidate KEY_Hangul_Nieun 
           KEY_Hangul_NieunHieuh KEY_Hangul_NieunJieuj KEY_Hangul_O 
           KEY_Hangul_OE KEY_Hangul_PanSios KEY_Hangul_Phieuf KEY_Hangul_Pieub 
           KEY_Hangul_PieubSios KEY_Hangul_PostHanja KEY_Hangul_PreHanja 
           KEY_Hangul_PreviousCandidate KEY_Hangul_Rieul KEY_Hangul_RieulHieuh 
           KEY_Hangul_RieulKiyeog KEY_Hangul_RieulMieum KEY_Hangul_RieulPhieuf 
           KEY_Hangul_RieulPieub KEY_Hangul_RieulSios KEY_Hangul_RieulTieut 
           KEY_Hangul_RieulYeorinHieuh KEY_Hangul_Romaja 
           KEY_Hangul_SingleCandidate KEY_Hangul_Sios KEY_Hangul_Special 
           KEY_Hangul_SsangDikeud KEY_Hangul_SsangJieuj KEY_Hangul_SsangKiyeog 
           KEY_Hangul_SsangPieub KEY_Hangul_SsangSios KEY_Hangul_Start 
           KEY_Hangul_SunkyeongeumMieum KEY_Hangul_SunkyeongeumPhieuf 
           KEY_Hangul_SunkyeongeumPieub KEY_Hangul_Tieut KEY_Hangul_U 
           KEY_Hangul_WA KEY_Hangul_WAE KEY_Hangul_WE KEY_Hangul_WEO 
           KEY_Hangul_WI KEY_Hangul_YA KEY_Hangul_YAE KEY_Hangul_YE 
           KEY_Hangul_YEO KEY_Hangul_YI KEY_Hangul_YO KEY_Hangul_YU 
           KEY_Hangul_YeorinHieuh KEY_Hangul_switch KEY_Hankaku KEY_Hcircumflex 
           KEY_Hebrew_switch KEY_Help KEY_Henkan KEY_Henkan_Mode KEY_Hibernate 
           KEY_Hiragana KEY_Hiragana_Katakana KEY_History KEY_Home KEY_HomePage 
           KEY_HotLinks KEY_Hstroke KEY_Hyper_L KEY_Hyper_R KEY_I 
           KEY_ISO_Center_Object KEY_ISO_Continuous_Underline 
           KEY_ISO_Discontinuous_Underline KEY_ISO_Emphasize KEY_ISO_Enter 
           KEY_ISO_Fast_Cursor_Down KEY_ISO_Fast_Cursor_Left 
           KEY_ISO_Fast_Cursor_Right KEY_ISO_Fast_Cursor_Up KEY_ISO_First_Group 
           KEY_ISO_First_Group_Lock KEY_ISO_Group_Latch KEY_ISO_Group_Lock 
           KEY_ISO_Group_Shift KEY_ISO_Last_Group KEY_ISO_Last_Group_Lock 
           KEY_ISO_Left_Tab KEY_ISO_Level2_Latch KEY_ISO_Level3_Latch 
           KEY_ISO_Level3_Lock KEY_ISO_Level3_Shift KEY_ISO_Level5_Latch 
           KEY_ISO_Level5_Lock KEY_ISO_Level5_Shift KEY_ISO_Lock 
           KEY_ISO_Move_Line_Down KEY_ISO_Move_Line_Up KEY_ISO_Next_Group 
           KEY_ISO_Next_Group_Lock KEY_ISO_Partial_Line_Down 
           KEY_ISO_Partial_Line_Up KEY_ISO_Partial_Space_Left 
           KEY_ISO_Partial_Space_Right KEY_ISO_Prev_Group 
           KEY_ISO_Prev_Group_Lock KEY_ISO_Release_Both_Margins 
           KEY_ISO_Release_Margin_Left KEY_ISO_Release_Margin_Right 
           KEY_ISO_Set_Margin_Left KEY_ISO_Set_Margin_Right KEY_Iabovedot 
           KEY_Iacute KEY_Ibelowdot KEY_Ibreve KEY_Icircumflex KEY_Idiaeresis 
           KEY_Igrave KEY_Ihook KEY_Imacron KEY_Insert KEY_Iogonek KEY_Itilde 
           KEY_J KEY_Jcircumflex KEY_K KEY_KP_0 KEY_KP_1 KEY_KP_2 KEY_KP_3 
           KEY_KP_4 KEY_KP_5 KEY_KP_6 KEY_KP_7 KEY_KP_8 KEY_KP_9 KEY_KP_Add 
           KEY_KP_Begin KEY_KP_Decimal KEY_KP_Delete KEY_KP_Divide KEY_KP_Down 
           KEY_KP_End KEY_KP_Enter KEY_KP_Equal KEY_KP_F1 KEY_KP_F2 KEY_KP_F3 
           KEY_KP_F4 KEY_KP_Home KEY_KP_Insert KEY_KP_Left KEY_KP_Multiply 
           KEY_KP_Next KEY_KP_Page_Down KEY_KP_Page_Up KEY_KP_Prior 
           KEY_KP_Right KEY_KP_Separator KEY_KP_Space KEY_KP_Subtract 
           KEY_KP_Tab KEY_KP_Up KEY_Kana_Lock KEY_Kana_Shift KEY_Kanji 
           KEY_Kanji_Bangou KEY_Katakana KEY_KbdBrightnessDown 
           KEY_KbdBrightnessUp KEY_KbdLightOnOff KEY_Kcedilla KEY_Keyboard 
           KEY_Korean_Won KEY_L KEY_L1 KEY_L10 KEY_L2 KEY_L3 KEY_L4 KEY_L5 
           KEY_L6 KEY_L7 KEY_L8 KEY_L9 KEY_Lacute KEY_Last_Virtual_Screen 
           KEY_Launch0 KEY_Launch1 KEY_Launch2 KEY_Launch3 KEY_Launch4 
           KEY_Launch5 KEY_Launch6 KEY_Launch7 KEY_Launch8 KEY_Launch9 
           KEY_LaunchA KEY_LaunchB KEY_LaunchC KEY_LaunchD KEY_LaunchE 
           KEY_LaunchF KEY_Lbelowdot KEY_Lcaron KEY_Lcedilla KEY_Left 
           KEY_LightBulb KEY_Linefeed KEY_LiraSign KEY_LogGrabInfo KEY_LogOff 
           KEY_LogWindowTree KEY_Lstroke KEY_M KEY_Mabovedot KEY_Macedonia_DSE 
           KEY_Macedonia_GJE KEY_Macedonia_KJE KEY_Macedonia_dse 
           KEY_Macedonia_gje KEY_Macedonia_kje KEY_Mae_Koho KEY_Mail 
           KEY_MailForward KEY_Market KEY_Massyo KEY_Meeting KEY_Memo KEY_Menu 
           KEY_MenuKB KEY_MenuPB KEY_Messenger KEY_Meta_L KEY_Meta_R 
           KEY_MillSign KEY_ModeLock KEY_Mode_switch KEY_MonBrightnessDown 
           KEY_MonBrightnessUp KEY_MouseKeys_Accel_Enable KEY_MouseKeys_Enable 
           KEY_Muhenkan KEY_Multi_key KEY_MultipleCandidate KEY_Music 
           KEY_MyComputer KEY_MySites KEY_N KEY_Nacute KEY_NairaSign KEY_Ncaron 
           KEY_Ncedilla KEY_New KEY_NewSheqelSign KEY_News KEY_Next 
           KEY_Next_VMode KEY_Next_Virtual_Screen KEY_Ntilde KEY_Num_Lock KEY_O 
           KEY_OE KEY_Oacute KEY_Obarred KEY_Obelowdot KEY_Ocaron 
           KEY_Ocircumflex KEY_Ocircumflexacute KEY_Ocircumflexbelowdot 
           KEY_Ocircumflexgrave KEY_Ocircumflexhook KEY_Ocircumflextilde 
           KEY_Odiaeresis KEY_Odoubleacute KEY_OfficeHome KEY_Ograve KEY_Ohook 
           KEY_Ohorn KEY_Ohornacute KEY_Ohornbelowdot KEY_Ohorngrave 
           KEY_Ohornhook KEY_Ohorntilde KEY_Omacron KEY_Ooblique KEY_Open 
           KEY_OpenURL KEY_Option KEY_Oslash KEY_Otilde KEY_Overlay1_Enable 
           KEY_Overlay2_Enable KEY_P KEY_Pabovedot KEY_Page_Down KEY_Page_Up 
           KEY_Paste KEY_Pause KEY_PesetaSign KEY_Phone KEY_Pictures 
           KEY_Pointer_Accelerate KEY_Pointer_Button1 KEY_Pointer_Button2 
           KEY_Pointer_Button3 KEY_Pointer_Button4 KEY_Pointer_Button5 
           KEY_Pointer_Button_Dflt KEY_Pointer_DblClick1 KEY_Pointer_DblClick2 
           KEY_Pointer_DblClick3 KEY_Pointer_DblClick4 KEY_Pointer_DblClick5 
           KEY_Pointer_DblClick_Dflt KEY_Pointer_DfltBtnNext 
           KEY_Pointer_DfltBtnPrev KEY_Pointer_Down KEY_Pointer_DownLeft 
           KEY_Pointer_DownRight KEY_Pointer_Drag1 KEY_Pointer_Drag2 
           KEY_Pointer_Drag3 KEY_Pointer_Drag4 KEY_Pointer_Drag5 
           KEY_Pointer_Drag_Dflt KEY_Pointer_EnableKeys KEY_Pointer_Left 
           KEY_Pointer_Right KEY_Pointer_Up KEY_Pointer_UpLeft 
           KEY_Pointer_UpRight KEY_PowerDown KEY_PowerOff KEY_Prev_VMode 
           KEY_Prev_Virtual_Screen KEY_PreviousCandidate KEY_Print KEY_Prior 
           KEY_Q KEY_R KEY_R1 KEY_R10 KEY_R11 KEY_R12 KEY_R13 KEY_R14 KEY_R15 
           KEY_R2 KEY_R3 KEY_R4 KEY_R5 KEY_R6 KEY_R7 KEY_R8 KEY_R9 KEY_RFKill 
           KEY_Racute KEY_Rcaron KEY_Rcedilla KEY_Red KEY_Redo KEY_Refresh 
           KEY_Reload KEY_RepeatKeys_Enable KEY_Reply KEY_Return KEY_Right 
           KEY_RockerDown KEY_RockerEnter KEY_RockerUp KEY_Romaji 
           KEY_RotateWindows KEY_RotationKB KEY_RotationPB KEY_RupeeSign KEY_S 
           KEY_SCHWA KEY_Sabovedot KEY_Sacute KEY_Save KEY_Scaron KEY_Scedilla 
           KEY_Scircumflex KEY_ScreenSaver KEY_ScrollClick KEY_ScrollDown 
           KEY_ScrollUp KEY_Scroll_Lock KEY_Search KEY_Select KEY_SelectButton 
           KEY_Send KEY_Serbian_DJE KEY_Serbian_DZE KEY_Serbian_JE 
           KEY_Serbian_LJE KEY_Serbian_NJE KEY_Serbian_TSHE KEY_Serbian_dje 
           KEY_Serbian_dze KEY_Serbian_je KEY_Serbian_lje KEY_Serbian_nje 
           KEY_Serbian_tshe KEY_Shift_L KEY_Shift_Lock KEY_Shift_R KEY_Shop 
           KEY_SingleCandidate KEY_Sinh_a KEY_Sinh_aa KEY_Sinh_aa2 KEY_Sinh_ae 
           KEY_Sinh_ae2 KEY_Sinh_aee KEY_Sinh_aee2 KEY_Sinh_ai KEY_Sinh_ai2 
           KEY_Sinh_al KEY_Sinh_au KEY_Sinh_au2 KEY_Sinh_ba KEY_Sinh_bha 
           KEY_Sinh_ca KEY_Sinh_cha KEY_Sinh_dda KEY_Sinh_ddha KEY_Sinh_dha 
           KEY_Sinh_dhha KEY_Sinh_e KEY_Sinh_e2 KEY_Sinh_ee KEY_Sinh_ee2 
           KEY_Sinh_fa KEY_Sinh_ga KEY_Sinh_gha KEY_Sinh_h2 KEY_Sinh_ha 
           KEY_Sinh_i KEY_Sinh_i2 KEY_Sinh_ii KEY_Sinh_ii2 KEY_Sinh_ja 
           KEY_Sinh_jha KEY_Sinh_jnya KEY_Sinh_ka KEY_Sinh_kha 
           KEY_Sinh_kunddaliya KEY_Sinh_la KEY_Sinh_lla KEY_Sinh_lu 
           KEY_Sinh_lu2 KEY_Sinh_luu KEY_Sinh_luu2 KEY_Sinh_ma KEY_Sinh_mba 
           KEY_Sinh_na KEY_Sinh_ndda KEY_Sinh_ndha KEY_Sinh_ng KEY_Sinh_ng2 
           KEY_Sinh_nga KEY_Sinh_nja KEY_Sinh_nna KEY_Sinh_nya KEY_Sinh_o 
           KEY_Sinh_o2 KEY_Sinh_oo KEY_Sinh_oo2 KEY_Sinh_pa KEY_Sinh_pha 
           KEY_Sinh_ra KEY_Sinh_ri KEY_Sinh_rii KEY_Sinh_ru2 KEY_Sinh_ruu2 
           KEY_Sinh_sa KEY_Sinh_sha KEY_Sinh_ssha KEY_Sinh_tha KEY_Sinh_thha 
           KEY_Sinh_tta KEY_Sinh_ttha KEY_Sinh_u KEY_Sinh_u2 KEY_Sinh_uu 
           KEY_Sinh_uu2 KEY_Sinh_va KEY_Sinh_ya KEY_Sleep KEY_SlowKeys_Enable 
           KEY_Spell KEY_SplitScreen KEY_Standby KEY_Start 
           KEY_StickyKeys_Enable KEY_Stop KEY_Subtitle KEY_Super_L KEY_Super_R 
           KEY_Support KEY_Suspend KEY_Switch_VT_1 KEY_Switch_VT_10 
           KEY_Switch_VT_11 KEY_Switch_VT_12 KEY_Switch_VT_2 KEY_Switch_VT_3 
           KEY_Switch_VT_4 KEY_Switch_VT_5 KEY_Switch_VT_6 KEY_Switch_VT_7 
           KEY_Switch_VT_8 KEY_Switch_VT_9 KEY_Sys_Req KEY_T KEY_THORN KEY_Tab 
           KEY_Tabovedot KEY_TaskPane KEY_Tcaron KEY_Tcedilla KEY_Terminal 
           KEY_Terminate_Server KEY_Thai_baht KEY_Thai_bobaimai 
           KEY_Thai_chochan KEY_Thai_chochang KEY_Thai_choching 
           KEY_Thai_chochoe KEY_Thai_dochada KEY_Thai_dodek KEY_Thai_fofa 
           KEY_Thai_fofan KEY_Thai_hohip KEY_Thai_honokhuk KEY_Thai_khokhai 
           KEY_Thai_khokhon KEY_Thai_khokhuat KEY_Thai_khokhwai 
           KEY_Thai_khorakhang KEY_Thai_kokai KEY_Thai_lakkhangyao 
           KEY_Thai_lekchet KEY_Thai_lekha KEY_Thai_lekhok KEY_Thai_lekkao 
           KEY_Thai_leknung KEY_Thai_lekpaet KEY_Thai_leksam KEY_Thai_leksi 
           KEY_Thai_leksong KEY_Thai_leksun KEY_Thai_lochula KEY_Thai_loling 
           KEY_Thai_lu KEY_Thai_maichattawa KEY_Thai_maiek KEY_Thai_maihanakat 
           KEY_Thai_maihanakat_maitho KEY_Thai_maitaikhu KEY_Thai_maitho 
           KEY_Thai_maitri KEY_Thai_maiyamok KEY_Thai_moma KEY_Thai_ngongu 
           KEY_Thai_nikhahit KEY_Thai_nonen KEY_Thai_nonu KEY_Thai_oang 
           KEY_Thai_paiyannoi KEY_Thai_phinthu KEY_Thai_phophan 
           KEY_Thai_phophung KEY_Thai_phosamphao KEY_Thai_popla KEY_Thai_rorua 
           KEY_Thai_ru KEY_Thai_saraa KEY_Thai_saraaa KEY_Thai_saraae 
           KEY_Thai_saraaimaimalai KEY_Thai_saraaimaimuan KEY_Thai_saraam 
           KEY_Thai_sarae KEY_Thai_sarai KEY_Thai_saraii KEY_Thai_sarao 
           KEY_Thai_sarau KEY_Thai_saraue KEY_Thai_sarauee KEY_Thai_sarauu 
           KEY_Thai_sorusi KEY_Thai_sosala KEY_Thai_soso KEY_Thai_sosua 
           KEY_Thai_thanthakhat KEY_Thai_thonangmontho KEY_Thai_thophuthao 
           KEY_Thai_thothahan KEY_Thai_thothan KEY_Thai_thothong 
           KEY_Thai_thothung KEY_Thai_topatak KEY_Thai_totao KEY_Thai_wowaen 
           KEY_Thai_yoyak KEY_Thai_yoying KEY_Thorn KEY_Time KEY_ToDoList 
           KEY_Tools KEY_TopMenu KEY_TouchpadOff KEY_TouchpadOn 
           KEY_TouchpadToggle KEY_Touroku KEY_Travel KEY_Tslash KEY_U KEY_UWB 
           KEY_Uacute KEY_Ubelowdot KEY_Ubreve KEY_Ucircumflex KEY_Udiaeresis 
           KEY_Udoubleacute KEY_Ugrave KEY_Uhook KEY_Uhorn KEY_Uhornacute 
           KEY_Uhornbelowdot KEY_Uhorngrave KEY_Uhornhook KEY_Uhorntilde 
           KEY_Ukrainian_GHE_WITH_UPTURN KEY_Ukrainian_I KEY_Ukrainian_IE 
           KEY_Ukrainian_YI KEY_Ukrainian_ghe_with_upturn KEY_Ukrainian_i 
           KEY_Ukrainian_ie KEY_Ukrainian_yi KEY_Ukranian_I KEY_Ukranian_JE 
           KEY_Ukranian_YI KEY_Ukranian_i KEY_Ukranian_je KEY_Ukranian_yi 
           KEY_Umacron KEY_Undo KEY_Ungrab KEY_Uogonek KEY_Up KEY_Uring 
           KEY_User1KB KEY_User2KB KEY_UserPB KEY_Utilde KEY_V KEY_VendorHome 
           KEY_Video KEY_View KEY_VoidSymbol KEY_W KEY_WLAN KEY_WWAN KEY_WWW 
           KEY_Wacute KEY_WakeUp KEY_Wcircumflex KEY_Wdiaeresis KEY_WebCam 
           KEY_Wgrave KEY_WheelButton KEY_WindowClear KEY_WonSign KEY_Word 
           KEY_X KEY_Xabovedot KEY_Xfer KEY_Y KEY_Yacute KEY_Ybelowdot 
           KEY_Ycircumflex KEY_Ydiaeresis KEY_Yellow KEY_Ygrave KEY_Yhook 
           KEY_Ytilde KEY_Z KEY_Zabovedot KEY_Zacute KEY_Zcaron KEY_Zen_Koho 
           KEY_Zenkaku KEY_Zenkaku_Hankaku KEY_ZoomIn KEY_ZoomOut KEY_Zstroke 
           KEY_a KEY_aacute KEY_abelowdot KEY_abovedot KEY_abreve 
           KEY_abreveacute KEY_abrevebelowdot KEY_abrevegrave KEY_abrevehook 
           KEY_abrevetilde KEY_acircumflex KEY_acircumflexacute 
           KEY_acircumflexbelowdot KEY_acircumflexgrave KEY_acircumflexhook 
           KEY_acircumflextilde KEY_acute KEY_adiaeresis KEY_ae KEY_agrave 
           KEY_ahook KEY_amacron KEY_ampersand KEY_aogonek KEY_apostrophe 
           KEY_approxeq KEY_approximate KEY_aring KEY_asciicircum 
           KEY_asciitilde KEY_asterisk KEY_at KEY_atilde KEY_b KEY_babovedot 
           KEY_backslash KEY_ballotcross KEY_bar KEY_because KEY_blank 
           KEY_botintegral KEY_botleftparens KEY_botleftsqbracket 
           KEY_botleftsummation KEY_botrightparens KEY_botrightsqbracket 
           KEY_botrightsummation KEY_bott KEY_botvertsummationconnector 
           KEY_braceleft KEY_braceright KEY_bracketleft KEY_bracketright 
           KEY_braille_blank KEY_braille_dot_1 KEY_braille_dot_10 
           KEY_braille_dot_2 KEY_braille_dot_3 KEY_braille_dot_4 
           KEY_braille_dot_5 KEY_braille_dot_6 KEY_braille_dot_7 
           KEY_braille_dot_8 KEY_braille_dot_9 KEY_braille_dots_1 
           KEY_braille_dots_12 KEY_braille_dots_123 KEY_braille_dots_1234 
           KEY_braille_dots_12345 KEY_braille_dots_123456 
           KEY_braille_dots_1234567 KEY_braille_dots_12345678 
           KEY_braille_dots_1234568 KEY_braille_dots_123457 
           KEY_braille_dots_1234578 KEY_braille_dots_123458 
           KEY_braille_dots_12346 KEY_braille_dots_123467 
           KEY_braille_dots_1234678 KEY_braille_dots_123468 
           KEY_braille_dots_12347 KEY_braille_dots_123478 
           KEY_braille_dots_12348 KEY_braille_dots_1235 KEY_braille_dots_12356 
           KEY_braille_dots_123567 KEY_braille_dots_1235678 
           KEY_braille_dots_123568 KEY_braille_dots_12357 
           KEY_braille_dots_123578 KEY_braille_dots_12358 KEY_braille_dots_1236 
           KEY_braille_dots_12367 KEY_braille_dots_123678 
           KEY_braille_dots_12368 KEY_braille_dots_1237 KEY_braille_dots_12378 
           KEY_braille_dots_1238 KEY_braille_dots_124 KEY_braille_dots_1245 
           KEY_braille_dots_12456 KEY_braille_dots_124567 
           KEY_braille_dots_1245678 KEY_braille_dots_124568 
           KEY_braille_dots_12457 KEY_braille_dots_124578 
           KEY_braille_dots_12458 KEY_braille_dots_1246 KEY_braille_dots_12467 
           KEY_braille_dots_124678 KEY_braille_dots_12468 KEY_braille_dots_1247 
           KEY_braille_dots_12478 KEY_braille_dots_1248 KEY_braille_dots_125 
           KEY_braille_dots_1256 KEY_braille_dots_12567 KEY_braille_dots_125678 
           KEY_braille_dots_12568 KEY_braille_dots_1257 KEY_braille_dots_12578 
           KEY_braille_dots_1258 KEY_braille_dots_126 KEY_braille_dots_1267 
           KEY_braille_dots_12678 KEY_braille_dots_1268 KEY_braille_dots_127 
           KEY_braille_dots_1278 KEY_braille_dots_128 KEY_braille_dots_13 
           KEY_braille_dots_134 KEY_braille_dots_1345 KEY_braille_dots_13456 
           KEY_braille_dots_134567 KEY_braille_dots_1345678 
           KEY_braille_dots_134568 KEY_braille_dots_13457 
           KEY_braille_dots_134578 KEY_braille_dots_13458 KEY_braille_dots_1346 
           KEY_braille_dots_13467 KEY_braille_dots_134678 
           KEY_braille_dots_13468 KEY_braille_dots_1347 KEY_braille_dots_13478 
           KEY_braille_dots_1348 KEY_braille_dots_135 KEY_braille_dots_1356 
           KEY_braille_dots_13567 KEY_braille_dots_135678 
           KEY_braille_dots_13568 KEY_braille_dots_1357 KEY_braille_dots_13578 
           KEY_braille_dots_1358 KEY_braille_dots_136 KEY_braille_dots_1367 
           KEY_braille_dots_13678 KEY_braille_dots_1368 KEY_braille_dots_137 
           KEY_braille_dots_1378 KEY_braille_dots_138 KEY_braille_dots_14 
           KEY_braille_dots_145 KEY_braille_dots_1456 KEY_braille_dots_14567 
           KEY_braille_dots_145678 KEY_braille_dots_14568 KEY_braille_dots_1457 
           KEY_braille_dots_14578 KEY_braille_dots_1458 KEY_braille_dots_146 
           KEY_braille_dots_1467 KEY_braille_dots_14678 KEY_braille_dots_1468 
           KEY_braille_dots_147 KEY_braille_dots_1478 KEY_braille_dots_148 
           KEY_braille_dots_15 KEY_braille_dots_156 KEY_braille_dots_1567 
           KEY_braille_dots_15678 KEY_braille_dots_1568 KEY_braille_dots_157 
           KEY_braille_dots_1578 KEY_braille_dots_158 KEY_braille_dots_16 
           KEY_braille_dots_167 KEY_braille_dots_1678 KEY_braille_dots_168 
           KEY_braille_dots_17 KEY_braille_dots_178 KEY_braille_dots_18 
           KEY_braille_dots_2 KEY_braille_dots_23 KEY_braille_dots_234 
           KEY_braille_dots_2345 KEY_braille_dots_23456 KEY_braille_dots_234567 
           KEY_braille_dots_2345678 KEY_braille_dots_234568 
           KEY_braille_dots_23457 KEY_braille_dots_234578 
           KEY_braille_dots_23458 KEY_braille_dots_2346 KEY_braille_dots_23467 
           KEY_braille_dots_234678 KEY_braille_dots_23468 KEY_braille_dots_2347 
           KEY_braille_dots_23478 KEY_braille_dots_2348 KEY_braille_dots_235 
           KEY_braille_dots_2356 KEY_braille_dots_23567 KEY_braille_dots_235678 
           KEY_braille_dots_23568 KEY_braille_dots_2357 KEY_braille_dots_23578 
           KEY_braille_dots_2358 KEY_braille_dots_236 KEY_braille_dots_2367 
           KEY_braille_dots_23678 KEY_braille_dots_2368 KEY_braille_dots_237 
           KEY_braille_dots_2378 KEY_braille_dots_238 KEY_braille_dots_24 
           KEY_braille_dots_245 KEY_braille_dots_2456 KEY_braille_dots_24567 
           KEY_braille_dots_245678 KEY_braille_dots_24568 KEY_braille_dots_2457 
           KEY_braille_dots_24578 KEY_braille_dots_2458 KEY_braille_dots_246 
           KEY_braille_dots_2467 KEY_braille_dots_24678 KEY_braille_dots_2468 
           KEY_braille_dots_247 KEY_braille_dots_2478 KEY_braille_dots_248 
           KEY_braille_dots_25 KEY_braille_dots_256 KEY_braille_dots_2567 
           KEY_braille_dots_25678 KEY_braille_dots_2568 KEY_braille_dots_257 
           KEY_braille_dots_2578 KEY_braille_dots_258 KEY_braille_dots_26 
           KEY_braille_dots_267 KEY_braille_dots_2678 KEY_braille_dots_268 
           KEY_braille_dots_27 KEY_braille_dots_278 KEY_braille_dots_28 
           KEY_braille_dots_3 KEY_braille_dots_34 KEY_braille_dots_345 
           KEY_braille_dots_3456 KEY_braille_dots_34567 KEY_braille_dots_345678 
           KEY_braille_dots_34568 KEY_braille_dots_3457 KEY_braille_dots_34578 
           KEY_braille_dots_3458 KEY_braille_dots_346 KEY_braille_dots_3467 
           KEY_braille_dots_34678 KEY_braille_dots_3468 KEY_braille_dots_347 
           KEY_braille_dots_3478 KEY_braille_dots_348 KEY_braille_dots_35 
           KEY_braille_dots_356 KEY_braille_dots_3567 KEY_braille_dots_35678 
           KEY_braille_dots_3568 KEY_braille_dots_357 KEY_braille_dots_3578 
           KEY_braille_dots_358 KEY_braille_dots_36 KEY_braille_dots_367 
           KEY_braille_dots_3678 KEY_braille_dots_368 KEY_braille_dots_37 
           KEY_braille_dots_378 KEY_braille_dots_38 KEY_braille_dots_4 
           KEY_braille_dots_45 KEY_braille_dots_456 KEY_braille_dots_4567 
           KEY_braille_dots_45678 KEY_braille_dots_4568 KEY_braille_dots_457 
           KEY_braille_dots_4578 KEY_braille_dots_458 KEY_braille_dots_46 
           KEY_braille_dots_467 KEY_braille_dots_4678 KEY_braille_dots_468 
           KEY_braille_dots_47 KEY_braille_dots_478 KEY_braille_dots_48 
           KEY_braille_dots_5 KEY_braille_dots_56 KEY_braille_dots_567 
           KEY_braille_dots_5678 KEY_braille_dots_568 KEY_braille_dots_57 
           KEY_braille_dots_578 KEY_braille_dots_58 KEY_braille_dots_6 
           KEY_braille_dots_67 KEY_braille_dots_678 KEY_braille_dots_68 
           KEY_braille_dots_7 KEY_braille_dots_78 KEY_braille_dots_8 KEY_breve 
           KEY_brokenbar KEY_c KEY_c_h KEY_cabovedot KEY_cacute KEY_careof 
           KEY_caret KEY_caron KEY_ccaron KEY_ccedilla KEY_ccircumflex 
           KEY_cedilla KEY_cent KEY_ch KEY_checkerboard KEY_checkmark 
           KEY_circle KEY_club KEY_colon KEY_comma KEY_containsas KEY_copyright 
           KEY_cr KEY_crossinglines KEY_cuberoot KEY_currency KEY_cursor KEY_d 
           KEY_dabovedot KEY_dagger KEY_dcaron KEY_dead_A KEY_dead_E KEY_dead_I 
           KEY_dead_O KEY_dead_U KEY_dead_a KEY_dead_abovecomma 
           KEY_dead_abovedot KEY_dead_abovereversedcomma KEY_dead_abovering 
           KEY_dead_aboveverticalline KEY_dead_acute KEY_dead_belowbreve 
           KEY_dead_belowcircumflex KEY_dead_belowcomma KEY_dead_belowdiaeresis 
           KEY_dead_belowdot KEY_dead_belowmacron KEY_dead_belowring 
           KEY_dead_belowtilde KEY_dead_belowverticalline KEY_dead_breve 
           KEY_dead_capital_schwa KEY_dead_caron KEY_dead_cedilla 
           KEY_dead_circumflex KEY_dead_currency KEY_dead_dasia 
           KEY_dead_diaeresis KEY_dead_doubleacute KEY_dead_doublegrave 
           KEY_dead_e KEY_dead_grave KEY_dead_greek KEY_dead_hook KEY_dead_horn 
           KEY_dead_i KEY_dead_invertedbreve KEY_dead_iota 
           KEY_dead_longsolidusoverlay KEY_dead_lowline KEY_dead_macron 
           KEY_dead_o KEY_dead_ogonek KEY_dead_perispomeni KEY_dead_psili 
           KEY_dead_semivoiced_sound KEY_dead_small_schwa KEY_dead_stroke 
           KEY_dead_tilde KEY_dead_u KEY_dead_voiced_sound KEY_decimalpoint 
           KEY_degree KEY_diaeresis KEY_diamond KEY_digitspace KEY_dintegral 
           KEY_division KEY_dollar KEY_doubbaselinedot KEY_doubleacute 
           KEY_doubledagger KEY_doublelowquotemark KEY_downarrow KEY_downcaret 
           KEY_downshoe KEY_downstile KEY_downtack KEY_dstroke KEY_e 
           KEY_eabovedot KEY_eacute KEY_ebelowdot KEY_ecaron KEY_ecircumflex 
           KEY_ecircumflexacute KEY_ecircumflexbelowdot KEY_ecircumflexgrave 
           KEY_ecircumflexhook KEY_ecircumflextilde KEY_ediaeresis KEY_egrave 
           KEY_ehook KEY_eightsubscript KEY_eightsuperior KEY_elementof 
           KEY_ellipsis KEY_em3space KEY_em4space KEY_emacron KEY_emdash 
           KEY_emfilledcircle KEY_emfilledrect KEY_emopencircle 
           KEY_emopenrectangle KEY_emptyset KEY_emspace KEY_endash 
           KEY_enfilledcircbullet KEY_enfilledsqbullet KEY_eng 
           KEY_enopencircbullet KEY_enopensquarebullet KEY_enspace KEY_eogonek 
           KEY_equal KEY_eth KEY_etilde KEY_exclam KEY_exclamdown KEY_ezh KEY_f 
           KEY_fabovedot KEY_femalesymbol KEY_ff KEY_figdash 
           KEY_filledlefttribullet KEY_filledrectbullet 
           KEY_filledrighttribullet KEY_filledtribulletdown 
           KEY_filledtribulletup KEY_fiveeighths KEY_fivesixths 
           KEY_fivesubscript KEY_fivesuperior KEY_fourfifths KEY_foursubscript 
           KEY_foursuperior KEY_fourthroot KEY_function KEY_g KEY_gabovedot 
           KEY_gbreve KEY_gcaron KEY_gcedilla KEY_gcircumflex KEY_grave 
           KEY_greater KEY_greaterthanequal KEY_guillemotleft 
           KEY_guillemotright KEY_h KEY_hairspace KEY_hcircumflex KEY_heart 
           KEY_hebrew_aleph KEY_hebrew_ayin KEY_hebrew_bet KEY_hebrew_beth 
           KEY_hebrew_chet KEY_hebrew_dalet KEY_hebrew_daleth 
           KEY_hebrew_doublelowline KEY_hebrew_finalkaph KEY_hebrew_finalmem 
           KEY_hebrew_finalnun KEY_hebrew_finalpe KEY_hebrew_finalzade 
           KEY_hebrew_finalzadi KEY_hebrew_gimel KEY_hebrew_gimmel 
           KEY_hebrew_he KEY_hebrew_het KEY_hebrew_kaph KEY_hebrew_kuf 
           KEY_hebrew_lamed KEY_hebrew_mem KEY_hebrew_nun KEY_hebrew_pe 
           KEY_hebrew_qoph KEY_hebrew_resh KEY_hebrew_samech KEY_hebrew_samekh 
           KEY_hebrew_shin KEY_hebrew_taf KEY_hebrew_taw KEY_hebrew_tet 
           KEY_hebrew_teth KEY_hebrew_waw KEY_hebrew_yod KEY_hebrew_zade 
           KEY_hebrew_zadi KEY_hebrew_zain KEY_hebrew_zayin KEY_hexagram 
           KEY_horizconnector KEY_horizlinescan1 KEY_horizlinescan3 
           KEY_horizlinescan5 KEY_horizlinescan7 KEY_horizlinescan9 KEY_hstroke 
           KEY_ht KEY_hyphen KEY_i KEY_iTouch KEY_iacute KEY_ibelowdot 
           KEY_ibreve KEY_icircumflex KEY_identical KEY_idiaeresis KEY_idotless 
           KEY_ifonlyif KEY_igrave KEY_ihook KEY_imacron KEY_implies 
           KEY_includedin KEY_includes KEY_infinity KEY_integral 
           KEY_intersection KEY_iogonek KEY_itilde KEY_j KEY_jcircumflex 
           KEY_jot KEY_k KEY_kana_A KEY_kana_CHI KEY_kana_E KEY_kana_FU 
           KEY_kana_HA KEY_kana_HE KEY_kana_HI KEY_kana_HO KEY_kana_HU 
           KEY_kana_I KEY_kana_KA KEY_kana_KE KEY_kana_KI KEY_kana_KO 
           KEY_kana_KU KEY_kana_MA KEY_kana_ME KEY_kana_MI KEY_kana_MO 
           KEY_kana_MU KEY_kana_N KEY_kana_NA KEY_kana_NE KEY_kana_NI 
           KEY_kana_NO KEY_kana_NU KEY_kana_O KEY_kana_RA KEY_kana_RE 
           KEY_kana_RI KEY_kana_RO KEY_kana_RU KEY_kana_SA KEY_kana_SE 
           KEY_kana_SHI KEY_kana_SO KEY_kana_SU KEY_kana_TA KEY_kana_TE 
           KEY_kana_TI KEY_kana_TO KEY_kana_TSU KEY_kana_TU KEY_kana_U 
           KEY_kana_WA KEY_kana_WO KEY_kana_YA KEY_kana_YO KEY_kana_YU 
           KEY_kana_a KEY_kana_closingbracket KEY_kana_comma 
           KEY_kana_conjunctive KEY_kana_e KEY_kana_fullstop KEY_kana_i 
           KEY_kana_middledot KEY_kana_o KEY_kana_openingbracket 
           KEY_kana_switch KEY_kana_tsu KEY_kana_tu KEY_kana_u KEY_kana_ya 
           KEY_kana_yo KEY_kana_yu KEY_kappa KEY_kcedilla KEY_kra KEY_l 
           KEY_lacute KEY_latincross KEY_lbelowdot KEY_lcaron KEY_lcedilla 
           KEY_leftanglebracket KEY_leftarrow KEY_leftcaret 
           KEY_leftdoublequotemark KEY_leftmiddlecurlybrace 
           KEY_leftopentriangle KEY_leftpointer KEY_leftradical KEY_leftshoe 
           KEY_leftsinglequotemark KEY_leftt KEY_lefttack KEY_less 
           KEY_lessthanequal KEY_lf KEY_logicaland KEY_logicalor 
           KEY_lowleftcorner KEY_lowrightcorner KEY_lstroke KEY_m KEY_mabovedot 
           KEY_macron KEY_malesymbol KEY_maltesecross KEY_marker KEY_masculine 
           KEY_minus KEY_minutes KEY_mu KEY_multiply KEY_musicalflat 
           KEY_musicalsharp KEY_n KEY_nabla KEY_nacute KEY_ncaron KEY_ncedilla 
           KEY_ninesubscript KEY_ninesuperior KEY_nl KEY_nobreakspace 
           KEY_notapproxeq KEY_notelementof KEY_notequal KEY_notidentical 
           KEY_notsign KEY_ntilde KEY_numbersign KEY_numerosign KEY_o 
           KEY_oacute KEY_obarred KEY_obelowdot KEY_ocaron KEY_ocircumflex 
           KEY_ocircumflexacute KEY_ocircumflexbelowdot KEY_ocircumflexgrave 
           KEY_ocircumflexhook KEY_ocircumflextilde KEY_odiaeresis 
           KEY_odoubleacute KEY_oe KEY_ogonek KEY_ograve KEY_ohook KEY_ohorn 
           KEY_ohornacute KEY_ohornbelowdot KEY_ohorngrave KEY_ohornhook 
           KEY_ohorntilde KEY_omacron KEY_oneeighth KEY_onefifth KEY_onehalf 
           KEY_onequarter KEY_onesixth KEY_onesubscript KEY_onesuperior 
           KEY_onethird KEY_ooblique KEY_openrectbullet KEY_openstar 
           KEY_opentribulletdown KEY_opentribulletup KEY_ordfeminine KEY_oslash 
           KEY_otilde KEY_overbar KEY_overline KEY_p KEY_pabovedot 
           KEY_paragraph KEY_parenleft KEY_parenright KEY_partdifferential 
           KEY_partialderivative KEY_percent KEY_period KEY_periodcentered 
           KEY_permille KEY_phonographcopyright KEY_plus KEY_plusminus 
           KEY_prescription KEY_prolongedsound KEY_punctspace KEY_q KEY_quad 
           KEY_question KEY_questiondown KEY_quotedbl KEY_quoteleft 
           KEY_quoteright KEY_r KEY_racute KEY_radical KEY_rcaron KEY_rcedilla 
           KEY_registered KEY_rightanglebracket KEY_rightarrow KEY_rightcaret 
           KEY_rightdoublequotemark KEY_rightmiddlecurlybrace 
           KEY_rightmiddlesummation KEY_rightopentriangle KEY_rightpointer 
           KEY_rightshoe KEY_rightsinglequotemark KEY_rightt KEY_righttack 
           KEY_s KEY_sabovedot KEY_sacute KEY_scaron KEY_scedilla KEY_schwa 
           KEY_scircumflex KEY_script_switch KEY_seconds KEY_section 
           KEY_semicolon KEY_semivoicedsound KEY_seveneighths 
           KEY_sevensubscript KEY_sevensuperior KEY_signaturemark 
           KEY_signifblank KEY_similarequal KEY_singlelowquotemark 
           KEY_sixsubscript KEY_sixsuperior KEY_slash KEY_soliddiamond 
           KEY_space KEY_squareroot KEY_ssharp KEY_sterling KEY_stricteq KEY_t 
           KEY_tabovedot KEY_tcaron KEY_tcedilla KEY_telephone 
           KEY_telephonerecorder KEY_therefore KEY_thinspace KEY_thorn 
           KEY_threeeighths KEY_threefifths KEY_threequarters 
           KEY_threesubscript KEY_threesuperior KEY_tintegral KEY_topintegral 
           KEY_topleftparens KEY_topleftradical KEY_topleftsqbracket 
           KEY_topleftsummation KEY_toprightparens KEY_toprightsqbracket 
           KEY_toprightsummation KEY_topt KEY_topvertsummationconnector 
           KEY_trademark KEY_trademarkincircle KEY_tslash KEY_twofifths 
           KEY_twosubscript KEY_twosuperior KEY_twothirds KEY_u KEY_uacute 
           KEY_ubelowdot KEY_ubreve KEY_ucircumflex KEY_udiaeresis 
           KEY_udoubleacute KEY_ugrave KEY_uhook KEY_uhorn KEY_uhornacute 
           KEY_uhornbelowdot KEY_uhorngrave KEY_uhornhook KEY_uhorntilde 
           KEY_umacron KEY_underbar KEY_underscore KEY_union KEY_uogonek 
           KEY_uparrow KEY_upcaret KEY_upleftcorner KEY_uprightcorner 
           KEY_upshoe KEY_upstile KEY_uptack KEY_uring KEY_utilde KEY_v 
           KEY_variation KEY_vertbar KEY_vertconnector KEY_voicedsound KEY_vt 
           KEY_w KEY_wacute KEY_wcircumflex KEY_wdiaeresis KEY_wgrave KEY_x 
           KEY_xabovedot KEY_y KEY_yacute KEY_ybelowdot KEY_ycircumflex 
           KEY_ydiaeresis KEY_yen KEY_ygrave KEY_yhook KEY_ytilde KEY_z 
           KEY_zabovedot KEY_zacute KEY_zcaron KEY_zerosubscript 
           KEY_zerosuperior KEY_zstroke <GdkKeyEvent> get-consumed-modifiers 
           key-event:get-consumed-modifiers get-keycode key-event:get-keycode 
           get-keyval key-event:get-keyval get-layout key-event:get-layout 
           get-level key-event:get-level get-match key-event:get-match 
           is-modifier? key-event:is-modifier? <GdkKeyMatch> symbol->key-match 
           number->key-match key-match->symbol key-match->number matches 
           key-event:matches MODIFIER_MASK <GdkMemoryFormat> 
           symbol->memory-format number->memory-format memory-format->symbol 
           memory-format->number <GdkMemoryTexture> new memory-texture:new 
           get-connector monitor:get-connector get-description 
           monitor:get-description get-display monitor:get-display 
           <GdkRectangle> get-geometry! monitor:get-geometry! get-height-mm 
           monitor:get-height-mm get-manufacturer monitor:get-manufacturer 
           get-model monitor:get-model get-refresh-rate 
           monitor:get-refresh-rate get-scale-factor monitor:get-scale-factor 
           <GdkSubpixelLayout> symbol->subpixel-layout number->subpixel-layout 
           subpixel-layout->symbol subpixel-layout->number get-subpixel-layout 
           monitor:get-subpixel-layout get-width-mm monitor:get-width-mm 
           is-valid? monitor:is-valid? invalidate monitor:invalidate 
           <GdkMotionEvent> PRIORITY_REDRAW <GdkPadEvent> get-axis-value 
           pad-event:get-axis-value get-button pad-event:get-button 
           get-group-mode pad-event:get-group-mode new-empty 
           paintable:new-empty compute-concrete-size 
           paintable:compute-concrete-size get-current-image 
           paintable:get-current-image <GdkPaintableFlags> 
           list->paintable-flags number->paintable-flags paintable-flags->list 
           paintable-flags->number get-flags paintable:get-flags 
           get-intrinsic-aspect-ratio paintable:get-intrinsic-aspect-ratio 
           get-intrinsic-height paintable:get-intrinsic-height 
           get-intrinsic-width paintable:get-intrinsic-width 
           invalidate-contents paintable:invalidate-contents invalidate-size 
           paintable:invalidate-size <GdkSnapshot> snapshot paintable:snapshot 
           invalidate-contents paintable:invalidate-contents invalidate-size 
           paintable:invalidate-size <GdkPopup> get-autohide? 
           popup:get-autohide? get-parent popup:get-parent get-position-x 
           popup:get-position-x get-position-y popup:get-position-y 
           get-rect-anchor popup:get-rect-anchor get-surface-anchor 
           popup:get-surface-anchor <GdkPopupLayout> present? popup:present? 
           new popup-layout:new copy popup-layout:copy equal? 
           popup-layout:equal? get-anchor-hints popup-layout:get-anchor-hints 
           get-anchor-rect popup-layout:get-anchor-rect get-offset 
           popup-layout:get-offset get-rect-anchor popup-layout:get-rect-anchor 
           get-shadow-width popup-layout:get-shadow-width get-surface-anchor 
           popup-layout:get-surface-anchor ref popup-layout:ref 
           set-anchor-hints popup-layout:set-anchor-hints set-anchor-rect 
           popup-layout:set-anchor-rect set-offset popup-layout:set-offset 
           set-rect-anchor popup-layout:set-rect-anchor set-shadow-width 
           popup-layout:set-shadow-width set-surface-anchor 
           popup-layout:set-surface-anchor unref popup-layout:unref 
           <GdkProximityEvent> <GdkRGBA> copy rgba:copy equal? rgba:equal? free 
           rgba:free hash rgba:hash is-clear? rgba:is-clear? is-opaque? 
           rgba:is-opaque? parse? rgba:parse? to-string rgba:to-string 
           contains-point? rectangle:contains-point? equal? rectangle:equal? 
           intersect! rectangle:intersect! union! rectangle:union! 
           <GdkScrollDirection> symbol->scroll-direction 
           number->scroll-direction scroll-direction->symbol 
           scroll-direction->number <GdkScrollEvent> get-deltas 
           scroll-event:get-deltas get-direction scroll-event:get-direction 
           <GdkScrollUnit> symbol->scroll-unit number->scroll-unit 
           scroll-unit->symbol scroll-unit->number get-unit 
           scroll-event:get-unit is-stop? scroll-event:is-stop? 
           <GdkSeatCapabilities> list->seat-capabilities 
           number->seat-capabilities seat-capabilities->list 
           seat-capabilities->number get-capabilities seat:get-capabilities 
           get-devices seat:get-devices get-display seat:get-display 
           get-keyboard seat:get-keyboard get-pointer seat:get-pointer 
           get-tools seat:get-tools device-added seat:device-added 
           device-removed seat:device-removed tool-added seat:tool-added 
           tool-removed seat:tool-removed new-popup surface:new-popup 
           new-toplevel surface:new-toplevel beep surface:beep 
           create-cairo-context surface:create-cairo-context create-gl-context 
           surface:create-gl-context <CairoSurface> <cairo_content_t> 
           symbol->content number->content content->symbol content->number 
           create-similar-surface surface:create-similar-surface <GInitable> 
           <GdkVulkanContext> create-vulkan-context 
           surface:create-vulkan-context destroy surface:destroy get-cursor 
           surface:get-cursor get-device-cursor surface:get-device-cursor 
           get-device-position surface:get-device-position get-display 
           surface:get-display get-frame-clock surface:get-frame-clock 
           get-height surface:get-height get-mapped? surface:get-mapped? 
           get-scale-factor surface:get-scale-factor get-width 
           surface:get-width hide surface:hide is-destroyed? 
           surface:is-destroyed? queue-render surface:queue-render 
           request-layout surface:request-layout set-cursor surface:set-cursor 
           set-device-cursor surface:set-device-cursor set-input-region 
           surface:set-input-region set-opaque-region surface:set-opaque-region 
           translate-coordinates surface:translate-coordinates enter-monitor 
           surface:enter-monitor event surface:event layout surface:layout 
           leave-monitor surface:leave-monitor render surface:render 
           <GdkSurfaceEdge> symbol->surface-edge number->surface-edge 
           surface-edge->symbol surface-edge->number <GdkPixbuf> new-for-pixbuf 
           texture:new-for-pixbuf new-from-bytes texture:new-from-bytes 
           new-from-file texture:new-from-file new-from-filename 
           texture:new-from-filename new-from-resource 
           texture:new-from-resource download texture:download get-format 
           texture:get-format get-height texture:get-height get-width 
           texture:get-width save-to-png? texture:save-to-png? 
           save-to-png-bytes texture:save-to-png-bytes save-to-tiff? 
           texture:save-to-tiff? save-to-tiff-bytes texture:save-to-tiff-bytes 
           <GdkTextureDownloader> new texture-downloader:new copy 
           texture-downloader:copy download-bytes 
           texture-downloader:download-bytes download-into 
           texture-downloader:download-into free texture-downloader:free 
           get-format texture-downloader:get-format get-texture 
           texture-downloader:get-texture set-format 
           texture-downloader:set-format set-texture 
           texture-downloader:set-texture <GdkTextureError> 
           symbol->texture-error number->texture-error texture-error->symbol 
           texture-error->number quark texture-error:quark <GdkTitlebarGesture> 
           symbol->titlebar-gesture number->titlebar-gesture 
           titlebar-gesture->symbol titlebar-gesture->number <GdkToplevel> 
           begin-move toplevel:begin-move begin-resize toplevel:begin-resize 
           focus toplevel:focus <GdkToplevelState> list->toplevel-state 
           number->toplevel-state toplevel-state->list toplevel-state->number 
           get-state toplevel:get-state inhibit-system-shortcuts 
           toplevel:inhibit-system-shortcuts lower? toplevel:lower? minimize? 
           toplevel:minimize? <GdkToplevelLayout> present toplevel:present 
           restore-system-shortcuts toplevel:restore-system-shortcuts 
           set-decorated toplevel:set-decorated set-deletable 
           toplevel:set-deletable set-icon-list toplevel:set-icon-list 
           set-modal toplevel:set-modal set-startup-id toplevel:set-startup-id 
           set-title toplevel:set-title set-transient-for 
           toplevel:set-transient-for show-window-menu? 
           toplevel:show-window-menu? supports-edge-constraints? 
           toplevel:supports-edge-constraints? titlebar-gesture? 
           toplevel:titlebar-gesture? new toplevel-layout:new copy 
           toplevel-layout:copy equal? toplevel-layout:equal? get-fullscreen 
           toplevel-layout:get-fullscreen get-fullscreen-monitor 
           toplevel-layout:get-fullscreen-monitor get-maximized 
           toplevel-layout:get-maximized get-resizable? 
           toplevel-layout:get-resizable? ref toplevel-layout:ref 
           set-fullscreen toplevel-layout:set-fullscreen set-maximized 
           toplevel-layout:set-maximized set-resizable 
           toplevel-layout:set-resizable unref toplevel-layout:unref 
           <GdkTouchEvent> get-emulating-pointer? 
           touch-event:get-emulating-pointer? <GdkTouchpadEvent> get-deltas 
           touchpad-event:get-deltas <GdkTouchpadGesturePhase> 
           symbol->touchpad-gesture-phase number->touchpad-gesture-phase 
           touchpad-gesture-phase->symbol touchpad-gesture-phase->number 
           get-gesture-phase touchpad-event:get-gesture-phase get-n-fingers 
           touchpad-event:get-n-fingers get-pinch-angle-delta 
           touchpad-event:get-pinch-angle-delta get-pinch-scale 
           touchpad-event:get-pinch-scale images-updated 
           vulkan-context:images-updated <GdkVulkanError> symbol->vulkan-error 
           number->vulkan-error vulkan-error->symbol vulkan-error->number quark 
           vulkan-error:quark cairo-rectangle gdk:cairo-rectangle cairo-region 
           gdk:cairo-region cairo-region-create-from-surface 
           gdk:cairo-region-create-from-surface cairo-set-source-pixbuf 
           gdk:cairo-set-source-pixbuf cairo-set-source-rgba 
           gdk:cairo-set-source-rgba content-deserialize-async 
           gdk:content-deserialize-async content-deserialize-finish! 
           gdk:content-deserialize-finish! content-formats-parse 
           gdk:content-formats-parse content-register-deserializer 
           gdk:content-register-deserializer content-register-serializer 
           gdk:content-register-serializer content-serialize-async 
           gdk:content-serialize-async content-serialize-finish? 
           gdk:content-serialize-finish? drag-action-is-unique? 
           gdk:drag-action-is-unique? events-get-angle gdk:events-get-angle 
           events-get-center gdk:events-get-center events-get-distance 
           gdk:events-get-distance gl-error-quark gdk:gl-error-quark 
           intern-mime-type gdk:intern-mime-type keyval-convert-case 
           gdk:keyval-convert-case keyval-from-name gdk:keyval-from-name 
           keyval-is-lower? gdk:keyval-is-lower? keyval-is-upper? 
           gdk:keyval-is-upper? keyval-name gdk:keyval-name keyval-to-lower 
           gdk:keyval-to-lower keyval-to-unicode gdk:keyval-to-unicode 
           keyval-to-upper gdk:keyval-to-upper paintable-new-empty 
           gdk:paintable-new-empty pixbuf-get-from-surface 
           gdk:pixbuf-get-from-surface pixbuf-get-from-texture 
           gdk:pixbuf-get-from-texture set-allowed-backends 
           gdk:set-allowed-backends texture-error-quark gdk:texture-error-quark 
           toplevel-size-get-type gdk:toplevel-size-get-type unicode-to-keyval 
           gdk:unicode-to-keyval vulkan-error-quark gdk:vulkan-error-quark))

(eval-when (expand load eval)
(^library "Gdk" "4.0" '("libgtk-4-1.dll"))

(^initialize)

(^constant 'ACTION_ALL 7)

(^type 'Gdk
       '<GdkAnchorHints>
       "GdkAnchorHints"
       "gdk_anchor_hints_get_type")

(^flags-conv 'anchor-hints 'GdkAnchorHints)

(^type 'Gio
       '<GAppLaunchContext>
       "GAppLaunchContext"
       "g_app_launch_context_get_type")

(^type 'Gdk
       '<GdkAppLaunchContext>
       "GdkAppLaunchContext"
       "gdk_app_launch_context_get_type")

(^type 'Gdk
       '<GdkDisplay>
       "GdkDisplay"
       "gdk_display_get_type")

(^function
  'Gdk
  'GdkAppLaunchContext
  'app-launch-context:get-display
  'get-display
  'gdk_app_launch_context_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkAppLaunchContext
  'app-launch-context:set-desktop
  'set-desktop
  'gdk_app_launch_context_set_desktop
  '((name . "set_desktop")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "desktop")
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

(^type 'GObject
       '<GObject>
       "GObject"
       "g_object_get_type")

(^type 'Gio '<GIcon> "GIcon" "g_icon_get_type")

(^function
  'Gdk
  'GdkAppLaunchContext
  'app-launch-context:set-icon
  'set-icon
  'gdk_app_launch_context_set_icon
  '((name . "set_icon")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "icon")
            (meta (arg-type . GInterface)
                  (gtype-name . "GIcon")
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
  'Gdk
  'GdkAppLaunchContext
  'app-launch-context:set-icon-name
  'set-icon-name
  'gdk_app_launch_context_set_icon_name
  '((name . "set_icon_name")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "icon_name")
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
  'Gdk
  'GdkAppLaunchContext
  'app-launch-context:set-timestamp
  'set-timestamp
  'gdk_app_launch_context_set_timestamp
  '((name . "set_timestamp")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "timestamp")
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

(^property
  'GdkAppLaunchContext
  'app-launch-context:display
  'display
  'display)

(^type 'Gdk
       '<GdkAxisFlags>
       "GdkAxisFlags"
       "gdk_axis_flags_get_type")

(^flags-conv 'axis-flags 'GdkAxisFlags)

(^type 'Gdk
       '<GdkAxisUse>
       "GdkAxisUse"
       "gdk_axis_use_get_type")

(^enum-conv 'axis-use 'GdkAxisUse)

(^constant 'BUTTON_MIDDLE 2)

(^constant 'BUTTON_PRIMARY 1)

(^constant 'BUTTON_SECONDARY 3)

(^custom-type
  'Gdk
  '<GdkEvent>
  "GdkEvent"
  "gdk_event_get_type"
  "gdk_event_ref"
  "gdk_event_unref")

(^type 'Gdk
       '<GdkButtonEvent>
       "GdkButtonEvent"
       "gdk_button_event_get_type")

(^function
  'Gdk
  'GdkButtonEvent
  'button-event:get-button
  'get-button
  'gdk_button_event_get_button
  '((name . "get_button")
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

(^constant 'CURRENT_TIME 0)

(^type 'Gdk
       '<GdkDrawContext>
       "GdkDrawContext"
       "gdk_draw_context_get_type")

(^type 'Gdk
       '<GdkCairoContext>
       "GdkCairoContext"
       "gdk_cairo_context_get_type")

(^type 'cairo
       '<CairoContext>
       "CairoContext"
       "cairo_gobject_context_get_type")

(^function
  'Gdk
  'GdkCairoContext
  'cairo-context:cairo-create
  'cairo-create
  'gdk_cairo_context_cairo_create
  '((name . "cairo_create")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoContext")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkClipboard>
       "GdkClipboard"
       "gdk_clipboard_get_type")

(^type 'Gdk
       '<GdkContentProvider>
       "GdkContentProvider"
       "gdk_content_provider_get_type")

(^function
  'Gdk
  'GdkClipboard
  'clipboard:get-content
  'get-content
  'gdk_clipboard_get_content
  '((name . "get_content")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkContentProvider")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:get-display
  'get-display
  'gdk_clipboard_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkContentFormats>
       "GdkContentFormats"
       "gdk_content_formats_get_type")

(^function
  'Gdk
  'GdkClipboard
  'clipboard:get-formats
  'get-formats
  'gdk_clipboard_get_formats
  '((name . "get_formats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:is-local?
  'is-local?
  'gdk_clipboard_is_local
  '((name . "is_local")
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

(^type 'Gio
       '<GCancellable>
       "GCancellable"
       "g_cancellable_get_type")

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-async
  'read-async
  'gdk_clipboard_read_async
  '((name . "read_async")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 3)
    (c-input-len . 5)
    (pdata ((name . "mime_types")
            (meta (arg-type . array)
                  (flags ptr in zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "io_priority")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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

(^type 'Gio
       '<GInputStream>
       "GInputStream"
       "g_input_stream_get_type")

(^type 'Gio
       '<GAsyncResult>
       "GAsyncResult"
       "g_async_result_get_type")

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-finish
  'read-finish
  'gdk_clipboard_read_finish
  '((name . "read_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "out_mime_type")
            (meta (arg-type . utf8-string)
                  (flags ptr out optional)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GInputStream")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-text-async
  'read-text-async
  'gdk_clipboard_read_text_async
  '((name . "read_text_async")
    (is-method . #t)
    (s-input-opt . 3)
    (c-input-len . 3)
    (pdata ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-text-finish
  'read-text-finish
  'gdk_clipboard_read_text_finish
  '((name . "read_text_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-texture-async
  'read-texture-async
  'gdk_clipboard_read_texture_async
  '((name . "read_texture_async")
    (is-method . #t)
    (s-input-opt . 3)
    (c-input-len . 3)
    (pdata ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkPaintable>
       "GdkPaintable"
       "gdk_paintable_get_type")

(^type 'Gio
       '<GLoadableIcon>
       "GLoadableIcon"
       "g_loadable_icon_get_type")

(^type 'Gdk
       '<GdkTexture>
       "GdkTexture"
       "gdk_texture_get_type")

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-texture-finish
  'read-texture-finish
  'gdk_clipboard_read_texture_finish
  '((name . "read_texture_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-value-async
  'read-value-async
  'gdk_clipboard_read_value_async
  '((name . "read_value_async")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 3)
    (c-input-len . 5)
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
           ((name . "io_priority")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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

(^sized-type
  'GObject
  '<GValue>
  "GValue"
  "g_value_get_type"
  24)

(^function
  'Gdk
  'GdkClipboard
  'clipboard:read-value-finish
  'read-value-finish
  'gdk_clipboard_read_value_finish
  '((name . "read_value_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
  'Gdk
  'GdkClipboard
  'clipboard:set-content?
  'set-content?
  'gdk_clipboard_set_content
  '((name . "set_content")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "provider")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkContentProvider")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:set
  'set
  'gdk_clipboard_set_value
  '((name . "set")
    (is-method . #t)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkClipboard
  'clipboard:store-async
  'store-async
  'gdk_clipboard_store_async
  '((name . "store_async")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 3)
    (c-input-len . 4)
    (pdata ((name . "io_priority")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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

(^function
  'Gdk
  'GdkClipboard
  'clipboard:store-finish?
  'store-finish?
  'gdk_clipboard_store_finish
  '((name . "store_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkClipboard
  'clipboard:content
  'content
  'content)

(^property
  'GdkClipboard
  'clipboard:display
  'display
  'display)

(^property
  'GdkClipboard
  'clipboard:formats
  'formats
  'formats)

(^property
  'GdkClipboard
  'clipboard:local
  'local
  'local)

(^signal
  'Gdk
  'GdkClipboard
  'clipboard:changed
  'changed
  'changed
  '((name . "changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkContentDeserializer>
       "GdkContentDeserializer"
       "gdk_content_deserializer_get_type")

(^function
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-cancellable
  'get-cancellable
  'gdk_content_deserializer_get_cancellable
  '((name . "get_cancellable")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GCancellable")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-gtype
  'get-gtype
  'gdk_content_deserializer_get_gtype
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-input-stream
  'get-input-stream
  'gdk_content_deserializer_get_input_stream
  '((name . "get_input_stream")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GInputStream")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-mime-type
  'get-mime-type
  'gdk_content_deserializer_get_mime_type
  '((name . "get_mime_type")
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-priority
  'get-priority
  'gdk_content_deserializer_get_priority
  '((name . "get_priority")
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-task-data
  'get-task-data
  'gdk_content_deserializer_get_task_data
  '((name . "get_task_data")
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-user-data
  'get-user-data
  'gdk_content_deserializer_get_user_data
  '((name . "get_user_data")
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:get-value
  'get-value
  'gdk_content_deserializer_get_value
  '((name . "get_value")
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:return-error
  'return-error
  'gdk_content_deserializer_return_error
  '((name . "return_error")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "error")
            (meta (arg-type . GError)
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . everything))
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
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:return-success
  'return-success
  'gdk_content_deserializer_return_success
  '((name . "return_success")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentDeserializer
  'content-deserializer:set-task-data
  'set-task-data
  'gdk_content_deserializer_set_task_data
  '((name . "set_task_data")
    (is-method . #t)
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
           ((name . "notify")
            (meta (arg-type . callback)
                  (flags in)
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
  'Gdk
  'GdkContentFormats
  'content-formats:new
  'new
  'gdk_content_formats_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "mime_types")
            (meta (arg-type . array)
                  (flags ptr in nullable sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_mime_types")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:new-for-gtype
  'new-for-gtype
  'gdk_content_formats_new_for_gtype
  '((name . "new_for_gtype")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:contain-gtype?
  'contain-gtype?
  'gdk_content_formats_contain_gtype
  '((name . "contain_gtype")
    (is-method . #t)
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
  'Gdk
  'GdkContentFormats
  'content-formats:contain-mime-type?
  'contain-mime-type?
  'gdk_content_formats_contain_mime_type
  '((name . "contain_mime_type")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "mime_type")
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
  'Gdk
  'GdkContentFormats
  'content-formats:get-gtypes
  'get-gtypes
  'gdk_content_formats_get_gtypes
  '((name . "get_gtypes")
    (is-method . #t)
    (c-output-len . 1)
    (pdata ((name . "n_gtypes")
            (meta (arg-type . uint64)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out nullable zero-terminated sized)
            (transfer . nothing)
            (params
              ((arg-type . GType)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:get-mime-types
  'get-mime-types
  'gdk_content_formats_get_mime_types
  '((name . "get_mime_types")
    (is-method . #t)
    (c-output-len . 1)
    (pdata ((name . "n_mime_types")
            (meta (arg-type . uint64)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out nullable zero-terminated sized)
            (transfer . nothing)
            (params
              ((arg-type . utf8-string)
               (flags ptr)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:match?
  'match?
  'gdk_content_formats_match
  '((name . "match")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "second")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkContentFormats")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:match-gtype
  'match-gtype
  'gdk_content_formats_match_gtype
  '((name . "match_gtype")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "second")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkContentFormats")
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
      (meta (arg-type . GType)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:match-mime-type
  'match-mime-type
  'gdk_content_formats_match_mime_type
  '((name . "match_mime_type")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "second")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkContentFormats")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'GLib
  '<GString>
  "GString"
  "g_gstring_get_type"
  24)

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:print
  'print
  'gdk_content_formats_print
  '((name . "print")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "string")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GString")
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
  'Gdk
  'GdkContentFormats
  'content-formats:ref
  'ref
  'gdk_content_formats_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:to-string
  'to-string
  'gdk_content_formats_to_string
  '((name . "to_string")
    (is-method . #t)
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
  'Gdk
  'GdkContentFormats
  'content-formats:union
  'union
  'gdk_content_formats_union
  '((name . "union")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "second")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkContentFormats")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:union-deserialize-gtypes
  'union-deserialize-gtypes
  'gdk_content_formats_union_deserialize_gtypes
  '((name . "union_deserialize_gtypes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:union-deserialize-mime-types
  'union-deserialize-mime-types
  'gdk_content_formats_union_deserialize_mime_types
  '((name . "union_deserialize_mime_types")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:union-serialize-gtypes
  'union-serialize-gtypes
  'gdk_content_formats_union_serialize_gtypes
  '((name . "union_serialize_gtypes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:union-serialize-mime-types
  'union-serialize-mime-types
  'gdk_content_formats_union_serialize_mime_types
  '((name . "union_serialize_mime_types")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormats
  'content-formats:unref
  'unref
  'gdk_content_formats_unref
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
  'Gdk
  'GdkContentFormats
  'content-formats:parse
  'parse
  'gdk_content_formats_parse
  '((name . "parse")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "string")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkContentFormatsBuilder>
       "GdkContentFormatsBuilder"
       "gdk_content_formats_builder_get_type")

(^function
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:new
  'new
  'gdk_content_formats_builder_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormatsBuilder")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:add-formats
  'add-formats
  'gdk_content_formats_builder_add_formats
  '((name . "add_formats")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "formats")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkContentFormats")
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
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:add-gtype
  'add-gtype
  'gdk_content_formats_builder_add_gtype
  '((name . "add_gtype")
    (is-method . #t)
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
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:add-mime-type
  'add-mime-type
  'gdk_content_formats_builder_add_mime_type
  '((name . "add_mime_type")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "mime_type")
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
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:ref
  'ref
  'gdk_content_formats_builder_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormatsBuilder")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:to-formats
  'to-formats
  'gdk_content_formats_builder_to_formats
  '((name . "to_formats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentFormatsBuilder
  'content-formats-builder:unref
  'unref
  'gdk_content_formats_builder_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GLib
       '<GBytes>
       "GBytes"
       "g_bytes_get_type")

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:new-for-bytes
  'new-for-bytes
  'gdk_content_provider_new_for_bytes
  '((name . "new_for_bytes")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "mime_type")
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
           ((name . "bytes")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkContentProvider")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:new-for-value
  'new-for-value
  'gdk_content_provider_new_for_value
  '((name . "new_for_value")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkContentProvider")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:new-union
  'new-union
  'gdk_content_provider_new_union
  '((name . "new_union")
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "providers")
            (meta (arg-type . array)
                  (flags ptr in nullable sized)
                  (length-arg . 1)
                  (transfer . everything)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "GdkContentProvider")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_providers")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkContentProvider")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:content-changed
  'content-changed
  'gdk_content_provider_content_changed
  '((name . "content_changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:get-value!
  'get-value!
  'gdk_content_provider_get_value
  '((name . "get_value")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GValue")
                  (flags out preallocated)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkContentProvider
  'content-provider:ref-formats
  'ref-formats
  'gdk_content_provider_ref_formats
  '((name . "ref_formats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:ref-storable-formats
  'ref-storable-formats
  'gdk_content_provider_ref_storable_formats
  '((name . "ref_storable_formats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gio
       '<GOutputStream>
       "GOutputStream"
       "g_output_stream_get_type")

(^function
  'Gdk
  'GdkContentProvider
  'content-provider:write-mime-type-async
  'write-mime-type-async
  'gdk_content_provider_write_mime_type_async
  '((name . "write_mime_type_async")
    (is-method . #t)
    (s-input-req . 3)
    (s-input-opt . 3)
    (c-input-len . 6)
    (pdata ((name . "mime_type")
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
           ((name . "stream")
            (meta (arg-type . GObject)
                  (gtype-name . "GOutputStream")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "io_priority")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "user_data")
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
  'Gdk
  'GdkContentProvider
  'content-provider:write-mime-type-finish?
  'write-mime-type-finish?
  'gdk_content_provider_write_mime_type_finish
  '((name . "write_mime_type_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkContentProvider
  'content-provider:formats
  'formats
  'formats)

(^property
  'GdkContentProvider
  'content-provider:storable-formats
  'storable-formats
  'storable-formats)

(^signal
  'Gdk
  'GdkContentProvider
  'content-provider:content-changed
  'content-changed
  'content-changed
  '((name . "content-changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkContentSerializer>
       "GdkContentSerializer"
       "gdk_content_serializer_get_type")

(^function
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-cancellable
  'get-cancellable
  'gdk_content_serializer_get_cancellable
  '((name . "get_cancellable")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GCancellable")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-gtype
  'get-gtype
  'gdk_content_serializer_get_gtype
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-mime-type
  'get-mime-type
  'gdk_content_serializer_get_mime_type
  '((name . "get_mime_type")
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-output-stream
  'get-output-stream
  'gdk_content_serializer_get_output_stream
  '((name . "get_output_stream")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GOutputStream")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-priority
  'get-priority
  'gdk_content_serializer_get_priority
  '((name . "get_priority")
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-task-data
  'get-task-data
  'gdk_content_serializer_get_task_data
  '((name . "get_task_data")
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-user-data
  'get-user-data
  'gdk_content_serializer_get_user_data
  '((name . "get_user_data")
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:get-value
  'get-value
  'gdk_content_serializer_get_value
  '((name . "get_value")
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:return-error
  'return-error
  'gdk_content_serializer_return_error
  '((name . "return_error")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "error")
            (meta (arg-type . GError)
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . everything))
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
  'Gdk
  'GdkContentSerializer
  'content-serializer:return-success
  'return-success
  'gdk_content_serializer_return_success
  '((name . "return_success")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkContentSerializer
  'content-serializer:set-task-data
  'set-task-data
  'gdk_content_serializer_set_task_data
  '((name . "set_task_data")
    (is-method . #t)
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
           ((name . "notify")
            (meta (arg-type . callback)
                  (flags in)
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

(^type 'Gdk
       '<GdkCrossingEvent>
       "GdkCrossingEvent"
       "gdk_crossing_event_get_type")

(^type 'Gdk
       '<GdkNotifyType>
       "GdkNotifyType"
       "gdk_notify_type_get_type")

(^enum-conv 'notify-type 'GdkNotifyType)

(^function
  'Gdk
  'GdkCrossingEvent
  'crossing-event:get-detail
  'get-detail
  'gdk_crossing_event_get_detail
  '((name . "get_detail")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkNotifyType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkCrossingEvent
  'crossing-event:get-focus?
  'get-focus?
  'gdk_crossing_event_get_focus
  '((name . "get_focus")
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

(^type 'Gdk
       '<GdkCrossingMode>
       "GdkCrossingMode"
       "gdk_crossing_mode_get_type")

(^enum-conv 'crossing-mode 'GdkCrossingMode)

(^function
  'Gdk
  'GdkCrossingEvent
  'crossing-event:get-mode
  'get-mode
  'gdk_crossing_event_get_mode
  '((name . "get_mode")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkCrossingMode")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkCursor>
       "GdkCursor"
       "gdk_cursor_get_type")

(^function
  'Gdk
  'GdkCursor
  'cursor:new-from-name
  'new-from-name
  'gdk_cursor_new_from_name
  '((name . "new_from_name")
    (s-input-req . 1)
    (s-input-opt . 1)
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
           ((name . "fallback")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkCursor")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkCursor")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkCursor
  'cursor:new-from-texture
  'new-from-texture
  'gdk_cursor_new_from_texture
  '((name . "new_from_texture")
    (s-input-req . 3)
    (s-input-opt . 1)
    (c-input-len . 4)
    (pdata ((name . "texture")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkTexture")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "hotspot_x")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "hotspot_y")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "fallback")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkCursor")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkCursor")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkCursor
  'cursor:get-fallback
  'get-fallback
  'gdk_cursor_get_fallback
  '((name . "get_fallback")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkCursor")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkCursor
  'cursor:get-hotspot-x
  'get-hotspot-x
  'gdk_cursor_get_hotspot_x
  '((name . "get_hotspot_x")
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
  'Gdk
  'GdkCursor
  'cursor:get-hotspot-y
  'get-hotspot-y
  'gdk_cursor_get_hotspot_y
  '((name . "get_hotspot_y")
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
  'Gdk
  'GdkCursor
  'cursor:get-name
  'get-name
  'gdk_cursor_get_name
  '((name . "get_name")
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
  'Gdk
  'GdkCursor
  'cursor:get-texture
  'get-texture
  'gdk_cursor_get_texture
  '((name . "get_texture")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkCursor
  'cursor:fallback
  'fallback
  'fallback)

(^property
  'GdkCursor
  'cursor:hotspot-x
  'hotspot-x
  'hotspot-x)

(^property
  'GdkCursor
  'cursor:hotspot-y
  'hotspot-y
  'hotspot-y)

(^property 'GdkCursor 'cursor:name 'name 'name)

(^property
  'GdkCursor
  'cursor:texture
  'texture
  'texture)

(^type 'Gdk
       '<GdkDNDEvent>
       "GdkDNDEvent"
       "gdk_dnd_event_get_type")

(^type 'Gdk
       '<GdkDrop>
       "GdkDrop"
       "gdk_drop_get_type")

(^function
  'Gdk
  'GdkDNDEvent
  'dndevent:get-drop
  'get-drop
  'gdk_dnd_event_get_drop
  '((name . "get_drop")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDrop")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkDeleteEvent>
       "GdkDeleteEvent"
       "gdk_delete_event_get_type")

(^type 'Gdk
       '<GdkDevice>
       "GdkDevice"
       "gdk_device_get_type")

(^function
  'Gdk
  'GdkDevice
  'device:get-caps-lock-state?
  'get-caps-lock-state?
  'gdk_device_get_caps_lock_state
  '((name . "get_caps_lock_state")
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

(^type 'Gdk
       '<GdkDeviceTool>
       "GdkDeviceTool"
       "gdk_device_tool_get_type")

(^function
  'Gdk
  'GdkDevice
  'device:get-device-tool
  'get-device-tool
  'gdk_device_get_device_tool
  '((name . "get_device_tool")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDeviceTool")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoDirection>
       "PangoDirection"
       "pango_direction_get_type")

(^enum-conv 'direction 'PangoDirection)

(^function
  'Gdk
  'GdkDevice
  'device:get-direction
  'get-direction
  'gdk_device_get_direction
  '((name . "get_direction")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoDirection")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevice
  'device:get-display
  'get-display
  'gdk_device_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevice
  'device:get-has-cursor?
  'get-has-cursor?
  'gdk_device_get_has_cursor
  '((name . "get_has_cursor")
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

(^type 'Gdk
       '<GdkModifierType>
       "GdkModifierType"
       "gdk_modifier_type_get_type")

(^flags-conv 'modifier-type 'GdkModifierType)

(^function
  'Gdk
  'GdkDevice
  'device:get-modifier-state
  'get-modifier-state
  'gdk_device_get_modifier_state
  '((name . "get_modifier_state")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkModifierType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevice
  'device:get-name
  'get-name
  'gdk_device_get_name
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
  'Gdk
  'GdkDevice
  'device:get-num-lock-state?
  'get-num-lock-state?
  'gdk_device_get_num_lock_state
  '((name . "get_num_lock_state")
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
  'Gdk
  'GdkDevice
  'device:get-num-touches
  'get-num-touches
  'gdk_device_get_num_touches
  '((name . "get_num_touches")
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
  'Gdk
  'GdkDevice
  'device:get-product-id
  'get-product-id
  'gdk_device_get_product_id
  '((name . "get_product_id")
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
  'Gdk
  'GdkDevice
  'device:get-scroll-lock-state?
  'get-scroll-lock-state?
  'gdk_device_get_scroll_lock_state
  '((name . "get_scroll_lock_state")
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

(^type 'Gdk
       '<GdkSeat>
       "GdkSeat"
       "gdk_seat_get_type")

(^function
  'Gdk
  'GdkDevice
  'device:get-seat
  'get-seat
  'gdk_device_get_seat
  '((name . "get_seat")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSeat")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkInputSource>
       "GdkInputSource"
       "gdk_input_source_get_type")

(^enum-conv 'input-source 'GdkInputSource)

(^function
  'Gdk
  'GdkDevice
  'device:get-source
  'get-source
  'gdk_device_get_source
  '((name . "get_source")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkInputSource")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkSurface>
       "GdkSurface"
       "gdk_surface_get_type")

(^function
  'Gdk
  'GdkDevice
  'device:get-surface-at-position
  'get-surface-at-position
  'gdk_device_get_surface_at_position
  '((name . "get_surface_at_position")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "win_x")
            (meta (arg-type . double)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "win_y")
            (meta (arg-type . double)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevice
  'device:get-timestamp
  'get-timestamp
  'gdk_device_get_timestamp
  '((name . "get_timestamp")
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
  'Gdk
  'GdkDevice
  'device:get-vendor-id
  'get-vendor-id
  'gdk_device_get_vendor_id
  '((name . "get_vendor_id")
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
  'Gdk
  'GdkDevice
  'device:has-bidi-layouts?
  'has-bidi-layouts?
  'gdk_device_has_bidi_layouts
  '((name . "has_bidi_layouts")
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

(^property
  'GdkDevice
  'device:caps-lock-state
  'caps-lock-state
  'caps-lock-state)

(^property
  'GdkDevice
  'device:direction
  'direction
  'direction)

(^property
  'GdkDevice
  'device:display
  'display
  'display)

(^property
  'GdkDevice
  'device:has-bidi-layouts
  'has-bidi-layouts
  'has-bidi-layouts)

(^property
  'GdkDevice
  'device:has-cursor
  'has-cursor
  'has-cursor)

(^property
  'GdkDevice
  'device:modifier-state
  'modifier-state
  'modifier-state)

(^property
  'GdkDevice
  'device:n-axes
  'n-axes
  'n-axes)

(^property 'GdkDevice 'device:name 'name 'name)

(^property
  'GdkDevice
  'device:num-lock-state
  'num-lock-state
  'num-lock-state)

(^property
  'GdkDevice
  'device:num-touches
  'num-touches
  'num-touches)

(^property
  'GdkDevice
  'device:product-id
  'product-id
  'product-id)

(^property
  'GdkDevice
  'device:scroll-lock-state
  'scroll-lock-state
  'scroll-lock-state)

(^property 'GdkDevice 'device:seat 'seat 'seat)

(^property
  'GdkDevice
  'device:source
  'source
  'source)

(^property 'GdkDevice 'device:tool 'tool 'tool)

(^property
  'GdkDevice
  'device:vendor-id
  'vendor-id
  'vendor-id)

(^signal
  'Gdk
  'GdkDevice
  'device:changed
  'changed
  'changed
  '((name . "changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkDevice
  'device:tool-changed
  'tool-changed
  'tool-changed
  '((name . "tool-changed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "tool")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDeviceTool")
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

(^type 'Gdk
       '<GdkDevicePad>
       "GdkDevicePad"
       "gdk_device_pad_get_type")

(^type 'Gdk
       '<GdkDevicePadFeature>
       "GdkDevicePadFeature"
       "gdk_device_pad_feature_get_type")

(^enum-conv
  'device-pad-feature
  'GdkDevicePadFeature)

(^function
  'Gdk
  'GdkDevicePad
  'device-pad:get-feature-group
  'get-feature-group
  'gdk_device_pad_get_feature_group
  '((name . "get_feature_group")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "feature")
            (meta (arg-type . enum)
                  (gtype-name . "GdkDevicePadFeature")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "feature_idx")
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
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevicePad
  'device-pad:get-group-n-modes
  'get-group-n-modes
  'gdk_device_pad_get_group_n_modes
  '((name . "get_group_n_modes")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "group_idx")
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
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevicePad
  'device-pad:get-n-features
  'get-n-features
  'gdk_device_pad_get_n_features
  '((name . "get_n_features")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "feature")
            (meta (arg-type . enum)
                  (gtype-name . "GdkDevicePadFeature")
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
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDevicePad
  'device-pad:get-n-groups
  'get-n-groups
  'gdk_device_pad_get_n_groups
  '((name . "get_n_groups")
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
  'Gdk
  'GdkDeviceTool
  'device-tool:get-axes
  'get-axes
  'gdk_device_tool_get_axes
  '((name . "get_axes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkAxisFlags")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDeviceTool
  'device-tool:get-hardware-id
  'get-hardware-id
  'gdk_device_tool_get_hardware_id
  '((name . "get_hardware_id")
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
  'Gdk
  'GdkDeviceTool
  'device-tool:get-serial
  'get-serial
  'gdk_device_tool_get_serial
  '((name . "get_serial")
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

(^type 'Gdk
       '<GdkDeviceToolType>
       "GdkDeviceToolType"
       "gdk_device_tool_type_get_type")

(^enum-conv 'device-tool-type 'GdkDeviceToolType)

(^function
  'Gdk
  'GdkDeviceTool
  'device-tool:get-tool-type
  'get-tool-type
  'gdk_device_tool_get_tool_type
  '((name . "get_tool_type")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkDeviceToolType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkDeviceTool
  'device-tool:axes
  'axes
  'axes)

(^property
  'GdkDeviceTool
  'device-tool:hardware-id
  'hardware-id
  'hardware-id)

(^property
  'GdkDeviceTool
  'device-tool:serial
  'serial
  'serial)

(^property
  'GdkDeviceTool
  'device-tool:tool-type
  'tool-type
  'tool-type)

(^function
  'Gdk
  'GdkDisplay
  'display:get-default
  'get-default
  'gdk_display_get_default
  '((name . "get_default")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:open
  'open
  'gdk_display_open
  '((name . "open")
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "display_name")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:beep
  'beep
  'gdk_display_beep
  '((name . "beep")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:close
  'close
  'gdk_display_close
  '((name . "close")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkGLContext>
       "GdkGLContext"
       "gdk_gl_context_get_type")

(^function
  'Gdk
  'GdkDisplay
  'display:create-gl-context
  'create-gl-context
  'gdk_display_create_gl_context
  '((name . "create_gl_context")
    (is-method . #t)
    (can-throw-gerror . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkGLContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:device-is-grabbed?
  'device-is-grabbed?
  'gdk_display_device_is_grabbed
  '((name . "device_is_grabbed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:flush
  'flush
  'gdk_display_flush
  '((name . "flush")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:get-app-launch-context
  'get-app-launch-context
  'gdk_display_get_app_launch_context
  '((name . "get_app_launch_context")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkAppLaunchContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:get-clipboard
  'get-clipboard
  'gdk_display_get_clipboard
  '((name . "get_clipboard")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkClipboard")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:get-default-seat
  'get-default-seat
  'gdk_display_get_default_seat
  '((name . "get_default_seat")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSeat")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkMonitor>
       "GdkMonitor"
       "gdk_monitor_get_type")

(^function
  'Gdk
  'GdkDisplay
  'display:get-monitor-at-surface
  'get-monitor-at-surface
  'gdk_display_get_monitor_at_surface
  '((name . "get_monitor_at_surface")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "surface")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSurface")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkMonitor")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gio
       '<GListModel>
       "GListModel"
       "g_list_model_get_type")

(^function
  'Gdk
  'GdkDisplay
  'display:get-monitors
  'get-monitors
  'gdk_display_get_monitors
  '((name . "get_monitors")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GInterface)
            (gtype-name . "GListModel")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:get-name
  'get-name
  'gdk_display_get_name
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
  'Gdk
  'GdkDisplay
  'display:get-primary-clipboard
  'get-primary-clipboard
  'gdk_display_get_primary_clipboard
  '((name . "get_primary_clipboard")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkClipboard")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:get-setting?
  'get-setting?
  'gdk_display_get_setting
  '((name . "get_setting")
    (is-method . #t)
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
  'Gdk
  'GdkDisplay
  'display:get-startup-notification-id
  'get-startup-notification-id
  'gdk_display_get_startup_notification_id
  '((name . "get_startup_notification_id")
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
  'Gdk
  'GdkDisplay
  'display:is-closed?
  'is-closed?
  'gdk_display_is_closed
  '((name . "is_closed")
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
  'Gdk
  'GdkDisplay
  'display:is-composited?
  'is-composited?
  'gdk_display_is_composited
  '((name . "is_composited")
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
  'Gdk
  'GdkDisplay
  'display:is-rgba?
  'is-rgba?
  'gdk_display_is_rgba
  '((name . "is_rgba")
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
  'Gdk
  'GdkDisplay
  'display:list-seats
  'list-seats
  'gdk_display_list_seats
  '((name . "list_seats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GList)
            (flags ptr out)
            (transfer . container)
            (params
              ((arg-type . GObject)
               (gtype-name . "GdkSeat")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:notify-startup-complete
  'notify-startup-complete
  'gdk_display_notify_startup_complete
  '((name . "notify_startup_complete")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "startup_id")
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
  'Gdk
  'GdkDisplay
  'display:prepare-gl?
  'prepare-gl?
  'gdk_display_prepare_gl
  '((name . "prepare_gl")
    (is-method . #t)
    (can-throw-gerror . #t)
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
  'Gdk
  'GdkDisplay
  'display:put-event
  'put-event
  'gdk_display_put_event
  '((name . "put_event")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "event")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
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
  'Gdk
  'GdkDisplay
  'display:supports-input-shapes?
  'supports-input-shapes?
  'gdk_display_supports_input_shapes
  '((name . "supports_input_shapes")
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
  'Gdk
  'GdkDisplay
  'display:sync
  'sync
  'gdk_display_sync
  '((name . "sync")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplay
  'display:translate-key
  'translate-key
  'gdk_display_translate_key
  '((name . "translate_key")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 4)
    (c-input-len . 3)
    (c-output-len . 4)
    (pdata ((name . "keycode")
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
           ((name . "state")
            (meta (arg-type . flags)
                  (gtype-name . "GdkModifierType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "group")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "keyval")
            (meta (arg-type . uint32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "effective_group")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "level")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 5)
            (c-output-pos . 2)
            (s-output-pos . 2))
           ((name . "consumed")
            (meta (arg-type . flags)
                  (gtype-name . "GdkModifierType")
                  (flags out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 6)
            (c-output-pos . 3)
            (s-output-pos . 3)))
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

(^property
  'GdkDisplay
  'display:composited
  'composited
  'composited)

(^property
  'GdkDisplay
  'display:input-shapes
  'input-shapes
  'input-shapes)

(^property 'GdkDisplay 'display:rgba 'rgba 'rgba)

(^signal
  'Gdk
  'GdkDisplay
  'display:closed
  'closed
  'closed
  '((name . "closed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "is_error")
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

(^signal
  'Gdk
  'GdkDisplay
  'display:opened
  'opened
  'opened
  '((name . "opened")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkDisplay
  'display:seat-added
  'seat-added
  'seat-added
  '((name . "seat-added")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "seat")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSeat")
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
  'Gdk
  'GdkDisplay
  'display:seat-removed
  'seat-removed
  'seat-removed
  '((name . "seat-removed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "seat")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSeat")
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
  'Gdk
  'GdkDisplay
  'display:setting-changed
  'setting-changed
  'setting-changed
  '((name . "setting-changed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "setting")
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

(^type 'Gdk
       '<GdkDisplayManager>
       "GdkDisplayManager"
       "gdk_display_manager_get_type")

(^function
  'Gdk
  'GdkDisplayManager
  'display-manager:get
  'get
  'gdk_display_manager_get
  '((name . "get")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplayManager")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplayManager
  'display-manager:get-default-display
  'get-default-display
  'gdk_display_manager_get_default_display
  '((name . "get_default_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplayManager
  'display-manager:list-displays
  'list-displays
  'gdk_display_manager_list_displays
  '((name . "list_displays")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . container)
            (params
              ((arg-type . GObject)
               (gtype-name . "GdkDisplay")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplayManager
  'display-manager:open-display
  'open-display
  'gdk_display_manager_open_display
  '((name . "open_display")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDisplayManager
  'display-manager:set-default-display
  'set-default-display
  'gdk_display_manager_set_default_display
  '((name . "set_default_display")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "display")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDisplay")
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

(^property
  'GdkDisplayManager
  'display-manager:default-display
  'default-display
  'default-display)

(^signal
  'Gdk
  'GdkDisplayManager
  'display-manager:display-opened
  'display-opened
  'display-opened
  '((name . "display-opened")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "display")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDisplay")
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

(^type 'Gdk
       '<GdkDrag>
       "GdkDrag"
       "gdk_drag_get_type")

(^type 'Gdk
       '<GdkDragAction>
       "GdkDragAction"
       "gdk_drag_action_get_type")

(^flags-conv 'drag-action 'GdkDragAction)

(^function
  'Gdk
  'GdkDrag
  'drag:begin
  'begin
  'gdk_drag_begin
  '((name . "begin")
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "surface")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSurface")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "content")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkContentProvider")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "actions")
            (meta (arg-type . flags)
                  (gtype-name . "GdkDragAction")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "dx")
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
           ((name . "dy")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
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
            (gtype-name . "GdkDrag")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:drop-done
  'drop-done
  'gdk_drag_drop_done
  '((name . "drop_done")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "success")
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
  'Gdk
  'GdkDrag
  'drag:get-actions
  'get-actions
  'gdk_drag_get_actions
  '((name . "get_actions")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkDragAction")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-content
  'get-content
  'gdk_drag_get_content
  '((name . "get_content")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkContentProvider")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-device
  'get-device
  'gdk_drag_get_device
  '((name . "get_device")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDevice")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-display
  'get-display
  'gdk_drag_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-drag-surface
  'get-drag-surface
  'gdk_drag_get_drag_surface
  '((name . "get_drag_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-formats
  'get-formats
  'gdk_drag_get_formats
  '((name . "get_formats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-selected-action
  'get-selected-action
  'gdk_drag_get_selected_action
  '((name . "get_selected_action")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkDragAction")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:get-surface
  'get-surface
  'gdk_drag_get_surface
  '((name . "get_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrag
  'drag:set-hotspot
  'set-hotspot
  'gdk_drag_set_hotspot
  '((name . "set_hotspot")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "hot_x")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "hot_y")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkDrag
  'drag:actions
  'actions
  'actions)

(^property
  'GdkDrag
  'drag:content
  'content
  'content)

(^property 'GdkDrag 'drag:device 'device 'device)

(^property
  'GdkDrag
  'drag:display
  'display
  'display)

(^property
  'GdkDrag
  'drag:formats
  'formats
  'formats)

(^property
  'GdkDrag
  'drag:selected-action
  'selected-action
  'selected-action)

(^property
  'GdkDrag
  'drag:surface
  'surface
  'surface)

(^type 'Gdk
       '<GdkDragCancelReason>
       "GdkDragCancelReason"
       "gdk_drag_cancel_reason_get_type")

(^enum-conv
  'drag-cancel-reason
  'GdkDragCancelReason)

(^signal
  'Gdk
  'GdkDrag
  'drag:cancel
  'cancel
  'cancel
  '((name . "cancel")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "reason")
            (meta (arg-type . enum)
                  (gtype-name . "GdkDragCancelReason")
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
  'Gdk
  'GdkDrag
  'drag:dnd-finished
  'dnd-finished
  'dnd-finished
  '((name . "dnd-finished")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkDrag
  'drag:drop-performed
  'drop-performed
  'drop-performed
  '((name . "drop-performed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDragAction
  'drag-action:is-unique?
  'is-unique?
  'gdk_drag_action_is_unique
  '((name . "is_unique")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "action")
            (meta (arg-type . flags)
                  (gtype-name . "GdkDragAction")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkDragSurface>
       "GdkDragSurface"
       "gdk_drag_surface_get_type")

(^function
  'Gdk
  'GdkDragSurface
  'drag-surface:present?
  'present?
  'gdk_drag_surface_present
  '((name . "present")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "height")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'cairo
       '<CairoRegion>
       "CairoRegion"
       "cairo_gobject_region_get_type")

(^function
  'Gdk
  'GdkDrawContext
  'draw-context:begin-frame
  'begin-frame
  'gdk_draw_context_begin_frame
  '((name . "begin_frame")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "region")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoRegion")
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
  'Gdk
  'GdkDrawContext
  'draw-context:end-frame
  'end-frame
  'gdk_draw_context_end_frame
  '((name . "end_frame")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrawContext
  'draw-context:get-display
  'get-display
  'gdk_draw_context_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrawContext
  'draw-context:get-frame-region
  'get-frame-region
  'gdk_draw_context_get_frame_region
  '((name . "get_frame_region")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoRegion")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrawContext
  'draw-context:get-surface
  'get-surface
  'gdk_draw_context_get_surface
  '((name . "get_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrawContext
  'draw-context:is-in-frame?
  'is-in-frame?
  'gdk_draw_context_is_in_frame
  '((name . "is_in_frame")
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

(^property
  'GdkDrawContext
  'draw-context:display
  'display
  'display)

(^property
  'GdkDrawContext
  'draw-context:surface
  'surface
  'surface)

(^function
  'Gdk
  'GdkDrop
  'drop:finish
  'finish
  'gdk_drop_finish
  '((name . "finish")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "action")
            (meta (arg-type . flags)
                  (gtype-name . "GdkDragAction")
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

(^function
  'Gdk
  'GdkDrop
  'drop:get-actions
  'get-actions
  'gdk_drop_get_actions
  '((name . "get_actions")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkDragAction")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:get-device
  'get-device
  'gdk_drop_get_device
  '((name . "get_device")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDevice")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:get-display
  'get-display
  'gdk_drop_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:get-drag
  'get-drag
  'gdk_drop_get_drag
  '((name . "get_drag")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDrag")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:get-formats
  'get-formats
  'gdk_drop_get_formats
  '((name . "get_formats")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:get-surface
  'get-surface
  'gdk_drop_get_surface
  '((name . "get_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:read-async
  'read-async
  'gdk_drop_read_async
  '((name . "read_async")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 3)
    (c-input-len . 5)
    (pdata ((name . "mime_types")
            (meta (arg-type . array)
                  (flags ptr in zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "io_priority")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
  'Gdk
  'GdkDrop
  'drop:read-finish
  'read-finish
  'gdk_drop_read_finish
  '((name . "read_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "out_mime_type")
            (meta (arg-type . utf8-string)
                  (flags ptr out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GInputStream")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkDrop
  'drop:read-value-async
  'read-value-async
  'gdk_drop_read_value_async
  '((name . "read_value_async")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 3)
    (c-input-len . 5)
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
           ((name . "io_priority")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
  'Gdk
  'GdkDrop
  'drop:read-value-finish
  'read-value-finish
  'gdk_drop_read_value_finish
  '((name . "read_value_finish")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
  'Gdk
  'GdkDrop
  'drop:status
  'status
  'gdk_drop_status
  '((name . "status")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "actions")
            (meta (arg-type . flags)
                  (gtype-name . "GdkDragAction")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "preferred")
            (meta (arg-type . flags)
                  (gtype-name . "GdkDragAction")
                  (flags in)
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

(^property
  'GdkDrop
  'drop:actions
  'actions
  'actions)

(^property 'GdkDrop 'drop:device 'device 'device)

(^property
  'GdkDrop
  'drop:display
  'display
  'display)

(^property 'GdkDrop 'drop:drag 'drag 'drag)

(^property
  'GdkDrop
  'drop:formats
  'formats
  'formats)

(^property
  'GdkDrop
  'drop:surface
  'surface
  'surface)

(^constant 'EVENT_PROPAGATE #f)

(^constant 'EVENT_STOP #t)

(^function
  'Gdk
  'GdkEvent
  'event:-get-angle
  '-get-angle
  'gdk_events_get_angle
  '((name . "_get_angle")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "event2")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "angle")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkEvent
  'event:-get-center
  '-get-center
  'gdk_events_get_center
  '((name . "_get_center")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "event2")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "x")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "y")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 1)
            (s-output-pos . 1)))
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
  'Gdk
  'GdkEvent
  'event:-get-distance
  '-get-distance
  'gdk_events_get_distance
  '((name . "_get_distance")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "event2")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "distance")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkEvent
  'event:get-axes
  'get-axes
  'gdk_event_get_axes
  '((name . "get_axes")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 2)
    (pdata ((name . "axes")
            (meta (arg-type . array)
                  (flags ptr out sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . double)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_axes")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)))
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
  'Gdk
  'GdkEvent
  'event:get-axis
  'get-axis
  'gdk_event_get_axis
  '((name . "get_axis")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "axis_use")
            (meta (arg-type . enum)
                  (gtype-name . "GdkAxisUse")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "value")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkEvent
  'event:get-device
  'get-device
  'gdk_event_get_device
  '((name . "get_device")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDevice")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:get-device-tool
  'get-device-tool
  'gdk_event_get_device_tool
  '((name . "get_device_tool")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDeviceTool")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:get-display
  'get-display
  'gdk_event_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkEventSequence>
       "GdkEventSequence"
       "gdk_event_sequence_get_type")

(^function
  'Gdk
  'GdkEvent
  'event:get-event-sequence
  'get-event-sequence
  'gdk_event_get_event_sequence
  '((name . "get_event_sequence")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkEventSequence")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkEventType>
       "GdkEventType"
       "gdk_event_type_get_type")

(^enum-conv 'event-type 'GdkEventType)

(^function
  'Gdk
  'GdkEvent
  'event:get-event-type
  'get-event-type
  'gdk_event_get_event_type
  '((name . "get_event_type")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkEventType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:get-modifier-state
  'get-modifier-state
  'gdk_event_get_modifier_state
  '((name . "get_modifier_state")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkModifierType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:get-pointer-emulated?
  'get-pointer-emulated?
  'gdk_event_get_pointer_emulated
  '((name . "get_pointer_emulated")
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
  'Gdk
  'GdkEvent
  'event:get-position
  'get-position
  'gdk_event_get_position
  '((name . "get_position")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "x")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "y")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
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
  'Gdk
  'GdkEvent
  'event:get-seat
  'get-seat
  'gdk_event_get_seat
  '((name . "get_seat")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSeat")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:get-surface
  'get-surface
  'gdk_event_get_surface
  '((name . "get_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:get-time
  'get-time
  'gdk_event_get_time
  '((name . "get_time")
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
  'Gdk
  'GdkEvent
  'event:ref
  'ref
  'gdk_event_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkEvent")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkEvent
  'event:triggers-context-menu?
  'triggers-context-menu?
  'gdk_event_triggers_context_menu
  '((name . "triggers_context_menu")
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
  'Gdk
  'GdkEvent
  'event:unref
  'unref
  'gdk_event_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkFileList>
       "GdkFileList"
       "gdk_file_list_get_type")

(^type 'Gio '<GFile> "GFile" "g_file_get_type")

(^function
  'Gdk
  'GdkFileList
  'file-list:new-from-array
  'new-from-array
  'gdk_file_list_new_from_array
  '((name . "new_from_array")
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "files")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . GInterface)
                     (gtype-name . "GFile")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_files")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkFileList")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFileList
  'file-list:new-from-list
  'new-from-list
  'gdk_file_list_new_from_list
  '((name . "new_from_list")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "files")
            (meta (arg-type . GSList)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . GInterface)
                     (gtype-name . "GFile")
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkFileList")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFileList
  'file-list:get-files
  'get-files
  'gdk_file_list_get_files
  '((name . "get_files")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . container)
            (params
              ((arg-type . GInterface)
               (gtype-name . "GFile")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkFocusEvent>
       "GdkFocusEvent"
       "gdk_focus_event_get_type")

(^function
  'Gdk
  'GdkFocusEvent
  'focus-event:get-in?
  'get-in?
  'gdk_focus_event_get_in
  '((name . "get_in")
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

(^type 'Gdk
       '<GdkFrameClock>
       "GdkFrameClock"
       "gdk_frame_clock_get_type")

(^function
  'Gdk
  'GdkFrameClock
  'frame-clock:begin-updating
  'begin-updating
  'gdk_frame_clock_begin_updating
  '((name . "begin_updating")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFrameClock
  'frame-clock:end-updating
  'end-updating
  'gdk_frame_clock_end_updating
  '((name . "end_updating")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkFrameTimings>
       "GdkFrameTimings"
       "gdk_frame_timings_get_type")

(^function
  'Gdk
  'GdkFrameClock
  'frame-clock:get-current-timings
  'get-current-timings
  'gdk_frame_clock_get_current_timings
  '((name . "get_current_timings")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkFrameTimings")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFrameClock
  'frame-clock:get-fps
  'get-fps
  'gdk_frame_clock_get_fps
  '((name . "get_fps")
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
  'Gdk
  'GdkFrameClock
  'frame-clock:get-frame-counter
  'get-frame-counter
  'gdk_frame_clock_get_frame_counter
  '((name . "get_frame_counter")
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
  'Gdk
  'GdkFrameClock
  'frame-clock:get-frame-time
  'get-frame-time
  'gdk_frame_clock_get_frame_time
  '((name . "get_frame_time")
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
  'Gdk
  'GdkFrameClock
  'frame-clock:get-history-start
  'get-history-start
  'gdk_frame_clock_get_history_start
  '((name . "get_history_start")
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
  'Gdk
  'GdkFrameClock
  'frame-clock:get-refresh-info
  'get-refresh-info
  'gdk_frame_clock_get_refresh_info
  '((name . "get_refresh_info")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "base_time")
            (meta (arg-type . int64)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "refresh_interval_return")
            (meta (arg-type . int64)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "presentation_time_return")
            (meta (arg-type . int64)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFrameClock
  'frame-clock:get-timings
  'get-timings
  'gdk_frame_clock_get_timings
  '((name . "get_timings")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "frame_counter")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkFrameTimings")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkFrameClockPhase>
       "GdkFrameClockPhase"
       "gdk_frame_clock_phase_get_type")

(^flags-conv
  'frame-clock-phase
  'GdkFrameClockPhase)

(^function
  'Gdk
  'GdkFrameClock
  'frame-clock:request-phase
  'request-phase
  'gdk_frame_clock_request_phase
  '((name . "request_phase")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "phase")
            (meta (arg-type . flags)
                  (gtype-name . "GdkFrameClockPhase")
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
  'Gdk
  'GdkFrameClock
  'frame-clock:after-paint
  'after-paint
  'after-paint
  '((name . "after-paint")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkFrameClock
  'frame-clock:before-paint
  'before-paint
  'before-paint
  '((name . "before-paint")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkFrameClock
  'frame-clock:flush-events
  'flush-events
  'flush-events
  '((name . "flush-events")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkFrameClock
  'frame-clock:layout
  'layout
  'layout
  '((name . "layout")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkFrameClock
  'frame-clock:paint
  'paint
  'paint
  '((name . "paint")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkFrameClock
  'frame-clock:resume-events
  'resume-events
  'resume-events
  '((name . "resume-events")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkFrameClock
  'frame-clock:update
  'update
  'update
  '((name . "update")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFrameTimings
  'frame-timings:get-complete?
  'get-complete?
  'gdk_frame_timings_get_complete
  '((name . "get_complete")
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
  'Gdk
  'GdkFrameTimings
  'frame-timings:get-frame-counter
  'get-frame-counter
  'gdk_frame_timings_get_frame_counter
  '((name . "get_frame_counter")
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
  'Gdk
  'GdkFrameTimings
  'frame-timings:get-frame-time
  'get-frame-time
  'gdk_frame_timings_get_frame_time
  '((name . "get_frame_time")
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
  'Gdk
  'GdkFrameTimings
  'frame-timings:get-predicted-presentation-time
  'get-predicted-presentation-time
  'gdk_frame_timings_get_predicted_presentation_time
  '((name . "get_predicted_presentation_time")
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
  'Gdk
  'GdkFrameTimings
  'frame-timings:get-presentation-time
  'get-presentation-time
  'gdk_frame_timings_get_presentation_time
  '((name . "get_presentation_time")
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
  'Gdk
  'GdkFrameTimings
  'frame-timings:get-refresh-interval
  'get-refresh-interval
  'gdk_frame_timings_get_refresh_interval
  '((name . "get_refresh_interval")
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
  'Gdk
  'GdkFrameTimings
  'frame-timings:ref
  'ref
  'gdk_frame_timings_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkFrameTimings")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkFrameTimings
  'frame-timings:unref
  'unref
  'gdk_frame_timings_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkFullscreenMode>
       "GdkFullscreenMode"
       "gdk_fullscreen_mode_get_type")

(^enum-conv 'fullscreen-mode 'GdkFullscreenMode)

(^type 'Gdk
       '<GdkGLAPI>
       "GdkGLAPI"
       "gdk_gl_api_get_type")

(^flags-conv 'glapi 'GdkGLAPI)

(^function
  'Gdk
  'GdkGLContext
  'glcontext:clear-current
  'clear-current
  'gdk_gl_context_clear_current
  '((name . "clear_current")
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-current
  'get-current
  'gdk_gl_context_get_current
  '((name . "get_current")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkGLContext")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-allowed-apis
  'get-allowed-apis
  'gdk_gl_context_get_allowed_apis
  '((name . "get_allowed_apis")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkGLAPI")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-api
  'get-api
  'gdk_gl_context_get_api
  '((name . "get_api")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkGLAPI")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-debug-enabled?
  'get-debug-enabled?
  'gdk_gl_context_get_debug_enabled
  '((name . "get_debug_enabled")
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
  'Gdk
  'GdkGLContext
  'glcontext:get-display
  'get-display
  'gdk_gl_context_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-forward-compatible?
  'get-forward-compatible?
  'gdk_gl_context_get_forward_compatible
  '((name . "get_forward_compatible")
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
  'Gdk
  'GdkGLContext
  'glcontext:get-required-version
  'get-required-version
  'gdk_gl_context_get_required_version
  '((name . "get_required_version")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "major")
            (meta (arg-type . int32)
                  (flags out nullable)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "minor")
            (meta (arg-type . int32)
                  (flags out nullable)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-shared-context
  'get-shared-context
  'gdk_gl_context_get_shared_context
  '((name . "get_shared_context")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkGLContext")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-surface
  'get-surface
  'gdk_gl_context_get_surface
  '((name . "get_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:get-use-es?
  'get-use-es?
  'gdk_gl_context_get_use_es
  '((name . "get_use_es")
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
  'Gdk
  'GdkGLContext
  'glcontext:get-version
  'get-version
  'gdk_gl_context_get_version
  '((name . "get_version")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "major")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "minor")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:is-legacy?
  'is-legacy?
  'gdk_gl_context_is_legacy
  '((name . "is_legacy")
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
  'Gdk
  'GdkGLContext
  'glcontext:is-shared?
  'is-shared?
  'gdk_gl_context_is_shared
  '((name . "is_shared")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "other")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkGLContext")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:make-current
  'make-current
  'gdk_gl_context_make_current
  '((name . "make_current")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:realize?
  'realize?
  'gdk_gl_context_realize
  '((name . "realize")
    (is-method . #t)
    (can-throw-gerror . #t)
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
  'Gdk
  'GdkGLContext
  'glcontext:set-allowed-apis
  'set-allowed-apis
  'gdk_gl_context_set_allowed_apis
  '((name . "set_allowed_apis")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "apis")
            (meta (arg-type . flags)
                  (gtype-name . "GdkGLAPI")
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

(^function
  'Gdk
  'GdkGLContext
  'glcontext:set-debug-enabled
  'set-debug-enabled
  'gdk_gl_context_set_debug_enabled
  '((name . "set_debug_enabled")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "enabled")
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
  'Gdk
  'GdkGLContext
  'glcontext:set-forward-compatible
  'set-forward-compatible
  'gdk_gl_context_set_forward_compatible
  '((name . "set_forward_compatible")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "compatible")
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
  'Gdk
  'GdkGLContext
  'glcontext:set-required-version
  'set-required-version
  'gdk_gl_context_set_required_version
  '((name . "set_required_version")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "major")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "minor")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLContext
  'glcontext:set-use-es
  'set-use-es
  'gdk_gl_context_set_use_es
  '((name . "set_use_es")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "use_es")
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

(^property
  'GdkGLContext
  'glcontext:allowed-apis
  'allowed-apis
  'allowed-apis)

(^property
  'GdkGLContext
  'glcontext:api
  'api
  'api)

(^property
  'GdkGLContext
  'glcontext:shared-context
  'shared-context
  'shared-context)

(^type 'Gdk
       '<GdkGLError>
       "GdkGLError"
       "gdk_gl_error_get_type")

(^enum-conv 'glerror 'GdkGLError)

(^function
  'Gdk
  'GdkGLError
  'glerror:quark
  'quark
  'gdk_gl_error_quark
  '((name . "quark")
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

(^type 'Gdk
       '<GdkGLTexture>
       "GdkGLTexture"
       "gdk_gl_texture_get_type")

(^function
  'Gdk
  'GdkGLTexture
  'gltexture:new
  'new
  'gdk_gl_texture_new
  '((name . "new")
    (s-input-req . 5)
    (s-input-opt . 1)
    (c-input-len . 6)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkGLContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "id")
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
           ((name . "width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "height")
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
           ((name . "destroy")
            (meta (arg-type . callback)
                  (flags in)
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
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "data")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkGLTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGLTexture
  'gltexture:release
  'release
  'gdk_gl_texture_release
  '((name . "release")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkGrabBrokenEvent>
       "GdkGrabBrokenEvent"
       "gdk_grab_broken_event_get_type")

(^function
  'Gdk
  'GdkGrabBrokenEvent
  'grab-broken-event:get-grab-surface
  'get-grab-surface
  'gdk_grab_broken_event_get_grab_surface
  '((name . "get_grab_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkGrabBrokenEvent
  'grab-broken-event:get-implicit?
  'get-implicit?
  'gdk_grab_broken_event_get_implicit
  '((name . "get_implicit")
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

(^type 'Gdk
       '<GdkGravity>
       "GdkGravity"
       "gdk_gravity_get_type")

(^enum-conv 'gravity 'GdkGravity)

(^constant 'KEY_0 48)

(^constant 'KEY_1 49)

(^constant 'KEY_2 50)

(^constant 'KEY_3 51)

(^constant 'KEY_3270_AltCursor 64784)

(^constant 'KEY_3270_Attn 64782)

(^constant 'KEY_3270_BackTab 64773)

(^constant 'KEY_3270_ChangeScreen 64793)

(^constant 'KEY_3270_Copy 64789)

(^constant 'KEY_3270_CursorBlink 64783)

(^constant 'KEY_3270_CursorSelect 64796)

(^constant 'KEY_3270_DeleteWord 64794)

(^constant 'KEY_3270_Duplicate 64769)

(^constant 'KEY_3270_Enter 64798)

(^constant 'KEY_3270_EraseEOF 64774)

(^constant 'KEY_3270_EraseInput 64775)

(^constant 'KEY_3270_ExSelect 64795)

(^constant 'KEY_3270_FieldMark 64770)

(^constant 'KEY_3270_Ident 64787)

(^constant 'KEY_3270_Jump 64786)

(^constant 'KEY_3270_KeyClick 64785)

(^constant 'KEY_3270_Left2 64772)

(^constant 'KEY_3270_PA1 64778)

(^constant 'KEY_3270_PA2 64779)

(^constant 'KEY_3270_PA3 64780)

(^constant 'KEY_3270_Play 64790)

(^constant 'KEY_3270_PrintScreen 64797)

(^constant 'KEY_3270_Quit 64777)

(^constant 'KEY_3270_Record 64792)

(^constant 'KEY_3270_Reset 64776)

(^constant 'KEY_3270_Right2 64771)

(^constant 'KEY_3270_Rule 64788)

(^constant 'KEY_3270_Setup 64791)

(^constant 'KEY_3270_Test 64781)

(^constant 'KEY_4 52)

(^constant 'KEY_5 53)

(^constant 'KEY_6 54)

(^constant 'KEY_7 55)

(^constant 'KEY_8 56)

(^constant 'KEY_9 57)

(^constant 'KEY_A 65)

(^constant 'KEY_AE 198)

(^constant 'KEY_Aacute 193)

(^constant 'KEY_Abelowdot 16785056)

(^constant 'KEY_Abreve 451)

(^constant 'KEY_Abreveacute 16785070)

(^constant 'KEY_Abrevebelowdot 16785078)

(^constant 'KEY_Abrevegrave 16785072)

(^constant 'KEY_Abrevehook 16785074)

(^constant 'KEY_Abrevetilde 16785076)

(^constant 'KEY_AccessX_Enable 65136)

(^constant 'KEY_AccessX_Feedback_Enable 65137)

(^constant 'KEY_Acircumflex 194)

(^constant 'KEY_Acircumflexacute 16785060)

(^constant 'KEY_Acircumflexbelowdot 16785068)

(^constant 'KEY_Acircumflexgrave 16785062)

(^constant 'KEY_Acircumflexhook 16785064)

(^constant 'KEY_Acircumflextilde 16785066)

(^constant 'KEY_AddFavorite 269025081)

(^constant 'KEY_Adiaeresis 196)

(^constant 'KEY_Agrave 192)

(^constant 'KEY_Ahook 16785058)

(^constant 'KEY_Alt_L 65513)

(^constant 'KEY_Alt_R 65514)

(^constant 'KEY_Amacron 960)

(^constant 'KEY_Aogonek 417)

(^constant 'KEY_ApplicationLeft 269025104)

(^constant 'KEY_ApplicationRight 269025105)

(^constant 'KEY_Arabic_0 16778848)

(^constant 'KEY_Arabic_1 16778849)

(^constant 'KEY_Arabic_2 16778850)

(^constant 'KEY_Arabic_3 16778851)

(^constant 'KEY_Arabic_4 16778852)

(^constant 'KEY_Arabic_5 16778853)

(^constant 'KEY_Arabic_6 16778854)

(^constant 'KEY_Arabic_7 16778855)

(^constant 'KEY_Arabic_8 16778856)

(^constant 'KEY_Arabic_9 16778857)

(^constant 'KEY_Arabic_ain 1497)

(^constant 'KEY_Arabic_alef 1479)

(^constant 'KEY_Arabic_alefmaksura 1513)

(^constant 'KEY_Arabic_beh 1480)

(^constant 'KEY_Arabic_comma 1452)

(^constant 'KEY_Arabic_dad 1494)

(^constant 'KEY_Arabic_dal 1487)

(^constant 'KEY_Arabic_damma 1519)

(^constant 'KEY_Arabic_dammatan 1516)

(^constant 'KEY_Arabic_ddal 16778888)

(^constant 'KEY_Arabic_farsi_yeh 16778956)

(^constant 'KEY_Arabic_fatha 1518)

(^constant 'KEY_Arabic_fathatan 1515)

(^constant 'KEY_Arabic_feh 1505)

(^constant 'KEY_Arabic_fullstop 16778964)

(^constant 'KEY_Arabic_gaf 16778927)

(^constant 'KEY_Arabic_ghain 1498)

(^constant 'KEY_Arabic_ha 1511)

(^constant 'KEY_Arabic_hah 1485)

(^constant 'KEY_Arabic_hamza 1473)

(^constant 'KEY_Arabic_hamza_above 16778836)

(^constant 'KEY_Arabic_hamza_below 16778837)

(^constant 'KEY_Arabic_hamzaonalef 1475)

(^constant 'KEY_Arabic_hamzaonwaw 1476)

(^constant 'KEY_Arabic_hamzaonyeh 1478)

(^constant 'KEY_Arabic_hamzaunderalef 1477)

(^constant 'KEY_Arabic_heh 1511)

(^constant 'KEY_Arabic_heh_doachashmee 16778942)

(^constant 'KEY_Arabic_heh_goal 16778945)

(^constant 'KEY_Arabic_jeem 1484)

(^constant 'KEY_Arabic_jeh 16778904)

(^constant 'KEY_Arabic_kaf 1507)

(^constant 'KEY_Arabic_kasra 1520)

(^constant 'KEY_Arabic_kasratan 1517)

(^constant 'KEY_Arabic_keheh 16778921)

(^constant 'KEY_Arabic_khah 1486)

(^constant 'KEY_Arabic_lam 1508)

(^constant 'KEY_Arabic_madda_above 16778835)

(^constant 'KEY_Arabic_maddaonalef 1474)

(^constant 'KEY_Arabic_meem 1509)

(^constant 'KEY_Arabic_noon 1510)

(^constant 'KEY_Arabic_noon_ghunna 16778938)

(^constant 'KEY_Arabic_peh 16778878)

(^constant 'KEY_Arabic_percent 16778858)

(^constant 'KEY_Arabic_qaf 1506)

(^constant 'KEY_Arabic_question_mark 1471)

(^constant 'KEY_Arabic_ra 1489)

(^constant 'KEY_Arabic_rreh 16778897)

(^constant 'KEY_Arabic_sad 1493)

(^constant 'KEY_Arabic_seen 1491)

(^constant 'KEY_Arabic_semicolon 1467)

(^constant 'KEY_Arabic_shadda 1521)

(^constant 'KEY_Arabic_sheen 1492)

(^constant 'KEY_Arabic_sukun 1522)

(^constant 'KEY_Arabic_superscript_alef 16778864)

(^constant 'KEY_Arabic_switch 65406)

(^constant 'KEY_Arabic_tah 1495)

(^constant 'KEY_Arabic_tatweel 1504)

(^constant 'KEY_Arabic_tcheh 16778886)

(^constant 'KEY_Arabic_teh 1482)

(^constant 'KEY_Arabic_tehmarbuta 1481)

(^constant 'KEY_Arabic_thal 1488)

(^constant 'KEY_Arabic_theh 1483)

(^constant 'KEY_Arabic_tteh 16778873)

(^constant 'KEY_Arabic_veh 16778916)

(^constant 'KEY_Arabic_waw 1512)

(^constant 'KEY_Arabic_yeh 1514)

(^constant 'KEY_Arabic_yeh_baree 16778962)

(^constant 'KEY_Arabic_zah 1496)

(^constant 'KEY_Arabic_zain 1490)

(^constant 'KEY_Aring 197)

(^constant 'KEY_Armenian_AT 16778552)

(^constant 'KEY_Armenian_AYB 16778545)

(^constant 'KEY_Armenian_BEN 16778546)

(^constant 'KEY_Armenian_CHA 16778569)

(^constant 'KEY_Armenian_DA 16778548)

(^constant 'KEY_Armenian_DZA 16778561)

(^constant 'KEY_Armenian_E 16778551)

(^constant 'KEY_Armenian_FE 16778582)

(^constant 'KEY_Armenian_GHAT 16778562)

(^constant 'KEY_Armenian_GIM 16778547)

(^constant 'KEY_Armenian_HI 16778565)

(^constant 'KEY_Armenian_HO 16778560)

(^constant 'KEY_Armenian_INI 16778555)

(^constant 'KEY_Armenian_JE 16778571)

(^constant 'KEY_Armenian_KE 16778580)

(^constant 'KEY_Armenian_KEN 16778559)

(^constant 'KEY_Armenian_KHE 16778557)

(^constant 'KEY_Armenian_LYUN 16778556)

(^constant 'KEY_Armenian_MEN 16778564)

(^constant 'KEY_Armenian_NU 16778566)

(^constant 'KEY_Armenian_O 16778581)

(^constant 'KEY_Armenian_PE 16778570)

(^constant 'KEY_Armenian_PYUR 16778579)

(^constant 'KEY_Armenian_RA 16778572)

(^constant 'KEY_Armenian_RE 16778576)

(^constant 'KEY_Armenian_SE 16778573)

(^constant 'KEY_Armenian_SHA 16778567)

(^constant 'KEY_Armenian_TCHE 16778563)

(^constant 'KEY_Armenian_TO 16778553)

(^constant 'KEY_Armenian_TSA 16778558)

(^constant 'KEY_Armenian_TSO 16778577)

(^constant 'KEY_Armenian_TYUN 16778575)

(^constant 'KEY_Armenian_VEV 16778574)

(^constant 'KEY_Armenian_VO 16778568)

(^constant 'KEY_Armenian_VYUN 16778578)

(^constant 'KEY_Armenian_YECH 16778549)

(^constant 'KEY_Armenian_ZA 16778550)

(^constant 'KEY_Armenian_ZHE 16778554)

(^constant 'KEY_Armenian_accent 16778587)

(^constant 'KEY_Armenian_amanak 16778588)

(^constant 'KEY_Armenian_apostrophe 16778586)

(^constant 'KEY_Armenian_at 16778600)

(^constant 'KEY_Armenian_ayb 16778593)

(^constant 'KEY_Armenian_ben 16778594)

(^constant 'KEY_Armenian_but 16778589)

(^constant 'KEY_Armenian_cha 16778617)

(^constant 'KEY_Armenian_da 16778596)

(^constant 'KEY_Armenian_dza 16778609)

(^constant 'KEY_Armenian_e 16778599)

(^constant 'KEY_Armenian_exclam 16778588)

(^constant 'KEY_Armenian_fe 16778630)

(^constant 'KEY_Armenian_full_stop 16778633)

(^constant 'KEY_Armenian_ghat 16778610)

(^constant 'KEY_Armenian_gim 16778595)

(^constant 'KEY_Armenian_hi 16778613)

(^constant 'KEY_Armenian_ho 16778608)

(^constant 'KEY_Armenian_hyphen 16778634)

(^constant 'KEY_Armenian_ini 16778603)

(^constant 'KEY_Armenian_je 16778619)

(^constant 'KEY_Armenian_ke 16778628)

(^constant 'KEY_Armenian_ken 16778607)

(^constant 'KEY_Armenian_khe 16778605)

(^constant 'KEY_Armenian_ligature_ew 16778631)

(^constant 'KEY_Armenian_lyun 16778604)

(^constant 'KEY_Armenian_men 16778612)

(^constant 'KEY_Armenian_nu 16778614)

(^constant 'KEY_Armenian_o 16778629)

(^constant 'KEY_Armenian_paruyk 16778590)

(^constant 'KEY_Armenian_pe 16778618)

(^constant 'KEY_Armenian_pyur 16778627)

(^constant 'KEY_Armenian_question 16778590)

(^constant 'KEY_Armenian_ra 16778620)

(^constant 'KEY_Armenian_re 16778624)

(^constant 'KEY_Armenian_se 16778621)

(^constant
  'KEY_Armenian_separation_mark
  16778589)

(^constant 'KEY_Armenian_sha 16778615)

(^constant 'KEY_Armenian_shesht 16778587)

(^constant 'KEY_Armenian_tche 16778611)

(^constant 'KEY_Armenian_to 16778601)

(^constant 'KEY_Armenian_tsa 16778606)

(^constant 'KEY_Armenian_tso 16778625)

(^constant 'KEY_Armenian_tyun 16778623)

(^constant 'KEY_Armenian_verjaket 16778633)

(^constant 'KEY_Armenian_vev 16778622)

(^constant 'KEY_Armenian_vo 16778616)

(^constant 'KEY_Armenian_vyun 16778626)

(^constant 'KEY_Armenian_yech 16778597)

(^constant 'KEY_Armenian_yentamna 16778634)

(^constant 'KEY_Armenian_za 16778598)

(^constant 'KEY_Armenian_zhe 16778602)

(^constant 'KEY_Atilde 195)

(^constant 'KEY_AudibleBell_Enable 65146)

(^constant 'KEY_AudioCycleTrack 269025179)

(^constant 'KEY_AudioForward 269025175)

(^constant 'KEY_AudioLowerVolume 269025041)

(^constant 'KEY_AudioMedia 269025074)

(^constant 'KEY_AudioMicMute 269025202)

(^constant 'KEY_AudioMute 269025042)

(^constant 'KEY_AudioNext 269025047)

(^constant 'KEY_AudioPause 269025073)

(^constant 'KEY_AudioPlay 269025044)

(^constant 'KEY_AudioPreset 269025206)

(^constant 'KEY_AudioPrev 269025046)

(^constant 'KEY_AudioRaiseVolume 269025043)

(^constant 'KEY_AudioRandomPlay 269025177)

(^constant 'KEY_AudioRecord 269025052)

(^constant 'KEY_AudioRepeat 269025176)

(^constant 'KEY_AudioRewind 269025086)

(^constant 'KEY_AudioStop 269025045)

(^constant 'KEY_Away 269025165)

(^constant 'KEY_B 66)

(^constant 'KEY_Babovedot 16784898)

(^constant 'KEY_Back 269025062)

(^constant 'KEY_BackForward 269025087)

(^constant 'KEY_BackSpace 65288)

(^constant 'KEY_Battery 269025171)

(^constant 'KEY_Begin 65368)

(^constant 'KEY_Blue 269025190)

(^constant 'KEY_Bluetooth 269025172)

(^constant 'KEY_Book 269025106)

(^constant 'KEY_BounceKeys_Enable 65140)

(^constant 'KEY_Break 65387)

(^constant 'KEY_BrightnessAdjust 269025083)

(^constant 'KEY_Byelorussian_SHORTU 1726)

(^constant 'KEY_Byelorussian_shortu 1710)

(^constant 'KEY_C 67)

(^constant 'KEY_CD 269025107)

(^constant 'KEY_CH 65186)

(^constant 'KEY_C_H 65189)

(^constant 'KEY_C_h 65188)

(^constant 'KEY_Cabovedot 709)

(^constant 'KEY_Cacute 454)

(^constant 'KEY_Calculator 269025053)

(^constant 'KEY_Calendar 269025056)

(^constant 'KEY_Cancel 65385)

(^constant 'KEY_Caps_Lock 65509)

(^constant 'KEY_Ccaron 456)

(^constant 'KEY_Ccedilla 199)

(^constant 'KEY_Ccircumflex 710)

(^constant 'KEY_Ch 65185)

(^constant 'KEY_Clear 65291)

(^constant 'KEY_ClearGrab 269024801)

(^constant 'KEY_Close 269025110)

(^constant 'KEY_Codeinput 65335)

(^constant 'KEY_ColonSign 16785569)

(^constant 'KEY_Community 269025085)

(^constant 'KEY_ContrastAdjust 269025058)

(^constant 'KEY_Control_L 65507)

(^constant 'KEY_Control_R 65508)

(^constant 'KEY_Copy 269025111)

(^constant 'KEY_CruzeiroSign 16785570)

(^constant 'KEY_Cut 269025112)

(^constant 'KEY_CycleAngle 269025180)

(^constant 'KEY_Cyrillic_A 1761)

(^constant 'KEY_Cyrillic_BE 1762)

(^constant 'KEY_Cyrillic_CHE 1790)

(^constant 'KEY_Cyrillic_CHE_descender 16778422)

(^constant 'KEY_Cyrillic_CHE_vertstroke 16778424)

(^constant 'KEY_Cyrillic_DE 1764)

(^constant 'KEY_Cyrillic_DZHE 1727)

(^constant 'KEY_Cyrillic_E 1788)

(^constant 'KEY_Cyrillic_EF 1766)

(^constant 'KEY_Cyrillic_EL 1772)

(^constant 'KEY_Cyrillic_EM 1773)

(^constant 'KEY_Cyrillic_EN 1774)

(^constant 'KEY_Cyrillic_EN_descender 16778402)

(^constant 'KEY_Cyrillic_ER 1778)

(^constant 'KEY_Cyrillic_ES 1779)

(^constant 'KEY_Cyrillic_GHE 1767)

(^constant 'KEY_Cyrillic_GHE_bar 16778386)

(^constant 'KEY_Cyrillic_HA 1768)

(^constant 'KEY_Cyrillic_HARDSIGN 1791)

(^constant 'KEY_Cyrillic_HA_descender 16778418)

(^constant 'KEY_Cyrillic_I 1769)

(^constant 'KEY_Cyrillic_IE 1765)

(^constant 'KEY_Cyrillic_IO 1715)

(^constant 'KEY_Cyrillic_I_macron 16778466)

(^constant 'KEY_Cyrillic_JE 1720)

(^constant 'KEY_Cyrillic_KA 1771)

(^constant 'KEY_Cyrillic_KA_descender 16778394)

(^constant 'KEY_Cyrillic_KA_vertstroke 16778396)

(^constant 'KEY_Cyrillic_LJE 1721)

(^constant 'KEY_Cyrillic_NJE 1722)

(^constant 'KEY_Cyrillic_O 1775)

(^constant 'KEY_Cyrillic_O_bar 16778472)

(^constant 'KEY_Cyrillic_PE 1776)

(^constant 'KEY_Cyrillic_SCHWA 16778456)

(^constant 'KEY_Cyrillic_SHA 1787)

(^constant 'KEY_Cyrillic_SHCHA 1789)

(^constant 'KEY_Cyrillic_SHHA 16778426)

(^constant 'KEY_Cyrillic_SHORTI 1770)

(^constant 'KEY_Cyrillic_SOFTSIGN 1784)

(^constant 'KEY_Cyrillic_TE 1780)

(^constant 'KEY_Cyrillic_TSE 1763)

(^constant 'KEY_Cyrillic_U 1781)

(^constant 'KEY_Cyrillic_U_macron 16778478)

(^constant 'KEY_Cyrillic_U_straight 16778414)

(^constant 'KEY_Cyrillic_U_straight_bar 16778416)

(^constant 'KEY_Cyrillic_VE 1783)

(^constant 'KEY_Cyrillic_YA 1777)

(^constant 'KEY_Cyrillic_YERU 1785)

(^constant 'KEY_Cyrillic_YU 1760)

(^constant 'KEY_Cyrillic_ZE 1786)

(^constant 'KEY_Cyrillic_ZHE 1782)

(^constant 'KEY_Cyrillic_ZHE_descender 16778390)

(^constant 'KEY_Cyrillic_a 1729)

(^constant 'KEY_Cyrillic_be 1730)

(^constant 'KEY_Cyrillic_che 1758)

(^constant 'KEY_Cyrillic_che_descender 16778423)

(^constant 'KEY_Cyrillic_che_vertstroke 16778425)

(^constant 'KEY_Cyrillic_de 1732)

(^constant 'KEY_Cyrillic_dzhe 1711)

(^constant 'KEY_Cyrillic_e 1756)

(^constant 'KEY_Cyrillic_ef 1734)

(^constant 'KEY_Cyrillic_el 1740)

(^constant 'KEY_Cyrillic_em 1741)

(^constant 'KEY_Cyrillic_en 1742)

(^constant 'KEY_Cyrillic_en_descender 16778403)

(^constant 'KEY_Cyrillic_er 1746)

(^constant 'KEY_Cyrillic_es 1747)

(^constant 'KEY_Cyrillic_ghe 1735)

(^constant 'KEY_Cyrillic_ghe_bar 16778387)

(^constant 'KEY_Cyrillic_ha 1736)

(^constant 'KEY_Cyrillic_ha_descender 16778419)

(^constant 'KEY_Cyrillic_hardsign 1759)

(^constant 'KEY_Cyrillic_i 1737)

(^constant 'KEY_Cyrillic_i_macron 16778467)

(^constant 'KEY_Cyrillic_ie 1733)

(^constant 'KEY_Cyrillic_io 1699)

(^constant 'KEY_Cyrillic_je 1704)

(^constant 'KEY_Cyrillic_ka 1739)

(^constant 'KEY_Cyrillic_ka_descender 16778395)

(^constant 'KEY_Cyrillic_ka_vertstroke 16778397)

(^constant 'KEY_Cyrillic_lje 1705)

(^constant 'KEY_Cyrillic_nje 1706)

(^constant 'KEY_Cyrillic_o 1743)

(^constant 'KEY_Cyrillic_o_bar 16778473)

(^constant 'KEY_Cyrillic_pe 1744)

(^constant 'KEY_Cyrillic_schwa 16778457)

(^constant 'KEY_Cyrillic_sha 1755)

(^constant 'KEY_Cyrillic_shcha 1757)

(^constant 'KEY_Cyrillic_shha 16778427)

(^constant 'KEY_Cyrillic_shorti 1738)

(^constant 'KEY_Cyrillic_softsign 1752)

(^constant 'KEY_Cyrillic_te 1748)

(^constant 'KEY_Cyrillic_tse 1731)

(^constant 'KEY_Cyrillic_u 1749)

(^constant 'KEY_Cyrillic_u_macron 16778479)

(^constant 'KEY_Cyrillic_u_straight 16778415)

(^constant 'KEY_Cyrillic_u_straight_bar 16778417)

(^constant 'KEY_Cyrillic_ve 1751)

(^constant 'KEY_Cyrillic_ya 1745)

(^constant 'KEY_Cyrillic_yeru 1753)

(^constant 'KEY_Cyrillic_yu 1728)

(^constant 'KEY_Cyrillic_ze 1754)

(^constant 'KEY_Cyrillic_zhe 1750)

(^constant 'KEY_Cyrillic_zhe_descender 16778391)

(^constant 'KEY_D 68)

(^constant 'KEY_DOS 269025114)

(^constant 'KEY_Dabovedot 16784906)

(^constant 'KEY_Dcaron 463)

(^constant 'KEY_Delete 65535)

(^constant 'KEY_Display 269025113)

(^constant 'KEY_Documents 269025115)

(^constant 'KEY_DongSign 16785579)

(^constant 'KEY_Down 65364)

(^constant 'KEY_Dstroke 464)

(^constant 'KEY_E 69)

(^constant 'KEY_ENG 957)

(^constant 'KEY_ETH 208)

(^constant 'KEY_EZH 16777655)

(^constant 'KEY_Eabovedot 972)

(^constant 'KEY_Eacute 201)

(^constant 'KEY_Ebelowdot 16785080)

(^constant 'KEY_Ecaron 460)

(^constant 'KEY_Ecircumflex 202)

(^constant 'KEY_Ecircumflexacute 16785086)

(^constant 'KEY_Ecircumflexbelowdot 16785094)

(^constant 'KEY_Ecircumflexgrave 16785088)

(^constant 'KEY_Ecircumflexhook 16785090)

(^constant 'KEY_Ecircumflextilde 16785092)

(^constant 'KEY_EcuSign 16785568)

(^constant 'KEY_Ediaeresis 203)

(^constant 'KEY_Egrave 200)

(^constant 'KEY_Ehook 16785082)

(^constant 'KEY_Eisu_Shift 65327)

(^constant 'KEY_Eisu_toggle 65328)

(^constant 'KEY_Eject 269025068)

(^constant 'KEY_Emacron 938)

(^constant 'KEY_End 65367)

(^constant 'KEY_Eogonek 458)

(^constant 'KEY_Escape 65307)

(^constant 'KEY_Eth 208)

(^constant 'KEY_Etilde 16785084)

(^constant 'KEY_EuroSign 8364)

(^constant 'KEY_Excel 269025116)

(^constant 'KEY_Execute 65378)

(^constant 'KEY_Explorer 269025117)

(^constant 'KEY_F 70)

(^constant 'KEY_F1 65470)

(^constant 'KEY_F10 65479)

(^constant 'KEY_F11 65480)

(^constant 'KEY_F12 65481)

(^constant 'KEY_F13 65482)

(^constant 'KEY_F14 65483)

(^constant 'KEY_F15 65484)

(^constant 'KEY_F16 65485)

(^constant 'KEY_F17 65486)

(^constant 'KEY_F18 65487)

(^constant 'KEY_F19 65488)

(^constant 'KEY_F2 65471)

(^constant 'KEY_F20 65489)

(^constant 'KEY_F21 65490)

(^constant 'KEY_F22 65491)

(^constant 'KEY_F23 65492)

(^constant 'KEY_F24 65493)

(^constant 'KEY_F25 65494)

(^constant 'KEY_F26 65495)

(^constant 'KEY_F27 65496)

(^constant 'KEY_F28 65497)

(^constant 'KEY_F29 65498)

(^constant 'KEY_F3 65472)

(^constant 'KEY_F30 65499)

(^constant 'KEY_F31 65500)

(^constant 'KEY_F32 65501)

(^constant 'KEY_F33 65502)

(^constant 'KEY_F34 65503)

(^constant 'KEY_F35 65504)

(^constant 'KEY_F4 65473)

(^constant 'KEY_F5 65474)

(^constant 'KEY_F6 65475)

(^constant 'KEY_F7 65476)

(^constant 'KEY_F8 65477)

(^constant 'KEY_F9 65478)

(^constant 'KEY_FFrancSign 16785571)

(^constant 'KEY_Fabovedot 16784926)

(^constant 'KEY_Farsi_0 16778992)

(^constant 'KEY_Farsi_1 16778993)

(^constant 'KEY_Farsi_2 16778994)

(^constant 'KEY_Farsi_3 16778995)

(^constant 'KEY_Farsi_4 16778996)

(^constant 'KEY_Farsi_5 16778997)

(^constant 'KEY_Farsi_6 16778998)

(^constant 'KEY_Farsi_7 16778999)

(^constant 'KEY_Farsi_8 16779000)

(^constant 'KEY_Farsi_9 16779001)

(^constant 'KEY_Farsi_yeh 16778956)

(^constant 'KEY_Favorites 269025072)

(^constant 'KEY_Finance 269025084)

(^constant 'KEY_Find 65384)

(^constant 'KEY_First_Virtual_Screen 65232)

(^constant 'KEY_Forward 269025063)

(^constant 'KEY_FrameBack 269025181)

(^constant 'KEY_FrameForward 269025182)

(^constant 'KEY_G 71)

(^constant 'KEY_Gabovedot 725)

(^constant 'KEY_Game 269025118)

(^constant 'KEY_Gbreve 683)

(^constant 'KEY_Gcaron 16777702)

(^constant 'KEY_Gcedilla 939)

(^constant 'KEY_Gcircumflex 728)

(^constant 'KEY_Georgian_an 16781520)

(^constant 'KEY_Georgian_ban 16781521)

(^constant 'KEY_Georgian_can 16781546)

(^constant 'KEY_Georgian_char 16781549)

(^constant 'KEY_Georgian_chin 16781545)

(^constant 'KEY_Georgian_cil 16781548)

(^constant 'KEY_Georgian_don 16781523)

(^constant 'KEY_Georgian_en 16781524)

(^constant 'KEY_Georgian_fi 16781558)

(^constant 'KEY_Georgian_gan 16781522)

(^constant 'KEY_Georgian_ghan 16781542)

(^constant 'KEY_Georgian_hae 16781552)

(^constant 'KEY_Georgian_har 16781556)

(^constant 'KEY_Georgian_he 16781553)

(^constant 'KEY_Georgian_hie 16781554)

(^constant 'KEY_Georgian_hoe 16781557)

(^constant 'KEY_Georgian_in 16781528)

(^constant 'KEY_Georgian_jhan 16781551)

(^constant 'KEY_Georgian_jil 16781547)

(^constant 'KEY_Georgian_kan 16781529)

(^constant 'KEY_Georgian_khar 16781541)

(^constant 'KEY_Georgian_las 16781530)

(^constant 'KEY_Georgian_man 16781531)

(^constant 'KEY_Georgian_nar 16781532)

(^constant 'KEY_Georgian_on 16781533)

(^constant 'KEY_Georgian_par 16781534)

(^constant 'KEY_Georgian_phar 16781540)

(^constant 'KEY_Georgian_qar 16781543)

(^constant 'KEY_Georgian_rae 16781536)

(^constant 'KEY_Georgian_san 16781537)

(^constant 'KEY_Georgian_shin 16781544)

(^constant 'KEY_Georgian_tan 16781527)

(^constant 'KEY_Georgian_tar 16781538)

(^constant 'KEY_Georgian_un 16781539)

(^constant 'KEY_Georgian_vin 16781525)

(^constant 'KEY_Georgian_we 16781555)

(^constant 'KEY_Georgian_xan 16781550)

(^constant 'KEY_Georgian_zen 16781526)

(^constant 'KEY_Georgian_zhar 16781535)

(^constant 'KEY_Go 269025119)

(^constant 'KEY_Greek_ALPHA 1985)

(^constant 'KEY_Greek_ALPHAaccent 1953)

(^constant 'KEY_Greek_BETA 1986)

(^constant 'KEY_Greek_CHI 2007)

(^constant 'KEY_Greek_DELTA 1988)

(^constant 'KEY_Greek_EPSILON 1989)

(^constant 'KEY_Greek_EPSILONaccent 1954)

(^constant 'KEY_Greek_ETA 1991)

(^constant 'KEY_Greek_ETAaccent 1955)

(^constant 'KEY_Greek_GAMMA 1987)

(^constant 'KEY_Greek_IOTA 1993)

(^constant 'KEY_Greek_IOTAaccent 1956)

(^constant 'KEY_Greek_IOTAdiaeresis 1957)

(^constant 'KEY_Greek_IOTAdieresis 1957)

(^constant 'KEY_Greek_KAPPA 1994)

(^constant 'KEY_Greek_LAMBDA 1995)

(^constant 'KEY_Greek_LAMDA 1995)

(^constant 'KEY_Greek_MU 1996)

(^constant 'KEY_Greek_NU 1997)

(^constant 'KEY_Greek_OMEGA 2009)

(^constant 'KEY_Greek_OMEGAaccent 1963)

(^constant 'KEY_Greek_OMICRON 1999)

(^constant 'KEY_Greek_OMICRONaccent 1959)

(^constant 'KEY_Greek_PHI 2006)

(^constant 'KEY_Greek_PI 2000)

(^constant 'KEY_Greek_PSI 2008)

(^constant 'KEY_Greek_RHO 2001)

(^constant 'KEY_Greek_SIGMA 2002)

(^constant 'KEY_Greek_TAU 2004)

(^constant 'KEY_Greek_THETA 1992)

(^constant 'KEY_Greek_UPSILON 2005)

(^constant 'KEY_Greek_UPSILONaccent 1960)

(^constant 'KEY_Greek_UPSILONdieresis 1961)

(^constant 'KEY_Greek_XI 1998)

(^constant 'KEY_Greek_ZETA 1990)

(^constant 'KEY_Greek_accentdieresis 1966)

(^constant 'KEY_Greek_alpha 2017)

(^constant 'KEY_Greek_alphaaccent 1969)

(^constant 'KEY_Greek_beta 2018)

(^constant 'KEY_Greek_chi 2039)

(^constant 'KEY_Greek_delta 2020)

(^constant 'KEY_Greek_epsilon 2021)

(^constant 'KEY_Greek_epsilonaccent 1970)

(^constant 'KEY_Greek_eta 2023)

(^constant 'KEY_Greek_etaaccent 1971)

(^constant 'KEY_Greek_finalsmallsigma 2035)

(^constant 'KEY_Greek_gamma 2019)

(^constant 'KEY_Greek_horizbar 1967)

(^constant 'KEY_Greek_iota 2025)

(^constant 'KEY_Greek_iotaaccent 1972)

(^constant 'KEY_Greek_iotaaccentdieresis 1974)

(^constant 'KEY_Greek_iotadieresis 1973)

(^constant 'KEY_Greek_kappa 2026)

(^constant 'KEY_Greek_lambda 2027)

(^constant 'KEY_Greek_lamda 2027)

(^constant 'KEY_Greek_mu 2028)

(^constant 'KEY_Greek_nu 2029)

(^constant 'KEY_Greek_omega 2041)

(^constant 'KEY_Greek_omegaaccent 1979)

(^constant 'KEY_Greek_omicron 2031)

(^constant 'KEY_Greek_omicronaccent 1975)

(^constant 'KEY_Greek_phi 2038)

(^constant 'KEY_Greek_pi 2032)

(^constant 'KEY_Greek_psi 2040)

(^constant 'KEY_Greek_rho 2033)

(^constant 'KEY_Greek_sigma 2034)

(^constant 'KEY_Greek_switch 65406)

(^constant 'KEY_Greek_tau 2036)

(^constant 'KEY_Greek_theta 2024)

(^constant 'KEY_Greek_upsilon 2037)

(^constant 'KEY_Greek_upsilonaccent 1976)

(^constant 'KEY_Greek_upsilonaccentdieresis 1978)

(^constant 'KEY_Greek_upsilondieresis 1977)

(^constant 'KEY_Greek_xi 2030)

(^constant 'KEY_Greek_zeta 2022)

(^constant 'KEY_Green 269025188)

(^constant 'KEY_H 72)

(^constant 'KEY_Hangul 65329)

(^constant 'KEY_Hangul_A 3775)

(^constant 'KEY_Hangul_AE 3776)

(^constant 'KEY_Hangul_AraeA 3830)

(^constant 'KEY_Hangul_AraeAE 3831)

(^constant 'KEY_Hangul_Banja 65337)

(^constant 'KEY_Hangul_Cieuc 3770)

(^constant 'KEY_Hangul_Codeinput 65335)

(^constant 'KEY_Hangul_Dikeud 3751)

(^constant 'KEY_Hangul_E 3780)

(^constant 'KEY_Hangul_EO 3779)

(^constant 'KEY_Hangul_EU 3793)

(^constant 'KEY_Hangul_End 65331)

(^constant 'KEY_Hangul_Hanja 65332)

(^constant 'KEY_Hangul_Hieuh 3774)

(^constant 'KEY_Hangul_I 3795)

(^constant 'KEY_Hangul_Ieung 3767)

(^constant 'KEY_Hangul_J_Cieuc 3818)

(^constant 'KEY_Hangul_J_Dikeud 3802)

(^constant 'KEY_Hangul_J_Hieuh 3822)

(^constant 'KEY_Hangul_J_Ieung 3816)

(^constant 'KEY_Hangul_J_Jieuj 3817)

(^constant 'KEY_Hangul_J_Khieuq 3819)

(^constant 'KEY_Hangul_J_Kiyeog 3796)

(^constant 'KEY_Hangul_J_KiyeogSios 3798)

(^constant 'KEY_Hangul_J_KkogjiDalrinIeung 3833)

(^constant 'KEY_Hangul_J_Mieum 3811)

(^constant 'KEY_Hangul_J_Nieun 3799)

(^constant 'KEY_Hangul_J_NieunHieuh 3801)

(^constant 'KEY_Hangul_J_NieunJieuj 3800)

(^constant 'KEY_Hangul_J_PanSios 3832)

(^constant 'KEY_Hangul_J_Phieuf 3821)

(^constant 'KEY_Hangul_J_Pieub 3812)

(^constant 'KEY_Hangul_J_PieubSios 3813)

(^constant 'KEY_Hangul_J_Rieul 3803)

(^constant 'KEY_Hangul_J_RieulHieuh 3810)

(^constant 'KEY_Hangul_J_RieulKiyeog 3804)

(^constant 'KEY_Hangul_J_RieulMieum 3805)

(^constant 'KEY_Hangul_J_RieulPhieuf 3809)

(^constant 'KEY_Hangul_J_RieulPieub 3806)

(^constant 'KEY_Hangul_J_RieulSios 3807)

(^constant 'KEY_Hangul_J_RieulTieut 3808)

(^constant 'KEY_Hangul_J_Sios 3814)

(^constant 'KEY_Hangul_J_SsangKiyeog 3797)

(^constant 'KEY_Hangul_J_SsangSios 3815)

(^constant 'KEY_Hangul_J_Tieut 3820)

(^constant 'KEY_Hangul_J_YeorinHieuh 3834)

(^constant 'KEY_Hangul_Jamo 65333)

(^constant 'KEY_Hangul_Jeonja 65336)

(^constant 'KEY_Hangul_Jieuj 3768)

(^constant 'KEY_Hangul_Khieuq 3771)

(^constant 'KEY_Hangul_Kiyeog 3745)

(^constant 'KEY_Hangul_KiyeogSios 3747)

(^constant 'KEY_Hangul_KkogjiDalrinIeung 3827)

(^constant 'KEY_Hangul_Mieum 3761)

(^constant 'KEY_Hangul_MultipleCandidate 65341)

(^constant 'KEY_Hangul_Nieun 3748)

(^constant 'KEY_Hangul_NieunHieuh 3750)

(^constant 'KEY_Hangul_NieunJieuj 3749)

(^constant 'KEY_Hangul_O 3783)

(^constant 'KEY_Hangul_OE 3786)

(^constant 'KEY_Hangul_PanSios 3826)

(^constant 'KEY_Hangul_Phieuf 3773)

(^constant 'KEY_Hangul_Pieub 3762)

(^constant 'KEY_Hangul_PieubSios 3764)

(^constant 'KEY_Hangul_PostHanja 65339)

(^constant 'KEY_Hangul_PreHanja 65338)

(^constant 'KEY_Hangul_PreviousCandidate 65342)

(^constant 'KEY_Hangul_Rieul 3753)

(^constant 'KEY_Hangul_RieulHieuh 3760)

(^constant 'KEY_Hangul_RieulKiyeog 3754)

(^constant 'KEY_Hangul_RieulMieum 3755)

(^constant 'KEY_Hangul_RieulPhieuf 3759)

(^constant 'KEY_Hangul_RieulPieub 3756)

(^constant 'KEY_Hangul_RieulSios 3757)

(^constant 'KEY_Hangul_RieulTieut 3758)

(^constant 'KEY_Hangul_RieulYeorinHieuh 3823)

(^constant 'KEY_Hangul_Romaja 65334)

(^constant 'KEY_Hangul_SingleCandidate 65340)

(^constant 'KEY_Hangul_Sios 3765)

(^constant 'KEY_Hangul_Special 65343)

(^constant 'KEY_Hangul_SsangDikeud 3752)

(^constant 'KEY_Hangul_SsangJieuj 3769)

(^constant 'KEY_Hangul_SsangKiyeog 3746)

(^constant 'KEY_Hangul_SsangPieub 3763)

(^constant 'KEY_Hangul_SsangSios 3766)

(^constant 'KEY_Hangul_Start 65330)

(^constant 'KEY_Hangul_SunkyeongeumMieum 3824)

(^constant 'KEY_Hangul_SunkyeongeumPhieuf 3828)

(^constant 'KEY_Hangul_SunkyeongeumPieub 3825)

(^constant 'KEY_Hangul_Tieut 3772)

(^constant 'KEY_Hangul_U 3788)

(^constant 'KEY_Hangul_WA 3784)

(^constant 'KEY_Hangul_WAE 3785)

(^constant 'KEY_Hangul_WE 3790)

(^constant 'KEY_Hangul_WEO 3789)

(^constant 'KEY_Hangul_WI 3791)

(^constant 'KEY_Hangul_YA 3777)

(^constant 'KEY_Hangul_YAE 3778)

(^constant 'KEY_Hangul_YE 3782)

(^constant 'KEY_Hangul_YEO 3781)

(^constant 'KEY_Hangul_YI 3794)

(^constant 'KEY_Hangul_YO 3787)

(^constant 'KEY_Hangul_YU 3792)

(^constant 'KEY_Hangul_YeorinHieuh 3829)

(^constant 'KEY_Hangul_switch 65406)

(^constant 'KEY_Hankaku 65321)

(^constant 'KEY_Hcircumflex 678)

(^constant 'KEY_Hebrew_switch 65406)

(^constant 'KEY_Help 65386)

(^constant 'KEY_Henkan 65315)

(^constant 'KEY_Henkan_Mode 65315)

(^constant 'KEY_Hibernate 269025192)

(^constant 'KEY_Hiragana 65317)

(^constant 'KEY_Hiragana_Katakana 65319)

(^constant 'KEY_History 269025079)

(^constant 'KEY_Home 65360)

(^constant 'KEY_HomePage 269025048)

(^constant 'KEY_HotLinks 269025082)

(^constant 'KEY_Hstroke 673)

(^constant 'KEY_Hyper_L 65517)

(^constant 'KEY_Hyper_R 65518)

(^constant 'KEY_I 73)

(^constant 'KEY_ISO_Center_Object 65075)

(^constant 'KEY_ISO_Continuous_Underline 65072)

(^constant
  'KEY_ISO_Discontinuous_Underline
  65073)

(^constant 'KEY_ISO_Emphasize 65074)

(^constant 'KEY_ISO_Enter 65076)

(^constant 'KEY_ISO_Fast_Cursor_Down 65071)

(^constant 'KEY_ISO_Fast_Cursor_Left 65068)

(^constant 'KEY_ISO_Fast_Cursor_Right 65069)

(^constant 'KEY_ISO_Fast_Cursor_Up 65070)

(^constant 'KEY_ISO_First_Group 65036)

(^constant 'KEY_ISO_First_Group_Lock 65037)

(^constant 'KEY_ISO_Group_Latch 65030)

(^constant 'KEY_ISO_Group_Lock 65031)

(^constant 'KEY_ISO_Group_Shift 65406)

(^constant 'KEY_ISO_Last_Group 65038)

(^constant 'KEY_ISO_Last_Group_Lock 65039)

(^constant 'KEY_ISO_Left_Tab 65056)

(^constant 'KEY_ISO_Level2_Latch 65026)

(^constant 'KEY_ISO_Level3_Latch 65028)

(^constant 'KEY_ISO_Level3_Lock 65029)

(^constant 'KEY_ISO_Level3_Shift 65027)

(^constant 'KEY_ISO_Level5_Latch 65042)

(^constant 'KEY_ISO_Level5_Lock 65043)

(^constant 'KEY_ISO_Level5_Shift 65041)

(^constant 'KEY_ISO_Lock 65025)

(^constant 'KEY_ISO_Move_Line_Down 65058)

(^constant 'KEY_ISO_Move_Line_Up 65057)

(^constant 'KEY_ISO_Next_Group 65032)

(^constant 'KEY_ISO_Next_Group_Lock 65033)

(^constant 'KEY_ISO_Partial_Line_Down 65060)

(^constant 'KEY_ISO_Partial_Line_Up 65059)

(^constant 'KEY_ISO_Partial_Space_Left 65061)

(^constant 'KEY_ISO_Partial_Space_Right 65062)

(^constant 'KEY_ISO_Prev_Group 65034)

(^constant 'KEY_ISO_Prev_Group_Lock 65035)

(^constant 'KEY_ISO_Release_Both_Margins 65067)

(^constant 'KEY_ISO_Release_Margin_Left 65065)

(^constant 'KEY_ISO_Release_Margin_Right 65066)

(^constant 'KEY_ISO_Set_Margin_Left 65063)

(^constant 'KEY_ISO_Set_Margin_Right 65064)

(^constant 'KEY_Iabovedot 681)

(^constant 'KEY_Iacute 205)

(^constant 'KEY_Ibelowdot 16785098)

(^constant 'KEY_Ibreve 16777516)

(^constant 'KEY_Icircumflex 206)

(^constant 'KEY_Idiaeresis 207)

(^constant 'KEY_Igrave 204)

(^constant 'KEY_Ihook 16785096)

(^constant 'KEY_Imacron 975)

(^constant 'KEY_Insert 65379)

(^constant 'KEY_Iogonek 967)

(^constant 'KEY_Itilde 933)

(^constant 'KEY_J 74)

(^constant 'KEY_Jcircumflex 684)

(^constant 'KEY_K 75)

(^constant 'KEY_KP_0 65456)

(^constant 'KEY_KP_1 65457)

(^constant 'KEY_KP_2 65458)

(^constant 'KEY_KP_3 65459)

(^constant 'KEY_KP_4 65460)

(^constant 'KEY_KP_5 65461)

(^constant 'KEY_KP_6 65462)

(^constant 'KEY_KP_7 65463)

(^constant 'KEY_KP_8 65464)

(^constant 'KEY_KP_9 65465)

(^constant 'KEY_KP_Add 65451)

(^constant 'KEY_KP_Begin 65437)

(^constant 'KEY_KP_Decimal 65454)

(^constant 'KEY_KP_Delete 65439)

(^constant 'KEY_KP_Divide 65455)

(^constant 'KEY_KP_Down 65433)

(^constant 'KEY_KP_End 65436)

(^constant 'KEY_KP_Enter 65421)

(^constant 'KEY_KP_Equal 65469)

(^constant 'KEY_KP_F1 65425)

(^constant 'KEY_KP_F2 65426)

(^constant 'KEY_KP_F3 65427)

(^constant 'KEY_KP_F4 65428)

(^constant 'KEY_KP_Home 65429)

(^constant 'KEY_KP_Insert 65438)

(^constant 'KEY_KP_Left 65430)

(^constant 'KEY_KP_Multiply 65450)

(^constant 'KEY_KP_Next 65435)

(^constant 'KEY_KP_Page_Down 65435)

(^constant 'KEY_KP_Page_Up 65434)

(^constant 'KEY_KP_Prior 65434)

(^constant 'KEY_KP_Right 65432)

(^constant 'KEY_KP_Separator 65452)

(^constant 'KEY_KP_Space 65408)

(^constant 'KEY_KP_Subtract 65453)

(^constant 'KEY_KP_Tab 65417)

(^constant 'KEY_KP_Up 65431)

(^constant 'KEY_Kana_Lock 65325)

(^constant 'KEY_Kana_Shift 65326)

(^constant 'KEY_Kanji 65313)

(^constant 'KEY_Kanji_Bangou 65335)

(^constant 'KEY_Katakana 65318)

(^constant 'KEY_KbdBrightnessDown 269025030)

(^constant 'KEY_KbdBrightnessUp 269025029)

(^constant 'KEY_KbdLightOnOff 269025028)

(^constant 'KEY_Kcedilla 979)

(^constant 'KEY_Keyboard 269025203)

(^constant 'KEY_Korean_Won 3839)

(^constant 'KEY_L 76)

(^constant 'KEY_L1 65480)

(^constant 'KEY_L10 65489)

(^constant 'KEY_L2 65481)

(^constant 'KEY_L3 65482)

(^constant 'KEY_L4 65483)

(^constant 'KEY_L5 65484)

(^constant 'KEY_L6 65485)

(^constant 'KEY_L7 65486)

(^constant 'KEY_L8 65487)

(^constant 'KEY_L9 65488)

(^constant 'KEY_Lacute 453)

(^constant 'KEY_Last_Virtual_Screen 65236)

(^constant 'KEY_Launch0 269025088)

(^constant 'KEY_Launch1 269025089)

(^constant 'KEY_Launch2 269025090)

(^constant 'KEY_Launch3 269025091)

(^constant 'KEY_Launch4 269025092)

(^constant 'KEY_Launch5 269025093)

(^constant 'KEY_Launch6 269025094)

(^constant 'KEY_Launch7 269025095)

(^constant 'KEY_Launch8 269025096)

(^constant 'KEY_Launch9 269025097)

(^constant 'KEY_LaunchA 269025098)

(^constant 'KEY_LaunchB 269025099)

(^constant 'KEY_LaunchC 269025100)

(^constant 'KEY_LaunchD 269025101)

(^constant 'KEY_LaunchE 269025102)

(^constant 'KEY_LaunchF 269025103)

(^constant 'KEY_Lbelowdot 16784950)

(^constant 'KEY_Lcaron 421)

(^constant 'KEY_Lcedilla 934)

(^constant 'KEY_Left 65361)

(^constant 'KEY_LightBulb 269025077)

(^constant 'KEY_Linefeed 65290)

(^constant 'KEY_LiraSign 16785572)

(^constant 'KEY_LogGrabInfo 269024805)

(^constant 'KEY_LogOff 269025121)

(^constant 'KEY_LogWindowTree 269024804)

(^constant 'KEY_Lstroke 419)

(^constant 'KEY_M 77)

(^constant 'KEY_Mabovedot 16784960)

(^constant 'KEY_Macedonia_DSE 1717)

(^constant 'KEY_Macedonia_GJE 1714)

(^constant 'KEY_Macedonia_KJE 1724)

(^constant 'KEY_Macedonia_dse 1701)

(^constant 'KEY_Macedonia_gje 1698)

(^constant 'KEY_Macedonia_kje 1708)

(^constant 'KEY_Mae_Koho 65342)

(^constant 'KEY_Mail 269025049)

(^constant 'KEY_MailForward 269025168)

(^constant 'KEY_Market 269025122)

(^constant 'KEY_Massyo 65324)

(^constant 'KEY_Meeting 269025123)

(^constant 'KEY_Memo 269025054)

(^constant 'KEY_Menu 65383)

(^constant 'KEY_MenuKB 269025125)

(^constant 'KEY_MenuPB 269025126)

(^constant 'KEY_Messenger 269025166)

(^constant 'KEY_Meta_L 65511)

(^constant 'KEY_Meta_R 65512)

(^constant 'KEY_MillSign 16785573)

(^constant 'KEY_ModeLock 269025025)

(^constant 'KEY_Mode_switch 65406)

(^constant 'KEY_MonBrightnessDown 269025027)

(^constant 'KEY_MonBrightnessUp 269025026)

(^constant 'KEY_MouseKeys_Accel_Enable 65143)

(^constant 'KEY_MouseKeys_Enable 65142)

(^constant 'KEY_Muhenkan 65314)

(^constant 'KEY_Multi_key 65312)

(^constant 'KEY_MultipleCandidate 65341)

(^constant 'KEY_Music 269025170)

(^constant 'KEY_MyComputer 269025075)

(^constant 'KEY_MySites 269025127)

(^constant 'KEY_N 78)

(^constant 'KEY_Nacute 465)

(^constant 'KEY_NairaSign 16785574)

(^constant 'KEY_Ncaron 466)

(^constant 'KEY_Ncedilla 977)

(^constant 'KEY_New 269025128)

(^constant 'KEY_NewSheqelSign 16785578)

(^constant 'KEY_News 269025129)

(^constant 'KEY_Next 65366)

(^constant 'KEY_Next_VMode 269024802)

(^constant 'KEY_Next_Virtual_Screen 65234)

(^constant 'KEY_Ntilde 209)

(^constant 'KEY_Num_Lock 65407)

(^constant 'KEY_O 79)

(^constant 'KEY_OE 5052)

(^constant 'KEY_Oacute 211)

(^constant 'KEY_Obarred 16777631)

(^constant 'KEY_Obelowdot 16785100)

(^constant 'KEY_Ocaron 16777681)

(^constant 'KEY_Ocircumflex 212)

(^constant 'KEY_Ocircumflexacute 16785104)

(^constant 'KEY_Ocircumflexbelowdot 16785112)

(^constant 'KEY_Ocircumflexgrave 16785106)

(^constant 'KEY_Ocircumflexhook 16785108)

(^constant 'KEY_Ocircumflextilde 16785110)

(^constant 'KEY_Odiaeresis 214)

(^constant 'KEY_Odoubleacute 469)

(^constant 'KEY_OfficeHome 269025130)

(^constant 'KEY_Ograve 210)

(^constant 'KEY_Ohook 16785102)

(^constant 'KEY_Ohorn 16777632)

(^constant 'KEY_Ohornacute 16785114)

(^constant 'KEY_Ohornbelowdot 16785122)

(^constant 'KEY_Ohorngrave 16785116)

(^constant 'KEY_Ohornhook 16785118)

(^constant 'KEY_Ohorntilde 16785120)

(^constant 'KEY_Omacron 978)

(^constant 'KEY_Ooblique 216)

(^constant 'KEY_Open 269025131)

(^constant 'KEY_OpenURL 269025080)

(^constant 'KEY_Option 269025132)

(^constant 'KEY_Oslash 216)

(^constant 'KEY_Otilde 213)

(^constant 'KEY_Overlay1_Enable 65144)

(^constant 'KEY_Overlay2_Enable 65145)

(^constant 'KEY_P 80)

(^constant 'KEY_Pabovedot 16784982)

(^constant 'KEY_Page_Down 65366)

(^constant 'KEY_Page_Up 65365)

(^constant 'KEY_Paste 269025133)

(^constant 'KEY_Pause 65299)

(^constant 'KEY_PesetaSign 16785575)

(^constant 'KEY_Phone 269025134)

(^constant 'KEY_Pictures 269025169)

(^constant 'KEY_Pointer_Accelerate 65274)

(^constant 'KEY_Pointer_Button1 65257)

(^constant 'KEY_Pointer_Button2 65258)

(^constant 'KEY_Pointer_Button3 65259)

(^constant 'KEY_Pointer_Button4 65260)

(^constant 'KEY_Pointer_Button5 65261)

(^constant 'KEY_Pointer_Button_Dflt 65256)

(^constant 'KEY_Pointer_DblClick1 65263)

(^constant 'KEY_Pointer_DblClick2 65264)

(^constant 'KEY_Pointer_DblClick3 65265)

(^constant 'KEY_Pointer_DblClick4 65266)

(^constant 'KEY_Pointer_DblClick5 65267)

(^constant 'KEY_Pointer_DblClick_Dflt 65262)

(^constant 'KEY_Pointer_DfltBtnNext 65275)

(^constant 'KEY_Pointer_DfltBtnPrev 65276)

(^constant 'KEY_Pointer_Down 65251)

(^constant 'KEY_Pointer_DownLeft 65254)

(^constant 'KEY_Pointer_DownRight 65255)

(^constant 'KEY_Pointer_Drag1 65269)

(^constant 'KEY_Pointer_Drag2 65270)

(^constant 'KEY_Pointer_Drag3 65271)

(^constant 'KEY_Pointer_Drag4 65272)

(^constant 'KEY_Pointer_Drag5 65277)

(^constant 'KEY_Pointer_Drag_Dflt 65268)

(^constant 'KEY_Pointer_EnableKeys 65273)

(^constant 'KEY_Pointer_Left 65248)

(^constant 'KEY_Pointer_Right 65249)

(^constant 'KEY_Pointer_Up 65250)

(^constant 'KEY_Pointer_UpLeft 65252)

(^constant 'KEY_Pointer_UpRight 65253)

(^constant 'KEY_PowerDown 269025057)

(^constant 'KEY_PowerOff 269025066)

(^constant 'KEY_Prev_VMode 269024803)

(^constant 'KEY_Prev_Virtual_Screen 65233)

(^constant 'KEY_PreviousCandidate 65342)

(^constant 'KEY_Print 65377)

(^constant 'KEY_Prior 65365)

(^constant 'KEY_Q 81)

(^constant 'KEY_R 82)

(^constant 'KEY_R1 65490)

(^constant 'KEY_R10 65499)

(^constant 'KEY_R11 65500)

(^constant 'KEY_R12 65501)

(^constant 'KEY_R13 65502)

(^constant 'KEY_R14 65503)

(^constant 'KEY_R15 65504)

(^constant 'KEY_R2 65491)

(^constant 'KEY_R3 65492)

(^constant 'KEY_R4 65493)

(^constant 'KEY_R5 65494)

(^constant 'KEY_R6 65495)

(^constant 'KEY_R7 65496)

(^constant 'KEY_R8 65497)

(^constant 'KEY_R9 65498)

(^constant 'KEY_RFKill 269025205)

(^constant 'KEY_Racute 448)

(^constant 'KEY_Rcaron 472)

(^constant 'KEY_Rcedilla 931)

(^constant 'KEY_Red 269025187)

(^constant 'KEY_Redo 65382)

(^constant 'KEY_Refresh 269025065)

(^constant 'KEY_Reload 269025139)

(^constant 'KEY_RepeatKeys_Enable 65138)

(^constant 'KEY_Reply 269025138)

(^constant 'KEY_Return 65293)

(^constant 'KEY_Right 65363)

(^constant 'KEY_RockerDown 269025060)

(^constant 'KEY_RockerEnter 269025061)

(^constant 'KEY_RockerUp 269025059)

(^constant 'KEY_Romaji 65316)

(^constant 'KEY_RotateWindows 269025140)

(^constant 'KEY_RotationKB 269025142)

(^constant 'KEY_RotationPB 269025141)

(^constant 'KEY_RupeeSign 16785576)

(^constant 'KEY_S 83)

(^constant 'KEY_SCHWA 16777615)

(^constant 'KEY_Sabovedot 16784992)

(^constant 'KEY_Sacute 422)

(^constant 'KEY_Save 269025143)

(^constant 'KEY_Scaron 425)

(^constant 'KEY_Scedilla 426)

(^constant 'KEY_Scircumflex 734)

(^constant 'KEY_ScreenSaver 269025069)

(^constant 'KEY_ScrollClick 269025146)

(^constant 'KEY_ScrollDown 269025145)

(^constant 'KEY_ScrollUp 269025144)

(^constant 'KEY_Scroll_Lock 65300)

(^constant 'KEY_Search 269025051)

(^constant 'KEY_Select 65376)

(^constant 'KEY_SelectButton 269025184)

(^constant 'KEY_Send 269025147)

(^constant 'KEY_Serbian_DJE 1713)

(^constant 'KEY_Serbian_DZE 1727)

(^constant 'KEY_Serbian_JE 1720)

(^constant 'KEY_Serbian_LJE 1721)

(^constant 'KEY_Serbian_NJE 1722)

(^constant 'KEY_Serbian_TSHE 1723)

(^constant 'KEY_Serbian_dje 1697)

(^constant 'KEY_Serbian_dze 1711)

(^constant 'KEY_Serbian_je 1704)

(^constant 'KEY_Serbian_lje 1705)

(^constant 'KEY_Serbian_nje 1706)

(^constant 'KEY_Serbian_tshe 1707)

(^constant 'KEY_Shift_L 65505)

(^constant 'KEY_Shift_Lock 65510)

(^constant 'KEY_Shift_R 65506)

(^constant 'KEY_Shop 269025078)

(^constant 'KEY_SingleCandidate 65340)

(^constant 'KEY_Sinh_a 16780677)

(^constant 'KEY_Sinh_aa 16780678)

(^constant 'KEY_Sinh_aa2 16780751)

(^constant 'KEY_Sinh_ae 16780679)

(^constant 'KEY_Sinh_ae2 16780752)

(^constant 'KEY_Sinh_aee 16780680)

(^constant 'KEY_Sinh_aee2 16780753)

(^constant 'KEY_Sinh_ai 16780691)

(^constant 'KEY_Sinh_ai2 16780763)

(^constant 'KEY_Sinh_al 16780746)

(^constant 'KEY_Sinh_au 16780694)

(^constant 'KEY_Sinh_au2 16780766)

(^constant 'KEY_Sinh_ba 16780726)

(^constant 'KEY_Sinh_bha 16780727)

(^constant 'KEY_Sinh_ca 16780704)

(^constant 'KEY_Sinh_cha 16780705)

(^constant 'KEY_Sinh_dda 16780713)

(^constant 'KEY_Sinh_ddha 16780714)

(^constant 'KEY_Sinh_dha 16780719)

(^constant 'KEY_Sinh_dhha 16780720)

(^constant 'KEY_Sinh_e 16780689)

(^constant 'KEY_Sinh_e2 16780761)

(^constant 'KEY_Sinh_ee 16780690)

(^constant 'KEY_Sinh_ee2 16780762)

(^constant 'KEY_Sinh_fa 16780742)

(^constant 'KEY_Sinh_ga 16780700)

(^constant 'KEY_Sinh_gha 16780701)

(^constant 'KEY_Sinh_h2 16780675)

(^constant 'KEY_Sinh_ha 16780740)

(^constant 'KEY_Sinh_i 16780681)

(^constant 'KEY_Sinh_i2 16780754)

(^constant 'KEY_Sinh_ii 16780682)

(^constant 'KEY_Sinh_ii2 16780755)

(^constant 'KEY_Sinh_ja 16780706)

(^constant 'KEY_Sinh_jha 16780707)

(^constant 'KEY_Sinh_jnya 16780709)

(^constant 'KEY_Sinh_ka 16780698)

(^constant 'KEY_Sinh_kha 16780699)

(^constant 'KEY_Sinh_kunddaliya 16780788)

(^constant 'KEY_Sinh_la 16780733)

(^constant 'KEY_Sinh_lla 16780741)

(^constant 'KEY_Sinh_lu 16780687)

(^constant 'KEY_Sinh_lu2 16780767)

(^constant 'KEY_Sinh_luu 16780688)

(^constant 'KEY_Sinh_luu2 16780787)

(^constant 'KEY_Sinh_ma 16780728)

(^constant 'KEY_Sinh_mba 16780729)

(^constant 'KEY_Sinh_na 16780721)

(^constant 'KEY_Sinh_ndda 16780716)

(^constant 'KEY_Sinh_ndha 16780723)

(^constant 'KEY_Sinh_ng 16780674)

(^constant 'KEY_Sinh_ng2 16780702)

(^constant 'KEY_Sinh_nga 16780703)

(^constant 'KEY_Sinh_nja 16780710)

(^constant 'KEY_Sinh_nna 16780715)

(^constant 'KEY_Sinh_nya 16780708)

(^constant 'KEY_Sinh_o 16780692)

(^constant 'KEY_Sinh_o2 16780764)

(^constant 'KEY_Sinh_oo 16780693)

(^constant 'KEY_Sinh_oo2 16780765)

(^constant 'KEY_Sinh_pa 16780724)

(^constant 'KEY_Sinh_pha 16780725)

(^constant 'KEY_Sinh_ra 16780731)

(^constant 'KEY_Sinh_ri 16780685)

(^constant 'KEY_Sinh_rii 16780686)

(^constant 'KEY_Sinh_ru2 16780760)

(^constant 'KEY_Sinh_ruu2 16780786)

(^constant 'KEY_Sinh_sa 16780739)

(^constant 'KEY_Sinh_sha 16780737)

(^constant 'KEY_Sinh_ssha 16780738)

(^constant 'KEY_Sinh_tha 16780717)

(^constant 'KEY_Sinh_thha 16780718)

(^constant 'KEY_Sinh_tta 16780711)

(^constant 'KEY_Sinh_ttha 16780712)

(^constant 'KEY_Sinh_u 16780683)

(^constant 'KEY_Sinh_u2 16780756)

(^constant 'KEY_Sinh_uu 16780684)

(^constant 'KEY_Sinh_uu2 16780758)

(^constant 'KEY_Sinh_va 16780736)

(^constant 'KEY_Sinh_ya 16780730)

(^constant 'KEY_Sleep 269025071)

(^constant 'KEY_SlowKeys_Enable 65139)

(^constant 'KEY_Spell 269025148)

(^constant 'KEY_SplitScreen 269025149)

(^constant 'KEY_Standby 269025040)

(^constant 'KEY_Start 269025050)

(^constant 'KEY_StickyKeys_Enable 65141)

(^constant 'KEY_Stop 269025064)

(^constant 'KEY_Subtitle 269025178)

(^constant 'KEY_Super_L 65515)

(^constant 'KEY_Super_R 65516)

(^constant 'KEY_Support 269025150)

(^constant 'KEY_Suspend 269025191)

(^constant 'KEY_Switch_VT_1 269024769)

(^constant 'KEY_Switch_VT_10 269024778)

(^constant 'KEY_Switch_VT_11 269024779)

(^constant 'KEY_Switch_VT_12 269024780)

(^constant 'KEY_Switch_VT_2 269024770)

(^constant 'KEY_Switch_VT_3 269024771)

(^constant 'KEY_Switch_VT_4 269024772)

(^constant 'KEY_Switch_VT_5 269024773)

(^constant 'KEY_Switch_VT_6 269024774)

(^constant 'KEY_Switch_VT_7 269024775)

(^constant 'KEY_Switch_VT_8 269024776)

(^constant 'KEY_Switch_VT_9 269024777)

(^constant 'KEY_Sys_Req 65301)

(^constant 'KEY_T 84)

(^constant 'KEY_THORN 222)

(^constant 'KEY_Tab 65289)

(^constant 'KEY_Tabovedot 16785002)

(^constant 'KEY_TaskPane 269025151)

(^constant 'KEY_Tcaron 427)

(^constant 'KEY_Tcedilla 478)

(^constant 'KEY_Terminal 269025152)

(^constant 'KEY_Terminate_Server 65237)

(^constant 'KEY_Thai_baht 3551)

(^constant 'KEY_Thai_bobaimai 3514)

(^constant 'KEY_Thai_chochan 3496)

(^constant 'KEY_Thai_chochang 3498)

(^constant 'KEY_Thai_choching 3497)

(^constant 'KEY_Thai_chochoe 3500)

(^constant 'KEY_Thai_dochada 3502)

(^constant 'KEY_Thai_dodek 3508)

(^constant 'KEY_Thai_fofa 3517)

(^constant 'KEY_Thai_fofan 3519)

(^constant 'KEY_Thai_hohip 3531)

(^constant 'KEY_Thai_honokhuk 3534)

(^constant 'KEY_Thai_khokhai 3490)

(^constant 'KEY_Thai_khokhon 3493)

(^constant 'KEY_Thai_khokhuat 3491)

(^constant 'KEY_Thai_khokhwai 3492)

(^constant 'KEY_Thai_khorakhang 3494)

(^constant 'KEY_Thai_kokai 3489)

(^constant 'KEY_Thai_lakkhangyao 3557)

(^constant 'KEY_Thai_lekchet 3575)

(^constant 'KEY_Thai_lekha 3573)

(^constant 'KEY_Thai_lekhok 3574)

(^constant 'KEY_Thai_lekkao 3577)

(^constant 'KEY_Thai_leknung 3569)

(^constant 'KEY_Thai_lekpaet 3576)

(^constant 'KEY_Thai_leksam 3571)

(^constant 'KEY_Thai_leksi 3572)

(^constant 'KEY_Thai_leksong 3570)

(^constant 'KEY_Thai_leksun 3568)

(^constant 'KEY_Thai_lochula 3532)

(^constant 'KEY_Thai_loling 3525)

(^constant 'KEY_Thai_lu 3526)

(^constant 'KEY_Thai_maichattawa 3563)

(^constant 'KEY_Thai_maiek 3560)

(^constant 'KEY_Thai_maihanakat 3537)

(^constant 'KEY_Thai_maihanakat_maitho 3550)

(^constant 'KEY_Thai_maitaikhu 3559)

(^constant 'KEY_Thai_maitho 3561)

(^constant 'KEY_Thai_maitri 3562)

(^constant 'KEY_Thai_maiyamok 3558)

(^constant 'KEY_Thai_moma 3521)

(^constant 'KEY_Thai_ngongu 3495)

(^constant 'KEY_Thai_nikhahit 3565)

(^constant 'KEY_Thai_nonen 3507)

(^constant 'KEY_Thai_nonu 3513)

(^constant 'KEY_Thai_oang 3533)

(^constant 'KEY_Thai_paiyannoi 3535)

(^constant 'KEY_Thai_phinthu 3546)

(^constant 'KEY_Thai_phophan 3518)

(^constant 'KEY_Thai_phophung 3516)

(^constant 'KEY_Thai_phosamphao 3520)

(^constant 'KEY_Thai_popla 3515)

(^constant 'KEY_Thai_rorua 3523)

(^constant 'KEY_Thai_ru 3524)

(^constant 'KEY_Thai_saraa 3536)

(^constant 'KEY_Thai_saraaa 3538)

(^constant 'KEY_Thai_saraae 3553)

(^constant 'KEY_Thai_saraaimaimalai 3556)

(^constant 'KEY_Thai_saraaimaimuan 3555)

(^constant 'KEY_Thai_saraam 3539)

(^constant 'KEY_Thai_sarae 3552)

(^constant 'KEY_Thai_sarai 3540)

(^constant 'KEY_Thai_saraii 3541)

(^constant 'KEY_Thai_sarao 3554)

(^constant 'KEY_Thai_sarau 3544)

(^constant 'KEY_Thai_saraue 3542)

(^constant 'KEY_Thai_sarauee 3543)

(^constant 'KEY_Thai_sarauu 3545)

(^constant 'KEY_Thai_sorusi 3529)

(^constant 'KEY_Thai_sosala 3528)

(^constant 'KEY_Thai_soso 3499)

(^constant 'KEY_Thai_sosua 3530)

(^constant 'KEY_Thai_thanthakhat 3564)

(^constant 'KEY_Thai_thonangmontho 3505)

(^constant 'KEY_Thai_thophuthao 3506)

(^constant 'KEY_Thai_thothahan 3511)

(^constant 'KEY_Thai_thothan 3504)

(^constant 'KEY_Thai_thothong 3512)

(^constant 'KEY_Thai_thothung 3510)

(^constant 'KEY_Thai_topatak 3503)

(^constant 'KEY_Thai_totao 3509)

(^constant 'KEY_Thai_wowaen 3527)

(^constant 'KEY_Thai_yoyak 3522)

(^constant 'KEY_Thai_yoying 3501)

(^constant 'KEY_Thorn 222)

(^constant 'KEY_Time 269025183)

(^constant 'KEY_ToDoList 269025055)

(^constant 'KEY_Tools 269025153)

(^constant 'KEY_TopMenu 269025186)

(^constant 'KEY_TouchpadOff 269025201)

(^constant 'KEY_TouchpadOn 269025200)

(^constant 'KEY_TouchpadToggle 269025193)

(^constant 'KEY_Touroku 65323)

(^constant 'KEY_Travel 269025154)

(^constant 'KEY_Tslash 940)

(^constant 'KEY_U 85)

(^constant 'KEY_UWB 269025174)

(^constant 'KEY_Uacute 218)

(^constant 'KEY_Ubelowdot 16785124)

(^constant 'KEY_Ubreve 733)

(^constant 'KEY_Ucircumflex 219)

(^constant 'KEY_Udiaeresis 220)

(^constant 'KEY_Udoubleacute 475)

(^constant 'KEY_Ugrave 217)

(^constant 'KEY_Uhook 16785126)

(^constant 'KEY_Uhorn 16777647)

(^constant 'KEY_Uhornacute 16785128)

(^constant 'KEY_Uhornbelowdot 16785136)

(^constant 'KEY_Uhorngrave 16785130)

(^constant 'KEY_Uhornhook 16785132)

(^constant 'KEY_Uhorntilde 16785134)

(^constant 'KEY_Ukrainian_GHE_WITH_UPTURN 1725)

(^constant 'KEY_Ukrainian_I 1718)

(^constant 'KEY_Ukrainian_IE 1716)

(^constant 'KEY_Ukrainian_YI 1719)

(^constant 'KEY_Ukrainian_ghe_with_upturn 1709)

(^constant 'KEY_Ukrainian_i 1702)

(^constant 'KEY_Ukrainian_ie 1700)

(^constant 'KEY_Ukrainian_yi 1703)

(^constant 'KEY_Ukranian_I 1718)

(^constant 'KEY_Ukranian_JE 1716)

(^constant 'KEY_Ukranian_YI 1719)

(^constant 'KEY_Ukranian_i 1702)

(^constant 'KEY_Ukranian_je 1700)

(^constant 'KEY_Ukranian_yi 1703)

(^constant 'KEY_Umacron 990)

(^constant 'KEY_Undo 65381)

(^constant 'KEY_Ungrab 269024800)

(^constant 'KEY_Uogonek 985)

(^constant 'KEY_Up 65362)

(^constant 'KEY_Uring 473)

(^constant 'KEY_User1KB 269025157)

(^constant 'KEY_User2KB 269025158)

(^constant 'KEY_UserPB 269025156)

(^constant 'KEY_Utilde 989)

(^constant 'KEY_V 86)

(^constant 'KEY_VendorHome 269025076)

(^constant 'KEY_Video 269025159)

(^constant 'KEY_View 269025185)

(^constant 'KEY_VoidSymbol 16777215)

(^constant 'KEY_W 87)

(^constant 'KEY_WLAN 269025173)

(^constant 'KEY_WWAN 269025204)

(^constant 'KEY_WWW 269025070)

(^constant 'KEY_Wacute 16785026)

(^constant 'KEY_WakeUp 269025067)

(^constant 'KEY_Wcircumflex 16777588)

(^constant 'KEY_Wdiaeresis 16785028)

(^constant 'KEY_WebCam 269025167)

(^constant 'KEY_Wgrave 16785024)

(^constant 'KEY_WheelButton 269025160)

(^constant 'KEY_WindowClear 269025109)

(^constant 'KEY_WonSign 16785577)

(^constant 'KEY_Word 269025161)

(^constant 'KEY_X 88)

(^constant 'KEY_Xabovedot 16785034)

(^constant 'KEY_Xfer 269025162)

(^constant 'KEY_Y 89)

(^constant 'KEY_Yacute 221)

(^constant 'KEY_Ybelowdot 16785140)

(^constant 'KEY_Ycircumflex 16777590)

(^constant 'KEY_Ydiaeresis 5054)

(^constant 'KEY_Yellow 269025189)

(^constant 'KEY_Ygrave 16785138)

(^constant 'KEY_Yhook 16785142)

(^constant 'KEY_Ytilde 16785144)

(^constant 'KEY_Z 90)

(^constant 'KEY_Zabovedot 431)

(^constant 'KEY_Zacute 428)

(^constant 'KEY_Zcaron 430)

(^constant 'KEY_Zen_Koho 65341)

(^constant 'KEY_Zenkaku 65320)

(^constant 'KEY_Zenkaku_Hankaku 65322)

(^constant 'KEY_ZoomIn 269025163)

(^constant 'KEY_ZoomOut 269025164)

(^constant 'KEY_Zstroke 16777653)

(^constant 'KEY_a 97)

(^constant 'KEY_aacute 225)

(^constant 'KEY_abelowdot 16785057)

(^constant 'KEY_abovedot 511)

(^constant 'KEY_abreve 483)

(^constant 'KEY_abreveacute 16785071)

(^constant 'KEY_abrevebelowdot 16785079)

(^constant 'KEY_abrevegrave 16785073)

(^constant 'KEY_abrevehook 16785075)

(^constant 'KEY_abrevetilde 16785077)

(^constant 'KEY_acircumflex 226)

(^constant 'KEY_acircumflexacute 16785061)

(^constant 'KEY_acircumflexbelowdot 16785069)

(^constant 'KEY_acircumflexgrave 16785063)

(^constant 'KEY_acircumflexhook 16785065)

(^constant 'KEY_acircumflextilde 16785067)

(^constant 'KEY_acute 180)

(^constant 'KEY_adiaeresis 228)

(^constant 'KEY_ae 230)

(^constant 'KEY_agrave 224)

(^constant 'KEY_ahook 16785059)

(^constant 'KEY_amacron 992)

(^constant 'KEY_ampersand 38)

(^constant 'KEY_aogonek 433)

(^constant 'KEY_apostrophe 39)

(^constant 'KEY_approxeq 16785992)

(^constant 'KEY_approximate 2248)

(^constant 'KEY_aring 229)

(^constant 'KEY_asciicircum 94)

(^constant 'KEY_asciitilde 126)

(^constant 'KEY_asterisk 42)

(^constant 'KEY_at 64)

(^constant 'KEY_atilde 227)

(^constant 'KEY_b 98)

(^constant 'KEY_babovedot 16784899)

(^constant 'KEY_backslash 92)

(^constant 'KEY_ballotcross 2804)

(^constant 'KEY_bar 124)

(^constant 'KEY_because 16785973)

(^constant 'KEY_blank 2527)

(^constant 'KEY_botintegral 2213)

(^constant 'KEY_botleftparens 2220)

(^constant 'KEY_botleftsqbracket 2216)

(^constant 'KEY_botleftsummation 2226)

(^constant 'KEY_botrightparens 2222)

(^constant 'KEY_botrightsqbracket 2218)

(^constant 'KEY_botrightsummation 2230)

(^constant 'KEY_bott 2550)

(^constant 'KEY_botvertsummationconnector 2228)

(^constant 'KEY_braceleft 123)

(^constant 'KEY_braceright 125)

(^constant 'KEY_bracketleft 91)

(^constant 'KEY_bracketright 93)

(^constant 'KEY_braille_blank 16787456)

(^constant 'KEY_braille_dot_1 65521)

(^constant 'KEY_braille_dot_10 65530)

(^constant 'KEY_braille_dot_2 65522)

(^constant 'KEY_braille_dot_3 65523)

(^constant 'KEY_braille_dot_4 65524)

(^constant 'KEY_braille_dot_5 65525)

(^constant 'KEY_braille_dot_6 65526)

(^constant 'KEY_braille_dot_7 65527)

(^constant 'KEY_braille_dot_8 65528)

(^constant 'KEY_braille_dot_9 65529)

(^constant 'KEY_braille_dots_1 16787457)

(^constant 'KEY_braille_dots_12 16787459)

(^constant 'KEY_braille_dots_123 16787463)

(^constant 'KEY_braille_dots_1234 16787471)

(^constant 'KEY_braille_dots_12345 16787487)

(^constant 'KEY_braille_dots_123456 16787519)

(^constant 'KEY_braille_dots_1234567 16787583)

(^constant 'KEY_braille_dots_12345678 16787711)

(^constant 'KEY_braille_dots_1234568 16787647)

(^constant 'KEY_braille_dots_123457 16787551)

(^constant 'KEY_braille_dots_1234578 16787679)

(^constant 'KEY_braille_dots_123458 16787615)

(^constant 'KEY_braille_dots_12346 16787503)

(^constant 'KEY_braille_dots_123467 16787567)

(^constant 'KEY_braille_dots_1234678 16787695)

(^constant 'KEY_braille_dots_123468 16787631)

(^constant 'KEY_braille_dots_12347 16787535)

(^constant 'KEY_braille_dots_123478 16787663)

(^constant 'KEY_braille_dots_12348 16787599)

(^constant 'KEY_braille_dots_1235 16787479)

(^constant 'KEY_braille_dots_12356 16787511)

(^constant 'KEY_braille_dots_123567 16787575)

(^constant 'KEY_braille_dots_1235678 16787703)

(^constant 'KEY_braille_dots_123568 16787639)

(^constant 'KEY_braille_dots_12357 16787543)

(^constant 'KEY_braille_dots_123578 16787671)

(^constant 'KEY_braille_dots_12358 16787607)

(^constant 'KEY_braille_dots_1236 16787495)

(^constant 'KEY_braille_dots_12367 16787559)

(^constant 'KEY_braille_dots_123678 16787687)

(^constant 'KEY_braille_dots_12368 16787623)

(^constant 'KEY_braille_dots_1237 16787527)

(^constant 'KEY_braille_dots_12378 16787655)

(^constant 'KEY_braille_dots_1238 16787591)

(^constant 'KEY_braille_dots_124 16787467)

(^constant 'KEY_braille_dots_1245 16787483)

(^constant 'KEY_braille_dots_12456 16787515)

(^constant 'KEY_braille_dots_124567 16787579)

(^constant 'KEY_braille_dots_1245678 16787707)

(^constant 'KEY_braille_dots_124568 16787643)

(^constant 'KEY_braille_dots_12457 16787547)

(^constant 'KEY_braille_dots_124578 16787675)

(^constant 'KEY_braille_dots_12458 16787611)

(^constant 'KEY_braille_dots_1246 16787499)

(^constant 'KEY_braille_dots_12467 16787563)

(^constant 'KEY_braille_dots_124678 16787691)

(^constant 'KEY_braille_dots_12468 16787627)

(^constant 'KEY_braille_dots_1247 16787531)

(^constant 'KEY_braille_dots_12478 16787659)

(^constant 'KEY_braille_dots_1248 16787595)

(^constant 'KEY_braille_dots_125 16787475)

(^constant 'KEY_braille_dots_1256 16787507)

(^constant 'KEY_braille_dots_12567 16787571)

(^constant 'KEY_braille_dots_125678 16787699)

(^constant 'KEY_braille_dots_12568 16787635)

(^constant 'KEY_braille_dots_1257 16787539)

(^constant 'KEY_braille_dots_12578 16787667)

(^constant 'KEY_braille_dots_1258 16787603)

(^constant 'KEY_braille_dots_126 16787491)

(^constant 'KEY_braille_dots_1267 16787555)

(^constant 'KEY_braille_dots_12678 16787683)

(^constant 'KEY_braille_dots_1268 16787619)

(^constant 'KEY_braille_dots_127 16787523)

(^constant 'KEY_braille_dots_1278 16787651)

(^constant 'KEY_braille_dots_128 16787587)

(^constant 'KEY_braille_dots_13 16787461)

(^constant 'KEY_braille_dots_134 16787469)

(^constant 'KEY_braille_dots_1345 16787485)

(^constant 'KEY_braille_dots_13456 16787517)

(^constant 'KEY_braille_dots_134567 16787581)

(^constant 'KEY_braille_dots_1345678 16787709)

(^constant 'KEY_braille_dots_134568 16787645)

(^constant 'KEY_braille_dots_13457 16787549)

(^constant 'KEY_braille_dots_134578 16787677)

(^constant 'KEY_braille_dots_13458 16787613)

(^constant 'KEY_braille_dots_1346 16787501)

(^constant 'KEY_braille_dots_13467 16787565)

(^constant 'KEY_braille_dots_134678 16787693)

(^constant 'KEY_braille_dots_13468 16787629)

(^constant 'KEY_braille_dots_1347 16787533)

(^constant 'KEY_braille_dots_13478 16787661)

(^constant 'KEY_braille_dots_1348 16787597)

(^constant 'KEY_braille_dots_135 16787477)

(^constant 'KEY_braille_dots_1356 16787509)

(^constant 'KEY_braille_dots_13567 16787573)

(^constant 'KEY_braille_dots_135678 16787701)

(^constant 'KEY_braille_dots_13568 16787637)

(^constant 'KEY_braille_dots_1357 16787541)

(^constant 'KEY_braille_dots_13578 16787669)

(^constant 'KEY_braille_dots_1358 16787605)

(^constant 'KEY_braille_dots_136 16787493)

(^constant 'KEY_braille_dots_1367 16787557)

(^constant 'KEY_braille_dots_13678 16787685)

(^constant 'KEY_braille_dots_1368 16787621)

(^constant 'KEY_braille_dots_137 16787525)

(^constant 'KEY_braille_dots_1378 16787653)

(^constant 'KEY_braille_dots_138 16787589)

(^constant 'KEY_braille_dots_14 16787465)

(^constant 'KEY_braille_dots_145 16787481)

(^constant 'KEY_braille_dots_1456 16787513)

(^constant 'KEY_braille_dots_14567 16787577)

(^constant 'KEY_braille_dots_145678 16787705)

(^constant 'KEY_braille_dots_14568 16787641)

(^constant 'KEY_braille_dots_1457 16787545)

(^constant 'KEY_braille_dots_14578 16787673)

(^constant 'KEY_braille_dots_1458 16787609)

(^constant 'KEY_braille_dots_146 16787497)

(^constant 'KEY_braille_dots_1467 16787561)

(^constant 'KEY_braille_dots_14678 16787689)

(^constant 'KEY_braille_dots_1468 16787625)

(^constant 'KEY_braille_dots_147 16787529)

(^constant 'KEY_braille_dots_1478 16787657)

(^constant 'KEY_braille_dots_148 16787593)

(^constant 'KEY_braille_dots_15 16787473)

(^constant 'KEY_braille_dots_156 16787505)

(^constant 'KEY_braille_dots_1567 16787569)

(^constant 'KEY_braille_dots_15678 16787697)

(^constant 'KEY_braille_dots_1568 16787633)

(^constant 'KEY_braille_dots_157 16787537)

(^constant 'KEY_braille_dots_1578 16787665)

(^constant 'KEY_braille_dots_158 16787601)

(^constant 'KEY_braille_dots_16 16787489)

(^constant 'KEY_braille_dots_167 16787553)

(^constant 'KEY_braille_dots_1678 16787681)

(^constant 'KEY_braille_dots_168 16787617)

(^constant 'KEY_braille_dots_17 16787521)

(^constant 'KEY_braille_dots_178 16787649)

(^constant 'KEY_braille_dots_18 16787585)

(^constant 'KEY_braille_dots_2 16787458)

(^constant 'KEY_braille_dots_23 16787462)

(^constant 'KEY_braille_dots_234 16787470)

(^constant 'KEY_braille_dots_2345 16787486)

(^constant 'KEY_braille_dots_23456 16787518)

(^constant 'KEY_braille_dots_234567 16787582)

(^constant 'KEY_braille_dots_2345678 16787710)

(^constant 'KEY_braille_dots_234568 16787646)

(^constant 'KEY_braille_dots_23457 16787550)

(^constant 'KEY_braille_dots_234578 16787678)

(^constant 'KEY_braille_dots_23458 16787614)

(^constant 'KEY_braille_dots_2346 16787502)

(^constant 'KEY_braille_dots_23467 16787566)

(^constant 'KEY_braille_dots_234678 16787694)

(^constant 'KEY_braille_dots_23468 16787630)

(^constant 'KEY_braille_dots_2347 16787534)

(^constant 'KEY_braille_dots_23478 16787662)

(^constant 'KEY_braille_dots_2348 16787598)

(^constant 'KEY_braille_dots_235 16787478)

(^constant 'KEY_braille_dots_2356 16787510)

(^constant 'KEY_braille_dots_23567 16787574)

(^constant 'KEY_braille_dots_235678 16787702)

(^constant 'KEY_braille_dots_23568 16787638)

(^constant 'KEY_braille_dots_2357 16787542)

(^constant 'KEY_braille_dots_23578 16787670)

(^constant 'KEY_braille_dots_2358 16787606)

(^constant 'KEY_braille_dots_236 16787494)

(^constant 'KEY_braille_dots_2367 16787558)

(^constant 'KEY_braille_dots_23678 16787686)

(^constant 'KEY_braille_dots_2368 16787622)

(^constant 'KEY_braille_dots_237 16787526)

(^constant 'KEY_braille_dots_2378 16787654)

(^constant 'KEY_braille_dots_238 16787590)

(^constant 'KEY_braille_dots_24 16787466)

(^constant 'KEY_braille_dots_245 16787482)

(^constant 'KEY_braille_dots_2456 16787514)

(^constant 'KEY_braille_dots_24567 16787578)

(^constant 'KEY_braille_dots_245678 16787706)

(^constant 'KEY_braille_dots_24568 16787642)

(^constant 'KEY_braille_dots_2457 16787546)

(^constant 'KEY_braille_dots_24578 16787674)

(^constant 'KEY_braille_dots_2458 16787610)

(^constant 'KEY_braille_dots_246 16787498)

(^constant 'KEY_braille_dots_2467 16787562)

(^constant 'KEY_braille_dots_24678 16787690)

(^constant 'KEY_braille_dots_2468 16787626)

(^constant 'KEY_braille_dots_247 16787530)

(^constant 'KEY_braille_dots_2478 16787658)

(^constant 'KEY_braille_dots_248 16787594)

(^constant 'KEY_braille_dots_25 16787474)

(^constant 'KEY_braille_dots_256 16787506)

(^constant 'KEY_braille_dots_2567 16787570)

(^constant 'KEY_braille_dots_25678 16787698)

(^constant 'KEY_braille_dots_2568 16787634)

(^constant 'KEY_braille_dots_257 16787538)

(^constant 'KEY_braille_dots_2578 16787666)

(^constant 'KEY_braille_dots_258 16787602)

(^constant 'KEY_braille_dots_26 16787490)

(^constant 'KEY_braille_dots_267 16787554)

(^constant 'KEY_braille_dots_2678 16787682)

(^constant 'KEY_braille_dots_268 16787618)

(^constant 'KEY_braille_dots_27 16787522)

(^constant 'KEY_braille_dots_278 16787650)

(^constant 'KEY_braille_dots_28 16787586)

(^constant 'KEY_braille_dots_3 16787460)

(^constant 'KEY_braille_dots_34 16787468)

(^constant 'KEY_braille_dots_345 16787484)

(^constant 'KEY_braille_dots_3456 16787516)

(^constant 'KEY_braille_dots_34567 16787580)

(^constant 'KEY_braille_dots_345678 16787708)

(^constant 'KEY_braille_dots_34568 16787644)

(^constant 'KEY_braille_dots_3457 16787548)

(^constant 'KEY_braille_dots_34578 16787676)

(^constant 'KEY_braille_dots_3458 16787612)

(^constant 'KEY_braille_dots_346 16787500)

(^constant 'KEY_braille_dots_3467 16787564)

(^constant 'KEY_braille_dots_34678 16787692)

(^constant 'KEY_braille_dots_3468 16787628)

(^constant 'KEY_braille_dots_347 16787532)

(^constant 'KEY_braille_dots_3478 16787660)

(^constant 'KEY_braille_dots_348 16787596)

(^constant 'KEY_braille_dots_35 16787476)

(^constant 'KEY_braille_dots_356 16787508)

(^constant 'KEY_braille_dots_3567 16787572)

(^constant 'KEY_braille_dots_35678 16787700)

(^constant 'KEY_braille_dots_3568 16787636)

(^constant 'KEY_braille_dots_357 16787540)

(^constant 'KEY_braille_dots_3578 16787668)

(^constant 'KEY_braille_dots_358 16787604)

(^constant 'KEY_braille_dots_36 16787492)

(^constant 'KEY_braille_dots_367 16787556)

(^constant 'KEY_braille_dots_3678 16787684)

(^constant 'KEY_braille_dots_368 16787620)

(^constant 'KEY_braille_dots_37 16787524)

(^constant 'KEY_braille_dots_378 16787652)

(^constant 'KEY_braille_dots_38 16787588)

(^constant 'KEY_braille_dots_4 16787464)

(^constant 'KEY_braille_dots_45 16787480)

(^constant 'KEY_braille_dots_456 16787512)

(^constant 'KEY_braille_dots_4567 16787576)

(^constant 'KEY_braille_dots_45678 16787704)

(^constant 'KEY_braille_dots_4568 16787640)

(^constant 'KEY_braille_dots_457 16787544)

(^constant 'KEY_braille_dots_4578 16787672)

(^constant 'KEY_braille_dots_458 16787608)

(^constant 'KEY_braille_dots_46 16787496)

(^constant 'KEY_braille_dots_467 16787560)

(^constant 'KEY_braille_dots_4678 16787688)

(^constant 'KEY_braille_dots_468 16787624)

(^constant 'KEY_braille_dots_47 16787528)

(^constant 'KEY_braille_dots_478 16787656)

(^constant 'KEY_braille_dots_48 16787592)

(^constant 'KEY_braille_dots_5 16787472)

(^constant 'KEY_braille_dots_56 16787504)

(^constant 'KEY_braille_dots_567 16787568)

(^constant 'KEY_braille_dots_5678 16787696)

(^constant 'KEY_braille_dots_568 16787632)

(^constant 'KEY_braille_dots_57 16787536)

(^constant 'KEY_braille_dots_578 16787664)

(^constant 'KEY_braille_dots_58 16787600)

(^constant 'KEY_braille_dots_6 16787488)

(^constant 'KEY_braille_dots_67 16787552)

(^constant 'KEY_braille_dots_678 16787680)

(^constant 'KEY_braille_dots_68 16787616)

(^constant 'KEY_braille_dots_7 16787520)

(^constant 'KEY_braille_dots_78 16787648)

(^constant 'KEY_braille_dots_8 16787584)

(^constant 'KEY_breve 418)

(^constant 'KEY_brokenbar 166)

(^constant 'KEY_c 99)

(^constant 'KEY_c_h 65187)

(^constant 'KEY_cabovedot 741)

(^constant 'KEY_cacute 486)

(^constant 'KEY_careof 2744)

(^constant 'KEY_caret 2812)

(^constant 'KEY_caron 439)

(^constant 'KEY_ccaron 488)

(^constant 'KEY_ccedilla 231)

(^constant 'KEY_ccircumflex 742)

(^constant 'KEY_cedilla 184)

(^constant 'KEY_cent 162)

(^constant 'KEY_ch 65184)

(^constant 'KEY_checkerboard 2529)

(^constant 'KEY_checkmark 2803)

(^constant 'KEY_circle 3023)

(^constant 'KEY_club 2796)

(^constant 'KEY_colon 58)

(^constant 'KEY_comma 44)

(^constant 'KEY_containsas 16785931)

(^constant 'KEY_copyright 169)

(^constant 'KEY_cr 2532)

(^constant 'KEY_crossinglines 2542)

(^constant 'KEY_cuberoot 16785947)

(^constant 'KEY_currency 164)

(^constant 'KEY_cursor 2815)

(^constant 'KEY_d 100)

(^constant 'KEY_dabovedot 16784907)

(^constant 'KEY_dagger 2801)

(^constant 'KEY_dcaron 495)

(^constant 'KEY_dead_A 65153)

(^constant 'KEY_dead_E 65155)

(^constant 'KEY_dead_I 65157)

(^constant 'KEY_dead_O 65159)

(^constant 'KEY_dead_U 65161)

(^constant 'KEY_dead_a 65152)

(^constant 'KEY_dead_abovecomma 65124)

(^constant 'KEY_dead_abovedot 65110)

(^constant 'KEY_dead_abovereversedcomma 65125)

(^constant 'KEY_dead_abovering 65112)

(^constant 'KEY_dead_aboveverticalline 65169)

(^constant 'KEY_dead_acute 65105)

(^constant 'KEY_dead_belowbreve 65131)

(^constant 'KEY_dead_belowcircumflex 65129)

(^constant 'KEY_dead_belowcomma 65134)

(^constant 'KEY_dead_belowdiaeresis 65132)

(^constant 'KEY_dead_belowdot 65120)

(^constant 'KEY_dead_belowmacron 65128)

(^constant 'KEY_dead_belowring 65127)

(^constant 'KEY_dead_belowtilde 65130)

(^constant 'KEY_dead_belowverticalline 65170)

(^constant 'KEY_dead_breve 65109)

(^constant 'KEY_dead_capital_schwa 65163)

(^constant 'KEY_dead_caron 65114)

(^constant 'KEY_dead_cedilla 65115)

(^constant 'KEY_dead_circumflex 65106)

(^constant 'KEY_dead_currency 65135)

(^constant 'KEY_dead_dasia 65125)

(^constant 'KEY_dead_diaeresis 65111)

(^constant 'KEY_dead_doubleacute 65113)

(^constant 'KEY_dead_doublegrave 65126)

(^constant 'KEY_dead_e 65154)

(^constant 'KEY_dead_grave 65104)

(^constant 'KEY_dead_greek 65164)

(^constant 'KEY_dead_hook 65121)

(^constant 'KEY_dead_horn 65122)

(^constant 'KEY_dead_i 65156)

(^constant 'KEY_dead_invertedbreve 65133)

(^constant 'KEY_dead_iota 65117)

(^constant 'KEY_dead_longsolidusoverlay 65171)

(^constant 'KEY_dead_lowline 65168)

(^constant 'KEY_dead_macron 65108)

(^constant 'KEY_dead_o 65158)

(^constant 'KEY_dead_ogonek 65116)

(^constant 'KEY_dead_perispomeni 65107)

(^constant 'KEY_dead_psili 65124)

(^constant 'KEY_dead_semivoiced_sound 65119)

(^constant 'KEY_dead_small_schwa 65162)

(^constant 'KEY_dead_stroke 65123)

(^constant 'KEY_dead_tilde 65107)

(^constant 'KEY_dead_u 65160)

(^constant 'KEY_dead_voiced_sound 65118)

(^constant 'KEY_decimalpoint 2749)

(^constant 'KEY_degree 176)

(^constant 'KEY_diaeresis 168)

(^constant 'KEY_diamond 2797)

(^constant 'KEY_digitspace 2725)

(^constant 'KEY_dintegral 16785964)

(^constant 'KEY_division 247)

(^constant 'KEY_dollar 36)

(^constant 'KEY_doubbaselinedot 2735)

(^constant 'KEY_doubleacute 445)

(^constant 'KEY_doubledagger 2802)

(^constant 'KEY_doublelowquotemark 2814)

(^constant 'KEY_downarrow 2302)

(^constant 'KEY_downcaret 2984)

(^constant 'KEY_downshoe 3030)

(^constant 'KEY_downstile 3012)

(^constant 'KEY_downtack 3010)

(^constant 'KEY_dstroke 496)

(^constant 'KEY_e 101)

(^constant 'KEY_eabovedot 1004)

(^constant 'KEY_eacute 233)

(^constant 'KEY_ebelowdot 16785081)

(^constant 'KEY_ecaron 492)

(^constant 'KEY_ecircumflex 234)

(^constant 'KEY_ecircumflexacute 16785087)

(^constant 'KEY_ecircumflexbelowdot 16785095)

(^constant 'KEY_ecircumflexgrave 16785089)

(^constant 'KEY_ecircumflexhook 16785091)

(^constant 'KEY_ecircumflextilde 16785093)

(^constant 'KEY_ediaeresis 235)

(^constant 'KEY_egrave 232)

(^constant 'KEY_ehook 16785083)

(^constant 'KEY_eightsubscript 16785544)

(^constant 'KEY_eightsuperior 16785528)

(^constant 'KEY_elementof 16785928)

(^constant 'KEY_ellipsis 2734)

(^constant 'KEY_em3space 2723)

(^constant 'KEY_em4space 2724)

(^constant 'KEY_emacron 954)

(^constant 'KEY_emdash 2729)

(^constant 'KEY_emfilledcircle 2782)

(^constant 'KEY_emfilledrect 2783)

(^constant 'KEY_emopencircle 2766)

(^constant 'KEY_emopenrectangle 2767)

(^constant 'KEY_emptyset 16785925)

(^constant 'KEY_emspace 2721)

(^constant 'KEY_endash 2730)

(^constant 'KEY_enfilledcircbullet 2790)

(^constant 'KEY_enfilledsqbullet 2791)

(^constant 'KEY_eng 959)

(^constant 'KEY_enopencircbullet 2784)

(^constant 'KEY_enopensquarebullet 2785)

(^constant 'KEY_enspace 2722)

(^constant 'KEY_eogonek 490)

(^constant 'KEY_equal 61)

(^constant 'KEY_eth 240)

(^constant 'KEY_etilde 16785085)

(^constant 'KEY_exclam 33)

(^constant 'KEY_exclamdown 161)

(^constant 'KEY_ezh 16777874)

(^constant 'KEY_f 102)

(^constant 'KEY_fabovedot 16784927)

(^constant 'KEY_femalesymbol 2808)

(^constant 'KEY_ff 2531)

(^constant 'KEY_figdash 2747)

(^constant 'KEY_filledlefttribullet 2780)

(^constant 'KEY_filledrectbullet 2779)

(^constant 'KEY_filledrighttribullet 2781)

(^constant 'KEY_filledtribulletdown 2793)

(^constant 'KEY_filledtribulletup 2792)

(^constant 'KEY_fiveeighths 2757)

(^constant 'KEY_fivesixths 2743)

(^constant 'KEY_fivesubscript 16785541)

(^constant 'KEY_fivesuperior 16785525)

(^constant 'KEY_fourfifths 2741)

(^constant 'KEY_foursubscript 16785540)

(^constant 'KEY_foursuperior 16785524)

(^constant 'KEY_fourthroot 16785948)

(^constant 'KEY_function 2294)

(^constant 'KEY_g 103)

(^constant 'KEY_gabovedot 757)

(^constant 'KEY_gbreve 699)

(^constant 'KEY_gcaron 16777703)

(^constant 'KEY_gcedilla 955)

(^constant 'KEY_gcircumflex 760)

(^constant 'KEY_grave 96)

(^constant 'KEY_greater 62)

(^constant 'KEY_greaterthanequal 2238)

(^constant 'KEY_guillemotleft 171)

(^constant 'KEY_guillemotright 187)

(^constant 'KEY_h 104)

(^constant 'KEY_hairspace 2728)

(^constant 'KEY_hcircumflex 694)

(^constant 'KEY_heart 2798)

(^constant 'KEY_hebrew_aleph 3296)

(^constant 'KEY_hebrew_ayin 3314)

(^constant 'KEY_hebrew_bet 3297)

(^constant 'KEY_hebrew_beth 3297)

(^constant 'KEY_hebrew_chet 3303)

(^constant 'KEY_hebrew_dalet 3299)

(^constant 'KEY_hebrew_daleth 3299)

(^constant 'KEY_hebrew_doublelowline 3295)

(^constant 'KEY_hebrew_finalkaph 3306)

(^constant 'KEY_hebrew_finalmem 3309)

(^constant 'KEY_hebrew_finalnun 3311)

(^constant 'KEY_hebrew_finalpe 3315)

(^constant 'KEY_hebrew_finalzade 3317)

(^constant 'KEY_hebrew_finalzadi 3317)

(^constant 'KEY_hebrew_gimel 3298)

(^constant 'KEY_hebrew_gimmel 3298)

(^constant 'KEY_hebrew_he 3300)

(^constant 'KEY_hebrew_het 3303)

(^constant 'KEY_hebrew_kaph 3307)

(^constant 'KEY_hebrew_kuf 3319)

(^constant 'KEY_hebrew_lamed 3308)

(^constant 'KEY_hebrew_mem 3310)

(^constant 'KEY_hebrew_nun 3312)

(^constant 'KEY_hebrew_pe 3316)

(^constant 'KEY_hebrew_qoph 3319)

(^constant 'KEY_hebrew_resh 3320)

(^constant 'KEY_hebrew_samech 3313)

(^constant 'KEY_hebrew_samekh 3313)

(^constant 'KEY_hebrew_shin 3321)

(^constant 'KEY_hebrew_taf 3322)

(^constant 'KEY_hebrew_taw 3322)

(^constant 'KEY_hebrew_tet 3304)

(^constant 'KEY_hebrew_teth 3304)

(^constant 'KEY_hebrew_waw 3301)

(^constant 'KEY_hebrew_yod 3305)

(^constant 'KEY_hebrew_zade 3318)

(^constant 'KEY_hebrew_zadi 3318)

(^constant 'KEY_hebrew_zain 3302)

(^constant 'KEY_hebrew_zayin 3302)

(^constant 'KEY_hexagram 2778)

(^constant 'KEY_horizconnector 2211)

(^constant 'KEY_horizlinescan1 2543)

(^constant 'KEY_horizlinescan3 2544)

(^constant 'KEY_horizlinescan5 2545)

(^constant 'KEY_horizlinescan7 2546)

(^constant 'KEY_horizlinescan9 2547)

(^constant 'KEY_hstroke 689)

(^constant 'KEY_ht 2530)

(^constant 'KEY_hyphen 173)

(^constant 'KEY_i 105)

(^constant 'KEY_iTouch 269025120)

(^constant 'KEY_iacute 237)

(^constant 'KEY_ibelowdot 16785099)

(^constant 'KEY_ibreve 16777517)

(^constant 'KEY_icircumflex 238)

(^constant 'KEY_identical 2255)

(^constant 'KEY_idiaeresis 239)

(^constant 'KEY_idotless 697)

(^constant 'KEY_ifonlyif 2253)

(^constant 'KEY_igrave 236)

(^constant 'KEY_ihook 16785097)

(^constant 'KEY_imacron 1007)

(^constant 'KEY_implies 2254)

(^constant 'KEY_includedin 2266)

(^constant 'KEY_includes 2267)

(^constant 'KEY_infinity 2242)

(^constant 'KEY_integral 2239)

(^constant 'KEY_intersection 2268)

(^constant 'KEY_iogonek 999)

(^constant 'KEY_itilde 949)

(^constant 'KEY_j 106)

(^constant 'KEY_jcircumflex 700)

(^constant 'KEY_jot 3018)

(^constant 'KEY_k 107)

(^constant 'KEY_kana_A 1201)

(^constant 'KEY_kana_CHI 1217)

(^constant 'KEY_kana_E 1204)

(^constant 'KEY_kana_FU 1228)

(^constant 'KEY_kana_HA 1226)

(^constant 'KEY_kana_HE 1229)

(^constant 'KEY_kana_HI 1227)

(^constant 'KEY_kana_HO 1230)

(^constant 'KEY_kana_HU 1228)

(^constant 'KEY_kana_I 1202)

(^constant 'KEY_kana_KA 1206)

(^constant 'KEY_kana_KE 1209)

(^constant 'KEY_kana_KI 1207)

(^constant 'KEY_kana_KO 1210)

(^constant 'KEY_kana_KU 1208)

(^constant 'KEY_kana_MA 1231)

(^constant 'KEY_kana_ME 1234)

(^constant 'KEY_kana_MI 1232)

(^constant 'KEY_kana_MO 1235)

(^constant 'KEY_kana_MU 1233)

(^constant 'KEY_kana_N 1245)

(^constant 'KEY_kana_NA 1221)

(^constant 'KEY_kana_NE 1224)

(^constant 'KEY_kana_NI 1222)

(^constant 'KEY_kana_NO 1225)

(^constant 'KEY_kana_NU 1223)

(^constant 'KEY_kana_O 1205)

(^constant 'KEY_kana_RA 1239)

(^constant 'KEY_kana_RE 1242)

(^constant 'KEY_kana_RI 1240)

(^constant 'KEY_kana_RO 1243)

(^constant 'KEY_kana_RU 1241)

(^constant 'KEY_kana_SA 1211)

(^constant 'KEY_kana_SE 1214)

(^constant 'KEY_kana_SHI 1212)

(^constant 'KEY_kana_SO 1215)

(^constant 'KEY_kana_SU 1213)

(^constant 'KEY_kana_TA 1216)

(^constant 'KEY_kana_TE 1219)

(^constant 'KEY_kana_TI 1217)

(^constant 'KEY_kana_TO 1220)

(^constant 'KEY_kana_TSU 1218)

(^constant 'KEY_kana_TU 1218)

(^constant 'KEY_kana_U 1203)

(^constant 'KEY_kana_WA 1244)

(^constant 'KEY_kana_WO 1190)

(^constant 'KEY_kana_YA 1236)

(^constant 'KEY_kana_YO 1238)

(^constant 'KEY_kana_YU 1237)

(^constant 'KEY_kana_a 1191)

(^constant 'KEY_kana_closingbracket 1187)

(^constant 'KEY_kana_comma 1188)

(^constant 'KEY_kana_conjunctive 1189)

(^constant 'KEY_kana_e 1194)

(^constant 'KEY_kana_fullstop 1185)

(^constant 'KEY_kana_i 1192)

(^constant 'KEY_kana_middledot 1189)

(^constant 'KEY_kana_o 1195)

(^constant 'KEY_kana_openingbracket 1186)

(^constant 'KEY_kana_switch 65406)

(^constant 'KEY_kana_tsu 1199)

(^constant 'KEY_kana_tu 1199)

(^constant 'KEY_kana_u 1193)

(^constant 'KEY_kana_ya 1196)

(^constant 'KEY_kana_yo 1198)

(^constant 'KEY_kana_yu 1197)

(^constant 'KEY_kappa 930)

(^constant 'KEY_kcedilla 1011)

(^constant 'KEY_kra 930)

(^constant 'KEY_l 108)

(^constant 'KEY_lacute 485)

(^constant 'KEY_latincross 2777)

(^constant 'KEY_lbelowdot 16784951)

(^constant 'KEY_lcaron 437)

(^constant 'KEY_lcedilla 950)

(^constant 'KEY_leftanglebracket 2748)

(^constant 'KEY_leftarrow 2299)

(^constant 'KEY_leftcaret 2979)

(^constant 'KEY_leftdoublequotemark 2770)

(^constant 'KEY_leftmiddlecurlybrace 2223)

(^constant 'KEY_leftopentriangle 2764)

(^constant 'KEY_leftpointer 2794)

(^constant 'KEY_leftradical 2209)

(^constant 'KEY_leftshoe 3034)

(^constant 'KEY_leftsinglequotemark 2768)

(^constant 'KEY_leftt 2548)

(^constant 'KEY_lefttack 3036)

(^constant 'KEY_less 60)

(^constant 'KEY_lessthanequal 2236)

(^constant 'KEY_lf 2533)

(^constant 'KEY_logicaland 2270)

(^constant 'KEY_logicalor 2271)

(^constant 'KEY_lowleftcorner 2541)

(^constant 'KEY_lowrightcorner 2538)

(^constant 'KEY_lstroke 435)

(^constant 'KEY_m 109)

(^constant 'KEY_mabovedot 16784961)

(^constant 'KEY_macron 175)

(^constant 'KEY_malesymbol 2807)

(^constant 'KEY_maltesecross 2800)

(^constant 'KEY_marker 2751)

(^constant 'KEY_masculine 186)

(^constant 'KEY_minus 45)

(^constant 'KEY_minutes 2774)

(^constant 'KEY_mu 181)

(^constant 'KEY_multiply 215)

(^constant 'KEY_musicalflat 2806)

(^constant 'KEY_musicalsharp 2805)

(^constant 'KEY_n 110)

(^constant 'KEY_nabla 2245)

(^constant 'KEY_nacute 497)

(^constant 'KEY_ncaron 498)

(^constant 'KEY_ncedilla 1009)

(^constant 'KEY_ninesubscript 16785545)

(^constant 'KEY_ninesuperior 16785529)

(^constant 'KEY_nl 2536)

(^constant 'KEY_nobreakspace 160)

(^constant 'KEY_notapproxeq 16785991)

(^constant 'KEY_notelementof 16785929)

(^constant 'KEY_notequal 2237)

(^constant 'KEY_notidentical 16786018)

(^constant 'KEY_notsign 172)

(^constant 'KEY_ntilde 241)

(^constant 'KEY_numbersign 35)

(^constant 'KEY_numerosign 1712)

(^constant 'KEY_o 111)

(^constant 'KEY_oacute 243)

(^constant 'KEY_obarred 16777845)

(^constant 'KEY_obelowdot 16785101)

(^constant 'KEY_ocaron 16777682)

(^constant 'KEY_ocircumflex 244)

(^constant 'KEY_ocircumflexacute 16785105)

(^constant 'KEY_ocircumflexbelowdot 16785113)

(^constant 'KEY_ocircumflexgrave 16785107)

(^constant 'KEY_ocircumflexhook 16785109)

(^constant 'KEY_ocircumflextilde 16785111)

(^constant 'KEY_odiaeresis 246)

(^constant 'KEY_odoubleacute 501)

(^constant 'KEY_oe 5053)

(^constant 'KEY_ogonek 434)

(^constant 'KEY_ograve 242)

(^constant 'KEY_ohook 16785103)

(^constant 'KEY_ohorn 16777633)

(^constant 'KEY_ohornacute 16785115)

(^constant 'KEY_ohornbelowdot 16785123)

(^constant 'KEY_ohorngrave 16785117)

(^constant 'KEY_ohornhook 16785119)

(^constant 'KEY_ohorntilde 16785121)

(^constant 'KEY_omacron 1010)

(^constant 'KEY_oneeighth 2755)

(^constant 'KEY_onefifth 2738)

(^constant 'KEY_onehalf 189)

(^constant 'KEY_onequarter 188)

(^constant 'KEY_onesixth 2742)

(^constant 'KEY_onesubscript 16785537)

(^constant 'KEY_onesuperior 185)

(^constant 'KEY_onethird 2736)

(^constant 'KEY_ooblique 248)

(^constant 'KEY_openrectbullet 2786)

(^constant 'KEY_openstar 2789)

(^constant 'KEY_opentribulletdown 2788)

(^constant 'KEY_opentribulletup 2787)

(^constant 'KEY_ordfeminine 170)

(^constant 'KEY_oslash 248)

(^constant 'KEY_otilde 245)

(^constant 'KEY_overbar 3008)

(^constant 'KEY_overline 1150)

(^constant 'KEY_p 112)

(^constant 'KEY_pabovedot 16784983)

(^constant 'KEY_paragraph 182)

(^constant 'KEY_parenleft 40)

(^constant 'KEY_parenright 41)

(^constant 'KEY_partdifferential 16785922)

(^constant 'KEY_partialderivative 2287)

(^constant 'KEY_percent 37)

(^constant 'KEY_period 46)

(^constant 'KEY_periodcentered 183)

(^constant 'KEY_permille 2773)

(^constant 'KEY_phonographcopyright 2811)

(^constant 'KEY_plus 43)

(^constant 'KEY_plusminus 177)

(^constant 'KEY_prescription 2772)

(^constant 'KEY_prolongedsound 1200)

(^constant 'KEY_punctspace 2726)

(^constant 'KEY_q 113)

(^constant 'KEY_quad 3020)

(^constant 'KEY_question 63)

(^constant 'KEY_questiondown 191)

(^constant 'KEY_quotedbl 34)

(^constant 'KEY_quoteleft 96)

(^constant 'KEY_quoteright 39)

(^constant 'KEY_r 114)

(^constant 'KEY_racute 480)

(^constant 'KEY_radical 2262)

(^constant 'KEY_rcaron 504)

(^constant 'KEY_rcedilla 947)

(^constant 'KEY_registered 174)

(^constant 'KEY_rightanglebracket 2750)

(^constant 'KEY_rightarrow 2301)

(^constant 'KEY_rightcaret 2982)

(^constant 'KEY_rightdoublequotemark 2771)

(^constant 'KEY_rightmiddlecurlybrace 2224)

(^constant 'KEY_rightmiddlesummation 2231)

(^constant 'KEY_rightopentriangle 2765)

(^constant 'KEY_rightpointer 2795)

(^constant 'KEY_rightshoe 3032)

(^constant 'KEY_rightsinglequotemark 2769)

(^constant 'KEY_rightt 2549)

(^constant 'KEY_righttack 3068)

(^constant 'KEY_s 115)

(^constant 'KEY_sabovedot 16784993)

(^constant 'KEY_sacute 438)

(^constant 'KEY_scaron 441)

(^constant 'KEY_scedilla 442)

(^constant 'KEY_schwa 16777817)

(^constant 'KEY_scircumflex 766)

(^constant 'KEY_script_switch 65406)

(^constant 'KEY_seconds 2775)

(^constant 'KEY_section 167)

(^constant 'KEY_semicolon 59)

(^constant 'KEY_semivoicedsound 1247)

(^constant 'KEY_seveneighths 2758)

(^constant 'KEY_sevensubscript 16785543)

(^constant 'KEY_sevensuperior 16785527)

(^constant 'KEY_signaturemark 2762)

(^constant 'KEY_signifblank 2732)

(^constant 'KEY_similarequal 2249)

(^constant 'KEY_singlelowquotemark 2813)

(^constant 'KEY_sixsubscript 16785542)

(^constant 'KEY_sixsuperior 16785526)

(^constant 'KEY_slash 47)

(^constant 'KEY_soliddiamond 2528)

(^constant 'KEY_space 32)

(^constant 'KEY_squareroot 16785946)

(^constant 'KEY_ssharp 223)

(^constant 'KEY_sterling 163)

(^constant 'KEY_stricteq 16786019)

(^constant 'KEY_t 116)

(^constant 'KEY_tabovedot 16785003)

(^constant 'KEY_tcaron 443)

(^constant 'KEY_tcedilla 510)

(^constant 'KEY_telephone 2809)

(^constant 'KEY_telephonerecorder 2810)

(^constant 'KEY_therefore 2240)

(^constant 'KEY_thinspace 2727)

(^constant 'KEY_thorn 254)

(^constant 'KEY_threeeighths 2756)

(^constant 'KEY_threefifths 2740)

(^constant 'KEY_threequarters 190)

(^constant 'KEY_threesubscript 16785539)

(^constant 'KEY_threesuperior 179)

(^constant 'KEY_tintegral 16785965)

(^constant 'KEY_topintegral 2212)

(^constant 'KEY_topleftparens 2219)

(^constant 'KEY_topleftradical 2210)

(^constant 'KEY_topleftsqbracket 2215)

(^constant 'KEY_topleftsummation 2225)

(^constant 'KEY_toprightparens 2221)

(^constant 'KEY_toprightsqbracket 2217)

(^constant 'KEY_toprightsummation 2229)

(^constant 'KEY_topt 2551)

(^constant 'KEY_topvertsummationconnector 2227)

(^constant 'KEY_trademark 2761)

(^constant 'KEY_trademarkincircle 2763)

(^constant 'KEY_tslash 956)

(^constant 'KEY_twofifths 2739)

(^constant 'KEY_twosubscript 16785538)

(^constant 'KEY_twosuperior 178)

(^constant 'KEY_twothirds 2737)

(^constant 'KEY_u 117)

(^constant 'KEY_uacute 250)

(^constant 'KEY_ubelowdot 16785125)

(^constant 'KEY_ubreve 765)

(^constant 'KEY_ucircumflex 251)

(^constant 'KEY_udiaeresis 252)

(^constant 'KEY_udoubleacute 507)

(^constant 'KEY_ugrave 249)

(^constant 'KEY_uhook 16785127)

(^constant 'KEY_uhorn 16777648)

(^constant 'KEY_uhornacute 16785129)

(^constant 'KEY_uhornbelowdot 16785137)

(^constant 'KEY_uhorngrave 16785131)

(^constant 'KEY_uhornhook 16785133)

(^constant 'KEY_uhorntilde 16785135)

(^constant 'KEY_umacron 1022)

(^constant 'KEY_underbar 3014)

(^constant 'KEY_underscore 95)

(^constant 'KEY_union 2269)

(^constant 'KEY_uogonek 1017)

(^constant 'KEY_uparrow 2300)

(^constant 'KEY_upcaret 2985)

(^constant 'KEY_upleftcorner 2540)

(^constant 'KEY_uprightcorner 2539)

(^constant 'KEY_upshoe 3011)

(^constant 'KEY_upstile 3027)

(^constant 'KEY_uptack 3022)

(^constant 'KEY_uring 505)

(^constant 'KEY_utilde 1021)

(^constant 'KEY_v 118)

(^constant 'KEY_variation 2241)

(^constant 'KEY_vertbar 2552)

(^constant 'KEY_vertconnector 2214)

(^constant 'KEY_voicedsound 1246)

(^constant 'KEY_vt 2537)

(^constant 'KEY_w 119)

(^constant 'KEY_wacute 16785027)

(^constant 'KEY_wcircumflex 16777589)

(^constant 'KEY_wdiaeresis 16785029)

(^constant 'KEY_wgrave 16785025)

(^constant 'KEY_x 120)

(^constant 'KEY_xabovedot 16785035)

(^constant 'KEY_y 121)

(^constant 'KEY_yacute 253)

(^constant 'KEY_ybelowdot 16785141)

(^constant 'KEY_ycircumflex 16777591)

(^constant 'KEY_ydiaeresis 255)

(^constant 'KEY_yen 165)

(^constant 'KEY_ygrave 16785139)

(^constant 'KEY_yhook 16785143)

(^constant 'KEY_ytilde 16785145)

(^constant 'KEY_z 122)

(^constant 'KEY_zabovedot 447)

(^constant 'KEY_zacute 444)

(^constant 'KEY_zcaron 446)

(^constant 'KEY_zerosubscript 16785536)

(^constant 'KEY_zerosuperior 16785520)

(^constant 'KEY_zstroke 16777654)

(^type 'Gdk
       '<GdkKeyEvent>
       "GdkKeyEvent"
       "gdk_key_event_get_type")

(^function
  'Gdk
  'GdkKeyEvent
  'key-event:get-consumed-modifiers
  'get-consumed-modifiers
  'gdk_key_event_get_consumed_modifiers
  '((name . "get_consumed_modifiers")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkModifierType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkKeyEvent
  'key-event:get-keycode
  'get-keycode
  'gdk_key_event_get_keycode
  '((name . "get_keycode")
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
  'Gdk
  'GdkKeyEvent
  'key-event:get-keyval
  'get-keyval
  'gdk_key_event_get_keyval
  '((name . "get_keyval")
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
  'Gdk
  'GdkKeyEvent
  'key-event:get-layout
  'get-layout
  'gdk_key_event_get_layout
  '((name . "get_layout")
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
  'Gdk
  'GdkKeyEvent
  'key-event:get-level
  'get-level
  'gdk_key_event_get_level
  '((name . "get_level")
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
  'Gdk
  'GdkKeyEvent
  'key-event:get-match
  'get-match
  'gdk_key_event_get_match
  '((name . "get_match")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "keyval")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "modifiers")
            (meta (arg-type . flags)
                  (gtype-name . "GdkModifierType")
                  (flags out)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
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
  'Gdk
  'GdkKeyEvent
  'key-event:is-modifier?
  'is-modifier?
  'gdk_key_event_is_modifier
  '((name . "is_modifier")
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

(^type 'Gdk
       '<GdkKeyMatch>
       "GdkKeyMatch"
       "gdk_key_match_get_type")

(^enum-conv 'key-match 'GdkKeyMatch)

(^function
  'Gdk
  'GdkKeyEvent
  'key-event:matches
  'matches
  'gdk_key_event_matches
  '((name . "matches")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "keyval")
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
           ((name . "modifiers")
            (meta (arg-type . flags)
                  (gtype-name . "GdkModifierType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkKeyMatch")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^constant 'MODIFIER_MASK 469769999)

(^type 'Gdk
       '<GdkMemoryFormat>
       "GdkMemoryFormat"
       "gdk_memory_format_get_type")

(^enum-conv 'memory-format 'GdkMemoryFormat)

(^type 'Gdk
       '<GdkMemoryTexture>
       "GdkMemoryTexture"
       "gdk_memory_texture_get_type")

(^function
  'Gdk
  'GdkMemoryTexture
  'memory-texture:new
  'new
  'gdk_memory_texture_new
  '((name . "new")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "height")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "format")
            (meta (arg-type . enum)
                  (gtype-name . "GdkMemoryFormat")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "bytes")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "stride")
            (meta (arg-type . uint64)
                  (flags in)
                  (item-size . 8)
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
            (gtype-name . "GdkMemoryTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkMonitor
  'monitor:get-connector
  'get-connector
  'gdk_monitor_get_connector
  '((name . "get_connector")
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
  'Gdk
  'GdkMonitor
  'monitor:get-description
  'get-description
  'gdk_monitor_get_description
  '((name . "get_description")
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
  'Gdk
  'GdkMonitor
  'monitor:get-display
  'get-display
  'gdk_monitor_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Gdk
  '<GdkRectangle>
  "GdkRectangle"
  "gdk_rectangle_get_type"
  16)

(^function
  'Gdk
  'GdkMonitor
  'monitor:get-geometry!
  'get-geometry!
  'gdk_monitor_get_geometry
  '((name . "get_geometry")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "geometry")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkMonitor
  'monitor:get-height-mm
  'get-height-mm
  'gdk_monitor_get_height_mm
  '((name . "get_height_mm")
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
  'Gdk
  'GdkMonitor
  'monitor:get-manufacturer
  'get-manufacturer
  'gdk_monitor_get_manufacturer
  '((name . "get_manufacturer")
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
  'Gdk
  'GdkMonitor
  'monitor:get-model
  'get-model
  'gdk_monitor_get_model
  '((name . "get_model")
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
  'Gdk
  'GdkMonitor
  'monitor:get-refresh-rate
  'get-refresh-rate
  'gdk_monitor_get_refresh_rate
  '((name . "get_refresh_rate")
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
  'Gdk
  'GdkMonitor
  'monitor:get-scale-factor
  'get-scale-factor
  'gdk_monitor_get_scale_factor
  '((name . "get_scale_factor")
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

(^type 'Gdk
       '<GdkSubpixelLayout>
       "GdkSubpixelLayout"
       "gdk_subpixel_layout_get_type")

(^enum-conv 'subpixel-layout 'GdkSubpixelLayout)

(^function
  'Gdk
  'GdkMonitor
  'monitor:get-subpixel-layout
  'get-subpixel-layout
  'gdk_monitor_get_subpixel_layout
  '((name . "get_subpixel_layout")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkSubpixelLayout")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkMonitor
  'monitor:get-width-mm
  'get-width-mm
  'gdk_monitor_get_width_mm
  '((name . "get_width_mm")
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
  'Gdk
  'GdkMonitor
  'monitor:is-valid?
  'is-valid?
  'gdk_monitor_is_valid
  '((name . "is_valid")
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

(^property
  'GdkMonitor
  'monitor:connector
  'connector
  'connector)

(^property
  'GdkMonitor
  'monitor:description
  'description
  'description)

(^property
  'GdkMonitor
  'monitor:display
  'display
  'display)

(^property
  'GdkMonitor
  'monitor:geometry
  'geometry
  'geometry)

(^property
  'GdkMonitor
  'monitor:height-mm
  'height-mm
  'height-mm)

(^property
  'GdkMonitor
  'monitor:manufacturer
  'manufacturer
  'manufacturer)

(^property
  'GdkMonitor
  'monitor:model
  'model
  'model)

(^property
  'GdkMonitor
  'monitor:refresh-rate
  'refresh-rate
  'refresh-rate)

(^property
  'GdkMonitor
  'monitor:scale-factor
  'scale-factor
  'scale-factor)

(^property
  'GdkMonitor
  'monitor:subpixel-layout
  'subpixel-layout
  'subpixel-layout)

(^property
  'GdkMonitor
  'monitor:valid
  'valid
  'valid)

(^property
  'GdkMonitor
  'monitor:width-mm
  'width-mm
  'width-mm)

(^signal
  'Gdk
  'GdkMonitor
  'monitor:invalidate
  'invalidate
  'invalidate
  '((name . "invalidate")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkMotionEvent>
       "GdkMotionEvent"
       "gdk_motion_event_get_type")

(^constant 'PRIORITY_REDRAW 120)

(^type 'Gdk
       '<GdkPadEvent>
       "GdkPadEvent"
       "gdk_pad_event_get_type")

(^function
  'Gdk
  'GdkPadEvent
  'pad-event:get-axis-value
  'get-axis-value
  'gdk_pad_event_get_axis_value
  '((name . "get_axis_value")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "index")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "value")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPadEvent
  'pad-event:get-button
  'get-button
  'gdk_pad_event_get_button
  '((name . "get_button")
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
  'Gdk
  'GdkPadEvent
  'pad-event:get-group-mode
  'get-group-mode
  'gdk_pad_event_get_group_mode
  '((name . "get_group_mode")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "group")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "mode")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPaintable
  'paintable:new-empty
  'new-empty
  'gdk_paintable_new_empty
  '((name . "new_empty")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "intrinsic_width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "intrinsic_height")
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
      (meta (arg-type . GInterface)
            (gtype-name . "GdkPaintable")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPaintable
  'paintable:compute-concrete-size
  'compute-concrete-size
  'gdk_paintable_compute_concrete_size
  '((name . "compute_concrete_size")
    (is-method . #t)
    (s-input-req . 4)
    (s-output-len . 2)
    (c-input-len . 4)
    (c-output-len . 2)
    (pdata ((name . "specified_width")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "specified_height")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "default_width")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "default_height")
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
           ((name . "concrete_width")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "concrete_height")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 5)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPaintable
  'paintable:get-current-image
  'get-current-image
  'gdk_paintable_get_current_image
  '((name . "get_current_image")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GInterface)
            (gtype-name . "GdkPaintable")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkPaintableFlags>
       "GdkPaintableFlags"
       "gdk_paintable_flags_get_type")

(^flags-conv 'paintable-flags 'GdkPaintableFlags)

(^function
  'Gdk
  'GdkPaintable
  'paintable:get-flags
  'get-flags
  'gdk_paintable_get_flags
  '((name . "get_flags")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkPaintableFlags")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPaintable
  'paintable:get-intrinsic-aspect-ratio
  'get-intrinsic-aspect-ratio
  'gdk_paintable_get_intrinsic_aspect_ratio
  '((name . "get_intrinsic_aspect_ratio")
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
  'Gdk
  'GdkPaintable
  'paintable:get-intrinsic-height
  'get-intrinsic-height
  'gdk_paintable_get_intrinsic_height
  '((name . "get_intrinsic_height")
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
  'Gdk
  'GdkPaintable
  'paintable:get-intrinsic-width
  'get-intrinsic-width
  'gdk_paintable_get_intrinsic_width
  '((name . "get_intrinsic_width")
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
  'Gdk
  'GdkPaintable
  'paintable:invalidate-contents
  'invalidate-contents
  'gdk_paintable_invalidate_contents
  '((name . "invalidate_contents")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPaintable
  'paintable:invalidate-size
  'invalidate-size
  'gdk_paintable_invalidate_size
  '((name . "invalidate_size")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkSnapshot>
       "GdkSnapshot"
       "gdk_snapshot_get_type")

(^function
  'Gdk
  'GdkPaintable
  'paintable:snapshot
  'snapshot
  'gdk_paintable_snapshot
  '((name . "snapshot")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "snapshot")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSnapshot")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "width")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "height")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
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

(^signal
  'Gdk
  'GdkPaintable
  'paintable:invalidate-contents
  'invalidate-contents
  'invalidate-contents
  '((name . "invalidate-contents")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkPaintable
  'paintable:invalidate-size
  'invalidate-size
  'invalidate-size
  '((name . "invalidate-size")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkPopup>
       "GdkPopup"
       "gdk_popup_get_type")

(^function
  'Gdk
  'GdkPopup
  'popup:get-autohide?
  'get-autohide?
  'gdk_popup_get_autohide
  '((name . "get_autohide")
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
  'Gdk
  'GdkPopup
  'popup:get-parent
  'get-parent
  'gdk_popup_get_parent
  '((name . "get_parent")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopup
  'popup:get-position-x
  'get-position-x
  'gdk_popup_get_position_x
  '((name . "get_position_x")
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
  'Gdk
  'GdkPopup
  'popup:get-position-y
  'get-position-y
  'gdk_popup_get_position_y
  '((name . "get_position_y")
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
  'Gdk
  'GdkPopup
  'popup:get-rect-anchor
  'get-rect-anchor
  'gdk_popup_get_rect_anchor
  '((name . "get_rect_anchor")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopup
  'popup:get-surface-anchor
  'get-surface-anchor
  'gdk_popup_get_surface_anchor
  '((name . "get_surface_anchor")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkPopupLayout>
       "GdkPopupLayout"
       "gdk_popup_layout_get_type")

(^function
  'Gdk
  'GdkPopup
  'popup:present?
  'present?
  'gdk_popup_present
  '((name . "present")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "height")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "layout")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkPopupLayout")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkPopup
  'popup:autohide
  'autohide
  'autohide)

(^property
  'GdkPopup
  'popup:parent
  'parent
  'parent)

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:new
  'new
  'gdk_popup_layout_new
  '((name . "new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "anchor_rect")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "rect_anchor")
            (meta (arg-type . enum)
                  (gtype-name . "GdkGravity")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "surface_anchor")
            (meta (arg-type . enum)
                  (gtype-name . "GdkGravity")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPopupLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:copy
  'copy
  'gdk_popup_layout_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPopupLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:equal?
  'equal?
  'gdk_popup_layout_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "other")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkPopupLayout")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:get-anchor-hints
  'get-anchor-hints
  'gdk_popup_layout_get_anchor_hints
  '((name . "get_anchor_hints")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkAnchorHints")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:get-anchor-rect
  'get-anchor-rect
  'gdk_popup_layout_get_anchor_rect
  '((name . "get_anchor_rect")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRectangle")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:get-offset
  'get-offset
  'gdk_popup_layout_get_offset
  '((name . "get_offset")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "dx")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "dy")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:get-rect-anchor
  'get-rect-anchor
  'gdk_popup_layout_get_rect_anchor
  '((name . "get_rect_anchor")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:get-shadow-width
  'get-shadow-width
  'gdk_popup_layout_get_shadow_width
  '((name . "get_shadow_width")
    (is-method . #t)
    (s-output-len . 4)
    (c-output-len . 4)
    (pdata ((name . "left")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "right")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "top")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 2)
            (s-output-pos . 2))
           ((name . "bottom")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 3)
            (s-output-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:get-surface-anchor
  'get-surface-anchor
  'gdk_popup_layout_get_surface_anchor
  '((name . "get_surface_anchor")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:ref
  'ref
  'gdk_popup_layout_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPopupLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:set-anchor-hints
  'set-anchor-hints
  'gdk_popup_layout_set_anchor_hints
  '((name . "set_anchor_hints")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "anchor_hints")
            (meta (arg-type . flags)
                  (gtype-name . "GdkAnchorHints")
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

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:set-anchor-rect
  'set-anchor-rect
  'gdk_popup_layout_set_anchor_rect
  '((name . "set_anchor_rect")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "anchor_rect")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gdk
  'GdkPopupLayout
  'popup-layout:set-offset
  'set-offset
  'gdk_popup_layout_set_offset
  '((name . "set_offset")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "dx")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dy")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:set-rect-anchor
  'set-rect-anchor
  'gdk_popup_layout_set_rect_anchor
  '((name . "set_rect_anchor")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "anchor")
            (meta (arg-type . enum)
                  (gtype-name . "GdkGravity")
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

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:set-shadow-width
  'set-shadow-width
  'gdk_popup_layout_set_shadow_width
  '((name . "set_shadow_width")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "left")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "right")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "top")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "bottom")
            (meta (arg-type . int32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:set-surface-anchor
  'set-surface-anchor
  'gdk_popup_layout_set_surface_anchor
  '((name . "set_surface_anchor")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "anchor")
            (meta (arg-type . enum)
                  (gtype-name . "GdkGravity")
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

(^function
  'Gdk
  'GdkPopupLayout
  'popup-layout:unref
  'unref
  'gdk_popup_layout_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkProximityEvent>
       "GdkProximityEvent"
       "gdk_proximity_event_get_type")

(^sized-type
  'Gdk
  '<GdkRGBA>
  "GdkRGBA"
  "gdk_rgba_get_type"
  16)

(^function
  'Gdk
  'GdkRGBA
  'rgba:copy
  'copy
  'gdk_rgba_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRGBA")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkRGBA
  'rgba:equal?
  'equal?
  'gdk_rgba_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRGBA")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gdk
  'GdkRGBA
  'rgba:free
  'free
  'gdk_rgba_free
  '((name . "free")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkRGBA
  'rgba:hash
  'hash
  'gdk_rgba_hash
  '((name . "hash")
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
  'Gdk
  'GdkRGBA
  'rgba:is-clear?
  'is-clear?
  'gdk_rgba_is_clear
  '((name . "is_clear")
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
  'Gdk
  'GdkRGBA
  'rgba:is-opaque?
  'is-opaque?
  'gdk_rgba_is_opaque
  '((name . "is_opaque")
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
  'Gdk
  'GdkRGBA
  'rgba:parse?
  'parse?
  'gdk_rgba_parse
  '((name . "parse")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "spec")
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
  'Gdk
  'GdkRGBA
  'rgba:to-string
  'to-string
  'gdk_rgba_to_string
  '((name . "to_string")
    (is-method . #t)
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
  'Gdk
  'GdkRectangle
  'rectangle:contains-point?
  'contains-point?
  'gdk_rectangle_contains_point
  '((name . "contains_point")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "x")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "y")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkRectangle
  'rectangle:equal?
  'equal?
  'gdk_rectangle_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "rect2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gdk
  'GdkRectangle
  'rectangle:intersect!
  'intersect!
  'gdk_rectangle_intersect
  '((name . "intersect")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "src2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags out preallocated optional)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 0)
            (s-input-pos . 1)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkRectangle
  'rectangle:union!
  'union!
  'gdk_rectangle_union
  '((name . "union")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "src2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 0)
            (s-input-pos . 1)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkScrollDirection>
       "GdkScrollDirection"
       "gdk_scroll_direction_get_type")

(^enum-conv
  'scroll-direction
  'GdkScrollDirection)

(^type 'Gdk
       '<GdkScrollEvent>
       "GdkScrollEvent"
       "gdk_scroll_event_get_type")

(^function
  'Gdk
  'GdkScrollEvent
  'scroll-event:get-deltas
  'get-deltas
  'gdk_scroll_event_get_deltas
  '((name . "get_deltas")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "delta_x")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "delta_y")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkScrollEvent
  'scroll-event:get-direction
  'get-direction
  'gdk_scroll_event_get_direction
  '((name . "get_direction")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkScrollDirection")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkScrollUnit>
       "GdkScrollUnit"
       "gdk_scroll_unit_get_type")

(^enum-conv 'scroll-unit 'GdkScrollUnit)

(^function
  'Gdk
  'GdkScrollEvent
  'scroll-event:get-unit
  'get-unit
  'gdk_scroll_event_get_unit
  '((name . "get_unit")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkScrollUnit")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkScrollEvent
  'scroll-event:is-stop?
  'is-stop?
  'gdk_scroll_event_is_stop
  '((name . "is_stop")
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

(^type 'Gdk
       '<GdkSeatCapabilities>
       "GdkSeatCapabilities"
       "gdk_seat_capabilities_get_type")

(^flags-conv
  'seat-capabilities
  'GdkSeatCapabilities)

(^function
  'Gdk
  'GdkSeat
  'seat:get-capabilities
  'get-capabilities
  'gdk_seat_get_capabilities
  '((name . "get_capabilities")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkSeatCapabilities")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSeat
  'seat:get-devices
  'get-devices
  'gdk_seat_get_devices
  '((name . "get_devices")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "capabilities")
            (meta (arg-type . flags)
                  (gtype-name . "GdkSeatCapabilities")
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
      (meta (arg-type . GList)
            (flags ptr out)
            (transfer . container)
            (params
              ((arg-type . GObject)
               (gtype-name . "GdkDevice")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSeat
  'seat:get-display
  'get-display
  'gdk_seat_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSeat
  'seat:get-keyboard
  'get-keyboard
  'gdk_seat_get_keyboard
  '((name . "get_keyboard")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDevice")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSeat
  'seat:get-pointer
  'get-pointer
  'gdk_seat_get_pointer
  '((name . "get_pointer")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDevice")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSeat
  'seat:get-tools
  'get-tools
  'gdk_seat_get_tools
  '((name . "get_tools")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GList)
            (flags ptr out)
            (transfer . container)
            (params
              ((arg-type . GObject)
               (gtype-name . "GdkDeviceTool")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkSeat
  'seat:display
  'display
  'display)

(^signal
  'Gdk
  'GdkSeat
  'seat:device-added
  'device-added
  'device-added
  '((name . "device-added")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
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
  'Gdk
  'GdkSeat
  'seat:device-removed
  'device-removed
  'device-removed
  '((name . "device-removed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
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
  'Gdk
  'GdkSeat
  'seat:tool-added
  'tool-added
  'tool-added
  '((name . "tool-added")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "tool")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDeviceTool")
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
  'Gdk
  'GdkSeat
  'seat:tool-removed
  'tool-removed
  'tool-removed
  '((name . "tool-removed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "tool")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDeviceTool")
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

(^function
  'Gdk
  'GdkSurface
  'surface:new-popup
  'new-popup
  'gdk_surface_new_popup
  '((name . "new_popup")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "parent")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSurface")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "autohide")
            (meta (arg-type . gboolean)
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:new-toplevel
  'new-toplevel
  'gdk_surface_new_toplevel
  '((name . "new_toplevel")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "display")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDisplay")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkSurface")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:beep
  'beep
  'gdk_surface_beep
  '((name . "beep")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:create-cairo-context
  'create-cairo-context
  'gdk_surface_create_cairo_context
  '((name . "create_cairo_context")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkCairoContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:create-gl-context
  'create-gl-context
  'gdk_surface_create_gl_context
  '((name . "create_gl_context")
    (is-method . #t)
    (can-throw-gerror . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkGLContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'cairo
       '<CairoSurface>
       "CairoSurface"
       "cairo_gobject_surface_get_type")

(^type 'cairo
       '<cairo_content_t>
       "cairo_content_t"
       "cairo_gobject_content_get_type")

(^enum-conv 'content 'cairo_content_t)

(^function
  'Gdk
  'GdkSurface
  'surface:create-similar-surface
  'create-similar-surface
  'gdk_surface_create_similar_surface
  '((name . "create_similar_surface")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "content")
            (meta (arg-type . enum)
                  (gtype-name . "cairo_content_t")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "height")
            (meta (arg-type . int32)
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
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoSurface")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gio
       '<GInitable>
       "GInitable"
       "g_initable_get_type")

(^type 'Gdk
       '<GdkVulkanContext>
       "GdkVulkanContext"
       "gdk_vulkan_context_get_type")

(^function
  'Gdk
  'GdkSurface
  'surface:create-vulkan-context
  'create-vulkan-context
  'gdk_surface_create_vulkan_context
  '((name . "create_vulkan_context")
    (is-method . #t)
    (can-throw-gerror . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkVulkanContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:destroy
  'destroy
  'gdk_surface_destroy
  '((name . "destroy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:get-cursor
  'get-cursor
  'gdk_surface_get_cursor
  '((name . "get_cursor")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkCursor")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:get-device-cursor
  'get-device-cursor
  'gdk_surface_get_device_cursor
  '((name . "get_device_cursor")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkCursor")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:get-device-position
  'get-device-position
  'gdk_surface_get_device_position
  '((name . "get_device_position")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 3)
    (c-input-len . 1)
    (c-output-len . 3)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "x")
            (meta (arg-type . double)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "y")
            (meta (arg-type . double)
                  (flags out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "mask")
            (meta (arg-type . flags)
                  (gtype-name . "GdkModifierType")
                  (flags out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 2)
            (s-output-pos . 2)))
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
  'Gdk
  'GdkSurface
  'surface:get-display
  'get-display
  'gdk_surface_get_display
  '((name . "get_display")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkDisplay")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:get-frame-clock
  'get-frame-clock
  'gdk_surface_get_frame_clock
  '((name . "get_frame_clock")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkFrameClock")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:get-height
  'get-height
  'gdk_surface_get_height
  '((name . "get_height")
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
  'Gdk
  'GdkSurface
  'surface:get-mapped?
  'get-mapped?
  'gdk_surface_get_mapped
  '((name . "get_mapped")
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
  'Gdk
  'GdkSurface
  'surface:get-scale-factor
  'get-scale-factor
  'gdk_surface_get_scale_factor
  '((name . "get_scale_factor")
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
  'Gdk
  'GdkSurface
  'surface:get-width
  'get-width
  'gdk_surface_get_width
  '((name . "get_width")
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
  'Gdk
  'GdkSurface
  'surface:hide
  'hide
  'gdk_surface_hide
  '((name . "hide")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:is-destroyed?
  'is-destroyed?
  'gdk_surface_is_destroyed
  '((name . "is_destroyed")
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
  'Gdk
  'GdkSurface
  'surface:queue-render
  'queue-render
  'gdk_surface_queue_render
  '((name . "queue_render")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:request-layout
  'request-layout
  'gdk_surface_request_layout
  '((name . "request_layout")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkSurface
  'surface:set-cursor
  'set-cursor
  'gdk_surface_set_cursor
  '((name . "set_cursor")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "cursor")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkCursor")
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
  'Gdk
  'GdkSurface
  'surface:set-device-cursor
  'set-device-cursor
  'gdk_surface_set_device_cursor
  '((name . "set_device_cursor")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "cursor")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkCursor")
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
  'Gdk
  'GdkSurface
  'surface:set-input-region
  'set-input-region
  'gdk_surface_set_input_region
  '((name . "set_input_region")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "region")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoRegion")
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
  'Gdk
  'GdkSurface
  'surface:set-opaque-region
  'set-opaque-region
  'gdk_surface_set_opaque_region
  '((name . "set_opaque_region")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "region")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoRegion")
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
  'Gdk
  'GdkSurface
  'surface:translate-coordinates
  'translate-coordinates
  'gdk_surface_translate_coordinates
  '((name . "translate_coordinates")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 2)
    (c-input-len . 3)
    (c-output-len . 2)
    (pdata ((name . "to")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSurface")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "x")
            (meta (arg-type . double)
                  (flags in out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (c-output-pos . 0)
            (s-input-pos . 1)
            (s-output-pos . 0))
           ((name . "y")
            (meta (arg-type . double)
                  (flags in out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (c-output-pos . 1)
            (s-input-pos . 2)
            (s-output-pos . 1)))
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

(^property
  'GdkSurface
  'surface:cursor
  'cursor
  'cursor)

(^property
  'GdkSurface
  'surface:display
  'display
  'display)

(^property
  'GdkSurface
  'surface:frame-clock
  'frame-clock
  'frame-clock)

(^property
  'GdkSurface
  'surface:height
  'height
  'height)

(^property
  'GdkSurface
  'surface:mapped
  'mapped
  'mapped)

(^property
  'GdkSurface
  'surface:scale-factor
  'scale-factor
  'scale-factor)

(^property
  'GdkSurface
  'surface:width
  'width
  'width)

(^signal
  'Gdk
  'GdkSurface
  'surface:enter-monitor
  'enter-monitor
  'enter-monitor
  '((name . "enter-monitor")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "monitor")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkMonitor")
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
  'Gdk
  'GdkSurface
  'surface:event
  'event
  'event
  '((name . "event")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "event")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkSurface
  'surface:layout
  'layout
  'layout
  '((name . "layout")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "height")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'Gdk
  'GdkSurface
  'surface:leave-monitor
  'leave-monitor
  'leave-monitor
  '((name . "leave-monitor")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "monitor")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkMonitor")
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
  'Gdk
  'GdkSurface
  'surface:render
  'render
  'render
  '((name . "render")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "region")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoRegion")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkSurfaceEdge>
       "GdkSurfaceEdge"
       "gdk_surface_edge_get_type")

(^enum-conv 'surface-edge 'GdkSurfaceEdge)

(^type 'GdkPixbuf
       '<GdkPixbuf>
       "GdkPixbuf"
       "gdk_pixbuf_get_type")

(^function
  'Gdk
  'GdkTexture
  'texture:new-for-pixbuf
  'new-for-pixbuf
  'gdk_texture_new_for_pixbuf
  '((name . "new_for_pixbuf")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "pixbuf")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:new-from-bytes
  'new-from-bytes
  'gdk_texture_new_from_bytes
  '((name . "new_from_bytes")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "bytes")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:new-from-file
  'new-from-file
  'gdk_texture_new_from_file
  '((name . "new_from_file")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "file")
            (meta (arg-type . GInterface)
                  (gtype-name . "GFile")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:new-from-filename
  'new-from-filename
  'gdk_texture_new_from_filename
  '((name . "new_from_filename")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "path")
            (meta (arg-type . locale-string)
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:new-from-resource
  'new-from-resource
  'gdk_texture_new_from_resource
  '((name . "new_from_resource")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "resource_path")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:download
  'download
  'gdk_texture_download
  '((name . "download")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . uint8)
                     (item-size . 1)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "stride")
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
  'Gdk
  'GdkTexture
  'texture:get-format
  'get-format
  'gdk_texture_get_format
  '((name . "get_format")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkMemoryFormat")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:get-height
  'get-height
  'gdk_texture_get_height
  '((name . "get_height")
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
  'Gdk
  'GdkTexture
  'texture:get-width
  'get-width
  'gdk_texture_get_width
  '((name . "get_width")
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
  'Gdk
  'GdkTexture
  'texture:save-to-png?
  'save-to-png?
  'gdk_texture_save_to_png
  '((name . "save_to_png")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "filename")
            (meta (arg-type . locale-string)
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
  'Gdk
  'GdkTexture
  'texture:save-to-png-bytes
  'save-to-png-bytes
  'gdk_texture_save_to_png_bytes
  '((name . "save_to_png_bytes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GBytes")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTexture
  'texture:save-to-tiff?
  'save-to-tiff?
  'gdk_texture_save_to_tiff
  '((name . "save_to_tiff")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "filename")
            (meta (arg-type . locale-string)
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
  'Gdk
  'GdkTexture
  'texture:save-to-tiff-bytes
  'save-to-tiff-bytes
  'gdk_texture_save_to_tiff_bytes
  '((name . "save_to_tiff_bytes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GBytes")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkTexture
  'texture:height
  'height
  'height)

(^property
  'GdkTexture
  'texture:width
  'width
  'width)

(^type 'Gdk
       '<GdkTextureDownloader>
       "GdkTextureDownloader"
       "gdk_texture_downloader_get_type")

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:new
  'new
  'gdk_texture_downloader_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "texture")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkTexture")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkTextureDownloader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:copy
  'copy
  'gdk_texture_downloader_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkTextureDownloader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:download-bytes
  'download-bytes
  'gdk_texture_downloader_download_bytes
  '((name . "download_bytes")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "out_stride")
            (meta (arg-type . uint64)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GBytes")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:download-into
  'download-into
  'gdk_texture_downloader_download_into
  '((name . "download_into")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . uint8)
                     (item-size . 1)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "stride")
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
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:free
  'free
  'gdk_texture_downloader_free
  '((name . "free")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:get-format
  'get-format
  'gdk_texture_downloader_get_format
  '((name . "get_format")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkMemoryFormat")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:get-texture
  'get-texture
  'gdk_texture_downloader_get_texture
  '((name . "get_texture")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:set-format
  'set-format
  'gdk_texture_downloader_set_format
  '((name . "set_format")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "format")
            (meta (arg-type . enum)
                  (gtype-name . "GdkMemoryFormat")
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

(^function
  'Gdk
  'GdkTextureDownloader
  'texture-downloader:set-texture
  'set-texture
  'gdk_texture_downloader_set_texture
  '((name . "set_texture")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "texture")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkTexture")
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

(^type 'Gdk
       '<GdkTextureError>
       "GdkTextureError"
       "gdk_texture_error_get_type")

(^enum-conv 'texture-error 'GdkTextureError)

(^function
  'Gdk
  'GdkTextureError
  'texture-error:quark
  'quark
  'gdk_texture_error_quark
  '((name . "quark")
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

(^type 'Gdk
       '<GdkTitlebarGesture>
       "GdkTitlebarGesture"
       "gdk_titlebar_gesture_get_type")

(^enum-conv
  'titlebar-gesture
  'GdkTitlebarGesture)

(^type 'Gdk
       '<GdkToplevel>
       "GdkToplevel"
       "gdk_toplevel_get_type")

(^function
  'Gdk
  'GdkToplevel
  'toplevel:begin-move
  'begin-move
  'gdk_toplevel_begin_move
  '((name . "begin_move")
    (is-method . #t)
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "button")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "x")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "y")
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
           ((name . "timestamp")
            (meta (arg-type . uint32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevel
  'toplevel:begin-resize
  'begin-resize
  'gdk_toplevel_begin_resize
  '((name . "begin_resize")
    (is-method . #t)
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "edge")
            (meta (arg-type . enum)
                  (gtype-name . "GdkSurfaceEdge")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "device")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkDevice")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "button")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "x")
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
           ((name . "y")
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
           ((name . "timestamp")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
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
  'Gdk
  'GdkToplevel
  'toplevel:focus
  'focus
  'gdk_toplevel_focus
  '((name . "focus")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "timestamp")
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

(^type 'Gdk
       '<GdkToplevelState>
       "GdkToplevelState"
       "gdk_toplevel_state_get_type")

(^flags-conv 'toplevel-state 'GdkToplevelState)

(^function
  'Gdk
  'GdkToplevel
  'toplevel:get-state
  'get-state
  'gdk_toplevel_get_state
  '((name . "get_state")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "GdkToplevelState")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevel
  'toplevel:inhibit-system-shortcuts
  'inhibit-system-shortcuts
  'gdk_toplevel_inhibit_system_shortcuts
  '((name . "inhibit_system_shortcuts")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "event")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
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
  'Gdk
  'GdkToplevel
  'toplevel:lower?
  'lower?
  'gdk_toplevel_lower
  '((name . "lower")
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
  'Gdk
  'GdkToplevel
  'toplevel:minimize?
  'minimize?
  'gdk_toplevel_minimize
  '((name . "minimize")
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

(^type 'Gdk
       '<GdkToplevelLayout>
       "GdkToplevelLayout"
       "gdk_toplevel_layout_get_type")

(^function
  'Gdk
  'GdkToplevel
  'toplevel:present
  'present
  'gdk_toplevel_present
  '((name . "present")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "layout")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkToplevelLayout")
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
  'Gdk
  'GdkToplevel
  'toplevel:restore-system-shortcuts
  'restore-system-shortcuts
  'gdk_toplevel_restore_system_shortcuts
  '((name . "restore_system_shortcuts")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevel
  'toplevel:set-decorated
  'set-decorated
  'gdk_toplevel_set_decorated
  '((name . "set_decorated")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "decorated")
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
  'Gdk
  'GdkToplevel
  'toplevel:set-deletable
  'set-deletable
  'gdk_toplevel_set_deletable
  '((name . "set_deletable")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "deletable")
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
  'Gdk
  'GdkToplevel
  'toplevel:set-icon-list
  'set-icon-list
  'gdk_toplevel_set_icon_list
  '((name . "set_icon_list")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "surfaces")
            (meta (arg-type . GList)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "GdkTexture")
                     (transfer . nothing))))
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
  'Gdk
  'GdkToplevel
  'toplevel:set-modal
  'set-modal
  'gdk_toplevel_set_modal
  '((name . "set_modal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "modal")
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
  'Gdk
  'GdkToplevel
  'toplevel:set-startup-id
  'set-startup-id
  'gdk_toplevel_set_startup_id
  '((name . "set_startup_id")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "startup_id")
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
  'Gdk
  'GdkToplevel
  'toplevel:set-title
  'set-title
  'gdk_toplevel_set_title
  '((name . "set_title")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "title")
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
  'Gdk
  'GdkToplevel
  'toplevel:set-transient-for
  'set-transient-for
  'gdk_toplevel_set_transient_for
  '((name . "set_transient_for")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "parent")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSurface")
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
  'Gdk
  'GdkToplevel
  'toplevel:show-window-menu?
  'show-window-menu?
  'gdk_toplevel_show_window_menu
  '((name . "show_window_menu")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "event")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevel
  'toplevel:supports-edge-constraints?
  'supports-edge-constraints?
  'gdk_toplevel_supports_edge_constraints
  '((name . "supports_edge_constraints")
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
  'Gdk
  'GdkToplevel
  'toplevel:titlebar-gesture?
  'titlebar-gesture?
  'gdk_toplevel_titlebar_gesture
  '((name . "titlebar_gesture")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "gesture")
            (meta (arg-type . enum)
                  (gtype-name . "GdkTitlebarGesture")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GdkToplevel
  'toplevel:decorated
  'decorated
  'decorated)

(^property
  'GdkToplevel
  'toplevel:deletable
  'deletable
  'deletable)

(^property
  'GdkToplevel
  'toplevel:fullscreen-mode
  'fullscreen-mode
  'fullscreen-mode)

(^property
  'GdkToplevel
  'toplevel:icon-list
  'icon-list
  'icon-list)

(^property
  'GdkToplevel
  'toplevel:modal
  'modal
  'modal)

(^property
  'GdkToplevel
  'toplevel:shortcuts-inhibited
  'shortcuts-inhibited
  'shortcuts-inhibited)

(^property
  'GdkToplevel
  'toplevel:startup-id
  'startup-id
  'startup-id)

(^property
  'GdkToplevel
  'toplevel:state
  'state
  'state)

(^property
  'GdkToplevel
  'toplevel:title
  'title
  'title)

(^property
  'GdkToplevel
  'toplevel:transient-for
  'transient-for
  'transient-for)

(^function
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:new
  'new
  'gdk_toplevel_layout_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkToplevelLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:copy
  'copy
  'gdk_toplevel_layout_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkToplevelLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:equal?
  'equal?
  'gdk_toplevel_layout_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "other")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkToplevelLayout")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:get-fullscreen
  'get-fullscreen
  'gdk_toplevel_layout_get_fullscreen
  '((name . "get_fullscreen")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "fullscreen")
            (meta (arg-type . gboolean)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:get-fullscreen-monitor
  'get-fullscreen-monitor
  'gdk_toplevel_layout_get_fullscreen_monitor
  '((name . "get_fullscreen_monitor")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkMonitor")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:get-maximized
  'get-maximized
  'gdk_toplevel_layout_get_maximized
  '((name . "get_maximized")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "maximized")
            (meta (arg-type . gboolean)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:get-resizable?
  'get-resizable?
  'gdk_toplevel_layout_get_resizable
  '((name . "get_resizable")
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
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:ref
  'ref
  'gdk_toplevel_layout_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkToplevelLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:set-fullscreen
  'set-fullscreen
  'gdk_toplevel_layout_set_fullscreen
  '((name . "set_fullscreen")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "fullscreen")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "monitor")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkMonitor")
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
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:set-maximized
  'set-maximized
  'gdk_toplevel_layout_set_maximized
  '((name . "set_maximized")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "maximized")
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
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:set-resizable
  'set-resizable
  'gdk_toplevel_layout_set_resizable
  '((name . "set_resizable")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "resizable")
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
  'Gdk
  'GdkToplevelLayout
  'toplevel-layout:unref
  'unref
  'gdk_toplevel_layout_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkTouchEvent>
       "GdkTouchEvent"
       "gdk_touch_event_get_type")

(^function
  'Gdk
  'GdkTouchEvent
  'touch-event:get-emulating-pointer?
  'get-emulating-pointer?
  'gdk_touch_event_get_emulating_pointer
  '((name . "get_emulating_pointer")
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

(^type 'Gdk
       '<GdkTouchpadEvent>
       "GdkTouchpadEvent"
       "gdk_touchpad_event_get_type")

(^function
  'Gdk
  'GdkTouchpadEvent
  'touchpad-event:get-deltas
  'get-deltas
  'gdk_touchpad_event_get_deltas
  '((name . "get_deltas")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "dx")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "dy")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkTouchpadGesturePhase>
       "GdkTouchpadGesturePhase"
       "gdk_touchpad_gesture_phase_get_type")

(^enum-conv
  'touchpad-gesture-phase
  'GdkTouchpadGesturePhase)

(^function
  'Gdk
  'GdkTouchpadEvent
  'touchpad-event:get-gesture-phase
  'get-gesture-phase
  'gdk_touchpad_event_get_gesture_phase
  '((name . "get_gesture_phase")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkTouchpadGesturePhase")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  'GdkTouchpadEvent
  'touchpad-event:get-n-fingers
  'get-n-fingers
  'gdk_touchpad_event_get_n_fingers
  '((name . "get_n_fingers")
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
  'Gdk
  'GdkTouchpadEvent
  'touchpad-event:get-pinch-angle-delta
  'get-pinch-angle-delta
  'gdk_touchpad_event_get_pinch_angle_delta
  '((name . "get_pinch_angle_delta")
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
  'Gdk
  'GdkTouchpadEvent
  'touchpad-event:get-pinch-scale
  'get-pinch-scale
  'gdk_touchpad_event_get_pinch_scale
  '((name . "get_pinch_scale")
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

(^signal
  'Gdk
  'GdkVulkanContext
  'vulkan-context:images-updated
  'images-updated
  'images-updated
  '((name . "images-updated")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gdk
       '<GdkVulkanError>
       "GdkVulkanError"
       "gdk_vulkan_error_get_type")

(^enum-conv 'vulkan-error 'GdkVulkanError)

(^function
  'Gdk
  'GdkVulkanError
  'vulkan-error:quark
  'quark
  'gdk_vulkan_error_quark
  '((name . "quark")
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
  'Gdk
  #f
  'gdk:cairo-rectangle
  'cairo-rectangle
  'gdk_cairo_rectangle
  '((name . "cairo_rectangle")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "cr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "rectangle")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRectangle")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gdk
  #f
  'gdk:cairo-region
  'cairo-region
  'gdk_cairo_region
  '((name . "cairo_region")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "cr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "region")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoRegion")
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
  'Gdk
  #f
  'gdk:cairo-region-create-from-surface
  'cairo-region-create-from-surface
  'gdk_cairo_region_create_from_surface
  '((name . "cairo_region_create_from_surface")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "surface")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoSurface")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoRegion")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:cairo-set-source-pixbuf
  'cairo-set-source-pixbuf
  'gdk_cairo_set_source_pixbuf
  '((name . "cairo_set_source_pixbuf")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "cr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "pixbuf")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "pixbuf_x")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "pixbuf_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
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

(^function
  'Gdk
  #f
  'gdk:cairo-set-source-rgba
  'cairo-set-source-rgba
  'gdk_cairo_set_source_rgba
  '((name . "cairo_set_source_rgba")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "cr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "rgba")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRGBA")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gdk
  #f
  'gdk:content-deserialize-async
  'content-deserialize-async
  'gdk_content_deserialize_async
  '((name . "content_deserialize_async")
    (s-input-req . 4)
    (s-input-opt . 3)
    (c-input-len . 7)
    (pdata ((name . "stream")
            (meta (arg-type . GObject)
                  (gtype-name . "GInputStream")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mime_type")
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
           ((name . "type")
            (meta (arg-type . GType)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "io_priority")
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
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "user_data")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:content-deserialize-finish!
  'content-deserialize-finish!
  'gdk_content_deserialize_finish
  '((name . "content_deserialize_finish")
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
                  (flags out preallocated)
                  (item-size . 24)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 0)
            (s-input-pos . 1)
            (s-output-pos . 0)))
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
  'Gdk
  #f
  'gdk:content-formats-parse
  'content-formats-parse
  'gdk_content_formats_parse
  '((name . "content_formats_parse")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "string")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkContentFormats")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:content-register-deserializer
  'content-register-deserializer
  'gdk_content_register_deserializer
  '((name . "content_register_deserializer")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "mime_type")
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
            (s-input-pos . 1))
           ((name . "deserialize")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "ContentDeserializeFunc")
                    (s-input-req . 1)
                    (c-input-len . 1)
                    (pdata ((name . "deserializer")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GdkContentDeserializer")
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
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "notify")
            (meta (arg-type . callback)
                  (flags in)
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
  'Gdk
  #f
  'gdk:content-register-serializer
  'content-register-serializer
  'gdk_content_register_serializer
  '((name . "content_register_serializer")
    (s-input-req . 5)
    (c-input-len . 5)
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
           ((name . "mime_type")
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
           ((name . "serialize")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "ContentSerializeFunc")
                    (s-input-req . 1)
                    (c-input-len . 1)
                    (pdata ((name . "serializer")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GdkContentSerializer")
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
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "notify")
            (meta (arg-type . callback)
                  (flags in)
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
  'Gdk
  #f
  'gdk:content-serialize-async
  'content-serialize-async
  'gdk_content_serialize_async
  '((name . "content_serialize_async")
    (s-input-req . 4)
    (s-input-opt . 3)
    (c-input-len . 7)
    (pdata ((name . "stream")
            (meta (arg-type . GObject)
                  (gtype-name . "GOutputStream")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mime_type")
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
           ((name . "value")
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
           ((name . "io_priority")
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
           ((name . "cancellable")
            (meta (arg-type . GObject)
                  (gtype-name . "GCancellable")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "callback")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AsyncReadyCallback")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "source_object")
                            (meta (arg-type . GObject)
                                  (gtype-name . "GObject")
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "res")
                            (meta (arg-type . GInterface)
                                  (gtype-name . "GAsyncResult")
                                  (flags ptr in)
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "user_data")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:content-serialize-finish?
  'content-serialize-finish?
  'gdk_content_serialize_finish
  '((name . "content_serialize_finish")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "result")
            (meta (arg-type . GInterface)
                  (gtype-name . "GAsyncResult")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:drag-action-is-unique?
  'drag-action-is-unique?
  'gdk_drag_action_is_unique
  '((name . "drag_action_is_unique")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "action")
            (meta (arg-type . flags)
                  (gtype-name . "GdkDragAction")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:events-get-angle
  'events-get-angle
  'gdk_events_get_angle
  '((name . "events_get_angle")
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "event1")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "event2")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "angle")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  #f
  'gdk:events-get-center
  'events-get-center
  'gdk_events_get_center
  '((name . "events_get_center")
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "event1")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "event2")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "x")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "y")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 1)
            (s-output-pos . 1)))
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
  'Gdk
  #f
  'gdk:events-get-distance
  'events-get-distance
  'gdk_events_get_distance
  '((name . "events_get_distance")
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "event1")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "event2")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkEvent")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "distance")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0)))
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
  'Gdk
  #f
  'gdk:gl-error-quark
  'gl-error-quark
  'gdk_gl_error_quark
  '((name . "gl_error_quark")
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
  'Gdk
  #f
  'gdk:intern-mime-type
  'intern-mime-type
  'gdk_intern_mime_type
  '((name . "intern_mime_type")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "string")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-convert-case
  'keyval-convert-case
  'gdk_keyval_convert_case
  '((name . "keyval_convert_case")
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "symbol")
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
           ((name . "lower")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "upper")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-from-name
  'keyval-from-name
  'gdk_keyval_from_name
  '((name . "keyval_from_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval_name")
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
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-is-lower?
  'keyval-is-lower?
  'gdk_keyval_is_lower
  '((name . "keyval_is_lower")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-is-upper?
  'keyval-is-upper?
  'gdk_keyval_is_upper
  '((name . "keyval_is_upper")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-name
  'keyval-name
  'gdk_keyval_name
  '((name . "keyval_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval")
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
  'Gdk
  #f
  'gdk:keyval-to-lower
  'keyval-to-lower
  'gdk_keyval_to_lower
  '((name . "keyval_to_lower")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval")
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
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-to-unicode
  'keyval-to-unicode
  'gdk_keyval_to_unicode
  '((name . "keyval_to_unicode")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval")
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
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:keyval-to-upper
  'keyval-to-upper
  'gdk_keyval_to_upper
  '((name . "keyval_to_upper")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "keyval")
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
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:paintable-new-empty
  'paintable-new-empty
  'gdk_paintable_new_empty
  '((name . "paintable_new_empty")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "intrinsic_width")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "intrinsic_height")
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
      (meta (arg-type . GInterface)
            (gtype-name . "GdkPaintable")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:pixbuf-get-from-surface
  'pixbuf-get-from-surface
  'gdk_pixbuf_get_from_surface
  '((name . "pixbuf_get_from_surface")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "surface")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoSurface")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "src_x")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "src_y")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "width")
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
           ((name . "height")
            (meta (arg-type . int32)
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:pixbuf-get-from-texture
  'pixbuf-get-from-texture
  'gdk_pixbuf_get_from_texture
  '((name . "pixbuf_get_from_texture")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "texture")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkTexture")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:set-allowed-backends
  'set-allowed-backends
  'gdk_set_allowed_backends
  '((name . "set_allowed_backends")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "backends")
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
  'Gdk
  #f
  'gdk:texture-error-quark
  'texture-error-quark
  'gdk_texture_error_quark
  '((name . "texture_error_quark")
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
  'Gdk
  #f
  'gdk:toplevel-size-get-type
  'toplevel-size-get-type
  'gdk_toplevel_size_get_type
  '((name . "toplevel_size_get_type")
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
  'Gdk
  #f
  'gdk:unicode-to-keyval
  'unicode-to-keyval
  'gdk_unicode_to_keyval
  '((name . "unicode_to_keyval")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "wc")
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
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gdk
  #f
  'gdk:vulkan-error-quark
  'vulkan-error-quark
  'gdk_vulkan_error_quark
  '((name . "vulkan_error_quark")
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


)
