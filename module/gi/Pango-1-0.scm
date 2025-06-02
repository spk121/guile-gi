(define-module (gi Pango-1-0)
  #:use-module (gi runtime)
  #:use-module (gi cairo-1-0)
  #:use-module (gi HarfBuzz-0-0)
  #:use-module (gi Gio-2-0)
  #:use-module (gi GObject-2-0)
  #:export(ANALYSIS_FLAG_CENTERED_BASELINE ANALYSIS_FLAG_IS_ELLIPSIS 
           ANALYSIS_FLAG_NEED_HYPHEN ATTR_INDEX_FROM_TEXT_BEGINNING 
           ATTR_INDEX_TO_TEXT_END <PangoAlignment> symbol->alignment 
           number->alignment alignment->symbol alignment->number 
           <PangoAttrIterator> copy attr-iterator:copy destroy 
           attr-iterator:destroy <PangoAttribute> <PangoAttrType> 
           symbol->attr-type number->attr-type attr-type->symbol 
           attr-type->number get attr-iterator:get get-attrs 
           attr-iterator:get-attrs <PangoFontDescription> <PangoLanguage> 
           get-font attr-iterator:get-font next? attr-iterator:next? range 
           attr-iterator:range <PangoAttrList> new attr-list:new change 
           attr-list:change copy attr-list:copy equal? attr-list:equal? filter 
           attr-list:filter get-attributes attr-list:get-attributes 
           get-iterator attr-list:get-iterator insert attr-list:insert 
           insert-before attr-list:insert-before ref attr-list:ref splice 
           attr-list:splice to-string attr-list:to-string unref attr-list:unref 
           update attr-list:update from-string attr-list:from-string get-name 
           attr-type:get-name register attr-type:register copy attribute:copy 
           destroy attribute:destroy equal? attribute:equal? 
           <PangoBaselineShift> symbol->baseline-shift number->baseline-shift 
           baseline-shift->symbol baseline-shift->number <PangoColor> copy 
           color:copy free color:free parse? color:parse? parse-with-alpha 
           color:parse-with-alpha to-string color:to-string <PangoContext> new 
           context:new changed context:changed <PangoDirection> 
           symbol->direction number->direction direction->symbol 
           direction->number get-base-dir context:get-base-dir <PangoGravity> 
           symbol->gravity number->gravity gravity->symbol gravity->number 
           get-base-gravity context:get-base-gravity get-font-description 
           context:get-font-description <GObject> <GListModel> <PangoFontMap> 
           get-font-map context:get-font-map get-gravity context:get-gravity 
           <PangoGravityHint> symbol->gravity-hint number->gravity-hint 
           gravity-hint->symbol gravity-hint->number get-gravity-hint 
           context:get-gravity-hint get-language context:get-language 
           <PangoMatrix> get-matrix context:get-matrix <PangoFontMetrics> 
           get-metrics context:get-metrics get-round-glyph-positions? 
           context:get-round-glyph-positions? get-serial context:get-serial 
           <PangoFontFamily> list-families context:list-families <PangoFont> 
           load-font context:load-font <PangoFontset> load-fontset 
           context:load-fontset set-base-dir context:set-base-dir 
           set-base-gravity context:set-base-gravity set-font-description 
           context:set-font-description set-font-map context:set-font-map 
           set-gravity-hint context:set-gravity-hint set-language 
           context:set-language set-matrix context:set-matrix 
           set-round-glyph-positions context:set-round-glyph-positions 
           <PangoCoverage> new coverage:new from-bytes coverage:from-bytes copy 
           coverage:copy <PangoCoverageLevel> symbol->coverage-level 
           number->coverage-level coverage-level->symbol coverage-level->number 
           get coverage:get max coverage:max ref coverage:ref set coverage:set 
           to-bytes coverage:to-bytes unref coverage:unref <PangoEllipsizeMode> 
           symbol->ellipsize-mode number->ellipsize-mode ellipsize-mode->symbol 
           ellipsize-mode->number descriptions-free font:descriptions-free 
           <GBytes> deserialize font:deserialize describe font:describe 
           describe-with-absolute-size font:describe-with-absolute-size 
           get-coverage font:get-coverage <PangoFontFace> get-face 
           font:get-face <hb_feature_t> get-features! font:get-features! 
           get-font-map font:get-font-map get-languages font:get-languages 
           get-metrics font:get-metrics has-char? font:has-char? serialize 
           font:serialize new font-description:new better-match? 
           font-description:better-match? copy font-description:copy 
           copy-static font-description:copy-static equal? 
           font-description:equal? free font-description:free get-family 
           font-description:get-family get-gravity font-description:get-gravity 
           <PangoFontMask> list->font-mask number->font-mask font-mask->list 
           font-mask->number get-set-fields font-description:get-set-fields 
           get-size font-description:get-size get-size-is-absolute? 
           font-description:get-size-is-absolute? <PangoStretch> 
           symbol->stretch number->stretch stretch->symbol stretch->number 
           get-stretch font-description:get-stretch <PangoStyle> symbol->style 
           number->style style->symbol style->number get-style 
           font-description:get-style <PangoVariant> symbol->variant 
           number->variant variant->symbol variant->number get-variant 
           font-description:get-variant get-variations 
           font-description:get-variations <PangoWeight> symbol->weight 
           number->weight weight->symbol weight->number get-weight 
           font-description:get-weight hash font-description:hash merge 
           font-description:merge merge-static font-description:merge-static 
           set-absolute-size font-description:set-absolute-size set-family 
           font-description:set-family set-family-static 
           font-description:set-family-static set-gravity 
           font-description:set-gravity set-size font-description:set-size 
           set-stretch font-description:set-stretch set-style 
           font-description:set-style set-variant font-description:set-variant 
           set-variations font-description:set-variations set-variations-static 
           font-description:set-variations-static set-weight 
           font-description:set-weight to-filename font-description:to-filename 
           to-string font-description:to-string unset-fields 
           font-description:unset-fields from-string 
           font-description:from-string describe font-face:describe 
           get-face-name font-face:get-face-name get-family 
           font-face:get-family is-synthesized? font-face:is-synthesized? 
           list-sizes font-face:list-sizes get-face font-family:get-face 
           get-name font-family:get-name is-monospace? 
           font-family:is-monospace? is-variable? font-family:is-variable? 
           list-faces font-family:list-faces changed font-map:changed 
           create-context font-map:create-context get-family 
           font-map:get-family get-serial font-map:get-serial list-families 
           font-map:list-families load-font font-map:load-font load-fontset 
           font-map:load-fontset get-approximate-char-width 
           font-metrics:get-approximate-char-width get-approximate-digit-width 
           font-metrics:get-approximate-digit-width get-ascent 
           font-metrics:get-ascent get-descent font-metrics:get-descent 
           get-height font-metrics:get-height get-strikethrough-position 
           font-metrics:get-strikethrough-position get-strikethrough-thickness 
           font-metrics:get-strikethrough-thickness get-underline-position 
           font-metrics:get-underline-position get-underline-thickness 
           font-metrics:get-underline-thickness ref font-metrics:ref unref 
           font-metrics:unref <PangoFontScale> symbol->font-scale 
           number->font-scale font-scale->symbol font-scale->number foreach 
           fontset:foreach get-font fontset:get-font get-metrics 
           fontset:get-metrics <PangoFontsetSimple> new fontset-simple:new 
           append fontset-simple:append size fontset-simple:size GLYPH_EMPTY 
           GLYPH_INVALID_INPUT GLYPH_UNKNOWN_FLAG <PangoGlyphItem> apply-attrs 
           glyph-item:apply-attrs copy glyph-item:copy free glyph-item:free 
           get-logical-widths glyph-item:get-logical-widths split 
           glyph-item:split <PangoGlyphItemIter> copy glyph-item-iter:copy free 
           glyph-item-iter:free init-end? glyph-item-iter:init-end? init-start? 
           glyph-item-iter:init-start? next-cluster? 
           glyph-item-iter:next-cluster? prev-cluster? 
           glyph-item-iter:prev-cluster? <PangoGlyphString> new 
           glyph-string:new copy glyph-string:copy free glyph-string:free 
           get-logical-widths glyph-string:get-logical-widths get-width 
           glyph-string:get-width set-size glyph-string:set-size get-for-matrix 
           gravity:get-for-matrix <PangoScript> symbol->script number->script 
           script->symbol script->number get-for-script gravity:get-for-script 
           get-for-script-and-width gravity:get-for-script-and-width 
           to-rotation gravity:to-rotation <PangoItem> new item:new apply-attrs 
           item:apply-attrs copy item:copy free item:free split item:split 
           get-sample-string language:get-sample-string get-scripts 
           language:get-scripts includes-script? language:includes-script? 
           matches? language:matches? to-string language:to-string from-string 
           language:from-string get-default language:get-default get-preferred 
           language:get-preferred <PangoLayout> new layout:new 
           <PangoLayoutDeserializeFlags> list->layout-deserialize-flags 
           number->layout-deserialize-flags layout-deserialize-flags->list 
           layout-deserialize-flags->number deserialize layout:deserialize 
           context-changed layout:context-changed copy layout:copy 
           get-alignment layout:get-alignment get-attributes 
           layout:get-attributes get-auto-dir? layout:get-auto-dir? 
           get-baseline layout:get-baseline get-character-count 
           layout:get-character-count get-context layout:get-context 
           get-direction layout:get-direction get-ellipsize 
           layout:get-ellipsize get-font-description 
           layout:get-font-description get-height layout:get-height get-indent 
           layout:get-indent <PangoLayoutIter> get-iter layout:get-iter 
           get-justify? layout:get-justify? get-justify-last-line? 
           layout:get-justify-last-line? <PangoLayoutLine> get-line 
           layout:get-line get-line-count layout:get-line-count 
           get-line-readonly layout:get-line-readonly get-line-spacing 
           layout:get-line-spacing get-lines layout:get-lines 
           get-lines-readonly layout:get-lines-readonly get-pixel-size 
           layout:get-pixel-size get-serial layout:get-serial 
           get-single-paragraph-mode? layout:get-single-paragraph-mode? 
           get-size layout:get-size get-spacing layout:get-spacing 
           <PangoTabArray> get-tabs layout:get-tabs get-text layout:get-text 
           get-unknown-glyphs-count layout:get-unknown-glyphs-count get-width 
           layout:get-width <PangoWrapMode> symbol->wrap-mode number->wrap-mode 
           wrap-mode->symbol wrap-mode->number get-wrap layout:get-wrap 
           index-to-line-x layout:index-to-line-x is-ellipsized? 
           layout:is-ellipsized? is-wrapped? layout:is-wrapped? 
           move-cursor-visually layout:move-cursor-visually 
           <PangoLayoutSerializeFlags> list->layout-serialize-flags 
           number->layout-serialize-flags layout-serialize-flags->list 
           layout-serialize-flags->number serialize layout:serialize 
           set-alignment layout:set-alignment set-attributes 
           layout:set-attributes set-auto-dir layout:set-auto-dir set-ellipsize 
           layout:set-ellipsize set-font-description 
           layout:set-font-description set-height layout:set-height set-indent 
           layout:set-indent set-justify layout:set-justify 
           set-justify-last-line layout:set-justify-last-line set-line-spacing 
           layout:set-line-spacing set-markup layout:set-markup 
           set-markup-with-accel layout:set-markup-with-accel 
           set-single-paragraph-mode layout:set-single-paragraph-mode 
           set-spacing layout:set-spacing set-tabs layout:set-tabs set-text 
           layout:set-text set-width layout:set-width set-wrap layout:set-wrap 
           write-to-file? layout:write-to-file? xy-to-index layout:xy-to-index 
           <PangoLayoutDeserializeError> symbol->layout-deserialize-error 
           number->layout-deserialize-error layout-deserialize-error->symbol 
           layout-deserialize-error->number quark 
           layout-deserialize-error:quark at-last-line? 
           layout-iter:at-last-line? copy layout-iter:copy free 
           layout-iter:free get-baseline layout-iter:get-baseline get-index 
           layout-iter:get-index get-layout layout-iter:get-layout get-line 
           layout-iter:get-line get-line-readonly layout-iter:get-line-readonly 
           get-line-yrange layout-iter:get-line-yrange get-run 
           layout-iter:get-run get-run-baseline layout-iter:get-run-baseline 
           get-run-readonly layout-iter:get-run-readonly next-char? 
           layout-iter:next-char? next-cluster? layout-iter:next-cluster? 
           next-line? layout-iter:next-line? next-run? layout-iter:next-run? 
           get-height layout-line:get-height get-length layout-line:get-length 
           get-resolved-direction layout-line:get-resolved-direction 
           get-start-index layout-line:get-start-index get-x-ranges 
           layout-line:get-x-ranges index-to-x layout-line:index-to-x 
           is-paragraph-start? layout-line:is-paragraph-start? ref 
           layout-line:ref unref layout-line:unref x-to-index 
           layout-line:x-to-index concat matrix:concat copy matrix:copy free 
           matrix:free get-font-scale-factor matrix:get-font-scale-factor 
           get-font-scale-factors matrix:get-font-scale-factors get-slant-ratio 
           matrix:get-slant-ratio rotate matrix:rotate scale matrix:scale 
           transform-distance matrix:transform-distance transform-point 
           matrix:transform-point translate matrix:translate <PangoOverline> 
           symbol->overline number->overline overline->symbol overline->number 
           <PangoRenderPart> symbol->render-part number->render-part 
           render-part->symbol render-part->number <PangoRenderer> activate 
           renderer:activate deactivate renderer:deactivate 
           draw-error-underline renderer:draw-error-underline draw-glyph 
           renderer:draw-glyph draw-glyph-item renderer:draw-glyph-item 
           draw-glyphs renderer:draw-glyphs draw-layout renderer:draw-layout 
           draw-layout-line renderer:draw-layout-line draw-rectangle 
           renderer:draw-rectangle draw-trapezoid renderer:draw-trapezoid 
           get-alpha renderer:get-alpha get-color renderer:get-color get-layout 
           renderer:get-layout get-layout-line renderer:get-layout-line 
           get-matrix renderer:get-matrix part-changed renderer:part-changed 
           set-alpha renderer:set-alpha set-color renderer:set-color set-matrix 
           renderer:set-matrix SCALE for-unichar script:for-unichar 
           get-sample-language script:get-sample-language <PangoScriptIter> new 
           script-iter:new free script-iter:free get-range 
           script-iter:get-range next? script-iter:next? <PangoShapeFlags> 
           list->shape-flags number->shape-flags shape-flags->list 
           shape-flags->number <PangoShowFlags> list->show-flags 
           number->show-flags show-flags->list show-flags->number 
           <PangoTabAlign> symbol->tab-align number->tab-align 
           tab-align->symbol tab-align->number new tab-array:new copy 
           tab-array:copy free tab-array:free get-decimal-point 
           tab-array:get-decimal-point get-positions-in-pixels? 
           tab-array:get-positions-in-pixels? get-size tab-array:get-size 
           get-tab tab-array:get-tab get-tabs tab-array:get-tabs resize 
           tab-array:resize set-decimal-point tab-array:set-decimal-point 
           set-positions-in-pixels tab-array:set-positions-in-pixels set-tab 
           tab-array:set-tab sort tab-array:sort to-string tab-array:to-string 
           from-string tab-array:from-string <PangoTextTransform> 
           symbol->text-transform number->text-transform text-transform->symbol 
           text-transform->number <PangoUnderline> symbol->underline 
           number->underline underline->symbol underline->number VERSION_MAJOR 
           VERSION_MICRO VERSION_MINOR VERSION_STRING attr-allow-breaks-new 
           pango:attr-allow-breaks-new attr-background-alpha-new 
           pango:attr-background-alpha-new attr-background-new 
           pango:attr-background-new attr-baseline-shift-new 
           pango:attr-baseline-shift-new attr-fallback-new 
           pango:attr-fallback-new attr-family-new pango:attr-family-new 
           attr-font-desc-new pango:attr-font-desc-new attr-font-features-new 
           pango:attr-font-features-new attr-font-scale-new 
           pango:attr-font-scale-new attr-foreground-alpha-new 
           pango:attr-foreground-alpha-new attr-foreground-new 
           pango:attr-foreground-new attr-gravity-hint-new 
           pango:attr-gravity-hint-new attr-gravity-new pango:attr-gravity-new 
           attr-insert-hyphens-new pango:attr-insert-hyphens-new 
           attr-language-new pango:attr-language-new attr-letter-spacing-new 
           pango:attr-letter-spacing-new attr-line-height-new 
           pango:attr-line-height-new attr-line-height-new-absolute 
           pango:attr-line-height-new-absolute attr-list-from-string 
           pango:attr-list-from-string attr-overline-color-new 
           pango:attr-overline-color-new attr-overline-new 
           pango:attr-overline-new attr-rise-new pango:attr-rise-new 
           attr-scale-new pango:attr-scale-new attr-sentence-new 
           pango:attr-sentence-new attr-show-new pango:attr-show-new 
           attr-size-new pango:attr-size-new attr-size-new-absolute 
           pango:attr-size-new-absolute attr-stretch-new pango:attr-stretch-new 
           attr-strikethrough-color-new pango:attr-strikethrough-color-new 
           attr-strikethrough-new pango:attr-strikethrough-new attr-style-new 
           pango:attr-style-new attr-text-transform-new 
           pango:attr-text-transform-new attr-type-get-name 
           pango:attr-type-get-name attr-type-register pango:attr-type-register 
           attr-underline-color-new pango:attr-underline-color-new 
           attr-underline-new pango:attr-underline-new attr-variant-new 
           pango:attr-variant-new attr-weight-new pango:attr-weight-new 
           attr-word-new pango:attr-word-new <PangoBidiType> symbol->bidi-type 
           number->bidi-type bidi-type->symbol bidi-type->number 
           bidi-type-for-unichar pango:bidi-type-for-unichar find-base-dir 
           pango:find-base-dir find-paragraph-boundary 
           pango:find-paragraph-boundary font-description-from-string 
           pango:font-description-from-string gravity-get-for-matrix 
           pango:gravity-get-for-matrix gravity-get-for-script 
           pango:gravity-get-for-script gravity-get-for-script-and-width 
           pango:gravity-get-for-script-and-width gravity-to-rotation 
           pango:gravity-to-rotation is-zero-width? pango:is-zero-width? 
           itemize pango:itemize itemize-with-base-dir 
           pango:itemize-with-base-dir language-from-string 
           pango:language-from-string language-get-default 
           pango:language-get-default language-get-preferred 
           pango:language-get-preferred layout-deserialize-error-quark 
           pango:layout-deserialize-error-quark log2vis-get-embedding-levels 
           pango:log2vis-get-embedding-levels <GMarkupParseContext> 
           markup-parser-finish pango:markup-parser-finish markup-parser-new 
           pango:markup-parser-new parse-markup pango:parse-markup 
           parse-stretch pango:parse-stretch parse-style pango:parse-style 
           parse-variant pango:parse-variant parse-weight pango:parse-weight 
           quantize-line-geometry pango:quantize-line-geometry reorder-items 
           pango:reorder-items script-get-sample-language 
           pango:script-get-sample-language tab-array-from-string 
           pango:tab-array-from-string unichar-direction 
           pango:unichar-direction units-from-double pango:units-from-double 
           units-to-double pango:units-to-double version pango:version 
           version-check pango:version-check version-string 
           pango:version-string))

(eval-when (expand load eval)
(^library "Pango" "1.0" '("libpango-1.0-0.dll"))

(^initialize)

(^constant 'ANALYSIS_FLAG_CENTERED_BASELINE 1)

(^constant 'ANALYSIS_FLAG_IS_ELLIPSIS 2)

(^constant 'ANALYSIS_FLAG_NEED_HYPHEN 4)

(^constant 'ATTR_INDEX_FROM_TEXT_BEGINNING 0)

(^constant 'ATTR_INDEX_TO_TEXT_END 4294967295)

(^type 'Pango
       '<PangoAlignment>
       "PangoAlignment"
       "pango_alignment_get_type")

(^enum-conv 'alignment 'PangoAlignment)

(^type 'Pango
       '<PangoAttrIterator>
       "PangoAttrIterator"
       "pango_attr_iterator_get_type")

(^function
  'Pango
  'PangoAttrIterator
  'attr-iterator:copy
  'copy
  'pango_attr_iterator_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttrIterator")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrIterator
  'attr-iterator:destroy
  'destroy
  'pango_attr_iterator_destroy
  '((name . "destroy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Pango
  '<PangoAttribute>
  "PangoAttribute"
  "pango_attribute_get_type"
  16)

(^type 'Pango
       '<PangoAttrType>
       "PangoAttrType"
       "pango_attr_type_get_type")

(^enum-conv 'attr-type 'PangoAttrType)

(^function
  'Pango
  'PangoAttrIterator
  'attr-iterator:get
  'get
  'pango_attr_iterator_get
  '((name . "get")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . enum)
                  (gtype-name . "PangoAttrType")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out nullable)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrIterator
  'attr-iterator:get-attrs
  'get-attrs
  'pango_attr_iterator_get_attrs
  '((name . "get_attrs")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . everything)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoAttribute")
               (item-size . 16)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFontDescription>
       "PangoFontDescription"
       "pango_font_description_get_type")

(^type 'Pango
       '<PangoLanguage>
       "PangoLanguage"
       "pango_language_get_type")

(^function
  'Pango
  'PangoAttrIterator
  'attr-iterator:get-font
  'get-font
  'pango_attr_iterator_get_font
  '((name . "get_font")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
                  (flags ptr out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "extra_attrs")
            (meta (arg-type . GSList)
                  (flags ptr out optional)
                  (transfer . everything)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "PangoAttribute")
                     (item-size . 16)
                     (transfer . nothing))))
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
  'Pango
  'PangoAttrIterator
  'attr-iterator:next?
  'next?
  'pango_attr_iterator_next
  '((name . "next")
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
  'Pango
  'PangoAttrIterator
  'attr-iterator:range
  'range
  'pango_attr_iterator_range
  '((name . "range")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "start")
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
           ((name . "end")
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

(^type 'Pango
       '<PangoAttrList>
       "PangoAttrList"
       "pango_attr_list_get_type")

(^function
  'Pango
  'PangoAttrList
  'attr-list:new
  'new
  'pango_attr_list_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttrList")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:change
  'change
  'pango_attr_list_change
  '((name . "change")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "attr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttribute")
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
  'Pango
  'PangoAttrList
  'attr-list:copy
  'copy
  'pango_attr_list_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttrList")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:equal?
  'equal?
  'pango_attr_list_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "other_list")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
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
  'Pango
  'PangoAttrList
  'attr-list:filter
  'filter
  'pango_attr_list_filter
  '((name . "filter")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "func")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "AttrFilterFunc")
                    (s-input-req . 1)
                    (s-input-opt . 1)
                    (c-input-len . 2)
                    (pdata ((name . "attribute")
                            (meta (arg-type . GBoxed)
                                  (gtype-name . "PangoAttribute")
                                  (flags ptr in)
                                  (item-size . 16)
                                  (transfer . nothing))
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
            (gtype-name . "PangoAttrList")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:get-attributes
  'get-attributes
  'pango_attr_list_get_attributes
  '((name . "get_attributes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . everything)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoAttribute")
               (item-size . 16)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:get-iterator
  'get-iterator
  'pango_attr_list_get_iterator
  '((name . "get_iterator")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttrIterator")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:insert
  'insert
  'pango_attr_list_insert
  '((name . "insert")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "attr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttribute")
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
  'Pango
  'PangoAttrList
  'attr-list:insert-before
  'insert-before
  'pango_attr_list_insert_before
  '((name . "insert_before")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "attr")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttribute")
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
  'Pango
  'PangoAttrList
  'attr-list:ref
  'ref
  'pango_attr_list_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttrList")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:splice
  'splice
  'pango_attr_list_splice
  '((name . "splice")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "other")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "pos")
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
           ((name . "len")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:to-string
  'to-string
  'pango_attr_list_to_string
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
  'Pango
  'PangoAttrList
  'attr-list:unref
  'unref
  'pango_attr_list_unref
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
  'Pango
  'PangoAttrList
  'attr-list:update
  'update
  'pango_attr_list_update
  '((name . "update")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "pos")
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
           ((name . "remove")
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
           ((name . "add")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrList
  'attr-list:from-string
  'from-string
  'pango_attr_list_from_string
  '((name . "from_string")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "text")
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
            (gtype-name . "PangoAttrList")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrType
  'attr-type:get-name
  'get-name
  'pango_attr_type_get_name
  '((name . "get_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . enum)
                  (gtype-name . "PangoAttrType")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttrType
  'attr-type:register
  'register
  'pango_attr_type_register
  '((name . "register")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoAttrType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttribute
  'attribute:copy
  'copy
  'pango_attribute_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoAttribute
  'attribute:destroy
  'destroy
  'pango_attribute_destroy
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
  'Pango
  'PangoAttribute
  'attribute:equal?
  'equal?
  'pango_attribute_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "attr2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttribute")
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

(^type 'Pango
       '<PangoBaselineShift>
       "PangoBaselineShift"
       "pango_baseline_shift_get_type")

(^enum-conv 'baseline-shift 'PangoBaselineShift)

(^sized-type
  'Pango
  '<PangoColor>
  "PangoColor"
  "pango_color_get_type"
  6)

(^function
  'Pango
  'PangoColor
  'color:copy
  'copy
  'pango_color_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoColor")
            (flags ptr out nullable)
            (item-size . 6)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoColor
  'color:free
  'free
  'pango_color_free
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
  'Pango
  'PangoColor
  'color:parse?
  'parse?
  'pango_color_parse
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
  'Pango
  'PangoColor
  'color:parse-with-alpha
  'parse-with-alpha
  'pango_color_parse_with_alpha
  '((name . "parse_with_alpha")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "alpha")
            (meta (arg-type . uint16)
                  (flags out optional)
                  (item-size . 2)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "spec")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
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
  'Pango
  'PangoColor
  'color:to-string
  'to-string
  'pango_color_to_string
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

(^type 'Pango
       '<PangoContext>
       "PangoContext"
       "pango_context_get_type")

(^function
  'Pango
  'PangoContext
  'context:new
  'new
  'pango_context_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoContext
  'context:changed
  'changed
  'pango_context_changed
  '((name . "changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoDirection>
       "PangoDirection"
       "pango_direction_get_type")

(^enum-conv 'direction 'PangoDirection)

(^function
  'Pango
  'PangoContext
  'context:get-base-dir
  'get-base-dir
  'pango_context_get_base_dir
  '((name . "get_base_dir")
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

(^type 'Pango
       '<PangoGravity>
       "PangoGravity"
       "pango_gravity_get_type")

(^enum-conv 'gravity 'PangoGravity)

(^function
  'Pango
  'PangoContext
  'context:get-base-gravity
  'get-base-gravity
  'pango_context_get_base_gravity
  '((name . "get_base_gravity")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoContext
  'context:get-font-description
  'get-font-description
  'pango_context_get_font_description
  '((name . "get_font_description")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GObject
       '<GObject>
       "GObject"
       "g_object_get_type")

(^type 'Gio
       '<GListModel>
       "GListModel"
       "g_list_model_get_type")

(^type 'Pango
       '<PangoFontMap>
       "PangoFontMap"
       "pango_font_map_get_type")

(^function
  'Pango
  'PangoContext
  'context:get-font-map
  'get-font-map
  'pango_context_get_font_map
  '((name . "get_font_map")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoContext
  'context:get-gravity
  'get-gravity
  'pango_context_get_gravity
  '((name . "get_gravity")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoGravityHint>
       "PangoGravityHint"
       "pango_gravity_hint_get_type")

(^enum-conv 'gravity-hint 'PangoGravityHint)

(^function
  'Pango
  'PangoContext
  'context:get-gravity-hint
  'get-gravity-hint
  'pango_context_get_gravity_hint
  '((name . "get_gravity_hint")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoGravityHint")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoContext
  'context:get-language
  'get-language
  'pango_context_get_language
  '((name . "get_language")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Pango
  '<PangoMatrix>
  "PangoMatrix"
  "pango_matrix_get_type"
  48)

(^function
  'Pango
  'PangoContext
  'context:get-matrix
  'get-matrix
  'pango_context_get_matrix
  '((name . "get_matrix")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoMatrix")
            (flags ptr out nullable)
            (item-size . 48)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Pango
  '<PangoFontMetrics>
  "PangoFontMetrics"
  "pango_font_metrics_get_type"
  40)

(^function
  'Pango
  'PangoContext
  'context:get-metrics
  'get-metrics
  'pango_context_get_metrics
  '((name . "get_metrics")
    (is-method . #t)
    (s-input-opt . 2)
    (c-input-len . 2)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
            (gtype-name . "PangoFontMetrics")
            (flags ptr out)
            (item-size . 40)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoContext
  'context:get-round-glyph-positions?
  'get-round-glyph-positions?
  'pango_context_get_round_glyph_positions
  '((name . "get_round_glyph_positions")
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
  'Pango
  'PangoContext
  'context:get-serial
  'get-serial
  'pango_context_get_serial
  '((name . "get_serial")
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

(^type 'Pango
       '<PangoFontFamily>
       "PangoFontFamily"
       "pango_font_family_get_type")

(^function
  'Pango
  'PangoContext
  'context:list-families
  'list-families
  'pango_context_list_families
  '((name . "list_families")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 2)
    (pdata ((name . "families")
            (meta (arg-type . array)
                  (flags ptr out sized)
                  (length-arg . 1)
                  (transfer . container)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "PangoFontFamily")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_families")
            (meta (arg-type . int32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFont>
       "PangoFont"
       "pango_font_get_type")

(^function
  'Pango
  'PangoContext
  'context:load-font
  'load-font
  'pango_context_load_font
  '((name . "load_font")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
            (gtype-name . "PangoFont")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFontset>
       "PangoFontset"
       "pango_fontset_get_type")

(^function
  'Pango
  'PangoContext
  'context:load-fontset
  'load-fontset
  'pango_context_load_fontset
  '((name . "load_fontset")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
            (gtype-name . "PangoFontset")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoContext
  'context:set-base-dir
  'set-base-dir
  'pango_context_set_base_dir
  '((name . "set_base_dir")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "direction")
            (meta (arg-type . enum)
                  (gtype-name . "PangoDirection")
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
  'Pango
  'PangoContext
  'context:set-base-gravity
  'set-base-gravity
  'pango_context_set_base_gravity
  '((name . "set_base_gravity")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
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
  'Pango
  'PangoContext
  'context:set-font-description
  'set-font-description
  'pango_context_set_font_description
  '((name . "set_font_description")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
  'Pango
  'PangoContext
  'context:set-font-map
  'set-font-map
  'pango_context_set_font_map
  '((name . "set_font_map")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "font_map")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoFontMap")
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
  'Pango
  'PangoContext
  'context:set-gravity-hint
  'set-gravity-hint
  'pango_context_set_gravity_hint
  '((name . "set_gravity_hint")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "hint")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravityHint")
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
  'Pango
  'PangoContext
  'context:set-language
  'set-language
  'pango_context_set_language
  '((name . "set_language")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
  'Pango
  'PangoContext
  'context:set-matrix
  'set-matrix
  'pango_context_set_matrix
  '((name . "set_matrix")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoMatrix")
                  (flags ptr in nullable)
                  (item-size . 48)
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
  'Pango
  'PangoContext
  'context:set-round-glyph-positions
  'set-round-glyph-positions
  'pango_context_set_round_glyph_positions
  '((name . "set_round_glyph_positions")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "round_positions")
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

(^type 'Pango
       '<PangoCoverage>
       "PangoCoverage"
       "pango_coverage_get_type")

(^function
  'Pango
  'PangoCoverage
  'coverage:new
  'new
  'pango_coverage_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoCoverage")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoCoverage
  'coverage:from-bytes
  'from-bytes
  'pango_coverage_from_bytes
  '((name . "from_bytes")
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "bytes")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . uint8)
                     (item-size . 1)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_bytes")
            (meta (arg-type . int32)
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
      (meta (arg-type . GObject)
            (gtype-name . "PangoCoverage")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoCoverage
  'coverage:copy
  'copy
  'pango_coverage_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoCoverage")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoCoverageLevel>
       "PangoCoverageLevel"
       "pango_coverage_level_get_type")

(^enum-conv 'coverage-level 'PangoCoverageLevel)

(^function
  'Pango
  'PangoCoverage
  'coverage:get
  'get
  'pango_coverage_get
  '((name . "get")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "index_")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoCoverageLevel")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoCoverage
  'coverage:max
  'max
  'pango_coverage_max
  '((name . "max")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "other")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoCoverage")
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
  'Pango
  'PangoCoverage
  'coverage:ref
  'ref
  'pango_coverage_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoCoverage")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoCoverage
  'coverage:set
  'set
  'pango_coverage_set
  '((name . "set")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "index_")
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
           ((name . "level")
            (meta (arg-type . enum)
                  (gtype-name . "PangoCoverageLevel")
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

(^function
  'Pango
  'PangoCoverage
  'coverage:to-bytes
  'to-bytes
  'pango_coverage_to_bytes
  '((name . "to_bytes")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 2)
    (pdata ((name . "bytes")
            (meta (arg-type . array)
                  (flags ptr out sized)
                  (length-arg . 1)
                  (transfer . everything)
                  (params
                    ((arg-type . uint8)
                     (item-size . 1)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_bytes")
            (meta (arg-type . int32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoCoverage
  'coverage:unref
  'unref
  'pango_coverage_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoEllipsizeMode>
       "PangoEllipsizeMode"
       "pango_ellipsize_mode_get_type")

(^enum-conv 'ellipsize-mode 'PangoEllipsizeMode)

(^function
  'Pango
  'PangoFont
  'font:descriptions-free
  'descriptions-free
  'pango_font_descriptions_free
  '((name . "descriptions_free")
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "descs")
            (meta (arg-type . array)
                  (flags ptr in nullable sized)
                  (length-arg . 1)
                  (transfer . everything)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "PangoFontDescription")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_descs")
            (meta (arg-type . int32)
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
  'Pango
  'PangoFont
  'font:deserialize
  'deserialize
  'pango_font_deserialize
  '((name . "deserialize")
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
                  (flags ptr in)
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
            (gtype-name . "PangoFont")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:describe
  'describe
  'pango_font_describe
  '((name . "describe")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:describe-with-absolute-size
  'describe-with-absolute-size
  'pango_font_describe_with_absolute_size
  '((name . "describe_with_absolute_size")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:get-coverage
  'get-coverage
  'pango_font_get_coverage
  '((name . "get_coverage")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
            (gtype-name . "PangoCoverage")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFontFace>
       "PangoFontFace"
       "pango_font_face_get_type")

(^function
  'Pango
  'PangoFont
  'font:get-face
  'get-face
  'pango_font_get_face
  '((name . "get_face")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontFace")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'HarfBuzz
  '<hb_feature_t>
  "hb_feature_t"
  "hb_gobject_feature_get_type"
  16)

(^function
  'Pango
  'PangoFont
  'font:get-features!
  'get-features!
  'pango_font_get_features
  '((name . "get_features")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 3)
    (pdata ((name . "features")
            (meta (arg-type . array)
                  (flags ptr out preallocated sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "hb_feature_t")
                     (item-size . 16)
                     (transfer . nothing))))
            (s-direction . prealloc)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "len")
            (meta (arg-type . uint32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1))
           ((name . "num_features")
            (meta (arg-type . uint32)
                  (flags in out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 0)
            (c-output-pos . 2)
            (s-input-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:get-font-map
  'get-font-map
  'pango_font_get_font_map
  '((name . "get_font_map")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:get-languages
  'get-languages
  'pango_font_get_languages
  '((name . "get_languages")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out nullable zero-terminated)
            (transfer . nothing)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoLanguage")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:get-metrics
  'get-metrics
  'pango_font_get_metrics
  '((name . "get_metrics")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontMetrics")
            (flags ptr out)
            (item-size . 40)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFont
  'font:has-char?
  'has-char?
  'pango_font_has_char
  '((name . "has_char")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "wc")
            (meta (arg-type . unichar)
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
  'Pango
  'PangoFont
  'font:serialize
  'serialize
  'pango_font_serialize
  '((name . "serialize")
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
  'Pango
  'PangoFontDescription
  'font-description:new
  'new
  'pango_font_description_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:better-match?
  'better-match?
  'pango_font_description_better_match
  '((name . "better_match")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "old_match")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "new_match")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:copy
  'copy
  'pango_font_description_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:copy-static
  'copy-static
  'pango_font_description_copy_static
  '((name . "copy_static")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:equal?
  'equal?
  'pango_font_description_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "desc2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
  'Pango
  'PangoFontDescription
  'font-description:free
  'free
  'pango_font_description_free
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
  'Pango
  'PangoFontDescription
  'font-description:get-family
  'get-family
  'pango_font_description_get_family
  '((name . "get_family")
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
  'Pango
  'PangoFontDescription
  'font-description:get-gravity
  'get-gravity
  'pango_font_description_get_gravity
  '((name . "get_gravity")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFontMask>
       "PangoFontMask"
       "pango_font_mask_get_type")

(^flags-conv 'font-mask 'PangoFontMask)

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-set-fields
  'get-set-fields
  'pango_font_description_get_set_fields
  '((name . "get_set_fields")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . flags)
            (gtype-name . "PangoFontMask")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-size
  'get-size
  'pango_font_description_get_size
  '((name . "get_size")
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
  'Pango
  'PangoFontDescription
  'font-description:get-size-is-absolute?
  'get-size-is-absolute?
  'pango_font_description_get_size_is_absolute
  '((name . "get_size_is_absolute")
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

(^type 'Pango
       '<PangoStretch>
       "PangoStretch"
       "pango_stretch_get_type")

(^enum-conv 'stretch 'PangoStretch)

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-stretch
  'get-stretch
  'pango_font_description_get_stretch
  '((name . "get_stretch")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoStretch")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoStyle>
       "PangoStyle"
       "pango_style_get_type")

(^enum-conv 'style 'PangoStyle)

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-style
  'get-style
  'pango_font_description_get_style
  '((name . "get_style")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoStyle")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoVariant>
       "PangoVariant"
       "pango_variant_get_type")

(^enum-conv 'variant 'PangoVariant)

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-variant
  'get-variant
  'pango_font_description_get_variant
  '((name . "get_variant")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoVariant")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-variations
  'get-variations
  'pango_font_description_get_variations
  '((name . "get_variations")
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

(^type 'Pango
       '<PangoWeight>
       "PangoWeight"
       "pango_weight_get_type")

(^enum-conv 'weight 'PangoWeight)

(^function
  'Pango
  'PangoFontDescription
  'font-description:get-weight
  'get-weight
  'pango_font_description_get_weight
  '((name . "get_weight")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoWeight")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:hash
  'hash
  'pango_font_description_hash
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
  'Pango
  'PangoFontDescription
  'font-description:merge
  'merge
  'pango_font_description_merge
  '((name . "merge")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "desc_to_merge")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "replace_existing")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:merge-static
  'merge-static
  'pango_font_description_merge_static
  '((name . "merge_static")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "desc_to_merge")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "replace_existing")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontDescription
  'font-description:set-absolute-size
  'set-absolute-size
  'pango_font_description_set_absolute_size
  '((name . "set_absolute_size")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "size")
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
  'Pango
  'PangoFontDescription
  'font-description:set-family
  'set-family
  'pango_font_description_set_family
  '((name . "set_family")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "family")
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
  'Pango
  'PangoFontDescription
  'font-description:set-family-static
  'set-family-static
  'pango_font_description_set_family_static
  '((name . "set_family_static")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "family")
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
  'Pango
  'PangoFontDescription
  'font-description:set-gravity
  'set-gravity
  'pango_font_description_set_gravity
  '((name . "set_gravity")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
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
  'Pango
  'PangoFontDescription
  'font-description:set-size
  'set-size
  'pango_font_description_set_size
  '((name . "set_size")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "size")
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
  'Pango
  'PangoFontDescription
  'font-description:set-stretch
  'set-stretch
  'pango_font_description_set_stretch
  '((name . "set_stretch")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "stretch")
            (meta (arg-type . enum)
                  (gtype-name . "PangoStretch")
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
  'Pango
  'PangoFontDescription
  'font-description:set-style
  'set-style
  'pango_font_description_set_style
  '((name . "set_style")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "style")
            (meta (arg-type . enum)
                  (gtype-name . "PangoStyle")
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
  'Pango
  'PangoFontDescription
  'font-description:set-variant
  'set-variant
  'pango_font_description_set_variant
  '((name . "set_variant")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "variant")
            (meta (arg-type . enum)
                  (gtype-name . "PangoVariant")
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
  'Pango
  'PangoFontDescription
  'font-description:set-variations
  'set-variations
  'pango_font_description_set_variations
  '((name . "set_variations")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "variations")
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
  'Pango
  'PangoFontDescription
  'font-description:set-variations-static
  'set-variations-static
  'pango_font_description_set_variations_static
  '((name . "set_variations_static")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "variations")
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
  'Pango
  'PangoFontDescription
  'font-description:set-weight
  'set-weight
  'pango_font_description_set_weight
  '((name . "set_weight")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "weight")
            (meta (arg-type . enum)
                  (gtype-name . "PangoWeight")
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
  'Pango
  'PangoFontDescription
  'font-description:to-filename
  'to-filename
  'pango_font_description_to_filename
  '((name . "to_filename")
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

(^function
  'Pango
  'PangoFontDescription
  'font-description:to-string
  'to-string
  'pango_font_description_to_string
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
  'Pango
  'PangoFontDescription
  'font-description:unset-fields
  'unset-fields
  'pango_font_description_unset_fields
  '((name . "unset_fields")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "to_unset")
            (meta (arg-type . flags)
                  (gtype-name . "PangoFontMask")
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
  'Pango
  'PangoFontDescription
  'font-description:from-string
  'from-string
  'pango_font_description_from_string
  '((name . "from_string")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "str")
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
            (gtype-name . "PangoFontDescription")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontFace
  'font-face:describe
  'describe
  'pango_font_face_describe
  '((name . "describe")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontFace
  'font-face:get-face-name
  'get-face-name
  'pango_font_face_get_face_name
  '((name . "get_face_name")
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
  'Pango
  'PangoFontFace
  'font-face:get-family
  'get-family
  'pango_font_face_get_family
  '((name . "get_family")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontFamily")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontFace
  'font-face:is-synthesized?
  'is-synthesized?
  'pango_font_face_is_synthesized
  '((name . "is_synthesized")
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
  'Pango
  'PangoFontFace
  'font-face:list-sizes
  'list-sizes
  'pango_font_face_list_sizes
  '((name . "list_sizes")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 2)
    (pdata ((name . "sizes")
            (meta (arg-type . array)
                  (flags ptr out optional nullable sized)
                  (length-arg . 1)
                  (transfer . everything)
                  (params
                    ((arg-type . int32)
                     (item-size . 4)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_sizes")
            (meta (arg-type . int32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontFamily
  'font-family:get-face
  'get-face
  'pango_font_family_get_face
  '((name . "get_face")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "name")
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
            (gtype-name . "PangoFontFace")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontFamily
  'font-family:get-name
  'get-name
  'pango_font_family_get_name
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
  'Pango
  'PangoFontFamily
  'font-family:is-monospace?
  'is-monospace?
  'pango_font_family_is_monospace
  '((name . "is_monospace")
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
  'Pango
  'PangoFontFamily
  'font-family:is-variable?
  'is-variable?
  'pango_font_family_is_variable
  '((name . "is_variable")
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
  'Pango
  'PangoFontFamily
  'font-family:list-faces
  'list-faces
  'pango_font_family_list_faces
  '((name . "list_faces")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 2)
    (pdata ((name . "faces")
            (meta (arg-type . array)
                  (flags ptr out optional sized)
                  (length-arg . 1)
                  (transfer . container)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "PangoFontFace")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_faces")
            (meta (arg-type . int32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'PangoFontFamily
  'font-family:item-type
  'item-type
  'item-type)

(^property
  'PangoFontFamily
  'font-family:n-items
  'n-items
  'n-items)

(^function
  'Pango
  'PangoFontMap
  'font-map:changed
  'changed
  'pango_font_map_changed
  '((name . "changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontMap
  'font-map:create-context
  'create-context
  'pango_font_map_create_context
  '((name . "create_context")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoContext")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontMap
  'font-map:get-family
  'get-family
  'pango_font_map_get_family
  '((name . "get_family")
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
            (gtype-name . "PangoFontFamily")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontMap
  'font-map:get-serial
  'get-serial
  'pango_font_map_get_serial
  '((name . "get_serial")
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
  'Pango
  'PangoFontMap
  'font-map:list-families
  'list-families
  'pango_font_map_list_families
  '((name . "list_families")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 2)
    (pdata ((name . "families")
            (meta (arg-type . array)
                  (flags ptr out sized)
                  (length-arg . 1)
                  (transfer . container)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "PangoFontFamily")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_families")
            (meta (arg-type . int32)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontMap
  'font-map:load-font
  'load-font
  'pango_font_map_load_font
  '((name . "load_font")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
            (gtype-name . "PangoFont")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontMap
  'font-map:load-fontset
  'load-fontset
  'pango_font_map_load_fontset
  '((name . "load_fontset")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontset")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'PangoFontMap
  'font-map:item-type
  'item-type
  'item-type)

(^property
  'PangoFontMap
  'font-map:n-items
  'n-items
  'n-items)

(^function
  'Pango
  'PangoFontMetrics
  'font-metrics:get-approximate-char-width
  'get-approximate-char-width
  'pango_font_metrics_get_approximate_char_width
  '((name . "get_approximate_char_width")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-approximate-digit-width
  'get-approximate-digit-width
  'pango_font_metrics_get_approximate_digit_width
  '((name . "get_approximate_digit_width")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-ascent
  'get-ascent
  'pango_font_metrics_get_ascent
  '((name . "get_ascent")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-descent
  'get-descent
  'pango_font_metrics_get_descent
  '((name . "get_descent")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-height
  'get-height
  'pango_font_metrics_get_height
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-strikethrough-position
  'get-strikethrough-position
  'pango_font_metrics_get_strikethrough_position
  '((name . "get_strikethrough_position")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-strikethrough-thickness
  'get-strikethrough-thickness
  'pango_font_metrics_get_strikethrough_thickness
  '((name . "get_strikethrough_thickness")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-underline-position
  'get-underline-position
  'pango_font_metrics_get_underline_position
  '((name . "get_underline_position")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:get-underline-thickness
  'get-underline-thickness
  'pango_font_metrics_get_underline_thickness
  '((name . "get_underline_thickness")
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
  'Pango
  'PangoFontMetrics
  'font-metrics:ref
  'ref
  'pango_font_metrics_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontMetrics")
            (flags ptr out nullable)
            (item-size . 40)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontMetrics
  'font-metrics:unref
  'unref
  'pango_font_metrics_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFontScale>
       "PangoFontScale"
       "pango_font_scale_get_type")

(^enum-conv 'font-scale 'PangoFontScale)

(^function
  'Pango
  'PangoFontset
  'fontset:foreach
  'foreach
  'pango_fontset_foreach
  '((name . "foreach")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "func")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "FontsetForeachFunc")
                    (s-input-req . 2)
                    (s-input-opt . 1)
                    (c-input-len . 3)
                    (pdata ((name . "fontset")
                            (meta (arg-type . GObject)
                                  (gtype-name . "PangoFontset")
                                  (flags ptr in)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "font")
                            (meta (arg-type . GObject)
                                  (gtype-name . "PangoFont")
                                  (flags ptr in)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
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
  'Pango
  'PangoFontset
  'fontset:get-font
  'get-font
  'pango_fontset_get_font
  '((name . "get_font")
    (is-method . #t)
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
      (meta (arg-type . GObject)
            (gtype-name . "PangoFont")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontset
  'fontset:get-metrics
  'get-metrics
  'pango_fontset_get_metrics
  '((name . "get_metrics")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontMetrics")
            (flags ptr out)
            (item-size . 40)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoFontsetSimple>
       "PangoFontsetSimple"
       "pango_fontset_simple_get_type")

(^function
  'Pango
  'PangoFontsetSimple
  'fontset-simple:new
  'new
  'pango_fontset_simple_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
            (gtype-name . "PangoFontsetSimple")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoFontsetSimple
  'fontset-simple:append
  'append
  'pango_fontset_simple_append
  '((name . "append")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "font")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoFont")
                  (flags ptr in)
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
  'Pango
  'PangoFontsetSimple
  'fontset-simple:size
  'size
  'pango_fontset_simple_size
  '((name . "size")
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

(^constant 'GLYPH_EMPTY 268435455)

(^constant 'GLYPH_INVALID_INPUT 4294967295)

(^constant 'GLYPH_UNKNOWN_FLAG 268435456)

(^sized-type
  'Pango
  '<PangoGlyphItem>
  "PangoGlyphItem"
  "pango_glyph_item_get_type"
  32)

(^function
  'Pango
  'PangoGlyphItem
  'glyph-item:apply-attrs
  'apply-attrs
  'pango_glyph_item_apply_attrs
  '((name . "apply_attrs")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "text")
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
           ((name . "list")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
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
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . everything)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoGlyphItem")
               (item-size . 32)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphItem
  'glyph-item:copy
  'copy
  'pango_glyph_item_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphItem")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphItem
  'glyph-item:free
  'free
  'pango_glyph_item_free
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
  'Pango
  'PangoGlyphItem
  'glyph-item:get-logical-widths
  'get-logical-widths
  'pango_glyph_item_get_logical_widths
  '((name . "get_logical_widths")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "text")
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
           ((name . "logical_widths")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . int32)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Pango
  'PangoGlyphItem
  'glyph-item:split
  'split
  'pango_glyph_item_split
  '((name . "split")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "text")
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
           ((name . "split_index")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphItem")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Pango
  '<PangoGlyphItemIter>
  "PangoGlyphItemIter"
  "pango_glyph_item_iter_get_type"
  40)

(^function
  'Pango
  'PangoGlyphItemIter
  'glyph-item-iter:copy
  'copy
  'pango_glyph_item_iter_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphItemIter")
            (flags ptr out nullable)
            (item-size . 40)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphItemIter
  'glyph-item-iter:free
  'free
  'pango_glyph_item_iter_free
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
  'Pango
  'PangoGlyphItemIter
  'glyph-item-iter:init-end?
  'init-end?
  'pango_glyph_item_iter_init_end
  '((name . "init_end")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "glyph_item")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphItem")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "text")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphItemIter
  'glyph-item-iter:init-start?
  'init-start?
  'pango_glyph_item_iter_init_start
  '((name . "init_start")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "glyph_item")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphItem")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "text")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphItemIter
  'glyph-item-iter:next-cluster?
  'next-cluster?
  'pango_glyph_item_iter_next_cluster
  '((name . "next_cluster")
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
  'Pango
  'PangoGlyphItemIter
  'glyph-item-iter:prev-cluster?
  'prev-cluster?
  'pango_glyph_item_iter_prev_cluster
  '((name . "prev_cluster")
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

(^sized-type
  'Pango
  '<PangoGlyphString>
  "PangoGlyphString"
  "pango_glyph_string_get_type"
  32)

(^function
  'Pango
  'PangoGlyphString
  'glyph-string:new
  'new
  'pango_glyph_string_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphString")
            (flags ptr out)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphString
  'glyph-string:copy
  'copy
  'pango_glyph_string_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphString")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGlyphString
  'glyph-string:free
  'free
  'pango_glyph_string_free
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
  'Pango
  'PangoGlyphString
  'glyph-string:get-logical-widths
  'get-logical-widths
  'pango_glyph_string_get_logical_widths
  '((name . "get_logical_widths")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "text")
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
           ((name . "length")
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
           ((name . "embedding_level")
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
           ((name . "logical_widths")
            (meta (arg-type . pointer)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . int32)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Pango
  'PangoGlyphString
  'glyph-string:get-width
  'get-width
  'pango_glyph_string_get_width
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
  'Pango
  'PangoGlyphString
  'glyph-string:set-size
  'set-size
  'pango_glyph_string_set_size
  '((name . "set_size")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "new_len")
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
  'Pango
  'PangoGravity
  'gravity:get-for-matrix
  'get-for-matrix
  'pango_gravity_get_for_matrix
  '((name . "get_for_matrix")
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoMatrix")
                  (flags ptr in nullable)
                  (item-size . 48)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoScript>
       "PangoScript"
       "pango_script_get_type")

(^enum-conv 'script 'PangoScript)

(^function
  'Pango
  'PangoGravity
  'gravity:get-for-script
  'get-for-script
  'pango_gravity_get_for_script
  '((name . "get_for_script")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "base_gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "hint")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravityHint")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGravity
  'gravity:get-for-script-and-width
  'get-for-script-and-width
  'pango_gravity_get_for_script_and_width
  '((name . "get_for_script_and_width")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "wide")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "base_gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "hint")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravityHint")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoGravity
  'gravity:to-rotation
  'to-rotation
  'pango_gravity_to_rotation
  '((name . "to_rotation")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
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
      (meta (arg-type . double)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Pango
  '<PangoItem>
  "PangoItem"
  "pango_item_get_type"
  64)

(^function
  'Pango
  'PangoItem
  'item:new
  'new
  'pango_item_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoItem")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoItem
  'item:apply-attrs
  'apply-attrs
  'pango_item_apply_attrs
  '((name . "apply_attrs")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "iter")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrIterator")
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
  'Pango
  'PangoItem
  'item:copy
  'copy
  'pango_item_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoItem")
            (flags ptr out nullable)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoItem
  'item:free
  'free
  'pango_item_free
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
  'Pango
  'PangoItem
  'item:split
  'split
  'pango_item_split
  '((name . "split")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "split_index")
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
           ((name . "split_offset")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoItem")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLanguage
  'language:get-sample-string
  'get-sample-string
  'pango_language_get_sample_string
  '((name . "get_sample_string")
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
  'Pango
  'PangoLanguage
  'language:get-scripts
  'get-scripts
  'pango_language_get_scripts
  '((name . "get_scripts")
    (is-method . #t)
    (c-output-len . 1)
    (pdata ((name . "num_scripts")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out nullable sized)
            (transfer . nothing)
            (params
              ((arg-type . enum)
               (gtype-name . "PangoScript")
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLanguage
  'language:includes-script?
  'includes-script?
  'pango_language_includes_script
  '((name . "includes_script")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
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
  'Pango
  'PangoLanguage
  'language:matches?
  'matches?
  'pango_language_matches
  '((name . "matches")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "range_list")
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
  'Pango
  'PangoLanguage
  'language:to-string
  'to-string
  'pango_language_to_string
  '((name . "to_string")
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
  'Pango
  'PangoLanguage
  'language:from-string
  'from-string
  'pango_language_from_string
  '((name . "from_string")
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLanguage
  'language:get-default
  'get-default
  'pango_language_get_default
  '((name . "get_default")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLanguage
  'language:get-preferred
  'get-preferred
  'pango_language_get_preferred
  '((name . "get_preferred")
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out nullable zero-terminated)
            (transfer . nothing)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoLanguage")
               (flags ptr)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoLayout>
       "PangoLayout"
       "pango_layout_get_type")

(^function
  'Pango
  'PangoLayout
  'layout:new
  'new
  'pango_layout_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
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
            (gtype-name . "PangoLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoLayoutDeserializeFlags>
       "PangoLayoutDeserializeFlags"
       "pango_layout_deserialize_flags_get_type")

(^flags-conv
  'layout-deserialize-flags
  'PangoLayoutDeserializeFlags)

(^function
  'Pango
  'PangoLayout
  'layout:deserialize
  'deserialize
  'pango_layout_deserialize
  '((name . "deserialize")
    (can-throw-gerror . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
                  (flags ptr in)
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
            (s-input-pos . 1))
           ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "PangoLayoutDeserializeFlags")
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
      (meta (arg-type . GObject)
            (gtype-name . "PangoLayout")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:context-changed
  'context-changed
  'pango_layout_context_changed
  '((name . "context_changed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:copy
  'copy
  'pango_layout_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoLayout")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-alignment
  'get-alignment
  'pango_layout_get_alignment
  '((name . "get_alignment")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoAlignment")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-attributes
  'get-attributes
  'pango_layout_get_attributes
  '((name . "get_attributes")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttrList")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-auto-dir?
  'get-auto-dir?
  'pango_layout_get_auto_dir
  '((name . "get_auto_dir")
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
  'Pango
  'PangoLayout
  'layout:get-baseline
  'get-baseline
  'pango_layout_get_baseline
  '((name . "get_baseline")
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
  'Pango
  'PangoLayout
  'layout:get-character-count
  'get-character-count
  'pango_layout_get_character_count
  '((name . "get_character_count")
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
  'Pango
  'PangoLayout
  'layout:get-context
  'get-context
  'pango_layout_get_context
  '((name . "get_context")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoContext")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-direction
  'get-direction
  'pango_layout_get_direction
  '((name . "get_direction")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "index")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoDirection")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-ellipsize
  'get-ellipsize
  'pango_layout_get_ellipsize
  '((name . "get_ellipsize")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoEllipsizeMode")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-font-description
  'get-font-description
  'pango_layout_get_font_description
  '((name . "get_font_description")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoFontDescription")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-height
  'get-height
  'pango_layout_get_height
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
  'Pango
  'PangoLayout
  'layout:get-indent
  'get-indent
  'pango_layout_get_indent
  '((name . "get_indent")
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

(^type 'Pango
       '<PangoLayoutIter>
       "PangoLayoutIter"
       "pango_layout_iter_get_type")

(^function
  'Pango
  'PangoLayout
  'layout:get-iter
  'get-iter
  'pango_layout_get_iter
  '((name . "get_iter")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutIter")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-justify?
  'get-justify?
  'pango_layout_get_justify
  '((name . "get_justify")
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
  'Pango
  'PangoLayout
  'layout:get-justify-last-line?
  'get-justify-last-line?
  'pango_layout_get_justify_last_line
  '((name . "get_justify_last_line")
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

(^sized-type
  'Pango
  '<PangoLayoutLine>
  "PangoLayoutLine"
  "pango_layout_line_get_type"
  32)

(^function
  'Pango
  'PangoLayout
  'layout:get-line
  'get-line
  'pango_layout_get_line
  '((name . "get_line")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "line")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutLine")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-line-count
  'get-line-count
  'pango_layout_get_line_count
  '((name . "get_line_count")
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
  'Pango
  'PangoLayout
  'layout:get-line-readonly
  'get-line-readonly
  'pango_layout_get_line_readonly
  '((name . "get_line_readonly")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "line")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutLine")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-line-spacing
  'get-line-spacing
  'pango_layout_get_line_spacing
  '((name . "get_line_spacing")
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
  'Pango
  'PangoLayout
  'layout:get-lines
  'get-lines
  'pango_layout_get_lines
  '((name . "get_lines")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . nothing)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoLayoutLine")
               (item-size . 32)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-lines-readonly
  'get-lines-readonly
  'pango_layout_get_lines_readonly
  '((name . "get_lines_readonly")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . nothing)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoLayoutLine")
               (item-size . 32)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-pixel-size
  'get-pixel-size
  'pango_layout_get_pixel_size
  '((name . "get_pixel_size")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "width")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "height")
            (meta (arg-type . int32)
                  (flags out optional)
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
  'Pango
  'PangoLayout
  'layout:get-serial
  'get-serial
  'pango_layout_get_serial
  '((name . "get_serial")
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
  'Pango
  'PangoLayout
  'layout:get-single-paragraph-mode?
  'get-single-paragraph-mode?
  'pango_layout_get_single_paragraph_mode
  '((name . "get_single_paragraph_mode")
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
  'Pango
  'PangoLayout
  'layout:get-size
  'get-size
  'pango_layout_get_size
  '((name . "get_size")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "width")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "height")
            (meta (arg-type . int32)
                  (flags out optional)
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
  'Pango
  'PangoLayout
  'layout:get-spacing
  'get-spacing
  'pango_layout_get_spacing
  '((name . "get_spacing")
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

(^type 'Pango
       '<PangoTabArray>
       "PangoTabArray"
       "pango_tab_array_get_type")

(^function
  'Pango
  'PangoLayout
  'layout:get-tabs
  'get-tabs
  'pango_layout_get_tabs
  '((name . "get_tabs")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoTabArray")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:get-text
  'get-text
  'pango_layout_get_text
  '((name . "get_text")
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
  'Pango
  'PangoLayout
  'layout:get-unknown-glyphs-count
  'get-unknown-glyphs-count
  'pango_layout_get_unknown_glyphs_count
  '((name . "get_unknown_glyphs_count")
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
  'Pango
  'PangoLayout
  'layout:get-width
  'get-width
  'pango_layout_get_width
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

(^type 'Pango
       '<PangoWrapMode>
       "PangoWrapMode"
       "pango_wrap_mode_get_type")

(^enum-conv 'wrap-mode 'PangoWrapMode)

(^function
  'Pango
  'PangoLayout
  'layout:get-wrap
  'get-wrap
  'pango_layout_get_wrap
  '((name . "get_wrap")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoWrapMode")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:index-to-line-x
  'index-to-line-x
  'pango_layout_index_to_line_x
  '((name . "index_to_line_x")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "index_")
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
           ((name . "trailing")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "line")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "x_pos")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
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
  'Pango
  'PangoLayout
  'layout:is-ellipsized?
  'is-ellipsized?
  'pango_layout_is_ellipsized
  '((name . "is_ellipsized")
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
  'Pango
  'PangoLayout
  'layout:is-wrapped?
  'is-wrapped?
  'pango_layout_is_wrapped
  '((name . "is_wrapped")
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
  'Pango
  'PangoLayout
  'layout:move-cursor-visually
  'move-cursor-visually
  'pango_layout_move_cursor_visually
  '((name . "move_cursor_visually")
    (is-method . #t)
    (s-input-req . 4)
    (s-output-len . 2)
    (c-input-len . 4)
    (c-output-len . 2)
    (pdata ((name . "strong")
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
           ((name . "old_index")
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
           ((name . "old_trailing")
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
           ((name . "direction")
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
           ((name . "new_index")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "new_trailing")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
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

(^type 'Pango
       '<PangoLayoutSerializeFlags>
       "PangoLayoutSerializeFlags"
       "pango_layout_serialize_flags_get_type")

(^flags-conv
  'layout-serialize-flags
  'PangoLayoutSerializeFlags)

(^function
  'Pango
  'PangoLayout
  'layout:serialize
  'serialize
  'pango_layout_serialize
  '((name . "serialize")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "PangoLayoutSerializeFlags")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GBytes")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:set-alignment
  'set-alignment
  'pango_layout_set_alignment
  '((name . "set_alignment")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "alignment")
            (meta (arg-type . enum)
                  (gtype-name . "PangoAlignment")
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
  'Pango
  'PangoLayout
  'layout:set-attributes
  'set-attributes
  'pango_layout_set_attributes
  '((name . "set_attributes")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "attrs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
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
  'Pango
  'PangoLayout
  'layout:set-auto-dir
  'set-auto-dir
  'pango_layout_set_auto_dir
  '((name . "set_auto_dir")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "auto_dir")
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
  'Pango
  'PangoLayout
  'layout:set-ellipsize
  'set-ellipsize
  'pango_layout_set_ellipsize
  '((name . "set_ellipsize")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "ellipsize")
            (meta (arg-type . enum)
                  (gtype-name . "PangoEllipsizeMode")
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
  'Pango
  'PangoLayout
  'layout:set-font-description
  'set-font-description
  'pango_layout_set_font_description
  '((name . "set_font_description")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
  'Pango
  'PangoLayout
  'layout:set-height
  'set-height
  'pango_layout_set_height
  '((name . "set_height")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "height")
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
  'Pango
  'PangoLayout
  'layout:set-indent
  'set-indent
  'pango_layout_set_indent
  '((name . "set_indent")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "indent")
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
  'Pango
  'PangoLayout
  'layout:set-justify
  'set-justify
  'pango_layout_set_justify
  '((name . "set_justify")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "justify")
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
  'Pango
  'PangoLayout
  'layout:set-justify-last-line
  'set-justify-last-line
  'pango_layout_set_justify_last_line
  '((name . "set_justify_last_line")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "justify")
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
  'Pango
  'PangoLayout
  'layout:set-line-spacing
  'set-line-spacing
  'pango_layout_set_line_spacing
  '((name . "set_line_spacing")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "factor")
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
  'Pango
  'PangoLayout
  'layout:set-markup
  'set-markup
  'pango_layout_set_markup
  '((name . "set_markup")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "markup")
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
           ((name . "length")
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
  'Pango
  'PangoLayout
  'layout:set-markup-with-accel
  'set-markup-with-accel
  'pango_layout_set_markup_with_accel
  '((name . "set_markup_with_accel")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 3)
    (c-output-len . 1)
    (pdata ((name . "markup")
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
           ((name . "length")
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
           ((name . "accel_marker")
            (meta (arg-type . unichar)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "accel_char")
            (meta (arg-type . unichar)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:set-single-paragraph-mode
  'set-single-paragraph-mode
  'pango_layout_set_single_paragraph_mode
  '((name . "set_single_paragraph_mode")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "setting")
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
  'Pango
  'PangoLayout
  'layout:set-spacing
  'set-spacing
  'pango_layout_set_spacing
  '((name . "set_spacing")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "spacing")
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
  'Pango
  'PangoLayout
  'layout:set-tabs
  'set-tabs
  'pango_layout_set_tabs
  '((name . "set_tabs")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "tabs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoTabArray")
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
  'Pango
  'PangoLayout
  'layout:set-text
  'set-text
  'pango_layout_set_text
  '((name . "set_text")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "text")
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
           ((name . "length")
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
  'Pango
  'PangoLayout
  'layout:set-width
  'set-width
  'pango_layout_set_width
  '((name . "set_width")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
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
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:set-wrap
  'set-wrap
  'pango_layout_set_wrap
  '((name . "set_wrap")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "wrap")
            (meta (arg-type . enum)
                  (gtype-name . "PangoWrapMode")
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
  'Pango
  'PangoLayout
  'layout:write-to-file?
  'write-to-file?
  'pango_layout_write_to_file
  '((name . "write_to_file")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "PangoLayoutSerializeFlags")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "filename")
            (meta (arg-type . locale-string)
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayout
  'layout:xy-to-index
  'xy-to-index
  'pango_layout_xy_to_index
  '((name . "xy_to_index")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
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
            (s-input-pos . 1))
           ((name . "index_")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "trailing")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
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

(^type 'Pango
       '<PangoLayoutDeserializeError>
       "PangoLayoutDeserializeError"
       "pango_layout_deserialize_error_get_type")

(^enum-conv
  'layout-deserialize-error
  'PangoLayoutDeserializeError)

(^function
  'Pango
  'PangoLayoutDeserializeError
  'layout-deserialize-error:quark
  'quark
  'pango_layout_deserialize_error_quark
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
  'Pango
  'PangoLayoutIter
  'layout-iter:at-last-line?
  'at-last-line?
  'pango_layout_iter_at_last_line
  '((name . "at_last_line")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:copy
  'copy
  'pango_layout_iter_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutIter")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutIter
  'layout-iter:free
  'free
  'pango_layout_iter_free
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
  'Pango
  'PangoLayoutIter
  'layout-iter:get-baseline
  'get-baseline
  'pango_layout_iter_get_baseline
  '((name . "get_baseline")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:get-index
  'get-index
  'pango_layout_iter_get_index
  '((name . "get_index")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:get-layout
  'get-layout
  'pango_layout_iter_get_layout
  '((name . "get_layout")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoLayout")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutIter
  'layout-iter:get-line
  'get-line
  'pango_layout_iter_get_line
  '((name . "get_line")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutLine")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutIter
  'layout-iter:get-line-readonly
  'get-line-readonly
  'pango_layout_iter_get_line_readonly
  '((name . "get_line_readonly")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutLine")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutIter
  'layout-iter:get-line-yrange
  'get-line-yrange
  'pango_layout_iter_get_line_yrange
  '((name . "get_line_yrange")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "y0_")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "y1_")
            (meta (arg-type . int32)
                  (flags out optional)
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
  'Pango
  'PangoLayoutIter
  'layout-iter:get-run
  'get-run
  'pango_layout_iter_get_run
  '((name . "get_run")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphItem")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutIter
  'layout-iter:get-run-baseline
  'get-run-baseline
  'pango_layout_iter_get_run_baseline
  '((name . "get_run_baseline")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:get-run-readonly
  'get-run-readonly
  'pango_layout_iter_get_run_readonly
  '((name . "get_run_readonly")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoGlyphItem")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutIter
  'layout-iter:next-char?
  'next-char?
  'pango_layout_iter_next_char
  '((name . "next_char")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:next-cluster?
  'next-cluster?
  'pango_layout_iter_next_cluster
  '((name . "next_cluster")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:next-line?
  'next-line?
  'pango_layout_iter_next_line
  '((name . "next_line")
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
  'Pango
  'PangoLayoutIter
  'layout-iter:next-run?
  'next-run?
  'pango_layout_iter_next_run
  '((name . "next_run")
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
  'Pango
  'PangoLayoutLine
  'layout-line:get-height
  'get-height
  'pango_layout_line_get_height
  '((name . "get_height")
    (is-method . #t)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "height")
            (meta (arg-type . int32)
                  (flags out optional)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutLine
  'layout-line:get-length
  'get-length
  'pango_layout_line_get_length
  '((name . "get_length")
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
  'Pango
  'PangoLayoutLine
  'layout-line:get-resolved-direction
  'get-resolved-direction
  'pango_layout_line_get_resolved_direction
  '((name . "get_resolved_direction")
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
  'Pango
  'PangoLayoutLine
  'layout-line:get-start-index
  'get-start-index
  'pango_layout_line_get_start_index
  '((name . "get_start_index")
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
  'Pango
  'PangoLayoutLine
  'layout-line:get-x-ranges
  'get-x-ranges
  'pango_layout_line_get_x_ranges
  '((name . "get_x_ranges")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "start_index")
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
           ((name . "end_index")
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
           ((name . "ranges")
            (meta (arg-type . array)
                  (flags ptr out sized)
                  (length-arg . 3)
                  (transfer . everything)
                  (params
                    ((arg-type . int32)
                     (item-size . 4)
                     (transfer . nothing))))
            (s-direction . output)
            (tuple . array)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "n_ranges")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutLine
  'layout-line:index-to-x
  'index-to-x
  'pango_layout_line_index_to_x
  '((name . "index_to_x")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "index_")
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
           ((name . "trailing")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "x_pos")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutLine
  'layout-line:is-paragraph-start?
  'is-paragraph-start?
  'pango_layout_line_is_paragraph_start
  '((name . "is_paragraph_start")
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
  'Pango
  'PangoLayoutLine
  'layout-line:ref
  'ref
  'pango_layout_line_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutLine")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoLayoutLine
  'layout-line:unref
  'unref
  'pango_layout_line_unref
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
  'Pango
  'PangoLayoutLine
  'layout-line:x-to-index
  'x-to-index
  'pango_layout_line_x_to_index
  '((name . "x_to_index")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "x_pos")
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
           ((name . "index_")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "trailing")
            (meta (arg-type . int32)
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoMatrix
  'matrix:concat
  'concat
  'pango_matrix_concat
  '((name . "concat")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "new_matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoMatrix")
                  (flags ptr in)
                  (item-size . 48)
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
  'Pango
  'PangoMatrix
  'matrix:copy
  'copy
  'pango_matrix_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoMatrix")
            (flags ptr out nullable)
            (item-size . 48)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoMatrix
  'matrix:free
  'free
  'pango_matrix_free
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
  'Pango
  'PangoMatrix
  'matrix:get-font-scale-factor
  'get-font-scale-factor
  'pango_matrix_get_font_scale_factor
  '((name . "get_font_scale_factor")
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
  'Pango
  'PangoMatrix
  'matrix:get-font-scale-factors
  'get-font-scale-factors
  'pango_matrix_get_font_scale_factors
  '((name . "get_font_scale_factors")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "xscale")
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
           ((name . "yscale")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoMatrix
  'matrix:get-slant-ratio
  'get-slant-ratio
  'pango_matrix_get_slant_ratio
  '((name . "get_slant_ratio")
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
  'Pango
  'PangoMatrix
  'matrix:rotate
  'rotate
  'pango_matrix_rotate
  '((name . "rotate")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "degrees")
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
  'Pango
  'PangoMatrix
  'matrix:scale
  'scale
  'pango_matrix_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "scale_x")
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
           ((name . "scale_y")
            (meta (arg-type . double)
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
  'Pango
  'PangoMatrix
  'matrix:transform-distance
  'transform-distance
  'pango_matrix_transform_distance
  '((name . "transform_distance")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "dx")
            (meta (arg-type . double)
                  (flags in out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "dy")
            (meta (arg-type . double)
                  (flags in out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (c-output-pos . 1)
            (s-input-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoMatrix
  'matrix:transform-point
  'transform-point
  'pango_matrix_transform_point
  '((name . "transform_point")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "x")
            (meta (arg-type . double)
                  (flags in out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "y")
            (meta (arg-type . double)
                  (flags in out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (c-output-pos . 1)
            (s-input-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoMatrix
  'matrix:translate
  'translate
  'pango_matrix_translate
  '((name . "translate")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "tx")
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
           ((name . "ty")
            (meta (arg-type . double)
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

(^type 'Pango
       '<PangoOverline>
       "PangoOverline"
       "pango_overline_get_type")

(^enum-conv 'overline 'PangoOverline)

(^type 'Pango
       '<PangoRenderPart>
       "PangoRenderPart"
       "pango_render_part_get_type")

(^enum-conv 'render-part 'PangoRenderPart)

(^type 'Pango
       '<PangoRenderer>
       "PangoRenderer"
       "pango_renderer_get_type")

(^function
  'Pango
  'PangoRenderer
  'renderer:activate
  'activate
  'pango_renderer_activate
  '((name . "activate")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:deactivate
  'deactivate
  'pango_renderer_deactivate
  '((name . "deactivate")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:draw-error-underline
  'draw-error-underline
  'pango_renderer_draw_error_underline
  '((name . "draw_error_underline")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
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
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:draw-glyph
  'draw-glyph
  'pango_renderer_draw_glyph
  '((name . "draw_glyph")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "font")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoFont")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "glyph")
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
            (s-input-pos . 3)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:draw-glyph-item
  'draw-glyph-item
  'pango_renderer_draw_glyph_item
  '((name . "draw_glyph_item")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "text")
            (meta (arg-type . utf8-string)
                  (flags ptr in nullable)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "glyph_item")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphItem")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "x")
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
           ((name . "y")
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
  'Pango
  'PangoRenderer
  'renderer:draw-glyphs
  'draw-glyphs
  'pango_renderer_draw_glyphs
  '((name . "draw_glyphs")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "font")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoFont")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "glyphs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphString")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "x")
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
           ((name . "y")
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
  'Pango
  'PangoRenderer
  'renderer:draw-layout
  'draw-layout
  'pango_renderer_draw_layout
  '((name . "draw_layout")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "layout")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoLayout")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "x")
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
           ((name . "y")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:draw-layout-line
  'draw-layout-line
  'pango_renderer_draw_layout_line
  '((name . "draw_layout_line")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "line")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLayoutLine")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "x")
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
           ((name . "y")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:draw-rectangle
  'draw-rectangle
  'pango_renderer_draw_rectangle
  '((name . "draw_rectangle")
    (is-method . #t)
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "x")
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
           ((name . "y")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:draw-trapezoid
  'draw-trapezoid
  'pango_renderer_draw_trapezoid
  '((name . "draw_trapezoid")
    (is-method . #t)
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "y1_")
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
           ((name . "x11")
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
           ((name . "x21")
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
           ((name . "y2")
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
           ((name . "x12")
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
           ((name . "x22")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
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
  'Pango
  'PangoRenderer
  'renderer:get-alpha
  'get-alpha
  'pango_renderer_get_alpha
  '((name . "get_alpha")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
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
      (meta (arg-type . uint16)
            (flags out)
            (item-size . 2)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:get-color
  'get-color
  'pango_renderer_get_color
  '((name . "get_color")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoColor")
            (flags ptr out nullable)
            (item-size . 6)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:get-layout
  'get-layout
  'pango_renderer_get_layout
  '((name . "get_layout")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoLayout")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:get-layout-line
  'get-layout-line
  'pango_renderer_get_layout_line
  '((name . "get_layout_line")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLayoutLine")
            (flags ptr out nullable)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:get-matrix
  'get-matrix
  'pango_renderer_get_matrix
  '((name . "get_matrix")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoMatrix")
            (flags ptr out nullable)
            (item-size . 48)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoRenderer
  'renderer:part-changed
  'part-changed
  'pango_renderer_part_changed
  '((name . "part_changed")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
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
  'Pango
  'PangoRenderer
  'renderer:set-alpha
  'set-alpha
  'pango_renderer_set_alpha
  '((name . "set_alpha")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "alpha")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
  'Pango
  'PangoRenderer
  'renderer:set-color
  'set-color
  'pango_renderer_set_color
  '((name . "set_color")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "part")
            (meta (arg-type . enum)
                  (gtype-name . "PangoRenderPart")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "color")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoColor")
                  (flags ptr in nullable)
                  (item-size . 6)
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
  'Pango
  'PangoRenderer
  'renderer:set-matrix
  'set-matrix
  'pango_renderer_set_matrix
  '((name . "set_matrix")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoMatrix")
                  (flags ptr in nullable)
                  (item-size . 48)
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

(^constant 'SCALE 1024)

(^function
  'Pango
  'PangoScript
  'script:for-unichar
  'for-unichar
  'pango_script_for_unichar
  '((name . "for_unichar")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "ch")
            (meta (arg-type . unichar)
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
      (meta (arg-type . enum)
            (gtype-name . "PangoScript")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoScript
  'script:get-sample-language
  'get-sample-language
  'pango_script_get_sample_language
  '((name . "get_sample_language")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoScriptIter>
       "PangoScriptIter"
       "pango_script_iter_get_type")

(^function
  'Pango
  'PangoScriptIter
  'script-iter:new
  'new
  'pango_script_iter_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "text")
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
           ((name . "length")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoScriptIter")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoScriptIter
  'script-iter:free
  'free
  'pango_script_iter_free
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
  'Pango
  'PangoScriptIter
  'script-iter:get-range
  'get-range
  'pango_script_iter_get_range
  '((name . "get_range")
    (is-method . #t)
    (s-output-len . 3)
    (c-output-len . 3)
    (pdata ((name . "start")
            (meta (arg-type . utf8-string)
                  (flags ptr out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "end")
            (meta (arg-type . utf8-string)
                  (flags ptr out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
                  (flags out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 2)
            (s-output-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoScriptIter
  'script-iter:next?
  'next?
  'pango_script_iter_next
  '((name . "next")
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

(^type 'Pango
       '<PangoShapeFlags>
       "PangoShapeFlags"
       "pango_shape_flags_get_type")

(^flags-conv 'shape-flags 'PangoShapeFlags)

(^type 'Pango
       '<PangoShowFlags>
       "PangoShowFlags"
       "pango_show_flags_get_type")

(^flags-conv 'show-flags 'PangoShowFlags)

(^type 'Pango
       '<PangoTabAlign>
       "PangoTabAlign"
       "pango_tab_align_get_type")

(^enum-conv 'tab-align 'PangoTabAlign)

(^function
  'Pango
  'PangoTabArray
  'tab-array:new
  'new
  'pango_tab_array_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "initial_size")
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
           ((name . "positions_in_pixels")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoTabArray")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoTabArray
  'tab-array:copy
  'copy
  'pango_tab_array_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoTabArray")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoTabArray
  'tab-array:free
  'free
  'pango_tab_array_free
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
  'Pango
  'PangoTabArray
  'tab-array:get-decimal-point
  'get-decimal-point
  'pango_tab_array_get_decimal_point
  '((name . "get_decimal_point")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "tab_index")
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
      (meta (arg-type . unichar)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoTabArray
  'tab-array:get-positions-in-pixels?
  'get-positions-in-pixels?
  'pango_tab_array_get_positions_in_pixels
  '((name . "get_positions_in_pixels")
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
  'Pango
  'PangoTabArray
  'tab-array:get-size
  'get-size
  'pango_tab_array_get_size
  '((name . "get_size")
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
  'Pango
  'PangoTabArray
  'tab-array:get-tab
  'get-tab
  'pango_tab_array_get_tab
  '((name . "get_tab")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "tab_index")
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
           ((name . "alignment")
            (meta (arg-type . enum)
                  (gtype-name . "PangoTabAlign")
                  (flags out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "location")
            (meta (arg-type . int32)
                  (flags out optional)
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
  'Pango
  'PangoTabArray
  'tab-array:get-tabs
  'get-tabs
  'pango_tab_array_get_tabs
  '((name . "get_tabs")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "alignments")
            (meta (arg-type . enum)
                  (gtype-name . "PangoTabAlign")
                  (flags ptr out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "locations")
            (meta (arg-type . pointer)
                  (flags ptr out optional)
                  (transfer . everything)
                  (params
                    ((arg-type . int32)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Pango
  'PangoTabArray
  'tab-array:resize
  'resize
  'pango_tab_array_resize
  '((name . "resize")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "new_size")
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
  'Pango
  'PangoTabArray
  'tab-array:set-decimal-point
  'set-decimal-point
  'pango_tab_array_set_decimal_point
  '((name . "set_decimal_point")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "tab_index")
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
           ((name . "decimal_point")
            (meta (arg-type . unichar)
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
  'Pango
  'PangoTabArray
  'tab-array:set-positions-in-pixels
  'set-positions-in-pixels
  'pango_tab_array_set_positions_in_pixels
  '((name . "set_positions_in_pixels")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "positions_in_pixels")
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
  'Pango
  'PangoTabArray
  'tab-array:set-tab
  'set-tab
  'pango_tab_array_set_tab
  '((name . "set_tab")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "tab_index")
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
           ((name . "alignment")
            (meta (arg-type . enum)
                  (gtype-name . "PangoTabAlign")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "location")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoTabArray
  'tab-array:sort
  'sort
  'pango_tab_array_sort
  '((name . "sort")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  'PangoTabArray
  'tab-array:to-string
  'to-string
  'pango_tab_array_to_string
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
  'Pango
  'PangoTabArray
  'tab-array:from-string
  'from-string
  'pango_tab_array_from_string
  '((name . "from_string")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "text")
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
            (gtype-name . "PangoTabArray")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoTextTransform>
       "PangoTextTransform"
       "pango_text_transform_get_type")

(^enum-conv 'text-transform 'PangoTextTransform)

(^type 'Pango
       '<PangoUnderline>
       "PangoUnderline"
       "pango_underline_get_type")

(^enum-conv 'underline 'PangoUnderline)

(^constant 'VERSION_MAJOR 1)

(^constant 'VERSION_MICRO 14)

(^constant 'VERSION_MINOR 50)

(^constant 'VERSION_STRING "1.50.14")

(^function
  'Pango
  #f
  'pango:attr-allow-breaks-new
  'attr-allow-breaks-new
  'pango_attr_allow_breaks_new
  '((name . "attr_allow_breaks_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "allow_breaks")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-background-alpha-new
  'attr-background-alpha-new
  'pango_attr_background_alpha_new
  '((name . "attr_background_alpha_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "alpha")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-background-new
  'attr-background-new
  'pango_attr_background_new
  '((name . "attr_background_new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "red")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "green")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blue")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-baseline-shift-new
  'attr-baseline-shift-new
  'pango_attr_baseline_shift_new
  '((name . "attr_baseline_shift_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "shift")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-fallback-new
  'attr-fallback-new
  'pango_attr_fallback_new
  '((name . "attr_fallback_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "enable_fallback")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-family-new
  'attr-family-new
  'pango_attr_family_new
  '((name . "attr_family_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "family")
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-font-desc-new
  'attr-font-desc-new
  'pango_attr_font_desc_new
  '((name . "attr_font_desc_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "desc")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoFontDescription")
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-font-features-new
  'attr-font-features-new
  'pango_attr_font_features_new
  '((name . "attr_font_features_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "features")
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-font-scale-new
  'attr-font-scale-new
  'pango_attr_font_scale_new
  '((name . "attr_font_scale_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "scale")
            (meta (arg-type . enum)
                  (gtype-name . "PangoFontScale")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-foreground-alpha-new
  'attr-foreground-alpha-new
  'pango_attr_foreground_alpha_new
  '((name . "attr_foreground_alpha_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "alpha")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-foreground-new
  'attr-foreground-new
  'pango_attr_foreground_new
  '((name . "attr_foreground_new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "red")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "green")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blue")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-gravity-hint-new
  'attr-gravity-hint-new
  'pango_attr_gravity_hint_new
  '((name . "attr_gravity_hint_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "hint")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravityHint")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-gravity-new
  'attr-gravity-new
  'pango_attr_gravity_new
  '((name . "attr_gravity_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-insert-hyphens-new
  'attr-insert-hyphens-new
  'pango_attr_insert_hyphens_new
  '((name . "attr_insert_hyphens_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "insert_hyphens")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-language-new
  'attr-language-new
  'pango_attr_language_new
  '((name . "attr_language_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLanguage")
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-letter-spacing-new
  'attr-letter-spacing-new
  'pango_attr_letter_spacing_new
  '((name . "attr_letter_spacing_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "letter_spacing")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-line-height-new
  'attr-line-height-new
  'pango_attr_line_height_new
  '((name . "attr_line_height_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "factor")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-line-height-new-absolute
  'attr-line-height-new-absolute
  'pango_attr_line_height_new_absolute
  '((name . "attr_line_height_new_absolute")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "height")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-list-from-string
  'attr-list-from-string
  'pango_attr_list_from_string
  '((name . "attr_list_from_string")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "text")
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
            (gtype-name . "PangoAttrList")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-overline-color-new
  'attr-overline-color-new
  'pango_attr_overline_color_new
  '((name . "attr_overline_color_new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "red")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "green")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blue")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-overline-new
  'attr-overline-new
  'pango_attr_overline_new
  '((name . "attr_overline_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "overline")
            (meta (arg-type . enum)
                  (gtype-name . "PangoOverline")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-rise-new
  'attr-rise-new
  'pango_attr_rise_new
  '((name . "attr_rise_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "rise")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-scale-new
  'attr-scale-new
  'pango_attr_scale_new
  '((name . "attr_scale_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "scale_factor")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-sentence-new
  'attr-sentence-new
  'pango_attr_sentence_new
  '((name . "attr_sentence_new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-show-new
  'attr-show-new
  'pango_attr_show_new
  '((name . "attr_show_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "flags")
            (meta (arg-type . flags)
                  (gtype-name . "PangoShowFlags")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-size-new
  'attr-size-new
  'pango_attr_size_new
  '((name . "attr_size_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "size")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-size-new-absolute
  'attr-size-new-absolute
  'pango_attr_size_new_absolute
  '((name . "attr_size_new_absolute")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "size")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-stretch-new
  'attr-stretch-new
  'pango_attr_stretch_new
  '((name . "attr_stretch_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "stretch")
            (meta (arg-type . enum)
                  (gtype-name . "PangoStretch")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-strikethrough-color-new
  'attr-strikethrough-color-new
  'pango_attr_strikethrough_color_new
  '((name . "attr_strikethrough_color_new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "red")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "green")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blue")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-strikethrough-new
  'attr-strikethrough-new
  'pango_attr_strikethrough_new
  '((name . "attr_strikethrough_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "strikethrough")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-style-new
  'attr-style-new
  'pango_attr_style_new
  '((name . "attr_style_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "style")
            (meta (arg-type . enum)
                  (gtype-name . "PangoStyle")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-text-transform-new
  'attr-text-transform-new
  'pango_attr_text_transform_new
  '((name . "attr_text_transform_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "transform")
            (meta (arg-type . enum)
                  (gtype-name . "PangoTextTransform")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-type-get-name
  'attr-type-get-name
  'pango_attr_type_get_name
  '((name . "attr_type_get_name")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "type")
            (meta (arg-type . enum)
                  (gtype-name . "PangoAttrType")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-type-register
  'attr-type-register
  'pango_attr_type_register
  '((name . "attr_type_register")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoAttrType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-underline-color-new
  'attr-underline-color-new
  'pango_attr_underline_color_new
  '((name . "attr_underline_color_new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "red")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "green")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blue")
            (meta (arg-type . uint16)
                  (flags in)
                  (item-size . 2)
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
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-underline-new
  'attr-underline-new
  'pango_attr_underline_new
  '((name . "attr_underline_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "underline")
            (meta (arg-type . enum)
                  (gtype-name . "PangoUnderline")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-variant-new
  'attr-variant-new
  'pango_attr_variant_new
  '((name . "attr_variant_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "variant")
            (meta (arg-type . enum)
                  (gtype-name . "PangoVariant")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-weight-new
  'attr-weight-new
  'pango_attr_weight_new
  '((name . "attr_weight_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "weight")
            (meta (arg-type . enum)
                  (gtype-name . "PangoWeight")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:attr-word-new
  'attr-word-new
  'pango_attr_word_new
  '((name . "attr_word_new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoAttribute")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Pango
       '<PangoBidiType>
       "PangoBidiType"
       "pango_bidi_type_get_type")

(^enum-conv 'bidi-type 'PangoBidiType)

(^function
  'Pango
  #f
  'pango:bidi-type-for-unichar
  'bidi-type-for-unichar
  'pango_bidi_type_for_unichar
  '((name . "bidi_type_for_unichar")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "ch")
            (meta (arg-type . unichar)
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
      (meta (arg-type . enum)
            (gtype-name . "PangoBidiType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:find-base-dir
  'find-base-dir
  'pango_find_base_dir
  '((name . "find_base_dir")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "text")
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
           ((name . "length")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoDirection")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:find-paragraph-boundary
  'find-paragraph-boundary
  'pango_find_paragraph_boundary
  '((name . "find_paragraph_boundary")
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "text")
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
           ((name . "length")
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
           ((name . "paragraph_delimiter_index")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "next_paragraph_start")
            (meta (arg-type . int32)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
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
  'Pango
  #f
  'pango:font-description-from-string
  'font-description-from-string
  'pango_font_description_from_string
  '((name . "font_description_from_string")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "str")
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
            (gtype-name . "PangoFontDescription")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:gravity-get-for-matrix
  'gravity-get-for-matrix
  'pango_gravity_get_for_matrix
  '((name . "gravity_get_for_matrix")
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoMatrix")
                  (flags ptr in nullable)
                  (item-size . 48)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:gravity-get-for-script
  'gravity-get-for-script
  'pango_gravity_get_for_script
  '((name . "gravity_get_for_script")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "base_gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "hint")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravityHint")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:gravity-get-for-script-and-width
  'gravity-get-for-script-and-width
  'pango_gravity_get_for_script_and_width
  '((name . "gravity_get_for_script_and_width")
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "wide")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "base_gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "hint")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravityHint")
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
      (meta (arg-type . enum)
            (gtype-name . "PangoGravity")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:gravity-to-rotation
  'gravity-to-rotation
  'pango_gravity_to_rotation
  '((name . "gravity_to_rotation")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "gravity")
            (meta (arg-type . enum)
                  (gtype-name . "PangoGravity")
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
      (meta (arg-type . double)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:is-zero-width?
  'is-zero-width?
  'pango_is_zero_width
  '((name . "is_zero_width")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "ch")
            (meta (arg-type . unichar)
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
  'Pango
  #f
  'pango:itemize
  'itemize
  'pango_itemize
  '((name . "itemize")
    (s-input-req . 5)
    (s-input-opt . 1)
    (c-input-len . 6)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "text")
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
           ((name . "start_index")
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
           ((name . "length")
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
           ((name . "attrs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "cached_iter")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrIterator")
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
      (meta (arg-type . GList)
            (flags ptr out)
            (transfer . everything)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoItem")
               (item-size . 64)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:itemize-with-base-dir
  'itemize-with-base-dir
  'pango_itemize_with_base_dir
  '((name . "itemize_with_base_dir")
    (s-input-req . 6)
    (s-input-opt . 1)
    (c-input-len . 7)
    (pdata ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "base_dir")
            (meta (arg-type . enum)
                  (gtype-name . "PangoDirection")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "text")
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
           ((name . "start_index")
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
           ((name . "length")
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
           ((name . "attrs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5))
           ((name . "cached_iter")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrIterator")
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
      (meta (arg-type . GList)
            (flags ptr out)
            (transfer . everything)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoItem")
               (item-size . 64)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:language-from-string
  'language-from-string
  'pango_language_from_string
  '((name . "language_from_string")
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "language")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:language-get-default
  'language-get-default
  'pango_language_get_default
  '((name . "language_get_default")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:language-get-preferred
  'language-get-preferred
  'pango_language_get_preferred
  '((name . "language_get_preferred")
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out nullable zero-terminated)
            (transfer . nothing)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoLanguage")
               (flags ptr)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:layout-deserialize-error-quark
  'layout-deserialize-error-quark
  'pango_layout_deserialize_error_quark
  '((name . "layout_deserialize_error_quark")
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
  'Pango
  #f
  'pango:log2vis-get-embedding-levels
  'log2vis-get-embedding-levels
  'pango_log2vis_get_embedding_levels
  '((name . "log2vis_get_embedding_levels")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "text")
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
           ((name . "length")
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
           ((name . "pbase_dir")
            (meta (arg-type . enum)
                  (gtype-name . "PangoDirection")
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
      (meta (arg-type . uint8)
            (flags ptr out)
            (item-size . 1)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GLib
       '<GMarkupParseContext>
       "GMarkupParseContext"
       "g_markup_parse_context_get_type")

(^function
  'Pango
  #f
  'pango:markup-parser-finish
  'markup-parser-finish
  'pango_markup_parser_finish
  '((name . "markup_parser_finish")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-output-len . 3)
    (c-input-len . 1)
    (c-output-len . 3)
    (pdata ((name . "context")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GMarkupParseContext")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "attr_list")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
                  (flags ptr out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "text")
            (meta (arg-type . utf8-string)
                  (flags ptr out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "accel_char")
            (meta (arg-type . unichar)
                  (flags out optional)
                  (item-size . 4)
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
  'Pango
  #f
  'pango:markup-parser-new
  'markup-parser-new
  'pango_markup_parser_new
  '((name . "markup_parser_new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "accel_marker")
            (meta (arg-type . unichar)
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
            (gtype-name . "GMarkupParseContext")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:parse-markup
  'parse-markup
  'pango_parse_markup
  '((name . "parse_markup")
    (can-throw-gerror . #t)
    (s-input-req . 3)
    (s-output-len . 3)
    (c-input-len . 3)
    (c-output-len . 3)
    (pdata ((name . "markup_text")
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
           ((name . "length")
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
           ((name . "accel_marker")
            (meta (arg-type . unichar)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "attr_list")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoAttrList")
                  (flags ptr out optional)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "text")
            (meta (arg-type . utf8-string)
                  (flags ptr out optional)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "accel_char")
            (meta (arg-type . unichar)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 5)
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
  'Pango
  #f
  'pango:parse-stretch
  'parse-stretch
  'pango_parse_stretch
  '((name . "parse_stretch")
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "str")
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
           ((name . "stretch")
            (meta (arg-type . enum)
                  (gtype-name . "PangoStretch")
                  (flags out)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "warn")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
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
  'Pango
  #f
  'pango:parse-style
  'parse-style
  'pango_parse_style
  '((name . "parse_style")
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "str")
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
           ((name . "style")
            (meta (arg-type . enum)
                  (gtype-name . "PangoStyle")
                  (flags out)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "warn")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
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
  'Pango
  #f
  'pango:parse-variant
  'parse-variant
  'pango_parse_variant
  '((name . "parse_variant")
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "str")
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
           ((name . "variant")
            (meta (arg-type . enum)
                  (gtype-name . "PangoVariant")
                  (flags out)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "warn")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
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
  'Pango
  #f
  'pango:parse-weight
  'parse-weight
  'pango_parse_weight
  '((name . "parse_weight")
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "str")
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
           ((name . "weight")
            (meta (arg-type . enum)
                  (gtype-name . "PangoWeight")
                  (flags out)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "warn")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
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
  'Pango
  #f
  'pango:quantize-line-geometry
  'quantize-line-geometry
  'pango_quantize_line_geometry
  '((name . "quantize_line_geometry")
    (s-input-req . 2)
    (s-output-len . 2)
    (c-input-len . 2)
    (c-output-len . 2)
    (pdata ((name . "thickness")
            (meta (arg-type . int32)
                  (flags in out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "position")
            (meta (arg-type . int32)
                  (flags in out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . inout)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (c-output-pos . 1)
            (s-input-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:reorder-items
  'reorder-items
  'pango_reorder_items
  '((name . "reorder_items")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "items")
            (meta (arg-type . GList)
                  (flags ptr in)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "PangoItem")
                     (item-size . 64)
                     (transfer . nothing))))
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
            (transfer . everything)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "PangoItem")
               (item-size . 64)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:script-get-sample-language
  'script-get-sample-language
  'pango_script_get_sample_language
  '((name . "script_get_sample_language")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "script")
            (meta (arg-type . enum)
                  (gtype-name . "PangoScript")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "PangoLanguage")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:tab-array-from-string
  'tab-array-from-string
  'pango_tab_array_from_string
  '((name . "tab_array_from_string")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "text")
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
            (gtype-name . "PangoTabArray")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:unichar-direction
  'unichar-direction
  'pango_unichar_direction
  '((name . "unichar_direction")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "ch")
            (meta (arg-type . unichar)
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
      (meta (arg-type . enum)
            (gtype-name . "PangoDirection")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:units-from-double
  'units-from-double
  'pango_units_from_double
  '((name . "units_from_double")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "d")
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
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:units-to-double
  'units-to-double
  'pango_units_to_double
  '((name . "units_to_double")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "i")
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
      (meta (arg-type . double)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:version
  'version
  'pango_version
  '((name . "version")
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
  'Pango
  #f
  'pango:version-check
  'version-check
  'pango_version_check
  '((name . "version_check")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "required_major")
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
           ((name . "required_minor")
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
           ((name . "required_micro")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Pango
  #f
  'pango:version-string
  'version-string
  'pango_version_string
  '((name . "version_string")
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


)
