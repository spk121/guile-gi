(define-module (gi PangoCairo-1-0)
  #:use-module (gi runtime)
  #:use-module (gi cairo-1-0)
  #:use-module (gi Pango-1-0)
  #:use-module (gi GObject-2-0)
  #:export(<PangoFont> <PangoCairoFont> <CairoScaledFont> get-scaled-font 
           font:get-scaled-font <GObject> <GListModel> <PangoFontMap> 
           <PangoCairoFontMap> get-default font-map:get-default new 
           font-map:new <cairo_font_type_t> symbol->font-type number->font-type 
           font-type->symbol font-type->number new-for-font-type 
           font-map:new-for-font-type get-font-type font-map:get-font-type 
           get-resolution font-map:get-resolution set-default 
           font-map:set-default set-resolution font-map:set-resolution 
           <CairoFontOptions> <PangoContext> context-get-font-options 
           pango-cairo:context-get-font-options context-get-resolution 
           pango-cairo:context-get-resolution context-set-font-options 
           pango-cairo:context-set-font-options context-set-resolution 
           pango-cairo:context-set-resolution context-set-shape-renderer 
           pango-cairo:context-set-shape-renderer <CairoContext> create-context 
           pango-cairo:create-context <PangoLayout> create-layout 
           pango-cairo:create-layout error-underline-path 
           pango-cairo:error-underline-path font-map-get-default 
           pango-cairo:font-map-get-default font-map-new 
           pango-cairo:font-map-new font-map-new-for-font-type 
           pango-cairo:font-map-new-for-font-type <PangoGlyphString> 
           glyph-string-path pango-cairo:glyph-string-path <PangoLayoutLine> 
           layout-line-path pango-cairo:layout-line-path layout-path 
           pango-cairo:layout-path show-error-underline 
           pango-cairo:show-error-underline <PangoGlyphItem> show-glyph-item 
           pango-cairo:show-glyph-item show-glyph-string 
           pango-cairo:show-glyph-string show-layout pango-cairo:show-layout 
           show-layout-line pango-cairo:show-layout-line update-context 
           pango-cairo:update-context update-layout pango-cairo:update-layout))

(eval-when (expand load eval)
(^library
  "PangoCairo"
  "1.0"
  '("libpangocairo-1.0-0.dll"))

(^initialize)

(^type 'Pango
       '<PangoFont>
       "PangoFont"
       "pango_font_get_type")

(^type 'PangoCairo
       '<PangoCairoFont>
       "PangoCairoFont"
       "pango_cairo_font_get_type")

(^type 'cairo
       '<CairoScaledFont>
       "CairoScaledFont"
       "cairo_gobject_scaled_font_get_type")

(^function
  'PangoCairo
  'PangoCairoFont
  'font:get-scaled-font
  'get-scaled-font
  'pango_cairo_font_get_scaled_font
  '((name . "get_scaled_font")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoScaledFont")
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

(^type 'PangoCairo
       '<PangoCairoFontMap>
       "PangoCairoFontMap"
       "pango_cairo_font_map_get_type")

(^function
  'PangoCairo
  'PangoCairoFontMap
  'font-map:get-default
  'get-default
  'pango_cairo_font_map_get_default
  '((name . "get_default")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  'PangoCairoFontMap
  'font-map:new
  'new
  'pango_cairo_font_map_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'cairo
       '<cairo_font_type_t>
       "cairo_font_type_t"
       "cairo_gobject_font_type_get_type")

(^enum-conv 'font-type 'cairo_font_type_t)

(^function
  'PangoCairo
  'PangoCairoFontMap
  'font-map:new-for-font-type
  'new-for-font-type
  'pango_cairo_font_map_new_for_font_type
  '((name . "new_for_font_type")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "fonttype")
            (meta (arg-type . enum)
                  (gtype-name . "cairo_font_type_t")
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
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  'PangoCairoFontMap
  'font-map:get-font-type
  'get-font-type
  'pango_cairo_font_map_get_font_type
  '((name . "get_font_type")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "cairo_font_type_t")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  'PangoCairoFontMap
  'font-map:get-resolution
  'get-resolution
  'pango_cairo_font_map_get_resolution
  '((name . "get_resolution")
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
  'PangoCairo
  'PangoCairoFontMap
  'font-map:set-default
  'set-default
  'pango_cairo_font_map_set_default
  '((name . "set_default")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  'PangoCairoFontMap
  'font-map:set-resolution
  'set-resolution
  'pango_cairo_font_map_set_resolution
  '((name . "set_resolution")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "dpi")
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

(^type 'cairo
       '<CairoFontOptions>
       "CairoFontOptions"
       "cairo_gobject_font_options_get_type")

(^type 'Pango
       '<PangoContext>
       "PangoContext"
       "pango_context_get_type")

(^function
  'PangoCairo
  #f
  'pango-cairo:context-get-font-options
  'context-get-font-options
  'pango_cairo_context_get_font_options
  '((name . "context_get_font_options")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoFontOptions")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  #f
  'pango-cairo:context-get-resolution
  'context-get-resolution
  'pango_cairo_context_get_resolution
  '((name . "context_get_resolution")
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
      (meta (arg-type . double)
            (flags out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  #f
  'pango-cairo:context-set-font-options
  'context-set-font-options
  'pango_cairo_context_set_font_options
  '((name . "context_set_font_options")
    (s-input-req . 1)
    (s-input-opt . 1)
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
           ((name . "options")
            (meta (arg-type . GBoxed)
                  (gtype-name . "CairoFontOptions")
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
  'PangoCairo
  #f
  'pango-cairo:context-set-resolution
  'context-set-resolution
  'pango_cairo_context_set_resolution
  '((name . "context_set_resolution")
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
           ((name . "dpi")
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
  'PangoCairo
  #f
  'pango-cairo:context-set-shape-renderer
  'context-set-shape-renderer
  'pango_cairo_context_set_shape_renderer
  '((name . "context_set_shape_renderer")
    (s-input-req . 1)
    (s-input-opt . 3)
    (c-input-len . 4)
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
           ((name . "func")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "ShapeRendererFunc")
                    (s-input-req . 3)
                    (s-input-opt . 1)
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
                           ((name . "attr")
                            (meta (arg-type . GBoxed)
                                  (gtype-name . "void")
                                  (flags ptr in)
                                  (item-size . 72)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 1)
                            (c-input-pos . 1)
                            (s-input-pos . 1))
                           ((name . "do_path")
                            (meta (arg-type . gboolean)
                                  (flags in)
                                  (item-size . 4)
                                  (transfer . nothing))
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
            (s-input-pos . 2))
           ((name . "dnotify")
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

(^type 'cairo
       '<CairoContext>
       "CairoContext"
       "cairo_gobject_context_get_type")

(^function
  'PangoCairo
  #f
  'pango-cairo:create-context
  'create-context
  'pango_cairo_create_context
  '((name . "create_context")
    (s-input-req . 1)
    (c-input-len . 1)
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
            (s-input-pos . 0)))
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

(^type 'Pango
       '<PangoLayout>
       "PangoLayout"
       "pango_layout_get_type")

(^function
  'PangoCairo
  #f
  'pango-cairo:create-layout
  'create-layout
  'pango_cairo_create_layout
  '((name . "create_layout")
    (s-input-req . 1)
    (c-input-len . 1)
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

(^function
  'PangoCairo
  #f
  'pango-cairo:error-underline-path
  'error-underline-path
  'pango_cairo_error_underline_path
  '((name . "error_underline_path")
    (s-input-req . 5)
    (c-input-len . 5)
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
           ((name . "x")
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
           ((name . "y")
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
           ((name . "width")
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
           ((name . "height")
            (meta (arg-type . double)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  #f
  'pango-cairo:font-map-get-default
  'font-map-get-default
  'pango_cairo_font_map_get_default
  '((name . "font_map_get_default")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  #f
  'pango-cairo:font-map-new
  'font-map-new
  'pango_cairo_font_map_new
  '((name . "font_map_new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'PangoCairo
  #f
  'pango-cairo:font-map-new-for-font-type
  'font-map-new-for-font-type
  'pango_cairo_font_map_new_for_font_type
  '((name . "font_map_new_for_font_type")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "fonttype")
            (meta (arg-type . enum)
                  (gtype-name . "cairo_font_type_t")
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
      (meta (arg-type . GObject)
            (gtype-name . "PangoFontMap")
            (flags ptr out nullable)
            (transfer . everything))
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
  'PangoCairo
  #f
  'pango-cairo:glyph-string-path
  'glyph-string-path
  'pango_cairo_glyph_string_path
  '((name . "glyph_string_path")
    (s-input-req . 3)
    (c-input-len . 3)
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
           ((name . "glyphs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphString")
                  (flags ptr in)
                  (item-size . 32)
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

(^sized-type
  'Pango
  '<PangoLayoutLine>
  "PangoLayoutLine"
  "pango_layout_line_get_type"
  32)

(^function
  'PangoCairo
  #f
  'pango-cairo:layout-line-path
  'layout-line-path
  'pango_cairo_layout_line_path
  '((name . "layout_line_path")
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
           ((name . "line")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLayoutLine")
                  (flags ptr in)
                  (item-size . 32)
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
  'PangoCairo
  #f
  'pango-cairo:layout-path
  'layout-path
  'pango_cairo_layout_path
  '((name . "layout_path")
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
           ((name . "layout")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoLayout")
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
  'PangoCairo
  #f
  'pango-cairo:show-error-underline
  'show-error-underline
  'pango_cairo_show_error_underline
  '((name . "show_error_underline")
    (s-input-req . 5)
    (c-input-len . 5)
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
           ((name . "x")
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
           ((name . "y")
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
           ((name . "width")
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
           ((name . "height")
            (meta (arg-type . double)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Pango
  '<PangoGlyphItem>
  "PangoGlyphItem"
  "pango_glyph_item_get_type"
  32)

(^function
  'PangoCairo
  #f
  'pango-cairo:show-glyph-item
  'show-glyph-item
  'pango_cairo_show_glyph_item
  '((name . "show_glyph_item")
    (s-input-req . 3)
    (c-input-len . 3)
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
           ((name . "glyph_item")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphItem")
                  (flags ptr in)
                  (item-size . 32)
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
  'PangoCairo
  #f
  'pango-cairo:show-glyph-string
  'show-glyph-string
  'pango_cairo_show_glyph_string
  '((name . "show_glyph_string")
    (s-input-req . 3)
    (c-input-len . 3)
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
           ((name . "glyphs")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoGlyphString")
                  (flags ptr in)
                  (item-size . 32)
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
  'PangoCairo
  #f
  'pango-cairo:show-layout
  'show-layout
  'pango_cairo_show_layout
  '((name . "show_layout")
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
           ((name . "layout")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoLayout")
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
  'PangoCairo
  #f
  'pango-cairo:show-layout-line
  'show-layout-line
  'pango_cairo_show_layout_line
  '((name . "show_layout_line")
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
           ((name . "line")
            (meta (arg-type . GBoxed)
                  (gtype-name . "PangoLayoutLine")
                  (flags ptr in)
                  (item-size . 32)
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
  'PangoCairo
  #f
  'pango-cairo:update-context
  'update-context
  'pango_cairo_update_context
  '((name . "update_context")
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
           ((name . "context")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoContext")
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
  'PangoCairo
  #f
  'pango-cairo:update-layout
  'update-layout
  'pango_cairo_update_layout
  '((name . "update_layout")
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
           ((name . "layout")
            (meta (arg-type . GObject)
                  (gtype-name . "PangoLayout")
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


)
