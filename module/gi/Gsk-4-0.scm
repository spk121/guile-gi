(define-module (gi Gsk-4-0)
  #:use-module (gi runtime)
  #:use-module (gi Graphene-1-0)
  #:use-module (gi Gdk-4-0)
  #:export(<GskBlendMode> symbol->blend-mode number->blend-mode 
           blend-mode->symbol blend-mode->number <GskRenderNode> <GskBlendNode> 
           new blend-node:new get-blend-mode blend-node:get-blend-mode 
           get-bottom-child blend-node:get-bottom-child get-top-child 
           blend-node:get-top-child <GskBlurNode> new blur-node:new get-child 
           blur-node:get-child get-radius blur-node:get-radius <GskBorderNode> 
           <GdkRGBA> get-colors border-node:get-colors get-widths 
           border-node:get-widths <GskCairoNode> <GrapheneRect> new 
           cairo-node:new <CairoContext> get-draw-context 
           cairo-node:get-draw-context <CairoSurface> get-surface 
           cairo-node:get-surface <GskRenderer> <GskCairoRenderer> new 
           cairo-renderer:new <GskClipNode> new clip-node:new get-child 
           clip-node:get-child get-clip clip-node:get-clip <GskColorMatrixNode> 
           <GrapheneMatrix> <GrapheneVec4> new color-matrix-node:new get-child 
           color-matrix-node:get-child get-color-matrix 
           color-matrix-node:get-color-matrix get-color-offset 
           color-matrix-node:get-color-offset <GskColorNode> new color-node:new 
           get-color color-node:get-color <GskConicGradientNode> 
           <GraphenePoint> get-angle conic-gradient-node:get-angle get-center 
           conic-gradient-node:get-center get-n-color-stops 
           conic-gradient-node:get-n-color-stops get-rotation 
           conic-gradient-node:get-rotation <GskContainerNode> new 
           container-node:new get-child container-node:get-child get-n-children 
           container-node:get-n-children <GskCorner> symbol->corner 
           number->corner corner->symbol corner->number <GskCrossFadeNode> new 
           cross-fade-node:new get-end-child cross-fade-node:get-end-child 
           get-progress cross-fade-node:get-progress get-start-child 
           cross-fade-node:get-start-child <GskDebugNode> new debug-node:new 
           get-child debug-node:get-child get-message debug-node:get-message 
           <GskGLRenderer> new glrenderer:new <GskGLShader> <GBytes> 
           new-from-bytes glshader:new-from-bytes new-from-resource 
           glshader:new-from-resource compile? glshader:compile? 
           find-uniform-by-name glshader:find-uniform-by-name get-arg-bool? 
           glshader:get-arg-bool? get-arg-float glshader:get-arg-float 
           get-arg-int glshader:get-arg-int get-arg-uint glshader:get-arg-uint 
           <GrapheneVec2> get-arg-vec2 glshader:get-arg-vec2 <GrapheneVec3> 
           get-arg-vec3 glshader:get-arg-vec3 get-arg-vec4 
           glshader:get-arg-vec4 get-args-size glshader:get-args-size 
           get-n-textures glshader:get-n-textures get-n-uniforms 
           glshader:get-n-uniforms get-resource glshader:get-resource 
           get-source glshader:get-source get-uniform-name 
           glshader:get-uniform-name get-uniform-offset 
           glshader:get-uniform-offset <GskGLUniformType> 
           symbol->gluniform-type number->gluniform-type gluniform-type->symbol 
           gluniform-type->number get-uniform-type glshader:get-uniform-type 
           <GskGLShaderNode> new glshader-node:new get-args 
           glshader-node:get-args get-child glshader-node:get-child 
           get-n-children glshader-node:get-n-children get-shader 
           glshader-node:get-shader <GskInsetShadowNode> get-blur-radius 
           inset-shadow-node:get-blur-radius get-color 
           inset-shadow-node:get-color get-dx inset-shadow-node:get-dx get-dy 
           inset-shadow-node:get-dy get-spread inset-shadow-node:get-spread 
           <GskLinearGradientNode> get-end linear-gradient-node:get-end 
           get-n-color-stops linear-gradient-node:get-n-color-stops get-start 
           linear-gradient-node:get-start <GskMaskMode> symbol->mask-mode 
           number->mask-mode mask-mode->symbol mask-mode->number <GskMaskNode> 
           new mask-node:new get-mask mask-node:get-mask get-mask-mode 
           mask-node:get-mask-mode get-source mask-node:get-source 
           <GskNglRenderer> new ngl-renderer:new <GskOpacityNode> new 
           opacity-node:new get-child opacity-node:get-child get-opacity 
           opacity-node:get-opacity <GskOutsetShadowNode> get-blur-radius 
           outset-shadow-node:get-blur-radius get-color 
           outset-shadow-node:get-color get-dx outset-shadow-node:get-dx get-dy 
           outset-shadow-node:get-dy get-spread outset-shadow-node:get-spread 
           <GskRadialGradientNode> get-center radial-gradient-node:get-center 
           get-end radial-gradient-node:get-end get-hradius 
           radial-gradient-node:get-hradius get-n-color-stops 
           radial-gradient-node:get-n-color-stops get-start 
           radial-gradient-node:get-start get-vradius 
           radial-gradient-node:get-vradius deserialize render-node:deserialize 
           draw render-node:draw get-bounds! render-node:get-bounds! 
           <GskRenderNodeType> symbol->render-node-type 
           number->render-node-type render-node-type->symbol 
           render-node-type->number get-node-type render-node:get-node-type ref 
           render-node:ref serialize render-node:serialize unref 
           render-node:unref write-to-file? render-node:write-to-file? 
           <GdkSurface> new-for-surface renderer:new-for-surface get-surface 
           renderer:get-surface is-realized? renderer:is-realized? realize? 
           renderer:realize? <CairoRegion> render renderer:render <GObject> 
           <GdkPaintable> <GIcon> <GLoadableIcon> <GdkTexture> render-texture 
           renderer:render-texture unrealize renderer:unrealize <GskRepeatNode> 
           new repeat-node:new get-child repeat-node:get-child get-child-bounds 
           repeat-node:get-child-bounds <GskRepeatingLinearGradientNode> 
           <GskRepeatingRadialGradientNode> <GskRoundedClipNode> get-child 
           rounded-clip-node:get-child <GskScalingFilter> 
           symbol->scaling-filter number->scaling-filter scaling-filter->symbol 
           scaling-filter->number <GskSerializationError> 
           symbol->serialization-error number->serialization-error 
           serialization-error->symbol serialization-error->number quark 
           serialization-error:quark <GskShaderArgsBuilder> new 
           shader-args-builder:new ref shader-args-builder:ref set-bool 
           shader-args-builder:set-bool set-float shader-args-builder:set-float 
           set-int shader-args-builder:set-int set-uint 
           shader-args-builder:set-uint set-vec2 shader-args-builder:set-vec2 
           set-vec3 shader-args-builder:set-vec3 set-vec4 
           shader-args-builder:set-vec4 to-args shader-args-builder:to-args 
           unref shader-args-builder:unref <GskShadowNode> get-child 
           shadow-node:get-child get-n-shadows shadow-node:get-n-shadows 
           <GskTextNode> <PangoFont> <PangoGlyphString> new text-node:new 
           get-color text-node:get-color get-font text-node:get-font 
           get-num-glyphs text-node:get-num-glyphs get-offset 
           text-node:get-offset has-color-glyphs? text-node:has-color-glyphs? 
           <GskTextureNode> new texture-node:new get-texture 
           texture-node:get-texture <GskTextureScaleNode> new 
           texture-scale-node:new get-filter texture-scale-node:get-filter 
           get-texture texture-scale-node:get-texture <GskTransform> new 
           transform:new equal? transform:equal? <GskTransformCategory> 
           symbol->transform-category number->transform-category 
           transform-category->symbol transform-category->number get-category 
           transform:get-category invert transform:invert matrix 
           transform:matrix perspective transform:perspective <GString> print 
           transform:print ref transform:ref rotate transform:rotate rotate-3d 
           transform:rotate-3d scale transform:scale scale-3d 
           transform:scale-3d skew transform:skew to-2d transform:to-2d 
           to-2d-components transform:to-2d-components to-affine 
           transform:to-affine to-matrix! transform:to-matrix! to-string 
           transform:to-string to-translate transform:to-translate transform 
           transform:transform transform-bounds! transform:transform-bounds! 
           transform-point! transform:transform-point! translate 
           transform:translate <GraphenePoint3D> translate-3d 
           transform:translate-3d unref transform:unref parse transform:parse 
           <GskTransformNode> new transform-node:new get-child 
           transform-node:get-child get-transform transform-node:get-transform 
           serialization-error-quark gsk:serialization-error-quark 
           transform-parse gsk:transform-parse <GValue> value-dup-render-node 
           gsk:value-dup-render-node value-get-render-node 
           gsk:value-get-render-node value-set-render-node 
           gsk:value-set-render-node value-take-render-node 
           gsk:value-take-render-node))

(eval-when (expand load eval)
(^library "Gsk" "4.0" '("libgtk-4-1.dll"))

(^initialize)

(^type 'Gsk
       '<GskBlendMode>
       "GskBlendMode"
       "gsk_blend_mode_get_type")

(^enum-conv 'blend-mode 'GskBlendMode)

(^custom-type
  'Gsk
  '<GskRenderNode>
  "GskRenderNode"
  "gsk_render_node_get_type"
  "gsk_render_node_ref"
  "gsk_render_node_unref")

(^type 'Gsk
       '<GskBlendNode>
       "GskBlendNode"
       "gsk_blend_node_get_type")

(^function
  'Gsk
  'GskBlendNode
  'blend-node:new
  'new
  'gsk_blend_node_new
  '((name . "new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "bottom")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "top")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "blend_mode")
            (meta (arg-type . enum)
                  (gtype-name . "GskBlendMode")
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
            (gtype-name . "GskBlendNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskBlendNode
  'blend-node:get-blend-mode
  'get-blend-mode
  'gsk_blend_node_get_blend_mode
  '((name . "get_blend_mode")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GskBlendMode")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskBlendNode
  'blend-node:get-bottom-child
  'get-bottom-child
  'gsk_blend_node_get_bottom_child
  '((name . "get_bottom_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskBlendNode
  'blend-node:get-top-child
  'get-top-child
  'gsk_blend_node_get_top_child
  '((name . "get_top_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskBlurNode>
       "GskBlurNode"
       "gsk_blur_node_get_type")

(^function
  'Gsk
  'GskBlurNode
  'blur-node:new
  'new
  'gsk_blur_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "radius")
            (meta (arg-type . float)
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
            (gtype-name . "GskBlurNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskBlurNode
  'blur-node:get-child
  'get-child
  'gsk_blur_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskBlurNode
  'blur-node:get-radius
  'get-radius
  'gsk_blur_node_get_radius
  '((name . "get_radius")
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

(^type 'Gsk
       '<GskBorderNode>
       "GskBorderNode"
       "gsk_border_node_get_type")

(^sized-type
  'Gdk
  '<GdkRGBA>
  "GdkRGBA"
  "gdk_rgba_get_type"
  16)

(^function
  'Gsk
  'GskBorderNode
  'border-node:get-colors
  'get-colors
  'gsk_border_node_get_colors
  '((name . "get_colors")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRGBA")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskBorderNode
  'border-node:get-widths
  'get-widths
  'gsk_border_node_get_widths
  '((name . "get_widths")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out fixed-size)
            (fixed-size . 4)
            (transfer . nothing)
            (params
              ((arg-type . float)
               (item-size . 4)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskCairoNode>
       "GskCairoNode"
       "gsk_cairo_node_get_type")

(^sized-type
  'Graphene
  '<GrapheneRect>
  "GrapheneRect"
  "graphene_rect_get_type"
  16)

(^function
  'Gsk
  'GskCairoNode
  'cairo-node:new
  'new
  'gsk_cairo_node_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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
      (meta (arg-type . GObject)
            (gtype-name . "GskCairoNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'cairo
       '<CairoContext>
       "CairoContext"
       "cairo_gobject_context_get_type")

(^function
  'Gsk
  'GskCairoNode
  'cairo-node:get-draw-context
  'get-draw-context
  'gsk_cairo_node_get_draw_context
  '((name . "get_draw_context")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoContext")
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

(^function
  'Gsk
  'GskCairoNode
  'cairo-node:get-surface
  'get-surface
  'gsk_cairo_node_get_surface
  '((name . "get_surface")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "CairoSurface")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskRenderer>
       "GskRenderer"
       "gsk_renderer_get_type")

(^type 'Gsk
       '<GskCairoRenderer>
       "GskCairoRenderer"
       "gsk_cairo_renderer_get_type")

(^function
  'Gsk
  'GskCairoRenderer
  'cairo-renderer:new
  'new
  'gsk_cairo_renderer_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderer")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskClipNode>
       "GskClipNode"
       "gsk_clip_node_get_type")

(^function
  'Gsk
  'GskClipNode
  'clip-node:new
  'new
  'gsk_clip_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "clip")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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
      (meta (arg-type . GObject)
            (gtype-name . "GskClipNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskClipNode
  'clip-node:get-child
  'get-child
  'gsk_clip_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskClipNode
  'clip-node:get-clip
  'get-clip
  'gsk_clip_node_get_clip
  '((name . "get_clip")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskColorMatrixNode>
       "GskColorMatrixNode"
       "gsk_color_matrix_node_get_type")

(^sized-type
  'Graphene
  '<GrapheneMatrix>
  "GrapheneMatrix"
  "graphene_matrix_get_type"
  64)

(^sized-type
  'Graphene
  '<GrapheneVec4>
  "GrapheneVec4"
  "graphene_vec4_get_type"
  16)

(^function
  'Gsk
  'GskColorMatrixNode
  'color-matrix-node:new
  'new
  'gsk_color_matrix_node_new
  '((name . "new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "color_matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags ptr in)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "color_offset")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
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
            (gtype-name . "GskColorMatrixNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskColorMatrixNode
  'color-matrix-node:get-child
  'get-child
  'gsk_color_matrix_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskColorMatrixNode
  'color-matrix-node:get-color-matrix
  'get-color-matrix
  'gsk_color_matrix_node_get_color_matrix
  '((name . "get_color_matrix")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskColorMatrixNode
  'color-matrix-node:get-color-offset
  'get-color-offset
  'gsk_color_matrix_node_get_color_offset
  '((name . "get_color_offset")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskColorNode>
       "GskColorNode"
       "gsk_color_node_get_type")

(^function
  'Gsk
  'GskColorNode
  'color-node:new
  'new
  'gsk_color_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "rgba")
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
            (s-input-pos . 0))
           ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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
      (meta (arg-type . GObject)
            (gtype-name . "GskColorNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskColorNode
  'color-node:get-color
  'get-color
  'gsk_color_node_get_color
  '((name . "get_color")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRGBA")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskConicGradientNode>
       "GskConicGradientNode"
       "gsk_conic_gradient_node_get_type")

(^sized-type
  'Graphene
  '<GraphenePoint>
  "GraphenePoint"
  "graphene_point_get_type"
  8)

(^function
  'Gsk
  'GskConicGradientNode
  'conic-gradient-node:get-angle
  'get-angle
  'gsk_conic_gradient_node_get_angle
  '((name . "get_angle")
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
  'Gsk
  'GskConicGradientNode
  'conic-gradient-node:get-center
  'get-center
  'gsk_conic_gradient_node_get_center
  '((name . "get_center")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskConicGradientNode
  'conic-gradient-node:get-n-color-stops
  'get-n-color-stops
  'gsk_conic_gradient_node_get_n_color_stops
  '((name . "get_n_color_stops")
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
  'Gsk
  'GskConicGradientNode
  'conic-gradient-node:get-rotation
  'get-rotation
  'gsk_conic_gradient_node_get_rotation
  '((name . "get_rotation")
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

(^type 'Gsk
       '<GskContainerNode>
       "GskContainerNode"
       "gsk_container_node_get_type")

(^function
  'Gsk
  'GskContainerNode
  'container-node:new
  'new
  'gsk_container_node_new
  '((name . "new")
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "children")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (length-arg . 1)
                  (transfer . nothing)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "GskRenderNode")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "n_children")
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
      (meta (arg-type . GObject)
            (gtype-name . "GskContainerNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskContainerNode
  'container-node:get-child
  'get-child
  'gsk_container_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "idx")
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
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskContainerNode
  'container-node:get-n-children
  'get-n-children
  'gsk_container_node_get_n_children
  '((name . "get_n_children")
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

(^type 'Gsk
       '<GskCorner>
       "GskCorner"
       "gsk_corner_get_type")

(^enum-conv 'corner 'GskCorner)

(^type 'Gsk
       '<GskCrossFadeNode>
       "GskCrossFadeNode"
       "gsk_cross_fade_node_get_type")

(^function
  'Gsk
  'GskCrossFadeNode
  'cross-fade-node:new
  'new
  'gsk_cross_fade_node_new
  '((name . "new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "start")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "end")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "progress")
            (meta (arg-type . float)
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
      (meta (arg-type . GObject)
            (gtype-name . "GskCrossFadeNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskCrossFadeNode
  'cross-fade-node:get-end-child
  'get-end-child
  'gsk_cross_fade_node_get_end_child
  '((name . "get_end_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskCrossFadeNode
  'cross-fade-node:get-progress
  'get-progress
  'gsk_cross_fade_node_get_progress
  '((name . "get_progress")
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
  'Gsk
  'GskCrossFadeNode
  'cross-fade-node:get-start-child
  'get-start-child
  'gsk_cross_fade_node_get_start_child
  '((name . "get_start_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskDebugNode>
       "GskDebugNode"
       "gsk_debug_node_get_type")

(^function
  'Gsk
  'GskDebugNode
  'debug-node:new
  'new
  'gsk_debug_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "message")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskDebugNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskDebugNode
  'debug-node:get-child
  'get-child
  'gsk_debug_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskDebugNode
  'debug-node:get-message
  'get-message
  'gsk_debug_node_get_message
  '((name . "get_message")
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

(^type 'Gsk
       '<GskGLRenderer>
       "GskGLRenderer"
       "gsk_gl_renderer_get_type")

(^function
  'Gsk
  'GskGLRenderer
  'glrenderer:new
  'new
  'gsk_gl_renderer_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderer")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskGLShader>
       "GskGLShader"
       "gsk_gl_shader_get_type")

(^type 'GLib
       '<GBytes>
       "GBytes"
       "g_bytes_get_type")

(^function
  'Gsk
  'GskGLShader
  'glshader:new-from-bytes
  'new-from-bytes
  'gsk_gl_shader_new_from_bytes
  '((name . "new_from_bytes")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "sourcecode")
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
            (gtype-name . "GskGLShader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShader
  'glshader:new-from-resource
  'new-from-resource
  'gsk_gl_shader_new_from_resource
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
            (gtype-name . "GskGLShader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShader
  'glshader:compile?
  'compile?
  'gsk_gl_shader_compile
  '((name . "compile")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "renderer")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderer")
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
  'Gsk
  'GskGLShader
  'glshader:find-uniform-by-name
  'find-uniform-by-name
  'gsk_gl_shader_find_uniform_by_name
  '((name . "find_uniform_by_name")
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
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShader
  'glshader:get-arg-bool?
  'get-arg-bool?
  'gsk_gl_shader_get_arg_bool
  '((name . "get_arg_bool")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
  'Gsk
  'GskGLShader
  'glshader:get-arg-float
  'get-arg-float
  'gsk_gl_shader_get_arg_float
  '((name . "get_arg_float")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShader
  'glshader:get-arg-int
  'get-arg-int
  'gsk_gl_shader_get_arg_int
  '((name . "get_arg_int")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
  'Gsk
  'GskGLShader
  'glshader:get-arg-uint
  'get-arg-uint
  'gsk_gl_shader_get_arg_uint
  '((name . "get_arg_uint")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
      (meta (arg-type . uint32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneVec2>
  "GrapheneVec2"
  "graphene_vec2_get_type"
  16)

(^function
  'Gsk
  'GskGLShader
  'glshader:get-arg-vec2
  'get-arg-vec2
  'gsk_gl_shader_get_arg_vec2
  '((name . "get_arg_vec2")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
           ((name . "out_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
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
  'Graphene
  '<GrapheneVec3>
  "GrapheneVec3"
  "graphene_vec3_get_type"
  16)

(^function
  'Gsk
  'GskGLShader
  'glshader:get-arg-vec3
  'get-arg-vec3
  'gsk_gl_shader_get_arg_vec3
  '((name . "get_arg_vec3")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
           ((name . "out_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gsk
  'GskGLShader
  'glshader:get-arg-vec4
  'get-arg-vec4
  'gsk_gl_shader_get_arg_vec4
  '((name . "get_arg_vec4")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "idx")
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
           ((name . "out_value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
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
  'Gsk
  'GskGLShader
  'glshader:get-args-size
  'get-args-size
  'gsk_gl_shader_get_args_size
  '((name . "get_args_size")
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
  'Gsk
  'GskGLShader
  'glshader:get-n-textures
  'get-n-textures
  'gsk_gl_shader_get_n_textures
  '((name . "get_n_textures")
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
  'Gsk
  'GskGLShader
  'glshader:get-n-uniforms
  'get-n-uniforms
  'gsk_gl_shader_get_n_uniforms
  '((name . "get_n_uniforms")
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
  'Gsk
  'GskGLShader
  'glshader:get-resource
  'get-resource
  'gsk_gl_shader_get_resource
  '((name . "get_resource")
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
  'Gsk
  'GskGLShader
  'glshader:get-source
  'get-source
  'gsk_gl_shader_get_source
  '((name . "get_source")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GBytes")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShader
  'glshader:get-uniform-name
  'get-uniform-name
  'gsk_gl_shader_get_uniform_name
  '((name . "get_uniform_name")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "idx")
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
      (meta (arg-type . utf8-string)
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShader
  'glshader:get-uniform-offset
  'get-uniform-offset
  'gsk_gl_shader_get_uniform_offset
  '((name . "get_uniform_offset")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "idx")
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

(^type 'Gsk
       '<GskGLUniformType>
       "GskGLUniformType"
       "gsk_gl_uniform_type_get_type")

(^enum-conv 'gluniform-type 'GskGLUniformType)

(^function
  'Gsk
  'GskGLShader
  'glshader:get-uniform-type
  'get-uniform-type
  'gsk_gl_shader_get_uniform_type
  '((name . "get_uniform_type")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "idx")
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
            (gtype-name . "GskGLUniformType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GskGLShader
  'glshader:resource
  'resource
  'resource)

(^property
  'GskGLShader
  'glshader:source
  'source
  'source)

(^type 'Gsk
       '<GskGLShaderNode>
       "GskGLShaderNode"
       "gsk_gl_shader_node_get_type")

(^function
  'Gsk
  'GskGLShaderNode
  'glshader-node:new
  'new
  'gsk_gl_shader_node_new
  '((name . "new")
    (s-input-req . 4)
    (c-input-len . 5)
    (pdata ((name . "shader")
            (meta (arg-type . GObject)
                  (gtype-name . "GskGLShader")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "args")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "children")
            (meta (arg-type . array)
                  (flags ptr in nullable sized)
                  (length-arg . 4)
                  (transfer . nothing)
                  (params
                    ((arg-type . GObject)
                     (gtype-name . "GskRenderNode")
                     (flags ptr)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "n_children")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 4)
            (c-input-pos . 4)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskGLShaderNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShaderNode
  'glshader-node:get-args
  'get-args
  'gsk_gl_shader_node_get_args
  '((name . "get_args")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GBytes")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShaderNode
  'glshader-node:get-child
  'get-child
  'gsk_gl_shader_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "idx")
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
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskGLShaderNode
  'glshader-node:get-n-children
  'get-n-children
  'gsk_gl_shader_node_get_n_children
  '((name . "get_n_children")
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
  'Gsk
  'GskGLShaderNode
  'glshader-node:get-shader
  'get-shader
  'gsk_gl_shader_node_get_shader
  '((name . "get_shader")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskGLShader")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskInsetShadowNode>
       "GskInsetShadowNode"
       "gsk_inset_shadow_node_get_type")

(^function
  'Gsk
  'GskInsetShadowNode
  'inset-shadow-node:get-blur-radius
  'get-blur-radius
  'gsk_inset_shadow_node_get_blur_radius
  '((name . "get_blur_radius")
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
  'Gsk
  'GskInsetShadowNode
  'inset-shadow-node:get-color
  'get-color
  'gsk_inset_shadow_node_get_color
  '((name . "get_color")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRGBA")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskInsetShadowNode
  'inset-shadow-node:get-dx
  'get-dx
  'gsk_inset_shadow_node_get_dx
  '((name . "get_dx")
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
  'Gsk
  'GskInsetShadowNode
  'inset-shadow-node:get-dy
  'get-dy
  'gsk_inset_shadow_node_get_dy
  '((name . "get_dy")
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
  'Gsk
  'GskInsetShadowNode
  'inset-shadow-node:get-spread
  'get-spread
  'gsk_inset_shadow_node_get_spread
  '((name . "get_spread")
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

(^type 'Gsk
       '<GskLinearGradientNode>
       "GskLinearGradientNode"
       "gsk_linear_gradient_node_get_type")

(^function
  'Gsk
  'GskLinearGradientNode
  'linear-gradient-node:get-end
  'get-end
  'gsk_linear_gradient_node_get_end
  '((name . "get_end")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskLinearGradientNode
  'linear-gradient-node:get-n-color-stops
  'get-n-color-stops
  'gsk_linear_gradient_node_get_n_color_stops
  '((name . "get_n_color_stops")
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
  'Gsk
  'GskLinearGradientNode
  'linear-gradient-node:get-start
  'get-start
  'gsk_linear_gradient_node_get_start
  '((name . "get_start")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskMaskMode>
       "GskMaskMode"
       "gsk_mask_mode_get_type")

(^enum-conv 'mask-mode 'GskMaskMode)

(^type 'Gsk
       '<GskMaskNode>
       "GskMaskNode"
       "gsk_mask_node_get_type")

(^function
  'Gsk
  'GskMaskNode
  'mask-node:new
  'new
  'gsk_mask_node_new
  '((name . "new")
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "source")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "mask")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "mask_mode")
            (meta (arg-type . enum)
                  (gtype-name . "GskMaskMode")
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
            (gtype-name . "GskMaskNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskMaskNode
  'mask-node:get-mask
  'get-mask
  'gsk_mask_node_get_mask
  '((name . "get_mask")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskMaskNode
  'mask-node:get-mask-mode
  'get-mask-mode
  'gsk_mask_node_get_mask_mode
  '((name . "get_mask_mode")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GskMaskMode")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskMaskNode
  'mask-node:get-source
  'get-source
  'gsk_mask_node_get_source
  '((name . "get_source")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskNglRenderer>
       "GskNglRenderer"
       "gsk_ngl_renderer_get_type")

(^function
  'Gsk
  'GskNglRenderer
  'ngl-renderer:new
  'new
  'gsk_ngl_renderer_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderer")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskOpacityNode>
       "GskOpacityNode"
       "gsk_opacity_node_get_type")

(^function
  'Gsk
  'GskOpacityNode
  'opacity-node:new
  'new
  'gsk_opacity_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "opacity")
            (meta (arg-type . float)
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
            (gtype-name . "GskOpacityNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskOpacityNode
  'opacity-node:get-child
  'get-child
  'gsk_opacity_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskOpacityNode
  'opacity-node:get-opacity
  'get-opacity
  'gsk_opacity_node_get_opacity
  '((name . "get_opacity")
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

(^type 'Gsk
       '<GskOutsetShadowNode>
       "GskOutsetShadowNode"
       "gsk_outset_shadow_node_get_type")

(^function
  'Gsk
  'GskOutsetShadowNode
  'outset-shadow-node:get-blur-radius
  'get-blur-radius
  'gsk_outset_shadow_node_get_blur_radius
  '((name . "get_blur_radius")
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
  'Gsk
  'GskOutsetShadowNode
  'outset-shadow-node:get-color
  'get-color
  'gsk_outset_shadow_node_get_color
  '((name . "get_color")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRGBA")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskOutsetShadowNode
  'outset-shadow-node:get-dx
  'get-dx
  'gsk_outset_shadow_node_get_dx
  '((name . "get_dx")
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
  'Gsk
  'GskOutsetShadowNode
  'outset-shadow-node:get-dy
  'get-dy
  'gsk_outset_shadow_node_get_dy
  '((name . "get_dy")
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
  'Gsk
  'GskOutsetShadowNode
  'outset-shadow-node:get-spread
  'get-spread
  'gsk_outset_shadow_node_get_spread
  '((name . "get_spread")
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

(^type 'Gsk
       '<GskRadialGradientNode>
       "GskRadialGradientNode"
       "gsk_radial_gradient_node_get_type")

(^function
  'Gsk
  'GskRadialGradientNode
  'radial-gradient-node:get-center
  'get-center
  'gsk_radial_gradient_node_get_center
  '((name . "get_center")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRadialGradientNode
  'radial-gradient-node:get-end
  'get-end
  'gsk_radial_gradient_node_get_end
  '((name . "get_end")
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
  'Gsk
  'GskRadialGradientNode
  'radial-gradient-node:get-hradius
  'get-hradius
  'gsk_radial_gradient_node_get_hradius
  '((name . "get_hradius")
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
  'Gsk
  'GskRadialGradientNode
  'radial-gradient-node:get-n-color-stops
  'get-n-color-stops
  'gsk_radial_gradient_node_get_n_color_stops
  '((name . "get_n_color_stops")
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
  'Gsk
  'GskRadialGradientNode
  'radial-gradient-node:get-start
  'get-start
  'gsk_radial_gradient_node_get_start
  '((name . "get_start")
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
  'Gsk
  'GskRadialGradientNode
  'radial-gradient-node:get-vradius
  'get-vradius
  'gsk_radial_gradient_node_get_vradius
  '((name . "get_vradius")
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
  'Gsk
  'GskRenderNode
  'render-node:deserialize
  'deserialize
  'gsk_render_node_deserialize
  '((name . "deserialize")
    (s-input-req . 1)
    (s-input-opt . 2)
    (c-input-len . 3)
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
            (s-input-pos . 0))
           ((name . "error_func")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "ParseErrorFunc")
                    (s-input-req . 3)
                    (s-input-opt . 1)
                    (c-input-len . 4)
                    (pdata ((name . "start")
                            (meta (arg-type . GBoxed)
                                  (gtype-name . "void")
                                  (flags ptr in)
                                  (item-size . 40)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 0)
                            (c-input-pos . 0)
                            (s-input-pos . 0))
                           ((name . "end")
                            (meta (arg-type . GBoxed)
                                  (gtype-name . "void")
                                  (flags ptr in)
                                  (item-size . 40)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
                            (i . 1)
                            (c-input-pos . 1)
                            (s-input-pos . 1))
                           ((name . "error")
                            (meta (arg-type . GError)
                                  (flags ptr in)
                                  (item-size . 16)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . required)
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
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRenderNode
  'render-node:draw
  'draw
  'gsk_render_node_draw
  '((name . "draw")
    (is-method . #t)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRenderNode
  'render-node:get-bounds!
  'get-bounds!
  'gsk_render_node_get_bounds
  '((name . "get_bounds")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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

(^type 'Gsk
       '<GskRenderNodeType>
       "GskRenderNodeType"
       "gsk_render_node_type_get_type")

(^enum-conv 'render-node-type 'GskRenderNodeType)

(^function
  'Gsk
  'GskRenderNode
  'render-node:get-node-type
  'get-node-type
  'gsk_render_node_get_node_type
  '((name . "get_node_type")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GskRenderNodeType")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRenderNode
  'render-node:ref
  'ref
  'gsk_render_node_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRenderNode
  'render-node:serialize
  'serialize
  'gsk_render_node_serialize
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
  'Gsk
  'GskRenderNode
  'render-node:unref
  'unref
  'gsk_render_node_unref
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
  'Gsk
  'GskRenderNode
  'render-node:write-to-file?
  'write-to-file?
  'gsk_render_node_write_to_file
  '((name . "write_to_file")
    (is-method . #t)
    (can-throw-gerror . #t)
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

(^type 'Gdk
       '<GdkSurface>
       "GdkSurface"
       "gdk_surface_get_type")

(^function
  'Gsk
  'GskRenderer
  'renderer:new-for-surface
  'new-for-surface
  'gsk_renderer_new_for_surface
  '((name . "new_for_surface")
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
            (gtype-name . "GskRenderer")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRenderer
  'renderer:get-surface
  'get-surface
  'gsk_renderer_get_surface
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
  'Gsk
  'GskRenderer
  'renderer:is-realized?
  'is-realized?
  'gsk_renderer_is_realized
  '((name . "is_realized")
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
  'Gsk
  'GskRenderer
  'renderer:realize?
  'realize?
  'gsk_renderer_realize
  '((name . "realize")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "surface")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkSurface")
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

(^type 'cairo
       '<CairoRegion>
       "CairoRegion"
       "cairo_gobject_region_get_type")

(^function
  'Gsk
  'GskRenderer
  'renderer:render
  'render
  'gsk_renderer_render
  '((name . "render")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "root")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
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

(^type 'GObject
       '<GObject>
       "GObject"
       "g_object_get_type")

(^type 'Gdk
       '<GdkPaintable>
       "GdkPaintable"
       "gdk_paintable_get_type")

(^type 'Gio '<GIcon> "GIcon" "g_icon_get_type")

(^type 'Gio
       '<GLoadableIcon>
       "GLoadableIcon"
       "g_loadable_icon_get_type")

(^type 'Gdk
       '<GdkTexture>
       "GdkTexture"
       "gdk_texture_get_type")

(^function
  'Gsk
  'GskRenderer
  'renderer:render-texture
  'render-texture
  'gsk_renderer_render_texture
  '((name . "render_texture")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "root")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "viewport")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags ptr in nullable)
                  (item-size . 16)
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
            (gtype-name . "GdkTexture")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRenderer
  'renderer:unrealize
  'unrealize
  'gsk_renderer_unrealize
  '((name . "unrealize")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^property
  'GskRenderer
  'renderer:realized
  'realized
  'realized)

(^property
  'GskRenderer
  'renderer:surface
  'surface
  'surface)

(^type 'Gsk
       '<GskRepeatNode>
       "GskRepeatNode"
       "gsk_repeat_node_get_type")

(^function
  'Gsk
  'GskRepeatNode
  'repeat-node:new
  'new
  'gsk_repeat_node_new
  '((name . "new")
    (s-input-req . 2)
    (s-input-opt . 1)
    (c-input-len . 3)
    (pdata ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "child_bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRepeatNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRepeatNode
  'repeat-node:get-child
  'get-child
  'gsk_repeat_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskRepeatNode
  'repeat-node:get-child-bounds
  'get-child-bounds
  'gsk_repeat_node_get_child_bounds
  '((name . "get_child_bounds")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskRepeatingLinearGradientNode>
       "GskRepeatingLinearGradientNode"
       "gsk_repeating_linear_gradient_node_get_type")

(^type 'Gsk
       '<GskRepeatingRadialGradientNode>
       "GskRepeatingRadialGradientNode"
       "gsk_repeating_radial_gradient_node_get_type")

(^type 'Gsk
       '<GskRoundedClipNode>
       "GskRoundedClipNode"
       "gsk_rounded_clip_node_get_type")

(^function
  'Gsk
  'GskRoundedClipNode
  'rounded-clip-node:get-child
  'get-child
  'gsk_rounded_clip_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskScalingFilter>
       "GskScalingFilter"
       "gsk_scaling_filter_get_type")

(^enum-conv 'scaling-filter 'GskScalingFilter)

(^type 'Gsk
       '<GskSerializationError>
       "GskSerializationError"
       "gsk_serialization_error_get_type")

(^enum-conv
  'serialization-error
  'GskSerializationError)

(^function
  'Gsk
  'GskSerializationError
  'serialization-error:quark
  'quark
  'gsk_serialization_error_quark
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

(^type 'Gsk
       '<GskShaderArgsBuilder>
       "GskShaderArgsBuilder"
       "gsk_shader_args_builder_get_type")

(^function
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:new
  'new
  'gsk_shader_args_builder_new
  '((name . "new")
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
    (pdata ((name . "shader")
            (meta (arg-type . GObject)
                  (gtype-name . "GskGLShader")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "initial_values")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GBytes")
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
            (gtype-name . "GskShaderArgsBuilder")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:ref
  'ref
  'gsk_shader_args_builder_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GskShaderArgsBuilder")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-bool
  'set-bool
  'gsk_shader_args_builder_set_bool
  '((name . "set_bool")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
           ((name . "value")
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
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-float
  'set-float
  'gsk_shader_args_builder_set_float
  '((name . "set_float")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
           ((name . "value")
            (meta (arg-type . float)
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
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-int
  'set-int
  'gsk_shader_args_builder_set_int
  '((name . "set_int")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-uint
  'set-uint
  'gsk_shader_args_builder_set_uint
  '((name . "set_uint")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-vec2
  'set-vec2
  'gsk_shader_args_builder_set_vec2
  '((name . "set_vec2")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-vec3
  'set-vec3
  'gsk_shader_args_builder_set_vec3
  '((name . "set_vec3")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:set-vec4
  'set-vec4
  'gsk_shader_args_builder_set_vec4
  '((name . "set_vec4")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "idx")
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
           ((name . "value")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:to-args
  'to-args
  'gsk_shader_args_builder_to_args
  '((name . "to_args")
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
  'Gsk
  'GskShaderArgsBuilder
  'shader-args-builder:unref
  'unref
  'gsk_shader_args_builder_unref
  '((name . "unref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gsk
       '<GskShadowNode>
       "GskShadowNode"
       "gsk_shadow_node_get_type")

(^function
  'Gsk
  'GskShadowNode
  'shadow-node:get-child
  'get-child
  'gsk_shadow_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskShadowNode
  'shadow-node:get-n-shadows
  'get-n-shadows
  'gsk_shadow_node_get_n_shadows
  '((name . "get_n_shadows")
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

(^type 'Gsk
       '<GskTextNode>
       "GskTextNode"
       "gsk_text_node_get_type")

(^type 'Pango
       '<PangoFont>
       "PangoFont"
       "pango_font_get_type")

(^sized-type
  'Pango
  '<PangoGlyphString>
  "PangoGlyphString"
  "pango_glyph_string_get_type"
  32)

(^function
  'Gsk
  'GskTextNode
  'text-node:new
  'new
  'gsk_text_node_new
  '((name . "new")
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
           ((name . "color")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GdkRGBA")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "offset")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags ptr in)
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
      (meta (arg-type . GObject)
            (gtype-name . "GskTextNode")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextNode
  'text-node:get-color
  'get-color
  'gsk_text_node_get_color
  '((name . "get_color")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkRGBA")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextNode
  'text-node:get-font
  'get-font
  'gsk_text_node_get_font
  '((name . "get_font")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "PangoFont")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextNode
  'text-node:get-num-glyphs
  'get-num-glyphs
  'gsk_text_node_get_num_glyphs
  '((name . "get_num_glyphs")
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
  'Gsk
  'GskTextNode
  'text-node:get-offset
  'get-offset
  'gsk_text_node_get_offset
  '((name . "get_offset")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextNode
  'text-node:has-color-glyphs?
  'has-color-glyphs?
  'gsk_text_node_has_color_glyphs
  '((name . "has_color_glyphs")
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

(^type 'Gsk
       '<GskTextureNode>
       "GskTextureNode"
       "gsk_texture_node_get_type")

(^function
  'Gsk
  'GskTextureNode
  'texture-node:new
  'new
  'gsk_texture_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
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
           ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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
      (meta (arg-type . GObject)
            (gtype-name . "GskTextureNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextureNode
  'texture-node:get-texture
  'get-texture
  'gsk_texture_node_get_texture
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

(^type 'Gsk
       '<GskTextureScaleNode>
       "GskTextureScaleNode"
       "gsk_texture_scale_node_get_type")

(^function
  'Gsk
  'GskTextureScaleNode
  'texture-scale-node:new
  'new
  'gsk_texture_scale_node_new
  '((name . "new")
    (s-input-req . 3)
    (c-input-len . 3)
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
           ((name . "bounds")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "filter")
            (meta (arg-type . enum)
                  (gtype-name . "GskScalingFilter")
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
            (gtype-name . "GskTextureScaleNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextureScaleNode
  'texture-scale-node:get-filter
  'get-filter
  'gsk_texture_scale_node_get_filter
  '((name . "get_filter")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GskScalingFilter")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTextureScaleNode
  'texture-scale-node:get-texture
  'get-texture
  'gsk_texture_scale_node_get_texture
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

(^type 'Gsk
       '<GskTransform>
       "GskTransform"
       "gsk_transform_get_type")

(^function
  'Gsk
  'GskTransform
  'transform:new
  'new
  'gsk_transform_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:equal?
  'equal?
  'gsk_transform_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "second")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GskTransform")
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

(^type 'Gsk
       '<GskTransformCategory>
       "GskTransformCategory"
       "gsk_transform_category_get_type")

(^enum-conv
  'transform-category
  'GskTransformCategory)

(^function
  'Gsk
  'GskTransform
  'transform:get-category
  'get-category
  'gsk_transform_get_category
  '((name . "get_category")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GskTransformCategory")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:invert
  'invert
  'gsk_transform_invert
  '((name . "invert")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:matrix
  'matrix
  'gsk_transform_matrix
  '((name . "matrix")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:perspective
  'perspective
  'gsk_transform_perspective
  '((name . "perspective")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "depth")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out)
            (transfer . everything))
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
  'Gsk
  'GskTransform
  'transform:print
  'print
  'gsk_transform_print
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
  'Gsk
  'GskTransform
  'transform:ref
  'ref
  'gsk_transform_ref
  '((name . "ref")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:rotate
  'rotate
  'gsk_transform_rotate
  '((name . "rotate")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "angle")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:rotate-3d
  'rotate-3d
  'gsk_transform_rotate_3d
  '((name . "rotate_3d")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "angle")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "axis")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:scale
  'scale
  'gsk_transform_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "factor_x")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor_y")
            (meta (arg-type . float)
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
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:scale-3d
  'scale-3d
  'gsk_transform_scale_3d
  '((name . "scale_3d")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "factor_x")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor_y")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "factor_z")
            (meta (arg-type . float)
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
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:skew
  'skew
  'gsk_transform_skew
  '((name . "skew")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "skew_x")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "skew_y")
            (meta (arg-type . float)
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
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:to-2d
  'to-2d
  'gsk_transform_to_2d
  '((name . "to_2d")
    (is-method . #t)
    (s-output-len . 6)
    (c-output-len . 6)
    (pdata ((name . "out_xx")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "out_yx")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "out_xy")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 2)
            (s-output-pos . 2))
           ((name . "out_yy")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 3)
            (s-output-pos . 3))
           ((name . "out_dx")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 4)
            (s-output-pos . 4))
           ((name . "out_dy")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 5)
            (c-output-pos . 5)
            (s-output-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:to-2d-components
  'to-2d-components
  'gsk_transform_to_2d_components
  '((name . "to_2d_components")
    (is-method . #t)
    (s-output-len . 7)
    (c-output-len . 7)
    (pdata ((name . "out_skew_x")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "out_skew_y")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "out_scale_x")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 2)
            (s-output-pos . 2))
           ((name . "out_scale_y")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 3)
            (s-output-pos . 3))
           ((name . "out_angle")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 4)
            (s-output-pos . 4))
           ((name . "out_dx")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 5)
            (c-output-pos . 5)
            (s-output-pos . 5))
           ((name . "out_dy")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 6)
            (c-output-pos . 6)
            (s-output-pos . 6)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:to-affine
  'to-affine
  'gsk_transform_to_affine
  '((name . "to_affine")
    (is-method . #t)
    (s-output-len . 4)
    (c-output-len . 4)
    (pdata ((name . "out_scale_x")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "out_scale_y")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "out_dx")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 2)
            (s-output-pos . 2))
           ((name . "out_dy")
            (meta (arg-type . float)
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
  'Gsk
  'GskTransform
  'transform:to-matrix!
  'to-matrix!
  'gsk_transform_to_matrix
  '((name . "to_matrix")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "out_matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags out preallocated)
                  (item-size . 64)
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
  'Gsk
  'GskTransform
  'transform:to-string
  'to-string
  'gsk_transform_to_string
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
  'Gsk
  'GskTransform
  'transform:to-translate
  'to-translate
  'gsk_transform_to_translate
  '((name . "to_translate")
    (is-method . #t)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "out_dx")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "out_dy")
            (meta (arg-type . float)
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
  'Gsk
  'GskTransform
  'transform:transform
  'transform
  'gsk_transform_transform
  '((name . "transform")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "other")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GskTransform")
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
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:transform-bounds!
  'transform-bounds!
  'gsk_transform_transform_bounds
  '((name . "transform_bounds")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "rect")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "out_rect")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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

(^function
  'Gsk
  'GskTransform
  'transform:transform-point!
  'transform-point!
  'gsk_transform_transform_point
  '((name . "transform_point")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "point")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "out_point")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
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

(^function
  'Gsk
  'GskTransform
  'transform:translate
  'translate
  'gsk_transform_translate
  '((name . "translate")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "point")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
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
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GraphenePoint3D>
  "GraphenePoint3D"
  "graphene_point3d_get_type"
  12)

(^function
  'Gsk
  'GskTransform
  'transform:translate-3d
  'translate-3d
  'gsk_transform_translate_3d
  '((name . "translate_3d")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "point")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in)
                  (item-size . 12)
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
            (gtype-name . "GskTransform")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransform
  'transform:unref
  'unref
  'gsk_transform_unref
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
  'Gsk
  'GskTransform
  'transform:parse
  'parse
  'gsk_transform_parse
  '((name . "parse")
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "out_transform")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GskTransform")
                  (flags ptr out)
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

(^type 'Gsk
       '<GskTransformNode>
       "GskTransformNode"
       "gsk_transform_node_get_type")

(^function
  'Gsk
  'GskTransformNode
  'transform-node:new
  'new
  'gsk_transform_node_new
  '((name . "new")
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "child")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "transform")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GskTransform")
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
            (gtype-name . "GskTransformNode")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransformNode
  'transform-node:get-child
  'get-child
  'gsk_transform_node_get_child
  '((name . "get_child")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GskRenderNode")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  'GskTransformNode
  'transform-node:get-transform
  'get-transform
  'gsk_transform_node_get_transform
  '((name . "get_transform")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GskTransform")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  #f
  'gsk:serialization-error-quark
  'serialization-error-quark
  'gsk_serialization_error_quark
  '((name . "serialization_error_quark")
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
  'Gsk
  #f
  'gsk:transform-parse
  'transform-parse
  'gsk_transform_parse
  '((name . "transform_parse")
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "out_transform")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GskTransform")
                  (flags ptr out)
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

(^sized-type
  'GObject
  '<GValue>
  "GValue"
  "g_value_get_type"
  24)

(^function
  'Gsk
  #f
  'gsk:value-dup-render-node
  'value-dup-render-node
  'gsk_value_dup_render_node
  '((name . "value_dup_render_node")
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
            (gtype-name . "GskRenderNode")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  #f
  'gsk:value-get-render-node
  'value-get-render-node
  'gsk_value_get_render_node
  '((name . "value_get_render_node")
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
            (gtype-name . "GskRenderNode")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Gsk
  #f
  'gsk:value-set-render-node
  'value-set-render-node
  'gsk_value_set_render_node
  '((name . "value_set_render_node")
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
           ((name . "node")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
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
  'Gsk
  #f
  'gsk:value-take-render-node
  'value-take-render-node
  'gsk_value_take_render_node
  '((name . "value_take_render_node")
    (s-input-req . 1)
    (s-input-opt . 1)
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
           ((name . "node")
            (meta (arg-type . GObject)
                  (gtype-name . "GskRenderNode")
                  (flags ptr in nullable)
                  (transfer . everything))
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


)
