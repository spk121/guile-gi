(define-module (gi cairo-1-0)
  #:use-module (gi runtime)
  #:export(<CairoContext> <CairoDevice> <CairoSurface> <CairoPattern> 
           <CairoRegion> <cairo_status_t> symbol->status number->status 
           status->symbol status->number <cairo_content_t> symbol->content 
           number->content content->symbol content->number <cairo_operator_t> 
           symbol->operator number->operator operator->symbol operator->number 
           <cairo_antialias_t> symbol->antialias number->antialias 
           antialias->symbol antialias->number <cairo_fill_rule_t> 
           symbol->fill-rule number->fill-rule fill-rule->symbol 
           fill-rule->number <cairo_line_cap_t> symbol->line-cap 
           number->line-cap line-cap->symbol line-cap->number 
           <cairo_line_join_t> symbol->line-join number->line-join 
           line-join->symbol line-join->number <cairo_text_cluster_flags_t> 
           symbol->text-cluster-flags number->text-cluster-flags 
           text-cluster-flags->symbol text-cluster-flags->number 
           <cairo_font_slant_t> symbol->font-slant number->font-slant 
           font-slant->symbol font-slant->number <cairo_font_weight_t> 
           symbol->font-weight number->font-weight font-weight->symbol 
           font-weight->number <cairo_subpixel_order_t> symbol->subpixel-order 
           number->subpixel-order subpixel-order->symbol subpixel-order->number 
           <cairo_hint_style_t> symbol->hint-style number->hint-style 
           hint-style->symbol hint-style->number <cairo_hint_metrics_t> 
           symbol->hint-metrics number->hint-metrics hint-metrics->symbol 
           hint-metrics->number <CairoFontOptions> <cairo_font_type_t> 
           symbol->font-type number->font-type font-type->symbol 
           font-type->number <cairo_path_data_type_t> symbol->path-data-type 
           number->path-data-type path-data-type->symbol path-data-type->number 
           <cairo_device_type_t> symbol->device-type number->device-type 
           device-type->symbol device-type->number <cairo_surface_type_t> 
           symbol->surface-type number->surface-type surface-type->symbol 
           surface-type->number <cairo_format_t> symbol->format number->format 
           format->symbol format->number <cairo_pattern_type_t> 
           symbol->pattern-type number->pattern-type pattern-type->symbol 
           pattern-type->number <cairo_extend_t> symbol->extend number->extend 
           extend->symbol extend->number <cairo_filter_t> symbol->filter 
           number->filter filter->symbol filter->number 
           <cairo_region_overlap_t> symbol->region-overlap 
           number->region-overlap region-overlap->symbol region-overlap->number 
           <CairoFontFace> <CairoScaledFont> <CairoRectangle> 
           <CairoRectangleInt> image-surface-create cairo:image-surface-create))

(eval-when (expand load eval)
(^library
  "cairo"
  "1.0"
  '("libcairo-gobject-2.dll"))

(^initialize)

(^type 'cairo
       '<CairoContext>
       "CairoContext"
       "cairo_gobject_context_get_type")

(^type 'cairo
       '<CairoDevice>
       "CairoDevice"
       "cairo_gobject_device_get_type")

(^type 'cairo
       '<CairoSurface>
       "CairoSurface"
       "cairo_gobject_surface_get_type")

(^type 'cairo
       '<CairoPattern>
       "CairoPattern"
       "cairo_gobject_pattern_get_type")

(^type 'cairo
       '<CairoRegion>
       "CairoRegion"
       "cairo_gobject_region_get_type")

(^type 'cairo
       '<cairo_status_t>
       "cairo_status_t"
       "cairo_gobject_status_get_type")

(^enum-conv 'status 'cairo_status_t)

(^type 'cairo
       '<cairo_content_t>
       "cairo_content_t"
       "cairo_gobject_content_get_type")

(^enum-conv 'content 'cairo_content_t)

(^type 'cairo
       '<cairo_operator_t>
       "cairo_operator_t"
       "cairo_gobject_operator_get_type")

(^enum-conv 'operator 'cairo_operator_t)

(^type 'cairo
       '<cairo_antialias_t>
       "cairo_antialias_t"
       "cairo_gobject_antialias_get_type")

(^enum-conv 'antialias 'cairo_antialias_t)

(^type 'cairo
       '<cairo_fill_rule_t>
       "cairo_fill_rule_t"
       "cairo_gobject_fill_rule_get_type")

(^enum-conv 'fill-rule 'cairo_fill_rule_t)

(^type 'cairo
       '<cairo_line_cap_t>
       "cairo_line_cap_t"
       "cairo_gobject_line_cap_get_type")

(^enum-conv 'line-cap 'cairo_line_cap_t)

(^type 'cairo
       '<cairo_line_join_t>
       "cairo_line_join_t"
       "cairo_gobject_line_join_get_type")

(^enum-conv 'line-join 'cairo_line_join_t)

(^type 'cairo
       '<cairo_text_cluster_flags_t>
       "cairo_text_cluster_flags_t"
       "cairo_gobject_text_cluster_flags_get_type")

(^enum-conv
  'text-cluster-flags
  'cairo_text_cluster_flags_t)

(^type 'cairo
       '<cairo_font_slant_t>
       "cairo_font_slant_t"
       "cairo_gobject_font_slant_get_type")

(^enum-conv 'font-slant 'cairo_font_slant_t)

(^type 'cairo
       '<cairo_font_weight_t>
       "cairo_font_weight_t"
       "cairo_gobject_font_weight_get_type")

(^enum-conv 'font-weight 'cairo_font_weight_t)

(^type 'cairo
       '<cairo_subpixel_order_t>
       "cairo_subpixel_order_t"
       "cairo_gobject_subpixel_order_get_type")

(^enum-conv
  'subpixel-order
  'cairo_subpixel_order_t)

(^type 'cairo
       '<cairo_hint_style_t>
       "cairo_hint_style_t"
       "cairo_gobject_hint_style_get_type")

(^enum-conv 'hint-style 'cairo_hint_style_t)

(^type 'cairo
       '<cairo_hint_metrics_t>
       "cairo_hint_metrics_t"
       "cairo_gobject_hint_metrics_get_type")

(^enum-conv 'hint-metrics 'cairo_hint_metrics_t)

(^type 'cairo
       '<CairoFontOptions>
       "CairoFontOptions"
       "cairo_gobject_font_options_get_type")

(^type 'cairo
       '<cairo_font_type_t>
       "cairo_font_type_t"
       "cairo_gobject_font_type_get_type")

(^enum-conv 'font-type 'cairo_font_type_t)

(^type 'cairo
       '<cairo_path_data_type_t>
       "cairo_path_data_type_t"
       "cairo_gobject_path_data_type_get_type")

(^enum-conv
  'path-data-type
  'cairo_path_data_type_t)

(^type 'cairo
       '<cairo_device_type_t>
       "cairo_device_type_t"
       "cairo_gobject_device_type_get_type")

(^enum-conv 'device-type 'cairo_device_type_t)

(^type 'cairo
       '<cairo_surface_type_t>
       "cairo_surface_type_t"
       "cairo_gobject_surface_type_get_type")

(^enum-conv 'surface-type 'cairo_surface_type_t)

(^type 'cairo
       '<cairo_format_t>
       "cairo_format_t"
       "cairo_gobject_format_get_type")

(^enum-conv 'format 'cairo_format_t)

(^type 'cairo
       '<cairo_pattern_type_t>
       "cairo_pattern_type_t"
       "cairo_gobject_pattern_type_get_type")

(^enum-conv 'pattern-type 'cairo_pattern_type_t)

(^type 'cairo
       '<cairo_extend_t>
       "cairo_extend_t"
       "cairo_gobject_extend_get_type")

(^enum-conv 'extend 'cairo_extend_t)

(^type 'cairo
       '<cairo_filter_t>
       "cairo_filter_t"
       "cairo_gobject_filter_get_type")

(^enum-conv 'filter 'cairo_filter_t)

(^type 'cairo
       '<cairo_region_overlap_t>
       "cairo_region_overlap_t"
       "cairo_gobject_region_overlap_get_type")

(^enum-conv
  'region-overlap
  'cairo_region_overlap_t)

(^type 'cairo
       '<CairoFontFace>
       "CairoFontFace"
       "cairo_gobject_font_face_get_type")

(^type 'cairo
       '<CairoScaledFont>
       "CairoScaledFont"
       "cairo_gobject_scaled_font_get_type")

(^sized-type
  'cairo
  '<CairoRectangle>
  "CairoRectangle"
  "cairo_gobject_rectangle_get_type"
  32)

(^sized-type
  'cairo
  '<CairoRectangleInt>
  "CairoRectangleInt"
  "cairo_gobject_rectangle_int_get_type"
  16)

(^function
  'cairo
  #f
  'cairo:image-surface-create
  'image-surface-create
  'cairo_image_surface_create
  '((name . "image_surface_create")
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))


)
