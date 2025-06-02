(define-module (gi GdkPixbuf-2-0)
  #:use-module (gi runtime)
  #:use-module (gi Gio-2-0)
  #:use-module (gi GModule-2-0)
  #:export(<GdkColorspace> symbol->colorspace number->colorspace 
           colorspace->symbol colorspace->number <GdkInterpType> 
           symbol->interp-type number->interp-type interp-type->symbol 
           interp-type->number PIXBUF_MAJOR PIXBUF_MICRO PIXBUF_MINOR 
           PIXBUF_VERSION <GObject> <GIcon> <GLoadableIcon> <GdkPixbuf> new 
           pixbuf:new <GBytes> new-from-bytes pixbuf:new-from-bytes 
           new-from-data pixbuf:new-from-data new-from-file 
           pixbuf:new-from-file new-from-file-at-scale 
           pixbuf:new-from-file-at-scale new-from-file-at-scale-utf8 
           pixbuf:new-from-file-at-scale-utf8 new-from-file-at-size 
           pixbuf:new-from-file-at-size new-from-file-at-size-utf8 
           pixbuf:new-from-file-at-size-utf8 new-from-file-utf8 
           pixbuf:new-from-file-utf8 new-from-inline pixbuf:new-from-inline 
           new-from-resource pixbuf:new-from-resource 
           new-from-resource-at-scale pixbuf:new-from-resource-at-scale 
           <GInputStream> <GCancellable> new-from-stream pixbuf:new-from-stream 
           new-from-stream-at-scale pixbuf:new-from-stream-at-scale 
           <GAsyncResult> new-from-stream-finish pixbuf:new-from-stream-finish 
           new-from-xpm-data pixbuf:new-from-xpm-data calculate-rowstride 
           pixbuf:calculate-rowstride <GdkPixbufFormat> get-file-info 
           pixbuf:get-file-info get-file-info-async pixbuf:get-file-info-async 
           get-file-info-finish pixbuf:get-file-info-finish get-formats 
           pixbuf:get-formats init-modules? pixbuf:init-modules? 
           new-from-stream-async pixbuf:new-from-stream-async 
           new-from-stream-at-scale-async pixbuf:new-from-stream-at-scale-async 
           save-to-stream-finish? pixbuf:save-to-stream-finish? add-alpha 
           pixbuf:add-alpha apply-embedded-orientation 
           pixbuf:apply-embedded-orientation composite pixbuf:composite 
           composite-color pixbuf:composite-color composite-color-simple 
           pixbuf:composite-color-simple copy pixbuf:copy copy-area 
           pixbuf:copy-area copy-options? pixbuf:copy-options? fill pixbuf:fill 
           flip pixbuf:flip get-bits-per-sample pixbuf:get-bits-per-sample 
           get-byte-length pixbuf:get-byte-length get-colorspace 
           pixbuf:get-colorspace get-has-alpha? pixbuf:get-has-alpha? 
           get-height pixbuf:get-height get-n-channels pixbuf:get-n-channels 
           get-option pixbuf:get-option get-options pixbuf:get-options 
           get-pixels pixbuf:get-pixels get-rowstride pixbuf:get-rowstride 
           get-width pixbuf:get-width new-subpixbuf pixbuf:new-subpixbuf 
           read-pixel-bytes pixbuf:read-pixel-bytes read-pixels 
           pixbuf:read-pixels remove-option? pixbuf:remove-option? 
           <GdkPixbufRotation> symbol->pixbuf-rotation number->pixbuf-rotation 
           pixbuf-rotation->symbol pixbuf-rotation->number rotate-simple 
           pixbuf:rotate-simple saturate-and-pixelate 
           pixbuf:saturate-and-pixelate save-to-bufferv pixbuf:save-to-bufferv 
           save-to-callbackv? pixbuf:save-to-callbackv? <GOutputStream> 
           save-to-streamv? pixbuf:save-to-streamv? save-to-streamv-async 
           pixbuf:save-to-streamv-async savev? pixbuf:savev? savev-utf8? 
           pixbuf:savev-utf8? scale pixbuf:scale scale-simple 
           pixbuf:scale-simple set-option? pixbuf:set-option? 
           <GdkPixbufAnimation> new-from-file pixbuf-animation:new-from-file 
           new-from-file-utf8 pixbuf-animation:new-from-file-utf8 
           new-from-resource pixbuf-animation:new-from-resource new-from-stream 
           pixbuf-animation:new-from-stream new-from-stream-finish 
           pixbuf-animation:new-from-stream-finish new-from-stream-async 
           pixbuf-animation:new-from-stream-async get-height 
           pixbuf-animation:get-height <GdkPixbufAnimationIter> 
           get-static-image pixbuf-animation:get-static-image get-width 
           pixbuf-animation:get-width is-static-image? 
           pixbuf-animation:is-static-image? get-delay-time 
           pixbuf-animation-iter:get-delay-time get-pixbuf 
           pixbuf-animation-iter:get-pixbuf on-currently-loading-frame? 
           pixbuf-animation-iter:on-currently-loading-frame? <GdkPixbufError> 
           symbol->pixbuf-error number->pixbuf-error pixbuf-error->symbol 
           pixbuf-error->number quark pixbuf-error:quark copy 
           pixbuf-format:copy free pixbuf-format:free get-description 
           pixbuf-format:get-description get-extensions 
           pixbuf-format:get-extensions get-license pixbuf-format:get-license 
           get-mime-types pixbuf-format:get-mime-types get-name 
           pixbuf-format:get-name is-disabled? pixbuf-format:is-disabled? 
           is-save-option-supported? pixbuf-format:is-save-option-supported? 
           is-scalable? pixbuf-format:is-scalable? is-writable? 
           pixbuf-format:is-writable? set-disabled pixbuf-format:set-disabled 
           <%GdkPixbufFormatFlags> list->pixbuf-format-flags 
           number->pixbuf-format-flags pixbuf-format-flags->list 
           pixbuf-format-flags->number <GdkPixbufLoader> new pixbuf-loader:new 
           new-with-mime-type pixbuf-loader:new-with-mime-type new-with-type 
           pixbuf-loader:new-with-type close? pixbuf-loader:close? 
           get-animation pixbuf-loader:get-animation get-format 
           pixbuf-loader:get-format get-pixbuf pixbuf-loader:get-pixbuf 
           set-size pixbuf-loader:set-size write? pixbuf-loader:write? 
           write-bytes? pixbuf-loader:write-bytes? area-prepared 
           pixbuf-loader:area-prepared area-updated pixbuf-loader:area-updated 
           closed pixbuf-loader:closed size-prepared 
           pixbuf-loader:size-prepared <GdkPixbufNonAnim> new 
           pixbuf-non-anim:new <GdkPixbufSimpleAnim> new pixbuf-simple-anim:new 
           add-frame pixbuf-simple-anim:add-frame get-loop? 
           pixbuf-simple-anim:get-loop? set-loop pixbuf-simple-anim:set-loop 
           <GdkPixbufSimpleAnimIter> pixbuf-error-quark 
           gdk-pixbuf:pixbuf-error-quark))

(eval-when (expand load eval)
(^library
  "GdkPixbuf"
  "2.0"
  '("libgdk_pixbuf-2.0-0.dll"))

(^initialize)

(^type 'GdkPixbuf
       '<GdkColorspace>
       "GdkColorspace"
       "gdk_colorspace_get_type")

(^enum-conv 'colorspace 'GdkColorspace)

(^type 'GdkPixbuf
       '<GdkInterpType>
       "GdkInterpType"
       "gdk_interp_type_get_type")

(^enum-conv 'interp-type 'GdkInterpType)

(^constant 'PIXBUF_MAJOR 2)

(^constant 'PIXBUF_MICRO 10)

(^constant 'PIXBUF_MINOR 42)

(^constant 'PIXBUF_VERSION "2.42.10")

(^type 'GObject
       '<GObject>
       "GObject"
       "g_object_get_type")

(^type 'Gio '<GIcon> "GIcon" "g_icon_get_type")

(^type 'Gio
       '<GLoadableIcon>
       "GLoadableIcon"
       "g_loadable_icon_get_type")

(^type 'GdkPixbuf
       '<GdkPixbuf>
       "GdkPixbuf"
       "gdk_pixbuf_get_type")

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new
  'new
  'gdk_pixbuf_new
  '((name . "new")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "colorspace")
            (meta (arg-type . enum)
                  (gtype-name . "GdkColorspace")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "has_alpha")
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
           ((name . "bits_per_sample")
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

(^type 'GLib
       '<GBytes>
       "GBytes"
       "g_bytes_get_type")

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-bytes
  'new-from-bytes
  'gdk_pixbuf_new_from_bytes
  '((name . "new_from_bytes")
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "data")
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
           ((name . "colorspace")
            (meta (arg-type . enum)
                  (gtype-name . "GdkColorspace")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "has_alpha")
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
           ((name . "bits_per_sample")
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
           ((name . "width")
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
           ((name . "height")
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
           ((name . "rowstride")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-data
  'new-from-data
  'gdk_pixbuf_new_from_data
  '((name . "new_from_data")
    (s-input-req . 7)
    (s-input-opt . 2)
    (c-input-len . 9)
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
           ((name . "colorspace")
            (meta (arg-type . enum)
                  (gtype-name . "GdkColorspace")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "has_alpha")
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
           ((name . "bits_per_sample")
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
           ((name . "width")
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
           ((name . "height")
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
           ((name . "rowstride")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6))
           ((name . "destroy_fn")
            (meta (arg-type . callback)
                  (flags in nullable)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "PixbufDestroyNotify")
                    (s-input-req . 1)
                    (s-input-opt . 1)
                    (c-input-len . 2)
                    (pdata ((name . "pixels")
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
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 7)
            (c-input-pos . 7)
            (s-input-pos . 7))
           ((name . "destroy_fn_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 8)
            (c-input-pos . 8)
            (s-input-pos . 8)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-file
  'new-from-file
  'gdk_pixbuf_new_from_file
  '((name . "new_from_file")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-file-at-scale
  'new-from-file-at-scale
  'gdk_pixbuf_new_from_file_at_scale
  '((name . "new_from_file_at_scale")
    (can-throw-gerror . #t)
    (s-input-req . 4)
    (c-input-len . 4)
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
            (s-input-pos . 2))
           ((name . "preserve_aspect_ratio")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-file-at-scale-utf8
  'new-from-file-at-scale-utf8
  'gdk_pixbuf_new_from_file_at_scale_utf8
  '((name . "new_from_file_at_scale_utf8")
    (can-throw-gerror . #t)
    (s-input-req . 4)
    (c-input-len . 4)
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
            (s-input-pos . 2))
           ((name . "preserve_aspect_ratio")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-file-at-size
  'new-from-file-at-size
  'gdk_pixbuf_new_from_file_at_size
  '((name . "new_from_file_at_size")
    (can-throw-gerror . #t)
    (s-input-req . 3)
    (c-input-len . 3)
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-file-at-size-utf8
  'new-from-file-at-size-utf8
  'gdk_pixbuf_new_from_file_at_size_utf8
  '((name . "new_from_file_at_size_utf8")
    (can-throw-gerror . #t)
    (s-input-req . 3)
    (c-input-len . 3)
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-file-utf8
  'new-from-file-utf8
  'gdk_pixbuf_new_from_file_utf8
  '((name . "new_from_file_utf8")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-inline
  'new-from-inline
  'gdk_pixbuf_new_from_inline
  '((name . "new_from_inline")
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (c-input-len . 3)
    (pdata ((name . "data_length")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-input-pos . 0))
           ((name . "data")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . uint8)
                     (item-size . 1)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 0))
           ((name . "copy_pixels")
            (meta (arg-type . gboolean)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-resource
  'new-from-resource
  'gdk_pixbuf_new_from_resource
  '((name . "new_from_resource")
    (can-throw-gerror . #t)
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-resource-at-scale
  'new-from-resource-at-scale
  'gdk_pixbuf_new_from_resource_at_scale
  '((name . "new_from_resource_at_scale")
    (can-throw-gerror . #t)
    (s-input-req . 4)
    (c-input-len . 4)
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
            (s-input-pos . 2))
           ((name . "preserve_aspect_ratio")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'Gio
       '<GInputStream>
       "GInputStream"
       "g_input_stream_get_type")

(^type 'Gio
       '<GCancellable>
       "GCancellable"
       "g_cancellable_get_type")

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-stream
  'new-from-stream
  'gdk_pixbuf_new_from_stream
  '((name . "new_from_stream")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
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
            (s-input-pos . 1)))
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-stream-at-scale
  'new-from-stream-at-scale
  'gdk_pixbuf_new_from_stream_at_scale
  '((name . "new_from_stream_at_scale")
    (can-throw-gerror . #t)
    (s-input-req . 4)
    (s-input-opt . 1)
    (c-input-len . 5)
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
            (s-input-pos . 2))
           ((name . "preserve_aspect_ratio")
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

(^type 'Gio
       '<GAsyncResult>
       "GAsyncResult"
       "g_async_result_get_type")

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-stream-finish
  'new-from-stream-finish
  'gdk_pixbuf_new_from_stream_finish
  '((name . "new_from_stream_finish")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "async_result")
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-xpm-data
  'new-from-xpm-data
  'gdk_pixbuf_new_from_xpm_data
  '((name . "new_from_xpm_data")
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "data")
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
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:calculate-rowstride
  'calculate-rowstride
  'gdk_pixbuf_calculate_rowstride
  '((name . "calculate_rowstride")
    (s-input-req . 5)
    (c-input-len . 5)
    (pdata ((name . "colorspace")
            (meta (arg-type . enum)
                  (gtype-name . "GdkColorspace")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "has_alpha")
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
           ((name . "bits_per_sample")
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
      (meta (arg-type . int32)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'GdkPixbuf
  '<GdkPixbufFormat>
  "GdkPixbufFormat"
  "gdk_pixbuf_format_get_type"
  64)

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-file-info
  'get-file-info
  'gdk_pixbuf_get_file_info
  '((name . "get_file_info")
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
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
            (s-input-pos . 0))
           ((name . "width")
            (meta (arg-type . int32)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
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
            (i . 2)
            (c-output-pos . 1)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPixbufFormat")
            (flags ptr out nullable)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-file-info-async
  'get-file-info-async
  'gdk_pixbuf_get_file_info_async
  '((name . "get_file_info_async")
    (s-input-req . 1)
    (s-input-opt . 3)
    (c-input-len . 4)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-file-info-finish
  'get-file-info-finish
  'gdk_pixbuf_get_file_info_finish
  '((name . "get_file_info_finish")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "async_result")
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
           ((name . "width")
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
           ((name . "height")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPixbufFormat")
            (flags ptr out nullable)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-formats
  'get-formats
  'gdk_pixbuf_get_formats
  '((name . "get_formats")
    (return-val
      (name . "%return")
      (meta (arg-type . GSList)
            (flags ptr out)
            (transfer . container)
            (params
              ((arg-type . GBoxed)
               (gtype-name . "GdkPixbufFormat")
               (item-size . 64)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:init-modules?
  'init-modules?
  'gdk_pixbuf_init_modules
  '((name . "init_modules")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "path")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-stream-async
  'new-from-stream-async
  'gdk_pixbuf_new_from_stream_async
  '((name . "new_from_stream_async")
    (s-input-req . 1)
    (s-input-opt . 3)
    (c-input-len . 4)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-from-stream-at-scale-async
  'new-from-stream-at-scale-async
  'gdk_pixbuf_new_from_stream_at_scale_async
  '((name . "new_from_stream_at_scale_async")
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
            (s-input-pos . 2))
           ((name . "preserve_aspect_ratio")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:save-to-stream-finish?
  'save-to-stream-finish?
  'gdk_pixbuf_save_to_stream_finish
  '((name . "save_to_stream_finish")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "async_result")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:add-alpha
  'add-alpha
  'gdk_pixbuf_add_alpha
  '((name . "add_alpha")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "substitute_color")
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
           ((name . "r")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "g")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "b")
            (meta (arg-type . uint8)
                  (flags in)
                  (item-size . 1)
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:apply-embedded-orientation
  'apply-embedded-orientation
  'gdk_pixbuf_apply_embedded_orientation
  '((name . "apply_embedded_orientation")
    (is-method . #t)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:composite
  'composite
  'gdk_pixbuf_composite
  '((name . "composite")
    (is-method . #t)
    (s-input-req . 11)
    (c-input-len . 11)
    (pdata ((name . "dest")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_x")
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
           ((name . "dest_y")
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
           ((name . "dest_width")
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
           ((name . "dest_height")
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
           ((name . "offset_x")
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
           ((name . "offset_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6))
           ((name . "scale_x")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 7)
            (c-input-pos . 7)
            (s-input-pos . 7))
           ((name . "scale_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 8)
            (c-input-pos . 8)
            (s-input-pos . 8))
           ((name . "interp_type")
            (meta (arg-type . enum)
                  (gtype-name . "GdkInterpType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 9)
            (c-input-pos . 9)
            (s-input-pos . 9))
           ((name . "overall_alpha")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 10)
            (c-input-pos . 10)
            (s-input-pos . 10)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:composite-color
  'composite-color
  'gdk_pixbuf_composite_color
  '((name . "composite_color")
    (is-method . #t)
    (s-input-req . 16)
    (c-input-len . 16)
    (pdata ((name . "dest")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_x")
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
           ((name . "dest_y")
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
           ((name . "dest_width")
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
           ((name . "dest_height")
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
           ((name . "offset_x")
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
           ((name . "offset_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6))
           ((name . "scale_x")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 7)
            (c-input-pos . 7)
            (s-input-pos . 7))
           ((name . "scale_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 8)
            (c-input-pos . 8)
            (s-input-pos . 8))
           ((name . "interp_type")
            (meta (arg-type . enum)
                  (gtype-name . "GdkInterpType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 9)
            (c-input-pos . 9)
            (s-input-pos . 9))
           ((name . "overall_alpha")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 10)
            (c-input-pos . 10)
            (s-input-pos . 10))
           ((name . "check_x")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 11)
            (c-input-pos . 11)
            (s-input-pos . 11))
           ((name . "check_y")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 12)
            (c-input-pos . 12)
            (s-input-pos . 12))
           ((name . "check_size")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 13)
            (c-input-pos . 13)
            (s-input-pos . 13))
           ((name . "color1")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 14)
            (c-input-pos . 14)
            (s-input-pos . 14))
           ((name . "color2")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 15)
            (c-input-pos . 15)
            (s-input-pos . 15)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:composite-color-simple
  'composite-color-simple
  'gdk_pixbuf_composite_color_simple
  '((name . "composite_color_simple")
    (is-method . #t)
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "dest_width")
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
           ((name . "dest_height")
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
           ((name . "interp_type")
            (meta (arg-type . enum)
                  (gtype-name . "GdkInterpType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "overall_alpha")
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
           ((name . "check_size")
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
           ((name . "color1")
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
           ((name . "color2")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:copy
  'copy
  'gdk_pixbuf_copy
  '((name . "copy")
    (is-method . #t)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:copy-area
  'copy-area
  'gdk_pixbuf_copy_area
  '((name . "copy_area")
    (is-method . #t)
    (s-input-req . 7)
    (c-input-len . 7)
    (pdata ((name . "src_x")
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
           ((name . "src_y")
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
            (s-input-pos . 3))
           ((name . "dest_pixbuf")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "dest_x")
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
           ((name . "dest_y")
            (meta (arg-type . int32)
                  (flags in)
                  (item-size . 4)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:copy-options?
  'copy-options?
  'gdk_pixbuf_copy_options
  '((name . "copy_options")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "dest_pixbuf")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:fill
  'fill
  'gdk_pixbuf_fill
  '((name . "fill")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "pixel")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:flip
  'flip
  'gdk_pixbuf_flip
  '((name . "flip")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "horizontal")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-bits-per-sample
  'get-bits-per-sample
  'gdk_pixbuf_get_bits_per_sample
  '((name . "get_bits_per_sample")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-byte-length
  'get-byte-length
  'gdk_pixbuf_get_byte_length
  '((name . "get_byte_length")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-colorspace
  'get-colorspace
  'gdk_pixbuf_get_colorspace
  '((name . "get_colorspace")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (gtype-name . "GdkColorspace")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-has-alpha?
  'get-has-alpha?
  'gdk_pixbuf_get_has_alpha
  '((name . "get_has_alpha")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-height
  'get-height
  'gdk_pixbuf_get_height
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-n-channels
  'get-n-channels
  'gdk_pixbuf_get_n_channels
  '((name . "get_n_channels")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-option
  'get-option
  'gdk_pixbuf_get_option
  '((name . "get_option")
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
      (meta (arg-type . utf8-string)
            (flags ptr out nullable)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-options
  'get-options
  'gdk_pixbuf_get_options
  '((name . "get_options")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GHash)
            (flags ptr out)
            (item-size . 8)
            (transfer . container)
            (params
              ((arg-type . utf8-string)
               (flags ptr)
               (item-size . 8)
               (transfer . nothing))
              ((arg-type . utf8-string)
               (flags ptr)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-pixels
  'get-pixels
  'gdk_pixbuf_get_pixels_with_length
  '((name . "get_pixels")
    (is-method . #t)
    (c-output-len . 1)
    (pdata ((name . "length")
            (meta (arg-type . uint32)
                  (flags out)
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
            (flags ptr out sized)
            (transfer . nothing)
            (params
              ((arg-type . uint8)
               (item-size . 1)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . array)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-rowstride
  'get-rowstride
  'gdk_pixbuf_get_rowstride
  '((name . "get_rowstride")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:get-width
  'get-width
  'gdk_pixbuf_get_width
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:new-subpixbuf
  'new-subpixbuf
  'gdk_pixbuf_new_subpixbuf
  '((name . "new_subpixbuf")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "src_x")
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
           ((name . "src_y")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:read-pixel-bytes
  'read-pixel-bytes
  'gdk_pixbuf_read_pixel_bytes
  '((name . "read_pixel_bytes")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:read-pixels
  'read-pixels
  'gdk_pixbuf_read_pixels
  '((name . "read_pixels")
    (is-method . #t)
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

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:remove-option?
  'remove-option?
  'gdk_pixbuf_remove_option
  '((name . "remove_option")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GdkPixbuf
       '<GdkPixbufRotation>
       "GdkPixbufRotation"
       "gdk_pixbuf_rotation_get_type")

(^enum-conv 'pixbuf-rotation 'GdkPixbufRotation)

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:rotate-simple
  'rotate-simple
  'gdk_pixbuf_rotate_simple
  '((name . "rotate_simple")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "angle")
            (meta (arg-type . enum)
                  (gtype-name . "GdkPixbufRotation")
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:saturate-and-pixelate
  'saturate-and-pixelate
  'gdk_pixbuf_saturate_and_pixelate
  '((name . "saturate_and_pixelate")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "dest")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "saturation")
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
           ((name . "pixelate")
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:save-to-bufferv
  'save-to-bufferv
  'gdk_pixbuf_save_to_bufferv
  '((name . "save_to_bufferv")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-input-opt . 2)
    (s-output-len . 1)
    (c-input-len . 3)
    (c-output-len . 2)
    (pdata ((name . "buffer")
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
           ((name . "buffer_size")
            (meta (arg-type . uint64)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . array_size)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1))
           ((name . "type")
            (meta (arg-type . utf8-string)
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "option_keys")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "option_values")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:save-to-callbackv?
  'save-to-callbackv?
  'gdk_pixbuf_save_to_callbackv
  '((name . "save_to_callbackv")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 3)
    (s-input-opt . 2)
    (c-input-len . 5)
    (pdata ((name . "save_func")
            (meta (arg-type . callback)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing)
                  (callable-info
                    (name . "PixbufSaveFunc")
                    (s-input-req . 1)
                    (s-input-opt . 1)
                    (s-output-len . 1)
                    (c-input-len . 3)
                    (c-output-len . 1)
                    (pdata ((name . "buf")
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
                           ((name . "count")
                            (meta (arg-type . uint64)
                                  (flags in)
                                  (item-size . 8)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . array_size)
                            (presence . implicit)
                            (i . 1)
                            (c-input-pos . 1))
                           ((name . "error")
                            (meta (arg-type . GError)
                                  (flags ptr out)
                                  (item-size . 16)
                                  (transfer . everything))
                            (s-direction . output)
                            (tuple . singleton)
                            (presence . implicit)
                            (i . 2)
                            (c-output-pos . 0)
                            (s-output-pos . 0))
                           ((name . "data")
                            (meta (arg-type . pointer)
                                  (flags ptr in nullable)
                                  (transfer . nothing))
                            (s-direction . input)
                            (tuple . singleton)
                            (presence . optional)
                            (i . 3)
                            (c-input-pos . 2)
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
           ((name . "user_data")
            (meta (arg-type . pointer)
                  (flags ptr in nullable)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "type")
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
           ((name . "option_keys")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "option_values")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4)))
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
       '<GOutputStream>
       "GOutputStream"
       "g_output_stream_get_type")

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:save-to-streamv?
  'save-to-streamv?
  'gdk_pixbuf_save_to_streamv
  '((name . "save_to_streamv")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (s-input-opt . 3)
    (c-input-len . 5)
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
           ((name . "type")
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
           ((name . "option_keys")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "option_values")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
            (s-input-pos . 4)))
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:save-to-streamv-async
  'save-to-streamv-async
  'gdk_pixbuf_save_to_streamv_async
  '((name . "save_to_streamv_async")
    (is-method . #t)
    (s-input-req . 2)
    (s-input-opt . 5)
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
           ((name . "type")
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
           ((name . "option_keys")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "option_values")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:savev?
  'savev?
  'gdk_pixbuf_savev
  '((name . "savev")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (s-input-opt . 2)
    (c-input-len . 4)
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
            (s-input-pos . 0))
           ((name . "type")
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
           ((name . "option_keys")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "option_values")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:savev-utf8?
  'savev-utf8?
  'gdk_pixbuf_savev_utf8
  '((name . "savev_utf8")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 2)
    (s-input-opt . 2)
    (c-input-len . 4)
    (pdata ((name . "filename")
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
           ((name . "option_keys")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "option_values")
            (meta (arg-type . array)
                  (flags ptr in nullable zero-terminated)
                  (transfer . nothing)
                  (params
                    ((arg-type . utf8-string)
                     (flags ptr)
                     (item-size . 8)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
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
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:scale
  'scale
  'gdk_pixbuf_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 10)
    (c-input-len . 10)
    (pdata ((name . "dest")
            (meta (arg-type . GObject)
                  (gtype-name . "GdkPixbuf")
                  (flags ptr in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "dest_x")
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
           ((name . "dest_y")
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
           ((name . "dest_width")
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
           ((name . "dest_height")
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
           ((name . "offset_x")
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
           ((name . "offset_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 6)
            (c-input-pos . 6)
            (s-input-pos . 6))
           ((name . "scale_x")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 7)
            (c-input-pos . 7)
            (s-input-pos . 7))
           ((name . "scale_y")
            (meta (arg-type . double)
                  (flags in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 8)
            (c-input-pos . 8)
            (s-input-pos . 8))
           ((name . "interp_type")
            (meta (arg-type . enum)
                  (gtype-name . "GdkInterpType")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 9)
            (c-input-pos . 9)
            (s-input-pos . 9)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:scale-simple
  'scale-simple
  'gdk_pixbuf_scale_simple
  '((name . "scale_simple")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "dest_width")
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
           ((name . "dest_height")
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
           ((name . "interp_type")
            (meta (arg-type . enum)
                  (gtype-name . "GdkInterpType")
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
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbuf
  'pixbuf:set-option?
  'set-option?
  'gdk_pixbuf_set_option
  '((name . "set_option")
    (is-method . #t)
    (s-input-req . 2)
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
           ((name . "value")
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

(^property
  'GdkPixbuf
  'pixbuf:bits-per-sample
  'bits-per-sample
  'bits-per-sample)

(^property
  'GdkPixbuf
  'pixbuf:colorspace
  'colorspace
  'colorspace)

(^property
  'GdkPixbuf
  'pixbuf:has-alpha
  'has-alpha
  'has-alpha)

(^property
  'GdkPixbuf
  'pixbuf:height
  'height
  'height)

(^property
  'GdkPixbuf
  'pixbuf:n-channels
  'n-channels
  'n-channels)

(^property
  'GdkPixbuf
  'pixbuf:pixel-bytes
  'pixel-bytes
  'pixel-bytes)

(^property
  'GdkPixbuf
  'pixbuf:pixels
  'pixels
  'pixels)

(^property
  'GdkPixbuf
  'pixbuf:rowstride
  'rowstride
  'rowstride)

(^property
  'GdkPixbuf
  'pixbuf:width
  'width
  'width)

(^type 'GdkPixbuf
       '<GdkPixbufAnimation>
       "GdkPixbufAnimation"
       "gdk_pixbuf_animation_get_type")

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:new-from-file
  'new-from-file
  'gdk_pixbuf_animation_new_from_file
  '((name . "new_from_file")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:new-from-file-utf8
  'new-from-file-utf8
  'gdk_pixbuf_animation_new_from_file_utf8
  '((name . "new_from_file_utf8")
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:new-from-resource
  'new-from-resource
  'gdk_pixbuf_animation_new_from_resource
  '((name . "new_from_resource")
    (can-throw-gerror . #t)
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
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:new-from-stream
  'new-from-stream
  'gdk_pixbuf_animation_new_from_stream
  '((name . "new_from_stream")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 2)
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
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:new-from-stream-finish
  'new-from-stream-finish
  'gdk_pixbuf_animation_new_from_stream_finish
  '((name . "new_from_stream_finish")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "async_result")
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
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out nullable)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:new-from-stream-async
  'new-from-stream-async
  'gdk_pixbuf_animation_new_from_stream_async
  '((name . "new_from_stream_async")
    (s-input-req . 1)
    (s-input-opt . 3)
    (c-input-len . 4)
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
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:get-height
  'get-height
  'gdk_pixbuf_animation_get_height
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

(^type 'GdkPixbuf
       '<GdkPixbufAnimationIter>
       "GdkPixbufAnimationIter"
       "gdk_pixbuf_animation_iter_get_type")

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:get-static-image
  'get-static-image
  'gdk_pixbuf_animation_get_static_image
  '((name . "get_static_image")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:get-width
  'get-width
  'gdk_pixbuf_animation_get_width
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
  'GdkPixbuf
  'GdkPixbufAnimation
  'pixbuf-animation:is-static-image?
  'is-static-image?
  'gdk_pixbuf_animation_is_static_image
  '((name . "is_static_image")
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
  'GdkPixbuf
  'GdkPixbufAnimationIter
  'pixbuf-animation-iter:get-delay-time
  'get-delay-time
  'gdk_pixbuf_animation_iter_get_delay_time
  '((name . "get_delay_time")
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
  'GdkPixbuf
  'GdkPixbufAnimationIter
  'pixbuf-animation-iter:get-pixbuf
  'get-pixbuf
  'gdk_pixbuf_animation_iter_get_pixbuf
  '((name . "get_pixbuf")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufAnimationIter
  'pixbuf-animation-iter:on-currently-loading-frame?
  'on-currently-loading-frame?
  'gdk_pixbuf_animation_iter_on_currently_loading_frame
  '((name . "on_currently_loading_frame")
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

(^type 'GdkPixbuf
       '<GdkPixbufError>
       "GdkPixbufError"
       "gdk_pixbuf_error_get_type")

(^enum-conv 'pixbuf-error 'GdkPixbufError)

(^function
  'GdkPixbuf
  'GdkPixbufError
  'pixbuf-error:quark
  'quark
  'gdk_pixbuf_error_quark
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:copy
  'copy
  'gdk_pixbuf_format_copy
  '((name . "copy")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPixbufFormat")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:free
  'free
  'gdk_pixbuf_format_free
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:get-description
  'get-description
  'gdk_pixbuf_format_get_description
  '((name . "get_description")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:get-extensions
  'get-extensions
  'gdk_pixbuf_format_get_extensions
  '((name . "get_extensions")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out zero-terminated)
            (transfer . everything)
            (params
              ((arg-type . utf8-string)
               (flags ptr)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:get-license
  'get-license
  'gdk_pixbuf_format_get_license
  '((name . "get_license")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:get-mime-types
  'get-mime-types
  'gdk_pixbuf_format_get_mime_types
  '((name . "get_mime_types")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . array)
            (flags ptr out zero-terminated)
            (transfer . everything)
            (params
              ((arg-type . utf8-string)
               (flags ptr)
               (item-size . 8)
               (transfer . nothing))))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:get-name
  'get-name
  'gdk_pixbuf_format_get_name
  '((name . "get_name")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:is-disabled?
  'is-disabled?
  'gdk_pixbuf_format_is_disabled
  '((name . "is_disabled")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:is-save-option-supported?
  'is-save-option-supported?
  'gdk_pixbuf_format_is_save_option_supported
  '((name . "is_save_option_supported")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "option_key")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:is-scalable?
  'is-scalable?
  'gdk_pixbuf_format_is_scalable
  '((name . "is_scalable")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:is-writable?
  'is-writable?
  'gdk_pixbuf_format_is_writable
  '((name . "is_writable")
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
  'GdkPixbuf
  'GdkPixbufFormat
  'pixbuf-format:set-disabled
  'set-disabled
  'gdk_pixbuf_format_set_disabled
  '((name . "set_disabled")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "disabled")
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

(^untyped-flags
  '<%GdkPixbufFormatFlags>
  '%GdkPixbufFormatFlags
  '((writable . 1) (scalable . 2) (threadsafe . 4)))

(^untyped-flags-conv
  'pixbuf-format-flags
  '%GdkPixbufFormatFlags)

(^type 'GdkPixbuf
       '<GdkPixbufLoader>
       "GdkPixbufLoader"
       "gdk_pixbuf_loader_get_type")

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:new
  'new
  'gdk_pixbuf_loader_new
  '((name . "new")
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbufLoader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:new-with-mime-type
  'new-with-mime-type
  'gdk_pixbuf_loader_new_with_mime_type
  '((name . "new_with_mime_type")
    (can-throw-gerror . #t)
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
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbufLoader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:new-with-type
  'new-with-type
  'gdk_pixbuf_loader_new_with_type
  '((name . "new_with_type")
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "image_type")
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
            (gtype-name . "GdkPixbufLoader")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:close?
  'close?
  'gdk_pixbuf_loader_close
  '((name . "close")
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
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:get-animation
  'get-animation
  'gdk_pixbuf_loader_get_animation
  '((name . "get_animation")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:get-format
  'get-format
  'gdk_pixbuf_loader_get_format
  '((name . "get_format")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GdkPixbufFormat")
            (flags ptr out nullable)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:get-pixbuf
  'get-pixbuf
  'gdk_pixbuf_loader_get_pixbuf
  '((name . "get_pixbuf")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GObject)
            (gtype-name . "GdkPixbuf")
            (flags ptr out nullable)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:set-size
  'set-size
  'gdk_pixbuf_loader_set_size
  '((name . "set_size")
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

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:write?
  'write?
  'gdk_pixbuf_loader_write
  '((name . "write")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "buf")
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
           ((name . "count")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:write-bytes?
  'write-bytes?
  'gdk_pixbuf_loader_write_bytes
  '((name . "write_bytes")
    (is-method . #t)
    (can-throw-gerror . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "buffer")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:area-prepared
  'area-prepared
  'area-prepared
  '((name . "area-prepared")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:area-updated
  'area-updated
  'area-updated
  '((name . "area-updated")
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

(^signal
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:closed
  'closed
  'closed
  '((name . "closed")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^signal
  'GdkPixbuf
  'GdkPixbufLoader
  'pixbuf-loader:size-prepared
  'size-prepared
  'size-prepared
  '((name . "size-prepared")
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

(^type 'GdkPixbuf
       '<GdkPixbufNonAnim>
       "GdkPixbufNonAnim"
       "gdk_pixbuf_non_anim_get_type")

(^function
  'GdkPixbuf
  'GdkPixbufNonAnim
  'pixbuf-non-anim:new
  'new
  'gdk_pixbuf_non_anim_new
  '((name . "new")
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
            (gtype-name . "GdkPixbufAnimation")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^type 'GdkPixbuf
       '<GdkPixbufSimpleAnim>
       "GdkPixbufSimpleAnim"
       "gdk_pixbuf_simple_anim_get_type")

(^function
  'GdkPixbuf
  'GdkPixbufSimpleAnim
  'pixbuf-simple-anim:new
  'new
  'gdk_pixbuf_simple_anim_new
  '((name . "new")
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
           ((name . "rate")
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
            (gtype-name . "GdkPixbufSimpleAnim")
            (flags ptr out)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufSimpleAnim
  'pixbuf-simple-anim:add-frame
  'add-frame
  'gdk_pixbuf_simple_anim_add_frame
  '((name . "add_frame")
    (is-method . #t)
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'GdkPixbuf
  'GdkPixbufSimpleAnim
  'pixbuf-simple-anim:get-loop?
  'get-loop?
  'gdk_pixbuf_simple_anim_get_loop
  '((name . "get_loop")
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
  'GdkPixbuf
  'GdkPixbufSimpleAnim
  'pixbuf-simple-anim:set-loop
  'set-loop
  'gdk_pixbuf_simple_anim_set_loop
  '((name . "set_loop")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "loop")
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

(^property
  'GdkPixbufSimpleAnim
  'pixbuf-simple-anim:loop
  'loop
  'loop)

(^type 'GdkPixbuf
       '<GdkPixbufSimpleAnimIter>
       "GdkPixbufSimpleAnimIter"
       "gdk_pixbuf_simple_anim_iter_get_type")

(^function
  'GdkPixbuf
  #f
  'gdk-pixbuf:pixbuf-error-quark
  'pixbuf-error-quark
  'gdk_pixbuf_error_quark
  '((name . "pixbuf_error_quark")
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
