(define-module (gi Graphene-1-0)
  #:use-module (gi runtime)
  #:use-module (gi GObject-2-0)
  #:export(<GrapheneBox> alloc box:alloc contains-box? box:contains-box? 
           <GraphenePoint3D> contains-point? box:contains-point? equal? 
           box:equal? expand! box:expand! expand-scalar! box:expand-scalar! 
           <GrapheneVec3> expand-vec3! box:expand-vec3! free box:free 
           <GrapheneSphere> get-bounding-sphere! box:get-bounding-sphere! 
           get-center! box:get-center! get-depth box:get-depth get-height 
           box:get-height get-max! box:get-max! get-min! box:get-min! get-size! 
           box:get-size! get-vertices! box:get-vertices! get-width 
           box:get-width init box:init init-from-box box:init-from-box 
           init-from-points box:init-from-points init-from-vec3 
           box:init-from-vec3 init-from-vectors box:init-from-vectors 
           intersection! box:intersection! union! box:union! empty box:empty 
           infinite box:infinite minus-one box:minus-one one box:one 
           one-minus-one box:one-minus-one zero box:zero <GrapheneEuler> alloc 
           euler:alloc equal? euler:equal? free euler:free get-alpha 
           euler:get-alpha get-beta euler:get-beta get-gamma euler:get-gamma 
           get-order euler:get-order get-x euler:get-x get-y euler:get-y get-z 
           euler:get-z init euler:init init-from-euler euler:init-from-euler 
           <GrapheneMatrix> init-from-matrix euler:init-from-matrix 
           <GrapheneQuaternion> init-from-quaternion euler:init-from-quaternion 
           init-from-radians euler:init-from-radians init-from-vec3 
           euler:init-from-vec3 init-with-order euler:init-with-order reorder! 
           euler:reorder! to-matrix! euler:to-matrix! to-quaternion! 
           euler:to-quaternion! to-vec3! euler:to-vec3! <%GrapheneEulerOrder> 
           symbol->euler-order number->euler-order euler-order->symbol 
           euler-order->number <GrapheneFrustum> alloc frustum:alloc 
           contains-point? frustum:contains-point? equal? frustum:equal? free 
           frustum:free <GraphenePlane> get-planes! frustum:get-planes! init 
           frustum:init init-from-frustum frustum:init-from-frustum 
           init-from-matrix frustum:init-from-matrix intersects-box? 
           frustum:intersects-box? intersects-sphere? 
           frustum:intersects-sphere? alloc matrix:alloc <GrapheneVec4> 
           decompose! matrix:decompose! determinant matrix:determinant equal? 
           matrix:equal? equal-fast? matrix:equal-fast? free matrix:free 
           get-row! matrix:get-row! get-value matrix:get-value get-x-scale 
           matrix:get-x-scale get-x-translation matrix:get-x-translation 
           get-y-scale matrix:get-y-scale get-y-translation 
           matrix:get-y-translation get-z-scale matrix:get-z-scale 
           get-z-translation matrix:get-z-translation init-from-2d 
           matrix:init-from-2d init-from-float matrix:init-from-float 
           init-from-matrix matrix:init-from-matrix init-from-vec4 
           matrix:init-from-vec4 init-frustum matrix:init-frustum init-identity 
           matrix:init-identity init-look-at matrix:init-look-at init-ortho 
           matrix:init-ortho init-perspective matrix:init-perspective 
           init-rotate matrix:init-rotate init-scale matrix:init-scale 
           init-skew matrix:init-skew init-translate matrix:init-translate 
           interpolate! matrix:interpolate! inverse! matrix:inverse! is-2d? 
           matrix:is-2d? is-backface-visible? matrix:is-backface-visible? 
           is-identity? matrix:is-identity? is-singular? matrix:is-singular? 
           multiply! matrix:multiply! near? matrix:near? normalize! 
           matrix:normalize! perspective! matrix:perspective! print 
           matrix:print <GraphenePoint> project-point! matrix:project-point! 
           <GrapheneRect> <GrapheneQuad> project-rect! matrix:project-rect! 
           project-rect-bounds! matrix:project-rect-bounds! rotate 
           matrix:rotate rotate-euler matrix:rotate-euler rotate-quaternion 
           matrix:rotate-quaternion rotate-x matrix:rotate-x rotate-y 
           matrix:rotate-y rotate-z matrix:rotate-z scale matrix:scale skew-xy 
           matrix:skew-xy skew-xz matrix:skew-xz skew-yz matrix:skew-yz to-2d 
           matrix:to-2d to-float! matrix:to-float! transform-bounds! 
           matrix:transform-bounds! transform-box! matrix:transform-box! 
           transform-point! matrix:transform-point! transform-point3d! 
           matrix:transform-point3d! <GrapheneRay> transform-ray! 
           matrix:transform-ray! transform-rect! matrix:transform-rect! 
           transform-sphere! matrix:transform-sphere! transform-vec3! 
           matrix:transform-vec3! transform-vec4! matrix:transform-vec4! 
           translate matrix:translate transpose! matrix:transpose! 
           unproject-point3d! matrix:unproject-point3d! untransform-bounds! 
           matrix:untransform-bounds! untransform-point! 
           matrix:untransform-point! PI PI_2 alloc plane:alloc distance 
           plane:distance equal? plane:equal? free plane:free get-constant 
           plane:get-constant get-normal! plane:get-normal! init plane:init 
           init-from-plane plane:init-from-plane init-from-point 
           plane:init-from-point init-from-points plane:init-from-points 
           init-from-vec4 plane:init-from-vec4 negate! plane:negate! normalize! 
           plane:normalize! transform! plane:transform! alloc point:alloc 
           distance point:distance equal? point:equal? free point:free init 
           point:init init-from-point point:init-from-point <GrapheneVec2> 
           init-from-vec2 point:init-from-vec2 interpolate! point:interpolate! 
           near? point:near? to-vec2! point:to-vec2! zero point:zero alloc 
           point3d:alloc cross! point3d:cross! distance! point3d:distance! dot 
           point3d:dot equal? point3d:equal? free point3d:free init 
           point3d:init init-from-point point3d:init-from-point init-from-vec3 
           point3d:init-from-vec3 interpolate! point3d:interpolate! length 
           point3d:length near? point3d:near? normalize! point3d:normalize! 
           normalize-viewport! point3d:normalize-viewport! scale! 
           point3d:scale! to-vec3! point3d:to-vec3! zero point3d:zero alloc 
           quad:alloc bounds! quad:bounds! contains? quad:contains? free 
           quad:free get-point quad:get-point init quad:init init-from-points 
           quad:init-from-points init-from-rect quad:init-from-rect alloc 
           quaternion:alloc add! quaternion:add! dot quaternion:dot equal? 
           quaternion:equal? free quaternion:free init quaternion:init 
           init-from-angle-vec3 quaternion:init-from-angle-vec3 
           init-from-angles quaternion:init-from-angles init-from-euler 
           quaternion:init-from-euler init-from-matrix 
           quaternion:init-from-matrix init-from-quaternion 
           quaternion:init-from-quaternion init-from-radians 
           quaternion:init-from-radians init-from-vec4 
           quaternion:init-from-vec4 init-identity quaternion:init-identity 
           invert! quaternion:invert! multiply! quaternion:multiply! normalize! 
           quaternion:normalize! scale! quaternion:scale! slerp! 
           quaternion:slerp! to-angle-vec3! quaternion:to-angle-vec3! to-angles 
           quaternion:to-angles to-matrix! quaternion:to-matrix! to-radians 
           quaternion:to-radians to-vec4! quaternion:to-vec4! alloc ray:alloc 
           equal? ray:equal? free ray:free get-closest-point-to-point! 
           ray:get-closest-point-to-point! get-direction! ray:get-direction! 
           get-distance-to-plane ray:get-distance-to-plane 
           get-distance-to-point ray:get-distance-to-point get-origin! 
           ray:get-origin! get-position-at! ray:get-position-at! init ray:init 
           init-from-ray ray:init-from-ray init-from-vec3 ray:init-from-vec3 
           intersect-box ray:intersect-box intersect-sphere 
           ray:intersect-sphere <GrapheneTriangle> intersect-triangle 
           ray:intersect-triangle intersects-box? ray:intersects-box? 
           intersects-sphere? ray:intersects-sphere? intersects-triangle? 
           ray:intersects-triangle? <%GrapheneRayIntersectionKind> 
           symbol->ray-intersection-kind number->ray-intersection-kind 
           ray-intersection-kind->symbol ray-intersection-kind->number 
           contains-point? rect:contains-point? contains-rect? 
           rect:contains-rect? equal? rect:equal? expand! rect:expand! free 
           rect:free get-area rect:get-area get-bottom-left! 
           rect:get-bottom-left! get-bottom-right! rect:get-bottom-right! 
           get-center! rect:get-center! get-height rect:get-height 
           get-top-left! rect:get-top-left! get-top-right! rect:get-top-right! 
           get-vertices! rect:get-vertices! get-width rect:get-width get-x 
           rect:get-x get-y rect:get-y init rect:init init-from-rect 
           rect:init-from-rect inset rect:inset inset-r! rect:inset-r! 
           interpolate! rect:interpolate! intersection! rect:intersection! 
           normalize rect:normalize normalize-r! rect:normalize-r! offset 
           rect:offset offset-r! rect:offset-r! round! rect:round! 
           round-extents! rect:round-extents! round-to-pixel 
           rect:round-to-pixel scale! rect:scale! union! rect:union! alloc 
           rect:alloc zero rect:zero <GrapheneSize> alloc size:alloc equal? 
           size:equal? free size:free init size:init init-from-size 
           size:init-from-size interpolate! size:interpolate! scale! 
           size:scale! zero size:zero alloc sphere:alloc contains-point? 
           sphere:contains-point? distance sphere:distance equal? sphere:equal? 
           free sphere:free get-bounding-box! sphere:get-bounding-box! 
           get-center! sphere:get-center! get-radius sphere:get-radius init 
           sphere:init init-from-points sphere:init-from-points 
           init-from-vectors sphere:init-from-vectors is-empty? 
           sphere:is-empty? translate! sphere:translate! alloc triangle:alloc 
           contains-point? triangle:contains-point? equal? triangle:equal? free 
           triangle:free get-area triangle:get-area get-barycoords! 
           triangle:get-barycoords! get-bounding-box! 
           triangle:get-bounding-box! get-midpoint! triangle:get-midpoint! 
           get-normal! triangle:get-normal! get-plane! triangle:get-plane! 
           get-points! triangle:get-points! get-uv! triangle:get-uv! 
           get-vertices! triangle:get-vertices! init-from-float 
           triangle:init-from-float init-from-point3d 
           triangle:init-from-point3d init-from-vec3 triangle:init-from-vec3 
           VEC2_LEN VEC3_LEN VEC4_LEN alloc vec2:alloc add! vec2:add! divide! 
           vec2:divide! dot vec2:dot equal? vec2:equal? free vec2:free get-x 
           vec2:get-x get-y vec2:get-y init vec2:init init-from-float 
           vec2:init-from-float init-from-vec2 vec2:init-from-vec2 interpolate! 
           vec2:interpolate! length vec2:length max! vec2:max! min! vec2:min! 
           multiply! vec2:multiply! near? vec2:near? negate! vec2:negate! 
           normalize! vec2:normalize! scale! vec2:scale! subtract! 
           vec2:subtract! to-float! vec2:to-float! one vec2:one x-axis 
           vec2:x-axis y-axis vec2:y-axis zero vec2:zero alloc vec3:alloc add! 
           vec3:add! cross! vec3:cross! divide! vec3:divide! dot vec3:dot 
           equal? vec3:equal? free vec3:free get-x vec3:get-x get-xy! 
           vec3:get-xy! get-xy0! vec3:get-xy0! get-xyz0! vec3:get-xyz0! 
           get-xyz1! vec3:get-xyz1! get-xyzw! vec3:get-xyzw! get-y vec3:get-y 
           get-z vec3:get-z init vec3:init init-from-float vec3:init-from-float 
           init-from-vec3 vec3:init-from-vec3 interpolate! vec3:interpolate! 
           length vec3:length max! vec3:max! min! vec3:min! multiply! 
           vec3:multiply! near? vec3:near? negate! vec3:negate! normalize! 
           vec3:normalize! scale! vec3:scale! subtract! vec3:subtract! 
           to-float! vec3:to-float! one vec3:one x-axis vec3:x-axis y-axis 
           vec3:y-axis z-axis vec3:z-axis zero vec3:zero alloc vec4:alloc add! 
           vec4:add! divide! vec4:divide! dot vec4:dot equal? vec4:equal? free 
           vec4:free get-w vec4:get-w get-x vec4:get-x get-xy! vec4:get-xy! 
           get-xyz! vec4:get-xyz! get-y vec4:get-y get-z vec4:get-z init 
           vec4:init init-from-float vec4:init-from-float init-from-vec2 
           vec4:init-from-vec2 init-from-vec3 vec4:init-from-vec3 
           init-from-vec4 vec4:init-from-vec4 interpolate! vec4:interpolate! 
           length vec4:length max! vec4:max! min! vec4:min! multiply! 
           vec4:multiply! near? vec4:near? negate! vec4:negate! normalize! 
           vec4:normalize! scale! vec4:scale! subtract! vec4:subtract! 
           to-float! vec4:to-float! one vec4:one w-axis vec4:w-axis x-axis 
           vec4:x-axis y-axis vec4:y-axis z-axis vec4:z-axis zero vec4:zero 
           box-empty graphene:box-empty box-infinite graphene:box-infinite 
           box-minus-one graphene:box-minus-one box-one graphene:box-one 
           box-one-minus-one graphene:box-one-minus-one box-zero 
           graphene:box-zero point3d-zero graphene:point3d-zero point-zero 
           graphene:point-zero rect-alloc graphene:rect-alloc rect-zero 
           graphene:rect-zero size-zero graphene:size-zero vec2-one 
           graphene:vec2-one vec2-x-axis graphene:vec2-x-axis vec2-y-axis 
           graphene:vec2-y-axis vec2-zero graphene:vec2-zero vec3-one 
           graphene:vec3-one vec3-x-axis graphene:vec3-x-axis vec3-y-axis 
           graphene:vec3-y-axis vec3-z-axis graphene:vec3-z-axis vec3-zero 
           graphene:vec3-zero vec4-one graphene:vec4-one vec4-w-axis 
           graphene:vec4-w-axis vec4-x-axis graphene:vec4-x-axis vec4-y-axis 
           graphene:vec4-y-axis vec4-z-axis graphene:vec4-z-axis vec4-zero 
           graphene:vec4-zero))

(eval-when (expand load eval)
(^library
  "Graphene"
  "1.0"
  '("libgraphene-1.0-0.dll"))

(^initialize)

(^sized-type
  'Graphene
  '<GrapheneBox>
  "GrapheneBox"
  "graphene_box_get_type"
  32)

(^function
  'Graphene
  'GrapheneBox
  'box:alloc
  'alloc
  'graphene_box_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:contains-box?
  'contains-box?
  'graphene_box_contains_box
  '((name . "contains_box")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
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

(^sized-type
  'Graphene
  '<GraphenePoint3D>
  "GraphenePoint3D"
  "graphene_point3d_get_type"
  12)

(^function
  'Graphene
  'GrapheneBox
  'box:contains-point?
  'contains-point?
  'graphene_box_contains_point
  '((name . "contains_point")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:equal?
  'equal?
  'graphene_box_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
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
  'Graphene
  'GrapheneBox
  'box:expand!
  'expand!
  'graphene_box_expand
  '((name . "expand")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneBox
  'box:expand-scalar!
  'expand-scalar!
  'graphene_box_expand_scalar
  '((name . "expand_scalar")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "scalar")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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

(^sized-type
  'Graphene
  '<GrapheneVec3>
  "GrapheneVec3"
  "graphene_vec3_get_type"
  16)

(^function
  'Graphene
  'GrapheneBox
  'box:expand-vec3!
  'expand-vec3!
  'graphene_box_expand_vec3
  '((name . "expand_vec3")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "vec")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneBox
  'box:free
  'free
  'graphene_box_free
  '((name . "free")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneSphere>
  "GrapheneSphere"
  "graphene_sphere_get_type"
  20)

(^function
  'Graphene
  'GrapheneBox
  'box:get-bounding-sphere!
  'get-bounding-sphere!
  'graphene_box_get_bounding_sphere
  '((name . "get_bounding_sphere")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "sphere")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GrapheneBox
  'box:get-center!
  'get-center!
  'graphene_box_get_center
  '((name . "get_center")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "center")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneBox
  'box:get-depth
  'get-depth
  'graphene_box_get_depth
  '((name . "get_depth")
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
  'Graphene
  'GrapheneBox
  'box:get-height
  'get-height
  'graphene_box_get_height
  '((name . "get_height")
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
  'Graphene
  'GrapheneBox
  'box:get-max!
  'get-max!
  'graphene_box_get_max
  '((name . "get_max")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "max")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneBox
  'box:get-min!
  'get-min!
  'graphene_box_get_min
  '((name . "get_min")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "min")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneBox
  'box:get-size!
  'get-size!
  'graphene_box_get_size
  '((name . "get_size")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "size")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneBox
  'box:get-vertices!
  'get-vertices!
  'graphene_box_get_vertices
  '((name . "get_vertices")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "vertices")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 8)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GrapheneVec3")
                     (item-size . 16)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneBox
  'box:get-width
  'get-width
  'graphene_box_get_width
  '((name . "get_width")
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
  'Graphene
  'GrapheneBox
  'box:init
  'init
  'graphene_box_init
  '((name . "init")
    (is-method . #t)
    (s-input-opt . 2)
    (c-input-len . 2)
    (pdata ((name . "min")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "max")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
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
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:init-from-box
  'init-from-box
  'graphene_box_init_from_box
  '((name . "init_from_box")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
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
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:init-from-points
  'init-from-points
  'graphene_box_init_from_points
  '((name . "init_from_points")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "n_points")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-input-pos . 0))
           ((name . "points")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GraphenePoint3D")
                     (item-size . 12)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:init-from-vec3
  'init-from-vec3
  'graphene_box_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-opt . 2)
    (c-input-len . 2)
    (pdata ((name . "min")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "max")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:init-from-vectors
  'init-from-vectors
  'graphene_box_init_from_vectors
  '((name . "init_from_vectors")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 2)
    (pdata ((name . "n_vectors")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-input-pos . 0))
           ((name . "vectors")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GrapheneVec3")
                     (item-size . 16)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:intersection!
  'intersection!
  'graphene_box_intersection
  '((name . "intersection")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated optional)
                  (item-size . 32)
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
  'Graphene
  'GrapheneBox
  'box:union!
  'union!
  'graphene_box_union
  '((name . "union")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneBox
  'box:empty
  'empty
  'graphene_box_empty
  '((name . "empty")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:infinite
  'infinite
  'graphene_box_infinite
  '((name . "infinite")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:minus-one
  'minus-one
  'graphene_box_minus_one
  '((name . "minus_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:one
  'one
  'graphene_box_one
  '((name . "one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:one-minus-one
  'one-minus-one
  'graphene_box_one_minus_one
  '((name . "one_minus_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneBox
  'box:zero
  'zero
  'graphene_box_zero
  '((name . "zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneEuler>
  "GrapheneEuler"
  "graphene_euler_get_type"
  20)

(^function
  'Graphene
  'GrapheneEuler
  'euler:alloc
  'alloc
  'graphene_euler_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:equal?
  'equal?
  'graphene_euler_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneEuler")
                  (flags ptr in)
                  (item-size . 20)
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
  'Graphene
  'GrapheneEuler
  'euler:free
  'free
  'graphene_euler_free
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
  'Graphene
  'GrapheneEuler
  'euler:get-alpha
  'get-alpha
  'graphene_euler_get_alpha
  '((name . "get_alpha")
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
  'Graphene
  'GrapheneEuler
  'euler:get-beta
  'get-beta
  'graphene_euler_get_beta
  '((name . "get_beta")
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
  'Graphene
  'GrapheneEuler
  'euler:get-gamma
  'get-gamma
  'graphene_euler_get_gamma
  '((name . "get_gamma")
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
  'Graphene
  'GrapheneEuler
  'euler:get-order
  'get-order
  'graphene_euler_get_order
  '((name . "get_order")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (qualified-name . "%GrapheneEulerOrder")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:get-x
  'get-x
  'graphene_euler_get_x
  '((name . "get_x")
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
  'Graphene
  'GrapheneEuler
  'euler:get-y
  'get-y
  'graphene_euler_get_y
  '((name . "get_y")
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
  'Graphene
  'GrapheneEuler
  'euler:get-z
  'get-z
  'graphene_euler_get_z
  '((name . "get_z")
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
  'Graphene
  'GrapheneEuler
  'euler:init
  'init
  'graphene_euler_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
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
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:init-from-euler
  'init-from-euler
  'graphene_euler_init_from_euler
  '((name . "init_from_euler")
    (is-method . #t)
    (s-input-opt . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneEuler")
                  (flags ptr in nullable)
                  (item-size . 20)
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
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneMatrix>
  "GrapheneMatrix"
  "graphene_matrix_get_type"
  64)

(^function
  'Graphene
  'GrapheneEuler
  'euler:init-from-matrix
  'init-from-matrix
  'graphene_euler_init_from_matrix
  '((name . "init_from_matrix")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "m")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "order")
            (meta (arg-type . enum)
                  (qualified-name . "%GrapheneEulerOrder")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneQuaternion>
  "GrapheneQuaternion"
  "graphene_quaternion_get_type"
  16)

(^function
  'Graphene
  'GrapheneEuler
  'euler:init-from-quaternion
  'init-from-quaternion
  'graphene_euler_init_from_quaternion
  '((name . "init_from_quaternion")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "q")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "order")
            (meta (arg-type . enum)
                  (qualified-name . "%GrapheneEulerOrder")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:init-from-radians
  'init-from-radians
  'graphene_euler_init_from_radians
  '((name . "init_from_radians")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "order")
            (meta (arg-type . enum)
                  (qualified-name . "%GrapheneEulerOrder")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:init-from-vec3
  'init-from-vec3
  'graphene_euler_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "v")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "order")
            (meta (arg-type . enum)
                  (qualified-name . "%GrapheneEulerOrder")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:init-with-order
  'init-with-order
  'graphene_euler_init_with_order
  '((name . "init_with_order")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "order")
            (meta (arg-type . enum)
                  (qualified-name . "%GrapheneEulerOrder")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneEuler")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneEuler
  'euler:reorder!
  'reorder!
  'graphene_euler_reorder
  '((name . "reorder")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "order")
            (meta (arg-type . enum)
                  (qualified-name . "%GrapheneEulerOrder")
                  (flags in)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneEuler")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GrapheneEuler
  'euler:to-matrix!
  'to-matrix!
  'graphene_euler_to_matrix
  '((name . "to_matrix")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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
  'Graphene
  'GrapheneEuler
  'euler:to-quaternion!
  'to-quaternion!
  'graphene_euler_to_quaternion
  '((name . "to_quaternion")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneEuler
  'euler:to-vec3!
  'to-vec3!
  'graphene_euler_to_vec3
  '((name . "to_vec3")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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

(^untyped-enum
  '<%GrapheneEulerOrder>
  '%GrapheneEulerOrder
  '((default . -1)
    (xyz . 0)
    (yzx . 1)
    (zxy . 2)
    (xzy . 3)
    (yxz . 4)
    (zyx . 5)
    (sxyz . 6)
    (sxyx . 7)
    (sxzy . 8)
    (sxzx . 9)
    (syzx . 10)
    (syzy . 11)
    (syxz . 12)
    (syxy . 13)
    (szxy . 14)
    (szxz . 15)
    (szyx . 16)
    (szyz . 17)
    (rzyx . 18)
    (rxyx . 19)
    (ryzx . 20)
    (rxzx . 21)
    (rxzy . 22)
    (ryzy . 23)
    (rzxy . 24)
    (ryxy . 25)
    (ryxz . 26)
    (rzxz . 27)
    (rxyz . 28)
    (rzyz . 29)))

(^untyped-enum-conv
  'euler-order
  '%GrapheneEulerOrder)

(^sized-type
  'Graphene
  '<GrapheneFrustum>
  "GrapheneFrustum"
  "graphene_frustum_get_type"
  120)

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:alloc
  'alloc
  'graphene_frustum_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneFrustum")
            (flags ptr out)
            (item-size . 120)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:contains-point?
  'contains-point?
  'graphene_frustum_contains_point
  '((name . "contains_point")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:equal?
  'equal?
  'graphene_frustum_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneFrustum")
                  (flags ptr in)
                  (item-size . 120)
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
  'Graphene
  'GrapheneFrustum
  'frustum:free
  'free
  'graphene_frustum_free
  '((name . "free")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GraphenePlane>
  "GraphenePlane"
  "graphene_plane_get_type"
  20)

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:get-planes!
  'get-planes!
  'graphene_frustum_get_planes
  '((name . "get_planes")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "planes")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 6)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GraphenePlane")
                     (item-size . 20)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneFrustum
  'frustum:init
  'init
  'graphene_frustum_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "p0")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "p1")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "p2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "p3")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "p4")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-input-pos . 4)
            (s-input-pos . 4))
           ((name . "p5")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 5)
            (c-input-pos . 5)
            (s-input-pos . 5)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneFrustum")
            (flags ptr out)
            (item-size . 120)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:init-from-frustum
  'init-from-frustum
  'graphene_frustum_init_from_frustum
  '((name . "init_from_frustum")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneFrustum")
                  (flags ptr in)
                  (item-size . 120)
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
            (gtype-name . "GrapheneFrustum")
            (flags ptr out)
            (item-size . 120)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:init-from-matrix
  'init-from-matrix
  'graphene_frustum_init_from_matrix
  '((name . "init_from_matrix")
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
            (gtype-name . "GrapheneFrustum")
            (flags ptr out)
            (item-size . 120)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneFrustum
  'frustum:intersects-box?
  'intersects-box?
  'graphene_frustum_intersects_box
  '((name . "intersects_box")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "box")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
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
  'Graphene
  'GrapheneFrustum
  'frustum:intersects-sphere?
  'intersects-sphere?
  'graphene_frustum_intersects_sphere
  '((name . "intersects_sphere")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "sphere")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags ptr in)
                  (item-size . 20)
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
  'Graphene
  'GrapheneMatrix
  'matrix:alloc
  'alloc
  'graphene_matrix_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneVec4>
  "GrapheneVec4"
  "graphene_vec4_get_type"
  16)

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:decompose!
  'decompose!
  'graphene_matrix_decompose
  '((name . "decompose")
    (is-method . #t)
    (s-input-req . 5)
    (s-output-len . 5)
    (c-output-len . 5)
    (pdata ((name . "translate")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "scale")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 1)
            (s-input-pos . 1)
            (s-output-pos . 1))
           ((name . "rotate")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 2)
            (s-input-pos . 2)
            (s-output-pos . 2))
           ((name . "shear")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-output-pos . 3)
            (s-input-pos . 3)
            (s-output-pos . 3))
           ((name . "perspective")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-output-pos . 4)
            (s-input-pos . 4)
            (s-output-pos . 4)))
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
  'Graphene
  'GrapheneMatrix
  'matrix:determinant
  'determinant
  'graphene_matrix_determinant
  '((name . "determinant")
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
  'Graphene
  'GrapheneMatrix
  'matrix:equal?
  'equal?
  'graphene_matrix_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:equal-fast?
  'equal-fast?
  'graphene_matrix_equal_fast
  '((name . "equal_fast")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:free
  'free
  'graphene_matrix_free
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-row!
  'get-row!
  'graphene_matrix_get_row
  '((name . "get_row")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-value
  'get-value
  'graphene_matrix_get_value
  '((name . "get_value")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "row")
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
           ((name . "col")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:get-x-scale
  'get-x-scale
  'graphene_matrix_get_x_scale
  '((name . "get_x_scale")
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-x-translation
  'get-x-translation
  'graphene_matrix_get_x_translation
  '((name . "get_x_translation")
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-y-scale
  'get-y-scale
  'graphene_matrix_get_y_scale
  '((name . "get_y_scale")
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-y-translation
  'get-y-translation
  'graphene_matrix_get_y_translation
  '((name . "get_y_translation")
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-z-scale
  'get-z-scale
  'graphene_matrix_get_z_scale
  '((name . "get_z_scale")
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
  'Graphene
  'GrapheneMatrix
  'matrix:get-z-translation
  'get-z-translation
  'graphene_matrix_get_z_translation
  '((name . "get_z_translation")
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-from-2d
  'init-from-2d
  'graphene_matrix_init_from_2d
  '((name . "init_from_2d")
    (is-method . #t)
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "xx")
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
           ((name . "yx")
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
           ((name . "xy")
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
           ((name . "yy")
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
           ((name . "x_0")
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
           ((name . "y_0")
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-from-float
  'init-from-float
  'graphene_matrix_init_from_float
  '((name . "init_from_float")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 16)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
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
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:init-from-matrix
  'init-from-matrix
  'graphene_matrix_init_from_matrix
  '((name . "init_from_matrix")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
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
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:init-from-vec4
  'init-from-vec4
  'graphene_matrix_init_from_vec4
  '((name . "init_from_vec4")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "v0")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "v1")
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
            (s-input-pos . 1))
           ((name . "v2")
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
            (s-input-pos . 2))
           ((name . "v3")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3)))
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-frustum
  'init-frustum
  'graphene_matrix_init_frustum
  '((name . "init_frustum")
    (is-method . #t)
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "left")
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
           ((name . "right")
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
           ((name . "bottom")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "top")
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
           ((name . "z_near")
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
           ((name . "z_far")
            (meta (arg-type . float)
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-identity
  'init-identity
  'graphene_matrix_init_identity
  '((name . "init_identity")
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-look-at
  'init-look-at
  'graphene_matrix_init_look_at
  '((name . "init_look_at")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "eye")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "center")
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
            (s-input-pos . 1))
           ((name . "up")
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-ortho
  'init-ortho
  'graphene_matrix_init_ortho
  '((name . "init_ortho")
    (is-method . #t)
    (s-input-req . 6)
    (c-input-len . 6)
    (pdata ((name . "left")
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
           ((name . "right")
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
           ((name . "top")
            (meta (arg-type . float)
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
           ((name . "z_near")
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
           ((name . "z_far")
            (meta (arg-type . float)
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-perspective
  'init-perspective
  'graphene_matrix_init_perspective
  '((name . "init_perspective")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "fovy")
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
           ((name . "aspect")
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
           ((name . "z_near")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "z_far")
            (meta (arg-type . float)
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
  'Graphene
  'GrapheneMatrix
  'matrix:init-rotate
  'init-rotate
  'graphene_matrix_init_rotate
  '((name . "init_rotate")
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
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:init-scale
  'init-scale
  'graphene_matrix_init_scale
  '((name . "init_scale")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
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
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:init-skew
  'init-skew
  'graphene_matrix_init_skew
  '((name . "init_skew")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "x_skew")
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
           ((name . "y_skew")
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
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:init-translate
  'init-translate
  'graphene_matrix_init_translate
  '((name . "init_translate")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p")
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
            (gtype-name . "GrapheneMatrix")
            (flags ptr out)
            (item-size . 64)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:interpolate!
  'interpolate!
  'graphene_matrix_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags out preallocated)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:inverse!
  'inverse!
  'graphene_matrix_inverse
  '((name . "inverse")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:is-2d?
  'is-2d?
  'graphene_matrix_is_2d
  '((name . "is_2d")
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
  'Graphene
  'GrapheneMatrix
  'matrix:is-backface-visible?
  'is-backface-visible?
  'graphene_matrix_is_backface_visible
  '((name . "is_backface_visible")
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
  'Graphene
  'GrapheneMatrix
  'matrix:is-identity?
  'is-identity?
  'graphene_matrix_is_identity
  '((name . "is_identity")
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
  'Graphene
  'GrapheneMatrix
  'matrix:is-singular?
  'is-singular?
  'graphene_matrix_is_singular
  '((name . "is_singular")
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
  'Graphene
  'GrapheneMatrix
  'matrix:multiply!
  'multiply!
  'graphene_matrix_multiply
  '((name . "multiply")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags out preallocated)
                  (item-size . 64)
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
  'Graphene
  'GrapheneMatrix
  'matrix:near?
  'near?
  'graphene_matrix_near
  '((name . "near")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "epsilon")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:normalize!
  'normalize!
  'graphene_matrix_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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
  'Graphene
  'GrapheneMatrix
  'matrix:perspective!
  'perspective!
  'graphene_matrix_perspective
  '((name . "perspective")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags out preallocated)
                  (item-size . 64)
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
  'Graphene
  'GrapheneMatrix
  'matrix:print
  'print
  'graphene_matrix_print
  '((name . "print")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GraphenePoint>
  "GraphenePoint"
  "graphene_point_get_type"
  8)

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:project-point!
  'project-point!
  'graphene_matrix_project_point
  '((name . "project_point")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
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
           ((name . "res")
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

(^sized-type
  'Graphene
  '<GrapheneRect>
  "GrapheneRect"
  "graphene_rect_get_type"
  16)

(^sized-type
  'Graphene
  '<GrapheneQuad>
  "GrapheneQuad"
  "graphene_quad_get_type"
  32)

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:project-rect!
  'project-rect!
  'graphene_matrix_project_rect
  '((name . "project_rect")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "r")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuad")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneMatrix
  'matrix:project-rect-bounds!
  'project-rect-bounds!
  'graphene_matrix_project_rect_bounds
  '((name . "project_rect_bounds")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "r")
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
           ((name . "res")
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
  'Graphene
  'GrapheneMatrix
  'matrix:rotate
  'rotate
  'graphene_matrix_rotate
  '((name . "rotate")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:rotate-euler
  'rotate-euler
  'graphene_matrix_rotate_euler
  '((name . "rotate_euler")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "e")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneEuler")
                  (flags ptr in)
                  (item-size . 20)
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
  'Graphene
  'GrapheneMatrix
  'matrix:rotate-quaternion
  'rotate-quaternion
  'graphene_matrix_rotate_quaternion
  '((name . "rotate_quaternion")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "q")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneMatrix
  'matrix:rotate-x
  'rotate-x
  'graphene_matrix_rotate_x
  '((name . "rotate_x")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:rotate-y
  'rotate-y
  'graphene_matrix_rotate_y
  '((name . "rotate_y")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:rotate-z
  'rotate-z
  'graphene_matrix_rotate_z
  '((name . "rotate_z")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:scale
  'scale
  'graphene_matrix_scale
  '((name . "scale")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:skew-xy
  'skew-xy
  'graphene_matrix_skew_xy
  '((name . "skew_xy")
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
  'Graphene
  'GrapheneMatrix
  'matrix:skew-xz
  'skew-xz
  'graphene_matrix_skew_xz
  '((name . "skew_xz")
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
  'Graphene
  'GrapheneMatrix
  'matrix:skew-yz
  'skew-yz
  'graphene_matrix_skew_yz
  '((name . "skew_yz")
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
  'Graphene
  'GrapheneMatrix
  'matrix:to-2d
  'to-2d
  'graphene_matrix_to_2d
  '((name . "to_2d")
    (is-method . #t)
    (s-output-len . 6)
    (c-output-len . 6)
    (pdata ((name . "xx")
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
           ((name . "yx")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "xy")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 2)
            (c-output-pos . 2)
            (s-output-pos . 2))
           ((name . "yy")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 3)
            (c-output-pos . 3)
            (s-output-pos . 3))
           ((name . "x_0")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 4)
            (c-output-pos . 4)
            (s-output-pos . 4))
           ((name . "y_0")
            (meta (arg-type . double)
                  (flags out)
                  (item-size . 8)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 5)
            (c-output-pos . 5)
            (s-output-pos . 5)))
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
  'Graphene
  'GrapheneMatrix
  'matrix:to-float!
  'to-float!
  'graphene_matrix_to_float
  '((name . "to_float")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 16)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-bounds!
  'transform-bounds!
  'graphene_matrix_transform_bounds
  '((name . "transform_bounds")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "r")
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
           ((name . "res")
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-box!
  'transform-box!
  'graphene_matrix_transform_box
  '((name . "transform_box")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-point!
  'transform-point!
  'graphene_matrix_transform_point
  '((name . "transform_point")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
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
           ((name . "res")
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-point3d!
  'transform-point3d!
  'graphene_matrix_transform_point3d
  '((name . "transform_point3d")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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

(^sized-type
  'Graphene
  '<GrapheneRay>
  "GrapheneRay"
  "graphene_ray_get_type"
  32)

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:transform-ray!
  'transform-ray!
  'graphene_matrix_transform_ray
  '((name . "transform_ray")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "r")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRay")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRay")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-rect!
  'transform-rect!
  'graphene_matrix_transform_rect
  '((name . "transform_rect")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "r")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuad")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-sphere!
  'transform-sphere!
  'graphene_matrix_transform_sphere
  '((name . "transform_sphere")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "s")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-vec3!
  'transform-vec3!
  'graphene_matrix_transform_vec3
  '((name . "transform_vec3")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneMatrix
  'matrix:transform-vec4!
  'transform-vec4!
  'graphene_matrix_transform_vec4
  '((name . "transform_vec4")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneMatrix
  'matrix:translate
  'translate
  'graphene_matrix_translate
  '((name . "translate")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "pos")
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
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:transpose!
  'transpose!
  'graphene_matrix_transpose
  '((name . "transpose")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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
  'Graphene
  'GrapheneMatrix
  'matrix:unproject-point3d!
  'unproject-point3d!
  'graphene_matrix_unproject_point3d
  '((name . "unproject_point3d")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "modelview")
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
            (s-input-pos . 0))
           ((name . "point")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:untransform-bounds!
  'untransform-bounds!
  'graphene_matrix_untransform_bounds
  '((name . "untransform_bounds")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "r")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneMatrix
  'matrix:untransform-point!
  'untransform-point!
  'graphene_matrix_untransform_point
  '((name . "untransform_point")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "p")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
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

(^constant 'PI 3.141593)

(^constant 'PI_2 1.570796)

(^function
  'Graphene
  'GraphenePlane
  'plane:alloc
  'alloc
  'graphene_plane_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePlane")
            (flags ptr out)
            (item-size . 20)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:distance
  'distance
  'graphene_plane_distance
  '((name . "distance")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:equal?
  'equal?
  'graphene_plane_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
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
  'Graphene
  'GraphenePlane
  'plane:free
  'free
  'graphene_plane_free
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
  'Graphene
  'GraphenePlane
  'plane:get-constant
  'get-constant
  'graphene_plane_get_constant
  '((name . "get_constant")
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
  'Graphene
  'GraphenePlane
  'plane:get-normal!
  'get-normal!
  'graphene_plane_get_normal
  '((name . "get_normal")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "normal")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GraphenePlane
  'plane:init
  'init
  'graphene_plane_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "normal")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "constant")
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
            (gtype-name . "GraphenePlane")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:init-from-plane
  'init-from-plane
  'graphene_plane_init_from_plane
  '((name . "init_from_plane")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
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
            (gtype-name . "GraphenePlane")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:init-from-point
  'init-from-point
  'graphene_plane_init_from_point
  '((name . "init_from_point")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "normal")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "point")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in)
                  (item-size . 12)
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
            (gtype-name . "GraphenePlane")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:init-from-points
  'init-from-points
  'graphene_plane_init_from_points
  '((name . "init_from_points")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "a")
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
            (s-input-pos . 0))
           ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "c")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in)
                  (item-size . 12)
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
            (gtype-name . "GraphenePlane")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:init-from-vec4
  'init-from-vec4
  'graphene_plane_init_from_vec4
  '((name . "init_from_vec4")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePlane")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePlane
  'plane:negate!
  'negate!
  'graphene_plane_negate
  '((name . "negate")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GraphenePlane
  'plane:normalize!
  'normalize!
  'graphene_plane_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GraphenePlane
  'plane:transform!
  'transform!
  'graphene_plane_transform
  '((name . "transform")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "normal_matrix")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneMatrix")
                  (flags ptr in nullable)
                  (item-size . 64)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags out preallocated)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:alloc
  'alloc
  'graphene_point_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:distance
  'distance
  'graphene_point_distance
  '((name . "distance")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-input-len . 1)
    (c-output-len . 2)
    (pdata ((name . "b")
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
           ((name . "d_x")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "d_y")
            (meta (arg-type . float)
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:equal?
  'equal?
  'graphene_point_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:free
  'free
  'graphene_point_free
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
  'Graphene
  'GraphenePoint
  'point:init
  'init
  'graphene_point_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "x")
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
           ((name . "y")
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
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:init-from-point
  'init-from-point
  'graphene_point_init_from_point
  '((name . "init_from_point")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
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
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
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
  'Graphene
  'GraphenePoint
  'point:init-from-vec2
  'init-from-vec2
  'graphene_point_init_from_vec2
  '((name . "init_from_vec2")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GraphenePoint
  'point:interpolate!
  'interpolate!
  'graphene_point_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "b")
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
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:near?
  'near?
  'graphene_point_near
  '((name . "near")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "b")
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
           ((name . "epsilon")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint
  'point:to-vec2!
  'to-vec2!
  'graphene_point_to_vec2
  '((name . "to_vec2")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GraphenePoint
  'point:zero
  'zero
  'graphene_point_zero
  '((name . "zero")
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
  'Graphene
  'GraphenePoint3D
  'point3d:alloc
  'alloc
  'graphene_point3d_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint3D")
            (flags ptr out)
            (item-size . 12)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:cross!
  'cross!
  'graphene_point3d_cross
  '((name . "cross")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GraphenePoint3D
  'point3d:distance!
  'distance!
  'graphene_point3d_distance
  '((name . "distance")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "delta")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:dot
  'dot
  'graphene_point3d_dot
  '((name . "dot")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:equal?
  'equal?
  'graphene_point3d_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:free
  'free
  'graphene_point3d_free
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
  'Graphene
  'GraphenePoint3D
  'point3d:init
  'init
  'graphene_point3d_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
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
            (gtype-name . "GraphenePoint3D")
            (flags ptr out)
            (item-size . 12)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:init-from-point
  'init-from-point
  'graphene_point3d_init_from_point
  '((name . "init_from_point")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
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
            (gtype-name . "GraphenePoint3D")
            (flags ptr out)
            (item-size . 12)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:init-from-vec3
  'init-from-vec3
  'graphene_point3d_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint3D")
            (flags ptr out)
            (item-size . 12)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:interpolate!
  'interpolate!
  'graphene_point3d_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:length
  'length
  'graphene_point3d_length
  '((name . "length")
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
  'Graphene
  'GraphenePoint3D
  'point3d:near?
  'near?
  'graphene_point3d_near
  '((name . "near")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "b")
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
            (s-input-pos . 0))
           ((name . "epsilon")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GraphenePoint3D
  'point3d:normalize!
  'normalize!
  'graphene_point3d_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GraphenePoint3D
  'point3d:normalize-viewport!
  'normalize-viewport!
  'graphene_point3d_normalize_viewport
  '((name . "normalize_viewport")
    (is-method . #t)
    (s-input-req . 4)
    (s-output-len . 1)
    (c-input-len . 3)
    (c-output-len . 1)
    (pdata ((name . "viewport")
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
           ((name . "z_near")
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
           ((name . "z_far")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 3)
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
  'Graphene
  'GraphenePoint3D
  'point3d:scale!
  'scale!
  'graphene_point3d_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GraphenePoint3D
  'point3d:to-vec3!
  'to-vec3!
  'graphene_point3d_to_vec3
  '((name . "to_vec3")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "v")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GraphenePoint3D
  'point3d:zero
  'zero
  'graphene_point3d_zero
  '((name . "zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint3D")
            (flags ptr out)
            (item-size . 12)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuad
  'quad:alloc
  'alloc
  'graphene_quad_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuad")
            (flags ptr out)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuad
  'quad:bounds!
  'bounds!
  'graphene_quad_bounds
  '((name . "bounds")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "r")
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

(^function
  'Graphene
  'GrapheneQuad
  'quad:contains?
  'contains?
  'graphene_quad_contains
  '((name . "contains")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuad
  'quad:free
  'free
  'graphene_quad_free
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
  'Graphene
  'GrapheneQuad
  'quad:get-point
  'get-point
  'graphene_quad_get_point
  '((name . "get_point")
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
            (gtype-name . "GraphenePoint")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuad
  'quad:init
  'init
  'graphene_quad_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "p1")
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
           ((name . "p2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "p3")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "p4")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuad")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuad
  'quad:init-from-points
  'init-from-points
  'graphene_quad_init_from_points
  '((name . "init_from_points")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "points")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 4)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GraphenePoint")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuad")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuad
  'quad:init-from-rect
  'init-from-rect
  'graphene_quad_init_from_rect
  '((name . "init_from_rect")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "r")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuad")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:alloc
  'alloc
  'graphene_quaternion_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:add!
  'add!
  'graphene_quaternion_add
  '((name . "add")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:dot
  'dot
  'graphene_quaternion_dot
  '((name . "dot")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:equal?
  'equal?
  'graphene_quaternion_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:free
  'free
  'graphene_quaternion_free
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:init
  'init
  'graphene_quaternion_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "w")
            (meta (arg-type . float)
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-angle-vec3
  'init-from-angle-vec3
  'graphene_quaternion_init_from_angle_vec3
  '((name . "init_from_angle_vec3")
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
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-angles
  'init-from-angles
  'graphene_quaternion_init_from_angles
  '((name . "init_from_angles")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "deg_x")
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
           ((name . "deg_y")
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
           ((name . "deg_z")
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
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-euler
  'init-from-euler
  'graphene_quaternion_init_from_euler
  '((name . "init_from_euler")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "e")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneEuler")
                  (flags ptr in)
                  (item-size . 20)
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
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-matrix
  'init-from-matrix
  'graphene_quaternion_init_from_matrix
  '((name . "init_from_matrix")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "m")
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
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-quaternion
  'init-from-quaternion
  'graphene_quaternion_init_from_quaternion
  '((name . "init_from_quaternion")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-radians
  'init-from-radians
  'graphene_quaternion_init_from_radians
  '((name . "init_from_radians")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "rad_x")
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
           ((name . "rad_y")
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
           ((name . "rad_z")
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
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-from-vec4
  'init-from-vec4
  'graphene_quaternion_init_from_vec4
  '((name . "init_from_vec4")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:init-identity
  'init-identity
  'graphene_quaternion_init_identity
  '((name . "init_identity")
    (is-method . #t)
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneQuaternion")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:invert!
  'invert!
  'graphene_quaternion_invert
  '((name . "invert")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:multiply!
  'multiply!
  'graphene_quaternion_multiply
  '((name . "multiply")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:normalize!
  'normalize!
  'graphene_quaternion_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:scale!
  'scale!
  'graphene_quaternion_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:slerp!
  'slerp!
  'graphene_quaternion_slerp
  '((name . "slerp")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneQuaternion")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:to-angle-vec3!
  'to-angle-vec3!
  'graphene_quaternion_to_angle_vec3
  '((name . "to_angle_vec3")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 2)
    (c-output-len . 2)
    (pdata ((name . "angle")
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
           ((name . "axis")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 1)
            (s-input-pos . 0)
            (s-output-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneQuaternion
  'quaternion:to-angles
  'to-angles
  'graphene_quaternion_to_angles
  '((name . "to_angles")
    (is-method . #t)
    (s-output-len . 3)
    (c-output-len . 3)
    (pdata ((name . "deg_x")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "deg_y")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "deg_z")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:to-matrix!
  'to-matrix!
  'graphene_quaternion_to_matrix
  '((name . "to_matrix")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "m")
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:to-radians
  'to-radians
  'graphene_quaternion_to_radians
  '((name . "to_radians")
    (is-method . #t)
    (s-output-len . 3)
    (c-output-len . 3)
    (pdata ((name . "rad_x")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 0)
            (c-output-pos . 0)
            (s-output-pos . 0))
           ((name . "rad_y")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 1)
            (s-output-pos . 1))
           ((name . "rad_z")
            (meta (arg-type . float)
                  (flags out optional)
                  (item-size . 4)
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
  'Graphene
  'GrapheneQuaternion
  'quaternion:to-vec4!
  'to-vec4!
  'graphene_quaternion_to_vec4
  '((name . "to_vec4")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneRay
  'ray:alloc
  'alloc
  'graphene_ray_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRay")
            (flags ptr out)
            (item-size . 32)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:equal?
  'equal?
  'graphene_ray_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRay")
                  (flags ptr in)
                  (item-size . 32)
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
  'Graphene
  'GrapheneRay
  'ray:free
  'free
  'graphene_ray_free
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
  'Graphene
  'GrapheneRay
  'ray:get-closest-point-to-point!
  'get-closest-point-to-point!
  'graphene_ray_get_closest_point_to_point
  '((name . "get_closest_point_to_point")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneRay
  'ray:get-direction!
  'get-direction!
  'graphene_ray_get_direction
  '((name . "get_direction")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "direction")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneRay
  'ray:get-distance-to-plane
  'get-distance-to-plane
  'graphene_ray_get_distance_to_plane
  '((name . "get_distance_to_plane")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0)))
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
  'Graphene
  'GrapheneRay
  'ray:get-distance-to-point
  'get-distance-to-point
  'graphene_ray_get_distance_to_point
  '((name . "get_distance_to_point")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:get-origin!
  'get-origin!
  'graphene_ray_get_origin
  '((name . "get_origin")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "origin")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneRay
  'ray:get-position-at!
  'get-position-at!
  'graphene_ray_get_position_at
  '((name . "get_position_at")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "t")
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
           ((name . "position")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneRay
  'ray:init
  'init
  'graphene_ray_init
  '((name . "init")
    (is-method . #t)
    (s-input-opt . 2)
    (c-input-len . 2)
    (pdata ((name . "origin")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "direction")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRay")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:init-from-ray
  'init-from-ray
  'graphene_ray_init_from_ray
  '((name . "init_from_ray")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRay")
                  (flags ptr in)
                  (item-size . 32)
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
            (gtype-name . "GrapheneRay")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:init-from-vec3
  'init-from-vec3
  'graphene_ray_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-opt . 2)
    (c-input-len . 2)
    (pdata ((name . "origin")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "direction")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRay")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:intersect-box
  'intersect-box
  'graphene_ray_intersect_box
  '((name . "intersect_box")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "t_out")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (qualified-name . "%GrapheneRayIntersectionKind")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:intersect-sphere
  'intersect-sphere
  'graphene_ray_intersect_sphere
  '((name . "intersect_sphere")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "s")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags ptr in)
                  (item-size . 20)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "t_out")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (qualified-name . "%GrapheneRayIntersectionKind")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^sized-type
  'Graphene
  '<GrapheneTriangle>
  "GrapheneTriangle"
  "graphene_triangle_get_type"
  48)

(^function
  'Graphene
  'GrapheneRay
  'ray:intersect-triangle
  'intersect-triangle
  'graphene_ray_intersect_triangle
  '((name . "intersect_triangle")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "t")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneTriangle")
                  (flags ptr in)
                  (item-size . 48)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "t_out")
            (meta (arg-type . float)
                  (flags out)
                  (item-size . 4)
                  (transfer . everything))
            (s-direction . output)
            (tuple . singleton)
            (presence . implicit)
            (i . 1)
            (c-output-pos . 0)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . enum)
            (qualified-name . "%GrapheneRayIntersectionKind")
            (flags out)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRay
  'ray:intersects-box?
  'intersects-box?
  'graphene_ray_intersects_box
  '((name . "intersects_box")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags ptr in)
                  (item-size . 32)
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
  'Graphene
  'GrapheneRay
  'ray:intersects-sphere?
  'intersects-sphere?
  'graphene_ray_intersects_sphere
  '((name . "intersects_sphere")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "s")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags ptr in)
                  (item-size . 20)
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
  'Graphene
  'GrapheneRay
  'ray:intersects-triangle?
  'intersects-triangle?
  'graphene_ray_intersects_triangle
  '((name . "intersects_triangle")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "t")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneTriangle")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^untyped-enum
  '<%GrapheneRayIntersectionKind>
  '%GrapheneRayIntersectionKind
  '((none . 0) (enter . 1) (leave . 2)))

(^untyped-enum-conv
  'ray-intersection-kind
  '%GrapheneRayIntersectionKind)

(^function
  'Graphene
  'GrapheneRect
  'rect:contains-point?
  'contains-point?
  'graphene_rect_contains_point
  '((name . "contains_point")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:contains-rect?
  'contains-rect?
  'graphene_rect_contains_rect
  '((name . "contains_rect")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:equal?
  'equal?
  'graphene_rect_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:expand!
  'expand!
  'graphene_rect_expand
  '((name . "expand")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
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
           ((name . "res")
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
  'Graphene
  'GrapheneRect
  'rect:free
  'free
  'graphene_rect_free
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
  'Graphene
  'GrapheneRect
  'rect:get-area
  'get-area
  'graphene_rect_get_area
  '((name . "get_area")
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
  'Graphene
  'GrapheneRect
  'rect:get-bottom-left!
  'get-bottom-left!
  'graphene_rect_get_bottom_left
  '((name . "get_bottom_left")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
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
  'Graphene
  'GrapheneRect
  'rect:get-bottom-right!
  'get-bottom-right!
  'graphene_rect_get_bottom_right
  '((name . "get_bottom_right")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
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
  'Graphene
  'GrapheneRect
  'rect:get-center!
  'get-center!
  'graphene_rect_get_center
  '((name . "get_center")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
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
  'Graphene
  'GrapheneRect
  'rect:get-height
  'get-height
  'graphene_rect_get_height
  '((name . "get_height")
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
  'Graphene
  'GrapheneRect
  'rect:get-top-left!
  'get-top-left!
  'graphene_rect_get_top_left
  '((name . "get_top_left")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
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
  'Graphene
  'GrapheneRect
  'rect:get-top-right!
  'get-top-right!
  'graphene_rect_get_top_right
  '((name . "get_top_right")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint")
                  (flags out preallocated)
                  (item-size . 8)
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
  'Graphene
  'GrapheneRect
  'rect:get-vertices!
  'get-vertices!
  'graphene_rect_get_vertices
  '((name . "get_vertices")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "vertices")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 4)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GrapheneVec2")
                     (item-size . 16)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneRect
  'rect:get-width
  'get-width
  'graphene_rect_get_width
  '((name . "get_width")
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
  'Graphene
  'GrapheneRect
  'rect:get-x
  'get-x
  'graphene_rect_get_x
  '((name . "get_x")
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
  'Graphene
  'GrapheneRect
  'rect:get-y
  'get-y
  'graphene_rect_get_y
  '((name . "get_y")
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
  'Graphene
  'GrapheneRect
  'rect:init
  'init
  'graphene_rect_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "width")
            (meta (arg-type . float)
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
            (meta (arg-type . float)
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:init-from-rect
  'init-from-rect
  'graphene_rect_init_from_rect
  '((name . "init_from_rect")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:inset
  'inset
  'graphene_rect_inset
  '((name . "inset")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "d_x")
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
           ((name . "d_y")
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
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:inset-r!
  'inset-r!
  'graphene_rect_inset_r
  '((name . "inset_r")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "d_x")
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
           ((name . "d_y")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:interpolate!
  'interpolate!
  'graphene_rect_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "b")
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
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:intersection!
  'intersection!
  'graphene_rect_intersection
  '((name . "intersection")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
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
  'Graphene
  'GrapheneRect
  'rect:normalize
  'normalize
  'graphene_rect_normalize
  '((name . "normalize")
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

(^function
  'Graphene
  'GrapheneRect
  'rect:normalize-r!
  'normalize-r!
  'graphene_rect_normalize_r
  '((name . "normalize_r")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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

(^function
  'Graphene
  'GrapheneRect
  'rect:offset
  'offset
  'graphene_rect_offset
  '((name . "offset")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "d_x")
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
           ((name . "d_y")
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
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:offset-r!
  'offset-r!
  'graphene_rect_offset_r
  '((name . "offset_r")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "d_x")
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
           ((name . "d_y")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:round!
  'round!
  'graphene_rect_round
  '((name . "round")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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

(^function
  'Graphene
  'GrapheneRect
  'rect:round-extents!
  'round-extents!
  'graphene_rect_round_extents
  '((name . "round_extents")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
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

(^function
  'Graphene
  'GrapheneRect
  'rect:round-to-pixel
  'round-to-pixel
  'graphene_rect_round_to_pixel
  '((name . "round_to_pixel")
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

(^function
  'Graphene
  'GrapheneRect
  'rect:scale!
  'scale!
  'graphene_rect_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "s_h")
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
           ((name . "s_v")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneRect")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:union!
  'union!
  'graphene_rect_union
  '((name . "union")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
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
           ((name . "res")
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
  'Graphene
  'GrapheneRect
  'rect:alloc
  'alloc
  'graphene_rect_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneRect
  'rect:zero
  'zero
  'graphene_rect_zero
  '((name . "zero")
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

(^sized-type
  'Graphene
  '<GrapheneSize>
  "GrapheneSize"
  "graphene_size_get_type"
  8)

(^function
  'Graphene
  'GrapheneSize
  'size:alloc
  'alloc
  'graphene_size_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSize")
            (flags ptr out)
            (item-size . 8)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSize
  'size:equal?
  'equal?
  'graphene_size_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSize")
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
  'Graphene
  'GrapheneSize
  'size:free
  'free
  'graphene_size_free
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
  'Graphene
  'GrapheneSize
  'size:init
  'init
  'graphene_size_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "width")
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
           ((name . "height")
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
            (gtype-name . "GrapheneSize")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSize
  'size:init-from-size
  'init-from-size
  'graphene_size_init_from_size
  '((name . "init_from_size")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSize")
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
            (gtype-name . "GrapheneSize")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSize
  'size:interpolate!
  'interpolate!
  'graphene_size_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSize")
                  (flags ptr in)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSize")
                  (flags out preallocated)
                  (item-size . 8)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSize
  'size:scale!
  'scale!
  'graphene_size_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSize")
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
  'Graphene
  'GrapheneSize
  'size:zero
  'zero
  'graphene_size_zero
  '((name . "zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSize")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:alloc
  'alloc
  'graphene_sphere_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSphere")
            (flags ptr out)
            (item-size . 20)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:contains-point?
  'contains-point?
  'graphene_sphere_contains_point
  '((name . "contains_point")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:distance
  'distance
  'graphene_sphere_distance
  '((name . "distance")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:equal?
  'equal?
  'graphene_sphere_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags ptr in)
                  (item-size . 20)
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
  'Graphene
  'GrapheneSphere
  'sphere:free
  'free
  'graphene_sphere_free
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
  'Graphene
  'GrapheneSphere
  'sphere:get-bounding-box!
  'get-bounding-box!
  'graphene_sphere_get_bounding_box
  '((name . "get_bounding_box")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "box")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneSphere
  'sphere:get-center!
  'get-center!
  'graphene_sphere_get_center
  '((name . "get_center")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "center")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneSphere
  'sphere:get-radius
  'get-radius
  'graphene_sphere_get_radius
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

(^function
  'Graphene
  'GrapheneSphere
  'sphere:init
  'init
  'graphene_sphere_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "center")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSphere")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:init-from-points
  'init-from-points
  'graphene_sphere_init_from_points
  '((name . "init_from_points")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 3)
    (pdata ((name . "n_points")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-input-pos . 0))
           ((name . "points")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GraphenePoint3D")
                     (item-size . 12)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 0))
           ((name . "center")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSphere")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:init-from-vectors
  'init-from-vectors
  'graphene_sphere_init_from_vectors
  '((name . "init_from_vectors")
    (is-method . #t)
    (s-input-req . 1)
    (s-input-opt . 1)
    (c-input-len . 3)
    (pdata ((name . "n_vectors")
            (meta (arg-type . uint32)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . array_size)
            (presence . implicit)
            (i . 0)
            (c-input-pos . 0))
           ((name . "vectors")
            (meta (arg-type . array)
                  (flags ptr in sized)
                  (transfer . nothing)
                  (params
                    ((arg-type . GBoxed)
                     (gtype-name . "GrapheneVec3")
                     (item-size . 16)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . array)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 0))
           ((name . "center")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 1)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSphere")
            (flags ptr out)
            (item-size . 20)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneSphere
  'sphere:is-empty?
  'is-empty?
  'graphene_sphere_is_empty
  '((name . "is_empty")
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
  'Graphene
  'GrapheneSphere
  'sphere:translate!
  'translate!
  'graphene_sphere_translate
  '((name . "translate")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneSphere")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GrapheneTriangle
  'triangle:alloc
  'alloc
  'graphene_triangle_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneTriangle")
            (flags ptr out)
            (item-size . 48)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:contains-point?
  'contains-point?
  'graphene_triangle_contains_point
  '((name . "contains_point")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "p")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:equal?
  'equal?
  'graphene_triangle_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneTriangle")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:free
  'free
  'graphene_triangle_free
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-area
  'get-area
  'graphene_triangle_get_area
  '((name . "get_area")
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-barycoords!
  'get-barycoords!
  'graphene_triangle_get_barycoords
  '((name . "get_barycoords")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:get-bounding-box!
  'get-bounding-box!
  'graphene_triangle_get_bounding_box
  '((name . "get_bounding_box")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneBox")
                  (flags out preallocated)
                  (item-size . 32)
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-midpoint!
  'get-midpoint!
  'graphene_triangle_get_midpoint
  '((name . "get_midpoint")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated)
                  (item-size . 12)
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-normal!
  'get-normal!
  'graphene_triangle_get_normal
  '((name . "get_normal")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-plane!
  'get-plane!
  'graphene_triangle_get_plane
  '((name . "get_plane")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePlane")
                  (flags out preallocated)
                  (item-size . 20)
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-points!
  'get-points!
  'graphene_triangle_get_points
  '((name . "get_points")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 3)
    (c-output-len . 3)
    (pdata ((name . "a")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated optional)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated optional)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 1)
            (s-input-pos . 1)
            (s-output-pos . 1))
           ((name . "c")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags out preallocated optional)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 2)
            (s-input-pos . 2)
            (s-output-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:get-uv!
  'get-uv!
  'graphene_triangle_get_uv
  '((name . "get_uv")
    (is-method . #t)
    (s-input-req . 5)
    (s-output-len . 1)
    (c-input-len . 4)
    (c-output-len . 1)
    (pdata ((name . "p")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "uv_a")
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
            (s-input-pos . 1))
           ((name . "uv_b")
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
            (s-input-pos . 2))
           ((name . "uv_c")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 3)
            (c-input-pos . 3)
            (s-input-pos . 3))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 4)
            (c-output-pos . 0)
            (s-input-pos . 4)
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
  'Graphene
  'GrapheneTriangle
  'triangle:get-vertices!
  'get-vertices!
  'graphene_triangle_get_vertices
  '((name . "get_vertices")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 3)
    (c-output-len . 3)
    (pdata ((name . "a")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated optional)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-output-pos . 0)
            (s-input-pos . 0)
            (s-output-pos . 0))
           ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated optional)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-output-pos . 1)
            (s-input-pos . 1)
            (s-output-pos . 1))
           ((name . "c")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated optional)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 2)
            (s-input-pos . 2)
            (s-output-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:init-from-float
  'init-from-float
  'graphene_triangle_init_from_float
  '((name . "init_from_float")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "a")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 3)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "b")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 3)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "c")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 3)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneTriangle")
            (flags ptr out)
            (item-size . 48)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:init-from-point3d
  'init-from-point3d
  'graphene_triangle_init_from_point3d
  '((name . "init_from_point3d")
    (is-method . #t)
    (s-input-opt . 3)
    (c-input-len . 3)
    (pdata ((name . "a")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "c")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GraphenePoint3D")
                  (flags ptr in nullable)
                  (item-size . 12)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2)))
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneTriangle")
            (flags ptr out)
            (item-size . 48)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneTriangle
  'triangle:init-from-vec3
  'init-from-vec3
  'graphene_triangle_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-opt . 3)
    (c-input-len . 3)
    (pdata ((name . "a")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in nullable)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . optional)
            (i . 1)
            (c-input-pos . 1)
            (s-input-pos . 1))
           ((name . "c")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneTriangle")
            (flags ptr out)
            (item-size . 48)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^constant 'VEC2_LEN 2)

(^constant 'VEC3_LEN 3)

(^constant 'VEC4_LEN 4)

(^function
  'Graphene
  'GrapheneVec2
  'vec2:alloc
  'alloc
  'graphene_vec2_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:add!
  'add!
  'graphene_vec2_add
  '((name . "add")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:divide!
  'divide!
  'graphene_vec2_divide
  '((name . "divide")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:dot
  'dot
  'graphene_vec2_dot
  '((name . "dot")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:equal?
  'equal?
  'graphene_vec2_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:free
  'free
  'graphene_vec2_free
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
  'Graphene
  'GrapheneVec2
  'vec2:get-x
  'get-x
  'graphene_vec2_get_x
  '((name . "get_x")
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
  'Graphene
  'GrapheneVec2
  'vec2:get-y
  'get-y
  'graphene_vec2_get_y
  '((name . "get_y")
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
  'Graphene
  'GrapheneVec2
  'vec2:init
  'init
  'graphene_vec2_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "x")
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
           ((name . "y")
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
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:init-from-float
  'init-from-float
  'graphene_vec2_init_from_float
  '((name . "init_from_float")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 2)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
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
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:init-from-vec2
  'init-from-vec2
  'graphene_vec2_init_from_vec2
  '((name . "init_from_vec2")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:interpolate!
  'interpolate!
  'graphene_vec2_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:length
  'length
  'graphene_vec2_length
  '((name . "length")
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
  'Graphene
  'GrapheneVec2
  'vec2:max!
  'max!
  'graphene_vec2_max
  '((name . "max")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:min!
  'min!
  'graphene_vec2_min
  '((name . "min")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:multiply!
  'multiply!
  'graphene_vec2_multiply
  '((name . "multiply")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:near?
  'near?
  'graphene_vec2_near
  '((name . "near")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "epsilon")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:negate!
  'negate!
  'graphene_vec2_negate
  '((name . "negate")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:normalize!
  'normalize!
  'graphene_vec2_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:scale!
  'scale!
  'graphene_vec2_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:subtract!
  'subtract!
  'graphene_vec2_subtract
  '((name . "subtract")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec2
  'vec2:to-float!
  'to-float!
  'graphene_vec2_to_float
  '((name . "to_float")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "dest")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 2)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneVec2
  'vec2:one
  'one
  'graphene_vec2_one
  '((name . "one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:x-axis
  'x-axis
  'graphene_vec2_x_axis
  '((name . "x_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:y-axis
  'y-axis
  'graphene_vec2_y_axis
  '((name . "y_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec2
  'vec2:zero
  'zero
  'graphene_vec2_zero
  '((name . "zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:alloc
  'alloc
  'graphene_vec3_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:add!
  'add!
  'graphene_vec3_add
  '((name . "add")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:cross!
  'cross!
  'graphene_vec3_cross
  '((name . "cross")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:divide!
  'divide!
  'graphene_vec3_divide
  '((name . "divide")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:dot
  'dot
  'graphene_vec3_dot
  '((name . "dot")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:equal?
  'equal?
  'graphene_vec3_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:free
  'free
  'graphene_vec3_free
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
  'Graphene
  'GrapheneVec3
  'vec3:get-x
  'get-x
  'graphene_vec3_get_x
  '((name . "get_x")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-xy!
  'get-xy!
  'graphene_vec3_get_xy
  '((name . "get_xy")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-xy0!
  'get-xy0!
  'graphene_vec3_get_xy0
  '((name . "get_xy0")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-xyz0!
  'get-xyz0!
  'graphene_vec3_get_xyz0
  '((name . "get_xyz0")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-xyz1!
  'get-xyz1!
  'graphene_vec3_get_xyz1
  '((name . "get_xyz1")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-xyzw!
  'get-xyzw!
  'graphene_vec3_get_xyzw
  '((name . "get_xyzw")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "w")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-y
  'get-y
  'graphene_vec3_get_y
  '((name . "get_y")
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
  'Graphene
  'GrapheneVec3
  'vec3:get-z
  'get-z
  'graphene_vec3_get_z
  '((name . "get_z")
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
  'Graphene
  'GrapheneVec3
  'vec3:init
  'init
  'graphene_vec3_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
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
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:init-from-float
  'init-from-float
  'graphene_vec3_init_from_float
  '((name . "init_from_float")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 3)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
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
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:init-from-vec3
  'init-from-vec3
  'graphene_vec3_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:interpolate!
  'interpolate!
  'graphene_vec3_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:length
  'length
  'graphene_vec3_length
  '((name . "length")
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
  'Graphene
  'GrapheneVec3
  'vec3:max!
  'max!
  'graphene_vec3_max
  '((name . "max")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:min!
  'min!
  'graphene_vec3_min
  '((name . "min")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:multiply!
  'multiply!
  'graphene_vec3_multiply
  '((name . "multiply")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:near?
  'near?
  'graphene_vec3_near
  '((name . "near")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "epsilon")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:negate!
  'negate!
  'graphene_vec3_negate
  '((name . "negate")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:normalize!
  'normalize!
  'graphene_vec3_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:scale!
  'scale!
  'graphene_vec3_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:subtract!
  'subtract!
  'graphene_vec3_subtract
  '((name . "subtract")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec3
  'vec3:to-float!
  'to-float!
  'graphene_vec3_to_float
  '((name . "to_float")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "dest")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 3)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneVec3
  'vec3:one
  'one
  'graphene_vec3_one
  '((name . "one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:x-axis
  'x-axis
  'graphene_vec3_x_axis
  '((name . "x_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:y-axis
  'y-axis
  'graphene_vec3_y_axis
  '((name . "y_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:z-axis
  'z-axis
  'graphene_vec3_z_axis
  '((name . "z_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec3
  'vec3:zero
  'zero
  'graphene_vec3_zero
  '((name . "zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:alloc
  'alloc
  'graphene_vec4_alloc
  '((name . "alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:add!
  'add!
  'graphene_vec4_add
  '((name . "add")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:divide!
  'divide!
  'graphene_vec4_divide
  '((name . "divide")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:dot
  'dot
  'graphene_vec4_dot
  '((name . "dot")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
      (meta (arg-type . float)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:equal?
  'equal?
  'graphene_vec4_equal
  '((name . "equal")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:free
  'free
  'graphene_vec4_free
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
  'Graphene
  'GrapheneVec4
  'vec4:get-w
  'get-w
  'graphene_vec4_get_w
  '((name . "get_w")
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
  'Graphene
  'GrapheneVec4
  'vec4:get-x
  'get-x
  'graphene_vec4_get_x
  '((name . "get_x")
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
  'Graphene
  'GrapheneVec4
  'vec4:get-xy!
  'get-xy!
  'graphene_vec4_get_xy
  '((name . "get_xy")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
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
  'Graphene
  'GrapheneVec4
  'vec4:get-xyz!
  'get-xyz!
  'graphene_vec4_get_xyz
  '((name . "get_xyz")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
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
  'Graphene
  'GrapheneVec4
  'vec4:get-y
  'get-y
  'graphene_vec4_get_y
  '((name . "get_y")
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
  'Graphene
  'GrapheneVec4
  'vec4:get-z
  'get-z
  'graphene_vec4_get_z
  '((name . "get_z")
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
  'Graphene
  'GrapheneVec4
  'vec4:init
  'init
  'graphene_vec4_init
  '((name . "init")
    (is-method . #t)
    (s-input-req . 4)
    (c-input-len . 4)
    (pdata ((name . "x")
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
           ((name . "y")
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
           ((name . "z")
            (meta (arg-type . float)
                  (flags in)
                  (item-size . 4)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-input-pos . 2)
            (s-input-pos . 2))
           ((name . "w")
            (meta (arg-type . float)
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:init-from-float
  'init-from-float
  'graphene_vec4_init_from_float
  '((name . "init_from_float")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . array)
                  (flags ptr in fixed-size)
                  (fixed-size . 4)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
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
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:init-from-vec2
  'init-from-vec2
  'graphene_vec4_init_from_vec2
  '((name . "init_from_vec2")
    (is-method . #t)
    (s-input-req . 3)
    (c-input-len . 3)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec2")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "z")
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
           ((name . "w")
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
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:init-from-vec3
  'init-from-vec3
  'graphene_vec4_init_from_vec3
  '((name . "init_from_vec3")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec3")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "w")
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
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:init-from-vec4
  'init-from-vec4
  'graphene_vec4_init_from_vec4
  '((name . "init_from_vec4")
    (is-method . #t)
    (s-input-req . 1)
    (c-input-len . 1)
    (pdata ((name . "src")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec4")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:interpolate!
  'interpolate!
  'graphene_vec4_interpolate
  '((name . "interpolate")
    (is-method . #t)
    (s-input-req . 3)
    (s-output-len . 1)
    (c-input-len . 2)
    (c-output-len . 1)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "factor")
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
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags out preallocated)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . prealloc)
            (tuple . singleton)
            (presence . required)
            (i . 2)
            (c-output-pos . 0)
            (s-input-pos . 2)
            (s-output-pos . 0)))
    (return-val
      (name . "%return")
      (meta (arg-type . void) (transfer . nothing))
      (s-direction . void)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:length
  'length
  'graphene_vec4_length
  '((name . "length")
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
  'Graphene
  'GrapheneVec4
  'vec4:max!
  'max!
  'graphene_vec4_max
  '((name . "max")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:min!
  'min!
  'graphene_vec4_min
  '((name . "min")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:multiply!
  'multiply!
  'graphene_vec4_multiply
  '((name . "multiply")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:near?
  'near?
  'graphene_vec4_near
  '((name . "near")
    (is-method . #t)
    (s-input-req . 2)
    (c-input-len . 2)
    (pdata ((name . "v2")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "epsilon")
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
      (meta (arg-type . gboolean)
            (flags out)
            (item-size . 4)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  'GrapheneVec4
  'vec4:negate!
  'negate!
  'graphene_vec4_negate
  '((name . "negate")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:normalize!
  'normalize!
  'graphene_vec4_normalize
  '((name . "normalize")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:scale!
  'scale!
  'graphene_vec4_scale
  '((name . "scale")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
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
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:subtract!
  'subtract!
  'graphene_vec4_subtract
  '((name . "subtract")
    (is-method . #t)
    (s-input-req . 2)
    (s-output-len . 1)
    (c-input-len . 1)
    (c-output-len . 1)
    (pdata ((name . "b")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
                  (flags ptr in)
                  (item-size . 16)
                  (transfer . nothing))
            (s-direction . input)
            (tuple . singleton)
            (presence . required)
            (i . 0)
            (c-input-pos . 0)
            (s-input-pos . 0))
           ((name . "res")
            (meta (arg-type . GBoxed)
                  (gtype-name . "GrapheneVec4")
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
  'Graphene
  'GrapheneVec4
  'vec4:to-float!
  'to-float!
  'graphene_vec4_to_float
  '((name . "to_float")
    (is-method . #t)
    (s-input-req . 1)
    (s-output-len . 1)
    (c-output-len . 1)
    (pdata ((name . "dest")
            (meta (arg-type . array)
                  (flags ptr out preallocated fixed-size)
                  (fixed-size . 4)
                  (transfer . nothing)
                  (params
                    ((arg-type . float)
                     (item-size . 4)
                     (transfer . nothing))))
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
  'Graphene
  'GrapheneVec4
  'vec4:one
  'one
  'graphene_vec4_one
  '((name . "one")
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

(^function
  'Graphene
  'GrapheneVec4
  'vec4:w-axis
  'w-axis
  'graphene_vec4_w_axis
  '((name . "w_axis")
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

(^function
  'Graphene
  'GrapheneVec4
  'vec4:x-axis
  'x-axis
  'graphene_vec4_x_axis
  '((name . "x_axis")
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

(^function
  'Graphene
  'GrapheneVec4
  'vec4:y-axis
  'y-axis
  'graphene_vec4_y_axis
  '((name . "y_axis")
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

(^function
  'Graphene
  'GrapheneVec4
  'vec4:z-axis
  'z-axis
  'graphene_vec4_z_axis
  '((name . "z_axis")
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

(^function
  'Graphene
  'GrapheneVec4
  'vec4:zero
  'zero
  'graphene_vec4_zero
  '((name . "zero")
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

(^function
  'Graphene
  #f
  'graphene:box-empty
  'box-empty
  'graphene_box_empty
  '((name . "box_empty")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:box-infinite
  'box-infinite
  'graphene_box_infinite
  '((name . "box_infinite")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:box-minus-one
  'box-minus-one
  'graphene_box_minus_one
  '((name . "box_minus_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:box-one
  'box-one
  'graphene_box_one
  '((name . "box_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:box-one-minus-one
  'box-one-minus-one
  'graphene_box_one_minus_one
  '((name . "box_one_minus_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:box-zero
  'box-zero
  'graphene_box_zero
  '((name . "box_zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneBox")
            (flags ptr out)
            (item-size . 32)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:point3d-zero
  'point3d-zero
  'graphene_point3d_zero
  '((name . "point3d_zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GraphenePoint3D")
            (flags ptr out)
            (item-size . 12)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:point-zero
  'point-zero
  'graphene_point_zero
  '((name . "point_zero")
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
  'Graphene
  #f
  'graphene:rect-alloc
  'rect-alloc
  'graphene_rect_alloc
  '((name . "rect_alloc")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneRect")
            (flags ptr out)
            (item-size . 16)
            (transfer . everything))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:rect-zero
  'rect-zero
  'graphene_rect_zero
  '((name . "rect_zero")
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

(^function
  'Graphene
  #f
  'graphene:size-zero
  'size-zero
  'graphene_size_zero
  '((name . "size_zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneSize")
            (flags ptr out)
            (item-size . 8)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec2-one
  'vec2-one
  'graphene_vec2_one
  '((name . "vec2_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec2-x-axis
  'vec2-x-axis
  'graphene_vec2_x_axis
  '((name . "vec2_x_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec2-y-axis
  'vec2-y-axis
  'graphene_vec2_y_axis
  '((name . "vec2_y_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec2-zero
  'vec2-zero
  'graphene_vec2_zero
  '((name . "vec2_zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec2")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec3-one
  'vec3-one
  'graphene_vec3_one
  '((name . "vec3_one")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec3-x-axis
  'vec3-x-axis
  'graphene_vec3_x_axis
  '((name . "vec3_x_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec3-y-axis
  'vec3-y-axis
  'graphene_vec3_y_axis
  '((name . "vec3_y_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec3-z-axis
  'vec3-z-axis
  'graphene_vec3_z_axis
  '((name . "vec3_z_axis")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec3-zero
  'vec3-zero
  'graphene_vec3_zero
  '((name . "vec3_zero")
    (return-val
      (name . "%return")
      (meta (arg-type . GBoxed)
            (gtype-name . "GrapheneVec3")
            (flags ptr out)
            (item-size . 16)
            (transfer . nothing))
      (s-direction . output)
      (tuple . singleton)
      (presence . required)
      (i . 0))))

(^function
  'Graphene
  #f
  'graphene:vec4-one
  'vec4-one
  'graphene_vec4_one
  '((name . "vec4_one")
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

(^function
  'Graphene
  #f
  'graphene:vec4-w-axis
  'vec4-w-axis
  'graphene_vec4_w_axis
  '((name . "vec4_w_axis")
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

(^function
  'Graphene
  #f
  'graphene:vec4-x-axis
  'vec4-x-axis
  'graphene_vec4_x_axis
  '((name . "vec4_x_axis")
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

(^function
  'Graphene
  #f
  'graphene:vec4-y-axis
  'vec4-y-axis
  'graphene_vec4_y_axis
  '((name . "vec4_y_axis")
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

(^function
  'Graphene
  #f
  'graphene:vec4-z-axis
  'vec4-z-axis
  'graphene_vec4_z_axis
  '((name . "vec4_z_axis")
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

(^function
  'Graphene
  #f
  'graphene:vec4-zero
  'vec4-zero
  'graphene_vec4_zero
  '((name . "vec4_zero")
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


)
