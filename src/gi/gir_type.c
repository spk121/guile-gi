#include <libguile.h>
#include <girepository.h>
#include "gir_type.h"

SCM s_GIRepository_type;
SCM s_GITypelib_type;

static void
gir_init_GIRepository_type()
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  
  name = scm_from_utf8_symbol ("GIRepository");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  finalizer = NULL;
  s_GIRepository_type =
    scm_make_foreign_object_type (name, slots, finalizer);
}

static void
gir_init_GITypelib_type()
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  
  name = scm_from_utf8_symbol ("GITypelib");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  finalizer = NULL;
  s_GITypelib_type =
    scm_make_foreign_object_type (name, slots, finalizer);
}

#define GITYPE(NAME)				\
  SCM s_ ## NAME ## _type;			\
  static void					\
  gir_init_ ## NAME ## _type()			\
  {						    \
    SCM name = scm_from_utf8_symbol(#NAME);		    \
    SCM slots = scm_list_1 (scm_from_utf8_symbol ("data")); \
    s_ ## NAME ## _type =				    \
      scm_make_foreign_object_type (name, slots, NULL);	    \
  }

#undef GITYPEX
#define GITYPEX(NAME) \
  gir_init_ ## NAME ## _type()

GITYPE(GIArgInfo);
GITYPE(GICallbackInfo);
GITYPE(GIConstInfo);
GITYPE(GIConstantInfo);
GITYPE(GIEnumInfo);
GITYPE(GIFieldInfo);
GITYPE(GIFunctionInfo);
GITYPE(GIInterfaceInfo);
GITYPE(GIObjectInfo);
GITYPE(GIPropertyInfo);
GITYPE(GISignalInfo);
GITYPE(GIStructInfo);
GITYPE(GITypeInfo);
GITYPE(GIUnionInfo);
GITYPE(GIVFuncInfo);
GITYPE(GIValueInfo);

#define P(NAME) \
  SCM gir_ ## NAME ## _p(SCM x) \
  { return scm_from_bool (SCM_IS_A_P(x, s_ ## NAME ## _type));}
#define PX(FUNCNAME, NAME)				\
  scm_c_define_gsubr(FUNCNAME, 1, 0, 0, gir_ ## NAME ## _p)

P(GIArgInfo);
P(GICallbackInfo);
P(GIConstInfo);
P(GIConstantInfo);
P(GIEnumInfo);
P(GIFieldInfo);
P(GIFunctionInfo);
P(GIInterfaceInfo);
P(GIObjectInfo);
P(GIPropertyInfo);
P(GISignalInfo);
P(GIStructInfo);
P(GITypeInfo);
P(GIUnionInfo);
P(GIVFuncInfo);
P(GIValueInfo);


void
gir_init_types()
{
  gir_init_GIRepository_type();
  gir_init_GITypelib_type();

  GITYPEX(GIArgInfo);
  GITYPEX(GICallbackInfo);
  GITYPEX(GIConstInfo);
  GITYPEX(GIConstantInfo);
  GITYPEX(GIEnumInfo);
  GITYPEX(GIFieldInfo);
  GITYPEX(GIFunctionInfo);
  GITYPEX(GIInterfaceInfo);
  GITYPEX(GIObjectInfo);
  GITYPEX(GIPropertyInfo);
  GITYPEX(GISignalInfo);
  GITYPEX(GIStructInfo);
  GITYPEX(GITypeInfo);
  GITYPEX(GIUnionInfo);
  GITYPEX(GIVFuncInfo);
  GITYPEX(GIValueInfo);

  PX("arg-info?", GIArgInfo);
  PX("callback-info?", GICallbackInfo);
  PX("const-info?", GIConstInfo);
  PX("constant-info?", GIConstantInfo);
  PX("enum-info?", GIEnumInfo);
  PX("field-info?", GIFieldInfo);
  PX("function-info?", GIFunctionInfo);
  PX("interface-info?", GIInterfaceInfo);
  PX("object-info?", GIObjectInfo);
  PX("property-info?", GIPropertyInfo);
  PX("signal-info?", GISignalInfo);
  PX("struct-info?", GIStructInfo);
  PX("type-info?", GITypeInfo);
  PX("union-info?", GIUnionInfo);
  PX("value-info?", GIValueInfo);
  PX("vfunc-info?", GIVFuncInfo);
}


#if 0
     struct image {
       int width, height;
       char *pixels;

       /* The name of this image */
       SCM name;

       /* A function to call when this image is
          modified, e.g., to update the screen,
          or SCM_BOOL_F if no action necessary */
       SCM update_func;
     };

     static SCM image_type;

     void
     init_image_type (void)
     {
       SCM name, slots;
       scm_t_struct_finalize finalizer;

       name = scm_from_utf8_symbol ("image");
       slots = scm_list_1 (scm_from_utf8_symbol ("data"));
       finalizer = NULL;

       image_type =
         scm_make_foreign_object_type (name, slots, finalizer);
     }
     SCM
     make_image (SCM name, SCM s_width, SCM s_height)
     {
       struct image *image;
       int width = scm_to_int (s_width);
       int height = scm_to_int (s_height);

       /* Allocate the `struct image'.  Because we
          use scm_gc_malloc, this memory block will
          be automatically reclaimed when it becomes
          inaccessible, and its members will be traced
          by the garbage collector.  */
       image = (struct image *)
         scm_gc_malloc (sizeof (struct image), "image");

       image->width = width;
       image->height = height;

       /* Allocating the pixels with
          scm_gc_malloc_pointerless means that the
          pixels data is collectable by GC, but
          that GC shouldn't spend time tracing its
          contents for nested pointers because there
          aren't any.  */
       image->pixels =
         scm_gc_malloc_pointerless (width * height, "image pixels");

       image->name = name;
       image->update_func = SCM_BOOL_F;

       /* Now wrap the struct image* in a new foreign
          object, and return that object.  */
       return scm_make_foreign_object_1 (image_type, image);
     }
     SCM
     clear_image (SCM image_obj)
     {
       int area;
       struct image *image;

       scm_assert_foreign_object_type (image_type, image_obj);

       image = scm_foreign_object_ref (image_obj, 0);
       area = image->width * image->height;
       memset (image->pixels, 0, area);

       /* Invoke the image's update function.  */
       if (scm_is_true (image->update_func))
         scm_call_0 (image->update_func);

       return SCM_UNSPECIFIED;
     }

     static SCM file_type;

     static void
     finalize_file (SCM file)
     {
       int fd = scm_foreign_object_signed_ref (file, 0);
       if (fd >= 0)
         {
           scm_foreign_object_signed_set_x (file, 0, -1);
           close (fd);
         }
     }

     static void
     init_file_type (void)
     {
       SCM name, slots;
       scm_t_struct_finalize finalizer;

       name = scm_from_utf8_symbol ("file");
       slots = scm_list_1 (scm_from_utf8_symbol ("fd"));
       finalizer = finalize_file;

       image_type =
         scm_make_foreign_object_type (name, slots, finalizer);
     }

     static SCM
     make_file (int fd)
     {
       return scm_make_foreign_object_1 (file_type, (void *) fd);
     }
     SCM
     read_bytes (SCM file, SCM n)
     {
       int fd;
       SCM buf;
       size_t len, pos;

       scm_assert_foreign_object_type (file_type, file);

       fd = scm_foreign_object_signed_ref (file, 0);
       if (fd < 0)
         scm_wrong_type_arg_msg ("read-bytes", SCM_ARG1,
                                 file, "open file");

       len = scm_to_size_t (n);
       SCM buf = scm_c_make_bytevector (scm_to_size_t (n));

       pos = 0;
       while (pos < len)
         {
           char *bytes = SCM_BYTEVECTOR_CONTENTS (buf);
           ssize_t count = read (fd, bytes + pos, len - pos);
           if (count < 0)
             scm_syserror ("read-bytes");
           if (count == 0)
             break;
           pos += count;
         }

       scm_remember_upto_here_1 (file);

       return scm_values (scm_list_2 (buf, scm_from_size_t (pos)));
     }
#endif
