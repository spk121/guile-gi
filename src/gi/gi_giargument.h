#ifndef _GI_GIARGUMENT_H_
#define _GI_GIARGUMENT_H_

#include <girepository.h>
#include <libguile.h>
#include "__gi_giargument.h"

typedef enum _GIArgumentStatus {
    GI_GIARGUMENT_OK,
	GI_GIARGUMENT_OUT_OF_RANGE,
    GI_GIARGUMENT_NON_CONST_VOID_POINTER,
    GI_GIARGUMENT_ARRAY_ELEMENT_TOO_BIG,
    GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE,
    GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE_CONVERSION,
    GI_GIARGUMENT_UNHANDLED_INTERFACE_TYPE,
    GI_GIARGUMENT_UNHANDLED_FOREIGN_TYPE,
    GI_GIARGUMENT_UNHANDLED_IMMEDIATE_TYPE,
    GI_GIARGUMENT_UNHANDLED_STRING_TYPE,
    GI_GIARGUMENT_UNHANDLED_TYPE,
    GI_GIARGUMENT_VOID,
    GI_GIARGUMENT_WRONG_TYPE_ARG,
    GI_GIARGUMENT_ERROR,
    GI_GIARGUMENT_TOO_FEW_ARGUMENTS,
    GI_GIARGUMENT_CALLBACK_HAS_WRONG_ARITY,
    GI_GIARGUMENT_N_ERRORS
} GIArgumentStatus;

const static char gi_giargument_error_messages[GI_GIARGUMENT_N_ERRORS][80] = {
    [GI_GIARGUMENT_OK] = "Conversion successful",
	[GI_GIARGUMENT_OUT_OF_RANGE] = "Argument out of range",
    [GI_GIARGUMENT_NON_CONST_VOID_POINTER] = "Cannot convert a non-const void pointer",
    [GI_GIARGUMENT_ARRAY_ELEMENT_TOO_BIG] = "The array element size is too big",
    [GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE] = "Cannot pack/unpack arrays of this element type",
    [GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE_CONVERSION] = "Cannot pack/unpack this type of scheme object to array elements of this type",
    [GI_GIARGUMENT_UNHANDLED_INTERFACE_TYPE] = "Cannot handle interfaces of this type",
    [GI_GIARGUMENT_UNHANDLED_FOREIGN_TYPE] = "Cannot handle non-GObject interfaces of this type",
    [GI_GIARGUMENT_UNHANDLED_IMMEDIATE_TYPE] = "Cannot handle immediate objects of this type",
    [GI_GIARGUMENT_UNHANDLED_STRING_TYPE] = "Unknown string type",
    [GI_GIARGUMENT_UNHANDLED_TYPE] = "Cannot handle this type",
    [GI_GIARGUMENT_VOID] = "Non-pointer void arguments cannot be unpacked",
    [GI_GIARGUMENT_WRONG_TYPE_ARG] = "Cannot pack/unpack this type of scheme object for this argument type",
    [GI_GIARGUMENT_ERROR] = "GIArgument conversion error",
    [GI_GIARGUMENT_CALLBACK_HAS_WRONG_ARITY] = "Callback procedure takes wrong number of arguments",
    [GI_GIARGUMENT_TOO_FEW_ARGUMENTS] = "Too few arguments",
};

#define GIR_FREE_NONE 0x00000
#define GIR_FREE_SIMPLE 0x10000
#define GIR_FREE_STRV 0x20000
#define GIR_FREE_PTR_ARRAY 0x40000
#define GIR_FREE_PTR_COUNT(x) ((x)&GIR_FREE_PTR_ARRAY ? (x)&0xFFFF : 0)

GIArgumentStatus
gi_giargument_convert_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);
GIArgumentStatus
gi_giargument_preallocate_output_arg_and_object(GIArgInfo *arg_info, GIArgument *arg, SCM *obj);
void
gi_giargument_free_args(int n, unsigned *must_free, GIArgument *args);
GIArgumentStatus
gi_giargument_convert_arg_to_object(GIArgument *arg, GIArgInfo *arg_info, SCM *obj);

SCM
gi_giargument_convert_return_val_to_object (GIArgument  *arg,
			 GITypeInfo *type_info,
			 GITransfer transfer, gboolean null_ok, gboolean skip);
GIArgumentStatus
gi_giargument_convert_return_type_object_to_arg(SCM obj,
             GITypeInfo *type_info,
             GITransfer transfer, gboolean null_ok, gboolean skip,
             GIArgument *arg);             
#if 0
typedef gssize (*GuGIArgArrayLengthPolicy) (gsize item_index,
					    void *user_data1,
					    void *user_data2);

gboolean
gi_giargument_to_gssize (const char *func,
			 GIArgument *arg_in,
                         GITypeTag  type_tag,
                         gssize *gssize_out);
gssize
gi_argument_array_length_marshal (gsize length_arg_index,
				  void *user_data1,
				  void *user_data2);
GArray *
gi_giargument_to_array (GIArgument  *arg,
		      GuGIArgArrayLengthPolicy array_length_policy,
		      void        *user_data1,
		      void        *user_data2,
		      GITypeInfo  *type_info,
			gboolean    *out_free_array);

GArray* _pygi_argument_to_array (GIArgument  *arg,
                                 GuGIArgArrayLengthPolicy array_length_policy,
                                 void        *user_data1,
                                 void        *user_data2,
                                 GITypeInfo  *type_info,
                                 gboolean    *out_free_array);

gboolean
gi_giargument_check_scm_type(SCM obj, GIArgInfo *ai, char **errstr);



void
gi_giargument_release (GIArgument   *arg,
                        GITypeInfo  *type_info,
                        GITransfer   transfer,
		       GIDirection  direction);
GIArgument
gi_argument_from_object (const char *func,
			 SCM object,
			 GITypeInfo *type_info,
			 GITransfer  transfer);
#endif

void gi_init_giargument (void);
#endif
