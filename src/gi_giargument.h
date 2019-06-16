#ifndef _GI_GIARGUMENT_H_
#define _GI_GIARGUMENT_H_

#include <girepository.h>
#include <libguile.h>
#include "__gi_giargument.h"

typedef struct _GirVoidBox
{
    int header;
    int size;
    SCM body;
} GirVoidBox;

// Presume 32-bit max size.  Choose a value that is not a valid memory
// location and should segfault.
static const int GIR_VOIDBOX_HEADER = ('v' | 'b' << 8 | 'x' << 16 | 32 << 24);

#define GIR_FREE_NONE 0x00000
#define GIR_FREE_SIMPLE 0x10000
#define GIR_FREE_STRV 0x20000
#define GIR_FREE_PTR_ARRAY 0x40000
#define GIR_FREE_PTR_COUNT(x) ((x)&GIR_FREE_PTR_ARRAY ? (x)&0xFFFF : 0)

void
gi_giargument_object_to_c_arg(char *subr, int argnum,
                              SCM obj,
                              GIArgInfo *arg_info,
                              unsigned *must_free,
                              GIArgument *arg);

char *gi_giargument_describe_arg_in(GIArgInfo *arg_info);
void
gi_giargument_preallocate_output_arg_and_object(GIArgInfo *arg_info, GIArgument *arg, SCM *obj);
void
gi_giargument_free_args(int n, unsigned *must_free, GIArgument *args);
void
gi_giargument_convert_arg_to_object(GIArgument *arg, GIArgInfo *arg_info, SCM *obj);

SCM
gi_giargument_convert_return_val_to_object(GIArgument  *arg,
    GITypeInfo *type_info,
    GITransfer transfer, gboolean null_ok, gboolean skip);
void
gi_giargument_convert_return_type_object_to_arg(SCM obj,
    GITypeInfo *type_info,
    GITransfer transfer, gboolean null_ok, gboolean skip,
    GIArgument *arg);
#if 0
typedef gssize(*GuGIArgArrayLengthPolicy) (gsize item_index,
    void *user_data1,
    void *user_data2);

gboolean
gi_giargument_to_gssize(const char *func,
    GIArgument *arg_in,
    GITypeTag  type_tag,
    gssize *gssize_out);
gssize
gi_argument_array_length_marshal(gsize length_arg_index,
    void *user_data1,
    void *user_data2);
GArray *
gi_giargument_to_array(GIArgument  *arg,
    GuGIArgArrayLengthPolicy array_length_policy,
    void        *user_data1,
    void        *user_data2,
    GITypeInfo  *type_info,
    gboolean    *out_free_array);

GArray* _pygi_argument_to_array(GIArgument  *arg,
    GuGIArgArrayLengthPolicy array_length_policy,
    void        *user_data1,
    void        *user_data2,
    GITypeInfo  *type_info,
    gboolean    *out_free_array);

gboolean
gi_giargument_check_scm_type(SCM obj, GIArgInfo *ai, char **errstr);



void
gi_giargument_release(GIArgument   *arg,
    GITypeInfo  *type_info,
    GITransfer   transfer,
    GIDirection  direction);
GIArgument
gi_argument_from_object(const char *func,
    SCM object,
    GITypeInfo *type_info,
    GITransfer  transfer);
#endif

void gi_init_giargument(void);
#endif
