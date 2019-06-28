#ifndef _GI_GIARGUMENT_H_
#define _GI_GIARGUMENT_H_

#include <girepository.h>
#include <libguile.h>

#define GIR_FREE_NONE 0x00000
#define GIR_FREE_SIMPLE 0x10000
#define GIR_FREE_STRV 0x20000
#define GIR_FREE_PTR_ARRAY 0x40000
#define GIR_FREE_PTR_COUNT(x) ((x)&GIR_FREE_PTR_ARRAY ? (x)&0xFFFF : 0)

void
gi_giargument_object_to_c_arg(char *subr, int argnum,
                              SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);

char *gi_giargument_describe_arg_in(GIArgInfo *arg_info);
void
gi_giargument_preallocate_output_arg_and_object(GIArgInfo *arg_info, GIArgument *arg, SCM *obj);
void gi_giargument_free_args(int n, unsigned *must_free, GIArgument *args);
void gi_giargument_convert_arg_to_object(GIArgument *arg, GIArgInfo *arg_info, SCM *obj);

SCM
gi_giargument_convert_return_val_to_object(GIArgument *arg,
                                           GITypeInfo *type_info,
                                           GITransfer transfer, gboolean null_ok, gboolean skip);
void
gi_giargument_convert_return_type_object_to_arg(SCM obj,
                                                GITypeInfo *type_info,
                                                GITransfer transfer, gboolean null_ok,
                                                gboolean skip, GIArgument *arg);

void gi_init_giargument(void);
#endif
