#ifndef _GI_GIARGUMENT_H_
#define _GI_GIARGUMENT_H_

#include <girepository.h>
#include <libguile.h>
#include "gig_arg_map.h"

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

#define GIR_FREE_NONE 0x00000
#define GIR_FREE_SIMPLE 0x10000
#define GIR_FREE_STRV 0x20000
#define GIR_FREE_PTR_ARRAY 0x40000
#define GIR_FREE_PTR_COUNT(x) ((x)&GIR_FREE_PTR_ARRAY ? (x)&0xFFFF : 0)

#define GIG_ARRAY_SIZE_UNKNOWN ((gsize)-1)
#define GIG_ARG_RETURN_VAL (-1)

#define S2C_ARG_DECL const gchar *subr, gint argpos,    \
        GigArgMapEntry *entry, SCM object,               \
        guint *must_free, GIArgument *arg, gsize *size
#define S2C_ARGS subr, argpos, entry, object, must_free, arg, size

#define C2S_ARG_DECL const gchar *subr, gint argpos,    \
        GigArgMapEntry *entry, GIArgument *arg,       \
        guint *must_free, SCM *object, gsize size
#define C2S_ARGS subr, argpos, entry, arg, must_free, object, size

typedef void (*GigArgumentScmToC)(S2C_ARG_DECL);

void gig_argument_scm_to_c(S2C_ARG_DECL);
void gig_argument_c_to_scm(C2S_ARG_DECL);
char *gi_giargument_describe_arg(GIArgInfo *arg_info);
char *gi_giargument_describe_return(GITypeInfo *type_info, GITransfer transfer, gboolean null_ok,
                                    gboolean skip);
void gi_giargument_preallocate_output_arg_and_object(GIArgInfo *arg_info, GIArgument *arg,
                                                     SCM *obj);
void gi_giargument_free_args(int n, unsigned *must_free, GIArgument *args);

void gi_init_giargument(void);

G_END_DECLS
#endif
