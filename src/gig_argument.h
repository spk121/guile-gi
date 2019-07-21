#ifndef GIG_GIARGUMENT_H
#define GIG_GIARGUMENT_H

#include <girepository.h>
#include <libguile.h>
#include "gig_arg_map.h"

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

#define GIG_FREE_NONE 0x00000
#define GIG_FREE_SIMPLE 0x10000
#define GIG_FREE_STRV 0x20000
#define GIG_FREE_PTR_ARRAY 0x40000
#define GIG_FREE_PTR_COUNT(x) ((x)&GIG_FREE_PTR_ARRAY ? (x)&0xFFFF : 0)

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

void gig_argument_scm_to_c(S2C_ARG_DECL);
void gig_argument_c_to_scm(C2S_ARG_DECL);
char *gig_argument_describe_arg(GIArgInfo *arg_info);
char *gig_argument_describe_return(GITypeInfo *type_info, GITransfer transfer, gboolean null_ok,
                                   gboolean skip);
void gig_argument_preallocate_output_arg_and_object(GIArgInfo *arg_info, GIArgument *arg,
                                                    SCM *obj);
void gig_argument_free_args(gint n, guint *must_free, GIArgument *args);

void gig_init_argument(void);

G_END_DECLS
#endif
