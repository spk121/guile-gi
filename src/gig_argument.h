#ifndef GIG_GIARGUMENT_H
#define GIG_GIARGUMENT_H

#include <stdbool.h>
#include <girepository.h>
#include <libguile.h>
#include "gig_arg_map.h"
#include "gig_mem_list.h"

#define S2C_ARG_DECL const char *subr, int argpos,    \
        GigTypeMeta *meta, SCM object,               \
        GigMemList **must_free, GIArgument *arg, size_t *size
#define S2C_ARGS subr, argpos, meta, object, must_free, arg, size

#define C2S_ARG_DECL const char *subr, int argpos,    \
        GigTypeMeta *meta, GIArgument *arg,             \
        SCM *object, size_t size
#define C2S_ARGS subr, argpos, meta, arg, object, size

void gig_argument_scm_to_c(S2C_ARG_DECL);
void gig_argument_c_to_scm(C2S_ARG_DECL);
char *gig_argument_describe_arg(GIArgInfo *arg_info);
char *gig_argument_describe_return(GITypeInfo *type_info, GITransfer transfer, bool null_ok,
                                   bool skip);

void gig_init_argument(void);
#endif
