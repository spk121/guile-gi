#ifndef Y_ARGUMENT_H
#define Y_ARGUMENT_H

#include <girepository.h>
#include <libguile.h>
#include "x.h"
#include "y/arg_map.h"

#define S2C_ARG_DECL const char *subr, int argpos,    \
        Arg *meta, SCM object,               \
        slist_t **must_free, GIArgument *arg, size_t *size
#define S2C_ARGS subr, argpos, meta, object, must_free, arg, size

#define C2S_ARG_DECL const char *subr, int argpos,    \
        Arg *meta, GIArgument *arg,             \
        SCM *object, size_t size
#define C2S_ARGS subr, argpos, meta, arg, object, size

void argument_scm_to_c(S2C_ARG_DECL);
void argument_c_to_scm(C2S_ARG_DECL);
char *argument_describe_arg(GIArgInfo *arg_info);
char *argument_describe_return(GITypeInfo *type_info, GITransfer transfer, gboolean null_ok,
                                   gboolean skip);
#endif
