#ifndef CORE_ARGUMENT_H
#define CORE_ARGUMENT_H

#include <stdbool.h>
#include <girepository.h>
#include <libguile.h>
#include "arg_map.h"
#include "mem_list.h"

#define S2C_ARG_DECL const char *subr, int argpos,    \
        MetaType *meta, SCM object,               \
        MemList **must_free, GIArgument *arg, size_t *size
#define S2C_ARGS subr, argpos, meta, object, must_free, arg, size

#define C2S_ARG_DECL const char *subr, int argpos,    \
        MetaType *meta, GIArgument *arg,             \
        SCM *object, size_t size
#define C2S_ARGS subr, argpos, meta, arg, object, size

void argument_scm_to_c(S2C_ARG_DECL);
void argument_c_to_scm(C2S_ARG_DECL);
#endif
