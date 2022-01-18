#ifndef Y_FUNCTION_ARGS_H
#define Y_FUNCTION_ARGS_H
#include <stdbool.h>
#include <glib-object.h>
#include <libguile.h>
#include <girepository.h>
#include "x.h"
#include "y/arg_map.h"

typedef struct _Args_store
{
    GIArgument *in;
    size_t in_len;
    GIArgument *out;
    GIArgument *boxed_out;
    size_t out_len;
    slist_t *free_list;
} Args_store;

void
initialize_args_store(Args_store *store, GObject *self, Arg_map *amap, SCM args,
                          const char *name);
SCM get_return_value(Args_store *store, GObject *self, Arg_map *amap, SCM args,
                     const char *name, GIArgument *return_arg, bool ok);
#endif
