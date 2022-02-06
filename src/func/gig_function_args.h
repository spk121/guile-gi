#ifndef GIG_FUNCTION_ARGS_H
#define GIG_FUNCTION_ARGS_H
#include <stdbool.h>
#include <glib-object.h>
#include <libguile.h>
#include <girepository.h>
#include "core.h"
#include "gig_argument.h"
#include "gig_arg_map.h"

typedef struct _GigArgsStore
{
    GIArgument *in;
    size_t in_len;
    GIArgument *out;
    GIArgument *boxed_out;
    size_t out_len;
    slist_t *free_list;
} GigArgsStore;

void
gig_args_store_initialize(GigArgsStore *store, GObject *self, GigArgMap *amap, SCM args,
                          const char *name);
SCM gig_args_store_return_value(GigArgsStore *store, GObject *self, GigArgMap *amap, SCM args,
                                const char *name, GIArgument *return_arg, bool ok);
#endif
