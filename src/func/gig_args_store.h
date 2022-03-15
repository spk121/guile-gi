#ifndef GIG_ARGS_STORE_H
#define GIG_ARGS_STORE_H
#include <stdbool.h>
#include <libguile.h>
#include "../core.h"
#include "gig_arg_map.h"

typedef struct GigArgsStore_
{
    GIArgument *in_args;
    size_t in_len;
    GIArgument *out_args;
    GIArgument *out_boxes;
    size_t out_len;
    slist_t *free_list;
} GigArgsStore;

GigArgsStore *gig_args_store_new(size_t in_len, size_t out_len);
void gig_args_store_free(GigArgsStore *store);
void gig_args_store_initialize(GigArgsStore *store, GigArgMap *amap, const char *name,
                               GObject *self, SCM args);
SCM gig_args_store_return_value(GigArgsStore *store, GigArgMap *amap, const char *name,
                                GObject *self, SCM args, bool ok, GIArgument *return_arg);

#endif
