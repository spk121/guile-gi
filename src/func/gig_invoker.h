#ifndef GIG_INVOKER_H
#define GIG_INVOKER_H

#include "../func.h"
#include "gig_arg_map.h"

_Bool gig_invoke_func(void *address, GigArgMap *amap, const GIArgument *in_args, int n_in_args,
                      const GIArgument *out_args, int n_out_args,
                      GIArgument *return_value, GError **error);
GType gig_invoke_get_type_func(void *address);
#endif
