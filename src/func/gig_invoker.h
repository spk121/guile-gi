#ifndef GIG_INVOKER_H
#define GIG_INVOKER_H

#include "../func.h"
#include "gig_arg_map.h"

_Bool gig_invoke_func(void *function, GigArgMap *amap, const GigArgument *in_args, int n_in_args,
                      const GigArgument *out_args, int n_out_args,
                      GigArgument *return_value, GError **error);
#endif
