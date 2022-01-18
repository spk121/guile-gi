#ifndef Y_CLOSURE_H
#define Y_CLOSURE_H
#include <girepository.h>
#include <libguile.h>
#include "x.h"

GClosure *closure_new(SCM callback, SCM inout_mask);

GIG_API void init_closure(void);

#endif
