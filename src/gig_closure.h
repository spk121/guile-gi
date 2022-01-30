#ifndef GIG_CLOSURE_H
#define GIG_CLOSURE_H
#include <girepository.h>
#include <libguile.h>
#include "core.h"

GClosure *gig_closure_new(SCM callback, SCM inout_mask);

GIG_API void gig_init_closure(void);

#endif
