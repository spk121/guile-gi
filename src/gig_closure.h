#ifndef GIG_CLOSURE_H
#define GIG_CLOSURE_H
#include <girepository.h>
#include <libguile.h>

GClosure *gig_closure_new(SCM callback, SCM inout_mask);
void gig_init_closure(void);

#endif
