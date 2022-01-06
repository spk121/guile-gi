#ifndef GIG_CALLBACK_H
#define GIG_CALLBACK_H

#include <girepository.h>
#include <libguile.h>
#include "gig_visibility.h"

SCM gig_callback_to_scm(const char *name, GICallbackInfo *info, void *proc);
void *gig_callback_to_c(const char *name, GICallbackInfo *callback_info, SCM s_func);

GIG_API void gig_init_callback(void);
#endif
