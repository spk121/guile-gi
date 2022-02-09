#ifndef GIG_CALLBACK_H
#define GIG_CALLBACK_H

#include "core.h"
#include <glib.h>
#include <girepository.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

SCM gig_callback_to_scm(const char *name, GICallbackInfo *info, void *proc);
void *gig_callback_to_c(const char *name, GICallbackInfo *callback_info, SCM s_func);
GIG_API void gig_init_callback(void);

G_END_DECLS
#endif
