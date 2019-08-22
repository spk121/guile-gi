#ifndef GIG_CALLBACK_H
#define GIG_CALLBACK_H

#include <glib.h>
#include <girepository.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

SCM gig_callback_to_scm(GICallbackInfo *info, gpointer proc);
gpointer gig_callback_to_c(GICallbackInfo *callback_info, SCM s_func);
void gig_init_callback(void);

G_END_DECLS
#endif
