#ifndef GIG_CALLBACK_H
#define GIG_CALLBACK_H

#include <glib.h>
#include <girepository.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

gpointer gig_callback_get_ptr(GICallbackInfo *callback_info, SCM s_func);
void gig_init_callback(void);

G_END_DECLS
#endif
