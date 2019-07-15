#ifndef GIG_SIGNAL_CLOSURE_H
#define GIG_SIGNAL_CLOSURE_H
#include <girepository.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

GClosure *gig_signal_closure_new(SCM instance, GType g_type, const gchar *signal_name,
                                 SCM callback);
G_END_DECLS
#endif
