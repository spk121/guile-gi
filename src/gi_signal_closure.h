#ifndef _GI_SIGNAL_CLOSURE_H_
#define _GI_SIGNAL_CLOSURE_H_
#include <girepository.h>
#include <libguile.h>
GClosure *gi_signal_closure_new(SCM instance, GType g_type, const gchar *signal_name,
                                SCM callback);

#endif
