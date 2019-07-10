#ifndef GIR_CALLBACK_H
#define GIR_CALLBACK_H
#include <glib.h>
#include <girepository.h>
#include <ffi.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

void gir_init_callback(void);
void *gir_callback_get_ptr(GICallbackInfo *callback_info, SCM s_func);

G_END_DECLS
#endif
