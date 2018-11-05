#ifndef _GIR_CALLBACK_H
#define _GIR_CALLBACK_H
#include <glib.h>
#include <girepository.h>
#include <ffi.h>

extern SCM gir_callback_type;

typedef struct _GirCallback
{
    GICallbackInfo *callback_info;
    ffi_closure *closure;
    ffi_cif cif;
    SCM s_func;
    void *callback_ptr;
} GirCallback;

void gir_init_callback (void);
GirCallback *gir_callback_new(GICallbackInfo *callback_info, SCM s_func);
void *gir_callback_get_func (SCM s_gcb);

void gir_callback_free(GirCallback *callback);

#endif