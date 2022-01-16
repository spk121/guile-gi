#ifndef Y_CALLBACK_H
#define Y_CALLBACK_H

#include <girepository.h>
#include <libguile.h>

SCM callback_to_scm(const char *name, GICallbackInfo *info, void *proc);
void *callback_to_c(const char *name, GICallbackInfo *callback_info, SCM s_func);

GIG_API void nit_callback(void);
#endif
