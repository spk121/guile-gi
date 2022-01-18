#ifndef Y_CALLBACK_H
#define Y_CALLBACK_H

#include <girepository.h>
#include <libguile.h>
#include "x.h"

SCM callback_to_scm(const char *name, GICallbackInfo *info, void *proc);
void *callback_to_c(const char *name, GICallbackInfo *callback_info, SCM s_func);

GIG_API void init_callback(void);
#endif
