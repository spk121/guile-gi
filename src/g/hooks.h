#ifndef G_HOOKS_H
#define G_HOOKS_H

#include "../clib.h"

void run_before_callback_hook(SCM name, SCM func, SCM args);
void run_before_c_callback_hook(SCM name, SCM ptr, SCM args);
void run_before_function_hook(SCM name, SCM args);

GIG_API void init_core_hooks(void);

#endif
