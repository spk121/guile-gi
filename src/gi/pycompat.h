/* Python compatibility functions. */
#pragma once
#include <libguile.h>
#include <glib.h>

G_BEGIN_DECLS

extern SCM scm_none;

int scm_is_none (SCM x);
void GuModule_AddIntConstant (SCM module, const char *name, long value);
int GuArg_ParseTuple(SCM args, const char *format, ...);
int GuArg_VaParse(SCM args, const char *format, va_list vargs);

void init_pycompat (void);

G_END_DECLS

