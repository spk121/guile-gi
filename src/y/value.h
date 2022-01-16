#ifndef Y_VALUE_H
#define Y_VALUE_H

#include <libguile.h>
#include <girepository.h>
#include "x.h"

SCM value_c2g(GValue *val);
SCM value_to_scm_basic_type(const GValue *value, GType fundamental, int *handled);
SCM value_param_as_scm(const GValue *gvalue, int copy_boxed, const GParamSpec *pspec);

SCM value_as_scm(const GValue *value, int copy_boxed);
SCM value_get(SCM value);
SCM value_get_type(SCM value);
SCM value_set(SCM where, SCM what);
SCM value_set_type(SCM where, SCM what);
SCM value_transform(SCM val, SCM type);

void value_from_scm_with_error(GValue *value, SCM obj, const char *subr, int pos);
int value_from_scm(GValue *value, SCM obj);

GIG_API void init_value(void);
#endif
