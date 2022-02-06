#ifndef GIG_VALUE_H_
#define GIG_VALUE_H_

#include <libguile.h>
#include <girepository.h>
#include "core.h"
#include "gig_types.h"


SCM gig_value_c2g(GValue *val);
SCM gig_value_to_scm_basic_type(GValue *value, GType fundamental, int *handled);
SCM gig_value_param_as_scm(GValue *gvalue, int copy_boxed, GParamSpec *pspec);

SCM gig_value_as_scm(GValue *value, int copy_boxed);
SCM gig_value_get(SCM value);
SCM gig_value_get_type(SCM value);
SCM gig_value_set(SCM where, SCM what);
SCM gig_value_set_type(SCM where, SCM what);
SCM gig_value_transform(SCM val, SCM type);

void gig_value_from_scm_with_error(GValue *value, SCM obj, const char *subr, int pos);
int gig_value_from_scm(GValue *value, SCM obj);

GIG_API void gig_init_value(void);
#endif
