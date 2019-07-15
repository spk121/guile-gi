#ifndef _GI_GVALUE_H_
#define _GI_GVALUE_H_

#include <glib.h>
#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>

SCM gi_gvalue_to_scm_basic_type(const GValue *value, GType fundamental, gboolean *handled);
SCM gi_param_gvalue_as_scm(const GValue *gvalue, gboolean copy_boxed, const GParamSpec *pspec);

SCM gi_gvalue_as_scm(const GValue *value, gboolean copy_boxed);
void gi_gvalue_from_scm_with_error(const char *subr, GValue *value, SCM obj, int pos);
int gi_gvalue_from_scm(GValue *value, SCM obj);
GIArgument gi_giargument_from_g_value(const GValue *value, GITypeInfo *type_info);
#endif
