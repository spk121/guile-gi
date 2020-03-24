#ifndef GIG_VALUE_H_
#define GIG_VALUE_H_

#include <glib.h>
#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*


SCM gig_value_c2g(GValue *val);
SCM gig_value_to_scm_basic_type(const GValue *value, GType fundamental, gboolean *handled);
SCM gig_value_param_as_scm(const GValue *gvalue, gboolean copy_boxed, const GParamSpec *pspec);

SCM gig_value_as_scm(const GValue *value, gboolean copy_boxed);
SCM gig_value_get(SCM value);
SCM gig_value_get_type(SCM value);
SCM gig_value_set(SCM where, SCM what);
SCM gig_value_set_type(SCM where, SCM what);
SCM gig_value_transform(SCM val, SCM type);

void gig_value_from_scm_with_error(GValue *value, SCM obj, const gchar *subr, gint pos);
int gig_value_from_scm(GValue *value, SCM obj);

void gig_init_value(void);

G_END_DECLS
#endif
