#ifndef GIG_UTIL_H
#define GIG_UTIL_H

#include <glib.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

const gchar *gig_constant_strip_prefix(const gchar *name, const gchar *strip_prefix);
gchar *gig_gname_to_scm_name(const gchar *gname);
SCM scm_c_list_ref(SCM list, gsize k);
gboolean scm_is_list(SCM obj);
gpointer scm_dynwind_or_bust(const gchar *subr, gpointer mem);
SCM scm_class_ref(SCM cls, SCM slot);
SCM scm_class_set_x(SCM cls, SCM slot, SCM val);
SCM scm_drop_right_1(SCM lst);
SCM scm_c_reexport(const gchar *name, ...);

#define SCM_UNBND_TO_BOOL_F(obj) \
    do {                         \
        if (SCM_UNBNDP (obj))    \
            obj = SCM_BOOL_F;    \
    } while (0)                  \

G_END_DECLS
#endif
