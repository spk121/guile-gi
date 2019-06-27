#ifndef _GIR_UTIL_H_
#define _GIR_UTIL_H_

#include <glib.h>

const gchar *gi_constant_strip_prefix(const gchar *name, const gchar *strip_prefix);
char * gname_to_scm_name(const char *gname);
SCM scm_c_list_ref (SCM list, size_t k);
int scm_is_list (SCM obj);
void* scm_dynwind_or_bust (const char *subr, void *mem);

#define SCM_UNBND_TO_BOOL_F(obj) \
    do {                         \
        if (SCM_UNBNDP (obj))    \
            obj = SCM_BOOL_F;    \
    } while (0)                  \

#endif
