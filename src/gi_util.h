#ifndef _GIR_UTIL_H_
#define _GIR_UTIL_H_

#include <glib.h>

const gchar *gi_constant_strip_prefix(const gchar *name, const gchar *strip_prefix);
char * gname_to_scm_name(const char *gname);
SCM scm_c_list_ref (SCM list, size_t k);

#endif
