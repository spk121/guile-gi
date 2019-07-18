#ifndef GI_CALLABLE_INFO_H
#define GI_CALLABLE_INFO_H

#include <girepository.h>
#include <glib.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

void gi_callable_info_count_args(GICallableInfo *info, gint *in, gint *out);
G_GNUC_MALLOC gchar *gi_callable_info_make_name(GICallableInfo *info, const gchar *prefix);

G_END_DECLS
#endif
