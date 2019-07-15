#ifndef GI_FUNCTION_INFO_H
#define GI_FUNCTION_INFO_H

#include <girepository.h>
#include <glib.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

void gi_function_info_count_args(GIFunctionInfo *info, gint *in, gint *out);
G_GNUC_MALLOC gchar *gi_function_info_make_name(GIFunctionInfo *info, const gchar *prefix);

G_END_DECLS
#endif
