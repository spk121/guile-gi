#ifndef GI_FUNCTION_INFO_H
#define GI_FUNCTION_INFO_H
#include <girepository.h>
void gi_function_info_count_args(GIFunctionInfo *info, int *in, int *out);
G_GNUC_MALLOC gchar *gi_function_info_make_name(GIFunctionInfo *info, const gchar *prefix);
#endif
