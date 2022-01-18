#ifndef Y_UTIL_H
#define Y_UTIL_H
#include <girepository.h>
char *make_callable_name(GICallableInfo *info, const char *prefix);
char *
g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info);
char *
make_callable_name(GICallableInfo *info, const char *prefix);

#endif
