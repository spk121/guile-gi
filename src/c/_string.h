#ifndef X_STRING_H
#define X_STRING_H
#include <stddef.h>

char *bracketize(const char *str);
char *concatenate(const char *str1, const char *str2);
char *concatenate3(const char *str1, const char *str2, const char *str3);
char *decorate_string(const char *fmt, const char *str);
char *g_name_to_scm_name(const char *gname);
const char *skip_prefix(const char *name, const char *prefix);
size_t strvlen(const char **x);

#endif
