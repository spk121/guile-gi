#ifndef X_STRING_H
#define X_STRING_H

char *bracketize(const char *str);
char *concatenate(const char *str1, const char *str2);
char *concatenate3(const char *str1, const char *str2, const char *str3);
char *concatenate4(const char *str1, const char *str2, const char *str3, const char *str4);
char *decorate_string(const char *fmt, const char *str);
char *make_scm_name(const char *gname);
const char *skip_prefix(const char *name, const char *prefix);
size_t strvlen(const char **x);

#endif
