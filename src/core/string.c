#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "string.h"

char *
bracketize(const char *str)
{
    size_t len = strlen("<>") + strlen(str) + 1;
    char *str2 = malloc(len);
    if (str2 == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    snprintf(str2, len, "<%s>", str);
    return str2;
}

char *
concatenate(const char *str1, const char *str2)
{
    size_t len = strlen(str1) + strlen(str2) + 1;
    char *str = malloc(len);
    if (str == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    snprintf(str, len, "%s%s", str1, str2);
    return str;
}

char *
concatenate3(const char *str1, const char *str2, const char *str3)
{
    size_t len = strlen(str1) + strlen(str2) + strlen(str3) + 1;
    char *str = malloc(len);
    if (str == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    snprintf(str, len, "%s%s%s", str1, str2, str3);
    return str;
}

char *
decorate_string(const char *fmt, const char *str)
{
    int len = snprintf(NULL, 0, fmt, str) + 1;
    char *out = malloc(len);
    if (out == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    snprintf(out, len, fmt, str);
    return out;
}

char *
g_name_to_scm_name(const char *gname)
{
    assert(gname != NULL);
    assert(strlen(gname) > 0);

    size_t len = strlen(gname);
    char *str = malloc(2 * len + 1);
    if (str == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    int was_lower = 0;

    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        if (islower(gname[i])) {
            str[j++] = gname[i];
            was_lower = 1;
        }
        else if (gname[i] == '_' || gname[i] == '-') {
            str[j++] = '-';
            was_lower = 0;
        }
        else if (gname[i] == '?' || gname[i] == ':' || gname[i] == '%') {
            str[j++] = gname[i];
            was_lower = 0;
        }
        else if (isdigit(gname[i])) {
            str[j++] = gname[i];
            was_lower = 0;
        }
        else if (isupper(gname[i])) {
            if (was_lower)
                str[j++] = '-';
            str[j++] = tolower(gname[i]);
            was_lower = 0;
        }
    }
    str[j] = '\0';
    return str;
}

const char *
skip_prefix(const char *name, const char *prefix)
{
    size_t prefix_len, i;

    prefix_len = strlen(prefix);

    /* Check if name starts with prefix, if it doesn't:
     * return the rest of the part which doesn't match
     */
    for (i = 0; i < prefix_len; i++) {
        if (name[i] != prefix[i] && name[i] != '_') {
            return &name[i];
        }
    }

    /* strip off prefix from value name, while keeping it a valid
     * identifier */
    for (i = prefix_len + 1; i > 0; i--) {
        if (isalpha(name[i - 1]) || name[i - 1] == '_') {
            return &name[i - 1];
        }
    }
    return name;
}

size_t
strvlen(const char **x)
{
    size_t i = 0;
    while (x[i] != NULL)
        i++;
    return i;
}
