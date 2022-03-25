// Copyright (C) 2022 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#define _GNU_SOURCE
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "string.h"

char *
bracketize(const char *str)
{
    if (str == NULL || strlen(str) == 0)
        return NULL;
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
    char *str;
    if (str1 == NULL && str2 == NULL)
        return NULL;
    if (str1 == NULL && str2 != NULL) {
        str = strdup(str2);
        if (str == NULL) {
            fprintf(stderr, "Out of memory\n");
            exit(1);
        }
        return str;
    }
    if (str1 != NULL & str2 == NULL) {
        str = strdup(str1);
        if (str == NULL) {
            fprintf(stderr, "Out of memory\n");
            exit(1);
        }
        return str;
    }

    size_t len = strlen(str1) + strlen(str2) + 1;
    str = malloc(len);
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
    if (str1 == NULL)
        return concatenate(str2, str3);
    else if (str2 == NULL)
        return concatenate(str1, str3);
    else if (str3 == NULL)
        return concatenate(str1, str2);

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
concatenate4(const char *str1, const char *str2, const char *str3, const char *str4)
{
    if (str1 == NULL)
        return concatenate3(str2, str3, str4);
    else if (str2 == NULL)
        return concatenate3(str1, str3, str4);
    else if (str3 == NULL)
        return concatenate3(str1, str2, str4);
    else if (str4 == NULL)
        return concatenate3(str1, str2, str3);


    size_t len = strlen(str1) + strlen(str2) + strlen(str3) + strlen(str4) + 1;
    char *str = malloc(len);
    if (str == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    snprintf(str, len, "%s%s%s%s", str1, str2, str3, str4);
    return str;
}

char *
decorate_string(const char *fmt, const char *str)
{
    if (fmt == NULL || str == NULL)
        return NULL;

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
make_scm_name(const char *gname)
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
