// Copyright (C) 2019, 2020, 2021, 2022 Michael L. Gran

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

#define _XOPEN_SOURCE 700       /* For strdup, strndup */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "util.h"

size_t
strvlen(const char **x)
{
    size_t i = 0;
    while (x[i] != NULL)
        i++;
    return i;
}

void *
xcalloc(size_t nmemb, size_t siz)
{
    void *x;
    x = calloc(nmemb, siz);
    if (x == 0) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

void *
xmalloc(size_t siz)
{
    void *x;
    x = malloc(siz);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

void *
xmemdup(const void *mem, size_t len)
{
    void *new_mem;

    new_mem = xmalloc(len);
    if (new_mem == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    memcpy(new_mem, mem, len);

    return new_mem;
}

void *
xrealloc(void *mem, size_t siz)
{
    void *x;
    x = realloc(mem, siz);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

char *
xstrdup(const char *S)
{
    char *x;
    if (S == NULL) {
        fprintf(stderr, "strdup null pointer error\n");
        exit(1);
    }
    x = strdup(S);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

char *
xstrndup(const char *S, size_t siz)
{
    char *x;
    if (siz == 0 || S == NULL) {
        fprintf(stderr, "strndup null exception\n");
        exit(1);
    }
    x = strndup(S, siz);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

void *
dynwind_free(void *mem)
{
    if (mem)
        scm_dynwind_free(mem);
    return mem;
}

////////////////////////////////////////////////////////////////
// NAMES

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
static void
count_args(GICallableInfo *info, int *in, int *out)
{
    // Count the number of required input arguments, and store
    // the arg info in a newly allocate array.
    int n_args = g_callable_info_get_n_args(info);
    int n_input_args = 0;
    int n_output_args = 0;

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(ai);
        g_base_info_unref(ai);

        if (dir == GI_DIRECTION_IN)
            n_input_args++;
        else if (dir == GI_DIRECTION_OUT)
            n_output_args++;
        else if (dir == GI_DIRECTION_INOUT) {
            n_input_args++;
            n_output_args++;
        }
    }
    *in = n_input_args;
    *out = n_output_args;
}


// Returns true if this function returns a single boolean.
static bool
is_predicate(GICallableInfo *info)
{
    bool predicate = false;
    GITypeInfo *return_type;

    if (GI_IS_SIGNAL_INFO(info))
        return false;

    return_type = g_callable_info_get_return_type(info);

    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type)) {
        int in, out;

        count_args(info, &in, &out);
        if (out == 0)
            predicate = true;
    }
    g_base_info_unref(return_type);
    return predicate;
}

static bool
is_destructive(GICallableInfo *info)
{
    bool destructive = false;
    int n_args = g_callable_info_get_n_args(info);

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GITypeInfo *ti = g_arg_info_get_type(ai);
        bool is_trivial;

        switch (g_type_info_get_tag(ti)) {
        case GI_TYPE_TAG_BOOLEAN:
        case GI_TYPE_TAG_DOUBLE:
        case GI_TYPE_TAG_FLOAT:
        case GI_TYPE_TAG_GTYPE:
        case GI_TYPE_TAG_INT8:
        case GI_TYPE_TAG_INT16:
        case GI_TYPE_TAG_INT32:
        case GI_TYPE_TAG_INT64:
        case GI_TYPE_TAG_UINT8:
        case GI_TYPE_TAG_UINT16:
        case GI_TYPE_TAG_UINT32:
        case GI_TYPE_TAG_UINT64:
        case GI_TYPE_TAG_UNICHAR:
            is_trivial = true;
            break;
        default:
            is_trivial = false;
        }
        g_base_info_unref(ti);

        if (!is_trivial) {
            destructive |= g_arg_info_is_caller_allocates(ai);
            destructive |= (g_arg_info_get_direction(ai) == GI_DIRECTION_INOUT);
        }
        g_base_info_unref(ai);
    }

    return destructive;
}

static char *
gname_to_scm_name(const char *gname)
{
    assert(gname != NULL);
    assert(strlen(gname) > 0);

    size_t len = strlen(gname);
    char *str = xmalloc(len * 2 + 1);
    bool was_lower = false;

    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        if (islower(gname[i])) {
            str[j++] = gname[i];
            was_lower = true;
        }
        else if (gname[i] == '_' || gname[i] == '-') {
            str[j++] = '-';
            was_lower = false;
        }
        else if (gname[i] == '?' || gname[i] == ':' || gname[i] == '%') {
            str[j++] = gname[i];
            was_lower = false;
        }
        else if (isdigit(gname[i])) {
            str[j++] = gname[i];
            was_lower = false;
        }
        else if (isupper(gname[i])) {
            if (was_lower)
                str[j++] = '-';
            str[j++] = tolower(gname[i]);
            was_lower = false;
        }
    }
    str[j++] = '\0';
    return xrealloc(str, j);
}

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
char *
callable_info_make_name(GICallableInfo *info, const char *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    bool predicate, destructive;

    predicate = is_predicate(info);
    destructive = is_destructive(info);
    if (prefix)
        str1 = gname_to_scm_name(prefix);
    str2 = gname_to_scm_name(g_base_info_get_name(info));

    int len = strlen(":!") + 1;
    if (str1)
        len += strlen(str1);
    if (str2)
        len += strlen(str2);
    name = malloc(len);

    if (!prefix) {
        if (destructive)
            snprintf(name, len, "%s!", str2);
        else if (predicate)
            snprintf(name, len, "%s?", str2);
        else
            snprintf(name, len, "%s", str2);
    }
    else {
        if (destructive)
            snprintf(name, len, "%s:%s!", str1, str2);
        else if (predicate)
            snprintf(name, len, "%s:%s?", str1, str2);
        else
            snprintf(name, len, "%s:%s", str1, str2);
    }
    free(str1);
    free(str2);
    return name;
}

////////////////////////////////////////////////////////////////
