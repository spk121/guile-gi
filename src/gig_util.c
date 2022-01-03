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
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gig_util.h"

static gboolean is_predicate(GIFunctionInfo *info);
static void count_args(GICallableInfo *info, gint *in, gint *out);

void *
xcalloc(size_t nmemb, size_t siz)
{
    void *x;
    if (nmemb == 0  || siz == 0)
        return NULL;
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
    if (siz == 0) {
        fprintf(stderr, "malloc zero size error\n");
        exit(1);
    }
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

    if (mem == NULL || len == 0) {
        fprintf(stderr, "memdup zero size error\n");
        exit(1);
    }
    new_mem = malloc(len);
    if (new_mem == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    memcpy(new_mem, mem, len);

    return new_mem;
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

// Returns TRUE if this function returns a single boolean.
static gboolean
is_predicate(GICallableInfo *info)
{
    gboolean predicate = FALSE;
    GITypeInfo *return_type;

    if (GI_IS_SIGNAL_INFO(info))
        return FALSE;

    return_type = g_callable_info_get_return_type(info);

    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type)) {
        gint in, out;

        count_args(info, &in, &out);
        if (out == 0)
            predicate = TRUE;
    }
    g_base_info_unref(return_type);
    return predicate;
}

static gboolean
is_destructive(GICallableInfo *info)
{
    gboolean destructive = FALSE;
    gint n_args = g_callable_info_get_n_args(info);

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GITypeInfo *ti = g_arg_info_get_type(ai);
        gboolean is_trivial;

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
            is_trivial = TRUE;
            break;
        default:
            is_trivial = FALSE;
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

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
static void
count_args(GICallableInfo *info, gint *in, gint *out)
{
    // Count the number of required input arguments, and store
    // the arg info in a newly allocate array.
    gint n_args = g_callable_info_get_n_args(info);
    gint n_input_args = 0;
    gint n_output_args = 0;

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

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
gchar *
gig_callable_info_make_name(GICallableInfo *info, const gchar *prefix)
{
    gchar *name, *str1 = NULL, *str2 = NULL;
    gboolean predicate, destructive;

    predicate = is_predicate(info);
    destructive = is_destructive(info);
    if (prefix)
        str1 = gig_gname_to_scm_name(prefix);
    str2 = gig_gname_to_scm_name(g_base_info_get_name(info));
    if (!prefix) {
        if (destructive)
            name = g_strdup_printf("%s!", str2);
        else if (predicate)
            name = g_strdup_printf("%s?", str2);
        else
            return str2;
    }
    else {
        if (destructive)
            name = g_strdup_printf("%s:%s!", str1, str2);
        else if (predicate)
            name = g_strdup_printf("%s:%s?", str1, str2);
        else
            name = g_strdup_printf("%s:%s", str1, str2);
    }
    free(str1);
    free(str2);
    return name;
}

/**
 * gig_constant_strip_prefix:
 * @name: the constant name.
 * @strip_prefix: the prefix to strip.
 *
 * Advances the pointer @name by strlen(@strip_prefix) characters.  If
 * the resulting name does not start with a letter or underscore, the
 * @name pointer will be rewound.  This is to ensure that the
 * resulting name is a valid identifier.  Hence the returned string is
 * a pointer into the string @name.
 *
 * Returns: the stripped constant name.
 */
const gchar *
gig_constant_strip_prefix(const gchar *name, const gchar *strip_prefix)
{
    gsize prefix_len, i;

    prefix_len = strlen(strip_prefix);

    /* Check so name starts with strip_prefix, if it doesn't:
     * return the rest of the part which doesn't match
     */
    for (i = 0; i < prefix_len; i++) {
        if (name[i] != strip_prefix[i] && name[i] != '_') {
            return &name[i];
        }
    }

    /* strip off prefix from value name, while keeping it a valid
     * identifier */
    for (i = prefix_len + 1; i > 0; i--) {
        if (g_ascii_isalpha(name[i - 1]) || name[i - 1] == '_') {
            return &name[i - 1];
        }
    }
    return name;
}

gchar *
gig_gname_to_scm_name(const gchar *gname)
{
    g_assert(gname != NULL);
    g_assert(strlen(gname) > 0);

    gsize len = strlen(gname);
    GString *str = g_string_new(NULL);
    gboolean was_lower = FALSE;

    for (gsize i = 0; i < len; i++) {
        if (g_ascii_islower(gname[i])) {
            g_string_append_c(str, gname[i]);
            was_lower = TRUE;
        }
        else if (gname[i] == '_' || gname[i] == '-') {
            g_string_append_c(str, '-');
            was_lower = FALSE;
        }
        else if (gname[i] == '?' || gname[i] == ':' || gname[i] == '%') {
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
        else if (g_ascii_isdigit(gname[i])) {
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
        else if (g_ascii_isupper(gname[i])) {
            if (was_lower)
                g_string_append_c(str, '-');
            g_string_append_c(str, g_ascii_tolower(gname[i]));
            was_lower = FALSE;
        }
    }
    return g_string_free(str, FALSE);
}

SCM
scm_c_list_ref(SCM list, gsize k)
{
    return scm_list_ref(list, scm_from_size_t(k));
}

gsize
scm_c_length(SCM list)
{
    return scm_to_size_t(scm_length(list));
}

int
scm_is_list(SCM obj)
{
    return scm_is_true(scm_list_p(obj));
}

gpointer
scm_dynwind_or_bust(const gchar *subr, gpointer mem)
{
    if (mem)
        scm_dynwind_free(mem);
    else {
        errno = ENOMEM;
        scm_syserror(subr);
    }
    return mem;
}

static SCM class_ref_proc = SCM_UNDEFINED;
static SCM class_set_proc = SCM_UNDEFINED;
static SCM srfi1_drop_right_proc = SCM_UNDEFINED;
static SCM module_reexport_proc = SCM_UNDEFINED;

SCM
scm_class_ref(SCM cls, SCM slot)
{
    if (SCM_UNBNDP(class_ref_proc))
        class_ref_proc = scm_c_public_ref("oop goops", "class-slot-ref");

    return scm_call_2(class_ref_proc, cls, slot);
}

SCM
scm_class_set_x(SCM cls, SCM slot, SCM val)
{
    if (SCM_UNBNDP(class_set_proc))
        class_set_proc = scm_c_public_ref("oop goops", "class-slot-set!");
    return scm_call_3(class_set_proc, cls, slot, val);
}

SCM
scm_drop_right_1(SCM lst)
{
    if (SCM_UNBNDP(srfi1_drop_right_proc))
        srfi1_drop_right_proc = scm_c_public_ref("srfi srfi-1", "drop-right");
    return scm_call_2(srfi1_drop_right_proc, lst, scm_from_int(1));
}

SCM
scm_c_reexport(const gchar *name, ...)
{
    if (SCM_UNBNDP(module_reexport_proc))
        module_reexport_proc = scm_c_public_ref("guile", "module-re-export!");

    va_list args;
    va_start(args, name);

    SCM current_module = scm_current_module();
    SCM syms = SCM_EOL;

    SCM _public = scm_module_public_interface(current_module);
    SCM obarray = SCM_MODULE_OBARRAY(_public);

    do {
        SCM sym = scm_from_utf8_symbol(name);

        if (scm_is_false(scm_hashq_get_handle(obarray, sym)))
            syms = scm_cons(scm_from_utf8_symbol(name), syms);

        name = va_arg(args, gchar *);
    } while (name != NULL);

    scm_call_2(module_reexport_proc, current_module, syms);
    va_end(args);

    return SCM_UNSPECIFIED;
}

void
scm_printf(SCM port, const gchar *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    gchar *_message = g_strdup_vprintf(fmt, args);
    SCM message = scm_from_utf8_string(_message);
    free(_message);
    scm_display(message, port);
}

const gchar *
g_base_info_get_name_safe(GIBaseInfo *info)
{
    GIInfoType type = g_base_info_get_type(info);
    switch (type) {
    case GI_INFO_TYPE_FUNCTION:
    case GI_INFO_TYPE_CALLBACK:
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
    case GI_INFO_TYPE_OBJECT:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_CONSTANT:
    case GI_INFO_TYPE_UNION:
    case GI_INFO_TYPE_VALUE:
    case GI_INFO_TYPE_SIGNAL:
    case GI_INFO_TYPE_PROPERTY:
    case GI_INFO_TYPE_VFUNC:
    case GI_INFO_TYPE_FIELD:
    case GI_INFO_TYPE_ARG:
    case GI_INFO_TYPE_UNRESOLVED:
        return g_base_info_get_name(info);
        break;
    case GI_INFO_TYPE_TYPE:
    default:
        return "(unnamed)";
        break;
    }
}

gchar *
g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info)
{
    const gchar *_name = g_base_info_get_attribute(info, "c:type");
    if (_name != NULL)
        return xstrdup(_name);

    const gchar *_namespace = g_base_info_get_namespace(info);
    const gchar *prefix = g_irepository_get_c_prefix(NULL, _namespace);

    // add initial % to ensure that the name is private
    return g_strdup_printf("%%%s%s", prefix, g_base_info_get_name(info));
}

gchar *
scm_write_to_utf8_stringn(SCM x, gsize max_len)
{
    static int first = 1;
    static SCM format;
    static SCM ellipses;
    if (first) {
        format = scm_from_utf8_string("~S");
        ellipses = scm_from_utf8_string("...");
        first = 0;
    }

    SCM args_str = scm_simple_format(SCM_BOOL_F, format, scm_list_1(x));
    gchar *cstr;
    if (scm_c_string_length(args_str) > max_len) {
        SCM truncated_args_str =
            scm_string_append(scm_list_2(scm_c_substring(args_str, 0, max_len), ellipses));
        cstr = scm_to_utf8_string(truncated_args_str);
    }
    else
        cstr = scm_to_utf8_string(args_str);

    return cstr;
}
