// Copyright (C) 2019, 2020, 2021 Michael L. Gran

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

#include "core.h"
#define _XOPEN_SOURCE 700       /* For strdup, strndup */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gig_util.h"

static bool is_predicate(GIFunctionInfo *info);
static void count_args(GICallableInfo *info, int *in, int *out);

// Returns TRUE if this function returns a single boolean.
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

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
char *
gig_callable_info_make_name(GICallableInfo *info, const char *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    bool predicate, destructive;

    predicate = is_predicate(info);
    destructive = is_destructive(info);
    if (prefix)
        str1 = make_scm_name(prefix);
    str2 = make_scm_name(g_base_info_get_name(info));
    if (!prefix) {
        if (destructive)
            name = concatenate(str2, "!");
        else if (predicate)
            name = concatenate(str2, "?");
        else
            return str2;
    }
    else {
        if (destructive)
            name = concatenate4(str1, ":", str2, "!");
        else if (predicate)
            name = concatenate4(str1, ":", str2, "?");
        else
            name = concatenate3(str1, ":", str2);
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
const char *
gig_constant_strip_prefix(const char *name, const char *strip_prefix)
{
    size_t prefix_len, i;

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
        if (isalpha(name[i - 1]) || name[i - 1] == '_') {
            return &name[i - 1];
        }
    }
    return name;
}

SCM
scm_c_list_ref(SCM list, size_t k)
{
    return scm_list_ref(list, scm_from_size_t(k));
}

size_t
scm_c_length(SCM list)
{
    return scm_to_size_t(scm_length(list));
}

bool
scm_is_list(SCM obj)
{
    return scm_is_true(scm_list_p(obj));
}

void *
scm_dynwind_or_bust(const char *subr, void *mem)
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
scm_c_reexport(const char *name, ...)
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

        name = va_arg(args, char *);
    } while (name != NULL);

    scm_call_2(module_reexport_proc, current_module, syms);
    va_end(args);

    return SCM_UNSPECIFIED;
}

void
scm_printf(SCM port, const char *fmt, ...)
{
#define SCM_PRINTF_MAX_LINE_LEN (1024)
    char _message[SCM_PRINTF_MAX_LINE_LEN];
    va_list args;
    va_start(args, fmt);
    vsnprintf(_message, SCM_PRINTF_MAX_LINE_LEN, fmt, args);
    va_end(args);
    SCM message = scm_from_utf8_string(_message);
    scm_display(message, port);
#undef SCM_PRINTF_MAX_LINE_LEN
}

const char *
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

char *
g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info)
{
    const char *_name = g_base_info_get_attribute(info, "c:type");
    if (_name != NULL)
        return xstrdup(_name);

    const char *_namespace = g_base_info_get_namespace(info);
    const char *prefix = g_irepository_get_c_prefix(NULL, _namespace);

    // add initial % to ensure that the name is private
    return concatenate3("%", prefix, g_base_info_get_name(info));
}

char *
scm_write_to_utf8_stringn(SCM x, size_t max_len)
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
    char *cstr;
    if (scm_c_string_length(args_str) > max_len) {
        SCM truncated_args_str =
            scm_string_append(scm_list_2(scm_c_substring(args_str, 0, max_len), ellipses));
        cstr = scm_to_utf8_string(truncated_args_str);
    }
    else
        cstr = scm_to_utf8_string(args_str);

    return cstr;
}
