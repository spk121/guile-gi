#include <stdio.h>
#include <girepository.h>
#include "x.h"
#include "util.h"

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

// Returns TRUE if this function returns a single boolean.
static int
is_predicate(GICallableInfo *info)
{
    int predicate = FALSE;
    GITypeInfo *return_type;

    if (GI_IS_SIGNAL_INFO(info))
        return FALSE;

    return_type = g_callable_info_get_return_type(info);

    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type)) {
        int in, out;

        count_args(info, &in, &out);
        if (out == 0)
            predicate = TRUE;
    }
    g_base_info_unref(return_type);
    return predicate;
}

static int
is_destructive(GICallableInfo *info)
{
    int destructive = FALSE;
    int n_args = g_callable_info_get_n_args(info);

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GITypeInfo *ti = g_arg_info_get_type(ai);
        int is_trivial;

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


// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
char *
make_callable_name(GICallableInfo *info, const char *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    int predicate, destructive;

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
    name = xmalloc(len);

    if (!prefix) {
        if (destructive)
            snprintf(name, len, "%s!", str2);
        else if (predicate)
            snprintf(name, len, "%s?", str2);
        else
            return str2;
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

char *
g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info)
{
    const char *_name = g_base_info_get_attribute(info, "c:type");
    if (_name != NULL)
        return xstrdup(_name);

    const char *_namespace = g_base_info_get_namespace(info);
    const char *prefix = g_irepository_get_c_prefix(NULL, _namespace);

    // add initial % to ensure that the name is private
    int len = strlen("%") + strlen(prefix) + strlen(g_base_info_get_name(info)) + 1;
    char *str = xmalloc(len);
    snprintf(str, len, "%%%s%s", prefix, g_base_info_get_name(info));
    return str;
}

