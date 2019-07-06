#include "gi_function_info.h"
#include "gi_util.h"
#include "gir_function.h"

static gboolean gi_function_info_is_predicate(GIFunctionInfo *info);

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
void
gi_function_info_count_args(GIFunctionInfo *info, int *in, int *out)
{
    // Count the number of required input arguments, and store
    // the arg info in a newly allocate array.
    int n_args = g_callable_info_get_n_args((GICallableInfo *)info);
    int n_input_args = 0;
    int n_output_args = 0;

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg((GICallableInfo *)info, i);
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
static gboolean
gi_function_info_is_predicate(GIFunctionInfo *info)
{
    gboolean predicate = FALSE;
    GITypeInfo *return_type;

    return_type = g_callable_info_get_return_type(info);

    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type)) {
        int in, out;

        gi_function_info_count_args(info, &in, &out);
        if (out == 0)
            predicate = TRUE;
    }
    g_base_info_unref(return_type);
    return predicate;
}

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
gchar *
gi_function_info_make_name(GIFunctionInfo *info, const gchar *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    gboolean predicate;

    predicate = gi_function_info_is_predicate(info);
    if (prefix)
        str1 = gname_to_scm_name(prefix);
    str2 = gname_to_scm_name(g_base_info_get_name(info));
    if (!prefix && !predicate)
        return str2;
    else if (!prefix && predicate)
        name = g_strdup_printf("%s?", str2);
    else if (prefix && !predicate)
        name = g_strdup_printf("%s:%s", str1, str2);
    else
        name = g_strdup_printf("%s:%s?", str1, str2);
    g_free(str1);
    g_free(str2);
    return name;
}
