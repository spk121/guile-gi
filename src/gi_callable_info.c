#include "gi_callable_info.h"
#include "gig_util.h"
#include "gig_function.h"


static gboolean is_predicate(GIFunctionInfo *info);

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

        gi_callable_info_count_args(info, &in, &out);
        if (out == 0)
            predicate = TRUE;
    }
    g_base_info_unref(return_type);
    return predicate;
}

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
void
gi_callable_info_count_args(GICallableInfo *info, gint *in, gint *out)
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
gi_callable_info_make_name(GICallableInfo *info, const gchar *prefix)
{
    gchar *name, *str1 = NULL, *str2 = NULL;
    gboolean predicate;

    predicate = is_predicate(info);
    if (prefix)
        str1 = gig_gname_to_scm_name(prefix);
    str2 = gig_gname_to_scm_name(g_base_info_get_name(info));
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
