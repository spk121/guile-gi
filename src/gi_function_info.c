#include "gi_function_info.h"
#include "gi_util.h"
#include "gir_function.h"

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
gchar *
gi_function_info_make_name(GIFunctionInfo *info, const gchar *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    gboolean predicate;

    predicate = gir_function_info_is_predicate(info);
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
