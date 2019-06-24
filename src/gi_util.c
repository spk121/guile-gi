#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gi_util.h"

/**
 * gi_constant_strip_prefix:
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
gi_constant_strip_prefix(const gchar *name, const gchar *strip_prefix)
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
	if (g_ascii_isalpha(name[i - 1]) || name[i - 1] == '_') {
	    return &name[i - 1];
	}
    }
    return name;
}

char *
gname_to_scm_name(const char *gname)
{
    g_assert (gname != NULL);
    g_assert (strlen(gname) > 0);

    size_t len = strlen(gname);
    GString *str = g_string_new(NULL);
    gboolean was_lower = FALSE;

    for (size_t i = 0; i < len; i++)
    {
        if (g_ascii_islower(gname[i]))
        {
            g_string_append_c(str, gname[i]);
            was_lower = TRUE;
        }
        else if (gname[i] == '_' || gname[i] == '-')
        {
            g_string_append_c(str, '-');
            was_lower = FALSE;
        }
        else if (gname[i] == '?')
        {
            // does this even occur?
            g_string_append_c(str, '?');
            was_lower = FALSE;
        }
        else if (g_ascii_isdigit(gname[i]))
        {
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
        else if (g_ascii_isupper(gname[i]))
        {
            if (was_lower)
                g_string_append_c(str, '-');
            g_string_append_c(str, g_ascii_tolower(gname[i]));
            was_lower = FALSE;
        }
    }
    return g_string_free(str, FALSE);
}

SCM
scm_c_list_ref (SCM list, size_t k)
{
    return scm_list_ref (list, scm_from_size_t (k));
}

int
scm_is_list (SCM obj)
{
    return scm_is_true (scm_list_p (obj));
}
