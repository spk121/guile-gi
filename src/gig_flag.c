// Copyright (C) 2018, 2019 Michael L. Gran

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

#include <inttypes.h>
#include <libguile.h>
#include "gig_flag.h"

static gchar *gname_to_scm_constant_name(const gchar *gname);

static gchar *
gname_to_scm_constant_name(const gchar *gname)
{
    gsize len = strlen(gname);
    GString *str = g_string_new(NULL);
    gboolean was_lower = FALSE;

    for (gsize i = 0; i < len; i++) {
        if (g_ascii_islower(gname[i])) {
            g_string_append_c(str, g_ascii_toupper(gname[i]));
            was_lower = TRUE;
        }
        else if (gname[i] == '_' || gname[i] == '-') {
            g_string_append_c(str, '_');
            was_lower = FALSE;
        }
        else if (g_ascii_isdigit(gname[i])) {
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
        else if (g_ascii_isupper(gname[i])) {
            if (was_lower)
                g_string_append_c(str, '_');
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
    }

    gchar *fptr = strstr(str->str, "_FLAGS");
    if (fptr) {
        memcpy(fptr, fptr + 6, str->len - (fptr - str->str) - 6);
        memset(str->str + str->len - 6, 0, 6);
        str->len -= 6;
    }

    return g_string_free(str, FALSE);
}

static gchar *
gig_flag_public_name(const gchar *parent, GIBaseInfo *info)
{
    gchar *short_parent, *tmp_str, *public_name;

    // Many flag collection names end in 'Type', which isn't informative.
    if (g_str_has_suffix(parent, "Type")) {
        gsize len = strlen(parent);
        short_parent = g_strndup(parent, len - 4);
    }
    else
        short_parent = g_strdup(parent);

    tmp_str = g_strdup_printf("%s-%s", short_parent, g_base_info_get_name(info));
    public_name = gname_to_scm_constant_name(tmp_str);
    g_free(short_parent);
    g_free(tmp_str);
    return public_name;
}

void
gig_flag_define(GIEnumInfo *info)
{
    g_assert(info != NULL);

    gint n_values = g_enum_info_get_n_values(info);
    gint i = 0;
    GIValueInfo *vi = NULL;
    gchar *public_name;

    while (i < n_values) {
        vi = g_enum_info_get_value(info, i);
        public_name = gig_flag_public_name(g_base_info_get_name(info), vi);
        gint64 val = g_value_info_get_value(vi);
        SCM ret = scm_from_int64(val);

        g_debug("defining flag/enum %s and %" PRId64, public_name, val);
        scm_permanent_object(scm_c_define(public_name, ret));
        scm_c_export(public_name, NULL);

        g_base_info_unref(vi);
        free(public_name);
        i++;
    }
}

void
gig_init_flag(void)
{

}
