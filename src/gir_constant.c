// gir_constant.c
// Copyright (C) 2018 Michael L. Gran

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

#include <libguile.h>
#include <inttypes.h>
#include "gir_constant.h"

#if 0
static char *gname_to_scm_constant_name(const char *gname);
static char *
gname_to_scm_constant_name(const char *gname)
{
    size_t len = strlen(gname);
    GString *str = g_string_new(NULL);
    gboolean was_lower = FALSE;

    for (size_t i = 0; i < len; i++)
    {
        if (g_ascii_islower(gname[i]))
        {
            g_string_append_c(str, g_ascii_toupper(gname[i]));
            was_lower = TRUE;
        }
        else if (gname[i] == '_' || gname[i] == '-')
        {
            g_string_append_c(str, '_');
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
                g_string_append_c(str, '_');
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
    }

    char *fptr = strstr(str->str, "_FLAGS");
    if (fptr)
    {
        memcpy(fptr, fptr + 6, str->len - (fptr - str->str) - 6);
        memset(str->str + str->len - 6, 0, 6);
        str->len -= 6;
    }

    return g_string_free(str, FALSE);
}
#endif

void
gir_constant_define(GIConstantInfo *info)
{
    const char *public_name = g_base_info_get_name(info);

    GITypeInfo *typeinfo;
    typeinfo = g_constant_info_get_type(info);
    GITypeTag typetag;
    typetag = g_type_info_get_tag(typeinfo);

    GIArgument value;
    SCM ret;

    g_constant_info_get_value(info, &value);

    switch (typetag)
    {
    case GI_TYPE_TAG_BOOLEAN:
        g_debug("defining boolean constant %s as %d", public_name, value.v_boolean);
        ret = scm_from_bool(value.v_boolean);
        break;
    case GI_TYPE_TAG_DOUBLE:
        g_debug("defining double constant %s as %lf", public_name, value.v_double);
        ret = scm_from_double(value.v_double);
        break;
    case GI_TYPE_TAG_INT8:
        g_debug("defining int8 constant %s as %d", public_name, (int)value.v_int8);
        ret = scm_from_int8(value.v_int8);
        break;
    case GI_TYPE_TAG_INT16:
        g_debug("defining int16 constant %s as %d", public_name, (int)value.v_int16);
        ret = scm_from_int16(value.v_int16);
        break;
    case GI_TYPE_TAG_INT32:
        g_debug("defining int32 constant %s as %d", public_name, (int)value.v_int32);
        ret = scm_from_int32(value.v_int32);
        break;
    case GI_TYPE_TAG_INT64:
        g_debug("defining int64 constant %s as %" PRId64, public_name, value.v_int64);
        ret = scm_from_int64(value.v_int64);
        break;
    case GI_TYPE_TAG_UINT8:
        g_debug("defining uint8 constant %s as %d", public_name, (int)value.v_uint8);
        ret = scm_from_uint8(value.v_uint8);
        break;
    case GI_TYPE_TAG_UINT16:
        g_debug("defining uint16 constant %s as %d", public_name, (int)value.v_uint16);
        ret = scm_from_uint16(value.v_uint16);
        break;
    case GI_TYPE_TAG_UINT32:
        g_debug("defining uint32 constant %s as %d", public_name, (int)value.v_uint32);
        ret = scm_from_uint32(value.v_uint32);
        break;
    case GI_TYPE_TAG_UINT64:
        g_debug("defining uint64 constant %s as %" PRIu64, public_name, value.v_uint64);
        ret = scm_from_uint64(value.v_uint64);
        break;
    case GI_TYPE_TAG_UTF8:
        g_debug("defining UTF8 constant %s as %s", public_name, value.v_string);
        ret = scm_from_utf8_string(value.v_string);
        break;
    default:
        g_critical("Constant %s has unsupported type %d",
            public_name, typetag);
        ret = SCM_BOOL_F;
    }
    g_constant_info_free_value(info, &value);
    g_base_info_unref(typeinfo);

    scm_permanent_object(scm_c_define(public_name, ret));
    scm_c_export(public_name, NULL);
}

void gir_init_constant(void)
{
    
}
