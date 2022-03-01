// Copyright (C) 2018, 2019, 2022 Michael L. Gran

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
#include "../core.h"
#include "gig_constant.h"

SCM
gig_constant_define(GIConstantInfo *info)
{
    SCM defs = SCM_EOL;
    const char *public_name = g_base_info_get_name(info);

    GITypeInfo *typeinfo;
    typeinfo = g_constant_info_get_type(info);
    GITypeTag typetag;
    typetag = g_type_info_get_tag(typeinfo);

    GIArgument value;
    SCM ret;

    g_constant_info_get_value(info, &value);

    switch (typetag) {
    case GI_TYPE_TAG_BOOLEAN:
        gig_debug_load("%s - boolean constant %d", public_name, value.v_boolean);
        ret = scm_from_bool(value.v_boolean);
        break;
    case GI_TYPE_TAG_DOUBLE:
        gig_debug_load("%s - double constant %lf", public_name, value.v_double);
        ret = scm_from_double(value.v_double);
        break;
    case GI_TYPE_TAG_INT8:
        gig_debug_load("%s - int8 constant %d", public_name, (int)value.v_int8);
        ret = scm_from_int8(value.v_int8);
        break;
    case GI_TYPE_TAG_INT16:
        gig_debug_load("%s - int16 constant %d", public_name, (int)value.v_int16);
        ret = scm_from_int16(value.v_int16);
        break;
    case GI_TYPE_TAG_INT32:
        gig_debug_load("%s - int32 constant %d", public_name, (int)value.v_int32);
        ret = scm_from_int32(value.v_int32);
        break;
    case GI_TYPE_TAG_INT64:
        gig_debug_load("%s - int64 constant %" PRId64, public_name, value.v_int64);
        ret = scm_from_int64(value.v_int64);
        break;
    case GI_TYPE_TAG_UINT8:
        gig_debug_load("%s - uint8 constant %d", public_name, (int)value.v_uint8);
        ret = scm_from_uint8(value.v_uint8);
        break;
    case GI_TYPE_TAG_UINT16:
        gig_debug_load("%s - uint16 constant %d", public_name, (int)value.v_uint16);
        ret = scm_from_uint16(value.v_uint16);
        break;
    case GI_TYPE_TAG_UINT32:
        gig_debug_load("%s - uint32 constant %d", public_name, (int)value.v_uint32);
        ret = scm_from_uint32(value.v_uint32);
        break;
    case GI_TYPE_TAG_UINT64:
        gig_debug_load("%s - uint64 constant %" PRIu64, public_name, value.v_uint64);
        ret = scm_from_uint64(value.v_uint64);
        break;
    case GI_TYPE_TAG_UTF8:
        gig_debug_load("%s - UTF8 constant %s", public_name, value.v_string);
        ret = scm_from_utf8_string(value.v_string);
        break;
    default:
        gig_critical_load("%s - unsupported constant type '%s'", public_name,
                          g_type_tag_to_string(typetag));
        ret = SCM_BOOL_F;
    }
    g_constant_info_free_value(info, &value);
    g_base_info_unref(typeinfo);

    scm_permanent_object(scm_c_define(public_name, ret));
    return scm_cons(scm_from_utf8_symbol(public_name), defs);
}
