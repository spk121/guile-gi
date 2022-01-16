// Copyright (C) 2019 Michael L. Gran

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

#include <string.h>
#include <stdio.h>
#include "x.h"
#include "y/arg.h"

static const char arg_payload_string[(size_t)TYPE_TAG_OTHER+1][16] =
{
    "INVALID",
    "VOID",
    "POINTER",
    "BOOLEAN",
    "INT8",
    "UINT8",
    "INT16",
    "UINT16",
    "INT32",
    "UINT32",
    "INT64",
    "UINT64",
    "FLOAT",
    "DOUBLE",
    "GTYPE",
    "UTF8_STRING",
    "LOCALE_STRING",
    "ARRAY",
    "GARRAY",
    "GPTRARRAY",
    "GBYTEARRAY",
    "GLIST",
    "GSLIST",
    "GHASH",
    "ERROR",
    "UNICHAR",
    "FUNCTION",
    "CALLBACK",
    "STRUCT",
    "UNION",
    "ENUM",
    "FLAGS",
    "OBJECT",
    "INTERFACE",
    "CONSTANT",
    "OTHER"
};

static const char *
get_info_name(GIBaseInfo *info)
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

static void arg_init_from_type_info(Arg *type, GITypeInfo *ti);
static void arg_init_from_basic_type_tag(Arg *meta, GITypeTag tag);

void
arg_init_from_arg_info(Arg *meta, GIArgInfo *ai)
{
    GITypeInfo *type_info = g_arg_info_get_type(ai);
    GIDirection dir = g_arg_info_get_direction(ai);
    GITransfer transfer = g_arg_info_get_ownership_transfer(ai);

    arg_init_from_type_info(meta, type_info);

    meta->is_in = (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT);
    meta->is_out = (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT);
    meta->is_skip = g_arg_info_is_skip(ai);

    meta->is_caller_allocates = g_arg_info_is_caller_allocates(ai);
    meta->is_optional = g_arg_info_is_optional(ai);
    meta->is_nullable = g_arg_info_may_be_null(ai);

    meta->transfer = transfer;
    g_base_info_unref(type_info);
}

void
arg_init_from_callable_info(Arg *meta, GICallableInfo *ci)
{
    GITypeInfo *type_info = g_callable_info_get_return_type(ci);
    GITransfer transfer = g_callable_info_get_caller_owns(ci);

    arg_init_from_type_info(meta, type_info);

    meta->is_in = FALSE;
    if (meta->payload != TYPE_TAG_VOID && meta->payload != TYPE_TAG_INVALID)
        meta->is_out = TRUE;
    else
        meta->is_out = FALSE;
    meta->is_skip = g_callable_info_skip_return(ci);

    meta->is_caller_allocates = FALSE;
    meta->is_optional = FALSE;
    meta->is_nullable = g_callable_info_may_return_null(ci);

    meta->transfer = transfer;
    g_base_info_unref(type_info);
}

static void
add_params(Arg *meta, int n)
{
    meta->params = xcalloc(n, sizeof(Arg));
    meta->n_params = n;
}

static void
add_child_params(Arg *meta, GITypeInfo *type_info, int n)
{
    GITypeInfo *param_type;
    add_params(meta, n);

    for (int i = 0; i < n; i++) {
        param_type = g_type_info_get_param_type((GITypeInfo *)type_info, i);
        arg_init_from_type_info(&meta->params[i], param_type);
        g_base_info_unref(param_type);

        if (meta->transfer == TRANSFER_EVERYTHING)
            meta->params[i].transfer = TRANSFER_EVERYTHING;
        else
            meta->params[i].transfer = TRANSFER_NOTHING;

        meta->is_invalid |= meta->params[i].is_invalid;
    }
}

static void
arg_init_from_basic_type_tag(Arg *meta, GITypeTag tag)
{
#define T(TYPETAG,GTYPE,CTYPE)                  \
    do {                                        \
        if (tag == TYPETAG) {                   \
            meta->payload = GTYPE;                \
            meta->item_size = sizeof (CTYPE);   \
            return;                             \
        }                                       \
    } while(FALSE)

    T(GI_TYPE_TAG_BOOLEAN, TYPE_TAG_BOOLEAN, int);
    T(GI_TYPE_TAG_DOUBLE, TYPE_TAG_DOUBLE, double);
    T(GI_TYPE_TAG_FLOAT, TYPE_TAG_FLOAT, float);
    T(GI_TYPE_TAG_GTYPE, TYPE_TAG_GTYPE, size_t);
    T(GI_TYPE_TAG_INT8, TYPE_TAG_INT8, int8_t);
    T(GI_TYPE_TAG_INT16, TYPE_TAG_INT16, int16_t);
    T(GI_TYPE_TAG_INT32, TYPE_TAG_INT32, int32_t);
    T(GI_TYPE_TAG_INT64, TYPE_TAG_INT64, int64_t);
    T(GI_TYPE_TAG_UINT8, TYPE_TAG_UINT8, uint8_t);
    T(GI_TYPE_TAG_UINT16, TYPE_TAG_UINT16, uint16_t);
    T(GI_TYPE_TAG_UINT32, TYPE_TAG_UINT32, uint32_t);
    T(GI_TYPE_TAG_UINT64, TYPE_TAG_UINT64, uint64_t);
    T(GI_TYPE_TAG_UNICHAR, TYPE_TAG_UNICHAR, uint32_t);
    T(GI_TYPE_TAG_UTF8, TYPE_TAG_UTF8_STRING, char *);
    T(GI_TYPE_TAG_UTF8, TYPE_TAG_LOCALE_STRING, char *);
    T(GI_TYPE_TAG_ERROR, G_TYPE_ERROR, GError);
    error_load("unhandled type '%s' %s %d", g_type_tag_to_string(tag), __FILE__, __LINE__);
#undef T
}

static void
arg_init_from_type_info(Arg *meta, GITypeInfo *type_info)
{
    GITypeTag tag = g_type_info_get_tag(type_info);
    meta->is_ptr = g_type_info_is_pointer(type_info);

    if (tag == GI_TYPE_TAG_VOID) {
        if (meta->is_ptr)
            meta->payload = TYPE_TAG_POINTER;
        else
            meta->payload = TYPE_TAG_VOID;
        meta->item_size = 0;
    }
    else if (tag == GI_TYPE_TAG_ARRAY) {
        GIArrayType array_type = g_type_info_get_array_type(type_info);
        int len;

        add_child_params(meta, type_info, 1);

        if (array_type == GI_ARRAY_TYPE_C) {
            meta->payload = TYPE_TAG_ARRAY;
            meta->is_raw_array = TRUE;
            meta->length = ARG_ARRAY_SIZE_UNKNOWN;

            if ((len = g_type_info_get_array_length(type_info)) != -1)
                meta->has_size = TRUE;
            else if ((len = g_type_info_get_array_fixed_size(type_info)) != -1)
                meta->length = len;

            if (g_type_info_is_zero_terminated(type_info))
                meta->is_zero_terminated = TRUE;

            if (len == -1 && !meta->is_zero_terminated) {
                warning_load("no way of determining array size of %s, coercing to pointer",
                                 g_base_info_get_name(type_info));
                meta->payload = TYPE_TAG_POINTER;
            }
        }
        else if (array_type == GI_ARRAY_TYPE_ARRAY)
            meta->payload = TYPE_TAG_GARRAY;
        else if (array_type == GI_ARRAY_TYPE_BYTE_ARRAY)
            meta->payload = TYPE_TAG_GBYTEARRAY;
        else if (array_type == GI_ARRAY_TYPE_PTR_ARRAY)
            meta->payload = TYPE_TAG_GPTRARRAY;
        elsel
            assert_not_reached();
    }
    else if (tag == GI_TYPE_TAG_GHASH) {
        meta->payload = TYPE_TAG_GHASH;
        meta->item_size = sizeof(GHashTable *);
        add_child_params(meta, type_info, 2);
    }
    else if (tag == GI_TYPE_TAG_GLIST) {
        meta->payload = TYPE_TAG_GLIST;
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_GSLIST) {
        meta->payload = TYPE_TAG_GSLIST;
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *referenced_base_info = g_type_info_get_interface(type_info);
        GIInfoType itype = g_base_info_get_type(referenced_base_info);
        switch (itype) {
        case GI_INFO_TYPE_UNRESOLVED:
            meta->payload = TYPE_TAG_INVALID;
            meta->is_invalid = TRUE;
            warning_load("Unrepresentable type: %s, %s, %s",
                             get_info_name(type_info),
                             get_info_name(referenced_base_info),
                             g_info_type_to_string(itype));
            break;
        case GI_INFO_TYPE_ENUM:
        case GI_INFO_TYPE_FLAGS:
            if (itype == GI_INFO_TYPE_ENUM)
                meta->payload = TYPE_TAG_ENUM;
            else
                meta->payload = TYPE_TAG_FLAGS;
            
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            meta->name = get_info_name(type_info);
            // Not all enum or flag types have an associated gtype_t
            // Hence we store the enum info for GIArgument conversions
            if (meta->gtype == G_TYPE_NONE) {
                meta->enum_info = g_base_info_ref(referenced_base_info);
            }
            break;

        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_UNION:
        case GI_INFO_TYPE_OBJECT:
        case GI_INFO_TYPE_INTERFACE:
            if (itype == GI_INFO_TYPE_STRUCT) {
                meta->payload = TYPE_TAG_STRUCT;
                meta->item_size = g_struct_info_get_size(referenced_base_info);
            }
            else if (itype == GI_INFO_TYPE_UNION) {
                meta->payload = TYPE_TAG_STRUCT;
                meta->item_size = g_union_info_get_size(referenced_base_info);
            }
            else if (itype == GI_INFO_TYPE_OBJECT) {
                meta->payload = TYPE_TAG_OBJECT;
            }
            else if (itype == GI_INFO_TYPE_INTERFACE) {
                meta->payload = TYPE_TAG_INTERFACE;
            }
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            meta->name = get_info_name(type_info);
            
            if (meta->gtype == G_TYPE_NONE)
                meta->is_invalid = TRUE;
            break;
        case GI_INFO_TYPE_CALLBACK:
            meta->payload = TYPE_TAG_CALLBACK;
            meta->callable_info = g_base_info_ref(referenced_base_info);

#if 0            
            // TODO: Find a way to reuse this amap, so that computing it is not a waste
            GigArgMap *_amap = gig_amap_new(NULL, meta->callable_info);
            if (_amap == NULL)
                meta->is_invalid = TRUE;
            else
                gig_amap_free(_amap);
#endif
            break;
            
        default:
            if (GI_IS_REGISTERED_TYPE_INFO(referenced_base_info)) {
                meta->payload = TYPE_TAG_OTHER;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                meta->name = get_info_name(type_info);
                
                if (meta->gtype == G_TYPE_NONE)
                    meta->is_invalid = TRUE;
            }
            else {
                critical_load("Unhandled item type in %s:%d", __FILE__, __LINE__);
                meta->is_invalid = TRUE;
            }
        }

        g_base_info_unref(referenced_base_info);
    }
    else
        arg_init_from_basic_type_tag(meta, tag);

    // FIXME: how did we get here?
    if (meta->payload == TYPE_TAG_VOID) {
        if (meta->is_ptr) {
            meta->payload = TYPE_TAG_POINTER;
            meta->item_size = sizeof(void *);
        }
    }
}

size_t
arg_item_size(const Arg *meta)
{
    return meta->item_size;
}

void
arg_free(Arg *meta)
{
    for (int i = 0; i < meta->n_params; i++)
        arg_free(&meta->params[i]);
    if (meta->n_params > 0)
        free(meta->params);

    if (meta->payload == TYPE_TAG_CALLBACK && meta->callable_info != NULL)
        g_base_info_unref(meta->callable_info);
    if (((meta->payload == TYPE_TAG_ENUM) || (meta->payload == TYPE_TAG_FLAGS))
        && meta->enum_info)
        g_base_info_unref(meta->enum_info);
}

#define STRLEN 128
static char arg_describe_buf[STRLEN];

const char *
arg_describe(const Arg *meta)
{
    snprintf(arg_describe_buf, STRLEN,
             "%s%s%s %s %s",
             meta->is_ptr ? "pointer to " : "",
             meta->is_caller_allocates ? "caller allocated " : "",
             arg_payload_string[meta->payload],
             meta->gtype ? meta->name : "",
             meta->is_nullable ? "or NULL" : "");
    return arg_describe_buf;
}

#undef STRLEN
