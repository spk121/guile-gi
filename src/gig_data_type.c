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
#include <glib-object.h>
#include "gig_argument.h"
#include "gig_data_type.h"
#include "gig_arg_map.h"
#include "gig_util.h"

static void gig_type_meta_init_from_type_info(GigTypeMeta *type, GITypeInfo *ti);
static void gig_type_meta_init_from_basic_type_tag(GigTypeMeta *meta, GITypeTag tag);

void
gig_type_meta_init_from_arg_info(GigTypeMeta *meta, GIArgInfo *ai)
{
    GITypeInfo *type_info = g_arg_info_get_type(ai);
    GIDirection dir = g_arg_info_get_direction(ai);
    GITransfer transfer = g_arg_info_get_ownership_transfer(ai);

    gig_type_meta_init_from_type_info(meta, type_info);

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
gig_type_meta_init_from_callable_info(GigTypeMeta *meta, GICallableInfo *ci)
{
    GITypeInfo *type_info = g_callable_info_get_return_type(ci);
    GITransfer transfer = g_callable_info_get_caller_owns(ci);

    gig_type_meta_init_from_type_info(meta, type_info);

    meta->is_in = false;
    if (meta->gtype != G_TYPE_NONE && meta->gtype != G_TYPE_INVALID)
        meta->is_out = true;
    else
        meta->is_out = false;
    meta->is_skip = g_callable_info_skip_return(ci);

    meta->is_caller_allocates = false;
    meta->is_optional = false;
    meta->is_nullable = g_callable_info_may_return_null(ci);

    meta->transfer = transfer;
    g_base_info_unref(type_info);
}

static void
add_params(GigTypeMeta *meta, int n)
{
    meta->params = g_new0(GigTypeMeta, n);
    meta->n_params = n;
}

static void
add_child_params(GigTypeMeta *meta, GITypeInfo *type_info, int n)
{
    GITypeInfo *param_type;
    add_params(meta, n);

    for (int i = 0; i < n; i++) {
        param_type = g_type_info_get_param_type((GITypeInfo *)type_info, i);
        gig_type_meta_init_from_type_info(&meta->params[i], param_type);
        g_base_info_unref(param_type);

        if (meta->transfer == GI_TRANSFER_EVERYTHING)
            meta->params[i].transfer = GI_TRANSFER_EVERYTHING;
        else
            meta->params[i].transfer = GI_TRANSFER_NOTHING;

        meta->is_invalid |= meta->params[i].is_invalid;
    }
}

static void
gig_type_meta_init_from_basic_type_tag(GigTypeMeta *meta, GITypeTag tag)
{
#define T(TYPETAG,GTYPE,CTYPE)                  \
    do {                                        \
        if (tag == TYPETAG) {                   \
            meta->gtype = GTYPE;                \
            meta->item_size = sizeof (CTYPE);   \
            return;                             \
        }                                       \
    } while(false)

    T(GI_TYPE_TAG_BOOLEAN, G_TYPE_BOOLEAN, gboolean);
    T(GI_TYPE_TAG_DOUBLE, G_TYPE_DOUBLE, double);
    T(GI_TYPE_TAG_FLOAT, G_TYPE_FLOAT, float);
    T(GI_TYPE_TAG_GTYPE, G_TYPE_GTYPE, GType);
    T(GI_TYPE_TAG_INT8, G_TYPE_CHAR, int8_t);
    T(GI_TYPE_TAG_INT16, G_TYPE_INT, int16_t);
    T(GI_TYPE_TAG_INT32, G_TYPE_INT, int32_t);
    T(GI_TYPE_TAG_INT64, G_TYPE_INT, int64_t);
    T(GI_TYPE_TAG_UINT8, G_TYPE_UCHAR, uint8_t);
    T(GI_TYPE_TAG_UINT16, G_TYPE_UINT, uint16_t);
    T(GI_TYPE_TAG_UINT32, G_TYPE_UINT, uint32_t);
    T(GI_TYPE_TAG_UINT64, G_TYPE_UINT, uint64_t);
    if (tag == GI_TYPE_TAG_UNICHAR) {
        meta->gtype = G_TYPE_UINT;
        meta->item_size = sizeof(gunichar);
        meta->is_unichar = true;
        return;
    }
    if (tag == GI_TYPE_TAG_UTF8) {
        meta->gtype = G_TYPE_STRING;
        meta->pointer_type = GIG_DATA_UTF8_STRING;
        return;
    }
    if (tag == GI_TYPE_TAG_FILENAME) {
        meta->gtype = G_TYPE_STRING;
        meta->pointer_type = GIG_DATA_LOCALE_STRING;
        return;
    }
    T(GI_TYPE_TAG_ERROR, G_TYPE_ERROR, GError);
    g_error("unhandled type '%s' %s %d", g_type_tag_to_string(tag), __FILE__, __LINE__);
#undef T
}

static void
gig_type_meta_init_from_type_info(GigTypeMeta *meta, GITypeInfo *type_info)
{
    GITypeTag tag = g_type_info_get_tag(type_info);
    meta->is_ptr = g_type_info_is_pointer(type_info);

    if (tag == GI_TYPE_TAG_VOID) {
        if (meta->is_ptr)
            meta->gtype = G_TYPE_POINTER;
        else
            meta->gtype = G_TYPE_NONE;
        // Also sets pointer_type to VOID, which is exactly, what we want
        meta->item_size = 0;
    }
    else if (tag == GI_TYPE_TAG_ARRAY) {
        GIArrayType array_type = g_type_info_get_array_type(type_info);
        int len;

        add_child_params(meta, type_info, 1);

        if (array_type == GI_ARRAY_TYPE_C) {
            meta->gtype = G_TYPE_ARRAY;
            meta->is_raw_array = true;
            meta->length = GIG_ARRAY_SIZE_UNKNOWN;

            if ((len = g_type_info_get_array_length(type_info)) != -1)
                meta->has_size = true;
            else if ((len = g_type_info_get_array_fixed_size(type_info)) != -1)
                meta->length = len;

            if (g_type_info_is_zero_terminated(type_info))
                meta->is_zero_terminated = true;

            if (len == -1 && !meta->is_zero_terminated) {
                g_warning("no way of determining array size of %s, coercing to pointer",
                          g_type_name(meta->gtype));
                meta->gtype = G_TYPE_POINTER;
            }
        }
        else if (array_type == GI_ARRAY_TYPE_ARRAY)
            meta->gtype = G_TYPE_ARRAY;
        else if (array_type == GI_ARRAY_TYPE_BYTE_ARRAY)
            meta->gtype = G_TYPE_BYTE_ARRAY;
        else if (array_type == GI_ARRAY_TYPE_PTR_ARRAY)
            meta->gtype = G_TYPE_PTR_ARRAY;
        else
            g_assert_not_reached();
    }
    else if (tag == GI_TYPE_TAG_GHASH) {
        meta->gtype = G_TYPE_HASH_TABLE;
        meta->item_size = sizeof(GHashTable *);
        add_child_params(meta, type_info, 2);
    }
    else if (tag == GI_TYPE_TAG_GLIST) {
        meta->gtype = G_TYPE_POINTER;
        meta->pointer_type = GIG_DATA_LIST;
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_GSLIST) {
        meta->gtype = G_TYPE_POINTER;
        meta->pointer_type = GIG_DATA_SLIST;
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *referenced_base_info = g_type_info_get_interface(type_info);
        GIInfoType itype = g_base_info_get_type(referenced_base_info);
        switch (itype) {
        case GI_INFO_TYPE_UNRESOLVED:
            meta->gtype = G_TYPE_INVALID;
            meta->is_invalid = true;
            g_warning("Unrepresentable type: %s, %s, %s",
                      g_base_info_get_name_safe(type_info),
                      g_base_info_get_name_safe(referenced_base_info),
                      g_info_type_to_string(itype));
            break;
        case GI_INFO_TYPE_ENUM:
        case GI_INFO_TYPE_FLAGS:
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            // Not all enum or flag types have an associated GType
            // Hence we store the enum info for GIArgument conversions
            if (meta->gtype == G_TYPE_NONE) {
                meta->enum_info = g_base_info_ref(referenced_base_info);
                meta->gtype = itype == GI_INFO_TYPE_ENUM ? G_TYPE_ENUM : G_TYPE_FLAGS;
            }
            break;

        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_UNION:
        case GI_INFO_TYPE_OBJECT:
        case GI_INFO_TYPE_INTERFACE:
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (itype == GI_INFO_TYPE_STRUCT)
                meta->item_size = g_struct_info_get_size(referenced_base_info);
            else if (itype == GI_INFO_TYPE_UNION)
                meta->item_size = g_union_info_get_size(referenced_base_info);

            if (meta->gtype == G_TYPE_NONE)
                meta->is_invalid = true;
            break;
        case GI_INFO_TYPE_CALLBACK:
        {
            meta->gtype = G_TYPE_POINTER;
            meta->pointer_type = GIG_DATA_CALLBACK;
            meta->callable_info = g_base_info_ref(referenced_base_info);
            // TODO: Find a way to reuse this amap, so that computing it is not a waste
            GigArgMap *_amap = gig_amap_new(NULL, meta->callable_info);
            if (_amap == NULL)
                meta->is_invalid = true;
            else
                gig_amap_free(_amap);
        }
            break;
        default:
            if (GI_IS_REGISTERED_TYPE_INFO(referenced_base_info)) {
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                if (meta->gtype == G_TYPE_NONE)
                    meta->is_invalid = true;
            }
            else {
                g_critical("Unhandled item type in %s:%d", __FILE__, __LINE__);
                meta->is_invalid = true;
            }
        }

        g_base_info_unref(referenced_base_info);
    }
    else
        gig_type_meta_init_from_basic_type_tag(meta, tag);

    // FIXME: how did we get here?
    if (meta->gtype == 0) {
        if (meta->is_ptr)
            meta->gtype = G_TYPE_POINTER;
        else
            meta->gtype = G_TYPE_NONE;
        meta->item_size = 0;
    }
}

G_GNUC_PURE size_t
gig_meta_real_item_size(const GigTypeMeta *meta)
{
    if (meta->gtype == G_TYPE_STRING || meta->gtype == G_TYPE_POINTER || meta->is_ptr)
        return sizeof(void *);
    return meta->item_size;
}

void
gig_data_type_free(GigTypeMeta *meta)
{
    for (int i = 0; i < meta->n_params; i++)
        gig_data_type_free(&meta->params[i]);
    if (meta->n_params > 0)
        g_free(meta->params);

    if ((meta->gtype == G_TYPE_POINTER) && (meta->pointer_type == GIG_DATA_CALLBACK) &&
        meta->callable_info)
        g_base_info_unref(meta->callable_info);
    if (((meta->gtype == G_TYPE_ENUM) || (meta->gtype == G_TYPE_FLAGS))
        && meta->enum_info)
        g_base_info_unref(meta->enum_info);
}

#define STRLEN 128
char gig_data_type_describe_buf[STRLEN];

const char *
gig_type_meta_describe(const GigTypeMeta *meta)
{
    GString *s = g_string_new(NULL);
    g_string_append_printf(s, "%s%s%s",
                           meta->is_ptr ? "pointer to " : "",
                           meta->is_caller_allocates ? "caller allocated " : "",
                           g_type_name(meta->gtype));
    if (!G_TYPE_IS_FUNDAMENTAL(meta->gtype))
        g_string_append_printf(s, " of type %s", g_type_name(G_TYPE_FUNDAMENTAL(meta->gtype)));
    if (meta->is_nullable)
        g_string_append_printf(s, " or NULL");
    strncpy(gig_data_type_describe_buf, s->str, STRLEN - 1);
    gig_data_type_describe_buf[STRLEN - 1] = '\0';
    g_string_free(s, TRUE);
    return gig_data_type_describe_buf;
}

#undef STRLEN

void
gig_init_data_type(void)
{
}
