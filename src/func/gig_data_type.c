// Copyright (C) 2019, 2022 Michael L. Gran

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
#include <glib-object.h>
#include "../core.h"
#include "../type.h"
#include "gig_argument.h"
#include "gig_data_type.h"
#include "gig_arg_map_priv.h"
#include "gig_util_priv.h"

static char arg_type_names[GIG_ARG_TYPE_N_ARGS][64] = {
    [GIG_ARG_TYPE_UNKNOWN] = "unknown",
    [GIG_ARG_TYPE_VOID] = "void",

    [GIG_ARG_TYPE_INT8] = "int8",
    [GIG_ARG_TYPE_UINT8] = "uint8",
    [GIG_ARG_TYPE_INT16] = "int16",
    [GIG_ARG_TYPE_UINT16] = "uint16",
    [GIG_ARG_TYPE_INT32] = "int32",
    [GIG_ARG_TYPE_UINT32] = "uint32",
    [GIG_ARG_TYPE_INT64] = "int64",
    [GIG_ARG_TYPE_UINT64] = "uint64",
    [GIG_ARG_TYPE_UNICHAR] = "unichar",
    [GIG_ARG_TYPE_FLOAT] = "float",
    [GIG_ARG_TYPE_DOUBLE] = "double",
    [GIG_ARG_TYPE_POINTER] = "pointer",

    [GIG_ARG_TYPE_UTF8_STRING] = "utf8-string",
    [GIG_ARG_TYPE_LOCALE_STRING] = "locale-string",
    [GIG_ARG_TYPE_ARRAY] = "array",

    [GIG_ARG_TYPE_GBOOLEAN] = "gboolean",
    [GIG_ARG_TYPE_GTYPE] = "GType",
    [GIG_ARG_TYPE_GERROR] = "GError",
    [GIG_ARG_TYPE_BOXED] = "GBoxed",
    [GIG_ARG_TYPE_INTERFACE] = "GInterface",
    [GIG_ARG_TYPE_OBJECT] = "GObject",
    [GIG_ARG_TYPE_ENUM] = "enum",
    [GIG_ARG_TYPE_FLAGS] = "flags",
    [GIG_ARG_TYPE_VARIANT] = "GVariant",
    [GIG_ARG_TYPE_VALUE] = "GValue",
    [GIG_ARG_TYPE_PARAM] = "GParam",
    [GIG_ARG_TYPE_CALLBACK] = "callback",
    [GIG_ARG_TYPE_OTHER] = "other",

    [GIG_ARG_TYPE_GARRAY] = "GArray",
    [GIG_ARG_TYPE_GPTRARRAY] = "GPtrArray",
    [GIG_ARG_TYPE_GBYTEARRAY] = "GByteArray",
    [GIG_ARG_TYPE_GLIST] = "GList",
    [GIG_ARG_TYPE_GSLIST] = "GSList",
    [GIG_ARG_TYPE_GHASH] = "GHash",
};

static void gig_type_meta_init_from_type_info(GigTypeMeta *type, GITypeInfo *ti);
static void gig_type_meta_init_from_basic_type_tag(GigTypeMeta *meta, GITypeTag tag);

static GigTransfer
convert_transfer(GITransfer x)
{
    if (x == GI_TRANSFER_NOTHING)
        return GIG_TRANSFER_NOTHING;
    if (x == GI_TRANSFER_CONTAINER)
        return GIG_TRANSFER_CONTAINER;
    return GIG_TRANSFER_EVERYTHING;
}


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

    meta->transfer = convert_transfer(transfer);
    g_base_info_unref(type_info);
}

void
gig_type_meta_init_from_callable_info(GigTypeMeta *meta, GICallableInfo *ci)
{
    GITypeInfo *type_info = g_callable_info_get_return_type(ci);
    GITransfer transfer = g_callable_info_get_caller_owns(ci);

    gig_type_meta_init_from_type_info(meta, type_info);

    meta->is_in = false;
    if (meta->arg_type != GIG_ARG_TYPE_UNKNOWN && meta->arg_type != GIG_ARG_TYPE_VOID)
        meta->is_out = true;
    else
        meta->is_out = false;
    meta->is_skip = g_callable_info_skip_return(ci);

    meta->is_caller_allocates = false;
    meta->is_optional = false;
    meta->is_nullable = g_callable_info_may_return_null(ci);

    meta->transfer = convert_transfer(transfer);
    g_base_info_unref(type_info);
}

static void
add_params(GigTypeMeta *meta, int n)
{
    meta->params = xcalloc(n, sizeof(GigTypeMeta));
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

        if (meta->transfer == GIG_TRANSFER_EVERYTHING)
            meta->params[i].transfer = GIG_TRANSFER_EVERYTHING;
        else
            meta->params[i].transfer = GIG_TRANSFER_NOTHING;

        meta->is_invalid |= meta->params[i].is_invalid;
    }
}

static void
gig_type_meta_init_from_basic_type_tag(GigTypeMeta *meta, GITypeTag tag)
{
#define T(TYPETAG,ATYPE,CTYPE)                  \
    do {                                        \
        if (tag == TYPETAG) {                   \
            meta->arg_type = ATYPE;             \
            meta->item_size = sizeof (CTYPE);   \
            return;                             \
        }                                       \
    } while(false)

    T(GI_TYPE_TAG_BOOLEAN, GIG_ARG_TYPE_GBOOLEAN, gboolean);
    T(GI_TYPE_TAG_DOUBLE, GIG_ARG_TYPE_DOUBLE, double);
    T(GI_TYPE_TAG_FLOAT, GIG_ARG_TYPE_FLOAT, float);
    T(GI_TYPE_TAG_GTYPE, GIG_ARG_TYPE_GTYPE, GType);
    T(GI_TYPE_TAG_INT8, GIG_ARG_TYPE_INT8, int8_t);
    T(GI_TYPE_TAG_INT16, GIG_ARG_TYPE_INT16, int16_t);
    T(GI_TYPE_TAG_INT32, GIG_ARG_TYPE_INT32, int32_t);
    T(GI_TYPE_TAG_INT64, GIG_ARG_TYPE_INT64, int64_t);
    T(GI_TYPE_TAG_UINT8, GIG_ARG_TYPE_UINT8, uint8_t);
    T(GI_TYPE_TAG_UINT16, GIG_ARG_TYPE_UINT16, uint16_t);
    T(GI_TYPE_TAG_UINT32, GIG_ARG_TYPE_UINT32, uint32_t);
    T(GI_TYPE_TAG_UINT64, GIG_ARG_TYPE_UINT64, uint64_t);
    T(GI_TYPE_TAG_UNICHAR, GIG_ARG_TYPE_UNICHAR, uint32_t);
    T(GI_TYPE_TAG_UTF8, GIG_ARG_TYPE_UTF8_STRING, char *);
    T(GI_TYPE_TAG_FILENAME, GIG_ARG_TYPE_LOCALE_STRING, char *);
    T(GI_TYPE_TAG_ERROR, GIG_ARG_TYPE_GERROR, GError);
    gig_error("unhandled type '%s' %s %d", g_type_tag_to_string(tag), __FILE__, __LINE__);
#undef T
}

static void
gig_type_meta_init_from_type_info(GigTypeMeta *meta, GITypeInfo *type_info)
{
    GITypeTag tag = g_type_info_get_tag(type_info);
    meta->is_ptr = g_type_info_is_pointer(type_info);

    if (tag == GI_TYPE_TAG_VOID) {
        if (meta->is_ptr)
            meta->arg_type = GIG_ARG_TYPE_POINTER;
        else
            meta->arg_type = GIG_ARG_TYPE_VOID;
    }
    else if (tag == GI_TYPE_TAG_ARRAY) {
        GIArrayType array_type = g_type_info_get_array_type(type_info);
        add_child_params(meta, type_info, 1);

        switch (array_type) {
        case GI_ARRAY_TYPE_C:
        {
            meta->arg_type = GIG_ARG_TYPE_ARRAY;
            int length_arg = g_type_info_get_array_length(type_info);
            int fixed_size = g_type_info_get_array_fixed_size(type_info);

            if (length_arg != -1) {
                meta->has_length_arg = true;
                meta->length_arg = length_arg;
            }
            if (fixed_size != -1) {
                meta->has_fixed_size = true;
                meta->fixed_size = fixed_size;
            }
            if (g_type_info_is_zero_terminated(type_info))
                meta->is_zero_terminated = true;
            if (!meta->has_length_arg && !meta->has_fixed_size && !meta->is_zero_terminated) {
                gig_debug_load
                    ("no way of determining array size of C array %s of %s, coercing to pointer",
                     g_base_info_get_namespace(type_info), g_type_name(meta->params[0].gtype));
                meta->arg_type = GIG_ARG_TYPE_POINTER;
            }
        }
            break;
        case GI_ARRAY_TYPE_ARRAY:
            meta->arg_type = GIG_ARG_TYPE_GARRAY;
            break;
        case GI_ARRAY_TYPE_BYTE_ARRAY:
            meta->arg_type = GIG_ARG_TYPE_GBYTEARRAY;
            break;
        case GI_ARRAY_TYPE_PTR_ARRAY:
            meta->arg_type = GIG_ARG_TYPE_GPTRARRAY;
            break;
        }
    }
    else if (tag == GI_TYPE_TAG_GHASH) {
        meta->arg_type = GIG_ARG_TYPE_GHASH;
        meta->item_size = sizeof(GHashTable *);
        add_child_params(meta, type_info, 2);
    }
    else if (tag == GI_TYPE_TAG_GLIST) {
        meta->arg_type = GIG_ARG_TYPE_GLIST;
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_GSLIST) {
        meta->arg_type = GIG_ARG_TYPE_GSLIST;
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *referenced_base_info;
        GIInfoType itype;
        GType fundamental_gtype;
        GigArgMap *_amap;

        referenced_base_info = g_type_info_get_interface(type_info);
        if (referenced_base_info == NULL)
            meta->is_invalid = true;
        else {
            itype = g_base_info_get_type(referenced_base_info);
            switch (itype) {
            case GI_INFO_TYPE_INVALID:
            case GI_INFO_TYPE_FUNCTION:
                meta->is_invalid = true;
                break;
            case GI_INFO_TYPE_CALLBACK:
                meta->arg_type = GIG_ARG_TYPE_CALLBACK;
                meta->callable_info = g_base_info_ref(referenced_base_info);
                _amap = gig_amap_new(NULL, meta->callable_info);
                if (_amap == NULL)
                    meta->is_invalid = true;
                else
                    gig_amap_free(_amap);
                break;
            case GI_INFO_TYPE_STRUCT:
                meta->arg_type = GIG_ARG_TYPE_BOXED;
                meta->item_size = g_struct_info_get_size(referenced_base_info);
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_BOXED:
                meta->is_invalid = true;
                break;
            case GI_INFO_TYPE_ENUM:
                meta->arg_type = GIG_ARG_TYPE_ENUM;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                if (meta->gtype == G_TYPE_NONE) {
                    meta->qname = gig_type_get_qualified_name(referenced_base_info);
                    meta->gtype = G_TYPE_INVALID;
                }
                break;
            case GI_INFO_TYPE_FLAGS:
                meta->arg_type = GIG_ARG_TYPE_FLAGS;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                if (meta->gtype == G_TYPE_NONE) {
                    meta->qname = gig_type_get_qualified_name(referenced_base_info);
                    meta->gtype = G_TYPE_INVALID;
                }
                break;
            case GI_INFO_TYPE_OBJECT:
                meta->arg_type = GIG_ARG_TYPE_OBJECT;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_INTERFACE:
                meta->arg_type = GIG_ARG_TYPE_INTERFACE;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                fundamental_gtype = G_TYPE_FUNDAMENTAL(meta->gtype);
                if (fundamental_gtype == G_TYPE_BOXED) {
                    meta->arg_type = GIG_ARG_TYPE_BOXED;
                    meta->item_size = g_struct_info_get_size(referenced_base_info);
                }
                else if (fundamental_gtype == G_TYPE_INTERFACE)
                    meta->arg_type = GIG_ARG_TYPE_INTERFACE;
                else {
                    printf("FOOBAR\n");
                    meta->is_invalid = true;
                }
                break;
            case GI_INFO_TYPE_CONSTANT:
            case GI_INFO_TYPE_INVALID_0:
                meta->is_invalid = true;
            case GI_INFO_TYPE_UNION:
                meta->arg_type = GIG_ARG_TYPE_BOXED;
                meta->item_size = g_union_info_get_size(referenced_base_info);
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_VALUE:
                meta->arg_type = GIG_ARG_TYPE_VALUE;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_SIGNAL:
            case GI_INFO_TYPE_VFUNC:
            case GI_INFO_TYPE_PROPERTY:
            case GI_INFO_TYPE_FIELD:
            case GI_INFO_TYPE_ARG:
                meta->is_invalid = true;
                break;
            case GI_INFO_TYPE_TYPE:
                meta->arg_type = GIG_ARG_TYPE_GTYPE;
                break;
            case GI_INFO_TYPE_UNRESOLVED:
                meta->is_invalid = true;
                break;
            }
/*                
            if (fundamental_gtype == G_TYPE_BOXED) {
                meta->arg_type = GIG_ARG_TYPE_BOXED;
                if (itype == GI_INFO_TYPE_STRUCT)
                    meta->item_size = g_struct_info_get_size(referenced_base_info);
                else if (itype == GI_INFO_TYPE_UNION)
                    meta->item_size = g_union_info_get_size(referenced_base_info);
                if (meta->gtype == G_TYPE_VALUE)
                    meta->arg_type = GIG_ARG_TYPE_VALUE;
                meta->gtype = gtype;
            }
            else if (fundamental_gtype == G_TYPE_ENUM) {
            }
            else if (fundamental_gtype == G_TYPE_FLAGS) {
                meta->arg_type = GIG_ARG_TYPE_FLAGS;
                meta->gtype = gtype;
            }
            else if (fundamental_gtype == G_TYPE_INTERFACE) {
                meta->arg_type = GIG_ARG_TYPE_INTERFACE;
                meta->gtype = gtype;
            }
            else if (fundamental_gtype == G_TYPE_OBJECT) {
                meta->arg_type = GIG_ARG_TYPE_OBJECT;
                meta->gtype = gtype;
            }
            else if (fundamental_gtype == G_TYPE_PARAM) {
                meta->arg_type = GIG_ARG_TYPE_PARAM;
            }
            else if (fundamental_gtype == G_TYPE_VARIANT) {
                meta->arg_type = GIG_ARG_TYPE_VARIANT;
            }
            else if (fundamental_gtype == G_TYPE_NONE) {
                if (itype == GI_INFO_TYPE_ENUM) {
                    meta->arg_type = GIG_ARG_TYPE_ENUM;
                    meta->qname = gig_type_get_qualified_name(referenced_base_info);
                }
                else if (itype == GI_INFO_TYPE_FLAGS) {
                    meta->arg_type = GIG_ARG_TYPE_FLAGS;
                    meta->qname = gig_type_get_qualified_name(referenced_base_info);
                }
                else if (itype == GI_INFO_TYPE_CALLBACK) {
                    meta->arg_type = GIG_ARG_TYPE_CALLBACK;
                    meta->callable_info = g_base_info_ref(referenced_base_info);
                    _amap = gig_amap_new(NULL, meta->callable_info);
                    if (_amap == NULL)
                        meta->is_invalid = true;
                    else
                        gig_amap_free(_amap);
                }
                else
                    meta->is_invalid = true;
            }
            else {
                // Newly defined fundamental types.
            }
            } */
        }
        assert(meta->arg_type != 0 || meta->is_invalid);
        g_base_info_unref(referenced_base_info);
    }
    else
        gig_type_meta_init_from_basic_type_tag(meta, tag);

    printf("%s %s %s %s\n",
           (meta->is_invalid ? "INVALID" : "VALID"),
           g_type_tag_to_string(tag), arg_type_names[meta->arg_type], g_type_name(meta->gtype));
    fflush(stdout);
    assert(meta->arg_type <= GIG_ARG_TYPE_GHASH);
    assert(meta->arg_type != 0 || meta->is_invalid);
}

size_t
gig_meta_real_item_size(const GigTypeMeta *meta)
{
    if (meta->arg_type == GIG_ARG_TYPE_UTF8_STRING
        || meta->arg_type == GIG_ARG_TYPE_LOCALE_STRING
        || meta->arg_type == GIG_ARG_TYPE_POINTER || meta->is_ptr)
        return sizeof(void *);
    return meta->item_size;
}

void
gig_data_type_free(GigTypeMeta *meta)
{
    for (int i = 0; i < meta->n_params; i++)
        gig_data_type_free(&meta->params[i]);
    if (meta->n_params > 0)
        free(meta->params);

    if (meta->callable_info) {
        g_base_info_unref(meta->callable_info);
        meta->callable_info = NULL;
    }
    free(meta->qname);
    meta->qname = NULL;
}

#define STRLEN 128
char gig_data_type_describe_buf[STRLEN];

const char *
gig_type_meta_describe(const GigTypeMeta *meta)
{
    size_t len = 0;
    const char *gtype_name = NULL;
    assert(meta->arg_type < GIG_ARG_TYPE_N_ARGS);
    if (meta->gtype)
        gtype_name = g_type_name(meta->gtype);
    len = snprintf(gig_data_type_describe_buf, STRLEN,
                   "%s%s%s%s%s",
                   (meta->is_ptr ? "pointer to " : ""),
                   (meta->is_caller_allocates ? "caller allocated " : ""),
                   arg_type_names[meta->arg_type],
                   (gtype_name ? " of type " : ""), (gtype_name ? gtype_name : ""));
    if (meta->is_nullable && len < STRLEN)
        snprintf(gig_data_type_describe_buf + len, STRLEN - len, " or NULL");
    return gig_data_type_describe_buf;
}

#undef STRLEN
