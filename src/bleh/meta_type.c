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

#include <stdio.h>
#include <stdbool.h>
#include "meta_type.h"

#define gig_critical_load(...)

static char payload_strings[META_COUNT][13] = {
    [META_NONE] = "none",
    [META_VOID] = "void",
    [META_INVALID] = "invalid",
    
    [META_BOOLEAN] = "boolean",
    [META_INT8] = "int8",
    [META_UINT8] = "uint8",
    [META_INT16] = "int16",
    [META_UINT16] = "uint16",
    [META_INT32] = "int32",
    [META_UINT32] = "uint32",
    [META_INT64] = "int64",
    [META_UINT64] = "uint64",
    [META_FLOAT] = "float",
    [META_DOUBLE] = "double",
    [META_GTYPE] = "GType",
    [META_UTF8_STRING] = "UTF-8 string",
    [META_LOCALE_STRING] = "string",
    [META_UNICHAR] = "unichar",

    // Types that use the params for extra info
    [META_POINTER] = "pointer",
    [META_ARRAY] = "array",
    [META_GARRAY] = "GArray",
    [META_GLIST] = "GList",
    [META_GSLIST] = "GSList",
    [META_GHASH] = "GHash",
    [META_GERROR] = "GError",
    [META_GBYTEARRAY] = "GByteArray",
    [META_GPTRARRAY] = "GPtrArray",
    
    // The base types of interface types
    [META_STRUCT] = "struct",
    [META_ENUM] = "enum",
    [META_FLAGS] = "flags",
    [META_INTERFACE] = "interface",
    [META_OBJECT] = "object",
    [META_UNION] = "union",
    [META_CALLBACK] = "callback",
    
    [META_OTHER] = "other"
};

static void
init_from_type_info(MetaType *meta, GITypeInfo *type_info);
static void
add_child_params(MetaType *meta, GITypeInfo *type_info, int n);
static void
init_from_basic_type_tag(MetaType *meta, GITypeTag tag);

#define STRLEN 128
static char describe_buf[STRLEN];

const char *
meta_type_describe(const MetaType *meta)
{
    snprintf(describe_buf, STRLEN,
             "%s%s%s%s",
             meta->is_ptr ? "pointer to " : "",
             meta->is_caller_allocates ? "caller allocated " : "",
             (meta->gtype == 0) ? payload_strings[meta->payload]
             : g_type_name(meta->gtype),
             meta->is_nullable ? " or NULL" : "");
    return describe_buf;
}

#undef STRLEN

// For arguments, but not return values
void meta_type_init_from_arg_info(MetaType *meta, GIArgInfo *ai)
{
    GITypeInfo *type_info = g_arg_info_get_type(ai);
    GIDirection dir = g_arg_info_get_direction(ai);
    MetaTransfer transfer = (MetaTransfer) g_arg_info_get_ownership_transfer(ai);

    init_from_type_info(meta, type_info);

    meta->is_in = (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT);
    meta->is_out = (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT);
    meta->is_skip = g_arg_info_is_skip(ai);

    meta->is_caller_allocates = g_arg_info_is_caller_allocates(ai);
    meta->is_optional = g_arg_info_is_optional(ai);
    meta->is_nullable = g_arg_info_may_be_null(ai);

    meta->transfer = transfer;
    g_base_info_unref(type_info);
}

// For return types only
void
meta_type_init_from_callable_info(MetaType *meta, GICallableInfo *ci)
{
    GITypeInfo *type_info = g_callable_info_get_return_type(ci);
    MetaTransfer transfer = (MetaTransfer) g_callable_info_get_caller_owns(ci);

    init_from_type_info(meta, type_info);

    meta->is_in = false;
    if (meta->payload != META_NONE && meta->gtype != META_INVALID)
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
init_from_type_info(MetaType *meta, GITypeInfo *type_info)
{
    GITypeTag tag = g_type_info_get_tag(type_info);
    meta->is_ptr = g_type_info_is_pointer(type_info);

    if (tag == GI_TYPE_TAG_VOID) {
        if (meta->is_ptr)
            meta->payload = META_POINTER;
        else
            meta->payload = META_NONE;
        meta->item_size = 0;
    }
    else if (tag == GI_TYPE_TAG_ARRAY) {
        GIArrayType array_type = g_type_info_get_array_type(type_info);
        int len;
        meta->item_size = sizeof(void *);

        add_child_params(meta, type_info, 1);

        if (array_type == GI_ARRAY_TYPE_C) {
            meta->payload = META_ARRAY;
            meta->is_raw_array = true;
            meta->length = META_ARRAY_SIZE_UNKNOWN;

            if ((len = g_type_info_get_array_length(type_info)) != -1)
                meta->has_size = true;
            else if ((len = g_type_info_get_array_fixed_size(type_info)) != -1)
                meta->length = len;

            if (g_type_info_is_zero_terminated(type_info))
                meta->is_zero_terminated = true;

            if (len == -1 && !meta->is_zero_terminated) {
                gig_critical_load("no way of determining array size of %s, coercing to pointer",
                                  g_base_info_get_name(type_info));
                meta->payload = META_POINTER;
            }
        }
        else if (array_type == GI_ARRAY_TYPE_ARRAY)
            meta->payload = META_GARRAY;
        else if (array_type == GI_ARRAY_TYPE_BYTE_ARRAY)
            meta->payload = META_GBYTEARRAY;
        else if (array_type == GI_ARRAY_TYPE_PTR_ARRAY)
            meta->payload = META_GPTRARRAY;
        else
            abort();
    }
    else if (tag == GI_TYPE_TAG_GHASH) {
        meta->payload = META_GHASH;
        meta->item_size = sizeof(GHashTable *);
        add_child_params(meta, type_info, 2);
    }
    else if (tag == GI_TYPE_TAG_GLIST) {
        meta->payload = META_GLIST;
        meta->item_size = sizeof(GList *);
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_GSLIST) {
        meta->payload = META_GSLIST;
        meta->item_size = sizeof(GSList *);
        add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *referenced_base_info = g_type_info_get_interface(type_info);
        GIInfoType itype = g_base_info_get_type(referenced_base_info);

        meta->item_size = sizeof(void *);
        
        switch (itype) {
        case GI_INFO_TYPE_UNRESOLVED:
            meta->payload = META_INVALID;
            meta->is_invalid = true;
            gig_critical_load("Unrepresentable type: %s, %s, %s",
                              g_base_info_get_name_safe(type_info),
                              g_base_info_get_name_safe(referenced_base_info),
                              g_info_type_to_string(itype));
            break;
        case GI_INFO_TYPE_ENUM:
            meta->payload = META_ENUM;
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (meta->gtype == G_TYPE_NONE)
                meta->info = g_base_info_ref(referenced_base_info);
            break;
        case GI_INFO_TYPE_FLAGS:
            meta->payload = META_FLAGS;
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (meta->gtype == G_TYPE_NONE)
                meta->info = g_base_info_ref(referenced_base_info);
            break;
        case GI_INFO_TYPE_STRUCT:
            meta->payload = META_STRUCT;
            meta->item_size = g_struct_info_get_size(referenced_base_info);
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (meta->gtype == G_TYPE_NONE)
                meta->is_invalid = true;
            break;
        case GI_INFO_TYPE_UNION:
            meta->payload = META_UNION;
            meta->item_size = g_union_info_get_size(referenced_base_info);
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (meta->gtype == G_TYPE_NONE)
                meta->is_invalid = true;
            break;
        case GI_INFO_TYPE_OBJECT:
            meta->payload = META_OBJECT;
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (meta->gtype == G_TYPE_NONE)
                meta->is_invalid = true;
            break;
        case GI_INFO_TYPE_INTERFACE:
            meta->payload = META_INTERFACE;
            meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
            if (meta->gtype == G_TYPE_NONE)
                meta->is_invalid = true;
            break;
        case GI_INFO_TYPE_CALLBACK:
            meta->payload = META_CALLBACK;
            meta->info = g_base_info_ref(referenced_base_info);

#if 0            
            // TODO: reuse this amap somehow?
            GigArgMap *_amap = gig_amap_new(NULL, meta->callable_info);
            if (_amap == NULL)
                meta->is_invalid = true;
            else
                gig_amap_free(_amap);
#endif
            break;
        default:
            meta->payload = META_OTHER;
            if (GI_IS_REGISTERED_TYPE_INFO(referenced_base_info)) {
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                if (meta->gtype == G_TYPE_NONE)
                    meta->is_invalid = true;
            }
            else {
                gig_critical_load("Unhandled item type in %s:%d", __FILE__, __LINE__);
                meta->is_invalid = true;
            }
        }

        g_base_info_unref(referenced_base_info);
    }
    else
        init_from_basic_type_tag(meta, tag);
}

static void
add_child_params(MetaType *meta, GITypeInfo *type_info, int n)
{
    GITypeInfo *param_type;

    meta->params = calloc(n, sizeof(MetaType));
    meta->n_params = n;

    for (int i = 0; i < n; i++) {
        param_type = g_type_info_get_param_type((GITypeInfo *)type_info, i);
        init_from_type_info(&meta->params[i], param_type);
        g_base_info_unref(param_type);

        if (meta->transfer == META_TRANSFER_EVERYTHING)
            meta->params[i].transfer = META_TRANSFER_EVERYTHING;
        else
            meta->params[i].transfer = META_TRANSFER_NOTHING;

        meta->is_invalid |= meta->params[i].is_invalid;
    }
}

static void
init_from_basic_type_tag(MetaType *meta, GITypeTag tag)
{
#define T(TYPETAG,PAYLOAD,CTYPE)                       \
    do {                                        \
        if (tag == TYPETAG) {                   \
            meta->payload = PAYLOAD;          \
            meta->item_size = sizeof (CTYPE);   \
            return;                             \
        }                                       \
    } while(false)

    T(GI_TYPE_TAG_BOOLEAN, META_BOOLEAN, gboolean);   // gboolean != bool
    T(GI_TYPE_TAG_DOUBLE, META_DOUBLE, double);
    T(GI_TYPE_TAG_FLOAT, META_FLOAT, float);
    T(GI_TYPE_TAG_GTYPE, META_GTYPE, GType);
    T(GI_TYPE_TAG_INT8, META_INT8, int8_t);
    T(GI_TYPE_TAG_INT16, META_INT16, int16_t);
    T(GI_TYPE_TAG_INT32, META_INT32, int32_t);
    T(GI_TYPE_TAG_INT64, META_INT64, int64_t);
    T(GI_TYPE_TAG_UINT8, META_UINT8, uint8_t);
    T(GI_TYPE_TAG_UINT16, META_UINT16, uint16_t);
    T(GI_TYPE_TAG_UINT32, META_UINT32, uint32_t);
    T(GI_TYPE_TAG_UINT64, META_UINT64, uint64_t);
    if (tag == GI_TYPE_TAG_UNICHAR) {
        meta->payload = META_UNICHAR;
        meta->item_size = sizeof(uint32_t);
        return;
    }
    if (tag == GI_TYPE_TAG_UTF8) {
        meta->payload = META_UTF8_STRING;
        return;
    }
    if (tag == GI_TYPE_TAG_FILENAME) {
        meta->payload = META_LOCALE_STRING;
        return;
    }
    T(GI_TYPE_TAG_ERROR, META_GERROR, GError);
    g_error("unhandled type '%s' %s %d", g_type_tag_to_string(tag), __FILE__, __LINE__);
#undef T
}


#if 0
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




size_t
meta_type_real_item_size(const MetaType *meta)
{
    return meta->item_size;
}

void
meta_type_free(MetaType *meta)
{
    for (int i = 0; i < meta->n_params; i++)
        meta_type_free(&meta->params[i]);
    if (meta->n_params > 0)
        free(meta->params);

    if (meta->payload == META_CALLBACK)
        g_base_info_unref(meta->info);
    if (meta->payload == META_ENUM || meta->payload == META_FLAGS) {
        if (meta->info)
            g_base_info_unref(meta->info);
    }
}
#endif

int main()
{
    MetaType *meta;
    return 1;
}
