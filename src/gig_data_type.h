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

#ifndef GIG_DATA_TYPE_H
#define GIG_DATA_TYPE_H
#include <glib.h>
#include <girepository.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

#define GIG_ARRAY_SIZE_UNKNOWN ((gsize)-1)

typedef struct _GigTypeMeta GigTypeMeta;
struct _GigTypeMeta
{
    GType gtype;
    guint16 is_ptr:1;

    // For argument and return values
    guint16 is_in:1;
    guint16 is_out:1;

    // For return values
    guint16 is_skip:1;          // TRUE when output is ignored

    // For pointers
    guint16 is_caller_allocates:1;
    guint16 is_optional:1;      // Out-only. Pass in NULL to ignore this.
    guint16 is_nullable:1;      // For in, can pass in NULL. For out, may return NULL.

    // Error status
    guint16 is_invalid:1;       // True when one of the arguments has invalid type
    guint16 is_raw_array:1;
    guint16 is_zero_terminated:1;
    guint16 has_size:1;
    guint16 is_unichar:1;
    guint16 padding1:4;

    union
    {
        // For string and pointer types
        enum
        {
            GIG_DATA_VOID = 0,
            GIG_DATA_UTF8_STRING,
            GIG_DATA_LOCALE_STRING,
            GIG_DATA_LIST,
            GIG_DATA_SLIST,
            GIG_DATA_HASH_TABLE,
            GIG_DATA_CALLBACK
        } pointer_type;

        // For C array types
        gsize length;
        // For C element types
        gsize item_size;
    };

    GITransfer transfer;

    // Subtypes and callables
    guint16 n_params;
    union
    {
        GigTypeMeta *params;
        GICallableInfo *callable_info;
        GIEnumInfo *enum_info;
    };
};

void gig_type_meta_init_from_arg_info(GigTypeMeta *type, GIArgInfo *ai);
void gig_type_meta_init_from_callable_info(GigTypeMeta *type, GICallableInfo *ci);
G_GNUC_PURE gsize gig_meta_real_item_size(const GigTypeMeta *meta);
const char *gig_type_meta_describe(const GigTypeMeta *meta);
void gig_data_type_free(GigTypeMeta *meta);
void gig_init_data_type(void);

G_END_DECLS
#endif
