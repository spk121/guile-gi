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

#ifndef GIG_DATA_TYPE_H
#define GIG_DATA_TYPE_H

#include <girepository.h>

#define GIG_ARRAY_SIZE_UNKNOWN ((size_t)-1)

typedef enum _GigPointerType
{
    GIG_DATA_VOID = 0,
    GIG_DATA_UTF8_STRING,
    GIG_DATA_LOCALE_STRING,
    GIG_DATA_LIST,
    GIG_DATA_SLIST,
    GIG_DATA_CALLBACK
} GigPointerType;

typedef struct GigTypeMeta_ GigTypeMeta;
struct GigTypeMeta_
{
    GType gtype;
    uint16_t is_ptr:1;

    // For argument and return values
    uint16_t is_in:1;
    uint16_t is_out:1;

    // For return values
    uint16_t is_skip:1;         // TRUE when output is ignored

    // For pointers
    uint16_t is_caller_allocates:1;
    uint16_t is_optional:1;     // Out-only. Pass in NULL to ignore this.
    uint16_t is_nullable:1;     // For in, can pass in NULL. For out, may return NULL.

    // Error status
    uint16_t is_invalid:1;      // True when one of the arguments has invalid type
    uint16_t is_zero_terminated:1;
    uint16_t has_length_arg:1;
    uint16_t has_fixed_size:1;
    uint16_t is_unichar:1;
    uint16_t padding1:4;

    // For C array types
    int length_arg;
    int fixed_size;

    union
    {
        // For string and pointer types
        GigPointerType pointer_type;
        // For C element types
        size_t item_size;
    };

    GITransfer transfer;

    // Subtypes and callables
    uint16_t n_params;
    union
    {
        struct GigTypeMeta_ *params;
        GICallableInfo *callable_info;
        GIEnumInfo *enum_info;
    };
};

void gig_type_meta_init_from_arg_info(GigTypeMeta *type, GIArgInfo *ai);
void gig_type_meta_init_from_callable_info(GigTypeMeta *type, GICallableInfo *ci);
size_t gig_meta_real_item_size(const GigTypeMeta *meta);
const char *gig_type_meta_describe(const GigTypeMeta *meta);
void gig_data_type_free(GigTypeMeta *meta);
void gig_init_data_type(void);

#endif
