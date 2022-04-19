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
#include "../type.h"

#define GIG_ARRAY_SIZE_UNKNOWN ((size_t)-1)

// All the categories of type conversions necessary for our FFI and
// SCM-to-C function argument conversion.
typedef enum GigArgType_
{
    GIG_ARG_TYPE_INVALID,
    GIG_ARG_TYPE_NONE,
    GIG_ARG_TYPE_INTERFACE,
    GIG_ARG_TYPE_BOOLEAN,
    GIG_ARG_TYPE_INT8,          /* Always signed */
    GIG_ARG_TYPE_UINT8,
    GIG_ARG_TYPE_INT16,
    GIG_ARG_TYPE_UINT16,
    GIG_ARG_TYPE_INT32,
    GIG_ARG_TYPE_UINT32,
    GIG_ARG_TYPE_INT64,
    GIG_ARG_TYPE_UINT64,
    GIG_ARG_TYPE_ENUM,
    GIG_ARG_TYPE_FLAGS,
    GIG_ARG_TYPE_FLOAT,
    GIG_ARG_TYPE_DOUBLE,
    GIG_ARG_TYPE_STRING,
    GIG_ARG_TYPE_POINTER,
    GIG_ARG_TYPE_BOXED,
    GIG_ARG_TYPE_PARAM,
    GIG_ARG_TYPE_OBJECT,
    GIG_ARG_TYPE_VARIANT,
    GIG_ARG_TYPE_OTHER
} GigArgType;

#define GIG_ARG_TYPE_N_ARGS (GIG_ARG_TYPE_GHASH + 1)

typedef enum _GigPointerType
{
    GIG_POINTER_VOID = 0,
    GIG_POINTER_C_ARRAY,
    GIG_POINTER_GTYPE,
    GIG_POINTER_LIST,
    GIG_POINTER_SLIST,
    GIG_POINTER_CALLBACK
} GigPointerType;

typedef enum _GigBoxedType
{
    GIG_BOXED_VOID,
    GIG_BOXED_GERROR,
    GIG_BOXED_GARRAY,
    GIG_BOXED_BYTE_ARRAY,
    GIG_BOXED_PTR_ARRAY,
    GIG_BOXED_HASH_TABLE,
    GIG_BOXED_VALUE
} GigBoxedType;

typedef enum _GigStringType
{
    GIG_STRING_VOID,
    GIG_STRING_LOCALE,
    GIG_STRING_UTF8
} GigStringType;

typedef struct GigTypeMeta_ GigTypeMeta;
struct GigTypeMeta_
{
    GigArgType arg_type;
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
        GigPointerType pointer_type;
        GigBoxedType boxed_type;
        GigStringType string_type;
    };
    // For C element types
    size_t item_size;

    GigTransfer transfer;

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
