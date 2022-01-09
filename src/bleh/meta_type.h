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

#ifndef CORE_META_TYPE_H
#define CORE_META_TYPE_H

#include <stdint.h>
#include <girepository.h>

#define META_ARRAY_SIZE_UNKNOWN ((size_t)-1)

typedef enum _MetaTypeCategory
{
    META_NONE,
    META_VOID,
    META_INVALID,

    // Simple types
    META_BOOLEAN,
    META_INT8,
    META_UINT8,
    META_INT16,
    META_UINT16,
    META_INT32,
    META_UINT32,
    META_INT64,
    META_UINT64,
    META_FLOAT,
    META_DOUBLE,
    META_GTYPE,
    META_UTF8_STRING,
    META_LOCALE_STRING,
    META_UNICHAR,

    // Types that use the params for extra info
    META_POINTER,
    META_ARRAY,
    META_GARRAY,
    META_GLIST,
    META_GSLIST,
    META_GHASH,
    META_GERROR,
    META_GBYTEARRAY,
    META_GPTRARRAY,
    
    // The base types of interface types
    META_STRUCT,
    META_ENUM,
    META_FLAGS,
    META_INTERFACE,
    META_OBJECT,
    META_UNION,
    META_CALLBACK,
    
    META_OTHER,
    META_COUNT
} MetaTypeCategory;

typedef enum _MetaTransfer
{
    META_TRANSFER_NOTHING = 0,
    META_TRANSFER_CONTAINER,
    META_TRANSFER_EVERYTHING
} MetaTransfer;

typedef struct _MetaType
{
    MetaTypeCategory payload;
    
    uint16_t is_ptr:1;

    // For argument and return values
    uint16_t is_in:1;
    uint16_t is_out:1;

    // For return values
    uint16_t is_skip:1;         // true when output is ignored

    // For pointers
    uint16_t is_caller_allocates:1;
    uint16_t is_optional:1;     // Out-only. Pass in NULL to ignore this.
    uint16_t is_nullable:1;     // For in, can pass in NULL. For out, may return NULL.

    // Error status
    uint16_t is_invalid:1;      // True when one of the arguments has invalid type
    uint16_t is_raw_array:1;
    uint16_t is_zero_terminated:1;
    uint16_t has_size:1;
    uint16_t is_unichar:1;
    uint16_t padding1:4;

    uint16_t n_params;

    MetaTransfer transfer;

    // For C array types
    size_t length;
    
    // For C element types, especially integers
    size_t item_size;

    // For lists and hash tables
    struct _MetaType *params;

    // Objects, structs, and most enums and flags
    size_t gtype;

    // Some odd enums and flags, and Other
    GIBaseInfo *info;
} MetaType;

void meta_type_init_from_arg_info(MetaType *type, GIArgInfo *ai);
void meta_type_init_from_callable_info(MetaType *type, GICallableInfo *ci);
size_t meta_type_real_item_size(const MetaType *meta);
const char *meta_type_describe(const MetaType *meta);
void meta_type_free(MetaType *meta);
#endif
