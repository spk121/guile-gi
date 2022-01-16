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

#ifndef Y_ARG_H
#define Y_ARG_H

#include <stdint.h>
#include <girepository.h>

// These are structures and functions that provide information about a
// single argument in a function call.

typedef enum _TypeTag
{
    TYPE_TAG_INVALID,
    TYPE_TAG_VOID,
    TYPE_TAG_POINTER,
    TYPE_TAG_BOOLEAN,
    TYPE_TAG_INT8,
    TYPE_TAG_UINT8,
    TYPE_TAG_INT16,
    TYPE_TAG_UINT16,
    TYPE_TAG_INT32,
    TYPE_TAG_UINT32,
    TYPE_TAG_INT64,
    TYPE_TAG_UINT64,
    TYPE_TAG_FLOAT,
    TYPE_TAG_DOUBLE,
    TYPE_TAG_GTYPE,
    TYPE_TAG_UTF8_STRING,
    TYPE_TAG_LOCALE_STRING,
    TYPE_TAG_ARRAY,
    TYPE_TAG_GARRAY,
    TYPE_TAG_GPTRARRAY,
    TYPE_TAG_GBYTEARRAY,
    TYPE_TAG_GLIST,
    TYPE_TAG_GSLIST,
    TYPE_TAG_GHASH,
    TYPE_TAG_ERROR,
    TYPE_TAG_UNICHAR,
    TYPE_TAG_FUNCTION,
    TYPE_TAG_CALLBACK,
    TYPE_TAG_STRUCT,
    TYPE_TAG_UNION,
    TYPE_TAG_ENUM,
    TYPE_TAG_FLAGS,
    TYPE_TAG_OBJECT,
    TYPE_TAG_INTERFACE,
    TYPE_TAG_CONSTANT,
    TYPE_TAG_OTHER
} Type_tag;

#define ARG_ARRAY_SIZE_UNKNOWN ((size_t)-1)

typedef enum _Transfer
{
    TRANSFER_NOTHING = 0,
    TRANSFER_CONTAINER,
    TRANSFER_EVERYTHING
} Transfer;

typedef struct _Arg
{
    Type_tag payload;
    
    // For argument and return values
    unsigned is_in:1;
    unsigned is_out:1;

    // For pointers
    unsigned is_ptr:1;
    unsigned is_caller_allocates:1;
    unsigned is_nullable:1;     // For in, can pass in NULL. For out, may return NULL.
    unsigned is_optional:1;     // Out-only. Pass in NULL to ignore this.
    unsigned transfer_ownership:1;
    unsigned transfer_container:1;
    unsigned is_return_value:1;
    unsigned scope:3;
    
    // Simple type info
    Type_tag tag;

    // For return values
    uint16_t is_skip:1;         // true when output is ignored


    // Error status
    uint16_t is_invalid:1;      // True when one of the arguments has invalid type
    uint16_t is_raw_array:1;
    uint16_t is_zero_terminated:1;
    uint16_t has_size:1;

    uint16_t n_params;

    Transfer transfer;

    // For C array types
    size_t length;
    
    // For C element types, especially integers
    size_t item_size;

    // For lists and hash tables
    struct _Arg *params;

    // Objects, structs, and most enums and flags
    size_t gtype;
    const char *name;

    // Some odd enums and flags, and Other
    GIEnumInfo *enum_info;
    GICallableInfo *callable_info;
} Arg;

void arg_init_from_arg_info(Arg *type, GIArgInfo *ai);
void arg_init_from_callable_info(Arg *type, GICallableInfo *ci);
size_t arg_real_item_size(const Arg *meta);
const char *arg_describe(const Arg *meta);
void arg_free(Arg *meta);
#endif
