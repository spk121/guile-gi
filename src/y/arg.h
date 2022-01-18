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

#define ARG_ARRAY_SIZE_UNKNOWN ((size_t)-1)

typedef enum _TypeTag
{
    TYPE_NONE,
    TYPE_INVALID,
    TYPE_VOID,
    TYPE_POINTER,
    TYPE_BOOLEAN,
    TYPE_INT8,
    TYPE_UINT8,
    TYPE_INT16,
    TYPE_UINT16,
    TYPE_INT32,
    TYPE_UINT32,
    TYPE_INT64,
    TYPE_UINT64,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_GTYPE,
    TYPE_UTF8_STRING,
    TYPE_LOCALE_STRING,
    TYPE_ARRAY,
    TYPE_GARRAY,
    TYPE_GPTRARRAY,
    TYPE_GBYTEARRAY,
    TYPE_GLIST,
    TYPE_GSLIST,
    TYPE_GHASH,
    TYPE_ERROR,
    TYPE_UNICHAR,
    TYPE_FUNCTION,
    TYPE_CALLBACK,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FLAGS,
    TYPE_OBJECT,
    TYPE_INTERFACE,
    TYPE_CONSTANT,
    TYPE_PARAM,
    TYPE_VARIANT,
    TYPE_OTHER
} Type_tag;



typedef struct _Arg
{
    Type_tag payload;
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
    
    // Simple type info
    Type_tag tag;

    union
    {
        // For C array types
        size_t length;
        // For C element types
        size_t item_size;
    };

    GITransfer transfer;

    // Subtypes and callables
    uint16_t n_params;
    union
    {
        struct _Arg *params;
        GICallableInfo *callable_info;
        GIEnumInfo *enum_info;
    };

    // Objects, structs, and most enums and flags
    size_t _gtype;
    const char *name;
} Arg;

void arg_init_from_arg_info(Arg *type, GIArgInfo *ai);
void arg_init_from_callable_info(Arg *type, GICallableInfo *ci);
size_t arg_item_size(const Arg *meta);
const char *arg_describe(const Arg *meta);
void arg_free(Arg *meta);
#endif
