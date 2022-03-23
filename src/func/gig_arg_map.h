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

#ifndef GIG_ARG_MAP_PUB_H
#define GIG_ARG_MAP_PUB_H

#include <stdbool.h>
#include <stdint.h>
#include <girepository.h>
#include "gig_data_type.h"

// This module gathers information to help convert between 3 paradigms
//  - arguments as they appear in GIArgInfo lists
//  - arguments as they appear in SCM gsubr calls
//  - arguments as they appear in g_function_info_invoke calls
typedef enum
{
    GIG_ARG_DIRECTION_VOID,
    GIG_ARG_DIRECTION_INPUT,
    GIG_ARG_DIRECTION_INOUT,
    GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT,
    GIG_ARG_DIRECTION_OUTPUT,
    GIG_ARG_DIRECTION_COUNT
} GigArgDirection;

typedef enum
{
    GIG_ARG_TUPLE_SINGLETON,
    GIG_ARG_TUPLE_ARRAY,
    GIG_ARG_TUPLE_ARRAY_SIZE,
    GIG_ARG_TUPLE_COUNT
} GigArgTuple;

typedef enum
{
    GIG_ARG_PRESENCE_REQUIRED,
    GIG_ARG_PRESENCE_OPTIONAL,
    GIG_ARG_PRESENCE_IMPLICIT,
    GIG_ARG_PRESENCE_COUNT
} GigArgPresence;

// This structure contains the information necessary for choosing how
// to convert between scheme and C arguments of a function or method
// call.
typedef struct _GigArgMapEntry GigArgMapEntry;
struct _GigArgMapEntry
{
    char *name;
    GigTypeMeta meta;

    ////////////////////////////////////////////////////////////////
    // This block is derived information about how to map
    // Scheme to C and back again

    GigArgDirection s_direction;
    // If this argument can be mapped standalone, or requires another
    // argument's information to map between C and Scheme
    GigArgTuple tuple;
    // If this arg is optional in the Scheme GSubr.
    GigArgPresence presence;
    // This arg's index in g_callable_info_get_arg()

    uint8_t is_c_input:1;
    uint8_t is_c_output:1;
    uint8_t is_s_input:1;
    uint8_t is_s_output:1;
    uint8_t padding:4;

    int i;
    // This arg's position in input args of g_function_info_invoke
    int c_input_pos;
    // This arg's position in the output args of g_function_info_invoke
    int c_output_pos;
    // This arg's position in the Scheme GSubr
    int s_input_pos;
    // This arg's position in the return values list
    int s_output_pos;
    // When non-NULL, this is the entry of the array length argument
    // for this array argument.
};

typedef struct _GigArgMap GigArgMap;
struct _GigArgMap
{
    char *name;

    bool is_method;
    bool can_throw_gerror;

    // S arguments.
    int s_input_req;
    int s_input_opt;

    // S return values
    int s_output_len;

    // For g_function_invoke call
    int c_input_len;
    int c_output_len;

    // If this arg map has invalid entries
    bool is_invalid;

    // An array of arg_map_entry
    GigArgMapEntry *pdata;
    int len;

    GigArgMapEntry return_val;
};

GigArgMap *gig_amap_new(const char *name, GICallableInfo *function_info);
void gig_amap_free(GigArgMap *am);
GigArgMapEntry *gig_amap_get_input_entry_by_s(GigArgMap *am, int spos);
GigArgMapEntry *gig_amap_get_output_entry_by_c(GigArgMap *am, int cpos);
void gig_amap_s_input_count(const GigArgMap *amap, int *required, int *optional);
void gig_amap_c_count(const GigArgMap *am, int *c_input_pos, int *c_output_pos);
bool gig_amap_return_child_i(const GigArgMap *amap, int *ichild);
bool gig_amap_output_i2c(const GigArgMap *amap, int i, int *c);

#endif
