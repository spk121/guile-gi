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

#ifndef ARG_MAP_H
#define ARG_MAP_H

#include <stdbool.h>
#include <stdint.h>
#include <girepository.h>
#include "arg.h"

// This module gathers information to help convert between 3 paradigms
//  - arguments as they appear in GIArgInfo lists
//  - arguments as they appear in SCM gsubr calls
//  - arguments as they appear in g_function_info_invoke calls
typedef enum
{
    ARG_DIRECTION_VOID,
    ARG_DIRECTION_INPUT,
    ARG_DIRECTION_INOUT,
    ARG_DIRECTION_PREALLOCATED_OUTPUT,
    ARG_DIRECTION_OUTPUT,
    ARG_DIRECTION_COUNT
} Arg_direction;

typedef enum
{
    ARG_TUPLE_SINGLETON,
    ARG_TUPLE_ARRAY,
    ARG_TUPLE_ARRAY_SIZE,
    ARG_TUPLE_COUNT
} Arg_tuple;

typedef enum
{
    ARG_PRESENCE_REQUIRED,
    ARG_PRESENCE_OPTIONAL,
    ARG_PRESENCE_IMPLICIT,
    ARG_PRESENCE_COUNT
} Arg_presence;

// This structure contains the information necessary for choosing how
// to convert between scheme and C arguments of a function or method
// call.
typedef struct _Arg_map_entry Arg_map_entry;
struct _Arg_map_entry
{
    char *name;
    Arg arg;

    ////////////////////////////////////////////////////////////////
    // This block is derived information about how to map
    // Scheme to C and back again

    Arg_direction s_direction;
    // If this argument can be mapped standalone, or requires another
    // argument's information to map between C and Scheme
    Arg_tuple tuple;
    // If this arg is optional in the Scheme GSubr.
    Arg_presence presence;
    // This arg's index in g_callable_info_get_arg()

    uint8_t is_c_input:1;
    uint8_t is_c_output:1;
    uint8_t is_s_input:1;
    uint8_t is_s_output:1;
    uint8_t padding:4;

    uint8_t padding2;

    uint16_t i;
    // This arg's position in input args of g_function_info_invoke
    uint16_t c_input_pos;
    // This arg's position in the output args of g_function_info_invoke
    uint16_t c_output_pos;
    // This arg's position in the Scheme GSubr
    uint16_t s_input_pos;
    // This arg's position in the return values list
    uint16_t s_output_pos;
    // When non-NULL, this is the entry of the array length argument
    // for this array argument.

    Arg_map_entry *child;
    Arg_map_entry *parent;
};

typedef struct _Arg_map Arg_map;
struct _Arg_map
{
    char *name;

    // S arguments.
    uint16_t s_input_req;
    uint16_t s_input_opt;

    // S return values
    uint16_t s_output_len;
    uint16_t padding1;
 
    // For g_function_invoke call
    uint16_t c_input_len;
    uint16_t c_output_len;

    // If this arg map has invalid entries
    bool is_invalid;
    uint8_t padding2;

    // An array of arg_map_entry
    uint16_t len;
    Arg_map_entry *pdata;

    Arg_map_entry return_val;
};

Arg_map *arg_map_new(const char *name, GICallableInfo *function_info);
void arg_map_free(Arg_map *am);
void arg_map_dump(const char *name, const Arg_map *am);

void arg_map_s_input_count(const Arg_map *amap, int *required, int *optional);
Arg_map_entry *arg_map_get_input_entry_by_s(Arg_map *am, int spos);
Arg_map_entry *arg_map_get_output_entry_by_c(Arg_map *am, int cpos);
bool arg_map_output_child_c(Arg_map *am, int c_output_pos,
                             int *cinvoke_output_array_size_index);
void arg_map_c_count(const Arg_map *am, int *c_input_pos, int *c_output_pos);
bool arg_map_input_s_2_input_c(const Arg_map *amap, int s_input, int *c_input);
bool arg_map_input_s_2_output_c(const Arg_map *amap, int s_input, int *c_output);
bool arg_map_input_s_2_child_input_c(const Arg_map *amap, int s_input, int *c_input);
bool arg_map_input_s_2_child_output_c(const Arg_map *amap, int s_input, int *c_output);
bool arg_map_get_cinvoke_array_length_indices(const Arg_map *am, int s_input_pos,
                                               int *c_input_pos, int *c_output_pos);
bool arg_map_input_i2c(const Arg_map *amap, int i, int *c);
bool arg_map_input_i2s(const Arg_map *amap, int i, int *s);
bool arg_map_input_c2i(const Arg_map *amap, int c, int *i);
bool arg_map_input_s2i(const Arg_map *amap, int s, int *i);
bool arg_map_input_c2s(const Arg_map *amap, int c, int *s);
bool arg_map_input_s2c(const Arg_map *amap, int s, int *c);

bool arg_map_output_i2c(const Arg_map *amap, int i, int *c);
bool arg_map_output_i2s(const Arg_map *amap, int i, int *s);
bool arg_map_output_c2i(const Arg_map *amap, int c, int *i);
bool arg_map_output_s2i(const Arg_map *amap, int s, int *i);
bool arg_map_output_c2s(const Arg_map *amap, int c, int *s);
bool arg_map_output_s2c(const Arg_map *amap, int s, int *c);

bool arg_map_child_i(const Arg_map *amap, int i, int *ichild);
bool arg_map_return_child_i(const Arg_map *amap, int *ichild);

#endif
