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

#ifndef GIR_ARG_MAP_H
#define GIR_ARG_MAP_H

#include <glib.h>
#include <girepository.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

// This module gathers information to help convert between 3 paradigms
//  - arguments as they appear in GIArgInfo lists
//  - arguments as they appear in SCM gsubr calls
//  - arguments as they appear in g_function_info_invoke calls
typedef enum
{
    GIR_ARG_DIRECTION_INPUT,
    GIR_ARG_DIRECTION_INOUT,
    GIR_ARG_DIRECTION_PREALLOCATED_OUTPUT,
    GIR_ARG_DIRECTION_OUTPUT,
    GIR_ARG_DIRECTION_COUNT
} GirArgDirection;

typedef enum
{
    GIR_ARG_TYPE_STANDALONE,
    GIR_ARG_TYPE_ARRAY,
    GIR_ARG_TYPE_ARRAY_SIZE,
    GIR_ARG_TYPE_COUNT
} GirArgType;

typedef enum
{
    GIR_ARG_PRESENCE_REQUIRED,
    GIR_ARG_PRESENCE_OPTIONAL,
    GIR_ARG_PRESENCE_IMPLICIT,
    GIR_ARG_PRESENCE_COUNT
} GirArgPresence;

typedef struct _GirArgMapEntry GirArgMapEntry;
struct _GirArgMapEntry
{
    GIArgInfo *arg_info;

    GirArgDirection direction;
    GirArgType type;
    // If this arg is optional in the Scheme GSubr.
    GirArgPresence presence;
    // This arg's index in g_callable_info_get_arg()
    int arg_info_index;
    // This arg's position in input args of g_function_info_invoke
    int cinvoke_input_index;
    // This arg's position in the output args of g_function_info_invoke
    int cinvoke_output_index;
    // This arg's position int the Scheme GSubr
    int gsubr_input_index;
    // This arg's position in the return values list
    int gsubr_output_index;
    // When non-NULL, this is the entry of the array length argument
    // for this array argument.
    GirArgMapEntry *child;
};

typedef struct _GirArgMap GirArgMap;
struct _GirArgMap
{
    // SCM arguments.
    int gsubr_required_input_count;
    int gsubr_optional_input_count;

    // SCM return values
    int gsubr_output_count;

    // For g_function_invoke call
    int cinvoke_input_count;
    int cinvoke_output_count;

    // An array of arg_map_entry
    GirArgMapEntry **pdata;
    int len;
};

GirArgMap *gir_arg_map_new(GIFunctionInfo *function_info);
void gir_arg_map_free(GirArgMap *am);
void gir_arg_map_dump(const GirArgMap *am);

void gir_arg_map_get_gsubr_args_count(const GirArgMap *am, int *gsubr_required_input_count,
                                      int *gsubr_optional_input_count);
GIArgInfo *gir_arg_map_get_arg_info(GirArgMap *am, int gsubr_input_index);
GIArgInfo *gir_arg_map_get_output_arg_info(GirArgMap *am, int cinvoke_output_index);
gboolean gir_arg_map_has_output_array_size_index(GirArgMap *am, int cinvoke_output_index,
                                                 int *cinvoke_output_array_size_index);
void gir_arg_map_get_cinvoke_args_count(const GirArgMap *am, int *cinvoke_input_count,
                                        int *cinvoke_output_count);
gboolean gir_arg_map_get_cinvoke_indices(const GirArgMap *am, int gsubr_input_index,
                                         int *cinvoke_input_index, int *cinvoke_output_index);
gboolean gir_arg_map_get_cinvoke_array_length_indices(const GirArgMap *am, int gsubr_input_index,
                                                      int *cinvoke_input_index,
                                                      int *cinvoke_output_index);
gboolean gir_arg_map_has_gsubr_output_index(const GirArgMap *am, int cinvoke_output_index,
                                            int *gsubr_output_index);
G_END_DECLS
#endif
