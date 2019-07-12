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
    GIR_ARG_TUPLE_SINGLETON,
    GIR_ARG_TUPLE_ARRAY,
    GIR_ARG_TUPLE_ARRAY_SIZE,
    GIR_ARG_TUPLE_COUNT
} GirArgTuple;

typedef enum
{
    GIR_ARG_PRESENCE_REQUIRED,
    GIR_ARG_PRESENCE_OPTIONAL,
    GIR_ARG_PRESENCE_IMPLICIT,
    GIR_ARG_PRESENCE_COUNT
} GirArgPresence;

// This structure contains the information necessary for choosing how
// to convert between scheme and C arguments of a function or method
// call.
typedef struct _GirArgMapEntry GirArgMapEntry;
struct _GirArgMapEntry
{
    gchar *name;

    ////////////////////////////////////////////////////////////////
    // This block is similar to GIArgInfo, except it is generic for
    // both arguments and return types
    GITypeInfo *type_info;
    GITypeTag type_tag;
    gboolean is_ptr;
    // The direction of the C argument, which may differ from the SCM
    GIDirection c_direction;
    // Nothing, container, or everything
    GITransfer transfer;
    // For C 'in' values, whether NULL is a valid value.  For 'out'
    // values, whether NULL may be returned.
    gboolean may_be_null;
    // For C 'out' values, whether this argument is allocated by the
    // caller.
    gboolean is_caller_allocates;

    ////////////////////////////////////////////////////////////////
    // This block is additional data that is valid only for arrays

    // The array itself
    GIArrayType array_type;
    gsize array_fixed_size;
    gint array_length_index;
    gboolean array_is_zero_terminated;

    // The elements of the array
    GITransfer item_transfer;
    GITypeTag item_type_tag;
    gboolean item_is_ptr;
    gsize item_size;

    // The objects held by elements of the array
    GIInfoType referenced_base_type;
    GType referenced_object_type;

    ////////////////////////////////////////////////////////////////
    // This block is derived information about how to map
    // Scheme to C and back again

    GirArgDirection s_direction;
    // If this argument can be mapped standalone, or requires another
    // argument's information to map between C and Scheme
    GirArgTuple tuple;
    // If this arg is optional in the Scheme GSubr.
    GirArgPresence presence;
    // This arg's index in g_callable_info_get_arg()
    gint arg_info_index;
    // This arg's position in input args of g_function_info_invoke
    gint cinvoke_input_index;
    // This arg's position in the output args of g_function_info_invoke
    gint cinvoke_output_index;
    // This arg's position int the Scheme GSubr
    gint gsubr_input_index;
    // This arg's position in the return values list
    gint gsubr_output_index;
    // When non-NULL, this is the entry of the array length argument
    // for this array argument.
    GirArgMapEntry *child;
};

typedef struct _GirArgMap GirArgMap;
struct _GirArgMap
{
    // SCM arguments.
    gint gsubr_required_input_count;
    gint gsubr_optional_input_count;

    // SCM return values
    gint gsubr_output_count;

    // For g_function_invoke call
    gint cinvoke_input_count;
    gint cinvoke_output_count;

    // An array of arg_map_entry
    GirArgMapEntry **pdata;
    gint len;

    GirArgMapEntry *return_val;
};

GirArgMap *gir_arg_map_new(GIFunctionInfo *function_info);
void gir_arg_map_free(GirArgMap *am);
void gir_arg_map_dump(const GirArgMap *am);

void gir_arg_map_get_gsubr_args_count(const GirArgMap *am, int *gsubr_required_input_count,
                                      int *gsubr_optional_input_count);
GirArgMapEntry *gig_arg_map_get_entry(GirArgMap *am, gint gsubr_input_index);
GirArgMapEntry *gig_arg_map_get_output_entry(GirArgMap *am, gint cinvoke_output_index);
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
