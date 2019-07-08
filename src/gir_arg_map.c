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

#include <stdio.h>
#include "gir_arg_map.h"

const char dir_strings[GIR_ARG_DIRECTION_COUNT][9] = {
    [GIR_ARG_DIRECTION_INPUT] = "INPUT",
    [GIR_ARG_DIRECTION_INOUT] = "INOUT",
    [GIR_ARG_DIRECTION_PREALLOCATED_OUTPUT] = "PREALLOC",
    [GIR_ARG_DIRECTION_OUTPUT] = "OUTPUT"
};

const char type_strings[GIR_ARG_TYPE_COUNT][11] = {
    [GIR_ARG_TYPE_STANDALONE] = "STANDALONE",
    [GIR_ARG_TYPE_ARRAY] = "ARRAY",
    [GIR_ARG_TYPE_ARRAY_SIZE] = "ARRAY_SIZE"
};

const char presence_strings[GIR_ARG_PRESENCE_COUNT][9] = {
    [GIR_ARG_PRESENCE_REQUIRED] = "REQUIRED",
    [GIR_ARG_PRESENCE_OPTIONAL] = "OPTIONAL",
    [GIR_ARG_PRESENCE_IMPLICIT] = "IMPLICIT"
};

static GirArgMapEntry *arg_map_entry_new(void);
static void arg_map_entry_initialize(GirArgMapEntry *map);

static GirArgMapEntry *
arg_map_entry_new()
{
    GirArgMapEntry *entry = g_new(GirArgMapEntry, 1);
    arg_map_entry_initialize(entry);
    return entry;
}

static void
arg_map_entry_initialize(GirArgMapEntry *entry)
{
    entry->arg_info = NULL;
    entry->direction = GIR_ARG_DIRECTION_INPUT;
    entry->type = GIR_ARG_TYPE_STANDALONE;
    entry->presence = GIR_ARG_PRESENCE_REQUIRED;
    entry->arg_info_index = -1;
    entry->cinvoke_input_index = -1;
    entry->cinvoke_output_index = -1;
    entry->gsubr_input_index = -1;
    entry->gsubr_output_index = -1;
    entry->child = NULL;
}

GirArgMap *
gir_arg_map_new(GIFunctionInfo *function_info)
{
    size_t arg_info_count = g_callable_info_get_n_args(function_info);
    GirArgMap *amap = g_new0(GirArgMap, 1);
    GPtrArray *entry_array = g_ptr_array_new();
    for (int arg_info_index = 0; arg_info_index < arg_info_count; arg_info_index++)
        g_ptr_array_add(entry_array, arg_map_entry_new());

    // may-be-null parameters at the end of the C call can be made
    // optional parameters in the gsubr call.
    gboolean opt_flag = TRUE;
    for (int arg_info_index = arg_info_count - 1; arg_info_index >= 0; arg_info_index--) {
        g_assert_cmpint(arg_info_index, <, entry_array->len);
        GIArgInfo *arg_info = g_callable_info_get_arg(function_info, arg_info_index);
        GIDirection dir = g_arg_info_get_direction(arg_info);
        GirArgMapEntry *entry = g_ptr_array_index(entry_array, arg_info_index);
        entry->arg_info = arg_info;
        g_base_info_ref(entry->arg_info);
        entry->arg_info_index = arg_info_index;
        if (dir == GI_DIRECTION_IN
            || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg_info))) {
            if (opt_flag && g_arg_info_may_be_null(arg_info))
                entry->presence = GIR_ARG_PRESENCE_OPTIONAL;
            else {
                entry->presence = GIR_ARG_PRESENCE_REQUIRED;
                opt_flag = FALSE;
            }
        }
        else {
            entry->presence = GIR_ARG_PRESENCE_IMPLICIT;
        }
        g_base_info_unref(arg_info);
    }

    int cinvoke_input_index = 0;
    int cinvoke_output_index = 0;
    int gsubr_input_index = 0;
    int gsubr_output_index = 0;
    for (int arg_info_index = 0; arg_info_index < arg_info_count; arg_info_index++) {
        g_assert_cmpint(arg_info_index, <, entry_array->len);
        GirArgMapEntry *entry = g_ptr_array_index(entry_array, arg_info_index);
        GIArgInfo *arg_info = g_callable_info_get_arg(function_info, arg_info_index);
        GIDirection dir = g_arg_info_get_direction(arg_info);
        GITypeInfo *type_info = g_arg_info_get_type(arg_info);
        GITypeTag type_tag = g_type_info_get_tag(type_info);

        // Sometime a single SCM array or list will map to two
        // arguments: a C array pointer and a C size parameter.

        if (type_tag == GI_TYPE_TAG_ARRAY) {
            int array_length_arg_info_index = g_type_info_get_array_length(type_info);
            if (array_length_arg_info_index >= 0) {
                entry->type = GIR_ARG_TYPE_ARRAY;
                entry->child = g_ptr_array_index(entry_array, array_length_arg_info_index);
                entry->child->type = GIR_ARG_TYPE_ARRAY_SIZE;
                entry->child->presence = GIR_ARG_PRESENCE_IMPLICIT;
            }
        }

        // Here we find the positions of this argument in the
        // g_function_info_invoke call.  Also, some output parameters
        // require a SCM container to be passed in to the SCM GSubr
        // call.
        if (dir == GI_DIRECTION_IN) {
            entry->direction = GIR_ARG_DIRECTION_INPUT;
            entry->cinvoke_input_index = cinvoke_input_index++;
        }
        else if (dir == GI_DIRECTION_INOUT) {
            entry->direction = GIR_ARG_DIRECTION_INOUT;
            entry->cinvoke_input_index = cinvoke_input_index++;
            entry->cinvoke_output_index = cinvoke_output_index++;
        }
        else if (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg_info)) {
            entry->direction = GIR_ARG_DIRECTION_PREALLOCATED_OUTPUT;
            entry->cinvoke_output_index = cinvoke_output_index++;
        }
        else {
            entry->direction = GIR_ARG_DIRECTION_OUTPUT;
            entry->cinvoke_output_index = cinvoke_output_index++;
        }
        g_base_info_unref(type_info);
        g_base_info_unref(arg_info);
    }

    // We now can decide where these arguments appear in the SCM GSubr
    // call.
    for (int arg_info_index = 0; arg_info_index < arg_info_count; arg_info_index++) {
        g_assert_cmpint(arg_info_index, <, entry_array->len);
        GirArgMapEntry *entry = g_ptr_array_index(entry_array, arg_info_index);
        if (entry->direction == GIR_ARG_DIRECTION_INPUT ||
            entry->direction == GIR_ARG_DIRECTION_INOUT ||
            entry->direction == GIR_ARG_DIRECTION_PREALLOCATED_OUTPUT) {
            if (entry->type == GIR_ARG_TYPE_STANDALONE || entry->type == GIR_ARG_TYPE_ARRAY) {
                entry->gsubr_input_index = gsubr_input_index++;
                if (entry->presence == GIR_ARG_PRESENCE_REQUIRED)
                    amap->gsubr_required_input_count++;
                else if (entry->presence == GIR_ARG_PRESENCE_OPTIONAL)
                    amap->gsubr_optional_input_count++;
            }
        }
        else if (entry->direction == GIR_ARG_DIRECTION_OUTPUT) {
            entry->gsubr_output_index = gsubr_output_index++;
        }
        else
            g_assert_not_reached();
    }
    g_assert_cmpint(amap->gsubr_required_input_count + amap->gsubr_optional_input_count, ==,
                    gsubr_input_index);
    amap->gsubr_output_count = gsubr_output_index;
    amap->cinvoke_input_count = cinvoke_input_index;
    amap->cinvoke_output_count = cinvoke_output_index;
    amap->len = entry_array->len;
    amap->pdata = (GirArgMapEntry **)(entry_array->pdata);
    g_ptr_array_free(entry_array, FALSE);
    return amap;
}

void
gir_arg_map_free(GirArgMap *amap)
{
    for (int i = 0; i < amap->len; i++) {
        g_base_info_unref(amap->pdata[i]->arg_info);
        free(amap->pdata[i]);
        amap->pdata[i] = NULL;
    }
    free(amap->pdata);
    amap->pdata = NULL;
    free(amap);
}

void
gir_arg_map_dump(const GirArgMap *amap)
{
    g_debug("Arg map %p", amap);
    g_debug(" Inputs required: %d, optional %d, Outputs %d", amap->gsubr_required_input_count,
            amap->gsubr_optional_input_count, amap->gsubr_output_count);
    g_debug(" C input params: %d, output params %d", amap->cinvoke_input_count,
            amap->cinvoke_output_count);
    for (int i = 0; i < amap->len; i++) {
        GirArgMapEntry *entry = (GirArgMapEntry *)(amap->pdata[i]);
        g_debug(" Arg %d: %13s %10s %10s %10s Index %d, SCM In %d, Out %d, C In %d, Out %d",
                i,
                g_base_info_get_name(entry->arg_info),
                dir_strings[entry->direction],
                type_strings[entry->type],
                presence_strings[entry->presence],
                entry->arg_info_index, entry->gsubr_input_index, entry->gsubr_output_index,
                entry->cinvoke_input_index, entry->cinvoke_output_index);
    }
}

void
gir_arg_map_get_gsubr_args_count(const GirArgMap *amap, int *required, int *optional)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(required);
    g_assert_nonnull(optional);
    *required = amap->gsubr_required_input_count;
    *optional = amap->gsubr_optional_input_count;
}

GIArgInfo *
gir_arg_map_get_arg_info(GirArgMap *amap, int input)
{
    g_assert_nonnull(amap);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i]->gsubr_input_index == input) {
            g_base_info_ref(amap->pdata[i]->arg_info);
            return amap->pdata[i]->arg_info;
        }
        i++;
    }
    g_assert_not_reached();;
}

GIArgInfo *
gir_arg_map_get_output_arg_info(GirArgMap *amap, int output)
{
    g_assert_nonnull(amap);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i]->cinvoke_output_index == output) {
            g_base_info_ref(amap->pdata[i]->arg_info);
            return amap->pdata[i]->arg_info;
        }
        i++;
    }
    g_return_val_if_reached(NULL);
}

// If this output element is an array with another output element
// being its size, this returns TRUE and stores the index of the other
// argument.
gboolean
gir_arg_map_has_output_array_size_index(GirArgMap *amap, int cinvoke_output_index,
                                        int *cinvoke_output_array_size_index)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(cinvoke_output_array_size_index);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i]->cinvoke_output_index == cinvoke_output_index) {
            if (amap->pdata[i]->child) {
                *cinvoke_output_array_size_index = amap->pdata[i]->child->cinvoke_output_index;
                return TRUE;
            }
        }
        i++;
    }
    return FALSE;
}

// Get the number of required and optional gsubr arguments for this
// gsubr call.
void
gir_arg_map_get_cinvoke_args_count(const GirArgMap *amap, int *cinvoke_input_count,
                                   int *cinvoke_output_count)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(cinvoke_input_count);
    g_assert_nonnull(cinvoke_output_count);

    *cinvoke_input_count = amap->cinvoke_input_count;
    *cinvoke_output_count = amap->cinvoke_output_count;
}

// For the gsubr argument at position INDEX, get the input and output
// index positions for this argument in the C function call.  Return
// TRUE if this gsubr argument is used in the C function call.
gboolean
gir_arg_map_get_cinvoke_indices(const GirArgMap *amap, int gsubr_input_index,
                                int *cinvoke_input_index, int *cinvoke_output_index)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(cinvoke_input_index);
    g_assert_nonnull(cinvoke_output_index);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i]->gsubr_input_index == gsubr_input_index) {
            *cinvoke_input_index = amap->pdata[i]->cinvoke_input_index;
            *cinvoke_output_index = amap->pdata[i]->cinvoke_output_index;
            return (*cinvoke_input_index >= 0 || *cinvoke_output_index >= 0);
        }
        i++;
    }
    *cinvoke_input_index = -1;
    *cinvoke_output_index = -1;
    g_return_val_if_reached(FALSE);
}

// For the gsubr argument at position INDEX, if it is an array whose
// length is supposed to be computed before the C function is invoked,
// return the input and output positions for the array size in the C
// function call.  Return TRUE if this gsubr argument's array size is
// used in the C function call.
gboolean
gir_arg_map_get_cinvoke_array_length_indices(const GirArgMap *amap, int gsubr_input_index,
                                             int *cinvoke_input_index, int *cinvoke_output_index)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(cinvoke_input_index);
    g_assert_nonnull(cinvoke_output_index);

    int arg_info_index = 0;
    while (arg_info_index < amap->len) {
        if (amap->pdata[arg_info_index]->gsubr_input_index == gsubr_input_index) {
            GirArgMapEntry *child = amap->pdata[arg_info_index]->child;
            if (child) {
                *cinvoke_input_index = child->cinvoke_input_index;
                *cinvoke_output_index = child->cinvoke_output_index;
                return (*cinvoke_input_index >= 0 || *cinvoke_output_index >= 0);
            }
            else {
                *cinvoke_input_index = -1;
                *cinvoke_output_index = -1;
                return FALSE;
            }
        }
        arg_info_index++;
    }
    *cinvoke_input_index = -1;
    *cinvoke_output_index = -1;
    g_return_val_if_reached(FALSE);
}

gboolean
gir_arg_map_has_gsubr_output_index(const GirArgMap *amap, int cinvoke_output_index,
                                   int *gsubr_output_index)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(gsubr_output_index);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i]->cinvoke_output_index == cinvoke_output_index) {
            int j = amap->pdata[i]->gsubr_output_index;
            if (j >= 0) {
                *gsubr_output_index = j;
                return TRUE;
            }
            else
                return FALSE;

        }
        i++;
    }
    return FALSE;
}
