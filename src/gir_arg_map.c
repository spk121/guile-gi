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
    entry->dir = GIR_ARG_DIRECTION_INPUT;
    entry->type = GIR_ARG_TYPE_STANDALONE;
    entry->presence = GIR_ARG_PRESENCE_REQUIRED;
    entry->index = -1;
    entry->invoke_in = -1;
    entry->invoke_out = -1;
    entry->in = -1;
    entry->out = -1;
    entry->child = NULL;
}

GirArgMap *
gir_arg_map_new(GIFunctionInfo *function_info)
{
    size_t n_args = g_callable_info_get_n_args(function_info);
    GirArgMap *am = g_new0(GirArgMap, 1);
    GPtrArray *arr = g_ptr_array_new();
    for (int i = 0; i < n_args; i++)
        g_ptr_array_add(arr, arg_map_entry_new());

    // may-be-null parameters at the end of the C call can be made
    // optional parameters in the gsubr call.
    gboolean opt_flag = TRUE;
    for (int i = n_args - 1; i >= 0; i--) {
        g_assert_cmpint(i, <, arr->len);
        GIArgInfo *ai = g_callable_info_get_arg(function_info, i);
        GIDirection dir = g_arg_info_get_direction(ai);
        GirArgMapEntry *entry = g_ptr_array_index(arr, i);
        entry->arg_info = ai;
        g_base_info_ref(entry->arg_info);
        entry->index = i;
        if (dir == GI_DIRECTION_IN
            || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(ai))) {
            if (opt_flag && g_arg_info_may_be_null(ai))
                entry->presence = GIR_ARG_PRESENCE_OPTIONAL;
            else {
                entry->presence = GIR_ARG_PRESENCE_REQUIRED;
                opt_flag = FALSE;
            }
        }
        else {
            entry->presence = GIR_ARG_PRESENCE_IMPLICIT;
        }
        g_base_info_unref(ai);
    }

    int i_invoke_in = 0;
    int i_invoke_out = 0;
    int i_in = 0;
    int i_out = 0;
    for (int i = 0; i < n_args; i++) {
        g_assert_cmpint(i, <, arr->len);
        GirArgMapEntry *entry = g_ptr_array_index(arr, i);
        GIArgInfo *ai = g_callable_info_get_arg(function_info, i);
        GIDirection dir = g_arg_info_get_direction(ai);
        GITypeInfo *type_info = g_arg_info_get_type(ai);
        GITypeTag type_tag = g_type_info_get_tag(type_info);

        // Sometime a single SCM array or list will map to two
        // arguments: a C array pointer and a C size parameter.

        if (type_tag == GI_TYPE_TAG_ARRAY) {
            int i_length = g_type_info_get_array_length(type_info);
            if (i_length >= 0) {
                entry->type = GIR_ARG_TYPE_ARRAY;
                entry->child = g_ptr_array_index(arr, i_length);
                entry->child->type = GIR_ARG_TYPE_ARRAY_SIZE;
                entry->child->presence = GIR_ARG_PRESENCE_IMPLICIT;
            }
        }

        // Here we find the positions of this argument in the
        // g_function_info_invoke call.  Also, some output parameters
        // require a SCM container to be passed in to the SCM GSubr
        // call.
        if (dir == GI_DIRECTION_IN) {
            entry->dir = GIR_ARG_DIRECTION_INPUT;
            entry->invoke_in = i_invoke_in++;
        }
        else if (dir == GI_DIRECTION_INOUT) {
            entry->dir = GIR_ARG_DIRECTION_INOUT;
            entry->invoke_in = i_invoke_in++;
            entry->invoke_out = i_invoke_out++;
        }
        else if (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(ai)) {
            entry->dir = GIR_ARG_DIRECTION_PREALLOCATED_OUTPUT;
            entry->invoke_out = i_invoke_out++;
        }
        else {
            entry->dir = GIR_ARG_DIRECTION_OUTPUT;
            entry->invoke_out = i_invoke_out++;
        }
        g_base_info_unref(type_info);
        g_base_info_unref(ai);
    }

    // We now can decide where these arguments appear in the SCM GSubr
    // call.
    for (int i = 0; i < n_args; i++) {
        g_assert_cmpint(i, <, arr->len);
        GirArgMapEntry *entry = g_ptr_array_index(arr, i);
        if (entry->dir == GIR_ARG_DIRECTION_INPUT || entry->dir == GIR_ARG_DIRECTION_INOUT ||
            entry->dir == GIR_ARG_DIRECTION_PREALLOCATED_OUTPUT) {
            if (entry->type == GIR_ARG_TYPE_STANDALONE || entry->type == GIR_ARG_TYPE_ARRAY) {
                entry->in = i_in++;
                if (entry->presence == GIR_ARG_PRESENCE_REQUIRED)
                    am->required_inputs_count++;
                else if (entry->presence == GIR_ARG_PRESENCE_OPTIONAL)
                    am->optional_inputs_count++;
            }
        }
        else if (entry->dir == GIR_ARG_DIRECTION_OUTPUT) {
            entry->out = i_out++;
        }
        else
            g_assert_not_reached();
    }
    g_assert_cmpint(am->required_inputs_count + am->optional_inputs_count, ==, i_in);
    am->outputs_count = i_out;
    am->invoked_inputs_count = i_invoke_in;
    am->invoked_outputs_count = i_invoke_out;
    am->len = arr->len;
    am->pdata = (GirArgMapEntry **)(arr->pdata);
    g_ptr_array_free(arr, FALSE);
    return am;
}

void
gir_arg_map_free(GirArgMap *am)
{
    for (int i = 0; i < am->len; i++) {
        g_base_info_unref(am->pdata[i]->arg_info);
        free(am->pdata[i]);
        am->pdata[i] = NULL;
    }
    free(am->pdata);
    am->pdata = NULL;
    free(am);
}

void
gir_arg_map_dump(const GirArgMap *am)
{
    g_debug("Arg map %p", am);
    g_debug(" Inputs required: %d, optional %d, Outputs %d", am->required_inputs_count,
            am->optional_inputs_count, am->outputs_count);
    g_debug(" C input params: %d, output params %d", am->invoked_inputs_count,
            am->invoked_outputs_count);
    for (int i = 0; i < am->len; i++) {
        GirArgMapEntry *entry = (GirArgMapEntry *)(am->pdata[i]);
        g_debug(" Arg %d: %10s %10s %10s Index %d, SCM In %d, Out %d, C In %d, Out %d",
                i,
                dir_strings[entry->dir],
                type_strings[entry->type],
                presence_strings[entry->presence],
                entry->index, entry->in, entry->out, entry->invoke_in, entry->invoke_out);
    }
}

void
gir_arg_map_get_args_count(const GirArgMap *am, int *required, int *optional)
{
    g_assert_nonnull(am);
    g_assert_nonnull(required);
    g_assert_nonnull(optional);
    *required = am->required_inputs_count;
    *optional = am->optional_inputs_count;
}

GIArgInfo *
gir_arg_map_get_arg_info(GirArgMap *am, int input)
{
    g_assert_nonnull(am);

    int i = 0;
    while (i < am->len) {
        if (am->pdata[i]->in == input) {
            g_base_info_ref(am->pdata[i]->arg_info);
            return am->pdata[i]->arg_info;
        }
        i++;
    }
    g_return_val_if_reached(NULL);
}

// Get the number of required and optional gsubr arguments for this
// gsubr call.
void
gir_arg_map_get_invoke_args_count(const GirArgMap *am, int *input, int *output)
{
    g_assert_nonnull(am);
    g_assert_nonnull(input);
    g_assert_nonnull(output);

    *input = am->invoked_inputs_count;
    *output = am->invoked_outputs_count;
}

// For the gsubr argument at position INDEX, get the input and output
// index positions for this argument in the C function call.  Return
// TRUE if this gsubr argument is used in the C function call.
gboolean
gir_arg_map_get_invoke_indices(const GirArgMap *am, int input, int *invoked_in, int *invoked_out)
{
    g_assert_nonnull(am);
    g_assert_nonnull(invoked_in);
    g_assert_nonnull(invoked_out);

    int i = 0;
    while (i < am->len) {
        if (am->pdata[i]->in == input) {
            *invoked_in = am->pdata[i]->invoke_in;
            *invoked_out = am->pdata[i]->invoke_out;
            return (*invoked_in >= 0 || *invoked_out >= 0);
        }
        i++;
    }
    *invoked_in = -1;
    *invoked_out = -1;
    g_return_val_if_reached(FALSE);
}

// For the gsubr argument at position INDEX, if it is an array whose
// length is supposed to be computed before the C function is invoked,
// return the input and output positions for the array size in the C
// function call.  Return TRUE if this gsubr argument's array size is
// used in the C function call.
gboolean
gir_arg_map_get_invoke_array_length_indices(const GirArgMap *am, int input, int *invoked_in,
                                            int *invoked_out)
{
    g_assert_nonnull(am);
    g_assert_nonnull(invoked_in);
    g_assert_nonnull(invoked_out);

    int i = 0;
    while (i < am->len) {
        if (am->pdata[i]->in == input) {
            GirArgMapEntry *child = am->pdata[i]->child;
            if (child) {
                *invoked_in = child->invoke_in;
                *invoked_out = child->invoke_out;
                return (*invoked_in >= 0 || *invoked_out >= 0);
            }
            else {
                *invoked_in = -1;
                *invoked_out = -1;
                return FALSE;
            }
        }
        i++;
    }
    *invoked_in = -1;
    *invoked_out = -1;
    g_return_val_if_reached(FALSE);
}
