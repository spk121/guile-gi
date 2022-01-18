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

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "x.h"
#include "arg_map.h"
#include "arg.h"

static char dir_strings[ARG_DIRECTION_COUNT][9] = {
    [ARG_DIRECTION_VOID] = "void",
    [ARG_DIRECTION_INPUT] = "input",
    [ARG_DIRECTION_INOUT] = "inout",
    [ARG_DIRECTION_PREALLOCATED_OUTPUT] = "prealloc",
    [ARG_DIRECTION_OUTPUT] = "output"
};

static char tuple_strings[ARG_TUPLE_COUNT][11] = {
    [ARG_TUPLE_SINGLETON] = "singleton",
    [ARG_TUPLE_ARRAY] = "array",
    [ARG_TUPLE_ARRAY_SIZE] = "array_size"
};

static char presence_strings[ARG_PRESENCE_COUNT][9] = {
    [ARG_PRESENCE_REQUIRED] = "required",
    [ARG_PRESENCE_OPTIONAL] = "optional",
    [ARG_PRESENCE_IMPLICIT] = "implicit"
};

static Arg_map *arg_map_allocate(size_t n);
static void arg_map_apply_function_info(Arg_map *amap, GIFunctionInfo *func_info);
static void arg_map_determine_argument_presence(Arg_map *amap, GIFunctionInfo *info);
static void arg_map_compute_c_invoke_positions(Arg_map *amap);
static void arg_map_compute_s_call_positions(Arg_map *amap);
static void arg_map_entry_init(Arg_map_entry *map);

static void
arg_map_entry_init(Arg_map_entry *entry)
{
    memset(entry, 0, sizeof(Arg_map_entry));
    entry->name = strdup("(uninitialized)");
}

// Gather information on how to map Scheme arguments to C arguments.
Arg_map *
arg_map_new(const char *name, GICallableInfo *function_info)
{
    Arg_map *amap;
    int n;

    n = g_callable_info_get_n_args(function_info);
    amap = arg_map_allocate(n);
    amap->name = strdup(g_base_info_get_name(function_info));
    arg_map_apply_function_info(amap, function_info);
    if (amap->is_invalid) {
        arg_map_free(amap);
        return NULL;
    }
    arg_map_determine_argument_presence(amap, function_info);
    arg_map_compute_c_invoke_positions(amap);
    arg_map_compute_s_call_positions(amap);
    arg_map_dump(name, amap);
    return amap;
}

static Arg_map *
arg_map_allocate(size_t n)
{
    Arg_map *amap;

    amap = xcalloc(1, sizeof(Arg_map));
    amap->pdata = xcalloc(n, sizeof(Arg_map_entry));
    amap->len = n;
    for (size_t i = 0; i < n; i++) {
        arg_map_entry_init(&amap->pdata[i]);
        amap->pdata[i].i = i;
    }
    arg_map_entry_init(&amap->return_val);

    return amap;
}

static void
arg_map_apply_function_info(Arg_map *amap, GIFunctionInfo *func_info)
{
    int i, n;
    GIArgInfo *arg_info;

    n = amap->len;

    for (i = 0; i < n; i++) {
        arg_info = g_callable_info_get_arg(func_info, i);
        arg_init_from_arg_info(&amap->pdata[i].arg, arg_info);
        free(amap->pdata[i].name);
        amap->pdata[i].name = strdup(g_base_info_get_name(arg_info));
        g_base_info_unref(arg_info);
        amap->is_invalid |= amap->pdata[i].arg.is_invalid;
    }

    arg_init_from_callable_info(&amap->return_val.arg, func_info);
    free(amap->return_val.name);
    amap->return_val.name = strdup("%return");
    amap->is_invalid |= amap->return_val.arg.is_invalid;
}

static void
arg_map_determine_array_length_index(Arg_map *amap, Arg_map_entry *entry, GITypeInfo *info)
{
    if (entry->arg.payload == TYPE_ARRAY && entry->arg.has_size) {
        int idx = g_type_info_get_array_length(info);

        assert(idx >= 0);

        entry->tuple = ARG_TUPLE_ARRAY;
        entry->child = amap->pdata + idx;
        entry->child->tuple = ARG_TUPLE_ARRAY_SIZE;
        entry->child->presence = ARG_PRESENCE_IMPLICIT;
        entry->child->parent = entry;
        entry->child->is_s_output = 0;
    }
}

static void
arg_map_determine_argument_presence(Arg_map *amap, GIFunctionInfo *info)
{
    Arg_map_entry *entry;
    bool opt_flag = true;
    int i, n;

    n = amap->len;

    // may-be-null parameters at the end of the C call can be made
    // optional parameters in the gsubr call.
    for (i = n - 1; i >= 0; i--) {
        entry = &amap->pdata[i];
        entry->tuple = ARG_TUPLE_SINGLETON;
        if (entry->arg.is_in || (entry->arg.is_out && entry->arg.is_caller_allocates)) {
            if (opt_flag && entry->arg.is_nullable)
                entry->presence = ARG_PRESENCE_OPTIONAL;
            else {
                entry->presence = ARG_PRESENCE_REQUIRED;
                opt_flag = false;
            }
        }
        else {
            entry->presence = ARG_PRESENCE_IMPLICIT;
        }
    }

    // In C, if there is an array defined as a pointer and a
    // length parameter, it becomes a single S parameter.
    for (i = 0; i < n; i++) {
        entry = amap->pdata + i;
        GIArgInfo *a = g_callable_info_get_arg(info, i);
        GITypeInfo *t = g_arg_info_get_type(a);
        arg_map_determine_array_length_index(amap, entry, t);
        g_base_info_unref(t);
        g_base_info_unref(a);
    }

    amap->return_val.tuple = ARG_TUPLE_SINGLETON;
    GITypeInfo *return_type = g_callable_info_get_return_type(info);
    arg_map_determine_array_length_index(amap, &amap->return_val, return_type);
    g_base_info_unref(return_type);
}

static void
arg_map_compute_c_invoke_positions(Arg_map *amap)
{
    int i, n;
    Arg_map_entry *entry;
    n = amap->len;

    int c_input_pos = 0;
    int c_output_pos = 0;
    for (i = 0; i < n; i++) {
        entry = &amap->pdata[i];

        // Here we find the positions of this argument in the
        // g_function_info_invoke call.  Also, some output parameters
        // require a SCM container to be passed in to the SCM GSubr
        // call.
        if (entry->arg.is_in && !entry->arg.is_out) {
            entry->s_direction = ARG_DIRECTION_INPUT;
            entry->is_c_input = 1;
            entry->c_input_pos = c_input_pos++;
        }
        else if (entry->arg.is_in && entry->arg.is_out) {
            entry->s_direction = ARG_DIRECTION_INOUT;
            entry->is_c_input = 1;
            entry->c_input_pos = c_input_pos++;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
        else if (entry->arg.is_out && entry->arg.is_caller_allocates) {
            entry->s_direction = ARG_DIRECTION_PREALLOCATED_OUTPUT;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
        else {
            entry->s_direction = ARG_DIRECTION_OUTPUT;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
    }

    if (amap->return_val.arg.is_out)
        amap->return_val.s_direction = ARG_DIRECTION_OUTPUT;
    else
        amap->return_val.s_direction = ARG_DIRECTION_VOID;

    amap->c_input_len = c_input_pos;
    amap->c_output_len = c_output_pos;
}

static void
arg_map_compute_s_call_positions(Arg_map *amap)
{
    int i, n;
    Arg_map_entry *entry;
    n = amap->len;

    int s_input_pos = 0;
    int s_output_pos = 0;
    // We now can decide where these arguments appear in the SCM GSubr
    // call.
    for (i = 0; i < n; i++) {
        entry = &amap->pdata[i];

        // TODO: Why check entry->tuple instead of entry->presence?
        //       The latter appears to be buggy in some way.
        if (entry->tuple == ARG_TUPLE_ARRAY_SIZE)
            continue;

        switch (entry->s_direction) {
        case ARG_DIRECTION_INPUT:
        case ARG_DIRECTION_INOUT:
        case ARG_DIRECTION_PREALLOCATED_OUTPUT:
            entry->is_s_input = 1;
            entry->s_input_pos = s_input_pos++;
            if (entry->presence == ARG_PRESENCE_REQUIRED)
                amap->s_input_req++;
            else if (entry->presence == ARG_PRESENCE_OPTIONAL)
                amap->s_input_opt++;

            if (entry->s_direction == ARG_DIRECTION_INPUT)
                break;
            /* fallthrough */
        case ARG_DIRECTION_OUTPUT:
            entry->is_s_output = 1;
            entry->s_output_pos = s_output_pos++;
            break;
        default:
            abort();
        }
    }

    amap->s_output_len = s_output_pos;
    assert(amap->s_input_req + amap->s_input_opt == s_input_pos);
}

void
arg_map_free(Arg_map *amap)
{
    if (!amap)
        return;

    for (int i = 0; i < amap->len; i++) {
        arg_free(&amap->pdata[i].arg);
        free(amap->pdata[i].name);
    }
    arg_free(&amap->return_val.arg);
    free(amap->return_val.name);
    free(amap->pdata);
    free(amap->name);
    amap->pdata = NULL;
    free(amap);
}

// FIXME: don't use gstring

void
arg_map_dump(const char *name, const Arg_map *amap)
{
    char s[1024];
    size_t len;
    
    debug_load("%s - argument mapping", name ? name : amap->name);
    debug_load("  SCM inputs required: %d, optional: %d, outputs: %d", amap->s_input_req,
                   amap->s_input_opt, amap->s_output_len);
    debug_load("  C inputs: %d, outputs: %d", amap->c_input_len, amap->c_output_len);
    for (int i = 0; i < amap->len; i++) {
        const Arg_map_entry *entry = &amap->pdata[i];
        len = snprintf(s, 100, "  Arg %d: '%s' %s",
                       i, entry->name, arg_describe(&entry->arg));
        len += snprintf(s + len, 100, ", %s, %s, %s",
                        dir_strings[entry->s_direction],
                        tuple_strings[entry->tuple], presence_strings[entry->presence]);
        if (entry->is_c_input)
            len += snprintf(s + len, 20, ", C input %d", entry->c_input_pos);
        if (entry->is_c_output)
            len += snprintf(s + len, 20, ", C output %d", entry->c_output_pos);
        if (entry->is_s_input)
            len += snprintf(s + len, 20, ", SCM input %d", entry->s_input_pos);
        if (entry->is_s_output)
            len += snprintf(s + len, 20, ", S output %d", entry->c_output_pos);
        debug_load("%s", s);
        s[0] = '\0';

        if (amap->pdata[i].arg.n_params > 0) {
            Arg *m2 = &amap->pdata[i].arg.params[0];
            len = snprintf(s, 100, "    Item Type: %s", arg_describe(m2));
            debug_load("%s", s);
        }
    }
    if (amap->return_val.arg._gtype != G_TYPE_NONE) {
        const Arg_map_entry *entry = &amap->return_val;
        s[0] = '\0';
        len = snprintf(s, 100, "  Return: '%s' %s",
                       entry->name, arg_describe(&entry->arg));
        len += snprintf(s + len, 100, ", %s, %s, %s",
                        dir_strings[entry->s_direction],
                        tuple_strings[entry->tuple], presence_strings[entry->presence]);
        debug_load("%s", s);
        if (amap->return_val.arg.n_params > 0) {
            Arg *m2 = &amap->return_val.arg.params[0];
            s[0] = '\0';
            len = snprintf(s, 100, "    Item Type: %s", arg_describe(m2));
            debug_load("%s", s);
        }
    }
}

void
arg_map_s_input_count(const Arg_map *amap, int *required, int *optional)
{
    assert(amap != NULL);
    assert(required != NULL);
    assert(optional != NULL);

    *required = amap->s_input_req;
    *optional = amap->s_input_opt;
}

Arg_map_entry *
arg_map_get_input_entry_by_s(Arg_map *amap, int spos)
{
    int i = 0;

    assert(amap != NULL);

    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == spos)) {
            return &amap->pdata[i];
        }
        i++;
    }
    abort();
}

Arg_map_entry *
arg_map_get_output_entry_by_c(Arg_map *amap, int cpos)
{
    assert(amap != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_c_output && (amap->pdata[i].c_output_pos == cpos)) {
            return &amap->pdata[i];
        }
        i++;
    }
    abort();
}

// If this output element is an array with another output element
// being its size, this returns true and stores the index of the other
// argument.
bool
arg_map_output_child_c(Arg_map *amap, int c_output_pos, int *cinvoke_output_array_size_index)
{
    assert(amap != NULL);
    assert(cinvoke_output_array_size_index != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_c_output && (amap->pdata[i].c_output_pos == c_output_pos)) {
            if (amap->pdata[i].child) {
                *cinvoke_output_array_size_index = amap->pdata[i].child->c_output_pos;
                return true;
            }
        }
        i++;
    }
    return false;
}

// Get the number of required and optional gsubr arguments for this
// gsubr call.
void
arg_map_c_count(const Arg_map *amap, int *c_input_len, int *c_output_len)
{
    assert(amap != NULL);
    assert(c_input_len != NULL);
    assert(c_output_len != NULL);

    *c_input_len = amap->c_input_len;
    *c_output_len = amap->c_output_len;
}

// For the gsubr argument at position INDEX, get the input and output
// index positions for this argument in the C function call.  Return
// true if this gsubr argument is used in the C function call.

bool
arg_map_input_s_2_input_c(const Arg_map *amap, int s_input_pos, int *c_input_pos)
{
    assert(amap != NULL);
    assert(c_input_pos != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            if (amap->pdata[i].is_c_input) {
                *c_input_pos = amap->pdata[i].c_input_pos;
                return true;
            }
            else
                return false;
        }
        i++;
    }
    return false;
}

bool
arg_map_input_s_2_output_c(const Arg_map *amap, int s_input_pos, int *c_output_pos)
{
    assert(amap != NULL);
    assert(c_output_pos != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            if (amap->pdata[i].is_c_output) {
                *c_output_pos = amap->pdata[i].c_output_pos;
                return true;
            }
            else
                return false;
        }
        i++;
    }
    return false;
}

// For the gsubr argument at position INDEX, if it is an array whose
// length is supposed to be computed before the C function is invoked,
// return the input and output positions for the array size in the C
// function call.  Return true if this gsubr argument's array size is
// used in the C function call.
bool
arg_map_input_s_2_child_input_c(const Arg_map *amap, int s_input_pos, int *c_input_pos)
{
    assert(amap != NULL);
    assert(c_input_pos != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            Arg_map_entry *child = amap->pdata[i].child;
            if (child) {
                if (child->is_c_input) {
                    *c_input_pos = child->c_input_pos;
                    return true;
                }
                else
                    return false;
            }
            else
                return false;
        }
        i++;
    }
    return false;
}

bool
arg_map_input_s_2_child_output_c(const Arg_map *amap, int s_input_pos, int *c_output_pos)
{
    assert(amap != NULL);
    assert(c_output_pos != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            Arg_map_entry *child = amap->pdata[i].child;
            if (child) {
                if (child->is_c_output) {
                    *c_output_pos = child->c_output_pos;
                    return true;
                }
                else
                    return false;
            }
            else
                return false;
        }
        i++;
    }
    return false;
}

////////////////////////////////////////////////////////////////

bool
arg_map_input_i2c(const Arg_map *amap, int i, int *cpos)
{
    if (i < 0 || i >= amap->len)
        return false;
    if (!amap->pdata[i].is_c_input)
        return false;
    *cpos = amap->pdata[i].c_input_pos;
    return true;
}

bool
arg_map_input_i2s(const Arg_map *amap, int i, int *spos)
{
    if (i < 0 || i >= amap->len)
        return false;
    if (!amap->pdata[i].is_s_input)
        return false;
    *spos = amap->pdata[i].s_input_pos;
    return true;
}

bool
arg_map_input_c2i(const Arg_map *amap, int cpos, int *i)
{
    if (cpos < 0 || cpos >= amap->c_input_len) {
        return false;
    }
    int j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_input && (amap->pdata[j].c_input_pos == cpos)) {
            *i = j;
            return true;
        }
        j++;
    }
    return false;
}


bool
arg_map_input_s2i(const Arg_map *amap, int spos, int *i)
{
    if (spos < 0 || spos >= amap->s_input_req + amap->s_input_opt) {
        return false;
    }
    int j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_s_input && (amap->pdata[j].s_input_pos == spos)) {
            *i = j;
            return true;
        }
        j++;
    }
    return false;
}

bool
arg_map_input_c2s(const Arg_map *amap, int cpos, int *spos)
{
    if (cpos < 0 || cpos >= amap->c_input_len) {
        return false;
    }
    int j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_input && (amap->pdata[j].c_input_pos == cpos)) {
            if (amap->pdata[j].is_s_input) {
                *spos = amap->pdata[j].s_input_pos;
                return true;
            }
            else {
                return false;
            }
        }
        j++;
    }
    return false;
}

bool
arg_map_input_s2c(const Arg_map *am, int spos, int *cpos)
{
    if (spos < 0 || spos >= am->s_input_req + am->s_input_opt) {
        return false;
    }
    int j = 0;
    while (j < am->len) {
        if (am->pdata[j].is_s_input && (am->pdata[j].s_input_pos == spos)) {
            if (am->pdata[j].is_c_input) {
                *cpos = am->pdata[j].c_input_pos;
                return true;
            }
            else
                return false;
        }
        j++;
    }
    return false;
}

////////////////////////////////////////////////////////////////

bool
arg_map_output_i2c(const Arg_map *amap, int i, int *cpos)
{
    if (i < 0 || i >= amap->len)
        return false;
    if (!amap->pdata[i].is_c_output)
        return false;
    *cpos = amap->pdata[i].c_output_pos;
    return true;
}

bool
arg_map_output_i2s(const Arg_map *amap, int i, int *spos)
{
    if (i < 0 || i >= amap->len)
        return false;
    if (!amap->pdata[i].is_s_output)
        return false;
    *spos = amap->pdata[i].s_output_pos;
    return true;
}

bool
arg_map_output_c2i(const Arg_map *amap, int cpos, int *i)
{
    if (cpos < 0 || cpos >= amap->c_output_len)
        return false;
    int j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_output && (amap->pdata[j].c_output_pos == cpos)) {
            *i = j;
            return true;
        }
        j++;
    }
    return false;
}


bool
arg_map_output_s2i(const Arg_map *amap, int spos, int *i)
{
    if (spos < 0 || spos >= amap->s_output_len)
        return false;
    int j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_s_output && (amap->pdata[j].s_output_pos == spos)) {
            *i = j;
            return true;
        }
        j++;
    }
    return false;
}

bool
arg_map_output_c2s(const Arg_map *amap, int cpos, int *spos)
{
    if (cpos < 0 || cpos >= amap->c_output_len)
        return false;
    int j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_output && (amap->pdata[j].c_output_pos == cpos)) {
            if (amap->pdata[j].is_s_output) {
                *spos = amap->pdata[j].s_output_pos;
                return true;
            }
            else
                return false;
        }
        j++;
    }
    return false;
}

bool
arg_map_output_s2c(const Arg_map *am, int spos, int *cpos)
{
    if (spos < 0 || spos >= am->s_output_len)
        return false;
    int j = 0;
    while (j < am->len) {
        if (am->pdata[j].is_s_output && (am->pdata[j].s_output_pos == spos)) {
            if (am->pdata[j].is_c_output) {
                *cpos = am->pdata[j].c_output_pos;
                return true;
            }
            else
                return false;
        }
        j++;
    }
    return false;
}

////////////////////////////////////////////////////////////////

bool
arg_map_child_i(const Arg_map *am, int i, int *ichild)
{
    if (am->pdata[i].child == NULL)
        return false;
    *ichild = am->pdata[i].child->i;
    return true;
}

bool
arg_map_return_child_i(const Arg_map *am, int *ichild)
{
    if (am->return_val.child == NULL)
        return false;
    *ichild = am->return_val.child->i;
    return true;
}
