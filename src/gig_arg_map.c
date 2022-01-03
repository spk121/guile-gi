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

#include <stdio.h>
#include <string.h>
#include "gig_arg_map.h"
#include "gig_data_type.h"
#include "gig_util.h"

#define gig_debug_amap(...) gig_debug_internal(G_LOG_LEVEL_DEBUG, "amap", __VA_ARGS__)

const gchar dir_strings[GIG_ARG_DIRECTION_COUNT][9] = {
    [GIG_ARG_DIRECTION_VOID] = "void",
    [GIG_ARG_DIRECTION_INPUT] = "input",
    [GIG_ARG_DIRECTION_INOUT] = "inout",
    [GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT] = "prealloc",
    [GIG_ARG_DIRECTION_OUTPUT] = "output"
};

const gchar tuple_strings[GIG_ARG_TUPLE_COUNT][11] = {
    [GIG_ARG_TUPLE_SINGLETON] = "singleton",
    [GIG_ARG_TUPLE_ARRAY] = "array",
    [GIG_ARG_TUPLE_ARRAY_SIZE] = "array_size"
};

const gchar presence_strings[GIG_ARG_PRESENCE_COUNT][9] = {
    [GIG_ARG_PRESENCE_REQUIRED] = "required",
    [GIG_ARG_PRESENCE_OPTIONAL] = "optional",
    [GIG_ARG_PRESENCE_IMPLICIT] = "implicit"
};

static GigArgMap *arg_map_allocate(gsize n);
static void arg_map_apply_function_info(GigArgMap *amap, GIFunctionInfo *func_info);
static void arg_map_determine_argument_presence(GigArgMap *amap, GIFunctionInfo *info);
static void arg_map_compute_c_invoke_positions(GigArgMap *amap);
static void arg_map_compute_s_call_positions(GigArgMap *amap);
static void arg_map_entry_init(GigArgMapEntry *map);

static void
arg_map_entry_init(GigArgMapEntry *entry)
{
    memset(entry, 0, sizeof(GigArgMapEntry));
    entry->name = xstrdup("(uninitialized)");
}

// Gather information on how to map Scheme arguments to C arguments.
GigArgMap *
gig_amap_new(const gchar *name, GICallableInfo *function_info)
{
    GigArgMap *amap;
    gsize n;

    n = g_callable_info_get_n_args(function_info);
    amap = arg_map_allocate(n);
    g_free(amap->name);
    amap->name = xstrdup(g_base_info_get_name(function_info));
    arg_map_apply_function_info(amap, function_info);
    if (amap->is_invalid) {
        gig_amap_free(amap);
        return NULL;
    }
    arg_map_determine_argument_presence(amap, function_info);
    arg_map_compute_c_invoke_positions(amap);
    arg_map_compute_s_call_positions(amap);
    gig_amap_dump(name, amap);
    return amap;
}

static GigArgMap *
arg_map_allocate(gsize n)
{
    GigArgMap *amap;

    amap = xcalloc(1, sizeof(GigArgMap));
    amap->pdata = xcalloc(n, sizeof(GigArgMapEntry));
    amap->len = n;
    for (gsize i = 0; i < n; i++) {
        arg_map_entry_init(&amap->pdata[i]);
        amap->pdata[i].i = i;
    }
    arg_map_entry_init(&amap->return_val);

    return amap;
}

static void
arg_map_apply_function_info(GigArgMap *amap, GIFunctionInfo *func_info)
{
    gint i, n;
    GIArgInfo *arg_info;

    n = amap->len;

    for (i = 0; i < n; i++) {
        arg_info = g_callable_info_get_arg(func_info, i);
        gig_type_meta_init_from_arg_info(&amap->pdata[i].meta, arg_info);
        g_free(amap->pdata[i].name);
        amap->pdata[i].name = xstrdup(g_base_info_get_name(arg_info));
        g_base_info_unref(arg_info);
        amap->is_invalid |= amap->pdata[i].meta.is_invalid;
    }

    gig_type_meta_init_from_callable_info(&amap->return_val.meta, func_info);
    g_free(amap->return_val.name);
    amap->return_val.name = xstrdup("%return");
    amap->is_invalid |= amap->return_val.meta.is_invalid;
}

static void
arg_map_determine_array_length_index(GigArgMap *amap, GigArgMapEntry *entry, GITypeInfo *info)
{
    if (entry->meta.gtype == G_TYPE_ARRAY && entry->meta.has_size) {
        gint idx = g_type_info_get_array_length(info);

        g_assert_cmpint(idx, !=, -1);

        entry->tuple = GIG_ARG_TUPLE_ARRAY;
        entry->child = amap->pdata + idx;
        entry->child->tuple = GIG_ARG_TUPLE_ARRAY_SIZE;
        entry->child->presence = GIG_ARG_PRESENCE_IMPLICIT;
        entry->child->parent = entry;
        entry->child->is_s_output = 0;
    }
}

static void
arg_map_determine_argument_presence(GigArgMap *amap, GIFunctionInfo *info)
{
    GigArgMapEntry *entry;
    gboolean opt_flag = TRUE;
    gint i, n;

    n = amap->len;

    // may-be-null parameters at the end of the C call can be made
    // optional parameters in the gsubr call.
    for (i = n - 1; i >= 0; i--) {
        entry = &amap->pdata[i];
        entry->tuple = GIG_ARG_TUPLE_SINGLETON;
        if (entry->meta.is_in || (entry->meta.is_out && entry->meta.is_caller_allocates)) {
            if (opt_flag && entry->meta.is_nullable)
                entry->presence = GIG_ARG_PRESENCE_OPTIONAL;
            else {
                entry->presence = GIG_ARG_PRESENCE_REQUIRED;
                opt_flag = FALSE;
            }
        }
        else {
            entry->presence = GIG_ARG_PRESENCE_IMPLICIT;
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

    amap->return_val.tuple = GIG_ARG_TUPLE_SINGLETON;
    GITypeInfo *return_type = g_callable_info_get_return_type(info);
    arg_map_determine_array_length_index(amap, &amap->return_val, return_type);
    g_base_info_unref(return_type);
}

static void
arg_map_compute_c_invoke_positions(GigArgMap *amap)
{
    gint i, n;
    GigArgMapEntry *entry;
    n = amap->len;

    gint c_input_pos = 0;
    gint c_output_pos = 0;
    for (i = 0; i < n; i++) {
        entry = &amap->pdata[i];

        // Here we find the positions of this argument in the
        // g_function_info_invoke call.  Also, some output parameters
        // require a SCM container to be passed in to the SCM GSubr
        // call.
        if (entry->meta.is_in && !entry->meta.is_out) {
            entry->s_direction = GIG_ARG_DIRECTION_INPUT;
            entry->is_c_input = 1;
            entry->c_input_pos = c_input_pos++;
        }
        else if (entry->meta.is_in && entry->meta.is_out) {
            entry->s_direction = GIG_ARG_DIRECTION_INOUT;
            entry->is_c_input = 1;
            entry->c_input_pos = c_input_pos++;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
        else if (entry->meta.is_out && entry->meta.is_caller_allocates) {
            entry->s_direction = GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
        else {
            entry->s_direction = GIG_ARG_DIRECTION_OUTPUT;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
    }

    if (amap->return_val.meta.is_out)
        amap->return_val.s_direction = GIG_ARG_DIRECTION_OUTPUT;
    else
        amap->return_val.s_direction = GIG_ARG_DIRECTION_VOID;

    amap->c_input_len = c_input_pos;
    amap->c_output_len = c_output_pos;
}

static void
arg_map_compute_s_call_positions(GigArgMap *amap)
{
    gint i, n;
    GigArgMapEntry *entry;
    n = amap->len;

    gint s_input_pos = 0;
    gint s_output_pos = 0;
    // We now can decide where these arguments appear in the SCM GSubr
    // call.
    for (i = 0; i < n; i++) {
        entry = &amap->pdata[i];

        // TODO: Why check entry->tuple instead of entry->presence?
        //       The latter appears to be buggy in some way.
        if (entry->tuple == GIG_ARG_TUPLE_ARRAY_SIZE)
            continue;

        switch (entry->s_direction) {
        case GIG_ARG_DIRECTION_INPUT:
        case GIG_ARG_DIRECTION_INOUT:
        case GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT:
            entry->is_s_input = 1;
            entry->s_input_pos = s_input_pos++;
            if (entry->presence == GIG_ARG_PRESENCE_REQUIRED)
                amap->s_input_req++;
            else if (entry->presence == GIG_ARG_PRESENCE_OPTIONAL)
                amap->s_input_opt++;

            if (entry->s_direction == GIG_ARG_DIRECTION_INPUT)
                break;
            /* fallthrough */
        case GIG_ARG_DIRECTION_OUTPUT:
            entry->is_s_output = 1;
            entry->s_output_pos = s_output_pos++;
            break;
        default:
            g_assert_not_reached();
        }
    }

    amap->s_output_len = s_output_pos;
    g_assert_cmpint(amap->s_input_req + amap->s_input_opt, ==, s_input_pos);
}

void
gig_amap_free(GigArgMap *amap)
{
    if (!amap)
        return;

    for (gint i = 0; i < amap->len; i++) {
        gig_data_type_free(&amap->pdata[i].meta);
        g_free(amap->pdata[i].name);
    }
    gig_data_type_free(&amap->return_val.meta);
    g_free(amap->return_val.name);
    g_free(amap->pdata);
    g_free(amap->name);
    amap->pdata = NULL;
    g_free(amap);
}

void
gig_amap_dump(const gchar *name, const GigArgMap *amap)
{
    gig_debug_amap("%s - argument mapping", name ? name : amap->name);
    gig_debug_amap(" SCM inputs required: %d, optional: %d, outputs: %d", amap->s_input_req,
                   amap->s_input_opt, amap->s_output_len);
    gig_debug_amap(" C inputs: %d, outputs: %d", amap->c_input_len, amap->c_output_len);
    for (gint i = 0; i < amap->len; i++) {
        const GigArgMapEntry *entry = &amap->pdata[i];
        GString *s = g_string_new(NULL);
        g_string_append_printf(s, " Arg %d: '%s' %s",
                               i, entry->name, gig_type_meta_describe(&entry->meta));
        g_string_append_printf(s, ", %s, %s, %s",
                               dir_strings[entry->s_direction],
                               tuple_strings[entry->tuple], presence_strings[entry->presence]);
        if (entry->is_c_input)
            g_string_append_printf(s, ", C input %d", entry->c_input_pos);
        if (entry->is_c_output)
            g_string_append_printf(s, ", C output %d", entry->c_output_pos);
        if (entry->is_s_input)
            g_string_append_printf(s, ", SCM input %d", entry->s_input_pos);
        if (entry->is_s_output)
            g_string_append_printf(s, ", S output %d", entry->c_output_pos);
        gig_debug_amap("%s", s->str);
        g_string_free(s, TRUE);

        if (amap->pdata[i].meta.n_params > 0) {
            GigTypeMeta *m2 = &amap->pdata[i].meta.params[0];
            s = g_string_new(NULL);
            g_string_append_printf(s, "    Item Type: %s", gig_type_meta_describe(m2));
            gig_debug_amap("%s", s->str);
            g_string_free(s, TRUE);
        }
    }
    if (amap->return_val.meta.gtype != G_TYPE_NONE) {
        const GigArgMapEntry *entry = &amap->return_val;
        GString *s = g_string_new(NULL);
        g_string_append_printf(s, " Return: '%s' %s",
                               entry->name, gig_type_meta_describe(&entry->meta));
        g_string_append_printf(s, ", %s, %s, %s",
                               dir_strings[entry->s_direction],
                               tuple_strings[entry->tuple], presence_strings[entry->presence]);
        gig_debug_amap("%s", s->str);
        g_string_free(s, TRUE);
        if (amap->return_val.meta.n_params > 0) {
            GigTypeMeta *m2 = &amap->return_val.meta.params[0];
            s = g_string_new(NULL);
            g_string_append_printf(s, "    Item Type: %s", gig_type_meta_describe(m2));
            gig_debug_amap("%s", s->str);
            g_string_free(s, TRUE);
        }
    }
}

void
gig_amap_s_input_count(const GigArgMap *amap, gint *required, gint *optional)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(required);
    g_assert_nonnull(optional);
    *required = amap->s_input_req;
    *optional = amap->s_input_opt;
}

GigArgMapEntry *
gig_amap_get_input_entry_by_s(GigArgMap *amap, gint spos)
{
    g_assert_nonnull(amap);
    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == spos)) {
            return &amap->pdata[i];
        }
        i++;
    }
    g_assert_not_reached();
}

GigArgMapEntry *
gig_amap_get_output_entry_by_c(GigArgMap *amap, gint cpos)
{
    g_assert_nonnull(amap);

    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_c_output && (amap->pdata[i].c_output_pos == cpos)) {
            return &amap->pdata[i];
        }
        i++;
    }
    g_return_val_if_reached(NULL);
}

// If this output element is an array with another output element
// being its size, this returns TRUE and stores the index of the other
// argument.
gboolean
gig_amap_output_child_c(GigArgMap *amap, gint c_output_pos, gint *cinvoke_output_array_size_index)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(cinvoke_output_array_size_index);

    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_c_output && (amap->pdata[i].c_output_pos == c_output_pos)) {
            if (amap->pdata[i].child) {
                *cinvoke_output_array_size_index = amap->pdata[i].child->c_output_pos;
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
gig_amap_c_count(const GigArgMap *amap, gint *c_input_len, gint *c_output_len)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(c_input_len);
    g_assert_nonnull(c_output_len);

    *c_input_len = amap->c_input_len;
    *c_output_len = amap->c_output_len;
}

// For the gsubr argument at position INDEX, get the input and output
// index positions for this argument in the C function call.  Return
// TRUE if this gsubr argument is used in the C function call.

gboolean
gig_amap_input_s_2_input_c(const GigArgMap *amap, gint s_input_pos, gint *c_input_pos)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(c_input_pos);

    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            if (amap->pdata[i].is_c_input) {
                *c_input_pos = amap->pdata[i].c_input_pos;
                return TRUE;
            }
            else
                return FALSE;
        }
        i++;
    }
    g_return_val_if_reached(FALSE);
}

gboolean
gig_amap_input_s_2_output_c(const GigArgMap *amap, gint s_input_pos, gint *c_output_pos)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(c_output_pos);

    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            if (amap->pdata[i].is_c_output) {
                *c_output_pos = amap->pdata[i].c_output_pos;
                return TRUE;
            }
            else
                return FALSE;
        }
        i++;
    }
    g_return_val_if_reached(FALSE);
}

// For the gsubr argument at position INDEX, if it is an array whose
// length is supposed to be computed before the C function is invoked,
// return the input and output positions for the array size in the C
// function call.  Return TRUE if this gsubr argument's array size is
// used in the C function call.
gboolean
gig_amap_input_s_2_child_input_c(const GigArgMap *amap, gint s_input_pos, gint *c_input_pos)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(c_input_pos);

    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            GigArgMapEntry *child = amap->pdata[i].child;
            if (child) {
                if (child->is_c_input) {
                    *c_input_pos = child->c_input_pos;
                    return TRUE;
                }
                else
                    return FALSE;
            }
            else
                return FALSE;
        }
        i++;
    }
    g_return_val_if_reached(FALSE);
}

gboolean
gig_amap_input_s_2_child_output_c(const GigArgMap *amap, gint s_input_pos, gint *c_output_pos)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(c_output_pos);

    gint i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            GigArgMapEntry *child = amap->pdata[i].child;
            if (child) {
                if (child->is_c_output) {
                    *c_output_pos = child->c_output_pos;
                    return TRUE;
                }
                else
                    return FALSE;
            }
            else
                return FALSE;
        }
        i++;
    }
    g_return_val_if_reached(FALSE);
}

////////////////////////////////////////////////////////////////

gboolean
gig_amap_input_i2c(const GigArgMap *amap, gint i, gint *cpos)
{
    if (i < 0 || i >= amap->len)
        g_return_val_if_reached(FALSE);
    if (!amap->pdata[i].is_c_input)
        return FALSE;
    *cpos = amap->pdata[i].c_input_pos;
    return TRUE;
}

gboolean
gig_amap_input_i2s(const GigArgMap *amap, gint i, gint *spos)
{
    if (i < 0 || i >= amap->len)
        g_return_val_if_reached(FALSE);
    if (!amap->pdata[i].is_s_input)
        return FALSE;
    *spos = amap->pdata[i].s_input_pos;
    return TRUE;
}

gboolean
gig_amap_input_c2i(const GigArgMap *amap, gint cpos, gint *i)
{
    if (cpos < 0 || cpos >= amap->c_input_len) {
        g_return_val_if_reached(FALSE);
    }
    gint j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_input && (amap->pdata[j].c_input_pos == cpos)) {
            *i = j;
            return TRUE;
        }
        j++;
    }
    return FALSE;
}


gboolean
gig_amap_input_s2i(const GigArgMap *amap, gint spos, gint *i)
{
    if (spos < 0 || spos >= amap->s_input_req + amap->s_input_opt) {
        g_return_val_if_reached(FALSE);
    }
    gint j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_s_input && (amap->pdata[j].s_input_pos == spos)) {
            *i = j;
            return TRUE;
        }
        j++;
    }
    return FALSE;
}

gboolean
gig_amap_input_c2s(const GigArgMap *amap, gint cpos, gint *spos)
{
    if (cpos < 0 || cpos >= amap->c_input_len) {
        g_return_val_if_reached(FALSE);
    }
    gint j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_input && (amap->pdata[j].c_input_pos == cpos)) {
            if (amap->pdata[j].is_s_input) {
                *spos = amap->pdata[j].s_input_pos;
                return TRUE;
            }
            else {
                return FALSE;
            }
        }
        j++;
    }
    return FALSE;
}

gboolean
gig_amap_input_s2c(const GigArgMap *am, gint spos, gint *cpos)
{
    if (spos < 0 || spos >= am->s_input_req + am->s_input_opt) {
        g_return_val_if_reached(FALSE);
    }
    int j = 0;
    while (j < am->len) {
        if (am->pdata[j].is_s_input && (am->pdata[j].s_input_pos == spos)) {
            if (am->pdata[j].is_c_input) {
                *cpos = am->pdata[j].c_input_pos;
                return TRUE;
            }
            else
                return FALSE;
        }
        j++;
    }
    return FALSE;
}

////////////////////////////////////////////////////////////////

gboolean
gig_amap_output_i2c(const GigArgMap *amap, gint i, gint *cpos)
{
    if (i < 0 || i >= amap->len)
        g_return_val_if_reached(FALSE);
    if (!amap->pdata[i].is_c_output)
        return FALSE;
    *cpos = amap->pdata[i].c_output_pos;
    return TRUE;
}

gboolean
gig_amap_output_i2s(const GigArgMap *amap, gint i, gint *spos)
{
    if (i < 0 || i >= amap->len)
        g_return_val_if_reached(FALSE);
    if (!amap->pdata[i].is_s_output)
        return FALSE;
    *spos = amap->pdata[i].s_output_pos;
    return TRUE;
}

gboolean
gig_amap_output_c2i(const GigArgMap *amap, gint cpos, gint *i)
{
    if (cpos < 0 || cpos >= amap->c_output_len)
        g_return_val_if_reached(FALSE);
    gint j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_output && (amap->pdata[j].c_output_pos == cpos)) {
            *i = j;
            return TRUE;
        }
        j++;
    }
    return FALSE;
}


gboolean
gig_amap_output_s2i(const GigArgMap *amap, gint spos, gint *i)
{
    if (spos < 0 || spos >= amap->s_output_len)
        g_return_val_if_reached(FALSE);
    gint j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_s_output && (amap->pdata[j].s_output_pos == spos)) {
            *i = j;
            return TRUE;
        }
        j++;
    }
    return FALSE;
}

gboolean
gig_amap_output_c2s(const GigArgMap *amap, gint cpos, gint *spos)
{
    if (cpos < 0 || cpos >= amap->c_output_len)
        g_return_val_if_reached(FALSE);
    gint j = 0;
    while (j < amap->len) {
        if (amap->pdata[j].is_c_output && (amap->pdata[j].c_output_pos == cpos)) {
            if (amap->pdata[j].is_s_output) {
                *spos = amap->pdata[j].s_output_pos;
                return TRUE;
            }
            else
                return FALSE;
        }
        j++;
    }
    return FALSE;
}

gboolean
gig_amap_output_s2c(const GigArgMap *am, gint spos, gint *cpos)
{
    if (spos < 0 || spos >= am->s_output_len)
        g_return_val_if_reached(FALSE);
    int j = 0;
    while (j < am->len) {
        if (am->pdata[j].is_s_output && (am->pdata[j].s_output_pos == spos)) {
            if (am->pdata[j].is_c_output) {
                *cpos = am->pdata[j].c_output_pos;
                return TRUE;
            }
            else
                return FALSE;
        }
        j++;
    }
    return FALSE;
}

////////////////////////////////////////////////////////////////

gboolean
gig_amap_child_i(const GigArgMap *am, gint i, gint *ichild)
{
    if (am->pdata[i].child == NULL)
        return FALSE;
    *ichild = am->pdata[i].child->i;
    return TRUE;
}

gboolean
gig_amap_return_child_i(const GigArgMap *am, gint *ichild)
{
    if (am->return_val.child == NULL)
        return FALSE;
    *ichild = am->return_val.child->i;
    return TRUE;
}
