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
#include "../core.h"
#include "../type.h"
#include "gig_arg_map_priv.h"
#include "gig_data_type.h"
//#include "gig_util_priv.h"

#define gig_debug_amap(...) gig_debug_internal(LOG_LEVEL_DEBUG, "amap", __VA_ARGS__)

const char dir_strings[GIG_ARG_DIRECTION_COUNT][9] = {
    [GIG_ARG_DIRECTION_VOID] = "void",
    [GIG_ARG_DIRECTION_INPUT] = "input",
    [GIG_ARG_DIRECTION_INOUT] = "inout",
    [GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT] = "prealloc",
    [GIG_ARG_DIRECTION_OUTPUT] = "output"
};

const char tuple_strings[GIG_ARG_TUPLE_COUNT][11] = {
    [GIG_ARG_TUPLE_SINGLETON] = "singleton",
    [GIG_ARG_TUPLE_ARRAY] = "array",
    [GIG_ARG_TUPLE_ARRAY_SIZE] = "array_size"
};

const char presence_strings[GIG_ARG_PRESENCE_COUNT][9] = {
    [GIG_ARG_PRESENCE_REQUIRED] = "required",
    [GIG_ARG_PRESENCE_OPTIONAL] = "optional",
    [GIG_ARG_PRESENCE_IMPLICIT] = "implicit"
};

static void arg_map_compute_c_invoke_positions(GigArgMap *amap);
static void arg_map_compute_s_call_positions(GigArgMap *amap);
static void arg_map_entry_init(GigArgMapEntry *map);

static void
arg_map_entry_init(GigArgMapEntry *entry)
{
    memset(entry, 0, sizeof(GigArgMapEntry));
    entry->name = xstrdup("(uninitialized)");
}

GigArgMap *
gig_amap_allocate(size_t n)
{
    GigArgMap *amap;

    amap = xcalloc(1, sizeof(GigArgMap));
    amap->pdata = xcalloc(n, sizeof(GigArgMapEntry));
    amap->len = n;
    for (size_t i = 0; i < n; i++) {
        arg_map_entry_init(&amap->pdata[i]);
        amap->pdata[i].i = i;
    }
    arg_map_entry_init(&amap->return_val);

    return amap;
}

void
gig_amap_free(GigArgMap *amap)
{
    if (!amap)
        return;

    for (int i = 0; i < amap->len; i++) {
        gig_data_type_free(&amap->pdata[i].meta);
        free(amap->pdata[i].name);
    }
    gig_data_type_free(&amap->return_val.meta);
    free(amap->return_val.name);
    free(amap->pdata);
    free(amap->name);
    amap->pdata = NULL;
    free(amap);
}

void
gig_amap_dump(const char *name, const GigArgMap *amap)
{
    char s[100 * 3 + 20 * 4 + 1];
    size_t len;

    gig_debug_amap("%s - argument mapping", name ? name : amap->name);
    gig_debug_amap(" SCM inputs required: %d, optional: %d, outputs: %d", amap->s_input_req,
                   amap->s_input_opt, amap->s_output_len);
    gig_debug_amap(" C inputs: %d, outputs: %d", amap->c_input_len, amap->c_output_len);
    for (int i = 0; i < amap->len; i++) {
        const GigArgMapEntry *entry = &amap->pdata[i];
        len = snprintf(s, 100, " Arg %d: '%s' %s",
                       i, entry->name, gig_type_meta_describe(&entry->meta));
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
            len += snprintf(s + len, 20, ", SCM output %d", entry->s_output_pos);
        gig_debug_amap("%s", s);
        s[0] = '\0';

        if (amap->pdata[i].meta.n_params > 0) {
            GigTypeMeta *m2 = &amap->pdata[i].meta.params[0];
            len = snprintf(s, 100, "    Item Type: %s", gig_type_meta_describe(m2));
            gig_debug_amap("%s", s);
            s[0] = '\0';
        }
    }
    if (amap->return_val.meta.gtype != G_TYPE_NONE) {
        const GigArgMapEntry *entry = &amap->return_val;
        len = snprintf(s, 100, " Return: '%s' %s",
                       entry->name, gig_type_meta_describe(&entry->meta));
        len += snprintf(s + len, 100, ", %s, %s, %s",
                        dir_strings[entry->s_direction],
                        tuple_strings[entry->tuple], presence_strings[entry->presence]);
        gig_debug_amap("%s", s);
        s[0] = '\0';
        if (amap->return_val.meta.n_params > 0) {
            GigTypeMeta *m2 = &amap->return_val.meta.params[0];
            len = snprintf(s, 100, "    Item Type: %s", gig_type_meta_describe(m2));
            gig_debug_amap("%s", s);
            s[0] = '\0';
        }
    }
}

void
gig_amap_s_input_count(const GigArgMap *amap, int *required, int *optional)
{
    assert(amap != NULL);
    assert(required != NULL);
    assert(optional != NULL);

    *required = amap->s_input_req;
    *optional = amap->s_input_opt;
}


GigArgMapEntry *
gig_amap_get_input_entry_by_s(GigArgMap *amap, int spos)
{
    int i = 0;

    assert(amap != NULL);

    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == spos)) {
            return &amap->pdata[i];
        }
        i++;
    }
    assert_not_reached();
}

GigArgMapEntry *
gig_amap_get_output_entry_by_c(GigArgMap *amap, int cpos)
{
    assert(amap != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_c_output && (amap->pdata[i].c_output_pos == cpos)) {
            return &amap->pdata[i];
        }
        i++;
    }
    gig_return_val_if_reached(NULL);
}

// If this output element is an array with another output element
// being its size, this returns TRUE and stores the index of the other
// argument.
bool
gig_amap_output_child_c(GigArgMap *amap, int c_output_pos, int *cinvoke_output_array_size_index)
{
    assert(amap != NULL);
    assert(cinvoke_output_array_size_index != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_c_output && (amap->pdata[i].c_output_pos == c_output_pos)) {
            if (amap->pdata[i].meta.has_length_arg) {
                size_t idx = amap->pdata[i].meta.length_arg;
                *cinvoke_output_array_size_index = amap->pdata[idx].c_output_pos;
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
gig_amap_c_count(const GigArgMap *amap, int *c_input_len, int *c_output_len)
{
    assert(amap != NULL);
    assert(c_input_len != NULL);
    assert(c_output_len != NULL);

    *c_input_len = amap->c_input_len;
    *c_output_len = amap->c_output_len;
}

// For the gsubr argument at position INDEX, get the input and output
// index positions for this argument in the C function call.  Return
// TRUE if this gsubr argument is used in the C function call.

bool
gig_amap_input_s_2_input_c(const GigArgMap *amap, int s_input_pos, int *c_input_pos)
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
    gig_return_val_if_reached(false);
}

bool
gig_amap_input_s_2_output_c(const GigArgMap *amap, int s_input_pos, int *c_output_pos)
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
    gig_return_val_if_reached(false);
}

// For the gsubr argument at position INDEX, if it is an array whose
// length is supposed to be computed before the C function is invoked,
// return the input and output positions for the array size in the C
// function call.  Return TRUE if this gsubr argument's array size is
// used in the C function call.
bool
gig_amap_input_s_2_child_input_c(const GigArgMap *amap, int s_input_pos, int *c_input_pos)
{
    assert(amap != NULL);
    assert(c_input_pos != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            if (amap->pdata[i].meta.has_length_arg) {
                size_t i_child = amap->pdata[i].meta.length_arg;
                GigArgMapEntry *child = &amap->pdata[i_child];
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
    gig_return_val_if_reached(false);
}

bool
gig_amap_input_s_2_child_output_c(const GigArgMap *amap, int s_input_pos, int *c_output_pos)
{
    assert(amap != NULL);
    assert(c_output_pos != NULL);

    int i = 0;
    while (i < amap->len) {
        if (amap->pdata[i].is_s_input && (amap->pdata[i].s_input_pos == s_input_pos)) {
            if (amap->pdata[i].meta.has_length_arg) {
                size_t idx = amap->pdata[i].meta.length_arg;
                GigArgMapEntry *child = &amap->pdata[idx];
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
    gig_return_val_if_reached(false);
}

////////////////////////////////////////////////////////////////

bool
gig_amap_input_i2c(const GigArgMap *amap, int i, int *cpos)
{
    if (i < 0 || i >= amap->len)
        gig_return_val_if_reached(false);
    if (!amap->pdata[i].is_c_input)
        return false;
    *cpos = amap->pdata[i].c_input_pos;
    return true;
}

bool
gig_amap_input_i2s(const GigArgMap *amap, int i, int *spos)
{
    if (i < 0 || i >= amap->len)
        gig_return_val_if_reached(FALSE);
    if (!amap->pdata[i].is_s_input)
        return false;
    *spos = amap->pdata[i].s_input_pos;
    return true;
}

bool
gig_amap_input_c2i(const GigArgMap *amap, int cpos, int *i)
{
    if (cpos < 0 || cpos >= amap->c_input_len) {
        gig_return_val_if_reached(FALSE);
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
gig_amap_input_s2i(const GigArgMap *amap, int spos, int *i)
{
    if (spos < 0 || spos >= amap->s_input_req + amap->s_input_opt) {
        gig_return_val_if_reached(FALSE);
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
gig_amap_input_c2s(const GigArgMap *amap, int cpos, int *spos)
{
    if (cpos < 0 || cpos >= amap->c_input_len) {
        gig_return_val_if_reached(FALSE);
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
gig_amap_input_s2c(const GigArgMap *am, int spos, int *cpos)
{
    if (spos < 0 || spos >= am->s_input_req + am->s_input_opt) {
        gig_return_val_if_reached(FALSE);
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
gig_amap_output_i2c(const GigArgMap *amap, int i, int *cpos)
{
    if (i < 0 || i >= amap->len)
        gig_return_val_if_reached(false);
    if (!amap->pdata[i].is_c_output)
        return false;
    *cpos = amap->pdata[i].c_output_pos;
    return true;
}

bool
gig_amap_output_i2s(const GigArgMap *amap, int i, int *spos)
{
    if (i < 0 || i >= amap->len)
        gig_return_val_if_reached(false);
    if (!amap->pdata[i].is_s_output)
        return false;
    *spos = amap->pdata[i].s_output_pos;
    return true;
}

bool
gig_amap_output_c2i(const GigArgMap *amap, int cpos, int *i)
{
    if (cpos < 0 || cpos >= amap->c_output_len)
        gig_return_val_if_reached(false);
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
gig_amap_output_s2i(const GigArgMap *amap, int spos, int *i)
{
    if (spos < 0 || spos >= amap->s_output_len)
        gig_return_val_if_reached(false);
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
gig_amap_output_c2s(const GigArgMap *amap, int cpos, int *spos)
{
    if (cpos < 0 || cpos >= amap->c_output_len)
        gig_return_val_if_reached(false);
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
gig_amap_output_s2c(const GigArgMap *am, int spos, int *cpos)
{
    if (spos < 0 || spos >= am->s_output_len)
        gig_return_val_if_reached(false);
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
gig_amap_child_i(const GigArgMap *am, int i, int *ichild)
{
    if (am->pdata[i].meta.has_length_arg == false)
        return false;
    *ichild = am->pdata[i].meta.length_arg;
    return true;
}

bool
gig_amap_return_child_i(const GigArgMap *am, int *ichild)
{
    if (am->return_val.meta.has_length_arg == false)
        return false;
    *ichild = am->return_val.meta.length_arg;
    return true;
}

// Returns a list of all the GTypes in the arg map
size_t *
gig_amap_get_gtype_list(GigArgMap *amap, size_t *len)
{
    size_t n = 0;
    GType *types = xcalloc((1 + amap->len) * 3, sizeof(GType));

    if (amap->return_val.meta.gtype)
        types[n++] = amap->return_val.meta.gtype;
    for (size_t i = 0; i < amap->return_val.meta.n_params; i++)
        if (amap->return_val.meta.params[i].gtype)
            types[n++] = amap->return_val.meta.params[i].gtype;
    for (int j = 0; j < amap->len; j++) {
        if (amap->pdata[j].meta.gtype)
            types[n++] = amap->pdata[j].meta.gtype;
        for (size_t i = 0; i < amap->pdata[j].meta.n_params; i++)
            if (amap->pdata[j].meta.params[i].gtype)
                types[n++] = amap->pdata[j].meta.params[i].gtype;
    }
    *len = n;
    return types;
}

static void
gig_amap_entry_from_il(SCM il, GigArgMapEntry *entry)
{
    SCM val;
    char *str;

    val = scm_assq_ref(il, scm_from_utf8_symbol("name"));
    entry->name = scm_to_utf8_string(val);

    val = scm_assq_ref(il, scm_from_utf8_symbol("meta"));
    gig_type_meta_from_il(val, &(entry->meta));

    val = scm_assq_ref(il, scm_from_utf8_symbol("s-direction"));
    str = scm_to_utf8_symbol(val);
    for (int i = 0; i < GIG_ARG_DIRECTION_COUNT; i++) {
        if (strcmp(str, dir_strings[i]) == 0) {
            entry->s_direction = (GigArgDirection) i;
            break;
        }
    }
    free(str);

    val = scm_assq_ref(il, scm_from_utf8_symbol("tuple"));
    str = scm_to_utf8_symbol(val);
    for (int i = 0; i < GIG_ARG_TUPLE_COUNT; i++) {
        if (strcmp(str, tuple_strings[i]) == 0) {
            entry->tuple = (GigArgTuple) i;
            break;
        }
    }
    free(str);

    val = scm_assq_ref(il, scm_from_utf8_symbol("presence"));
    str = scm_to_utf8_symbol(val);
    for (int i = 0; i < GIG_ARG_PRESENCE_COUNT; i++) {
        if (strcmp(str, presence_strings[i]) == 0) {
            entry->presence = (GigArgPresence) i;
            break;
        }
    }
    free(str);

    val = scm_assq_ref(il, scm_from_utf8_symbol("i"));
    entry->i = scm_to_int(val);

    val = scm_assq_ref(il, scm_from_utf8_symbol("c-input-pos"));
    if (scm_is_true(val)) {
        entry->is_c_input = TRUE;
        entry->c_input_pos = scm_to_int(val);
    }

    val = scm_assq_ref(il, scm_from_utf8_symbol("c-output-pos"));
    if (scm_is_true(val)) {
        entry->is_c_output = TRUE;
        entry->c_output_pos = scm_to_int(val);
    }

    val = scm_assq_ref(il, scm_from_utf8_symbol("s-input-pos"));
    if (scm_is_true(val)) {
        entry->is_s_input = TRUE;
        entry->s_input_pos = scm_to_int(val);
    }

    val = scm_assq_ref(il, scm_from_utf8_symbol("s-output-pos"));
    if (scm_is_true(val)) {
        entry->is_s_output = TRUE;
        entry->s_output_pos = scm_to_int(val);
    }
}

GigArgMap *
gig_amap_new_from_il(SCM il)
{
    SCM val;
    GigArgMap *amap = xcalloc(1, sizeof(GigArgMap));

    val = scm_assq_ref(il, scm_from_utf8_symbol("name"));
    if (scm_is_true(val))
        amap->name = scm_to_utf8_string(val);
    else
        amap->name = NULL;
    val = scm_assq_ref(il, scm_from_utf8_symbol("is-method"));
    if (scm_is_true(val))
        amap->is_method = scm_to_bool(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("can-throw-gerror"));
    if (scm_is_true(val))
        amap->can_throw_gerror = scm_to_bool(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("s-input-req"));
    if (scm_is_true(val))
        amap->s_input_req = scm_to_int(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("s-input-opt"));
    if (scm_is_true(val))
        amap->s_input_opt = scm_to_int(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("s-output-len"));
    if (scm_is_true(val))
        amap->s_output_len = scm_to_int(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("c-input-len"));
    if (scm_is_true(val))
        amap->c_input_len = scm_to_int(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("c-output-len"));
    if (scm_is_true(val))
        amap->c_output_len = scm_to_int(val);
    // val = scm_assq_ref(il, scm_from_utf8_symbol("is-invalid"));
    // amap->is_invalid = scm_to_bool(val);

    SCM pdata_il = scm_assq_ref(il, scm_from_utf8_symbol("pdata"));
    if (scm_is_true(pdata_il)) {
        int len = scm_c_length(pdata_il);
        amap->pdata = xcalloc(len, sizeof(GigArgMapEntry));
        for (int i = 0; i < len; i++) {
            SCM entry_il = scm_c_list_ref(pdata_il, i);
            gig_amap_entry_from_il(entry_il, &(amap->pdata[i]));
        }
        amap->len = len;
    }
    else {
        amap->len = 0;
        amap->pdata = NULL;
    }

    SCM return_il = scm_assq_ref(il, scm_from_utf8_symbol("return-val"));
    gig_amap_entry_from_il(return_il, &(amap->return_val));

    return amap;
}

#ifdef GIG_PARSER

#define D(k,v) \
    il = scm_cons(scm_cons(scm_from_utf8_symbol(k),(v)),il)
#define F(x) \
    flags = scm_cons(scm_from_utf8_symbol(x), flags);

static SCM
amap_entry_to_il(GigArgMapEntry *entry)
{
    SCM il = SCM_EOL;
    D("name", scm_from_utf8_string(entry->name));
    D("meta", gig_type_meta_to_il(&(entry->meta)));
    D("s-direction", scm_from_utf8_symbol(dir_strings[entry->s_direction]));
    D("tuple", scm_from_utf8_symbol(tuple_strings[entry->tuple]));
    D("presence", scm_from_utf8_symbol(presence_strings[entry->presence]));
    D("i", scm_from_int(entry->i));
    if (entry->is_c_input)
        D("c-input-pos", scm_from_int(entry->c_input_pos));
    if (entry->is_c_output)
        D("c-output-pos", scm_from_int(entry->c_output_pos));
    if (entry->is_s_input)
        D("s-input-pos", scm_from_int(entry->s_input_pos));
    if (entry->is_s_output)
        D("s-output-pos", scm_from_int(entry->s_output_pos));
    return scm_reverse(il);
}

SCM
gig_amap_to_il(GigArgMap *amap)
{
    SCM il = SCM_EOL;
    if (amap->name)
        D("name", scm_from_utf8_string(amap->name));
    else
        D("name", SCM_BOOL_F);
    if (amap->is_method)
        D("is-method", scm_from_bool(amap->is_method));
    if (amap->can_throw_gerror)
        D("can-throw-gerror", scm_from_bool(amap->can_throw_gerror));
    if (amap->s_input_req)
        D("s-input-req", scm_from_int(amap->s_input_req));
    if (amap->s_input_opt)
        D("s-input-opt", scm_from_int(amap->s_input_opt));
    if (amap->s_output_len)
        D("s-output-len", scm_from_int(amap->s_output_len));
    if (amap->c_input_len)
        D("c-input-len", scm_from_int(amap->c_input_len));
    if (amap->c_output_len)
        D("c-output-len", scm_from_int(amap->c_output_len));
    // D("is-invalid", scm_from_bool(amap->is_invalid));
    if (amap->len > 0) {
        SCM pdata_il = SCM_EOL;
        for (int i = 0; i < amap->len; i++) {
            pdata_il =
                scm_append(scm_list_2(pdata_il, scm_list_1(amap_entry_to_il(&(amap->pdata[i])))));
        }
        D("pdata", pdata_il);
    }
    D("return-val", amap_entry_to_il(&(amap->return_val)));
    return scm_reverse(il);
}

#undef D

#endif
