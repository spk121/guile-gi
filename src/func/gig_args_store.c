// Copyright (C) 2018, 2019, 2020, 2021, 2022 Michael L. Gran

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

#include "../core.h"
#include "../type.h"
#include "gig_arg_map_priv.h"
#include "gig_args_store.h"
#include "gig_argument.h"

static void
gig_callable_prepare_invoke(GigArgMap *amap, const char *name, GObject *self, SCM args,
                            GIArgument *in_args, GIArgument *out_args, slist_t **free_list,
                            GIArgument *out_boxes);
static void object_list_to_c_args(GigArgMap *amap, const char *subr, SCM s_args,
                                  GIArgument *in_args, slist_t **free_list, GIArgument *out_args);
static void object_to_c_arg(GigArgMap *amap, int s, const char *name, SCM obj, GIArgument *in_args,
                            slist_t **free_list, GIArgument *out_args);
static void store_argument(int invoke_in, int invoke_out, bool inout, bool inout_free,
                           GIArgument *arg, GIArgument *in_args, slist_t **free_list,
                           GIArgument *out_args);
static SCM gig_callable_return_value(GigArgMap *amap, const char *name, GObject *self, SCM args,
                                     bool ok, GIArgument *return_arg, GIArgument *in_args,
                                     GIArgument *out_args, slist_t **free_list,
                                     GIArgument *out_boxes);
static SCM convert_output_args(GigArgMap *amap, const char *func_name, GIArgument *in,
                               GIArgument *out, SCM output);
static GIArgument *find_output_arg(GigArgMapEntry *entry, GIArgument *in, GIArgument *out);


GigArgsStore *
gig_args_store_new(size_t in_len, size_t out_len)
{
    GigArgsStore *store = xcalloc(1, sizeof(GigArgsStore));
    store->in_len = in_len;
    store->out_len = out_len;
    store->in_args = xcalloc(in_len + 1, sizeof(GIArgument));
    store->out_args = xcalloc(out_len, sizeof(GIArgument));
    store->out_boxes = xcalloc(out_len, sizeof(GIArgument));
    store->free_list = NULL;
    return store;
}

void
gig_args_store_free(GigArgsStore *store)
{
    slist_free(&(store->free_list), free);
    free(store->out_boxes);
    free(store->out_args);
    free(store->in_args);
    free(store);
}

void
gig_args_store_initialize(GigArgsStore *store, GigArgMap *amap, const char *name, GObject *self,
                          SCM args)
{
    gig_callable_prepare_invoke(amap, name, self, args,
                                store->in_args,
                                store->out_args, &(store->free_list), store->out_boxes);
}

static void
gig_callable_prepare_invoke(GigArgMap *amap, const char *name, GObject *self, SCM args,
                            GIArgument *in_args, GIArgument *out_args, slist_t **free_list,
                            GIArgument *out_boxes)
{
    // Convert the scheme arguments into C.
    // For methods calls, the object gets inserted as the 1st argument.
    if (self != NULL) {
        in_args[0].v_pointer = self;
        object_list_to_c_args(amap, name, args, &(in_args)[1], free_list, out_args);
    }
    else {
        object_list_to_c_args(amap, name, args, in_args, free_list, out_args);
    }

    // Since, in the Guile binding, we're allocating the output
    // parameters in most cases, here's where we make space for
    // immediate return arguments.  There's a trick here.  Sometimes
    // GLib expects to use these out_args directly, and sometimes it
    // expects out_args->v_pointer to point to allocated space.  I
    // allocate space for *all* the output arguments, even when not
    // needed.  It is easier than figuring out which output arguments
    // need allocation.
    for (unsigned i = 0; i < amap->c_output_len; i++)
        if (out_args[i].v_pointer == NULL)
            out_args[i].v_pointer = &out_boxes[i];
}

static void
object_list_to_c_args(GigArgMap *amap, const char *subr, SCM s_args, GIArgument *in_args,
                      slist_t **free_list, GIArgument *out_args)
{
    int args_count, required, optional;
    if (SCM_UNBNDP(s_args))
        args_count = 0;
    else
        args_count = scm_c_length(s_args);
    gig_amap_s_input_count(amap, &required, &optional);
    if (args_count < required || args_count > required + optional)
        scm_error_num_args_subr(subr);

    for (int i = 0; i < args_count; i++) {
        SCM obj = scm_c_list_ref(s_args, i);
        object_to_c_arg(amap, i, subr, obj, in_args, free_list, out_args);

    }
    return;
}

static void
object_to_c_arg(GigArgMap *amap, int s, const char *name, SCM obj, GIArgument *in_args,
                slist_t **free_list, GIArgument *out_args)
{
    // Convert an input scheme argument to a C invoke argument
    GIArgument arg;
    GigArgMapEntry *entry;
    size_t size;
    int i;
    int c_invoke_in, c_invoke_out, c_child_invoke_in;
    bool is_in, is_out;
    bool inout, inout_free;

    entry = gig_amap_get_input_entry_by_s(amap, s);
    gig_argument_scm_to_c(name, s, &entry->meta, obj, free_list, &arg, &size);

    // Store the converted argument.
    is_in = gig_amap_input_s2c(amap, s, &c_invoke_in);
    is_out = gig_amap_input_s_2_output_c(amap, s, &c_invoke_out);
    if (!is_in)
        c_invoke_in = -1;
    if (!is_out)
        c_invoke_out = -1;

    // Input/Output arguments have an extra implied level of
    // indirection.
    inout = is_in && is_out;
    if (inout) {
        gig_amap_input_s2i(amap, s, &i);
        inout_free = (amap->pdata[i].meta.transfer == GIG_TRANSFER_NOTHING);
    }
    else
        inout_free = false;
    store_argument(c_invoke_in, c_invoke_out, inout, inout_free, &arg,
                   in_args, free_list, out_args);

    // If this argument is an array with an associated size, store the
    // array size as well.
    if (gig_amap_input_s_2_child_input_c(amap, s, &c_child_invoke_in)) {
        GigArgMapEntry *size_entry;
        GIArgument size_arg;
        size_t dummy_size;
        int c_child_invoke_out, i_child;
        gig_amap_input_c2i(amap, c_child_invoke_in, &i_child);
        size_entry = &amap->pdata[i_child];

        gig_argument_scm_to_c(name, s, &size_entry->meta, scm_from_size_t(size),
                              free_list, &size_arg, &dummy_size);

        is_in = gig_amap_input_c2i(amap, c_child_invoke_in, &i_child);
        is_out = gig_amap_output_i2c(amap, i_child, &c_child_invoke_out);
        assert(is_in);
        if (!is_out)
            c_child_invoke_out = -1;

        inout = is_in && is_out;
        if (inout)
            inout_free = (amap->pdata[i_child].meta.transfer == GIG_TRANSFER_NOTHING);
        else
            inout_free = false;
        store_argument(c_child_invoke_in, c_child_invoke_out, inout, inout_free, &size_arg,
                       in_args, free_list, out_args);
    }
}

static void
store_argument(int invoke_in, int invoke_out, bool inout, bool inout_free, GIArgument *arg,
               GIArgument *in_args, slist_t **free_list, GIArgument *out_args)
{
    if (invoke_in >= 0) {
        if (inout) {
            void **dup = xmemdup(arg, sizeof(GIArgument));
            in_args[invoke_in].v_pointer = dup;

            if (inout_free)
                slist_prepend(free_list, dup);

            out_args[invoke_out].v_pointer = NULL;
        }
        else {
            memcpy(&in_args[invoke_in], arg, sizeof(GIArgument));
        }
    }
    else if (invoke_out >= 0) {
        memcpy(&out_args[invoke_out], arg, sizeof(GIArgument));
    }
}

SCM
gig_args_store_return_value(GigArgsStore *store, GigArgMap *amap, const char *name, GObject *self,
                            SCM args, bool ok, GIArgument *return_arg)
{
    return
        gig_callable_return_value(amap, name, self, args, ok, return_arg,
                                  store->in_args, store->out_args,
                                  &(store->free_list), store->out_boxes);
}

static SCM
gig_callable_return_value(GigArgMap *amap, const char *name, GObject *self, SCM args, bool ok,
                          GIArgument *return_arg, GIArgument *in_args, GIArgument *out_args,
                          slist_t **free_list, GIArgument *out_boxes)
{
    SCM output = SCM_EOL;

    // Here is where I check to see if I used the allocated
    // output argument space created above.
    for (unsigned i = 0; i < amap->c_output_len; i++)
        if (out_args[i].v_pointer == &out_boxes[i])
            memcpy(&out_args[i], &out_boxes[i], sizeof(GIArgument));

    if (ok) {
        SCM s_return;
        size_t sz = -1;
        int i_child, c_child;
        if (gig_amap_return_child_i(amap, &i_child)) {
            gig_amap_output_i2c(amap, i_child, &c_child);
            sz = out_args[c_child].v_size;
        }

        gig_argument_c_to_scm(name, -1, &amap->return_val.meta, return_arg, &s_return, sz);
        if (scm_is_eq(s_return, SCM_UNSPECIFIED))
            output = SCM_EOL;
        else
            output = scm_list_1(s_return);


        if (self)
            output = convert_output_args(amap, name, &(in_args[1]), out_args, output);
        else
            output = convert_output_args(amap, name, in_args, out_args, output);
    }

    if (!ok)
        return SCM_UNDEFINED;

    switch (scm_c_length(output)) {
    case 0:
        return SCM_UNSPECIFIED;
    case 1:
        return scm_car(output);
    default:
        return scm_values(output);
    }
}

static SCM
convert_output_args(GigArgMap *amap, const char *func_name, GIArgument *in, GIArgument *out,
                    SCM output)
{
    gig_debug_transfer("%s - convert_output_args", func_name);
    int s_output_pos;

    for (int c_output_pos = 0; c_output_pos < amap->c_output_len; c_output_pos++) {
        if (!gig_amap_output_c2s(amap, c_output_pos, &s_output_pos))
            continue;

        GigArgMapEntry *entry = gig_amap_get_output_entry_by_c(amap, c_output_pos);

        GIArgument *arg;
        arg = find_output_arg(entry, in, out);
        if (arg == NULL)        // an INOUT argument has been eaten
        {
            output = scm_cons(SCM_UNSPECIFIED, output);
            continue;
        }

        SCM obj;
        size_t size = GIG_ARRAY_SIZE_UNKNOWN;

        if (entry->meta.has_length_arg) {
            size_t idx = entry->meta.length_arg;
            // We need to know the size argument before we can process
            // this array argument.
            GigArgMapEntry *size_entry = &amap->pdata[idx];
            GIArgument *size_arg = find_output_arg(size_entry, in, out);
            gig_argument_c_to_scm(func_name, size_entry->i, &size_entry->meta, size_arg, &obj, -1);
            size = scm_is_integer(obj) ? scm_to_int(obj) : 0;
        }

        gig_argument_c_to_scm(func_name, c_output_pos, &entry->meta, arg, &obj, size);
        output = scm_cons(obj, output);
    }
    return scm_reverse_x(output, SCM_EOL);
}

static GIArgument *
find_output_arg(GigArgMapEntry *entry, GIArgument *in, GIArgument *out)
{
    switch (entry->s_direction) {
    case GIG_ARG_DIRECTION_INPUT:      // may happen with sizes of preallocated outputs
        return in + entry->c_input_pos;
    case GIG_ARG_DIRECTION_INOUT:
        return (in + entry->c_input_pos)->v_pointer;
    case GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT:
    case GIG_ARG_DIRECTION_OUTPUT:
        return out + entry->c_output_pos;
    default:
        assert_not_reached();
    }
}
