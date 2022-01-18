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

#include <assert.h>
#include "y/argument.h"
#include "y/function_args.h"
#include "y/guile.h"

static void
store_argument(Args_store *store, GIArgument *arg, int invoke_in, int invoke_out,
                        bool inout, bool inout_free);

static void
store_scm(Args_store *store, SCM obj, Arg_map *amap, int s, const char *name);

static void
store_scm_list(Args_store *store, SCM s_args, Arg_map *amap, const char *subr);

static SCM
convert_output_args(Arg_map *amap, const gchar *func_name, GIArgument *in, GIArgument *out,
                     SCM output);

static GIArgument *find_output_arg(Arg_map_entry *entry, GIArgument *in, GIArgument *out);

void
initialize_args_store(Args_store *store, GObject *self, Arg_map *amap, SCM args,
                      const char *name)
{
    // Convert the scheme arguments into C.
    store_scm_list(store, args, amap, name);

    // For methods calls, the object gets inserted as the 1st argument.
    if (self) {
        store->in = realloc(store->in, (store->in_len + 1) * sizeof(GIArgument));
        memmove(store->in + 1, store->in, store->in_len * sizeof(GIArgument));
        store->in[0].v_pointer = self;
        store->in_len++;
    }

    // Since, in the Guile binding, we're allocating the output
    // parameters in most cases, here's where we make space for
    // immediate return arguments.  There's a trick here.  Sometimes
    // GLib expects to use these out_args directly, and sometimes it
    // expects out_args->v_pointer to point to allocated space.  I
    // allocate space for *all* the output arguments, even when not
    // needed.  It is easier than figuring out which output arguments
    // need allocation.
    store->boxed_out = xcalloc(store->out_len, sizeof(GIArgument));
    slist_prepend(&(store->free_list), store->boxed_out);
    for (size_t i = 0; i < store->out_len; i++)
        if (store->out[i].v_pointer == NULL)
            store->out[i].v_pointer = &(store->boxed_out[i]);
}

SCM
get_return_value(Args_store *store, GObject *self, Arg_map *amap, SCM args,
                 const char *name, GIArgument *return_arg, bool ok)
{
    SCM output = SCM_EOL;

    // Here is where I check to see if I used the allocated output
    // argument space created above.
    for (size_t i = 0; i < store->out_len; i++)
        if (store->out[i].v_pointer == &(store->boxed_out[i]))
            memcpy(&(store->out[i]), &(store->boxed_out[i]), sizeof(GIArgument));

    if (ok) {
        SCM s_return;
        size_t sz = -1;
        if (amap->return_val.arg.has_size) {
            size_t idx = amap->return_val.child->c_output_pos;
            sz = store->out[idx].v_size;
        }

        argument_c_to_scm(name, -1, &amap->return_val.arg, return_arg, &s_return, sz);
        if (scm_is_eq(s_return, SCM_UNSPECIFIED))
            output = SCM_EOL;
        else
            output = scm_list_1(s_return);


        if (self)
            output = convert_output_args(amap, name, store->in + 1, store->out, output);
        else
            output = convert_output_args(amap, name, store->in, store->out, output);
    }

    free(store->in);
    free(store->out);
    slist_free(&(store->free_list), free);

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
convert_output_args(Arg_map *amap, const gchar *func_name, GIArgument *in, GIArgument *out,
                     SCM output)
{
    debug_transfer("%s - convert_output_args", func_name);
    int s_output_pos;

    for (int c_output_pos = 0; c_output_pos < amap->c_output_len; c_output_pos++) {
        if (!arg_map_output_c2s(amap, c_output_pos, &s_output_pos))
            continue;

        Arg_map_entry *entry = arg_map_get_output_entry_by_c(amap, c_output_pos);

        GIArgument *arg;
        arg = find_output_arg(entry, in, out);
        if (arg == NULL)        // an INOUT argument has been eaten
        {
            output = scm_cons(SCM_UNSPECIFIED, output);
            continue;
        }

        SCM obj;
        size_t size = ARG_ARRAY_SIZE_UNKNOWN;

        if (entry->child) {
            // We need to know the size argument before we can process
            // this array argument.
            Arg_map_entry *size_entry = entry->child;
            GIArgument *size_arg = find_output_arg(size_entry, in, out);
            argument_c_to_scm(func_name, size_entry->i, &size_entry->arg, size_arg, &obj, -1);
            size = scm_is_integer(obj) ? scm_to_int(obj) : 0;
        }

        argument_c_to_scm(func_name, c_output_pos, &entry->arg, arg, &obj, size);
        output = scm_cons(obj, output);
    }
    return scm_reverse_x(output, SCM_EOL);
}

GIArgument *
find_output_arg(Arg_map_entry *entry, GIArgument *in, GIArgument *out)
{
    switch (entry->s_direction) {
    case ARG_DIRECTION_INPUT:      // may happen with sizes of preallocated outputs
        return in + entry->c_input_pos;
    case ARG_DIRECTION_INOUT:
        return (in + entry->c_input_pos)->v_pointer;
    case ARG_DIRECTION_PREALLOCATED_OUTPUT:
    case ARG_DIRECTION_OUTPUT:
        return out + entry->c_output_pos;
    default:
        assert_not_reached();
    }
}

static void
store_scm_list(Args_store *store, SCM s_args, Arg_map *amap, const char *subr)
{
    int args_count, required, optional;
    if (SCM_UNBNDP(s_args))
        args_count = 0;
    else
        args_count = scm_c_length(s_args);
    arg_map_s_input_count(amap, &required, &optional);
    if (args_count < required || args_count > required + optional)
        scm_error_num_args_subr(subr);

    int input_len, output_len;
    arg_map_c_count(amap, &input_len, &output_len);
    store->in_len = input_len;
    store->out_len = output_len;
    store->in = calloc(store->in_len, sizeof(GIArgument));
    store->out = calloc(store->out_len, sizeof(GIArgument));

    for (int i = 0; i < args_count; i++) {
        SCM obj = scm_c_list_ref(s_args, i);
        store_scm(store, obj, amap, i, subr);
    }
    return;
}

static void
store_scm(Args_store *store, SCM obj, Arg_map *amap, int s, const char *name)
{
    Arg_map_entry *entry;
    GIArgument arg;
    size_t size;

    // Convert an input scheme argument to a C invoke argument
    entry = arg_map_get_input_entry_by_s(amap, s);
    argument_scm_to_c(name, s, &entry->arg, obj, &(store->free_list), &arg, &size);

    int i;
    int c_invoke_in, c_invoke_out, c_child_invoke_in;
    bool is_in, is_out;
    bool inout, inout_free;

    // Store the converted argument.
    is_in = arg_map_input_s2c(amap, s, &c_invoke_in);
    is_out = arg_map_input_s_2_output_c(amap, s, &c_invoke_out);
    if (!is_in)
        c_invoke_in = -1;
    if (!is_out)
        c_invoke_out = -1;

    // Input/Output arguments have an extra implied level of
    // indirection.
    inout = is_in && is_out;
    if (inout) {
        arg_map_input_s2i(amap, s, &i);
        inout_free = (amap->pdata[i].arg.transfer == GI_TRANSFER_NOTHING);
    }
    else
        inout_free = FALSE;

    store_argument(store, &arg, c_invoke_in, c_invoke_out, inout, inout_free);

    // If this argument is an array with an associated size, store the
    // array size as well.
    if (arg_map_input_s_2_child_input_c(amap, s, &c_child_invoke_in)) {
        Arg_map_entry *size_entry = entry->child;
        GIArgument size_arg;
        size_t dummy_size;
        int c_child_invoke_out, i_child;

        argument_scm_to_c(name, s, &size_entry->arg, scm_from_size_t(size),
                              &(store->free_list), &size_arg, &dummy_size);

        is_in = arg_map_input_c2i(amap, c_child_invoke_in, &i_child);
        is_out = arg_map_output_i2c(amap, i_child, &c_child_invoke_out);
        assert(is_in);
        if (!is_out)
            c_child_invoke_out = -1;

        inout = is_in && is_out;
        if (inout)
            inout_free = (amap->pdata[i_child].arg.transfer == GI_TRANSFER_NOTHING);
        else
            inout_free = FALSE;
        store_argument(store, &size_arg, c_child_invoke_in, c_child_invoke_out, inout,
                                inout_free);
    }
}

static void
store_argument(Args_store *store, GIArgument *arg, int invoke_in, int invoke_out,
                        bool inout, bool inout_free)
{
    if (invoke_in >= 0) {
        if (inout) {
            GIArgument *dup = xmemdup(arg, sizeof(GIArgument));
            store->in[invoke_in].v_pointer = dup;
            if (inout_free)
                slist_prepend(&(store->free_list), dup);
            store->out[invoke_out].v_pointer = NULL;
        }
        else
            store->in[invoke_in] = *arg;
    }
    else if (invoke_out >= 0)
        store->out[invoke_out] = *arg;
}
