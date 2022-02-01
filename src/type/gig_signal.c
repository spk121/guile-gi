// Copyright (C) 2019, 2020 Michael L. Gran

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
#include "core.h"
#include "gig_signal.h"
#include "gig_object.h"
#include "gig_value.h"
#include "gig_type.h"
#include "gig_argument.h"
#include "gig_flag.h"

typedef void (*handler_func)(void *);
static SCM signal_slot_syms[GIG_SIGNAL_SLOT_COUNT];

SCM gig_signal_type;

SCM
gig_signal_ref(SCM signal, GigSignalSlot slot)
{
    return scm_slot_ref(signal, signal_slot_syms[slot]);
}

static SCM signal_accu_first_wins;
static SCM signal_accu_true_handled;

static SCM make_signal_proc;

static intbool_t
scm_signal_accu(GSignalInvocationHint *ihint, GValue *seed, const GValue *element, void *procedure)
{
    SCM _seed, _element, result;
    _seed = gig_value_as_scm(seed, FALSE);
    _element = gig_value_as_scm(element, FALSE);

    result = scm_call_2(SCM_PACK_POINTER(procedure), _seed, _element);
    switch (scm_c_nvalues(result)) {
    case 0:
        return TRUE;
    case 1:
        if (!scm_is_eq(result, SCM_UNSPECIFIED))
            return_val_if_fail(!gig_value_from_scm(seed, result), FALSE);
        return TRUE;
    case 2:
    {
        intbool_t ret = scm_is_true(scm_c_value_ref(result, 0));
        SCM next_seed = scm_c_value_ref(result, 1);
        return_val_if_fail(!gig_value_from_scm(seed, next_seed), FALSE);
        return ret;
    }
    default:
    {
        SCM name = scm_procedure_name(SCM_PACK_POINTER(procedure));
        char *_name = NULL;
        scm_dynwind_begin(0);
        if (scm_is_symbol(name)) {
            _name = scm_to_utf8_symbol(name);
            scm_dynwind_unwind_handler(free, _name, SCM_F_WIND_EXPLICITLY);
        }
        scm_misc_error(_name,
                       "callback accumulator returned ~S when 0, 1, or 2 values were expected",
                       scm_list_1(result));
        scm_dynwind_end();
    }
    }
}

GigSignalSpec *
gig_signalspec_from_obj(SCM obj)
{
    unsigned n_params;
    gtype_t *params;
    SCM sparams, saccu;
    GSignalFlags flags = 0;
    GigSignalSpec *spec = NULL;

    SCM_ASSERT_TYPE(SCM_IS_A_P(obj, gig_signal_type), obj, SCM_ARG1, "%scm->signalspec", "signal");

    scm_dynwind_begin(0);
    spec = xcalloc(1, sizeof(GigSignalSpec));
    scm_dynwind_unwind_handler((handler_func) gig_free_signalspec, spec, 0);
    spec->return_type = scm_to_gtype(gig_signal_ref(obj, GIG_SIGNAL_SLOT_RETURN_TYPE));
    if (spec->return_type == G_TYPE_INVALID)
        scm_misc_error("%scm->signalspec", "signal ~A has no return type",
                       scm_list_1(gig_signal_ref(obj, GIG_SIGNAL_SLOT_NAME)));
    spec->signal_name = scm_to_utf8_string(gig_signal_ref(obj, GIG_SIGNAL_SLOT_NAME));

    sparams = gig_signal_ref(obj, GIG_SIGNAL_SLOT_PARAM_TYPES);
    saccu = gig_signal_ref(obj, GIG_SIGNAL_SLOT_ACCUMULATOR);
    n_params = scm_c_length(sparams);
    params = xcalloc(n_params, sizeof(gtype_t));

    for (unsigned i = 0; i < n_params; i++, sparams = scm_cdr(sparams))
        params[i] = scm_to_gtype(scm_car(sparams));

    do {
        SCM _flags = gig_signal_ref(obj, GIG_SIGNAL_SLOT_FLAGS);
        // accept #f as 0, otherwise use flags->number
        if (scm_is_true(_flags))
            flags = gig_flags_to_uint(_flags);
    } while (0);

    spec->signal_flags = flags;
    if (SCM_UNBNDP(saccu) || scm_is_false(saccu)) {
        spec->accumulator = NULL;
        spec->accu_data = NULL;
    }
    else if (scm_is_eq(saccu, signal_accu_first_wins)) {
        if (spec->return_type == G_TYPE_NONE)
            scm_misc_error("%scm->signalspec",
                           "signal ~A must return a value to use the first-wins accumulator",
                           scm_list_1(scm_from_utf8_string(spec->signal_name)));
        spec->accumulator = g_signal_accumulator_first_wins;
        spec->accu_data = NULL;
    }
    else if (scm_is_eq(saccu, signal_accu_true_handled)) {
        if (spec->return_type != G_TYPE_BOOLEAN)
            scm_misc_error("%scm->signalspec",
                           "signal ~A must have a boolean return type to use the true-handled accumulator",
                           scm_list_1(scm_from_utf8_string(spec->signal_name)));
        spec->accumulator = g_signal_accumulator_true_handled;
        spec->accu_data = NULL;
    }
    else if (scm_is_procedure(saccu)) {
        if (spec->return_type == G_TYPE_NONE)
            scm_misc_error("%scm->signalspec",
                           "signal ~A must return a value to use an accumulator",
                           scm_list_1(scm_from_utf8_string(spec->signal_name)));
        spec->accumulator = scm_signal_accu;
        spec->accu_data = SCM_UNPACK_POINTER(saccu);
    }
    spec->n_params = n_params;
    spec->param_types = params;

    scm_dynwind_end();
    return spec;
}

void
gig_free_signalspec(GigSignalSpec *spec)
{
    if (spec) {
        if (spec->param_types) {
            free(spec->param_types);
            spec->param_types = NULL;
        }
        free(spec->signal_name);
        spec->signal_name = NULL;
    }
    free(spec);
}

SCM
gig_make_signal(size_t n_slots, GigSignalSlot *slots, SCM *slot_values)
{
    SCM args = scm_make_list(scm_from_size_t(n_slots * 2), SCM_UNDEFINED);
    SCM iter = args;

    for (size_t i = 0; i < n_slots; i++, iter = scm_cddr(iter)) {
        SCM key_iter = iter, val_iter = scm_cdr(iter);
        scm_set_car_x(key_iter, scm_symbol_to_keyword(signal_slot_syms[slots[i]]));
        scm_set_car_x(val_iter, slot_values[i]);
    }

    return scm_apply_0(make_signal_proc, args);
}

SCM
gig_make_signal2(SCM name, SCM mask)
{
    GigSignalSlot slots[] = { GIG_SIGNAL_SLOT_NAME, GIG_SIGNAL_SLOT_OUTPUT_MASK };
    SCM values[2] = { name, mask };
    SCM signal = gig_make_signal(2, slots, values);
    return signal;
}

void
gig_init_signal()
{
    gig_signal_type = scm_c_public_ref("gi types", "<signal>");
    make_signal_proc = scm_c_public_ref("gi types", "make-signal");

    signal_slot_syms[GIG_SIGNAL_SLOT_NAME] = scm_from_utf8_symbol("name");
    signal_slot_syms[GIG_SIGNAL_SLOT_FLAGS] = scm_from_utf8_symbol("flags");
    signal_slot_syms[GIG_SIGNAL_SLOT_ACCUMULATOR] = scm_from_utf8_symbol("accumulator");
    signal_slot_syms[GIG_SIGNAL_SLOT_RETURN_TYPE] = scm_from_utf8_symbol("return-type");
    signal_slot_syms[GIG_SIGNAL_SLOT_PARAM_TYPES] = scm_from_utf8_symbol("param-types");
    signal_slot_syms[GIG_SIGNAL_SLOT_OUTPUT_MASK] = scm_from_utf8_symbol("output-mask");

    signal_accu_first_wins = scm_from_utf8_symbol("first-wins");
    signal_accu_true_handled = scm_from_utf8_symbol("true-handled");
}
