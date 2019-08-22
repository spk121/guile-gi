#include "gig_signal.h"
#include "gig_object.h"
#include "gig_value.h"
#include "gig_type.h"
#include "gig_argument.h"
#include "gig_flag.h"

static SCM signal_slot_syms[GIG_SIGNAL_SLOT_COUNT];

SCM
gig_signal_ref(SCM signal, GigSignalSlot slot)
{
    return scm_slot_ref(signal, signal_slot_syms[slot]);
}

static SCM signal_accu_first_wins;
static SCM signal_accu_true_handled;

static SCM make_signal_proc;

static gboolean
scm_signal_accu(GSignalInvocationHint * ihint,
                GValue *seed, const GValue *element, gpointer procedure)
{
    SCM _seed, _element, result;
    _seed = gig_value_as_scm(seed, FALSE);
    _element = gig_value_as_scm(element, FALSE);

    result = scm_call_2(SCM_PACK_POINTER(procedure), _seed, _element);
    switch (scm_c_nvalues(result)) {
    case 0:
        return TRUE;
    case 1:
        g_return_val_if_fail(!gig_value_from_scm(seed, result), FALSE);
        return TRUE;
    case 2:
    {
        gboolean ret = scm_is_true(result);
        SCM next_seed = scm_c_value_ref(result, 2);
        g_return_val_if_fail(!gig_value_from_scm(seed, next_seed), FALSE);
        return ret;
    }
    default:
        g_return_val_if_reached(FALSE);
    }
}

GigSignalSpec *
gig_signalspec_from_obj(SCM obj)
{
    gchar *name;
    GType return_type;
    guint n_params;
    GType *params;
    SCM sparams, saccu;
    GSignalFlags flags;
    GigSignalSpec *spec = NULL;

    SCM_ASSERT_TYPE(SCM_IS_A_P(obj, gig_signal_type), obj, SCM_ARG1, "%scm->signalspec", "signal");

    name = scm_to_utf8_string(gig_signal_ref(obj, GIG_SIGNAL_SLOT_NAME));
    return_type = scm_to_gtype(gig_signal_ref(obj, GIG_SIGNAL_SLOT_RETURN_TYPE));
    sparams = gig_signal_ref(obj, GIG_SIGNAL_SLOT_PARAM_TYPES);
    saccu = gig_signal_ref(obj, GIG_SIGNAL_SLOT_ACCUMULATOR);
    n_params = scm_to_uint(scm_length(sparams));
    params = g_new0(GType, n_params);

    for (guint i = 0; i < n_params; i++, sparams = scm_cdr(sparams))
        params[i] = scm_to_gtype(scm_car(sparams));

    flags = gig_flags_to_uint(gig_signal_ref(obj, GIG_SIGNAL_SLOT_FLAGS));

    spec = g_new0(GigSignalSpec, 1);
    spec->signal_name = name;
    spec->signal_flags = flags;
    if (SCM_UNBNDP(saccu) || scm_is_false(saccu)) {
        spec->accumulator = NULL;
        spec->accu_data = NULL;
    }
    else if (scm_is_eq(saccu, signal_accu_first_wins)) {
        spec->accumulator = g_signal_accumulator_first_wins;
        spec->accu_data = NULL;
    }
    else if (scm_is_eq(saccu, signal_accu_true_handled)) {
        spec->accumulator = g_signal_accumulator_true_handled;
        spec->accu_data = NULL;
    }
    else if (scm_is_true(scm_procedure_p(saccu))) {
        spec->accumulator = scm_signal_accu;
        spec->accu_data = SCM_UNPACK_POINTER(saccu);
    }
    spec->return_type = return_type;
    spec->n_params = n_params;
    spec->param_types = params;
    return spec;
}

void
gig_free_signalspec(GigSignalSpec *spec)
{
    if (spec) {
        if (spec->param_types) {
            g_free(spec->param_types);
            spec->param_types = NULL;
        }
        g_free(spec);
    }
}

SCM
gig_make_signal(gsize n_slots, GigSignalSlot *slots, SCM *slot_values)
{
    SCM args = scm_make_list(scm_from_size_t(n_slots * 2), SCM_UNDEFINED);
    SCM iter = args;

    for (gsize i = 0; i < n_slots; i++, iter = scm_cddr(iter)) {
        SCM key_iter = iter, val_iter = scm_cdr(iter);
        scm_set_car_x(key_iter, scm_symbol_to_keyword(signal_slot_syms[slots[i]]));
        scm_set_car_x(val_iter, slot_values[i]);
    }

    return scm_apply_0(make_signal_proc, args);
}

void
gig_init_signal()
{
    gig_signal_type = scm_c_public_ref("gi oop", "<signal>");
    make_signal_proc = scm_c_public_ref("gi oop", "make-signal");

    signal_slot_syms[GIG_SIGNAL_SLOT_NAME] = scm_from_utf8_symbol("name");
    signal_slot_syms[GIG_SIGNAL_SLOT_FLAGS] = scm_from_utf8_symbol("flags");
    signal_slot_syms[GIG_SIGNAL_SLOT_ACCUMULATOR] = scm_from_utf8_symbol("accumulator");
    signal_slot_syms[GIG_SIGNAL_SLOT_RETURN_TYPE] = scm_from_utf8_symbol("return-type");
    signal_slot_syms[GIG_SIGNAL_SLOT_PARAM_TYPES] = scm_from_utf8_symbol("param-types");

    signal_accu_first_wins = scm_from_utf8_symbol("first-wins");
    signal_accu_true_handled = scm_from_utf8_symbol("true-handled");

}
