/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gvalue.h"
#include "gir_type.h"
#include "gig_object_private.h"
#include "gi_util.h"

static SCM signal_slot_syms[SIGNAL_SLOT_COUNT];
static SCM object_slot_syms[OBJECT_SLOT_COUNT];

SCM
signal_ref(SCM signal, SignalSlot slot)
{
    return scm_slot_ref(signal, signal_slot_syms[slot]);
}

SCM
object_ref(SCM object, ObjectSlot slot)
{
    return scm_slot_ref(object, object_slot_syms[slot]);
}

static SCM signal_accu_first_wins;
static SCM signal_accu_true_handled;

static gpointer
init_gi_oop_once(gpointer data)
{
    private_make_gobject_proc = scm_c_private_ref("gi oop", "%make-gobject");
    guile_signal = scm_c_public_ref("gi oop", "<signal>");
    return NULL;
}

void
init_gi_oop()
{
    static GOnce _init = G_ONCE_INIT;
    g_once(&_init, init_gi_oop_once, NULL);
}

static gboolean
scm_signal_accu(GSignalInvocationHint * ihint,
                GValue *seed, const GValue *element, gpointer procedure)
{
    SCM _seed, _element, result;
    _seed = gi_gvalue_as_scm(seed, FALSE);
    _element = gi_gvalue_as_scm(element, FALSE);

    result = scm_call_2(SCM_PACK_POINTER(procedure), _seed, _element);
    switch (scm_c_nvalues(result)) {
    case 0:
        return TRUE;
    case 1:
        g_return_val_if_fail(!gi_gvalue_from_scm(seed, result), FALSE);
        return TRUE;
    case 2:
    {
        gboolean ret = scm_is_true(result);
        SCM next_seed = scm_c_value_ref(result, 2);
        g_return_val_if_fail(!gi_gvalue_from_scm(seed, next_seed), FALSE);
        return ret;
    }
    default:
        g_return_val_if_reached(FALSE);
    }
}

SignalSpec *
gig_signalspec_from_obj(SCM obj)
{
    init_gi_oop();

    char *name;
    GType return_type;
    guint n_params;
    GType *params;
    SCM sparams, saccu;
    GSignalFlags flags;
    SignalSpec *spec = NULL;

    SCM_ASSERT_TYPE(SCM_IS_A_P(obj, guile_signal), obj, SCM_ARG1, "%scm->signalspec", "signal");

    name = scm_to_utf8_string(signal_ref(obj, SIGNAL_SLOT_NAME));
    return_type = scm_to_gtype(signal_ref(obj, SIGNAL_SLOT_RETURN_TYPE));
    sparams = signal_ref(obj, SIGNAL_SLOT_PARAM_TYPES);
    saccu = signal_ref(obj, SIGNAL_SLOT_ACCUMULATOR);
    n_params = scm_to_uint(scm_length(sparams));
    params = g_new0(GType, n_params);

    for (guint i = 0; i < n_params; i++, sparams = scm_cdr(sparams))
        params[i] = scm_to_gtype(scm_car(sparams));

    flags = scm_to_uint(signal_ref(obj, SIGNAL_SLOT_FLAGS));

    spec = g_new0(SignalSpec, 1);
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
gig_free_signalspec(SignalSpec *spec)
{
    if (spec) {
        if (spec->param_types) {
            g_free(spec->param_types);
            spec->param_types = NULL;
        }
        g_free(spec);
    }
}

void
gig_init_object_private(void)
{
    signal_slot_syms[SIGNAL_SLOT_NAME] = scm_from_utf8_symbol("name");
    signal_slot_syms[SIGNAL_SLOT_FLAGS] = scm_from_utf8_symbol("flags");
    signal_slot_syms[SIGNAL_SLOT_ACCUMULATOR] = scm_from_utf8_symbol("accumulator");
    signal_slot_syms[SIGNAL_SLOT_RETURN_TYPE] = scm_from_utf8_symbol("return-type");
    signal_slot_syms[SIGNAL_SLOT_PARAM_TYPES] = scm_from_utf8_symbol("param-types");

    object_slot_syms[OBJECT_SLOT_OBJECT] = scm_from_utf8_symbol("ptr");

    signal_accu_first_wins = scm_from_utf8_symbol("first-wins");
    signal_accu_true_handled = scm_from_utf8_symbol("true-handled");
}

GParamSpec *
gig_paramspec_peek(SCM obj)
{
    g_return_val_if_fail(SCM_IS_A_P(obj, gig_paramspec_type), NULL);
    return gir_type_peek_object(obj);
}
