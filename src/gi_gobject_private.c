/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gvalue.h"
#include "gir_type.h"
#include "gi_gobject_private.h"
#include "gi_util.h"

static SCM guile_property;
static SCM guile_signal;
static SCM guile_number_property;

typedef enum
{
    PROPERTY_SLOT_NAME,
    PROPERTY_SLOT_TYPE,
    PROPERTY_SLOT_NICK,
    PROPERTY_SLOT_BLURB,
    PROPERTY_SLOT_FLAGS,
    PROPERTY_SLOT_DEFAULT,
    PROPERTY_SLOT_MIN,
    PROPERTY_SLOT_MAX,
    PROPERTY_SLOT_COUNT
} PropertySlot;

typedef enum
{
    SIGNAL_SLOT_NAME,
    SIGNAL_SLOT_FLAGS,
    SIGNAL_SLOT_ACCUMULATOR,
    SIGNAL_SLOT_RETURN_TYPE,
    SIGNAL_SLOT_PARAM_TYPES,
    SIGNAL_SLOT_COUNT
} SignalSlot;

static SCM property_slot_syms[PROPERTY_SLOT_COUNT];
static SCM signal_slot_syms[SIGNAL_SLOT_COUNT];

static SCM
property_ref(SCM property, PropertySlot slot)
{
    return scm_slot_ref(property, property_slot_syms[slot]);
}

static SCM
signal_ref(SCM property, PropertySlot slot)
{
    return scm_slot_ref(property, signal_slot_syms[slot]);
}

static SCM signal_accu_first_wins;
static SCM signal_accu_true_handled;

static gpointer
init_gi_oop_once(gpointer data)
{
    guile_property = scm_c_public_ref("gi oop", "<property>");
    guile_number_property = scm_c_public_ref("gi oop", "<number-property>");
    guile_signal = scm_c_public_ref("gi oop", "<signal>");
    return NULL;
}

void
init_gi_oop()
{
    static GOnce _init = G_ONCE_INIT;
    g_once (&_init, init_gi_oop_once, NULL);
}

GParamSpec *
gi_gparamspec_from_scm(SCM x)
{
    init_gi_oop();

    SCM_ASSERT_TYPE(SCM_IS_A_P(x, guile_property),
                    x, SCM_ARG1, "%scm->gparamspec", "property");

    char *prop_name;
    GType prop_type;
    char *nick;
    char *blurb;
    GParamFlags flags;
    GParamSpec *pspec;

    prop_name = scm_to_utf8_string(property_ref(x, PROPERTY_SLOT_NAME));
    prop_type = scm_to_gtype(property_ref(x, PROPERTY_SLOT_TYPE));
    nick = scm_to_utf8_string(property_ref(x, PROPERTY_SLOT_NICK));
    blurb = scm_to_utf8_string(property_ref(x, PROPERTY_SLOT_BLURB));
    flags = scm_to_ulong(property_ref(x, PROPERTY_SLOT_FLAGS));

#define NUMBER_TYPE(ftype,ctype,gtype,scmtype)                          \
    case G_TYPE_ ## ftype:                                              \
    {                                                                   \
        ctype _min, _max, _default;                                     \
        SCM_ASSERT_TYPE(SCM_IS_A_P(x, guile_number_property),           \
                        x, SCM_ARGn, "%scm->gparamspec", "property");   \
        _min = scm_to_ ## scmtype (property_ref (x, PROPERTY_SLOT_MIN)); \
        _max = scm_to_ ## scmtype (property_ref (x, PROPERTY_SLOT_MAX)); \
        _default = scm_to_ ## scmtype (property_ref (x, PROPERTY_SLOT_DEFAULT)); \
        pspec = g_param_spec_ ## gtype (prop_name, nick, blurb, _min,   \
                                        _max, _default, flags);         \
    }                                                                   \
    break

    switch (G_TYPE_FUNDAMENTAL(prop_type)) {
        NUMBER_TYPE(CHAR, gint8, char, int8);
        NUMBER_TYPE(UCHAR, guint8, uchar, uint8);
        NUMBER_TYPE(INT, gint, int, int);
        NUMBER_TYPE(UINT, guint, uint, uint);
        NUMBER_TYPE(LONG, glong, long, long);
        NUMBER_TYPE(ULONG, gulong, ulong, ulong);
        NUMBER_TYPE(INT64, guint64, int64, int64);
        NUMBER_TYPE(UINT64, guint64, uint64, uint64);
        NUMBER_TYPE(FLOAT, float, float, double);
        NUMBER_TYPE(DOUBLE, double, double, double);
    case G_TYPE_BOOLEAN:
    {
        gboolean _default;
        _default = scm_to_bool(property_ref(x, PROPERTY_SLOT_DEFAULT));
        pspec = g_param_spec_boolean(prop_name, nick, blurb, _default, flags);
    }
    break;
    case G_TYPE_ENUM:
    {
        gint _default;
        _default = scm_to_uint(property_ref(x, PROPERTY_SLOT_DEFAULT));
        pspec = g_param_spec_enum(prop_name, nick, blurb, prop_type, _default, flags);
    }
    break;
    case G_TYPE_FLAGS:
    {
        guint _default;
        _default = scm_to_uint(property_ref(x, PROPERTY_SLOT_DEFAULT));
        pspec = g_param_spec_flags(prop_name, nick, blurb, prop_type, _default, flags);
    }
    break;
    case G_TYPE_STRING:
    {
        char *_default;
        _default = scm_to_utf8_string(property_ref(x, PROPERTY_SLOT_DEFAULT));
        pspec = g_param_spec_string(prop_name, nick, blurb, _default, flags);
        free(_default);
    }
    default:
        return NULL;
    }
}

static gboolean
scm_signal_accu(GSignalInvocationHint *ihint,
                GValue *seed,
                const GValue *element,
                gpointer procedure)
{
    SCM _seed, _element, result;
    _seed = gi_gvalue_as_scm(seed, FALSE);
    _element = gi_gvalue_as_scm(element, FALSE);

    result = scm_call_2(SCM_PACK_POINTER(procedure), _seed, _element);
    switch (scm_c_nvalues(result))
    {
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
gi_signalspec_from_obj(SCM obj)
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
gi_free_signalspec(SignalSpec *spec)
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
gi_init_gobject_private(void)
{
    property_slot_syms[PROPERTY_SLOT_NAME] = scm_from_utf8_symbol("name");
    property_slot_syms[PROPERTY_SLOT_TYPE] = scm_from_utf8_symbol("type");
    property_slot_syms[PROPERTY_SLOT_NICK] = scm_from_utf8_symbol("nick");
    property_slot_syms[PROPERTY_SLOT_BLURB] = scm_from_utf8_symbol("blurb");
    property_slot_syms[PROPERTY_SLOT_FLAGS] = scm_from_utf8_symbol("flags");
    property_slot_syms[PROPERTY_SLOT_DEFAULT] = scm_from_utf8_symbol("default");
    property_slot_syms[PROPERTY_SLOT_MIN] = scm_from_utf8_symbol("min");
    property_slot_syms[PROPERTY_SLOT_MAX] = scm_from_utf8_symbol("max");

    signal_slot_syms[SIGNAL_SLOT_NAME] = scm_from_utf8_symbol("name");
    signal_slot_syms[SIGNAL_SLOT_FLAGS] = scm_from_utf8_symbol("flags");
    signal_slot_syms[SIGNAL_SLOT_ACCUMULATOR] = scm_from_utf8_symbol("accumulator");
    signal_slot_syms[SIGNAL_SLOT_RETURN_TYPE] = scm_from_utf8_symbol("return-type");
    signal_slot_syms[SIGNAL_SLOT_PARAM_TYPES] = scm_from_utf8_symbol("param-types");

    signal_accu_first_wins = scm_from_utf8_symbol("first-wins");
    signal_accu_true_handled = scm_from_utf8_symbol("true-handled");

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_ulong(x)))
    D(G_PARAM_READABLE);
    D(G_PARAM_WRITABLE);
    D(G_PARAM_READWRITE);
    D(G_PARAM_CONSTRUCT);
    D(G_PARAM_CONSTRUCT_ONLY);

    D(G_SIGNAL_RUN_FIRST);
    D(G_SIGNAL_RUN_LAST);
    D(G_SIGNAL_RUN_CLEANUP);
    D(G_SIGNAL_NO_RECURSE);
    D(G_SIGNAL_DETAILED);
    D(G_SIGNAL_ACTION);
    D(G_SIGNAL_NO_HOOKS);
    D(G_SIGNAL_MUST_COLLECT);
    D(G_SIGNAL_DEPRECATED);
#undef D
}
