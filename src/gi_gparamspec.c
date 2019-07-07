/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gvalue.h"
#include "gir_type.h"
#include "gi_gparamspec.h"
#include "gi_util.h"

static SCM guile_property;
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

static SCM property_slot_syms[PROPERTY_SLOT_COUNT];

static SCM
property_ref(SCM property, PropertySlot slot)
{
    return scm_slot_ref(property, property_slot_syms[slot]);
}

static gpointer
init_guile_property_type(gpointer data)
{
    guile_property = scm_c_public_ref("gi oop", "<property>");
    guile_number_property = scm_c_public_ref("gi oop", "<number-property>");

    return NULL;
}

GParamSpec *
gi_gparamspec_from_scm(SCM x)
{
    static GOnce _init = G_ONCE_INIT;

    g_once (&_init, init_guile_property_type, NULL);

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

void
gi_init_gparamspec_private(void)
{
    property_slot_syms[PROPERTY_SLOT_NAME] = scm_from_utf8_symbol("name");
    property_slot_syms[PROPERTY_SLOT_TYPE] = scm_from_utf8_symbol("type");
    property_slot_syms[PROPERTY_SLOT_NICK] = scm_from_utf8_symbol("nick");
    property_slot_syms[PROPERTY_SLOT_BLURB] = scm_from_utf8_symbol("blurb");
    property_slot_syms[PROPERTY_SLOT_FLAGS] = scm_from_utf8_symbol("flags");
    property_slot_syms[PROPERTY_SLOT_DEFAULT] = scm_from_utf8_symbol("default");
    property_slot_syms[PROPERTY_SLOT_MIN] = scm_from_utf8_symbol("min");
    property_slot_syms[PROPERTY_SLOT_MAX] = scm_from_utf8_symbol("max");

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_ulong(x)))
    D(G_PARAM_READABLE);
    D(G_PARAM_WRITABLE);
    D(G_PARAM_READWRITE);
    D(G_PARAM_CONSTRUCT);
    D(G_PARAM_CONSTRUCT_ONLY);
#undef D
}
