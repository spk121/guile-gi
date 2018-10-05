/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gvalue.h"
#include "gi_gtype.h"
#include "gi_gparamspec.h"

static gboolean
gi_gparamspec_typecheck_scm (const char *func, SCM x)
{
    size_t i, len;
    SCM entry;
    GType prop_type;
    
    if (scm_is_false (scm_list_p (x)))
	scm_misc_error (func, "expected a property list: ~S", scm_list_1 (x));
    len = scm_to_size_t (scm_length (x));
    if (len < 4)
	scm_misc_error (func, "not enough elements in property list: ~S", scm_list_1 (x));
    if (!scm_is_string (scm_list_ref (x, scm_from_size_t (0))))
	scm_misc_error (func, "first element of property list should be a string name: ~S", scm_list_1 (x));
    if (!SCM_IS_A_P (scm_list_ref (x, scm_from_size_t (1)), gi_gtype_type))
	scm_misc_error (func, "second element of property list should be a parent type: ~S", scm_list_1 (x));
    if (!scm_is_a_string (scm_list_ref (x, scm_from_size_t (2))))
	scm_misc_error (func, "third element of property list should be a string blurb: ~S", scm_list_1 (x));
    if (!scm_is_a_string (scm_list_ref (x, scm_from_size_t (3))))
	scm_misc_error (func, "fourth element of property list should be a string description: ~S", scm_list_1 (x));
}

GParamSpec *
gi_gparamspec_from_scm (SCM x)
{
    char *prop_name;
    GType prop_type;
    char *nick;
    char *blurb;
    GParamFlags flags;
    GParamSpec *pspec;
    int i;
    SCM tmp;

#define scm_c_list_ref(x,n) scm_list_ref((x),scm_from_int(n))
    
    prop_name = scm_to_utf8_string (scm_c_list_ref (x, 0));
    prop_type = gi_gtype_get_type (scm_c_list_ref (x, 1));
    nick = scm_to_utf8_string (scm_c_list_ref (x, 2));
    blurb = scm_to_utf8_string (scm_c_list_ref (x, 3));
    i = 4;
    
#define NUMBER_TYPE(ftype,ctype,gtype,scmtype)			    \
    case G_TYPE_ ## ftype:					    \
	{							    \
	    ctype _min, _max, _default;				    \
	    _min = scm_to_ ## scmtype (scm_c_list_ref (x, i++));	    \
	    _max = scm_to_ ## scmtype (scm_c_list_ref (x, i++));	    \
	    _default = scm_to_ ## scmtype (scm_c_list_ref (x, i++));    \
	    flags = scm_to_ulong (scm_c_list_ref (x, i++));		\
	    pspec = g_param_spec_ ## gtype (prop_name, nick, blurb, _min, \
					    _max, _default, flags);	\
	}								\
	break
    
    switch (G_TYPE_FUNDAMENTAL (prop_type)) {
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
	    _default = scm_to_bool (scm_c_list_ref (x, i++));
	    flags = scm_to_ulong (scm_c_list_ref (x, i++));
	    pspec = g_param_spec_boolean (prop_name, nick, blurb,
					  _default, flags);
	}
	break;
    case G_TYPE_ENUM:
	{
	    gint _default;
	    _default = scm_to_uint (scm_c_list_ref (x, i++));
	    flags = scm_to_ulong (scm_c_list_ref (x, i++));
	    pspec = g_param_spec_enum (prop_name, nick, blurb,
				       prop_type, _default, flags);
	}
	break;
    case G_TYPE_FLAGS:
	{
	    guint _default;
	    _default = scm_to_uint (scm_c_list_ref (x, i++));
	    flags = scm_to_ulong (scm_c_list_ref (x, i++));
	    pspec = g_param_spec_flags (prop_name, nick, blurb,
					prop_type, _default, flags);
	}
	break;
    case G_TYPE_STRING:
	{
	    char *_default;
	    _default = scm_to_utf8_string (scm_c_list_ref (x, i++));
	    flags = scm_to_ulong (scm_c_list_ref (x, i++));
	    pspec = g_param_spec_string (prop_name, nick, blurb,
					 _default, flags);
	    free (_default);
	}
	break;
    default:
	return NULL;
    }
    return pspec;
}

static SCM
scm_list_to_gparamspec (SCM x)
{
    SCM obj;
    GParamSpec *spec = gi_gparamspec_from_scm (x);
    
    if (spec) {
	obj = scm_make_foreign_object_0 (gi_gparamspec_type);
	gi_gparamspec_set_spec (obj, spec);
	return obj;
    }
    return SCM_BOOL_F;
}

static SCM
scm_gparam_value_is_valid_p (SCM self, SCM gval)
{
    GParamSpec *spec;
    GValue *val;
    gboolean ret;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    scm_assert_foreign_object_type (gi_gvalue_type, gval);

    spec = gi_gparamspec_get_spec (self);
    val = gi_gvalue_get_value (gval);
    if (spec && val) {
	ret = g_param_value_validate (spec, val);
	return scm_from_bool (ret);
    }
    return SCM_BOOL_F;
}

static SCM
scm_gparamspec_get_default_value (SCM self)
{
    GParamSpec *spec;
    const GValue *val;
    GValue *val2;
    GType type;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    spec = gi_gparamspec_get_spec (self);
    if (spec) {
	val = g_param_spec_get_default_value (spec);
	type = G_VALUE_TYPE (val);
	val2 = g_new0(GValue, 1);
	g_value_init (val2, type);
	g_value_copy (val, val2);
	return gi_gvalue_c2g (val2);
    }
    return SCM_BOOL_F;
}

static SCM
scm_gparamspec_unref (SCM self)
{
    GParamSpec *spec;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    spec = gi_gparamspec_get_spec (self);
    if (spec)
	g_param_spec_unref (spec);
    return SCM_UNSPECIFIED;
}

static SCM
scm_gparamspec_ref (SCM self)
{
    GParamSpec *spec;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    spec = gi_gparamspec_get_spec (self);
    if (spec)
	g_param_spec_ref (spec);
    return SCM_UNSPECIFIED;
}

static SCM
scm_gparamspec_value_type (SCM self)
{
    GParamSpec *spec;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    spec = gi_gparamspec_get_spec (self);
    if (spec)
	return gi_gtype_c2g (G_PARAM_SPEC_VALUE_TYPE (self));
    return SCM_BOOL_F;
}

static SCM
scm_gparamspec_type (SCM self)
{
    GParamSpec *spec;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    spec = gi_gparamspec_get_spec (self);
    if (spec)
	return gi_gtype_c2g (G_PARAM_SPEC_TYPE (self));
    return SCM_BOOL_F;
}

static SCM
scm_gparamspec_type_name (SCM self)
{
    GParamSpec *spec;

    scm_assert_foreign_object_type (gi_gparamspec_type, self);
    spec = gi_gparamspec_get_spec (self);
    if (spec)
	return scm_from_utf8_string (G_PARAM_SPEC_TYPE_NAME (spec));
    return SCM_BOOL_F;
}

void
gi_gparamspec_finalizer (SCM self)
{
    GParamSpec *spec;
    
    spec = gi_gparamspec_get_spec (self);
    if (spec)
	g_param_spec_unref (spec);
    g_free (spec);
    gi_gparamspec_set_spec (self, NULL);
}

void
gi_init_gparamspec (void)
{
    gi_init_gparamspec_type ();

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_ulong(x)))
    D(G_PARAM_READABLE);
    D(G_PARAM_WRITABLE);
    D(G_PARAM_READWRITE);
    D(G_PARAM_CONSTRUCT);
    D(G_PARAM_CONSTRUCT_ONLY);
#undef D

    scm_c_define_gsubr("list->gparamspec", 1, 0, 0, scm_list_to_gparamspec);
    scm_c_define_gsubr("gparam-value-is-valid?", 2, 0, 0, scm_gparam_value_is_valid_p);
    scm_c_define_gsubr("gparamspec-get-default-value", 1, 0, 0, scm_gparamspec_get_default_value);
    scm_c_define_gsubr("gparamspec-ref", 1, 0, 0, scm_gparamspec_ref);
    scm_c_define_gsubr("gparamspec-unref", 1, 0, 0, scm_gparamspec_unref);
    scm_c_define_gsubr("gparamspec-value-type", 1, 0, 0, scm_gparamspec_value_type);
    scm_c_define_gsubr("gparamspec-type", 1, 0, 0, scm_gparamspec_type);
    scm_c_define_gsubr("gparamspec-type-name", 1, 0, 0, scm_gparamspec_type_name);
    scm_c_export ("G_PARAM_READABLE",
		  "G_PARAM_WRITABLE",
		  "G_PARAM_READWRITE",
		  "G_PARAM_CONSTRUCT",
		  "G_PARAM_CONSTRUCT_ONLY",
		  "list->gparamspec",
		  "gparam-value-is-valid?",
		  "gparamspec-get-default-value",
		  "gparamspec-ref",
		  "gparamspec-unref",
		  "gparamspec-value-type",
		  "gparamspec-type",
		  "gparamspec-type-name",
		  NULL);
}
