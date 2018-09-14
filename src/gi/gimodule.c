/* -*- Mode: C; c-basic-offset: 4 -*- */

#include <libguile.h>
#include <glib-object.h>
#include "gimodule.h"
#include "gugobject.h"
#include "pycompat.h"
#include "gugenum.h"
#include "gugflags.h"
#include "gugi-error.h"
#include "gugi-foreign.h"
#include "gugi-info.h"
#include "gugi-repository.h"
#include "gugi-type.h"
#include "gugi-util.h"
#include "gugi-value.h"
#include "stubs.h"
#include "gugobject-object.h"

SCM GuGIWarning;
SCM GuGIDeprecationWarning;
SCM _GuGIDefaultArgPlaceholder;

/* Returns a new flag/enum type or %NULL */
static SCM
flags_enum_from_gtype (GType g_type,
		       SCM (add_func) (SCM, const char *, const char *, GType))
{
    SCM new_type;
    GIRepository *repository;
    GIBaseInfo *info;
    const gchar *type_name;
    
    repository = g_irepository_get_default ();
    info = g_irepository_find_by_gtype (repository, g_type);
    if (info != NULL) {
	type_name = g_base_info_get_name (info);
	new_type = add_func (SCM_UNSPECIFIED, type_name, NULL, g_type);
	g_base_info_unref (info);
    } else {
	type_name = g_type_name (g_type);
	new_type = add_func (SCM_UNSPECIFIED, type_name, NULL, g_type);
    }

    return new_type;
}

static void
gug_flags_add_constants (SCM module, GType flags_type,
			 const gchar *strip_prefix);

/**
 * gug_enum_add_constants:
 * @module: a Guile module
 * @enum_type: the GType of the enumeration.
 * @stip_prefix: the prefix to strip from the constant names
 *
 * Adds constants to the given Guile module for each value name of
 * the enumeration.  A prefix will be stripped from each enum name.
 */
static void
gug_enum_add_constants (SCM module, GType enum_type,
			const gchar *strip_prefix)
{
    GEnumClass *eclass;
    guint i;

    if (!G_TYPE_IS_ENUM(enum_type)) {
	if (G_TYPE_IS_FLAGS(enum_type))
	    gug_flags_add_constants(module, enum_type, strip_prefix);
	else
	    g_warning("`%s' is not an enum type", g_type_name(enum_type));
	return;
    }
    g_return_if_fail (strip_prefix != NULL);

    eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));

    for (i = 0; i < eclass->n_values; i ++) {
	const gchar *name = eclass->values[i].value_name;
	gint value = eclass->values[i].value;

	GuModule_AddIntConstant(module,
				(char *) gug_constant_strip_prefix(name, strip_prefix),
				(long) value);
    }

    g_type_class_unref(eclass);
}

/**
 * gug_flags_add_constants:
 * @module: a Guile module
 * @flags_type: the GType of the flags type.
 * @strip_prefix: the prefix to strip from the constant names.
 *
 * Adds constants to the given Python module for each value name of
 * the flags set.  A prefix will be stripped from each flag name.
 */
static void
gug_flags_add_constants(SCM module, GType flags_type,
			const gchar *strip_prefix)
{
    GFlagsClass *fclass;
    guint i;

    if (!G_TYPE_IS_FLAGS(flags_type)) {
	if (G_TYPE_IS_ENUM(flags_type))
	    gug_enum_add_constants(module, flags_type, strip_prefix);
	else
	    g_warning("`%s' is not an flags type", g_type_name(flags_type));
	return;
    }
    g_return_if_fail (strip_prefix != NULL);

    fclass = G_FLAGS_CLASS(g_type_class_ref(flags_type));

    for (i = 0; i < fclass->n_values; i++) {
	const gchar *name = fclass->values[i].value_name;
	guint value = fclass->values[i].value;

	GuModule_AddIntConstant(module,
				(char*) gug_constant_strip_prefix(name, strip_prefix),
				(long) value);
    }

    g_type_class_unref(fclass);
}

/**
 * pyg_set_thread_block_funcs:
 * Deprecated, only available for ABI compatibility.
 */
#if 0
static void
_gug_set_thread_block_funcs (PyGThreadBlockFunc block_threads_func,
			     PyGThreadBlockFunc unblock_threads_func)
{
    PyGILState_STATE state = PyGILState_Ensure ();
    PyErr_Warn (PyExc_DeprecationWarning,
                "Using pyg_set_thread_block_funcs is not longer needed. "
                "PyGObject always uses Py_BLOCK/UNBLOCK_THREADS.");
    PyGILState_Release (state);
}
#endif

static GParamSpec *
create_property (const gchar  *prop_name,
		 GType         prop_type,
		 const gchar  *nick,
		 const gchar  *blurb,
		 SCM           args,
		 GParamFlags   flags)
{
    GParamSpec *pspec = NULL;

    switch (G_TYPE_FUNDAMENTAL(prop_type)) {
    case G_TYPE_CHAR:
	{
	    gchar minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "ccc", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_char (prop_name, nick, blurb, minimum,
				       maximum, default_value, flags);
	}
	break;
    case G_TYPE_UCHAR:
	{
	    gchar minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "ccc", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_uchar (prop_name, nick, blurb, minimum,
					maximum, default_value, flags);
	}
	break;
    case G_TYPE_BOOLEAN:
	{
	    gboolean default_value;

	    if (!GuArg_ParseTuple(args, "i", &default_value))
		return NULL;
	    pspec = g_param_spec_boolean (prop_name, nick, blurb,
					  default_value, flags);
	}
	break;
    case G_TYPE_INT:
	{
	    gint minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "iii", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_int (prop_name, nick, blurb, minimum,
				      maximum, default_value, flags);
	}
	break;
    case G_TYPE_UINT:
	{
	    guint minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "III", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_uint (prop_name, nick, blurb, minimum,
				       maximum, default_value, flags);
	}
	break;
    case G_TYPE_LONG:
	{
	    glong minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "lll", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_long (prop_name, nick, blurb, minimum,
				       maximum, default_value, flags);
	}
	break;
    case G_TYPE_ULONG:
	{
	    gulong minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "kkk", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_ulong (prop_name, nick, blurb, minimum,
					maximum, default_value, flags);
	}
	break;
    case G_TYPE_INT64:
	{
	    gint64 minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "LLL", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_int64 (prop_name, nick, blurb, minimum,
					maximum, default_value, flags);
	}
	break;
    case G_TYPE_UINT64:
	{
	    guint64 minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "KKK", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_uint64 (prop_name, nick, blurb, minimum,
					 maximum, default_value, flags);
	}
	break;
    case G_TYPE_ENUM:
	{
	    gint default_value;
	    SCM gudefault;

	    if (!GuArg_ParseTuple(args, "O", &gudefault))
		return NULL;

	    if (gug_enum_get_value(prop_type, gudefault,
				   (gint *)&default_value))
		return NULL;

	    pspec = g_param_spec_enum (prop_name, nick, blurb,
				       prop_type, default_value, flags);
	}
	break;
    case G_TYPE_FLAGS:
	{
	    guint default_value;
	    SCM gudefault;

	    if (!GuArg_ParseTuple(args, "O", &gudefault))
		return NULL;

	    if (gug_flags_get_value(prop_type, gudefault,
				    &default_value))
		return NULL;

	    pspec = g_param_spec_flags (prop_name, nick, blurb,
					prop_type, default_value, flags);
	}
	break;
    case G_TYPE_FLOAT:
	{
	    gfloat minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "fff", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_float (prop_name, nick, blurb, minimum,
					maximum, default_value, flags);
	}
	break;
    case G_TYPE_DOUBLE:
	{
	    gdouble minimum, maximum, default_value;

	    if (!GuArg_ParseTuple(args, "ddd", &minimum, &maximum,
				  &default_value))
		return NULL;
	    pspec = g_param_spec_double (prop_name, nick, blurb, minimum,
					 maximum, default_value, flags);
	}
	break;
    case G_TYPE_STRING:
	{
	    const gchar *default_value;

	    if (!GuArg_ParseTuple(args, "z", &default_value))
		return NULL;
	    pspec = g_param_spec_string (prop_name, nick, blurb,
					 default_value, flags);
	}
	break;
    case G_TYPE_PARAM:
	if (!GuArg_ParseTuple(args, ""))
	    return NULL;
	pspec = g_param_spec_param (prop_name, nick, blurb, prop_type, flags);
	break;
    case G_TYPE_BOXED:
	if (!GuArg_ParseTuple(args, ""))
	    return NULL;
	pspec = g_param_spec_boxed (prop_name, nick, blurb, prop_type, flags);
	break;
    case G_TYPE_POINTER:
	if (!GuArg_ParseTuple(args, ""))
	    return NULL;
	if (prop_type == G_TYPE_GTYPE)
	    pspec = g_param_spec_gtype (prop_name, nick, blurb, G_TYPE_NONE, flags);
	else
	    pspec = g_param_spec_pointer (prop_name, nick, blurb, flags);
	break;
    case G_TYPE_OBJECT:
    case G_TYPE_INTERFACE:
	if (!GuArg_ParseTuple(args, ""))
	    return NULL;
	pspec = g_param_spec_object (prop_name, nick, blurb, prop_type, flags);
	break;
    case G_TYPE_VARIANT:
	{
	    SCM gudefault;
            GVariant *default_value = NULL;

	    if (!GuArg_ParseTuple(args, "O", &gudefault))
		return NULL;
            if (gudefault != Gu_None) {
		#define GUGOBJECT_SLOT_DATA 0
		GuGBoxed *data = scm_foreign_object_ref (gudefault, GUGOBJECT_SLOT_DATA);
                default_value = gug_boxed_get (data, GVariant);
	    }
	    pspec = g_param_spec_variant (prop_name, nick, blurb, G_VARIANT_TYPE_ANY, default_value, flags);
	}
	break;
    default:
	/* unhandled pspec type ... */
	break;
    }

    if (!pspec) {
	char buf[128];

	g_snprintf(buf, sizeof(buf), "could not create param spec for type %s",
		   g_type_name(prop_type));
	GuErr_SetString(GuExc_TypeError, buf);
	return NULL;
    }

    return pspec;
}

static GParamSpec *
gug_param_spec_from_object (SCM list)
{
    ssize_t val_length;
    const gchar *prop_name;
    GType prop_type;
    const gchar *nick, *blurb;
    SCM slice, item, gu_prop_type;
    GParamSpec *pspec;
    gint intvalue;

    val_length = scm_to_ssize_t (scm_length (list));
    if (val_length < 4) {
	GuErr_SetString(GuExc_TypeError,
			"paramspec lists must be at least 4 elements long");
	return NULL;
    }

    slice = GuSequence_GetSlice(list, 0, 4);
    if (!slice) {
	return NULL;
    }

    if (!GuArg_ParseTuple(slice, "sOzz", &prop_name, &gu_prop_type, &nick, &blurb)) {
	Gu_DECREF(slice);
	return NULL;
    }

    Gu_DECREF(slice);

    prop_type = gug_type_from_object(gu_prop_type);
    if (!prop_type) {
	return NULL;
    }

    item = scm_list_ref (list, scm_from_ssize_t (val_length - 1));
    if (!scm_is_exact_integer(item)) {
	GuErr_SetString(GuExc_TypeError,
			"last element in list must be an int");
	return NULL;
    }

    intvalue = scm_to_int (item);

    /* slice is the extra items in the list */
    slice = GuSequence_GetSlice(list, 4, val_length-1);
    pspec = create_property(prop_name, prop_type,
			    nick, blurb, slice,
			    intvalue);

    return pspec;
}


/**
 * gug_parse_constructor_args: helper function for GuGObject constructors
 * @obj_type: GType of the GObject, for parameter introspection
 * @arg_names: %NULL-terminated array of constructor argument names
 * @prop_names: %NULL-terminated array of property names, with direct
 * correspondence to @arg_names
 * @params: GParameter array where parameters will be placed; length
 * of this array must be at least equal to the number of
 * arguments/properties
 * @nparams: output parameter to contain actual number of arguments found
 * @py_args: array of SCM containing the actual constructor arguments
 *
 * Parses an array of GuObject's and creates a GParameter array
 *
 * Return value: %TRUE if all is successful, otherwise %FALSE and
 * python exception set.
 **/
static gboolean
pyg_parse_constructor_args(GType        obj_type,
                           char       **arg_names,
                           char       **prop_names,
                           GParameter  *params,
                           guint       *nparams,
                           SCM         gu_args /* Contains a list */
			   )
{
    guint arg_i, param_i;
    GObjectClass *oclass;
    unsigned n_args;

    oclass = g_type_class_ref(obj_type);
    g_return_val_if_fail(oclass, FALSE);

    n_args = scm_to_int (scm_length (gu_args));

    for (param_i = arg_i = 0; arg_names[arg_i]; ++arg_i) {
        GParamSpec *spec;
	SCM entry;
	if (arg_i < n_args)
	    entry = scm_list_ref (gu_args, scm_from_uint (arg_i));
	else
	    entry = Gu_None;
        if (entry == Gu_None)
            continue;
        spec = g_object_class_find_property(oclass, prop_names[arg_i]);
        params[param_i].name = prop_names[arg_i];
        g_value_init(&params[param_i].value, spec->value_type);
        if (gug_value_from_guobject(&params[param_i].value, entry) == -1) {
            guint i;
            GuErr_Format(GuExc_TypeError, "could not convert parameter '%s' of type '%s'",
                         arg_names[arg_i], g_type_name(spec->value_type));
            g_type_class_unref(oclass);
            for (i = 0; i < param_i; ++i)
                g_value_unset(&params[i].value);
            return FALSE;
        }
        ++param_i;
    }
    g_type_class_unref(oclass);
    *nparams = param_i;
    return TRUE;
}

static void
gug_register_class_init(GType gtype, GuGClassInitFunc class_init)
{
    GSList *list;

    list = g_type_get_qdata(gtype, gugobject_class_init_key);
    list = g_slist_prepend(list, class_init);
    g_type_set_qdata(gtype, gugobject_class_init_key, list);
}

static gboolean
add_properties (GObjectClass *klass, SCM properties)
{
    gboolean ret = TRUE;
    ssize_t pos = 0;
    SCM key = SCM_BOOL_F;
    SCM value = SCM_BOOL_F;

    while (GuDict_Next(properties, &pos, key, value)) {
	char *prop_name;
	GType prop_type;
	const gchar *nick, *blurb;
	GParamFlags flags;
	ssize_t val_length;
	SCM slice, item, gu_prop_type;
	GParamSpec *pspec;

	/* values are of format (type,nick,blurb, type_specific_args, flags) */

	if (!scm_is_string(key)) {
	    GuErr_SetString(GuExc_TypeError,
			    "__gproperties__ keys must be strings");
	    ret = FALSE;
	    break;
	}
	prop_name = scm_to_utf8_string (key);

	if (!scm_is_true (scm_list_p (value))) {
	    GuErr_SetString(GuExc_TypeError,
			    "__gproperties__ values must be tuples");
	    ret = FALSE;
	    break;
	}
	val_length = scm_to_ssize_t(scm_length (value));
	if (val_length < 4) {
	    GuErr_SetString(GuExc_TypeError,
			    "__gproperties__ values must be at least 4 elements long");
	    ret = FALSE;
	    break;
	}

	slice = GuSequence_GetSlice(value, 0, 3);
	if (scm_is_false (slice)) {
	    ret = FALSE;
	    break;
	}
	if (!GuArg_ParseTuple(slice, "Ozz", &gu_prop_type, &nick, &blurb)) {
	    Gu_DECREF(slice);
	    ret = FALSE;
	    break;
	}
	Gu_DECREF(slice);
	prop_type = gug_type_from_object(gu_prop_type);
	if (!prop_type) {
	    ret = FALSE;
	    break;
	}
	item = scm_list_ref(value, scm_from_ssize_t (val_length-1));
	if (!scm_is_exact_integer (item)) {
	    GuErr_SetString(GuExc_TypeError,
		"last element in __gproperties__ value tuple must be an int");
	    ret = FALSE;
	    break;
	}
	flags = scm_to_long (item);

	/* slice is the extra items in the tuple */
	slice = GuSequence_GetSlice(value, 3, val_length-1);
	pspec = create_property(prop_name, prop_type, nick, blurb,
				slice, flags);
	Gu_DECREF(slice);

	if (pspec) {
	    g_object_class_install_property(klass, 1, pspec);
	} else {
	    SCM type, pvalue, traceback;
	    ret = FALSE;
            GuErr_Fetch(&type, &pvalue, &traceback);
            if (scm_is_string(pvalue)) {
                char msg[256];
		char *pvalstr;
		pvalstr = scm_to_utf8_string (pvalue);
                g_snprintf(msg, 256,
			   "%s (while registering property '%s' for GType '%s')",
			   pvalstr,
			   prop_name, G_OBJECT_CLASS_NAME(klass));
		free (pvalstr);
                Gu_DECREF(pvalue);
                value = scm_from_utf8_string(msg);
            }
            GuErr_Restore(type, pvalue, traceback);
	    break;
	}
    }

    return ret;
}

static gboolean
override_signal(GType instance_type, const gchar *signal_name)
{
    guint signal_id;

    signal_id = g_signal_lookup(signal_name, instance_type);
    if (!signal_id) {
	gchar buf[128];

	g_snprintf(buf, sizeof(buf), "could not look up %s", signal_name);
	GuErr_SetString(GuExc_TypeError, buf);
	return FALSE;
    }
    g_signal_override_class_closure(signal_id, instance_type,
				    gug_signal_class_closure_get());
    return TRUE;
}

typedef struct _GuGSignalAccumulatorData {
    SCM callable;
    SCM user_data;
} GuGSignalAccumulatorData;


static gboolean
_gug_signal_accumulator(GSignalInvocationHint *ihint,
                        GValue *return_accu,
                        const GValue *handler_return,
                        gpointer _data)
{
    SCM gu_ihint, gu_return_accu, gu_handler_return, gu_detail;
    SCM gu_retval;
    gboolean retval = FALSE;
    GuGSignalAccumulatorData *data = _data;
    GuGILState_STATE state;

    state = GuGILState_Ensure();
    if (ihint->detail)
        gu_detail = scm_from_utf8_string(g_quark_to_string(ihint->detail));
    else {
        Gu_INCREF(Gu_None);
        gu_detail = Gu_None;
    }

    gu_ihint = Gu_BuildValue("lNi", (long int) ihint->signal_id,
                             gu_detail, ihint->run_type);
    gu_handler_return = gug_value_as_scm(handler_return, TRUE);
    gu_return_accu = gug_value_as_scm(return_accu, FALSE);
    if (data->user_data)
        gu_retval = GuObject_CallFunction(data->callable, "NNNO", gu_ihint,
                                          gu_return_accu, gu_handler_return,
                                          data->user_data);
    else
        gu_retval = GuObject_CallFunction(data->callable, "NNN", gu_ihint,
                                          gu_return_accu, gu_handler_return);
    if (!gu_retval)
	GuErr_Print();
    else {
        if (!GuTuple_Check(gu_retval) || GuTuple_Size(gu_retval) != 2) {
            GuErr_SetString(GuExc_TypeError, "accumulator function must return"
                            " a (bool, object) tuple");
            GuErr_Print();
        } else {
            retval = scm_is_true(scm_list_ref(gu_retval, scm_from_int (0)));
            if (gug_value_from_scm(return_accu, scm_list_ref(gu_retval, scm_from_int (1)))) {
                GuErr_Print();
            }
        }
        Gu_DECREF(gu_retval);
    }
    GuGILState_Release(state);
    return retval;
}

static gboolean
create_signal (GType instance_type, const gchar *signal_name, SCM list)
{
    GSignalFlags signal_flags;
    SCM gu_return_type, gu_param_types;
    GType return_type;
    guint n_params, i;
    GType *param_types;
    guint signal_id;
    GSignalAccumulator accumulator = NULL;
    GuGSignalAccumulatorData *accum_data = NULL;
    SCM gu_accum = SCM_BOOL_F;
    SCM gu_accum_data = SCM_BOOL_F;

    if (!GuArg_ParseList(list, "iOO|OO", &signal_flags, &gu_return_type,
			 &gu_param_types, &gu_accum, &gu_accum_data))
    {
	gchar buf[128];

	GuErr_Clear();
	g_snprintf(buf, sizeof(buf),
		   "value for __gsignals__['%s'] not in correct format", signal_name);
	GuErr_SetString(GuExc_TypeError, buf);
	return FALSE;
    }

    if (gu_accum && gu_accum != Gu_None && !GuCallable_Check(gu_accum))
    {
	gchar buf[128];

	g_snprintf(buf, sizeof(buf),
		   "accumulator for __gsignals__['%s'] must be callable", signal_name);
	GuErr_SetString(GuExc_TypeError, buf);
	return FALSE;
    }

    return_type = gug_type_from_object(gu_return_type);
    if (!return_type)
	return FALSE;
    if (!scm_is_true(scm_list_p(gu_param_types))) {
	gchar buf[128];

	g_snprintf(buf, sizeof(buf),
		   "third element of __gsignals__['%s'] list must be a list", signal_name);
	GuErr_SetString(GuExc_TypeError, buf);
	return FALSE;
    }
    n_params = scm_to_uint (scm_length (gu_param_types));

    param_types = g_new(GType, n_params);
    for (i = 0; i < n_params; i++) {
	SCM item = scm_list_ref (gu_param_types, scm_from_uint (i));

	param_types[i] = gug_type_from_object(item);
	if (param_types[i] == 0) {
	    Gu_DECREF(item);
	    g_free(param_types);
	    return FALSE;
	}
	Gu_DECREF(item);
    }

    if (gu_accum != NULL && gu_accum != Gu_None) {
        accum_data = g_new(GuGSignalAccumulatorData, 1);
        accum_data->callable = gu_accum;
        Gu_INCREF(gu_accum);
        accum_data->user_data = gu_accum_data;
        Gu_XINCREF(gu_accum_data);
        accumulator = _gug_signal_accumulator;
    }

    signal_id = g_signal_newv(signal_name, instance_type, signal_flags,
			      gug_signal_class_closure_get(),
			      accumulator, accum_data,
			      gi_cclosure_marshal_generic,
			      return_type, n_params, param_types);
    g_free(param_types);

    if (signal_id == 0) {
	gchar buf[128];

	g_snprintf(buf, sizeof(buf), "could not create signal for %s",
		   signal_name);
	GuErr_SetString(GuExc_RuntimeError, buf);
	return FALSE;
    }
    return TRUE;
}

static SCM
add_signals (GObjectClass *klass, SCM signals)
{
    gboolean ret = TRUE;
    Gu_ssize_t pos = 0;
    SCM key, value, overridden_signals;
    GType instance_type = G_OBJECT_CLASS_TYPE (klass);

    overridden_signals = scm_c_make_hash_table (10);
    while (GuDict_Next(signals, &pos, key, value)) {
	gchar *signal_name;
        gchar *signal_name_canon, *c;
	gboolean override = FALSE;

	if (!scm_is_string (key)) {
	    GuErr_SetString(GuExc_TypeError,
			    "__gsignals__ keys must be strings");
	    ret = FALSE;
	    break;
	}
	signal_name = scm_to_utf8_string (key);

	if (value == Gu_None)
	    override = TRUE;
	if (scm_is_string (value)) {
	    gchar *value_str;
	    value_str = scm_to_utf8_string (value);
	    if (!strcmp (value_str, "override"))
		override = TRUE;
	    free (value_str);
	}
	if (override)
        {
	    /* canonicalize signal name, replacing '-' with '_' */
            signal_name_canon = g_strdup(signal_name);
            for (c = signal_name_canon; *c; ++c)
                if (*c == '-')
                    *c = '_';
            if (GuDict_SetItemString(overridden_signals,
				     signal_name_canon, key)) {
                g_free(signal_name_canon);
                ret = FALSE;
                break;
            }
            g_free(signal_name_canon);

	    ret = override_signal(instance_type, signal_name);
	} else {
	    ret = create_signal(instance_type, signal_name, value);
	}

	if (!ret)
	    break;
    }
    if (ret)
        return overridden_signals;
    else {
        Gu_XDECREF(overridden_signals);
        return NULL;
    }
}

static void
gug_object_get_property (GObject *object, guint property_id,
			 GValue *value, GParamSpec *pspec)
{
    SCM object_wrapper, retval;
    GuGILState_STATE state;

    state = GuGILState_Ensure();

    object_wrapper = g_object_get_qdata(object, gugobject_wrapper_key);

    if (object_wrapper)
      Gu_INCREF (object_wrapper);
    else
      object_wrapper = gugobject_new(object);

    if (object_wrapper == NULL) {
	GuGILState_Release(state);
	return;
    }

    retval = gugi_call_do_get_property (object_wrapper, pspec);
    if (retval && gug_value_from_scm (value, retval) < 0) {
        GuErr_Print();
    }
    Gu_DECREF(object_wrapper);
    Gu_XDECREF(retval);

    GuGILState_Release(state);
}

static void
gug_object_set_property (GObject *object, guint property_id,
			 const GValue *value, GParamSpec *pspec)
{
    SCM object_wrapper, retval;
    SCM gu_pspec, gu_value;
    GuGILState_STATE state;

    state = GuGILState_Ensure();

    object_wrapper = g_object_get_qdata(object, gugobject_wrapper_key);

    if (object_wrapper)
      Gu_INCREF (object_wrapper);
    else
      object_wrapper = gugobject_new(object);

    if (object_wrapper == NULL) {
	GuGILState_Release(state);
	return;
    }

    gu_pspec = gug_param_spec_new(pspec);
    gu_value = gug_value_as_guobject (value, TRUE);

    retval = GuObject_CallMethod(object_wrapper, "do_set_property",
				 "OO", gu_pspec, gu_value);
    if (retval) {
	Gu_DECREF(retval);
    } else {
	GuErr_Print();
    }

    Gu_DECREF(object_wrapper);
    Gu_DECREF(gu_pspec);
    Gu_DECREF(gu_value);

    GuGILState_Release(state);
}

static void
gug_object_class_init(GObjectClass *class, SCM gu_class)
{
    SCM gproperties, gsignals, overridden_signals;
    SCM class_dict;

    #define GOBJECT_TYPE_DICT_SLOT 0
    class_dict = scm_foreign_object_ref (gu_class, GOBJECT_TYPE_DICT_SLOT);

    class->set_property = gug_object_set_property;
    class->get_property = gug_object_get_property;

    /* install signals */
    /* we look this up in the instance dictionary, so we don't
     * accidentally get a parent type's __gsignals__ attribute. */
    gsignals = GuDict_GetItemString(class_dict, "__gsignals__");
    if (gsignals) {
	if (!GuDict_Check(gsignals)) {
	    GuErr_SetString(GuExc_TypeError,
			    "__gsignals__ attribute not a dict!");
	    return;
	}
	if (!(overridden_signals = add_signals(class, gsignals))) {
	    return;
	}
        if (GuDict_SetItemString(class_dict, "__gsignals__",
				 overridden_signals)) {
            return;
        }
        Gu_DECREF(overridden_signals);

        GuDict_DelItemString(class_dict, "__gsignals__");
    } else {
	GuErr_Clear();
    }

    /* install properties */
    /* we look this up in the instance dictionary, so we don't
     * accidentally get a parent type's __gproperties__ attribute. */
    gproperties = GuDict_GetItemString(class_dict, "__gproperties__");
    if (gproperties) {
	if (!GuDict_Check(gproperties)) {
	    GuErr_SetString(GuExc_TypeError,
			    "__gproperties__ attribute not a dict!");
	    return;
	}
	if (!add_properties(class, gproperties)) {
	    return;
	}
	GuDict_DelItemString(class_dict, "__gproperties__");
	/* Borrowed reference. Gu_DECREF(gproperties); */
    } else {
	GuErr_Clear();
    }
}

static GPrivate gugobject_construction_wrapper;

static inline void
gugobject_init_wrapper_set(SCM wrapper)
{
    g_private_set(&gugobject_construction_wrapper, wrapper);
}

static inline SCM 
gugobject_init_wrapper_get(void)
{
    return (SCM ) g_private_get(&gugobject_construction_wrapper);
}

int
gugobject_constructv(GuGObject  *self,
                     guint       n_parameters,
                     GParameter *parameters)
{
    GObject *obj;

    g_assert (self->obj == NULL);
    gugobject_init_wrapper_set((SCM ) self);
G_GNUC_BEGIN_IGNORE_DEPRECATIONS
    obj = g_object_newv(gug_type_from_object((SCM ) self),
                        n_parameters, parameters);
G_GNUC_END_IGNORE_DEPRECATIONS
    if (g_object_is_floating (obj))
        self->flags |= GUGOBJECT_GOBJECT_WAS_FLOATING;
    gugobject_sink (obj);

    gugobject_init_wrapper_set(NULL);
    self->obj = obj;
    gugobject_register_wrapper((SCM ) self);

    return 0;
}

static void
gugobject__g_instance_init(GTypeInstance   *instance,
                           gpointer         g_class)
{
    GObject *object = (GObject *) instance;
    SCM wrapper, result;
    GuGILState_STATE state;

    wrapper = g_object_get_qdata(object, gugobject_wrapper_key);
    if (wrapper == NULL) {
        wrapper = gugobject_init_wrapper_get();
        if (wrapper && ((GuGObject *) wrapper)->obj == NULL) {
            ((GuGObject *) wrapper)->obj = object;
            gugobject_register_wrapper(wrapper);
        }
    }
    gugobject_init_wrapper_set(NULL);

    state = GuGILState_Ensure();

    if (wrapper == NULL) {
          /* this looks like a guthon object created through
           * g_object_new -> we have no guthon wrapper, so create it
           * now */
        wrapper = gugobject_new_full(object,
                                     /*steal=*/ FALSE,
                                     g_class);

        /* float the wrapper ref here because we are going to orphan it
         * so we don't destroy the wrapper. The next call to gugobject_new_full
         * will take the ref */
        gugobject_ref_float ((GuGObject *) wrapper);

        result = GuObject_CallMethod (wrapper, "__init__", NULL);
        if (result == NULL)
            GuErr_Print ();
        else
            Gu_DECREF (result);
    }

    /* XXX: used for Gtk.Template */
    if (GuObject_HasAttrString (wrapper, "__dontuse_ginstance_init__")) {
        result = GuObject_CallMethod (wrapper, "__dontuse_ginstance_init__", NULL);
        if (result == NULL)
            GuErr_Print ();
        else
            Gu_DECREF (result);
    }

    GuGILState_Release(state);
}

/*  This implementation is bad, see bug 566571 for an example why.
 *  Instead of scanning explicitly declared bases for interfaces, we
 *  should automatically initialize all implemented interfaces to
 *  prevent bugs like that one.  However, this will lead to
 *  performance degradation as each virtual method in derived classes
 *  will round-trip through do_*() stuff, *even* if it is not
 *  overriden.  We need to teach codegen to retain parent method
 *  instead of setting virtual to *_proxy_do_*() if corresponding
 *  do_*() is not overriden.  Ok, that was a messy explanation.
 */
static void
gug_type_add_interfaces(SCM class, /* Contains a foreign object type */
			GType instance_type,
                        SCM bases,
                        GType *parent_interfaces, guint n_parent_interfaces)
{
    int i;

    if (!bases) {
        g_warning("type has no bases");
        return;
    }

    for (i = 0; i < GuTuple_GET_SIZE(bases); ++i) {
        SCM base = GuTuple_GET_ITEM(bases, i);
        GType itype;
        const GInterfaceInfo *iinfo;
        GInterfaceInfo iinfo_copy;

        /* 'base' can also be a GuClassObject, see bug #566571. */
        if (!GuType_Check(base))
            continue;

        if (!GuType_IsSubtype(base, /* A type object */
			      GuGInterface_Type))
            continue;

        itype = gug_type_from_object(base);

        /* Happens for _implementations_ of an interface. */
        if (!G_TYPE_IS_INTERFACE(itype))
            continue;

        iinfo = gug_lookup_interface_info(itype);
        if (!iinfo) {
            gchar *error;
#if 0	    
            error = g_strdup_printf("Interface type %s "
                                    "has no Guthon implementation support",
                                    ((GuTypeObject *) base)->tp_name);

#else
            error = g_strdup_printf("Interface type "
                                    "has no Guile implementation support");
#endif
            GuErr_Warn(GuExc_RuntimeWarning, error);
            g_free(error);
            continue;
        }

        iinfo_copy = *iinfo;
        iinfo_copy.interface_data = class;
        g_type_add_interface_static(instance_type, itype, &iinfo_copy);
    }
}

static int
gug_run_class_init(GType gtype, gpointer gclass,
		   SCM guclass /* a type */
		   )
{
    GSList *list;
    GuGClassInitFunc class_init;
    GType parent_type;
    int rv;

    parent_type = g_type_parent(gtype);
    if (parent_type) {
        rv = gug_run_class_init(parent_type, gclass, guclass);
        if (rv)
	    return rv;
    }

    list = g_type_get_qdata(gtype, gugobject_class_init_key);
    for (; list; list = list->next) {
	class_init = list->data;
        rv = class_init(gclass, guclass);
        if (rv)
	    return rv;
    }

    return 0;
}

static char *
get_type_name_for_class(SCM class /* contains a Guile foreign object type */
			)
{
    gint i, name_serial;
    char name_serial_str[16];
    SCM module;
    char *type_name = NULL;

    /* make name for new GType */
    name_serial = 1;
    /* give up after 1000 tries, just in case.. */
    while (name_serial < 1000)
    {
	g_free(type_name);
	g_snprintf(name_serial_str, 16, "-v%i", name_serial);
	module = GuObject_GetAttrString((SCM )class, "__module__");
	if (module && scm_is_string(module)) {
	    type_name = g_strconcat(GUGLIB_GuUnicode_AsString(module), ".",
				    gug_foreign_object_type_get_name(class),
				    name_serial > 1 ? name_serial_str : NULL,
				    NULL);
	    Gu_DECREF(module);
	} else {
	    if (module)
		Gu_DECREF(module);
	    else
		GuErr_Clear();
#if 0	    
	    type_name = g_strconcat(class->tp_name,
				    name_serial > 1 ? name_serial_str : NULL,
				    NULL);
#else
	    type_name = g_strconcat(gug_foreign_object_type_get_name (class),
				    name_serial > 1 ? name_serial_str : NULL,
				    NULL);
#endif
	}
	/* convert '.' in type name to '+', which isn't banned (grumble) */
	for (i = 0; type_name[i] != '\0'; i++)
	    if (type_name[i] == '.')
		type_name[i] = '+';
	if (g_type_from_name(type_name) == 0)
	    break;              /* we now have a unique name */
	++name_serial;
    }

    return type_name;
}

static int
gug_type_register(SCM class, /* contains a Guile type */
		  const char *type_name)
{
    SCM gtype;
    GType parent_type, instance_type;
    GType *parent_interfaces;
    guint n_parent_interfaces;
    GTypeQuery query;
    gpointer gclass;
    GTypeInfo type_info = {
	0,    /* class_size */

	(GBaseInitFunc) NULL,
	(GBaseFinalizeFunc) NULL,

	(GClassInitFunc) gug_object_class_init,
	(GClassFinalizeFunc) NULL,
	NULL, /* class_data */

	0,    /* instance_size */
	0,    /* n_preallocs */
	(GInstanceInitFunc) gugobject__g_instance_init
    };
    gchar *new_type_name;

    /* find the GType of the parent */
    parent_type = gug_type_from_object((SCM )class);
    if (!parent_type)
	return -1;

    parent_interfaces = g_type_interfaces(parent_type, &n_parent_interfaces);

    if (type_name)
	/* care is taken below not to free this */
        new_type_name = (gchar *) type_name;
    else
	new_type_name = get_type_name_for_class(class);

    /* set class_data that will be passed to the class_init function. */
    type_info.class_data = class;

    /* fill in missing values of GTypeInfo struct */
    g_type_query(parent_type, &query);
    type_info.class_size = (guint16)query.class_size;
    type_info.instance_size = (guint16)query.instance_size;

    /* create new typecode */
    instance_type = g_type_register_static(parent_type, new_type_name,
					   &type_info, 0);
    if (instance_type == 0) {
	GuErr_Format(GuExc_RuntimeError,
		     "could not create new GType: %s (subclass of %s)",
		     new_type_name,
		     g_type_name(parent_type));

        if (type_name == NULL)
            g_free(new_type_name);

	return -1;
    }

    if (type_name == NULL)
        g_free(new_type_name);

    /* store pointer to the class with the GType */
    Gu_INCREF(class);
    g_type_set_qdata(instance_type, g_quark_from_string("GuGObject::class"),
		     class);

    /* Mark this GType as a custom guthon type */
    g_type_set_qdata(instance_type, gugobject_custom_key,
                     GINT_TO_POINTER (1));

    /* set new value of __gtype__ on class */
    gtype = gug_type_wrapper_new(instance_type);
    GuObject_SetAttrString((SCM )class, "__gtype__", gtype);
    Gu_DECREF(gtype);

    /* if no __doc__, set it to the auto doc descriptor */
    if (GuDict_GetItemString(gug_foreign_object_type_get_dict(class), "__doc__") == NULL) {
	GuDict_SetItemString(gug_foreign_object_type_get_dict(class), "__doc__",
			     gug_object_descr_doc_get());
    }

    /*
     * Note, all interfaces need to be registered before the first
     * g_type_class_ref(), see bug #686149.
     *
     * See also comment above gug_type_add_interfaces().
     */
    gug_type_add_interfaces(class, instance_type, gug_foreign_object_type_get_bases(class),
                            parent_interfaces, n_parent_interfaces);


    gclass = g_type_class_ref(instance_type);
    if (GuErr_Occurred() != NULL) {
        g_type_class_unref(gclass);
        g_free(parent_interfaces);
        return -1;
    }

    if (gug_run_class_init(instance_type, gclass, class)) {
        g_type_class_unref(gclass);
        g_free(parent_interfaces);
        return -1;
    }
    g_type_class_unref(gclass);
    g_free(parent_interfaces);

    if (GuErr_Occurred() != NULL)
        return -1;
    return 0;
}

static SCM 
_wrap_gug_type_register(SCM self, SCM args)
{
    SCM class; /* a type */
    char *type_name = NULL;

    if (!GuArg_ParseTuple(args, "O!|z:gobject.type_register",
			  &GuType_Type, &class, &type_name))
	return NULL;
    if (!GuType_IsSubtype(class, GuGObject_Type)) {
	GuErr_SetString(GuExc_TypeError,
			"argument must be a GObject subclass");
	return NULL;
    }

      /* Check if type already registered */
    if (gug_type_from_object((SCM) class) ==
        gug_type_from_object(gug_foreign_object_type_get_base(class)))
    {
        if (gug_type_register(class, type_name))
            return NULL;
    }

    Gu_INCREF(class);
    return (SCM ) class;
}

static GHashTable *log_handlers = NULL;
static gboolean log_handlers_disabled = FALSE;

static void
remove_handler(gpointer domain,
               gpointer handler,
	       gpointer unused)
{
    g_log_remove_handler(domain, GPOINTER_TO_UINT(handler));
}

static void
_log_func(const gchar *log_domain,
          GLogLevelFlags log_level,
          const gchar *message,
          gpointer user_data)
{
    if (G_LIKELY(Gu_IsInitialized()))
    {
	GuGILState_STATE state;
	SCM  warning = user_data;

	state = GuGILState_Ensure();
	GuErr_Warn(warning, (char *) message);
	GuGILState_Release(state);
    } else
        g_log_default_handler(log_domain, log_level, message, user_data);
}

static void
add_warning_redirection(const char *domain,
                        SCM warning)
{
    g_return_if_fail(domain != NULL);
    g_return_if_fail(warning != NULL);

    if (!log_handlers_disabled)
    {
	guint handler;
	gpointer old_handler;

	if (!log_handlers)
	    log_handlers = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);

	if ((old_handler = g_hash_table_lookup(log_handlers, domain)))
	    g_log_remove_handler(domain, GPOINTER_TO_UINT(old_handler));

	handler = g_log_set_handler(domain, G_LOG_LEVEL_CRITICAL|G_LOG_LEVEL_WARNING,
	                            _log_func, warning);
	g_hash_table_insert(log_handlers, g_strdup(domain), GUINT_TO_POINTER(handler));
    }
}

static void
disable_warning_redirections(void)
{
    log_handlers_disabled = TRUE;

    if (log_handlers)
    {
	g_hash_table_foreach(log_handlers, remove_handler, NULL);
	g_hash_table_destroy(log_handlers);
	log_handlers = NULL;
    }
}

/**
 * Returns 0 on success, or -1 and sets an exception.
 */
static int
gugi_register_warnings(SCM d)
{
    SCM warning;

    warning = GuErr_NewException("gobject.Warning", GuExc_Warning, NULL);
    if (warning == NULL)
        return -1;
    GuDict_SetItemString(d, "Warning", warning);
    add_warning_redirection("GLib", warning);
    add_warning_redirection("GLib-GObject", warning);
    add_warning_redirection("GThread", warning);

    return 0;
}

static SCM 
_wrap_gug_enum_add (SCM self,
                    SCM args,
                    SCM kwargs)
{
    static char *kwlist[] = { "g_type", NULL };
    SCM gu_g_type;
    GType g_type;

    if (!GuArg_ParseTupleAndKeywords (args, kwargs,
                                      "O!:enum_add",
                                      kwlist, &GuGTypeWrapper_Type, &gu_g_type)) {
        return NULL;
    }

    g_type = gug_type_from_object (gu_g_type);
    if (g_type == G_TYPE_INVALID) {
        return NULL;
    }

    return flags_enum_from_gtype (g_type, gug_enum_add);
}

static SCM 
_wrap_gug_enum_register_new_gtype_and_add (SCM self,
                                           SCM args,
                                           SCM kwargs)
{
    static char *kwlist[] = { "info", NULL };
    GuGIBaseInfo *gu_info;
    GIEnumInfo *info;
    gint n_values;
    GEnumValue *g_enum_values;
    int i;
    const gchar *namespace;
    const gchar *type_name;
    gchar *full_name;
    GType g_type;

    if (!GuArg_ParseTupleAndKeywords (args, kwargs,
                                      "O:enum_add_make_new_gtype",
                                      kwlist, (SCM )&gu_info)) {
        return NULL;
    }

    if (!GI_IS_ENUM_INFO (gu_info->info) ||
            g_base_info_get_type ((GIBaseInfo *) gu_info->info) != GI_INFO_TYPE_ENUM) {
        GuErr_SetString (GuExc_TypeError, "info must be an EnumInfo with info type GI_INFO_TYPE_ENUM");
        return NULL;
    }

    info = (GIEnumInfo *)gu_info->info;
    n_values = g_enum_info_get_n_values (info);

    /* The new memory is zero filled which fulfills the registration
     * function requirement that the last item is zeroed out as a terminator.
     */
    g_enum_values = g_new0 (GEnumValue, n_values + 1);

    for (i = 0; i < n_values; i++) {
        GIValueInfo *value_info;
        GEnumValue *enum_value;
        const gchar *name;
        const gchar *c_identifier;

        value_info = g_enum_info_get_value (info, i);
        name = g_base_info_get_name ((GIBaseInfo *) value_info);
        c_identifier = g_base_info_get_attribute ((GIBaseInfo *) value_info,
                                                  "c:identifier");

        enum_value = &g_enum_values[i];
        enum_value->value_nick = g_strdup (name);
        enum_value->value = (gint)g_value_info_get_value (value_info);

        if (c_identifier == NULL) {
            enum_value->value_name = enum_value->value_nick;
        } else {
            enum_value->value_name = g_strdup (c_identifier);
        }

        g_base_info_unref ((GIBaseInfo *) value_info);
    }

    /* Obfuscate the full_name by prefixing it with "Gu" to avoid conflicts
     * with real GTypes. See: https://bugzilla.gnome.org/show_bug.cgi?id=692515
     */
    namespace = g_base_info_get_namespace ((GIBaseInfo *) info);
    type_name = g_base_info_get_name ((GIBaseInfo *) info);
    full_name = g_strconcat ("Gu", namespace, type_name, NULL);

    /* If enum registration fails, free all the memory allocated
     * for the values array. This needs to leak when successful
     * as GObject keeps a reference to the data as specified in the docs.
     */
    g_type = g_enum_register_static (full_name, g_enum_values);
    if (g_type == G_TYPE_INVALID) {
        for (i = 0; i < n_values; i++) {
            GEnumValue *enum_value = &g_enum_values[i];

            /* Only free value_name if it is different from value_nick to avoid
             * a double free. The pointer might have been is re-used in the case
             * c_identifier was NULL in the above loop.
             */
            if (enum_value->value_name != enum_value->value_nick)
                g_free ((gchar *) enum_value->value_name);
            g_free ((gchar *) enum_value->value_nick);
        }

        GuErr_Format (GuExc_RuntimeError, "Unable to register enum '%s'", full_name);

        g_free (g_enum_values);
        g_free (full_name);
        return NULL;
    }

    g_free (full_name);
    return gug_enum_add (NULL, type_name, NULL, g_type);
}

static SCM 
_wrap_gug_flags_add (SCM self,
                     SCM args,
                     SCM kwargs)
{
    static char *kwlist[] = { "g_type", NULL };
    SCM gu_g_type;
    GType g_type;

    if (!GuArg_ParseTupleAndKeywords (args, kwargs,
                                      "O!:flags_add",
                                      kwlist, &GuGTypeWrapper_Type, &gu_g_type)) {
        return NULL;
    }

    g_type = gug_type_from_object (gu_g_type);
    if (g_type == G_TYPE_INVALID) {
        return NULL;
    }

    return flags_enum_from_gtype (g_type, gug_flags_add);
}

static SCM 
_wrap_gug_flags_register_new_gtype_and_add (SCM self,
                                            SCM args,
                                            SCM kwargs)
{
    static char *kwlist[] = { "info", NULL };
    GuGIBaseInfo *gu_info;
    GIEnumInfo *info;
    gint n_values;
    GFlagsValue *g_flags_values;
    int i;
    const gchar *namespace;
    const gchar *type_name;
    gchar *full_name;
    GType g_type;

    if (!GuArg_ParseTupleAndKeywords (args, kwargs,
                                      "O:flags_add_make_new_gtype",
                                      kwlist, (SCM )&gu_info)) {
        return NULL;
    }

    if (!GI_IS_ENUM_INFO (gu_info->info) ||
            g_base_info_get_type ((GIBaseInfo *) gu_info->info) != GI_INFO_TYPE_FLAGS) {
        GuErr_SetString (GuExc_TypeError, "info must be an EnumInfo with info type GI_INFO_TYPE_FLAGS");
        return NULL;
    }

    info = (GIEnumInfo *)gu_info->info;
    n_values = g_enum_info_get_n_values (info);

    /* The new memory is zero filled which fulfills the registration
     * function requirement that the last item is zeroed out as a terminator.
     */
    g_flags_values = g_new0 (GFlagsValue, n_values + 1);

    for (i = 0; i < n_values; i++) {
        GIValueInfo *value_info;
        GFlagsValue *flags_value;
        const gchar *name;
        const gchar *c_identifier;

        value_info = g_enum_info_get_value (info, i);
        name = g_base_info_get_name ((GIBaseInfo *) value_info);
        c_identifier = g_base_info_get_attribute ((GIBaseInfo *) value_info,
                                                  "c:identifier");

        flags_value = &g_flags_values[i];
        flags_value->value_nick = g_strdup (name);
        flags_value->value = (guint)g_value_info_get_value (value_info);

        if (c_identifier == NULL) {
            flags_value->value_name = flags_value->value_nick;
        } else {
            flags_value->value_name = g_strdup (c_identifier);
        }

        g_base_info_unref ((GIBaseInfo *) value_info);
    }

    /* Obfuscate the full_name by prefixing it with "Gu" to avoid conflicts
     * with real GTypes. See: https://bugzilla.gnome.org/show_bug.cgi?id=692515
     */
    namespace = g_base_info_get_namespace ((GIBaseInfo *) info);
    type_name = g_base_info_get_name ((GIBaseInfo *) info);
    full_name = g_strconcat ("Gu", namespace, type_name, NULL);

    /* If enum registration fails, free all the memory allocated
     * for the values array. This needs to leak when successful
     * as GObject keeps a reference to the data as specified in the docs.
     */
    g_type = g_flags_register_static (full_name, g_flags_values);
    if (g_type == G_TYPE_INVALID) {
        for (i = 0; i < n_values; i++) {
            GFlagsValue *flags_value = &g_flags_values[i];

            /* Only free value_name if it is different from value_nick to avoid
             * a double free. The pointer might have been is re-used in the case
             * c_identifier was NULL in the above loop.
             */
            if (flags_value->value_name != flags_value->value_nick)
                g_free ((gchar *) flags_value->value_name);
            g_free ((gchar *) flags_value->value_nick);
        }

        GuErr_Format (GuExc_RuntimeError, "Unable to register flags '%s'", full_name);

        g_free (g_flags_values);
        g_free (full_name);
        return NULL;
    }

    g_free (full_name);
    return gug_flags_add (NULL, type_name, NULL, g_type);
}

static void
initialize_interface (GTypeInterface *iface, SCM gutype /* a type */)
{
    /* gugobject prints a warning if interface_init is NULL */
}

static SCM 
_wrap_gug_register_interface_info (SCM self, SCM args)
{
    SCM gu_g_type;
    GType g_type;
    GInterfaceInfo *info;

    if (!GuArg_ParseTuple (args, "O!:register_interface_info",
                           &GuGTypeWrapper_Type, &gu_g_type)) {
        return NULL;
    }

    g_type = gug_type_from_object (gu_g_type);
    if (!g_type_is_a (g_type, G_TYPE_INTERFACE)) {
        GuErr_SetString (GuExc_TypeError, "must be an interface");
        return NULL;
    }

    info = g_new0 (GInterfaceInfo, 1);
    info->interface_init = (GInterfaceInitFunc) initialize_interface;

    gug_register_interface_info (g_type, info);

    Gu_RETURN_NONE;
}

static void
find_vfunc_info (GIBaseInfo *vfunc_info,
                 GType implementor_gtype,
                 gpointer *implementor_class_ret,
                 gpointer *implementor_vtable_ret,
                 GIFieldInfo **field_info_ret)
{
    GType ancestor_g_type = 0;
    int length, i;
    GIBaseInfo *ancestor_info;
    GIStructInfo *struct_info;
    gpointer implementor_class = NULL;
    gboolean is_interface = FALSE;

    ancestor_info = g_base_info_get_container (vfunc_info);
    is_interface = g_base_info_get_type (ancestor_info) == GI_INFO_TYPE_INTERFACE;

    ancestor_g_type = g_registered_type_info_get_g_type (
                          (GIRegisteredTypeInfo *) ancestor_info);
    implementor_class = g_type_class_ref (implementor_gtype);
    if (is_interface) {
        GTypeInstance *implementor_iface_class;
        implementor_iface_class = g_type_interface_peek (implementor_class,
                                                         ancestor_g_type);
        if (implementor_iface_class == NULL) {
            g_type_class_unref (implementor_class);
            GuErr_Format (GuExc_RuntimeError,
                          "Couldn't find GType of implementor of interface %s. "
                          "Forgot to set __gtype_name__?",
                          g_type_name (ancestor_g_type));
            return;
        }

        *implementor_vtable_ret = implementor_iface_class;

        struct_info = g_interface_info_get_iface_struct ( (GIInterfaceInfo*) ancestor_info);
    } else {
        struct_info = g_object_info_get_class_struct ( (GIObjectInfo*) ancestor_info);
        *implementor_vtable_ret = implementor_class;
    }

    *implementor_class_ret = implementor_class;

    length = g_struct_info_get_n_fields (struct_info);
    for (i = 0; i < length; i++) {
        GIFieldInfo *field_info;
        GITypeInfo *type_info;

        field_info = g_struct_info_get_field (struct_info, i);

        if (strcmp (g_base_info_get_name ( (GIBaseInfo*) field_info),
                    g_base_info_get_name ( (GIBaseInfo*) vfunc_info)) != 0) {
            g_base_info_unref (field_info);
            continue;
        }

        type_info = g_field_info_get_type (field_info);
        if (g_type_info_get_tag (type_info) == GI_TYPE_TAG_INTERFACE) {
            g_base_info_unref (type_info);
            *field_info_ret = field_info;
            break;
        }

        g_base_info_unref (type_info);
        g_base_info_unref (field_info);
    }

    g_base_info_unref (struct_info);
}

static SCM 
_wrap_gug_hook_up_vfunc_implementation (SCM self, SCM args)
{
    GuGIBaseInfo *gu_info;
    SCM gu_type;
    SCM gu_function;
    GType implementor_gtype = 0;
    gpointer implementor_class = NULL;
    gpointer implementor_vtable = NULL;
    GIFieldInfo *field_info = NULL;
    gpointer *method_ptr = NULL;
    GuGICClosure *closure = NULL;
    GuGIClosureCache *cache = NULL;

    if (!GuArg_ParseTuple (args, "O!O!O:hook_up_vfunc_implementation",
                           &GuGIBaseInfo_Type, &gu_info,
                           &GuGTypeWrapper_Type, &gu_type,
                           &gu_function))
        return NULL;

    implementor_gtype = gug_type_from_object (gu_type);
    g_assert (G_TYPE_IS_CLASSED (implementor_gtype));

    find_vfunc_info (gu_info->info, implementor_gtype, &implementor_class, &implementor_vtable, &field_info);
    if (field_info != NULL) {
        GITypeInfo *type_info;
        GIBaseInfo *interface_info;
        GICallbackInfo *callback_info;
        gint offset;

        type_info = g_field_info_get_type (field_info);

        interface_info = g_type_info_get_interface (type_info);
        g_assert (g_base_info_get_type (interface_info) == GI_INFO_TYPE_CALLBACK);

        callback_info = (GICallbackInfo*) interface_info;
        offset = g_field_info_get_offset (field_info);
        method_ptr = G_STRUCT_MEMBER_P (implementor_vtable, offset);

        cache = gugi_closure_cache_new (callback_info);
        closure = _gugi_make_native_closure ( (GICallableInfo*) callback_info, cache,
                                              GI_SCOPE_TYPE_NOTIFIED, gu_function, NULL);

        *method_ptr = closure->closure;

        g_base_info_unref (interface_info);
        g_base_info_unref (type_info);
        g_base_info_unref (field_info);
    }
    g_type_class_unref (implementor_class);

    Gu_RETURN_NONE;
}

#if 0
/* Not used, left around for future reference */
static SCM 
_wrap_gug_has_vfunc_implementation (SCM self, SCM args)
{
    GuGIBaseInfo *gu_info;
    SCM gu_type;
    SCM gu_ret;
    gpointer implementor_class = NULL;
    gpointer implementor_vtable = NULL;
    GType implementor_gtype = 0;
    GIFieldInfo *field_info = NULL;

    if (!GuArg_ParseTuple (args, "O!O!:has_vfunc_implementation",
                           &GuGIBaseInfo_Type, &gu_info,
                           &GuGTypeWrapper_Type, &gu_type))
        return NULL;

    implementor_gtype = gug_type_from_object (gu_type);
    g_assert (G_TYPE_IS_CLASSED (implementor_gtype));

    gu_ret = Gu_False;
    find_vfunc_info (gu_info->info, implementor_gtype, &implementor_class, &implementor_vtable, &field_info);
    if (field_info != NULL) {
        gpointer *method_ptr;
        gint offset;

        offset = g_field_info_get_offset (field_info);
        method_ptr = G_STRUCT_MEMBER_P (implementor_vtable, offset);
        if (*method_ptr != NULL) {
            gu_ret = Gu_True;
        }

        g_base_info_unref (field_info);
    }
    g_type_class_unref (implementor_class);

    Gu_INCREF(gu_ret);
    return gu_ret;
}
#endif

static SCM 
_wrap_gug_variant_type_from_string (SCM self, SCM args)
{
    char *type_string;
    SCM gu_type;
    SCM gu_variant = NULL;

    if (!GuArg_ParseTuple (args, "s:variant_type_from_string",
                           &type_string)) {
        return NULL;
    }

    gu_type = gugi_type_import_by_name ("GLib", "VariantType");

    gu_variant = gugi_boxed_new (gu_type, type_string, FALSE, 0);

    return gu_variant;
}

#define CHUNK_SIZE 8192

static SCM 
gug_channel_read(SCM  self, SCM args, SCM kwargs)
{
    int max_count = -1;
    SCM gu_iochannel, ret_obj = SCM_BOOL_F;
    gsize total_read = 0;
    GError* error = NULL;
    GIOStatus status = G_IO_STATUS_NORMAL;
    GIOChannel *iochannel = NULL;

    if (!GuArg_ParseTuple (args, "Oi:gug_channel_read", &gu_iochannel, &max_count)) {
        return NULL;
    }
    if (!gug_boxed_check (gu_iochannel, G_TYPE_IO_CHANNEL)) {
        GuErr_SetString(GuExc_TypeError, "first argument is not a GLib.IOChannel");
        return NULL;
    }
	
    if (max_count == 0)
        return GUGLIB_GuBytes_FromString("");

    iochannel = gug_boxed_get (gu_iochannel, GIOChannel);

    while (status == G_IO_STATUS_NORMAL
	   && (max_count == -1 || total_read < (gsize)max_count)) {
	gsize single_read;
	char* buf;
	gsize buf_size;
	
	if (max_count == -1) 
	    buf_size = CHUNK_SIZE;
	else {
	    buf_size = max_count - total_read;
	    if (buf_size > CHUNK_SIZE)
		buf_size = CHUNK_SIZE;
        }
	
	if ( ret_obj == NULL ) {
	    ret_obj = GUGLIB_GuBytes_FromStringAndSize((char *)NULL, buf_size);
	    if (ret_obj == NULL)
		goto failure;
	}
	else if (buf_size + total_read > (gsize)GUGLIB_GuBytes_Size(ret_obj)) {
	    if (GUGLIB_GuBytes_Resize(ret_obj, buf_size + total_read) == -1)
		goto failure;
	}
       
        buf = GUGLIB_GuBytes_AsString(ret_obj) + total_read;

        Gu_BEGIN_ALLOW_THREADS;
        status = g_io_channel_read_chars (iochannel, buf, buf_size, &single_read, &error);
        Gu_END_ALLOW_THREADS;

        if (gugi_error_check (&error))
	    goto failure;
	
	total_read += single_read;
    }
	
    if ( total_read != (gsize)GUGLIB_GuBytes_Size(ret_obj) ) {
	if (GUGLIB_GuBytes_Resize(ret_obj, total_read) == -1)
	    goto failure;
    }

    return ret_obj;

  failure:
    Gu_XDECREF(ret_obj);
    return NULL;
}

static gboolean
marshal_emission_hook(GSignalInvocationHint *ihint,
		      guint n_param_values,
		      const GValue *param_values,
		      gpointer user_data)
{
    GuGILState_STATE state;
    gboolean retval = FALSE;
    SCM func, args;
    SCM retobj;
    SCM params;
    guint i;

    state = GuGILState_Ensure();

    /* construct Guthon tuple for the parameter values */
    params = GuTuple_New(n_param_values);

    for (i = 0; i < n_param_values; i++) {
	SCM item = gug_value_as_guobject(&param_values[i], FALSE);

	/* error condition */
	if (!item) {
	    goto out;
	}
	GuTuple_SetItem(params, i, item);
    }

    args = (SCM )user_data;
    func = GuTuple_GetItem(args, 0);
    args = GuSequence_Concat(params, GuTuple_GetItem(args, 1));
    Gu_DECREF(params);

    /* params passed to function may have extra arguments */

    retobj = GuObject_CallObject(func, args);
    Gu_DECREF(args);
    if (retobj == NULL) {
        GuErr_Print();
    }

    retval = (scm_is_true(retobj) ? TRUE : FALSE);
    Gu_XDECREF(retobj);
out:
    GuGILState_Release(state);
    return retval;
}

/**
 * gug_destroy_notify:
 * @user_data: a GuObject pointer.
 *
 * A function that can be used as a GDestroyNotify callback that will
 * call Gu_DECREF on the data.
 */
static void
gug_destroy_notify(gpointer user_data)
{
    SCM obj = (SCM )user_data;
    GuGILState_STATE state;

    state = GuGILState_Ensure();
    Gu_DECREF(obj);
    GuGILState_Release(state);
}

static SCM 
gug_add_emission_hook(GuGObject *self, SCM args)
{
    SCM first, callback, extra_args, data, repr;
    gchar *name;
    gulong hook_id;
    guint sigid;
    Gu_ssize_t len;
    GQuark detail = 0;
    GType gtype;
    SCM gugtype;

    len = GuTuple_Size(args);
    if (len < 3) {
	GuErr_SetString(GuExc_TypeError,
			"gobject.add_emission_hook requires at least 3 arguments");
	return NULL;
    }
    first = GuSequence_GetSlice(args, 0, 3);
    if (!GuArg_ParseTuple(first, "OsO:add_emission_hook",
			  &gugtype, &name, &callback)) {
	Gu_DECREF(first);
	return NULL;
    }
    Gu_DECREF(first);

    if ((gtype = gug_type_from_object(gugtype)) == 0) {
	return NULL;
    }
    if (!GuCallable_Check(callback)) {
	GuErr_SetString(GuExc_TypeError, "third argument must be callable");
	return NULL;
    }

    if (!g_signal_parse_name(name, gtype, &sigid, &detail, TRUE)) {
	repr = GuObject_Repr((SCM )self);
	GuErr_Format(GuExc_TypeError, "%s: unknown signal name: %s",
			GUGLIB_GuUnicode_AsString(repr),
		     name);
	Gu_DECREF(repr);
	return NULL;
    }
    extra_args = GuSequence_GetSlice(args, 3, len);
    if (extra_args == NULL)
	return NULL;

    data = Gu_BuildValue("(ON)", callback, extra_args);
    if (data == NULL)
      return NULL;

    hook_id = g_signal_add_emission_hook(sigid, detail,
					 marshal_emission_hook,
					 data,
					 (GDestroyNotify)gug_destroy_notify);

    return scm_from_ulong (hook_id);
}

static SCM 
gug_signal_new(SCM self, SCM args)
{
    gchar *signal_name;
    SCM gu_type;
    GSignalFlags signal_flags;
    GType return_type;
    SCM gu_return_type, gu_param_types;

    GType instance_type = 0;
    Gu_ssize_t gu_n_params;
    guint n_params, i;
    GType *param_types;

    guint signal_id;

    if (!GuArg_ParseTuple(args, "sOiOO:gobject.signal_new", &signal_name,
			  &gu_type, &signal_flags, &gu_return_type,
			  &gu_param_types))
	return NULL;

    instance_type = gug_type_from_object(gu_type);
    if (!instance_type)
	return NULL;
    if (!(G_TYPE_IS_INSTANTIATABLE(instance_type) || G_TYPE_IS_INTERFACE(instance_type))) {
	GuErr_SetString(GuExc_TypeError,
			"argument 2 must be an object type or interface type");
	return NULL;
    }

    return_type = gug_type_from_object(gu_return_type);
    if (!return_type)
	return NULL;

    if (!GuSequence_Check(gu_param_types)) {
	GuErr_SetString(GuExc_TypeError,
			"argument 5 must be a sequence of GType codes");
	return NULL;
    }

    gu_n_params = GuSequence_Length(gu_param_types);
    if (gu_n_params < 0)
        return FALSE;

    n_params = gu_n_params;

    param_types = g_new(GType, n_params);
    for (i = 0; i < n_params; i++) {
	SCM item = GuSequence_GetItem(gu_param_types, i);

	param_types[i] = gug_type_from_object(item);
	if (param_types[i] == 0) {
	    GuErr_Clear();
	    Gu_DECREF(item);
	    GuErr_SetString(GuExc_TypeError,
			    "argument 5 must be a sequence of GType codes");
	    g_free(param_types);
	    return NULL;
	}
	Gu_DECREF(item);
    }

    signal_id = g_signal_newv(signal_name, instance_type, signal_flags,
			      gug_signal_class_closure_get(),
			      (GSignalAccumulator)0, NULL,
			      (GSignalCMarshaller)0,
			      return_type, n_params, param_types);
    g_free(param_types);
    if (signal_id != 0)
	return scm_from_uint (signal_id);
    GuErr_SetString(GuExc_RuntimeError, "could not create signal");
    return NULL;
}

static SCM 
gug_object_class_list_properties (SCM self, SCM args)
{
    GParamSpec **specs;
    SCM gu_itype, list;
    GType itype;
    GObjectClass *class = NULL;
    gpointer iface = NULL;
    guint nprops;
    guint i;

    if (!GuArg_ParseTuple(args, "O:gobject.list_properties",
			  &gu_itype))
	return NULL;
    if ((itype = gug_type_from_object(gu_itype)) == 0)
	return NULL;

    if (G_TYPE_IS_INTERFACE(itype)) {
        iface = g_type_default_interface_ref(itype);
        if (!iface) {
            GuErr_SetString(GuExc_RuntimeError,
                            "could not get a reference to interface type");
            return NULL;
        }
        specs = g_object_interface_list_properties(iface, &nprops);
    } else if (g_type_is_a(itype, G_TYPE_OBJECT)) {
        class = g_type_class_ref(itype);
        if (!class) {
            GuErr_SetString(GuExc_RuntimeError,
                            "could not get a reference to type class");
            return NULL;
        }
        specs = g_object_class_list_properties(class, &nprops);
    } else {
	GuErr_SetString(GuExc_TypeError,
                        "type must be derived from GObject or an interface");
	return NULL;
    }

    list = GuTuple_New(nprops);
    if (list == NULL) {
	g_free(specs);
	g_type_class_unref(class);
	return NULL;
    }
    for (i = 0; i < nprops; i++) {
	GuTuple_SetItem(list, i, gug_param_spec_new(specs[i]));
    }
    g_free(specs);
    if (class)
        g_type_class_unref(class);
    else
        g_type_default_interface_unref(iface);

    return list;
}

static SCM 
gug__install_metaclass(SCM dummy, SCM metaclass /* a type */)
{
    Gu_INCREF(metaclass);
    GuGObject_MetaType = metaclass;
    Gu_INCREF(metaclass);

    //Gu_TYPE(&GuGObject_Type) = metaclass;
    gug_foreign_object_type_set_type (GuGObject_Type, metaclass);

    Gu_INCREF(Gu_None);
    return Gu_None;
}

static SCM 
_wrap_guig_guos_getsig (SCM self, SCM args)
{
    int sig_num;

    if (!GuArg_ParseTuple (args, "i:guos_getsig", &sig_num))
        return NULL;

    // return GuLong_FromVoidPtr ((void *)(GuOS_getsig (sig_num)));
    return scm_from_size_t ((size_t)(void *) (GuOS_getsig (sig_num)));
}

static SCM 
_wrap_gugobject_new_full (SCM self, SCM args)
{
    SCM ptr_value, long_value;
    SCM steal;
    GObject *obj;

    if (!GuArg_ParseTuple (args, "OO", &ptr_value, &steal))
        return NULL;

    // Convert pointer to long? 
    long_value = ptr_value;
    if (!long_value) {
        GuErr_SetString (GuExc_TypeError, "first argument must be an integer");
        return NULL;
    }
    obj = (void *)scm_to_size_t (long_value);
    Gu_DECREF (long_value);

    if (!G_IS_OBJECT (obj)) {
        GuErr_SetString (GuExc_TypeError, "pointer is not a GObject");
        return NULL;
    }

    return gugobject_new_full (obj, scm_is_true (steal), NULL);
}

static GuMethodDef _gi_functions[] = {
    { "gugobject_new_full", (GuCFunction) _wrap_gugobject_new_full, METH_VARARGS },
    { "enum_add", (GuCFunction) _wrap_gug_enum_add, METH_VARARGS | METH_KEYWORDS },
    { "enum_register_new_gtype_and_add", (GuCFunction) _wrap_gug_enum_register_new_gtype_and_add, METH_VARARGS | METH_KEYWORDS },
    { "flags_add", (GuCFunction) _wrap_gug_flags_add, METH_VARARGS | METH_KEYWORDS },
    { "flags_register_new_gtype_and_add", (GuCFunction) _wrap_gug_flags_register_new_gtype_and_add, METH_VARARGS | METH_KEYWORDS },

    { "register_interface_info", (GuCFunction) _wrap_gug_register_interface_info, METH_VARARGS },
    { "hook_up_vfunc_implementation", (GuCFunction) _wrap_gug_hook_up_vfunc_implementation, METH_VARARGS },
    { "variant_type_from_string", (GuCFunction) _wrap_gug_variant_type_from_string, METH_VARARGS },
    { "source_new", (GuCFunction) gugi_source_new, METH_NOARGS },
    { "guos_getsig", (GuCFunction) _wrap_guig_guos_getsig, METH_VARARGS },
    { "source_set_callback", (GuCFunction) gugi_source_set_callback, METH_VARARGS },
    { "io_channel_read", (GuCFunction) gug_channel_read, METH_VARARGS },
    { "require_foreign", (GuCFunction) gugi_require_foreign, METH_VARARGS | METH_KEYWORDS },
    { "spawn_async",
      (GuCFunction) guglib_spawn_async, METH_VARARGS|METH_KEYWORDS,
      "spawn_async(argv, envp=None, working_directory=None,\n"
      "            flags=0, child_setup=None, user_data=None,\n"
      "            standard_input=None, standard_output=None,\n"
      "            standard_error=None) -> (pid, stdin, stdout, stderr)\n"
      "\n"
      "Execute a child program asynchronously within a glib.MainLoop()\n"
      "See the reference manual for a complete reference.\n" },
    { "type_register", _wrap_gug_type_register, METH_VARARGS },
    { "signal_new", gug_signal_new, METH_VARARGS },
    { "list_properties",
      gug_object_class_list_properties, METH_VARARGS },
    { "new",
      (GuCFunction)gug_object_new, METH_VARARGS|METH_KEYWORDS },
    { "add_emission_hook",
      (GuCFunction)gug_add_emission_hook, METH_VARARGS },
    { "_install_metaclass",
      (GuCFunction)gug__install_metaclass, METH_O },
    { "_gvalue_get",
      (GuCFunction)gug__gvalue_get, METH_O },
    { "_gvalue_set",
      (GuCFunction)gug__gvalue_set, METH_VARARGS },
    { NULL, NULL, 0 }
};

static struct GuGI_API CAPI = {
  gugi_register_foreign_struct,
};

struct _GuGObject_Functions gugobject_api_functions = {
  gugobject_register_class,
  gugobject_register_wrapper,
  gugobject_lookup_class,
  gugobject_new,

  gug_closure_new,
  gugobject_watch_closure,
  gug_destroy_notify,

  gug_type_from_object,
  gug_type_wrapper_new,
  gug_enum_get_value,
  gug_flags_get_value,
  gug_register_gtype_custom,
  gug_value_from_guobject,
  gug_value_as_guobject,

  gug_register_interface,

  &GuGBoxed_Type,
  gugi_register_gboxed,
  gugi_gboxed_new,

  &GuGPointer_Type,
  gug_register_pointer,
  gug_pointer_new,

  gug_enum_add_constants,
  gug_flags_add_constants,

  gug_constant_strip_prefix,

  gugi_error_check,

  0, /* _gug_set_thread_block_funcs */
  (GuGThreadBlockFunc)0, /* block_threads */
  (GuGThreadBlockFunc)0, /* unblock_threads */

  &GuGParamSpec_Type,
  gug_param_spec_new,
  gug_param_spec_from_object,

  0, /*gug_guobj_to_unichar_conv, */
  0, /*gug_parse_constructor_args, */
  gug_param_gvalue_as_guobject,
  gug_param_gvalue_from_guobject,

  &GuGEnum_Type,
  gug_enum_add,
  0, /* gug_enum_from_gtype,*/

  &GuGFlags_Type,
  gug_flags_add,
  0, /*gug_flags_from_gtype, */

  TRUE, /* threads_enabled */

  0, /*gugobject_enable_threads,*/
  0, /*gugobject_gil_state_ensure,*/
  0, /*gugobject_gil_state_release,*/
  gug_register_class_init,
  gug_register_interface_info,

  gug_closure_set_exception_handler,

  add_warning_redirection,
  disable_warning_redirections,

  NULL, /* previously type_register_custom */

  0, /*gugi_gerror_exception_check,*/

  0, /*gug_option_group_new,*/
  gug_type_from_object_strict,

  gugobject_new_full,
  &GuGObject_Type,

  gug_value_from_guobject_with_error
};

/**
 * Returns 0 on success, or -1 and sets an exception.
 */
static int
gugi_register_api(SCM d)
{
    SCM api;

    api = GuCapsule_New (&gugobject_api_functions, "gobject._GuGObject_API", NULL);
    if (api == NULL)
        return -1;
    GuDict_SetItemString(d, "_GuGObject_API", api);
    Gu_DECREF(api);
    return 0;
}

/**
 * Returns 0 on success, or -1 and sets an exception.
 */
static int
gugi_register_constants(SCM m)
{
    /* GuFloat_ return a new ref, and add object takes the ref */
    GuModule_AddObject(m,       "G_MINFLOAT", scm_from_double (G_MINFLOAT));
    GuModule_AddObject(m,       "G_MAXFLOAT", scm_from_double (G_MAXFLOAT));
    GuModule_AddObject(m,       "G_MINDOUBLE", scm_from_double (G_MINDOUBLE));
    GuModule_AddObject(m,       "G_MAXDOUBLE", scm_from_double (G_MAXDOUBLE));
    GuModule_AddIntConstant(m,  "G_MINSHORT", G_MINSHORT);
    GuModule_AddIntConstant(m,  "G_MAXSHORT", G_MAXSHORT);
    GuModule_AddIntConstant(m,  "G_MAXUSHORT", G_MAXUSHORT);
    GuModule_AddIntConstant(m,  "G_MININT", G_MININT);
    GuModule_AddIntConstant(m,  "G_MAXINT", G_MAXINT);
    GuModule_AddObject(m,       "G_MAXUINT", scm_from_uint (G_MAXUINT));
    GuModule_AddObject(m,       "G_MINLONG", scm_from_long (G_MINLONG));
    GuModule_AddObject(m,       "G_MAXLONG", scm_from_long (G_MAXLONG));
    GuModule_AddObject(m,       "G_MAXULONG", scm_from_ulong (G_MAXULONG));
    GuModule_AddObject(m,       "G_MAXSIZE", scm_from_size_t (G_MAXSIZE));
    GuModule_AddObject(m,       "G_MAXSSIZE", scm_from_ssize_t (G_MAXSSIZE));
    GuModule_AddObject(m,       "G_MINSSIZE", scm_from_ssize_t (G_MINSSIZE));
    GuModule_AddObject(m,       "G_MINOFFSET", scm_from_int64 (G_MINOFFSET));
    GuModule_AddObject(m,       "G_MAXOFFSET", scm_from_int64 (G_MAXOFFSET));

    GuModule_AddIntConstant(m, "SIGNAL_RUN_FIRST", G_SIGNAL_RUN_FIRST);
    GuModule_AddIntConstant(m, "PARAM_READWRITE", G_PARAM_READWRITE);

    /* The rest of the types are set in __init__.gu */
    GuModule_AddObject(m, "TYPE_INVALID", gug_type_wrapper_new(G_TYPE_INVALID));
    GuModule_AddObject(m, "TYPE_GSTRING", gug_type_wrapper_new(G_TYPE_GSTRING));

    return 0;
}

/**
 * Returns 0 on success, or -1 and sets an exception.
 */
static int
gugi_register_version_tuples(SCM d)
{
    SCM tuple;

    /* gugobject version */
    tuple = Gu_BuildValue ("(iii)",
                           GUGOBJECT_MAJOR_VERSION,
                           GUGOBJECT_MINOR_VERSION,
                           GUGOBJECT_MICRO_VERSION);
    GuDict_SetItemString(d, "gugobject_version", tuple);
    Gu_DECREF (tuple);
    return 0;
}

GUGLIB_MODULE_START(_gi, "_gi")
{
    SCM api;
    SCM module_dict = GuModule_GetDict (module);

    /* Always enable Guthon threads since we cannot predict which GI repositories
     * might accept Guthon callbacks run within non-Guthon threads or might trigger
     * toggle ref notifications.
     * See: https://bugzilla.gnome.org/show_bug.cgi?id=709223
     */
    GuEval_InitThreads ();

    GuModule_AddStringConstant(module, "__package__", "gi._gi");
#if 0
    if (gugi_foreign_init () < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_error_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_repository_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_info_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_type_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_pointer_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_struct_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_gboxed_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_boxed_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_ccallback_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_resulttuple_register_types (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;

    if (gugi_spawn_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_option_context_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_option_group_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;

    if (gugi_register_api (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_register_constants (module) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_register_version_tuples (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_register_warnings (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gui_object_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_interface_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_paramspec_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_enum_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;
    if (gugi_flags_register_types (module_dict) < 0)
        return GUGLIB_MODULE_ERROR_RETURN;

    GuGIWarning = GuErr_NewException ("gi.GuGIWarning", GuExc_Warning, NULL);
    if (GuGIWarning == NULL)
        return GUGLIB_MODULE_ERROR_RETURN;

    /* Use RuntimeWarning as the base class of GuGIDeprecationWarning
     * for unstable (odd minor version) and use DeprecationWarning for
     * stable (even minor version). This is so GuGObject deprecations
     * behave the same as regular Guthon deprecations in stable releases.
     */
#if GUGOBJECT_MINOR_VERSION % 2
    GuGIDeprecationWarning = GuErr_NewException("gi.GuGIDeprecationWarning",
                                                GuExc_RuntimeWarning, NULL);
#else
    GuGIDeprecationWarning = GuErr_NewException("gi.GuGIDeprecationWarning",
                                                GuExc_DeprecationWarning, NULL);
#endif

    /* Place holder object used to fill in "from Guthon" argument lists
     * for values not supplied by the caller but support a GI default.
     */
    _GuGIDefaultArgPlaceholder = GuList_New(0);

    Gu_INCREF (GuGIWarning);
    GuModule_AddObject (module, "GuGIWarning", GuGIWarning);

    Gu_INCREF(GuGIDeprecationWarning);
    GuModule_AddObject(module, "GuGIDeprecationWarning", GuGIDeprecationWarning);

    api = GuCapsule_New ( (void *) &CAPI, "gi._API", NULL);
    if (api == NULL) {
        return GUGLIB_MODULE_ERROR_RETURN;
    }
    GuModule_AddObject (module, "_API", api);
#endif
}
GUGLIB_MODULE_END


#if 0
void
gir_log_handler (const gchar *log_domain,
		 GLogLevelFlags log_level,
		 const gchar *message,
		 gpointer user_data)
{
  time_t timer;
  char buffer[26];
  struct tm* tm_info;
  time(&timer);
  tm_info = localtime(&timer);
  strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_info);
  FILE *fp = fopen("gir-log.txt", "at");
  fprintf (fp, "%s: %s %d %s\n", buffer, log_domain, log_level, message);
  fclose (fp);
}

static void
scm_add_string_constant (const char *name, const char *val)
{
  SCM var = scm_c_define (name, scm_from_utf8_string (val));
  scm_permanent_object (var);
  scm_c_export (name, NULL);
}

void gir_module_init(void)
{
  SCM api;
  SCM module_dict = scm_c_make_hash_table(30);

  #if 0
  g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
		     | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
  g_log_set_handler ("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                   | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
  #endif

  g_debug ("begin initialization");
  scm_add_string_constant ("__package__", "gir._gi");
  init_pycompat();
  gir_foreign_init();
  gir_error_register_types();
  gir_repository_register_types();
  gir_info_register_types();
  g_debug ("end initialization");
}
#endif
