/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <glib.h>
#include "gi_gflags.h"
#include "gi_util.h"
#include "gir_xguile.h"

GQuark gugflags_class_key;

gboolean gi_gflags_check (SCM x)
{
    return (SCM_IS_A_P (x, gi_gflags_type));
}

/* re pyg_flags_val_new */
static SCM
gi_gflags_val_new(SCM subclass, GType gtype, int intval)
{
    SCM item;

    item = scm_make_foreign_object_0(gi_gflags_type);
    gi_gflags_set_gtype (item, gtype);
    gi_gflags_set_value (item, intval);
    return item;
}

/* Dynamically create a GFlags collection for the given GType */
/* re pyg_flags_add */
SCM gi_gflags_add (const char *typename,
		   const char *strip_prefix,
		   GType gtype)
{
    SCM stub, values, o;
    GFlagsClass *eclass;
    guint i;

    g_return_val_if_fail(typename != NULL, NULL);
    if (!g_type_is_a(gtype, G_TYPE_FLAGS))
	scm_misc_error ("gi_gflags_add",
			"Trying to register gtype '~S' as flags when in fact it is of type ~S",
			scm_list_2 (scm_from_utf8_string (g_type_name (gtype)),
				    scm_from_utf8_string (g_type_name (G_TYPE_FUNDAMENTAL (gtype)))));

    /* Create a new type derived from GFlags. This is the same as:
     * >>> stub = type(typename, (GFlags,), {})
     */
    stub = scm_make_foreign_object_0 (gi_gflags_type);
    g_type_set_qdata(gtype, gugflags_class_key, stub);

    /* Register flag values */
    eclass = G_FLAGS_CLASS(g_type_class_ref(gtype));
    values = scm_c_make_hash_table (10);
    gi_gflagscollection_set_gtype (stub, gtype);
    gi_gflagscollection_set_flags_values (stub, values);
    
    for (i = 0; i < eclass->n_values; i++) {
	SCM item, intval;
	char *prefix;
	  
	intval = scm_from_int (eclass->values[i].value);
	item = gi_gflags_val_new (stub, gtype, eclass->values[i].value);
	scm_hash_set_x (values, intval, item);

	prefix = g_strdup(gi_constant_strip_prefix(eclass->values[i].value_name, strip_prefix));
	scm_permanent_object (scm_c_define (prefix, item));
	g_free(prefix);
    }

    g_type_class_unref(eclass);

    return stub;
}

SCM gi_gflags_from_gtype (GType gtype,
			  guint value)
{
    void *ptr;
    SCM guclass;
    SCM intval;
    SCM retval;
    SCM values;
    
    g_return_val_if_fail (gtype != G_TYPE_INVALID, SCM_BOOL_F);

    ptr = g_type_get_qdata (gtype, gugflags_class_key);
    /* if (!ptr) */
    /* 	ptr = gi_gtype_import_by_g_type (gtype); */
    if (ptr)
	guclass = ptr;
    else
	guclass = gi_gflags_add (g_type_name (gtype), NULL, gtype);
    values = gi_gflagscollection_get_flags_values (guclass);
    intval = scm_from_ulong (value);
    retval = scm_hash_ref (values, intval, SCM_BOOL_F);
    if (scm_is_false (retval))
	retval = gi_gflags_val_new (guclass, gtype, value);
    return retval;
}

/**
 * pyg_flags_get_value:
 * @flag_type: the GType of the flag.
 * @obj: a Python object representing the flag value
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Python object to the integer equivalent.  The conversion
 * will depend on the type of the Python object.  If the object is an
 * integer, it is passed through directly.  If it is a string, it will
 * be treated as a full or short flag name as defined in the GType.
 * If it is a tuple, then the items are treated as strings and ORed
 * together.
 *
 * Returns: 0 on success or -1 on failure
 */
gint
gi_flags_get_value(GType flag_type, SCM obj, guint *val)
{
    GFlagsClass *fclass = NULL;

    g_return_val_if_fail(val != NULL, -1);
    if (scm_is_exact_integer (obj)) {
	*val = scm_to_int (obj);
	return 0;
    } else if (scm_is_string (obj)) {
	GFlagsValue *info;
	char *str = scm_to_utf8_string (obj);

	if (flag_type != G_TYPE_NONE)
	    fclass = G_FLAGS_CLASS(g_type_class_ref(flag_type));
	else {
	    scm_misc_error ("gi_flags_get_value",
			    "could not convert string to flag because there is no GType associated to look up the value",
			    SCM_EOL);
	    return -1;
	}
	info = g_flags_get_value_by_name(fclass, str);
	g_type_class_unref(fclass);

	if (!info)
	    info = g_flags_get_value_by_nick(fclass, str);
	if (info) {
	    *val = info->value;
	    return 0;
	} else {
	    scm_misc_error ("gi_flags_get_value",
			    "could not convert string '~S",
			    scm_list_1 (obj));
	    return -1;
	}
    } else if (scm_is_list (obj)) {
	ssize_t i, len;

	len = scm_to_ssize_t (scm_length (obj));
	*val = 0;

	if (flag_type != G_TYPE_NONE)
	    fclass = G_FLAGS_CLASS(g_type_class_ref(flag_type));
	else {
	    scm_misc_error ("gi_flags_get_value",
			    "could not convert string to flag because there is no GType associated to look up the value",
			    SCM_EOL);
	    return -1;
	}

	for (i = 0; i < len; i++) {
	    SCM item = scm_c_list_ref (obj, i);
	    char *str = scm_to_utf8_string (item);
	    GFlagsValue *info = g_flags_get_value_by_name(fclass, str);

	    if (!info)
		info = g_flags_get_value_by_nick(fclass, str);
	    if (info) {
		*val |= info->value;
	    } else {
		scm_misc_error ("gi_flags_get_value", "could not convert string '~S'", scm_list_1(item));
		return -1;
		break;
	    }
	}
	g_type_class_unref(fclass);
    } else {
	scm_misc_error ("gi_flags_get_value",
			"flag values must by strings, ints, or lists of strings", SCM_EOL);
	return -1;
    }
    return 0;
}


void
gi_init_gflags (void)
{
  gi_init_gflags_type ();
  gi_init_gflagscollection_type ();
  gugflags_class_key = g_quark_from_static_string ("guile-gi::gflags");

}
