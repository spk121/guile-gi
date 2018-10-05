/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_genum.h"
#include "gi_gtype.h"
#include "gi_util.h"

GQuark gugenum_class_key;

gboolean
gi_genum_check (SCM x)
{
    return (SCM_IS_A_P (x, gi_genum_type));
}

/* re pyg_enum_val_new */
static SCM
gi_genum_val_new (SCM subclass, GType gtype, int intval)
{
    SCM genum;

    genum = scm_make_foreign_object_0 (gi_genum_type);
    gi_genum_set_gtype (genum, gtype);
    gi_genum_set_value (genum, intval);
    return genum;
}

/* Creates a new enum value. Creating a wrapper class for
   gtype when necessary. */
/* re pyg_enum_from_gtype */
SCM
gi_genum_from_gtype (GType gtype, int value)
{
    void *ptr;
    SCM class;
    SCM values;
    SCM retval;
    
    g_return_val_if_fail (gtype != G_TYPE_INVALID, SCM_BOOL_F);

    /* Get a wrapper class by:
     * 1. check for one attached to gtype
     * 2. looking up one in a typelib
     * 3. creating a new one
     */
    ptr = g_type_get_qdata (gtype, gugenum_class_key);
    /* if (!ptr) */
    /* 	ptr = gi_gtype_import_by_g_type (gtype); */
    if (ptr)
	class = ptr;
    else
	class = gi_genum_add (g_type_name (gtype), NULL, gtype);

    values = gi_genumcollection_get_enum_values (class);
    retval = scm_hash_ref (values, scm_from_int (value), SCM_BOOL_F);
    if (scm_is_false (retval))
	/* MLG - so in here, we're making an enum value that doesn't
	   have a name attached? It is supposed to be inserted into
	   the GEnumCollection?*/
	retval = gi_genum_val_new (class, gtype, value);
    return retval;
}

/* Create a new Guile GEnumType class based on GType. Export all its
   constants into the current module. */
/* re pyg_enum_add */
SCM
gi_genum_add (const char *typename,
	      const char *strip_prefix,
	      GType gtype)
{
    SCM values;
    SCM stub;
    GEnumClass *eclass;
    
    g_return_val_if_fail (typename != NULL, SCM_BOOL_F);
    if (!g_type_is_a (gtype, G_TYPE_ENUM))
	scm_misc_error ("gi_genum_add",
			"Trying to register gtype '~S' as an enum when it is in face of type '~S'",
			scm_list_2 (scm_from_utf8_string (g_type_name (gtype)),
				    scm_from_utf8_string (g_type_name (G_TYPE_FUNDAMENTAL (gtype)))));

    /* Create a new type derived from GEnum. */
    stub = scm_make_foreign_object_0(gi_genumcollection_type);
    values = scm_c_make_hash_table(10);

    gi_genumcollection_set_gtype (stub, gtype);
    gi_genumcollection_set_enum_values (stub, values);

    g_type_set_qdata (gtype, gugenum_class_key, SCM_UNPACK_POINTER (stub));

    /* Register enum values */
    eclass = G_ENUM_CLASS (g_type_class_ref (gtype));
    for (guint i = 0; i < eclass->n_values; i ++) {
	SCM item, intval;
	char *prefix;
	
	intval = scm_from_long (eclass->values[i].value);
	item = gi_genum_val_new (stub, gtype, eclass->values[i].value);
	scm_hash_set_x (values, intval, item);
	prefix = g_strdup(gi_constant_strip_prefix (eclass->values[i].value_name, strip_prefix));
	scm_permanent_object (scm_c_define (prefix, item));
	g_free (prefix);
    }

    return stub;
}

/**
 * pyg_enum_get_value:
 * @enum_type: the GType of the flag.
 * @obj: a Python object representing the flag value
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Python object to the integer equivalent.  The conversion
 * will depend on the type of the Python object.  If the object is an
 * integer, it is passed through directly.  If it is a string, it will
 * be treated as a full or short enum name as defined in the GType.
 *
 * Returns: 0 on success or -1 on failure
 */
gint
gi_enum_get_value(GType enum_type, SCM obj, gint *val)
{
    GEnumClass *eclass = NULL;

    g_return_val_if_fail(val != NULL, -1);
    if (!obj) {
	*val = 0;
	return 0;
    } else if (scm_is_exact_integer (obj)) {
	*val = scm_to_int (obj);
	if (SCM_IS_A_P (obj, gi_genum_type) && gi_genum_get_gtype(obj) != enum_type) {
	    g_warning("expected enumeration type %s, but got %s instead",
		      g_type_name(enum_type),
		      g_type_name(gi_genum_get_gtype(obj)));
	    return -1;
	}
    } else if (scm_is_string (obj)) {
	GEnumValue *info;
	char *str = scm_to_utf8_string (obj);

	if (enum_type != G_TYPE_NONE)
	    eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));
	else {
	    scm_misc_error ("gi_enum_get_value",
			    "could not convert string '~S' to enum because there is no GType associated to look up the value",
			    scm_list_1 (obj));
	    return -1;
	}
	info = g_enum_get_value_by_name(eclass, str);
	g_type_class_unref(eclass);

	if (!info)
	    info = g_enum_get_value_by_nick(eclass, str);
	if (info) {
	    *val = info->value;
	    return 0;
	} else {
	    scm_misc_error ("gi_enum_get_value", "could not convert string", SCM_EOL);
	    return -1;
	}
    } else {
	scm_misc_error ("gi_enum_get_value", "enum values must by strings or ints", SCM_EOL);
	return -1;
    }
    return 0;
}


void
gi_init_genum(void)
{
  gi_init_genum_type();
  gi_init_genumcollection_type();
  gugenum_class_key = g_quark_from_static_string("guile-gi::genum");
}
