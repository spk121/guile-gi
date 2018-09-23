/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_genum.h"

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
    gi_genum_set_value (intval);
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
    SCM intval;
    SCM retval;
    
    g_return_val_if_fail (gtype != G_TYPE_INVALID, SCM_BOOL_F);

    /* Get a wrapper class by:
     * 1. check for one attached to gtype
     * 2. looking up one in a typelib
     * 3. creating a new one
     */
    ptr = g_type_get_qdata (gtype, gugenum_class_key);
    if (!ptr)
	ptr = gi_gtype_import_by_g_type (gtype);
    if (ptr)
	class = SCM_UNPACK_POINTER (ptr);
    else
	class = gi_genum_add (g_type_name (gtype), NULL, gtype);

    values = gi_genumcollection_get_enum_values (class);
    retval = scm_hash_ref (values, scm_from_int (value));
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
			scm_from_utf8_string (g_type_name (gtype)),
			scm_from_utf8_string (g_type_name (G_TYPE_FUNDAMENTAL (gtype))));

    /* Create a new type derived from GEnum. */
    instance_hash = scm_c_make_hash_table (10);
    stub = scm_make_foreign_object_0(gi_genumcollection_type);
    values = scm_c_make_hash_table(10);

    gi_genumcollection_set_gtype (stub, gtype);
    gi_genumcollection_set_enum_values (stub, values);

    g_type_set_qdata (gtype, gugenum_class_key, SCM_UNPACK_POINTER (stub));

    /* Register enum values */
    eclass = G_ENUM_CLASS (g_type_class_ref (gtype));
    for (i = 0; i < eclass->n_values; i ++) {
	SCM item, intval;
	intval = scm_from_long (eclass->values[i].value);
	item = gi_genum_val_new (stub, gtype, intval);
	scm_hash_set_x (values, intval, item);
	char *prefix = g_strdup(constant_strip_prefix (eclass->values[i].value_name, strip_prefix));
	scm_permanent_object (scm_c_define (prefix, item));
	g_free (prefix);
    }

    return stub;
}


void
gi_init_genum()
{
  gi_init_genum_type();
  gugenum_class_key = g_quark_from_static_string("guile-gi::genum");
}
