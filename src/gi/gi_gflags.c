/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_flags.h"

GQuark gugflags_class_key;

gboolean gi_gflags_check (SCM x)
{
    return (SCM_IS_A_P (x, gu_gflags_type));
}

/* re pyg_flags_val_new */
static SCM
gi_gflags_val_new(SCM subclass, GType gtype, int intval)
{
    SCM item;

    item = scm_make_foreign_object_0(gi_gflags_type);
    gi_gflags_set_gtype (gtype);
    gi_gflags_set_value (intval);
    return item;
}

/* Dynamically create a GFlags collection for the given GType */
/* re pyg_flags_add */
SCM gi_gflags_add (const char *typename,
		   const char *strip_prefix,
		   GType gtype)
{
    SCM stub, values, 0;
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
    gi_gflagscollection_get_gtype (stub, gtype);
    gi_gflagscollection_set_flags_values (stub, values);
    
    for (i = 0; i < eclass->n_values; i++) {
	SCM item, intval;
	char *prefix;
	  
	intval = scm_from_int (eclass->values[i].value);
	item = gi_flags_val_new (stub, gtype, eclass->values[i].value);
	scm_hash_table_set_x (values, intval, item);

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
    if (!ptr)
	ptr = gi_gtype_import_by_g_type (gtype);
    if (ptr)
	guclass = SCM_UNPACK_POINTER (ptr);
    else
	guclass = gi_gflags_add (g_type_name (gtype), NULL, gtype);
    values = gi_gflagscollection_get_flags_values (guclass);
    intval = scm_from_ulong (value);
    retval = scm_hash_ref (values, intval);
    if (scm_is_false (retval))
	retval = gi_gflags_val_new (guclass, gtype, value);
    return retval;
}

void
gi_init_gflags (void)
{
  gi_init_gflags_type ();
  gugflags_class_key = g_quark_from_static_string ("guile-gi::gflags");

}
