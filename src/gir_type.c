#include <libguile.h>
#include <glib-object.h>
#include "gir_type.h"

static GQuark gir_type_marshal_key = 0;
static GQuark gir_type_marshal_helper_key = 0;

static SCM gir_NONE_type;
SCM gir_NONE;

/**
 * pyg_register_gtype_custom:
 * @gtype: the GType for the new type
 * @from_func: a function to convert GValues to Python objects
 * @to_func: a function to convert Python objects to GValues
 *
 * In order to handle specific conversion of gboxed types or new
 * fundamental types, you may use this function to register conversion
 * handlers.
 */

void
gir_register_gtype_custom(GType gtype,
			  fromvaluefunc from_func,
                          tovaluefunc to_func)
{
    GirGTypeMarshal *tm;

    if (!gir_type_marshal_key) {
	gir_type_marshal_key = g_quark_from_static_string("GirGType::marshal");
	gir_type_marshal_helper_key = g_quark_from_static_string("GirGType::marshal-helper");
    }

    tm = g_new(GirGTypeMarshal, 1);
    tm->fromvalue = from_func;
    tm->tovalue = to_func;
    g_type_set_qdata(gtype, gir_type_marshal_key, tm);
}

void
gir_register_NONE_sigil ()
{
  gir_NONE_type = scm_make_foreign_object_type (scm_from_utf8_symbol ("NONE"), SCM_EOL, NULL);
  gir_NONE = scm_make_foreign_object_0 (gir_NONE_type);
}
