#include <libguile.h>
#include <glib-object.h>
#include "gugi-type.h"
#include "gugi-basictype.h"
#include "gugi-guile-compat.h"

static GQuark gir_type_marshal_key = 0;
static GQuark gir_type_marshal_helper_key = 0;

static SCM gir_NONE_type;
SCM gir_NONE;

/**
 * gug_enum_get_value:
 * @enum_type: the GType of the flag.
 * @obj: a Guile object representing the flag value
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Guile object to the integer equivalent.  The conversion
 * will depend on the type of the Guile object.  If the object is an
 * integer, it is passed through directly.  If it is a string, it will
 * be treated as a full or short enum name as defined in the GType.
 *
 * Returns: 0 on success or -1 on failure
 */
gint
gug_enum_get_value(GType enum_type, SCM obj, gint *val)
{
    GEnumClass *eclass = NULL;
    gint res = -1;

    g_return_val_if_fail(val != NULL, -1);
    if (!obj) {
	*val = 0;
	res = 0;
    } else if (GUGLIB_PyLong_Check(obj)) {
	if (!gugi_gint_from_py (obj, val))
	    res = -1;
	else
	    res = 0;

	if (PyObject_TypeCheck(obj, &PyGEnum_Type) && ((PyGEnum *) obj)->gtype != enum_type) {
	    g_warning("expected enumeration type %s, but got %s instead",
		      g_type_name(enum_type),
		      g_type_name(((PyGEnum *) obj)->gtype));
	}
    /* Dumb code duplication, but probably not worth it to have yet another macro. */
    } else if (PyLong_Check(obj)) {
	if (!pygi_gint_from_py (obj, val))
	    res = -1;
	else
	    res = 0;

	if (PyObject_TypeCheck(obj, &PyGEnum_Type) && ((PyGEnum *) obj)->gtype != enum_type) {
	    g_warning("expected enumeration type %s, but got %s instead",
		      g_type_name(enum_type),
		      g_type_name(((PyGEnum *) obj)->gtype));
	}
    } else if (PYGLIB_PyUnicode_Check(obj)) {
	GEnumValue *info;
	char *str = PYGLIB_PyUnicode_AsString(obj);

	if (enum_type != G_TYPE_NONE)
	    eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));
	else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string to enum because there is no GType associated to look up the value");
	    res = -1;
	}
	info = g_enum_get_value_by_name(eclass, str);
	g_type_class_unref(eclass);

	if (!info)
	    info = g_enum_get_value_by_nick(eclass, str);
	if (info) {
	    *val = info->value;
	    res = 0;
	} else {
	    PyErr_SetString(PyExc_TypeError, "could not convert string");
	    res = -1;
	}
    } else {
	PyErr_SetString(PyExc_TypeError,"enum values must be strings or ints");
	res = -1;
    }
    return res;
}

/**
 * gug_register_gtype_custom:
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
    GuGTypeMarshal *tm;

    if (!gug_type_marshal_key) {
	gug_type_marshal_key = g_quark_from_static_string("GirGType::marshal");
	gug_type_marshal_helper_key = g_quark_from_static_string("GirGType::marshal-helper");
    }

    tm = g_new(GuGTypeMarshal, 1);
    tm->fromvalue = from_func;
    tm->tovalue = to_func;
    g_type_set_qdata(gtype, gir_type_marshal_key, tm);
}
