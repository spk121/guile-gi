/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gir_g_type.h"
#include "gir_g_value.h"
#include "gir_xguile.h"

SCM GuGValue_Type;
SCM GuGValue_Type_Store;

////////////////////////////////////////////////////////////////
// GuGValue_Type : A foreign object type that is an envelope for a
//   a C GValue

// GuGValue Instance: A foreign object with the following slots
//  - slot 0: 'value', a C GValue pointer
//

#define MAKE_GVALUE_TYPE						\
    do {								\
	GuGValue_Type =							\
	    scm_make_foreign_object_type(scm_from_latin1_symbol("<GValue>"), \
					 scm_list_n (scm_from_latin1_symbol("value"), \
						     SCM_UNDEFINED),	\
					 NULL);				\
    } while(FALSE)

#define GVALUE_VALUE_SLOT 0

static GValue *
gu_g_value_get_value (SCM gvalue)
{
    GValue *value;
  
    scm_assert_foreign_object_type (GuGValue_Type, gvalue);
    value = (GValue *)scm_foreign_object_ref (gvalue, GVALUE_VALUE_SLOT);
    return value;
}

static SCM
gir_g_value_holds_p (SCM gvalue, SCM gtype)
{
    GValue *value;
    GType type;
    gboolean ret;
    scm_assert_foreign_object_type (GuGValue_Type, gvalue);
    scm_assert_foreign_object_type (GuGType_Type, gtype);

    value = gu_g_value_get_value(gvalue);
    type = GType_get_type(gtype);
    ret = G_VALUE_HOLDS(value, type);
    return scm_from_bool(ret);
}

/**
 * pyg_value_from_pyobject_with_error:
 * @value: the GValue object to store the converted value in.
 * @obj: the Python object to convert.
 *
 * This function converts a Python object and stores the result in a
 * GValue.  The GValue must be initialised in advance with
 * g_value_init().  If the Python object can't be converted to the
 * type of the GValue, then an error is returned.
 *
 * Returns: 0 on success, -1 on error.
 */
static int
gug_value_from_scm_with_error(GValue *value, SCM obj)
{
    GType value_type = G_VALUE_TYPE(value);

    switch (G_TYPE_FUNDAMENTAL(value_type)) {
    case G_TYPE_INTERFACE:
	g_return_val_if_reached(-1);
#if 0
        /* we only handle interface types that have a GObject prereq */
        if (g_type_is_a(value_type, G_TYPE_OBJECT)) {
            if (obj == SCM_NONE)
                g_value_set_object(value, NULL);
            else {
                if (!PyObject_TypeCheck(obj, &PyGObject_Type)) {
                    PyErr_SetString(PyExc_TypeError, "GObject is required");
                    return -1;
                }
                if (!G_TYPE_CHECK_INSTANCE_TYPE(pygobject_get(obj),
						value_type)) {
                    PyErr_SetString(PyExc_TypeError, "Invalid GObject type for assignment");
                    return -1;
                }
                g_value_set_object(value, pygobject_get(obj));
            }
        } else {
            PyErr_SetString(PyExc_TypeError, "Unsupported conversion");
            return -1;
        }
#endif
        break;
    case G_TYPE_CHAR:
	{
	    gint8 temp = scm_to_int8(obj);
	    g_value_set_schar (value, temp);
	    return 0;
	}
    case G_TYPE_UCHAR:
	{
	    guchar temp = scm_to_uint8(obj);
	    g_value_set_uchar(value, temp);
	    return 0;
	}
    case G_TYPE_BOOLEAN:
	{
	    gboolean temp = scm_is_true(obj);
	    g_value_set_boolean (value, temp);
	    return 0;
	}
    case G_TYPE_INT:
	{
	    gint temp = scm_to_int(obj);
	    g_value_set_int (value, temp);
	    return 0;
	}
    case G_TYPE_UINT:
	{
	    guint temp = scm_to_uint(obj);
	    g_value_set_uint (value, temp);
	    return 0;
	}
    case G_TYPE_LONG:
	{
	    glong temp = scm_to_long(obj);
	    g_value_set_long (value, temp);
	    return 0;
	}
    case G_TYPE_ULONG:
	{
	    gulong temp = scm_to_ulong(obj);
	    g_value_set_ulong (value, temp);
	    return 0;
	}
    case G_TYPE_INT64:
	{
	    gint64 temp = scm_to_int64(obj);
	    g_value_set_int64 (value, temp);
	    return 0;
	}
    case G_TYPE_UINT64:
	{
	    guint64 temp = scm_to_uint64(obj);
	    return 0;
	}
    case G_TYPE_ENUM:
	{
	    g_return_val_if_reached(-1);
#if 0
	    gint val = 0;
	    if (pyg_enum_get_value(G_VALUE_TYPE(value), obj, &val) < 0) {
		return -1;
	    }
	    g_value_set_enum(value, val);
#endif
	}
	break;
    case G_TYPE_FLAGS:
	{
	    g_return_val_if_reached(-1);
#if 0
	    guint val = 0;
	    if (pyg_flags_get_value(G_VALUE_TYPE(value), obj, &val) < 0) {
		return -1;
	    }
	    g_value_set_flags(value, val);
	    return 0;
#endif
	}
	break;
    case G_TYPE_FLOAT:
	{
	    gfloat temp = scm_to_double(obj);
	    g_value_set_float (value, temp);
	    return 0;
	}
    case G_TYPE_DOUBLE:
	{
	    gdouble temp = scm_to_double(obj);
	    g_value_set_double (value, temp);
	    return 0;
	}
    case G_TYPE_STRING:
	{
	    gchar *temp = scm_to_utf8_string(obj);
	    g_value_take_string (value, temp);
	    return 0;
#if 0	
	    {
		/* also allows setting anything implementing __str__ */
		PyObject* str;
		PyErr_Clear ();
		str = PyObject_Str (obj);
		if (str == NULL)
		    return -1;
		if (pygi_utf8_from_py (str, &temp)) {
		    Py_DECREF (str);
		    g_value_take_string (value, temp);
		    return 0;
		}
		Py_DECREF (str);
		return -1;
	    }
#endif
	}
    case G_TYPE_POINTER:
	g_return_val_if_reached(-1);
#if 0
        if (obj == SCM_NONE)
            g_value_set_pointer(value, NULL);
        else if (PyObject_TypeCheck(obj, &PyGPointer_Type) &&
		 G_VALUE_HOLDS(value, ((PyGPointer *)obj)->gtype))
            g_value_set_pointer(value, pyg_pointer_get(obj, gpointer));
        else if (PyCapsule_CheckExact (obj))
            g_value_set_pointer(value, PyCapsule_GetPointer (obj, NULL));
        else if (G_VALUE_HOLDS_GTYPE (value))
            g_value_set_gtype (value, pyg_type_from_object (obj));
        else {
            PyErr_SetString(PyExc_TypeError, "Expected pointer");
            return -1;
        }
#endif
        break;
    case G_TYPE_BOXED: {
	g_return_val_if_reached(-1);
#if 0
	PyGTypeMarshal *bm;
	gboolean holds_value_array;
	
	G_GNUC_BEGIN_IGNORE_DEPRECATIONS
	    holds_value_array = G_VALUE_HOLDS(value, G_TYPE_VALUE_ARRAY);
	G_GNUC_END_IGNORE_DEPRECATIONS
	    
	    if (obj == SCM_NONE)
		g_value_set_boxed(value, NULL);
	    else if (G_VALUE_HOLDS(value, PY_TYPE_OBJECT))
		g_value_set_boxed(value, obj);
	    else if (PyObject_TypeCheck(obj, &PyGBoxed_Type) &&
		     G_VALUE_HOLDS(value, ((PyGBoxed *)obj)->gtype))
		g_value_set_boxed(value, pyg_boxed_get(obj, gpointer));
	    else if (G_VALUE_HOLDS(value, G_TYPE_VALUE)) {
		GType type;
		GValue *n_value;
		
		type = pyg_type_from_object((PyObject*)Py_TYPE(obj));
		if (G_UNLIKELY (! type)) {
		    return -1;
		}
		n_value = g_new0 (GValue, 1);
		g_value_init (n_value, type);
		g_value_take_boxed (value, n_value);
		return pyg_value_from_pyobject_with_error (n_value, obj);
	    }
	    else if (PySequence_Check(obj) && holds_value_array)
		return pyg_value_array_from_pyobject(value, obj, NULL);

	    else if (PySequence_Check(obj) &&
		     G_VALUE_HOLDS(value, G_TYPE_ARRAY))
		return pyg_array_from_pyobject(value, obj);
	    else if (PYGLIB_PyUnicode_Check(obj) &&
		     G_VALUE_HOLDS(value, G_TYPE_GSTRING)) {
		GString *string;
		char *buffer;
		Py_ssize_t len;
		if (PYGLIB_PyUnicode_AsStringAndSize(obj, &buffer, &len))
		    return -1;
		string = g_string_new_len(buffer, len);
		g_value_set_boxed(value, string);
		g_string_free (string, TRUE);
		break;
	    }
	    else if ((bm = pyg_type_lookup(G_VALUE_TYPE(value))) != NULL)
		return bm->tovalue(value, obj);
	    else if (PyCapsule_CheckExact (obj))
		g_value_set_boxed(value, PyCapsule_GetPointer (obj, NULL));
	    else {
		PyErr_SetString(PyExc_TypeError, "Expected Boxed");
		return -1;
	    }
	break;
#endif
    }
    case G_TYPE_PARAM:
	g_return_val_if_reached(-1);
#if 0
	/* we need to support both the wrapped _gi.GParamSpec and the GI
	 * GObject.ParamSpec */
	if (G_IS_PARAM_SPEC (pygobject_get (obj)))
	    g_value_set_param(value, G_PARAM_SPEC (pygobject_get (obj)));
	else if (pyg_param_spec_check (obj))
	    g_value_set_param(value, PyCapsule_GetPointer (obj, NULL));
	else {
	    PyErr_SetString(PyExc_TypeError, "Expected ParamSpec");
	    return -1;
	}
#endif
	break;
    case G_TYPE_OBJECT:
	g_return_val_if_reached(-1);
#if 0
	if (obj == SCM_NONE) {
	    g_value_set_object(value, NULL);
	} else if (PyObject_TypeCheck(obj, &PyGObject_Type) &&
		   G_TYPE_CHECK_INSTANCE_TYPE(pygobject_get(obj),
					      G_VALUE_TYPE(value))) {
	    g_value_set_object(value, pygobject_get(obj));
	} else {
	    PyErr_SetString(PyExc_TypeError, "Expected GObject");
	    return -1;
	}
#endif
	break;
    case G_TYPE_VARIANT:
	{
	    g_return_val_if_reached(-1);
#if 0
	    if (obj == SCM_NONE)
		g_value_set_variant(value, NULL);
	    else if (pyg_type_from_object_strict(obj, FALSE) == G_TYPE_VARIANT)
		g_value_set_variant(value, pyg_boxed_get(obj, GVariant));
	    else {
		PyErr_SetString(PyExc_TypeError, "Expected Variant");
		return -1;
	    }
#endif
	    break;
	}
    default:
	{
	    g_return_val_if_reached(-1);
#if 0
	    PyGTypeMarshal *bm;
	    if ((bm = pyg_type_lookup(G_VALUE_TYPE(value))) != NULL) {
		return bm->tovalue(value, obj);
	    } else {
		PyErr_SetString(PyExc_TypeError, "Unknown value type");
		return -1;
	    }
#endif
	    break;
	}
    }

#if 0
    /* If an error occurred, unset the GValue but don't clear the Python error. */
    if (PyErr_Occurred()) {
	g_value_unset(value);
	return -1;
    }
#endif

    return 0;
}

#if 0
/**
 * pyg_value_from_pyobject:
 * @value: the GValue object to store the converted value in.
 * @obj: the Python object to convert.
 *
 * Same basic function as pyg_value_from_pyobject_with_error but clears
 * any Python errors before returning.
 *
 * Returns: 0 on success, -1 on error.
 */
int
pyg_value_from_pyobject(GValue *value, PyObject *obj)
{
    int res = pyg_value_from_pyobject_with_error (value, obj);

    if (PyErr_Occurred()) {
        PyErr_Clear();
        return -1;
    }
    return res;
}
#endif

static SCM
gir_make_GValue (SCM gobject, SCM gtype)
{
    GType type;
    GValue empty = G_VALUE_INIT;
    GValue *val;
    
    scm_assert_foreign_object_type (GuGType_Type, gtype);
    val = (GValue *) scm_gc_malloc (sizeof (GValue), "<GValue>");

    /* GLib require that the copy needs to be properly initialized,
       even though it is just going to be overwriten. */
    memcpy(val, &empty, sizeof(GValue));
	   
    type = GType_get_type (gtype);
    g_value_init(val, type);
    
    int ret = gug_value_from_scm_with_error(val, gobject);
    return scm_make_foreign_object_1 (GuGValue_Type, val);
}


/**
 * pygi_value_to_py_basic_type:
 * @value: the GValue object.
 * @handled: (out): TRUE if the return value is defined
 *
 * This function creates/returns a Python wrapper object that
 * represents the GValue passed as an argument limited to supporting basic types
 * like ints, bools, and strings.
 *
 * Returns: a PyObject representing the value.
 */
SCM
gir_value_to_scm_basic_type (const GValue *value, GType fundamental, gboolean *handled)
{
    *handled = TRUE;
    switch (fundamental) {
        case G_TYPE_CHAR:
	    return scm_from_char (g_value_get_schar (value));
        case G_TYPE_UCHAR:
	    return scm_from_uchar (g_value_get_uchar (value));
        case G_TYPE_BOOLEAN:
	    return scm_from_bool (g_value_get_boolean (value));
        case G_TYPE_INT:
	    return scm_from_int (g_value_get_int (value));
        case G_TYPE_UINT:
	    return scm_from_uint (g_value_get_uint (value));
        case G_TYPE_LONG:
	    return scm_from_long (g_value_get_long (value));
        case G_TYPE_ULONG:
	    return scm_from_ulong (g_value_get_ulong (value));
        case G_TYPE_INT64:
	    return scm_from_int64 (g_value_get_int64(value));
        case G_TYPE_UINT64:
	    return scm_from_uint64 (g_value_get_uint64(value));
        case G_TYPE_ENUM:
	    g_return_val_if_reached (SCM_BOOL_F);
            /* return pyg_enum_from_gtype (G_VALUE_TYPE (value), */
            /*                             g_value_get_enum (value)); */
        case G_TYPE_FLAGS:
	    g_return_val_if_reached (SCM_BOOL_F);
            /* return pyg_flags_from_gtype (G_VALUE_TYPE (value), */
            /*                              g_value_get_flags (value)); */
        case G_TYPE_FLOAT:
	    return scm_from_double (g_value_get_float (value));
        case G_TYPE_DOUBLE:
	    return scm_from_double (g_value_get_double (value));
        case G_TYPE_STRING:
	    return scm_from_utf8_string (g_value_get_string (value));
        default:
            *handled = FALSE;
            return NULL;
    }
}


/* Create an SCM from the value stored in a <GValue>.  If COPY_BOXED
   is true, deep-copy values where appropriate. */
static SCM
gir_unpack_GValue (SCM gvalue, SCM copy_boxed)
{
    SCM obj;
    GValue *value;
    GType fundamental;
    gboolean handled;
    
    scm_assert_foreign_object_type (GuGValue_Type, gvalue);
    SCM_ASSERT_TYPE(scm_is_bool(copy_boxed), copy_boxed, SCM_ARG2, "unpack-GValue", "boolean");
    value = gu_g_value_get_value (gvalue);
    fundamental = G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value));
    if (fundamental == G_TYPE_CHAR) {
	gint8 val = g_value_get_schar(value);
	return SCM_MAKE_CHAR(val);
    } else if (fundamental == G_TYPE_UCHAR) {
	guint8 val = g_value_get_uchar(value);
	return SCM_MAKE_CHAR(val);
    }

    obj = gir_value_to_scm_basic_type (value, fundamental, &handled);
    if (handled)
	return obj;
#if 0
    obj = gir_value_to_scm_structured_type (value, fundamental, scm_to_bool (copy_boxed));
    return obj;
#endif
    g_return_val_if_reached (SCM_BOOL_F);
}

void
gir_init_g_value (void)
{
    MAKE_GVALUE_TYPE;
    GuGValue_Type_Store = scm_c_define ("<GValue>", GuGValue_Type);
    scm_c_define_gsubr("GValue-holds?", 2, 0, 0, gir_g_value_holds_p);
    scm_c_define_gsubr("make-GValue", 2, 0, 0, gir_make_GValue);
    scm_c_define_gsubr("unpack-GValue", 2, 0, 0, gir_unpack_GValue);
}
