/* -*- Mode: C; c-basic-offset: 4 -*-
 * vim: tabstop=4 shiftwidth=4 expandtab
 *
 * Copyright (C) 2011 John (J5) Palmieri <johnp@redhat.com>
 * Copyright (C) 2014 Simon Feltman <sfeltman@gnome.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>
//#include "pygi-python-compat.h"
//#include "pygi-type.h"
#include "gi_basictype.h"
#include "gi_gtype.h"
//#include "pygi-argument.h"
//#include "pygi-util.h"
#include <girepository.h>

#if defined(G_OS_WIN32)
#include <float.h>
static gboolean
gi_isfinite (gdouble value) {
    return _finite (value);
}
#else
#include <math.h>
static gboolean
gi_isfinite (gdouble value) {
    return isfinite (value);
}
#endif

/* re pygi_gpointer_from_py */
static gpointer
scm_to_gpointer (SCM s_arg)
{
    if (scm_is_false (s_arg)) {
	return NULL;
    } else if (scm_is_exact_integer (s_arg)) {
	return (gpointer) scm_to_uintmax (s_arg);
    } else if (SCM_POINTER_P (s_arg)) {
	return scm_to_pointer (s_arg);
    } else if (scm_is_bytevector (s_arg) && SCM_BYTEVECTOR_LENGTH (s_arg) >= 8) {
	return SCM_BYTEVECTOR_CONTENTS (s_arg);
    } else {
	scm_misc_error (NULL,
			"cannot convert '~a' to gpointer",
			scm_list_1 (s_arg));
    }
}

static SCM
scm_from_gsize (gsize value)
{
    return scm_from_size_t (value);
}

static gsize
scm_to_gsize (SCM s_arg)
{
    return scm_to_size_t (s_arg);
}

static SCM
scm_from_gssize (gssize value)
{
    return scm_from_ssize_t (value);
}

static gdouble
scm_to_gdouble (SCM s_arg)
{
    return scm_to_double (s_arg);
}

static SCM
scm_from_gdouble (gdouble value)
{
    return scm_from_double (value);
}

static SCM
scm_from_gfloat (gfloat value)
{
    return scm_from_double (value);
}

static gfloat
scm_to_gfloat (SCM s_arg)
{
    double arg = scm_to_double (s_arg);
    if ( /*isfinite (arg) && */ (arg < -G_MAXFLOAT || arg > G_MAXFLOAT)) {
	scm_misc_error(NULL,
		       "cannot convert '~a' to gfloat, out of range",
		       scm_list_1 (s_arg));
    }
    return arg;
}

static gunichar
scm_to_gunichar(SCM x)
{
    if (scm_is_exact_integer (x)) {
	SCM c = scm_integer_to_char (x);
	return SCM_CHAR(c);
    } else if (SCM_CHARP(x)) {
	return SCM_CHAR(x);
    } else {
	scm_misc_error (NULL,
			"cannot convert '~a' to gunichar",
			scm_list_1 (x));
	g_return_val_if_reached (0);
    }
}

static SCM
scm_from_gunichar (gunichar value)
{
    SCM x = scm_from_uint32 (value);
    return scm_integer_to_char (x);
}

static GType
scm_to_gtype (SCM s_arg)
{
    return gi_gtype_from_scm (s_arg);
}

static gchar *
scm_to_utf8 (SCM s_arg)
{
    return scm_to_utf8_string (s_arg);
}

static gchar *
scm_to_filename (SCM s_arg)
{
    return scm_to_locale_string (s_arg);
}

static SCM
scm_from_filename (gchar *value)
{
    return scm_from_locale_string (value);
}

static SCM
scm_from_gboolean (gboolean value)
{
    return scm_from_bool (value);
}

static gboolean
scm_to_gboolean (SCM s_arg)
{
    return scm_to_bool (s_arg);
}


static const gpointer
scm_to_int32_pointer (SCM s_arg)
{
    if (!scm_is_bytevector (s_arg) || SCM_BYTEVECTOR_LENGTH (s_arg) < 4) {
	scm_misc_error (NULL,
			"cannot convert '~A' into an int32 pointer: bytevector expected",
			scm_list_1 (s_arg));
    }
    return SCM_BYTEVECTOR_CONTENTS (s_arg);
}


static gboolean
gi_gpointer_from_scm (SCM arg, gpointer *result)
{
    void* temp;

    if (scm_is_eq (arg, SCM_BOOL_F) || scm_is_eq (arg, SCM_EOL)) {
	*result = NULL;
	return TRUE;
    } else if (scm_is_exact_integer (arg)) {
	temp = (void *)scm_to_uintptr_t (arg);
	if (temp == NULL)
	    return FALSE;
	*result = temp;
	return TRUE;
    } else if (SCM_POINTER_P(arg)) {
	*result =  scm_to_pointer (arg);
	return TRUE;
    } else {
	scm_misc_error (NULL,
			"Cannot convert to pointer",
			SCM_EOL);
        return FALSE;
    }
}

#if 0
static gboolean
marshal_from_scm_void (PyGIInvokeState   *state,
		       PyGICallableCache *callable_cache,
		       PyGIArgCache      *arg_cache,
		       PyObject          *py_arg,
		       GIArgument        *arg,
		       gpointer          *cleanup_data)
{
    g_warn_if_fail (arg_cache->transfer == GI_TRANSFER_NOTHING);

    if (pygi_gpointer_from_py (py_arg, &(arg->v_pointer))) {
        *cleanup_data = arg->v_pointer;
        return TRUE;
    }

    return FALSE;
}
#endif

gboolean
gi_marshal_from_scm_basic_type (SCM object, /* in */
				GIArgument *arg,      /* out */
				GITypeTag   type_tag,
				gboolean is_ptr,
				GITransfer  transfer,
				gpointer   *cleanup_data /* out */)
{
    
    switch (type_tag) {
    case GI_TYPE_TAG_VOID:
	g_warn_if_fail (transfer == GI_TRANSFER_NOTHING);
	if (is_ptr) {
	    arg->v_pointer = scm_to_gpointer (object);
	    *cleanup_data = arg->v_pointer;
	    return TRUE;
	}
	return FALSE;

    case GI_TYPE_TAG_INT8:
	arg->v_int8 = scm_to_int8 (object);
	return TRUE;

    case GI_TYPE_TAG_UINT8:
	arg->v_uint8 = scm_to_uint8 (object);
	return TRUE;

    case GI_TYPE_TAG_INT16:
	arg->v_int16 = scm_to_int16 (object);
	return TRUE;

    case GI_TYPE_TAG_UINT16:
	arg->v_uint16 = scm_to_uint16 (object);
	return TRUE;

    case GI_TYPE_TAG_INT32:
	if (is_ptr)
	    arg->v_pointer = scm_to_int32_pointer (object);
	else
	    arg->v_int32 = scm_to_int32 (object);
	return TRUE;

    case GI_TYPE_TAG_UINT32:
	arg->v_uint32 = scm_to_uint32 (object);
	return TRUE;

    case GI_TYPE_TAG_INT64:
	arg->v_int64 = scm_to_int64 (object);
	return TRUE;

    case GI_TYPE_TAG_UINT64:
	arg->v_uint64 = scm_to_uint64 (object);
	return TRUE;

    case GI_TYPE_TAG_BOOLEAN:
	arg->v_boolean = scm_is_true (object);
	return TRUE;

    case GI_TYPE_TAG_FLOAT:
	arg->v_float = scm_to_gfloat (object);
	return TRUE;

    case GI_TYPE_TAG_DOUBLE:
	arg->v_double = scm_to_gdouble (object);
	return TRUE;

    case GI_TYPE_TAG_GTYPE:
	arg->v_size = scm_to_gsize (object);
	return TRUE;

    case GI_TYPE_TAG_UNICHAR:
	arg->v_uint32 = scm_to_gunichar (object);
	return TRUE;

    case GI_TYPE_TAG_UTF8:
	arg->v_string = scm_to_utf8 (object);
	*cleanup_data = arg->v_string;
	return TRUE;
	
    case GI_TYPE_TAG_FILENAME:
	arg->v_string = scm_to_filename (object);
	*cleanup_data = arg->v_string;
	return TRUE;
	
    default:
	scm_misc_error ("marshaling object",
			"type tag ~A not supported",
			scm_list_1 (scm_from_size_t (type_tag)));
	
	return FALSE;
    }
    
    return TRUE;
}

#if 0
static void
marshal_cleanup_from_py_utf8 (PyGIInvokeState *state,
			      PyGIArgCache    *arg_cache,
			      PyObject        *py_arg,
			      gpointer         data,
			      gboolean         was_processed)
{
    /* We strdup strings so free unless ownership is transferred to C. */
    if (was_processed && arg_cache->transfer == GI_TRANSFER_NOTHING)
	g_free (data);
}

static PyObject *
marshal_to_py_void (PyGIInvokeState   *state,
		    PyGICallableCache *callable_cache,
		    PyGIArgCache      *arg_cache,
		    GIArgument        *arg,
		    gpointer          *cleanup_data)
{
    if (arg_cache->is_pointer) {
	return PyLong_FromVoidPtr (arg->v_pointer);
    }
    Py_RETURN_NONE;
}

PyObject *
pygi_utf8_to_py (gchar *value)
{
    if (value == NULL) {
	Py_RETURN_NONE;
    }

    return PYGLIB_PyUnicode_FromString (value);
}

PyObject *
pygi_filename_to_py (gchar *value)
{
    PyObject *py_obj;

    if (value == NULL) {
	Py_RETURN_NONE;
    }

#if PY_VERSION_HEX < 0x03000000
    /* On PY2 we return str as is */
    py_obj = PyString_FromString (value);
#else
#ifdef G_OS_WIN32
    py_obj = PyUnicode_DecodeUTF8 (value, strlen(value),
				   "surrogatepass");
#else
    py_obj = PyUnicode_DecodeFSDefault (value);
#endif
#endif

    return py_obj;
}
#endif

/**
 * pygi_marshal_to_py_basic_type:
 * @arg: The argument to convert to an object.
 * @type_tag: Type tag for @arg
 * @transfer: Transfer annotation
 *
 * Convert the given argument to a Python object. This function
 * is restricted to simple types that only require the GITypeTag
 * and GITransfer. For a more complete conversion routine, use:
 * _pygi_argument_to_object.
 *
 * Returns: A PyObject representing @arg or NULL if it cannot convert
 *          the argument.
 */
SCM
gi_marshal_to_scm_basic_type (GIArgument  *arg,
			      GITypeTag type_tag,
			      GITransfer transfer)
{
    switch (type_tag) {
    case GI_TYPE_TAG_BOOLEAN:
	return scm_from_bool (arg->v_boolean);

    case GI_TYPE_TAG_INT8:
	return scm_from_int8 (arg->v_int8);

    case GI_TYPE_TAG_UINT8:
	return scm_from_uint8 (arg->v_uint8);

    case GI_TYPE_TAG_INT16:
	return scm_from_int16 (arg->v_int16);

    case GI_TYPE_TAG_UINT16:
	return scm_from_uint16 (arg->v_uint16);

    case GI_TYPE_TAG_INT32:
	return scm_from_int32(arg->v_int32);

    case GI_TYPE_TAG_UINT32:
	return scm_from_uint32(arg->v_uint32);

    case GI_TYPE_TAG_INT64:
	return scm_from_int64 (arg->v_int64);

    case GI_TYPE_TAG_UINT64:
	return scm_from_uint64 (arg->v_uint64);

    case GI_TYPE_TAG_FLOAT:
	return scm_from_double ((double) arg->v_float);

    case GI_TYPE_TAG_DOUBLE:
	return scm_from_double (arg->v_double);

    case GI_TYPE_TAG_GTYPE:
	return gi_gtype_c2g ((GType) arg->v_size);

    case GI_TYPE_TAG_UNICHAR:
	return SCM_MAKE_CHAR (arg->v_uint32);

    case GI_TYPE_TAG_UTF8:
	if (!arg->v_string)
	    return scm_c_make_string (0, SCM_MAKE_CHAR(0));
	else
	    return scm_from_utf8_string (arg->v_string);

    case GI_TYPE_TAG_FILENAME:
	if (!arg->v_string)
	    return scm_c_make_string (0, SCM_MAKE_CHAR(0));
	else
	    return scm_from_locale_string (arg->v_string);

    default:
	scm_misc_error ("argument marshal",
			"Type tag %d not supported",
			scm_list_1(scm_from_size_t(type_tag)));
	return SCM_BOOL_F;
    }
}

#if 0
PyObject *
pygi_marshal_to_py_basic_type_cache_adapter (PyGIInvokeState   *state,
					     PyGICallableCache *callable_cache,
					     PyGIArgCache      *arg_cache,
					     GIArgument        *arg,
					     gpointer          *cleanup_data)
{
    return pygi_marshal_to_py_basic_type (arg,
					  arg_cache->type_tag,
					  arg_cache->transfer);
}

static void
marshal_cleanup_to_py_utf8 (PyGIInvokeState *state,
			    PyGIArgCache    *arg_cache,
			    gpointer         cleanup_data,
			    gpointer         data,
			    gboolean         was_processed)
{
    /* Python copies the string so we need to free it
       if the interface is transfering ownership, 
       whether or not it has been processed yet */
    if (arg_cache->transfer == GI_TRANSFER_EVERYTHING)
	g_free (data);
}

static gboolean
arg_basic_type_setup_from_info (PyGIArgCache  *arg_cache,
				GITypeInfo    *type_info,
				GIArgInfo     *arg_info,
				GITransfer     transfer,
				PyGIDirection  direction)
{
    GITypeTag type_tag = g_type_info_get_tag (type_info);

    if (!pygi_arg_base_setup (arg_cache, type_info, arg_info, transfer, direction))
	return FALSE;

    switch (type_tag) {
    case GI_TYPE_TAG_VOID:
	if (direction & PYGI_DIRECTION_FROM_PYTHON)
	    arg_cache->from_py_marshaller = marshal_from_py_void;

	if (direction & PYGI_DIRECTION_TO_PYTHON)
	    arg_cache->to_py_marshaller = marshal_to_py_void;

	break;
    case GI_TYPE_TAG_BOOLEAN:
    case GI_TYPE_TAG_INT8:
    case GI_TYPE_TAG_UINT8:
    case GI_TYPE_TAG_INT16:
    case GI_TYPE_TAG_UINT16:
    case GI_TYPE_TAG_INT32:
    case GI_TYPE_TAG_UINT32:
    case GI_TYPE_TAG_INT64:
    case GI_TYPE_TAG_UINT64:
    case GI_TYPE_TAG_FLOAT:
    case GI_TYPE_TAG_DOUBLE:
    case GI_TYPE_TAG_UNICHAR:
    case GI_TYPE_TAG_GTYPE:
	if (direction & PYGI_DIRECTION_FROM_PYTHON)
	    arg_cache->from_py_marshaller = pygi_marshal_from_py_basic_type_cache_adapter;

	if (direction & PYGI_DIRECTION_TO_PYTHON)
	    arg_cache->to_py_marshaller = pygi_marshal_to_py_basic_type_cache_adapter;

	break;
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
	if (direction & PYGI_DIRECTION_FROM_PYTHON) {
	    arg_cache->from_py_marshaller = pygi_marshal_from_py_basic_type_cache_adapter;
	    arg_cache->from_py_cleanup = marshal_cleanup_from_py_utf8;
	}

	if (direction & PYGI_DIRECTION_TO_PYTHON) {
	    arg_cache->to_py_marshaller = pygi_marshal_to_py_basic_type_cache_adapter;
	    arg_cache->to_py_cleanup = marshal_cleanup_to_py_utf8;
	}

	break;
    default:
	g_assert_not_reached ();
    }

    return TRUE;
}

PyGIArgCache *
pygi_arg_basic_type_new_from_info (GITypeInfo   *type_info,
				   GIArgInfo    *arg_info,
				   GITransfer    transfer,
				   PyGIDirection direction)
{
    gboolean res = FALSE;
    PyGIArgCache *arg_cache = pygi_arg_cache_alloc ();

    res = arg_basic_type_setup_from_info (arg_cache,
					  type_info,
					  arg_info,
					  transfer,
					  direction);
    if (res) {
	return arg_cache;
    } else {
	pygi_arg_cache_free (arg_cache);
	return NULL;
    }
}
#endif
