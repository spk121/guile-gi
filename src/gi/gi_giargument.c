/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_giargument.h"
#include <libguile.h>
#include <math.h>
#include "gi_basictype.h"
#include "gi_gvalue.h"
#include "gir_func2.h"
#include "gi_gobject.h"
/*
 * vim: tabstop=4 shiftwidth=4 expandtab
 *
 * Copyright (C) 2005-2009 Johan Dahlin <johan@gnome.org>
 *
 *   pygi-argument.c: GIArgument - PyObject conversion functions.
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

static const intmax_t intmin[GI_TYPE_TAG_N_TYPES] =
    {[GI_TYPE_TAG_INT8] = INT8_MIN,
     [GI_TYPE_TAG_INT16] = INT16_MIN,
     [GI_TYPE_TAG_INT32] = INT32_MIN,
     [GI_TYPE_TAG_INT64] = INT64_MIN};

static const intmax_t intmax[GI_TYPE_TAG_N_TYPES] =
    {[GI_TYPE_TAG_INT8] = INT8_MAX,
     [GI_TYPE_TAG_INT16] = INT16_MAX,
     [GI_TYPE_TAG_INT32] = INT32_MAX,
     [GI_TYPE_TAG_INT64] = INT64_MAX};

static const uintmax_t uintmax[GI_TYPE_TAG_N_TYPES] =
    {[GI_TYPE_TAG_UINT8] = UINT8_MAX,
     [GI_TYPE_TAG_UINT16] = UINT16_MAX,
     [GI_TYPE_TAG_UINT32] = UINT32_MAX,
     [GI_TYPE_TAG_UINT64] = UINT64_MAX,
     [GI_TYPE_TAG_UNICHAR] = 0x10FFFF};

#define TYPE_TAG_IS_EXACT_INTEGER(x)		\
    ((x) == GI_TYPE_TAG_INT8			\
     || (x) == GI_TYPE_TAG_UINT8		\
     || (x) == GI_TYPE_TAG_INT16		\
     || (x) == GI_TYPE_TAG_UINT16		\
     || (x) == GI_TYPE_TAG_INT32		\
     || (x) == GI_TYPE_TAG_UINT32		\
     || (x) == GI_TYPE_TAG_INT64		\
     || (x) == GI_TYPE_TAG_UINT64)

#define TYPE_TAG_IS_SIGNED_INTEGER(x)		\
    ((x) == GI_TYPE_TAG_INT8			\
     || (x) == GI_TYPE_TAG_INT16		\
     || (x) == GI_TYPE_TAG_INT32		\
     || (x) == GI_TYPE_TAG_INT64)

#define TYPE_TAG_IS_REAL_NUMBER(x)		\
    ((x) == GI_TYPE_TAG_FLOAT			\
     || (x) == GI_TYPE_TAG_DOUBLE)


gboolean
gi_giargument_check_scm_type(SCM obj, GIArgInfo *ai, char **errstr)
{
    GITypeInfo *ti = g_arg_info_get_type (ai);
    GITransfer transfer = g_arg_info_get_ownership_transfer (ai);
    GIDirection dir = g_arg_info_get_direction (ai);
    GITypeTag type_tag = g_type_info_get_tag (ti);
    gboolean is_ptr = g_type_info_is_pointer (ti);
    gboolean ok;

    g_assert (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT);

    if (!is_ptr) {
	if (TYPE_TAG_IS_EXACT_INTEGER(type_tag)) {
	    if (!scm_is_exact_integer (obj)) {
		*errstr = g_strdup_printf ("expected exact integer");
		ok = FALSE;
	    } else {
		if (TYPE_TAG_IS_SIGNED_INTEGER (type_tag)) {
		    intmax_t val = scm_to_intmax (obj);
		    if (val < intmin[type_tag] || val > intmax[type_tag]) {
			*errstr = g_strdup_printf ("integer out of range");
			ok = FALSE;
		    } else
			ok = TRUE;
		} else {
		    uintmax_t val = scm_to_uintmax (obj);
		    if (val > uintmax[type_tag]) {
			*errstr = g_strdup_printf ("unsigned integer out of range");
			ok = FALSE;
		    } else
			ok = TRUE;
		}
	    }
	} else if (TYPE_TAG_IS_REAL_NUMBER (type_tag)) {
	    if (!scm_is_real (obj)) {
		*errstr = g_strdup_printf ("expected real number");
		ok = FALSE;
	    } else {
		// FIXME, if you really wanted to, you could make a scheme integer
		// bigger than DBL_MAX, so this would throw.
		double val = scm_to_double (obj);
		if (!isfinite (val)) {
		    *errstr = g_strdup_printf ("real number is infinite");
		    ok = FALSE;
		} else if (type_tag == GI_TYPE_TAG_FLOAT) {
		    if (val < -FLT_MAX || val > FLT_MAX) {
			*errstr = g_strdup_printf ("real number out of range");
			ok = FALSE;
		    } else
			ok = TRUE;
		} else
		    ok = TRUE;
	    }
	} else if (type_tag == GI_TYPE_TAG_BOOLEAN) {
	    if (obj != SCM_BOOL_F && obj != SCM_BOOL_T) {
		*errstr = g_strdup_printf ("expected boolean");
		ok = FALSE;
	    } else
		ok = TRUE;
	} else {
	    *errstr = g_strdup_printf ("unhandled type %u", type_tag);
	    ok = FALSE;
	}
    } else /* is_ptr */ {
	if (TYPE_TAG_IS_EXACT_INTEGER (type_tag) || TYPE_TAG_IS_REAL_NUMBER (type_tag)
	    || type_tag == GI_TYPE_TAG_UTF8 || type_tag == GI_TYPE_TAG_FILENAME || type_tag == GI_TYPE_TAG_VOID) {
	    if (!scm_is_bytevector (obj) && !scm_is_string (obj)) {
		*errstr = g_strdup_printf ("expected bytevector or string");
		ok = FALSE;
	    } else
		ok = TRUE;
	} else if (type_tag == GI_TYPE_TAG_INTERFACE) {
	    ok = TRUE;
	} else {
	    *errstr = g_strdup_printf ("unhandled pointer type %u", type_tag);
	    ok = FALSE;
	}
    }
    return ok;
}

gboolean
gi_giargument_to_gssize (const char *func,
			 GIArgument *arg_in,
                         GITypeTag  type_tag,
                         gssize *gssize_out)
{
    const gchar *type_name = g_type_tag_to_string (type_tag);

    switch (type_tag) {
    case GI_TYPE_TAG_INT8:
	*gssize_out = arg_in->v_int8;
	return TRUE;
    case GI_TYPE_TAG_UINT8:
	*gssize_out = arg_in->v_uint8;
	return TRUE;
    case GI_TYPE_TAG_INT16:
	*gssize_out = arg_in->v_int16;
	return TRUE;
    case GI_TYPE_TAG_UINT16:
	*gssize_out = arg_in->v_uint16;
	return TRUE;
    case GI_TYPE_TAG_INT32:
	*gssize_out = arg_in->v_int32;
	return TRUE;
    case GI_TYPE_TAG_UINT32:
	*gssize_out = arg_in->v_uint32;
	return TRUE;
    case GI_TYPE_TAG_INT64:
	if (arg_in->v_int64 > G_MAXSSIZE || arg_in->v_int64 < G_MINSSIZE) {
	    scm_misc_error (func,
			    "Unable to marshal ~A to gssize",
			    scm_list_1 (scm_from_utf8_string (type_name)));
	    return FALSE;
	}
	*gssize_out = (gssize)arg_in->v_int64;
	return TRUE;
    case GI_TYPE_TAG_UINT64:
	if (arg_in->v_uint64 > G_MAXSSIZE) {
	    scm_misc_error (func,
			    "Unable to marshal ~A to gssize",
			    scm_list_1 (scm_from_utf8_string (type_name)));
	    return FALSE;
          }
	*gssize_out = (gssize)arg_in->v_uint64;
	return TRUE;
    default:
	scm_misc_error (func,
			"Unable to marshall ~A to gssize",
			scm_list_1 (scm_from_utf8_string (type_name)));
	return FALSE;
    }
}

static GITypeTag
get_storage_type (GITypeInfo *type_info)
{
    GITypeTag type_tag = g_type_info_get_tag (type_info);

    if (type_tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *interface = g_type_info_get_interface (type_info);
        switch (g_base_info_get_type (interface)) {
	case GI_INFO_TYPE_ENUM:
	case GI_INFO_TYPE_FLAGS:
	    type_tag = g_enum_info_get_storage_type ((GIEnumInfo *)interface);
	    break;
	default:
	    /* FIXME: we might have something to do for other types */
	    break;
        }
        g_base_info_unref (interface);
    }
    return type_tag;
}

#if 0
static void
hash_pointer_to_arg (GIArgument *arg,
		     GITypeInfo *type_info)
{
    GITypeTag type_tag = get_storage_type (type_info);

    switch (type_tag) {
    case GI_TYPE_TAG_INT8:
	arg->v_int8 = (gint8)GPOINTER_TO_INT (arg->v_pointer);
	break;
    case GI_TYPE_TAG_INT16:
	arg->v_int16 = (gint16)GPOINTER_TO_INT (arg->v_pointer);
	break;
    case GI_TYPE_TAG_INT32:
	arg->v_int32 = (gint32)GPOINTER_TO_INT (arg->v_pointer);
	break;
    case GI_TYPE_TAG_UINT8:
	arg->v_uint8 = (guint8)GPOINTER_TO_UINT (arg->v_pointer);
	break;
    case GI_TYPE_TAG_UINT16:
	arg->v_uint16 = (guint16)GPOINTER_TO_UINT (arg->v_pointer);
	break;
    case GI_TYPE_TAG_UINT32:
	arg->v_uint32 = (guint32)GPOINTER_TO_UINT (arg->v_pointer);
	break;
    case GI_TYPE_TAG_GTYPE:
	arg->v_size = GPOINTER_TO_SIZE (arg->v_pointer);
	break;
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
    case GI_TYPE_TAG_INTERFACE:
    case GI_TYPE_TAG_ARRAY:
	break;
    default:
	g_critical ("Unsupported type %s", g_type_tag_to_string(type_tag));
    }
}

static gpointer
arg_to_hash_pointer (const GIArgument *arg,
		     GITypeInfo       *type_info)
{
    GITypeTag type_tag = gi_get_storage_type (type_info);

    switch (type_tag) {
    case GI_TYPE_TAG_INT8:
	return GINT_TO_POINTER (arg->v_int8);
    case GI_TYPE_TAG_UINT8:
	return GINT_TO_POINTER (arg->v_uint8);
    case GI_TYPE_TAG_INT16:
	return GINT_TO_POINTER (arg->v_int16);
    case GI_TYPE_TAG_UINT16:
	return GINT_TO_POINTER (arg->v_uint16);
    case GI_TYPE_TAG_INT32:
	return GINT_TO_POINTER (arg->v_int32);
    case GI_TYPE_TAG_UINT32:
	return GINT_TO_POINTER (arg->v_uint32);
    case GI_TYPE_TAG_GTYPE:
	return GSIZE_TO_POINTER (arg->v_size);
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
    case GI_TYPE_TAG_INTERFACE:
    case GI_TYPE_TAG_ARRAY:
	return arg->v_pointer;
    default:
	g_critical ("Unsupported type %s", g_type_tag_to_string(type_tag));
	return arg->v_pointer;
    }
}
#endif


/**
 * _pygi_argument_array_length_marshal:
 * @length_arg_index: Index of length argument in the callables args list.
 * @user_data1: (type Array(GValue)): Array of GValue arguments to retrieve length
 * @user_data2: (type GICallableInfo): Callable info to get the argument from.
 *
 * Generic marshalling policy for array length arguments in callables.
 *
 * Returns: The length of the array or -1 on failure.
 */
gssize
gi_argument_array_length_marshal (gsize length_arg_index,
				  void *user_data1,
				  void *user_data2)
{
    GIArgInfo length_arg_info;
    GITypeInfo length_type_info;
    GIArgument length_arg;
    gssize array_len = -1;
    GValue *values = (GValue *)user_data1;
    GICallableInfo *callable_info = (GICallableInfo *)user_data2;

    g_callable_info_load_arg (callable_info,
			      (gint)length_arg_index, &length_arg_info);
    g_arg_info_load_type (&length_arg_info, &length_type_info);

    length_arg = gi_giargument_from_g_value ( &(values[length_arg_index]),
                                              &length_type_info);
    if (!gi_giargument_to_gssize (NULL,
				  &length_arg,
				g_type_info_get_tag (&length_type_info),
				&array_len)) {
        return -1;
    }

    return array_len;
}

/**
 * _pygi_argument_to_array
 * @arg: The argument to convert
 * @array_length_policy: Closure for marshalling the array length argument when needed.
 * @user_data1: Generic user data passed to the array_length_policy.
 * @user_data2: Generic user data passed to the array_length_policy.
 * @type_info: The type info for @arg
 * @out_free_array: A return location for a gboolean that indicates whether
 *                  or not the wrapped GArray should be freed
 *
 * Make sure an array type argument is wrapped in a GArray.
 *
 * Note: This method can *not* be folded into _pygi_argument_to_object() because
 * arrays are special in the sense that they might require access to @args in
 * order to get the length.
 *
 * Returns: A GArray wrapping @arg. If @out_free_array has been set to TRUE then
 *          free the array with g_array_free() without freeing the data members.
 *          Otherwise don't free the array.
 */
#if 0
GArray *
gi_giargument_to_array (GIArgument  *arg,
		      GuGIArgArrayLengthPolicy array_length_policy,
		      void        *user_data1,
		      void        *user_data2,
		      GITypeInfo  *type_info,
		      gboolean    *out_free_array)
{
    GITypeInfo *item_type_info;
    gboolean is_zero_terminated;
    gsize item_size;
    gssize length;
    GArray *g_array;

    g_return_val_if_fail (g_type_info_get_tag (type_info) == GI_TYPE_TAG_ARRAY, NULL);

    if (arg->v_pointer == NULL) {
        return NULL;
    }

    switch (g_type_info_get_array_type (type_info)) {
        case GI_ARRAY_TYPE_C:
            is_zero_terminated = g_type_info_is_zero_terminated (type_info);
            item_type_info = g_type_info_get_param_type (type_info, 0);

            item_size = _pygi_g_type_info_size (item_type_info);

            g_base_info_unref ( (GIBaseInfo *) item_type_info);

            if (is_zero_terminated) {
                length = g_strv_length (arg->v_pointer);
            } else {
                length = g_type_info_get_array_fixed_size (type_info);
                if (length < 0) {
                    gint length_arg_pos;

                    if (G_UNLIKELY (array_length_policy == NULL)) {
                        g_critical ("Unable to determine array length for %p",
                                    arg->v_pointer);
                        g_array = g_array_new (is_zero_terminated,
					       FALSE,
					       (guint)item_size);
                        *out_free_array = TRUE;
                        return g_array;
                    }

                    length_arg_pos = g_type_info_get_array_length (type_info);
                    g_assert (length_arg_pos >= 0);

                    length = array_length_policy (length_arg_pos,
						  user_data1,
						  user_data2);
                    if (length < 0) {
                        return NULL;
                    }
                }
            }

            g_assert (length >= 0);

            g_array = g_array_new (is_zero_terminated,
				   FALSE,
				   (guint)item_size);

            g_free (g_array->data);
            g_array->data = arg->v_pointer;
            g_array->len = (guint)length;
            *out_free_array = TRUE;
            break;
        case GI_ARRAY_TYPE_ARRAY:
        case GI_ARRAY_TYPE_BYTE_ARRAY:
            /* Note: GByteArray is really just a GArray */
            g_array = arg->v_pointer;
            *out_free_array = FALSE;
            break;
        case GI_ARRAY_TYPE_PTR_ARRAY:
        {
            GPtrArray *ptr_array = (GPtrArray*) arg->v_pointer;
            g_array = g_array_sized_new (FALSE, FALSE,
                                         sizeof(gpointer),
                                         ptr_array->len);
             g_array->data = (char*) ptr_array->pdata;
             g_array->len = ptr_array->len;
             *out_free_array = TRUE;
             break;
        }
        default:
            g_critical ("Unexpected array type %u",
                        g_type_info_get_array_type (type_info));
            g_array = NULL;
            break;
    }

    return g_array;
}
#endif

GIArgument
gi_argument_from_object (const char *func,
			 SCM object,
			 GITypeInfo *type_info,
			 GITransfer  transfer)
{
    GIArgument arg;
    GITypeTag type_tag;
    gpointer cleanup_data = NULL;
    gboolean is_ptr;

    memset(&arg, 0, sizeof(GIArgument));
    type_tag = g_type_info_get_tag (type_info);
    is_ptr = g_type_info_is_pointer (type_info);

    switch (type_tag) {
#if 0
        case GI_TYPE_TAG_ARRAY:
        {
            ssize_t slength;
            guint length, i;
            gboolean is_zero_terminated;
            GITypeInfo *item_type_info;
            gsize item_size;
            GArray *array;
            GITransfer item_transfer;

            if (object == SCM_BOOL_F || object == SCM_EOL) {
                arg.v_pointer = NULL;
                break;
            }

            /* Note, strings are sequences, but we cannot accept them here */
	    if (!scm_is_bytevector (object))
		scm_misc_error (func, "expected bytevector", SCM_EOL);

            slength = scm_to_ssize_t (scm_bytevector_length (object));
            is_zero_terminated = g_type_info_is_zero_terminated (type_info);
            item_type_info = g_type_info_get_param_type (type_info, 0);

            /* we handle arrays that are really strings specially, see below */
            if (g_type_info_get_tag (item_type_info) == GI_TYPE_TAG_UINT8)
               item_size = 1;
            else
               item_size = sizeof (GIArgument);

            array = g_array_sized_new (is_zero_terminated,
				       FALSE, (guint)item_size, length);
            if (array == NULL) {
                g_base_info_unref ( (GIBaseInfo *) item_type_info);
		scm_misc_error (func, "allocation error", SCM_EOL);
                break;
            }

            if (g_type_info_get_tag (item_type_info) == GI_TYPE_TAG_UINT8) {
                memcpy(array->data, SCM_BYTEVECTOR_CONTENTS(object), length);
                array->len = length;
                goto array_success;
            }

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = 0; i < length; i++) {
                PyObject *py_item;
                GIArgument item;

                py_item = PySequence_GetItem (object, i);
                if (py_item == NULL) {
                    goto array_item_error;
                }

                item = _pygi_argument_from_object (py_item, item_type_info, item_transfer);

                Py_DECREF (py_item);

                if (PyErr_Occurred()) {
                    goto array_item_error;
                }

                g_array_insert_val (array, i, item);
                continue;

array_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &array, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                array = NULL;

                _PyGI_ERROR_PREFIX ("Item %u: ", i);
                break;
            }

array_success:
            arg.v_pointer = array;

            g_base_info_unref ( (GIBaseInfo *) item_type_info);
            break;
        }
#endif
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *info;
            GIInfoType info_type;

            info = g_type_info_get_interface (type_info);
            info_type = g_base_info_get_type (info);

            switch (info_type) {
                case GI_INFO_TYPE_CALLBACK:
                    /* This should be handled in invoke() */
                    g_assert_not_reached();
                    break;
                case GI_INFO_TYPE_BOXED:
                case GI_INFO_TYPE_STRUCT:
                case GI_INFO_TYPE_UNION:
                {
                    GType g_type;
		    SCM s_type;
                    gboolean is_foreign = (info_type == GI_INFO_TYPE_STRUCT) &&
                                          (g_struct_info_is_foreign ((GIStructInfo *) info));

                    g_type = g_registered_type_info_get_g_type ( (GIRegisteredTypeInfo *) info);
                    s_type = gi_type_import_by_gi_info ( (GIBaseInfo *) info);

                    /* Note for G_TYPE_VALUE g_type:
                     * This will currently leak the GValue that is allocated and
                     * stashed in arg.v_pointer. Out argument marshaling for caller
                     * allocated GValues already pass in memory for the GValue.
                     * Further re-factoring is needed to fix this leak.
                     * See: https://bugzilla.gnome.org/show_bug.cgi?id=693405
                     */
                    /* pygi_arg_struct_from_py_marshal (object, */
                    /*                                  &arg, */
                    /*                                  NULL, /\*arg_name*\/ */
                    /*                                  info, /\*interface_info*\/ */
                    /*                                  g_type, */
                    /*                                  py_type, */
                    /*                                  transfer, */
                    /*                                  FALSE, /\*copy_reference*\/ */
                    /*                                  is_foreign, */
                    /*                                  g_type_info_is_pointer (type_info)); */
		    g_critical ("Unimplemented");

                    // Py_DECREF (py_type);
                    break;
                }
                case GI_INFO_TYPE_ENUM:
                case GI_INFO_TYPE_FLAGS:
                {
		    arg.v_int = scm_to_int (object);
                    break;
                }
                case GI_INFO_TYPE_INTERFACE:
                case GI_INFO_TYPE_OBJECT:
                    /* An error within this call will result in a NULL arg */
                    /* pygi_arg_gobject_out_arg_from_py (object, &arg, transfer); */
		    g_critical ("Unimplemented");
                    break;

                default:
                    g_assert_not_reached();
            }
            g_base_info_unref (info);
            break;
        }
#if 0
        case GI_TYPE_TAG_GLIST:
        case GI_TYPE_TAG_GSLIST:
        {
            Py_ssize_t length;
            GITypeInfo *item_type_info;
            GSList *list = NULL;
            GITransfer item_transfer;
            Py_ssize_t i;

            if (object == Py_None) {
                arg.v_pointer = NULL;
                break;
            }

            length = PySequence_Length (object);
            if (length < 0) {
                break;
            }

            item_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (item_type_info != NULL);

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = length - 1; i >= 0; i--) {
                PyObject *py_item;
                GIArgument item;

                py_item = PySequence_GetItem (object, i);
                if (py_item == NULL) {
                    goto list_item_error;
                }

                item = _pygi_argument_from_object (py_item, item_type_info, item_transfer);

                Py_DECREF (py_item);

                if (PyErr_Occurred()) {
                    goto list_item_error;
                }

                if (type_tag == GI_TYPE_TAG_GLIST) {
                    list = (GSList *) g_list_prepend ( (GList *) list, item.v_pointer);
                } else {
                    list = g_slist_prepend (list, item.v_pointer);
                }

                continue;

list_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &list, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                list = NULL;

                _PyGI_ERROR_PREFIX ("Item %zd: ", i);
                break;
            }

            arg.v_pointer = list;

            g_base_info_unref ( (GIBaseInfo *) item_type_info);

            break;
        }
        case GI_TYPE_TAG_GHASH:
        {
            Py_ssize_t length;
            PyObject *keys;
            PyObject *values;
            GITypeInfo *key_type_info;
            GITypeInfo *value_type_info;
            GITypeTag key_type_tag;
            GHashFunc hash_func;
            GEqualFunc equal_func;
            GHashTable *hash_table;
            GITransfer item_transfer;
            Py_ssize_t i;


            if (object == Py_None) {
                arg.v_pointer = NULL;
                break;
            }

            length = PyMapping_Length (object);
            if (length < 0) {
                break;
            }

            keys = PyMapping_Keys (object);
            if (keys == NULL) {
                break;
            }

            values = PyMapping_Values (object);
            if (values == NULL) {
                Py_DECREF (keys);
                break;
            }

            key_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (key_type_info != NULL);

            value_type_info = g_type_info_get_param_type (type_info, 1);
            g_assert (value_type_info != NULL);

            key_type_tag = g_type_info_get_tag (key_type_info);

            switch (key_type_tag) {
                case GI_TYPE_TAG_UTF8:
                case GI_TYPE_TAG_FILENAME:
                    hash_func = g_str_hash;
                    equal_func = g_str_equal;
                    break;
                default:
                    hash_func = NULL;
                    equal_func = NULL;
            }

            hash_table = g_hash_table_new (hash_func, equal_func);
            if (hash_table == NULL) {
                PyErr_NoMemory();
                goto hash_table_release;
            }

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = 0; i < length; i++) {
                PyObject *py_key;
                PyObject *py_value;
                GIArgument key;
                GIArgument value;

                py_key = PyList_GET_ITEM (keys, i);
                py_value = PyList_GET_ITEM (values, i);

                key = _pygi_argument_from_object (py_key, key_type_info, item_transfer);
                if (PyErr_Occurred()) {
                    goto hash_table_item_error;
                }

                value = _pygi_argument_from_object (py_value, value_type_info, item_transfer);
                if (PyErr_Occurred()) {
                    _pygi_argument_release (&key, type_info, GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                    goto hash_table_item_error;
                }

                g_hash_table_insert (hash_table, key.v_pointer,
                                     arg_to_hash_pointer (&value, value_type_info));
                continue;

hash_table_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &hash_table, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                hash_table = NULL;

                _PyGI_ERROR_PREFIX ("Item %zd: ", i);
                break;
            }

            arg.v_pointer = hash_table;

hash_table_release:
            g_base_info_unref ( (GIBaseInfo *) key_type_info);
            g_base_info_unref ( (GIBaseInfo *) value_type_info);
            Py_DECREF (keys);
            Py_DECREF (values);
            break;
        }
        case GI_TYPE_TAG_ERROR:
            PyErr_SetString (PyExc_NotImplementedError,
			     "error marshalling is not supported yet");
            /* TODO */
            break;
#endif
        default:
            /* Ignores cleanup data for now. */
	    gi_marshal_from_scm_basic_type (object, &arg, type_tag, is_ptr, transfer,
					    &cleanup_data);
            break;
    }

    return arg;
}

/**
 * _pygi_argument_to_object:
 * @arg: The argument to convert to an object.
 * @type_info: Type info for @arg
 * @transfer:
 *
 * If the argument is of type array, it must be encoded in a GArray, by calling
 * _pygi_argument_to_array(). This logic can not be folded into this method
 * as determining array lengths may require access to method call arguments.
 *
 * Returns: A PyObject representing @arg
 */
SCM
gi_giargument_to_object (GIArgument  *arg,
			 GITypeInfo *type_info,
			 GITransfer transfer)
{
    GITypeTag type_tag;
    SCM object = SCM_BOOL_F;

    type_tag = g_type_info_get_tag (type_info);

    switch (type_tag) {
        case GI_TYPE_TAG_VOID:
        {
            if (g_type_info_is_pointer (type_info)) {
                g_warn_if_fail (transfer == GI_TRANSFER_NOTHING);

                object = scm_from_pointer (arg->v_pointer, NULL);
            }
            break;
        }
#if 0
        case GI_TYPE_TAG_ARRAY:
        {
            /* Arrays are assumed to be packed in a GArray */
            GArray *array;
            GITypeInfo *item_type_info;
            GITypeTag item_type_tag;
            GITransfer item_transfer;
            gsize i, item_size;

            if (arg->v_pointer == NULL)
                return PyList_New (0);

            item_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (item_type_info != NULL);

            item_type_tag = g_type_info_get_tag (item_type_info);
            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            array = arg->v_pointer;
            item_size = g_array_get_element_size (array);

            if (G_UNLIKELY (item_size > sizeof(GIArgument))) {
                g_critical ("Stack overflow protection. "
                            "Can't copy array element into GIArgument.");
                return PyList_New (0);
            }

            if (item_type_tag == GI_TYPE_TAG_UINT8) {
                /* Return as a byte array */
                object = PYGLIB_PyBytes_FromStringAndSize (array->data, array->len);
            } else {
                object = PyList_New (array->len);
                if (object == NULL) {
                    g_critical ("Failure to allocate array for %u items", array->len);
                    g_base_info_unref ( (GIBaseInfo *) item_type_info);
                    break;
                }

                for (i = 0; i < array->len; i++) {
                    GIArgument item = { 0 };
                    PyObject *py_item;

                    memcpy (&item, array->data + i * item_size, item_size);

                    py_item = _pygi_argument_to_object (&item, item_type_info, item_transfer);
                    if (py_item == NULL) {
                        Py_CLEAR (object);
                        _PyGI_ERROR_PREFIX ("Item %zu: ", i);
                        break;
                    }

                    PyList_SET_ITEM (object, i, py_item);
                }
            }

            g_base_info_unref ( (GIBaseInfo *) item_type_info);
            break;
        }
#endif
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *info;
            GIInfoType info_type;

            info = g_type_info_get_interface (type_info);
            info_type = g_base_info_get_type (info);

            switch (info_type) {
                case GI_INFO_TYPE_CALLBACK:
                {
                    g_assert_not_reached();
                }
                case GI_INFO_TYPE_BOXED:
                case GI_INFO_TYPE_STRUCT:
                case GI_INFO_TYPE_UNION:
                {
		    SCM s_type;
                    GType g_type = g_registered_type_info_get_g_type ( (GIRegisteredTypeInfo *) info);
                    gboolean is_foreign = (info_type == GI_INFO_TYPE_STRUCT) &&
                                          (g_struct_info_is_foreign ((GIStructInfo *) info));

                    /* Special case variant and none to force loading from py module. */
                    if (g_type == G_TYPE_VARIANT || g_type == G_TYPE_NONE) {
			g_assert_not_reached ();
                        //py_type = pygi_type_import_by_gi_info (info);
                    } else {
			// FIXME: make 
                    }

		    object = scm_make_foreign_object_0(gi_gobject_type);
		    gi_gobject_set_ob_type (object, g_type);
		    gi_gobject_set_obj (object, arg);

		    // FIXME: add all the transfer and cleanup info to object
                    /* object = pygi_arg_struct_to_py_marshal (arg, */
                    /*                                         info, /\*interface_info*\/ */
                    /*                                         g_type, */
                    /*                                         py_type, */
                    /*                                         transfer, */
                    /*                                         FALSE, /\*is_allocated*\/ */
                    /*                                         is_foreign); */

                    /* Py_XDECREF (py_type); */
                    break;
                }
#if 0
                case GI_INFO_TYPE_ENUM:
                case GI_INFO_TYPE_FLAGS:
                {
                    GType type;

                    type = g_registered_type_info_get_g_type ( (GIRegisteredTypeInfo *) info);

                    if (type == G_TYPE_NONE) {
                        /* An enum with a GType of None is an enum without GType */
                        PyObject *py_type = pygi_type_import_by_gi_info (info);
                        PyObject *py_args = NULL;

                        if (!py_type)
                            return NULL;

                        py_args = PyTuple_New (1);
                        if (PyTuple_SetItem (py_args, 0, pygi_gint_to_py (arg->v_int)) != 0) {
                            Py_DECREF (py_args);
                            Py_DECREF (py_type);
                            return NULL;
                        }

                        object = PyObject_CallFunction (py_type, "i", arg->v_int);

                        Py_DECREF (py_args);
                        Py_DECREF (py_type);

                    } else if (info_type == GI_INFO_TYPE_ENUM) {
                        object = pyg_enum_from_gtype (type, arg->v_int);
                    } else {
                        object = pyg_flags_from_gtype (type, arg->v_uint);
                    }

                    break;
                }
                case GI_INFO_TYPE_INTERFACE:
                case GI_INFO_TYPE_OBJECT:
                    object = pygi_arg_gobject_to_py_called_from_c (arg, transfer);
#endif
                    break;
                default:
                    g_assert_not_reached();
            }

            g_base_info_unref (info);
            break;
        }
#if 0
        case GI_TYPE_TAG_GLIST:
        case GI_TYPE_TAG_GSLIST:
        {
            GSList *list;
            gsize length;
            GITypeInfo *item_type_info;
            GITransfer item_transfer;
            gsize i;

            list = arg->v_pointer;
            length = g_slist_length (list);

            object = PyList_New (length);
            if (object == NULL) {
                break;
            }

            item_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (item_type_info != NULL);

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = 0; list != NULL; list = g_slist_next (list), i++) {
                GIArgument item;
                PyObject *py_item;

                item.v_pointer = list->data;

                py_item = _pygi_argument_to_object (&item, item_type_info, item_transfer);
                if (py_item == NULL) {
                    Py_CLEAR (object);
                    _PyGI_ERROR_PREFIX ("Item %zu: ", i);
                    break;
                }

                PyList_SET_ITEM (object, i, py_item);
            }

            g_base_info_unref ( (GIBaseInfo *) item_type_info);
            break;
        }
        case GI_TYPE_TAG_GHASH:
        {
            GITypeInfo *key_type_info;
            GITypeInfo *value_type_info;
            GITransfer item_transfer;
            GHashTableIter hash_table_iter;
            GIArgument key;
            GIArgument value;

            if (arg->v_pointer == NULL) {
                object = Py_None;
                Py_INCREF (object);
                break;
            }

            object = PyDict_New();
            if (object == NULL) {
                break;
            }

            key_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (key_type_info != NULL);
            g_assert (g_type_info_get_tag (key_type_info) != GI_TYPE_TAG_VOID);

            value_type_info = g_type_info_get_param_type (type_info, 1);
            g_assert (value_type_info != NULL);
            g_assert (g_type_info_get_tag (value_type_info) != GI_TYPE_TAG_VOID);

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            g_hash_table_iter_init (&hash_table_iter, (GHashTable *) arg->v_pointer);
            while (g_hash_table_iter_next (&hash_table_iter, &key.v_pointer, &value.v_pointer)) {
                PyObject *py_key;
                PyObject *py_value;
                int retval;

                py_key = _pygi_argument_to_object (&key, key_type_info, item_transfer);
                if (py_key == NULL) {
                    break;
                }

                hash_pointer_to_arg (&value, value_type_info);
                py_value = _pygi_argument_to_object (&value, value_type_info, item_transfer);
                if (py_value == NULL) {
                    Py_DECREF (py_key);
                    break;
                }

                retval = PyDict_SetItem (object, py_key, py_value);

                Py_DECREF (py_key);
                Py_DECREF (py_value);

                if (retval < 0) {
                    Py_CLEAR (object);
                    break;
                }
            }

            g_base_info_unref ( (GIBaseInfo *) key_type_info);
            g_base_info_unref ( (GIBaseInfo *) value_type_info);
            break;
        }
        case GI_TYPE_TAG_ERROR:
        {
            GError *error = (GError *) arg->v_pointer;
            if (error != NULL && transfer == GI_TRANSFER_NOTHING) {
                /* If we have not been transferred the ownership we must copy
                 * the error, because pygi_error_check() is going to free it.
                 */
                error = g_error_copy (error);
            }

            if (pygi_error_check (&error)) {
                PyObject *err_type;
                PyObject *err_value;
                PyObject *err_trace;
                PyErr_Fetch (&err_type, &err_value, &err_trace);
                Py_XDECREF (err_type);
                Py_XDECREF (err_trace);
                object = err_value;
            } else {
                object = Py_None;
                Py_INCREF (object);
                break;
            }
            break;
        }
#endif
    default:
        {
            object = gi_marshal_to_scm_basic_type (arg, type_tag, transfer);
        }
    }

    return object;
}

void
gi_giargument_release (GIArgument   *arg,
                        GITypeInfo  *type_info,
                        GITransfer   transfer,
                        GIDirection  direction)
{
    GITypeTag type_tag;
    gboolean is_out = (direction == GI_DIRECTION_OUT || direction == GI_DIRECTION_INOUT);

    type_tag = g_type_info_get_tag (type_info);

    switch (type_tag) {
        case GI_TYPE_TAG_VOID:
            /* Don't do anything, it's transparent to the C side */
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
        case GI_TYPE_TAG_GTYPE:
        case GI_TYPE_TAG_UNICHAR:
            break;
        case GI_TYPE_TAG_FILENAME:
        case GI_TYPE_TAG_UTF8:
            /* With allow-none support the string could be NULL */
            if ((arg->v_string != NULL &&
                    (direction == GI_DIRECTION_IN && transfer == GI_TRANSFER_NOTHING))
                    || (direction == GI_DIRECTION_OUT && transfer == GI_TRANSFER_EVERYTHING)) {
                g_free (arg->v_string);
            }
            break;
#if 0
        case GI_TYPE_TAG_ARRAY:
        {
            GArray *array;
            gsize i;

            if (arg->v_pointer == NULL) {
                return;
            }

            array = arg->v_pointer;

            if ( (direction == GI_DIRECTION_IN && transfer != GI_TRANSFER_EVERYTHING)
                    || (direction == GI_DIRECTION_OUT && transfer == GI_TRANSFER_EVERYTHING)) {
                GITypeInfo *item_type_info;
                GITransfer item_transfer;

                item_type_info = g_type_info_get_param_type (type_info, 0);

                item_transfer = direction == GI_DIRECTION_IN ? GI_TRANSFER_NOTHING : GI_TRANSFER_EVERYTHING;

                /* Free the items */
                for (i = 0; i < array->len; i++) {
                    GIArgument item;
                    memcpy (&item, array->data + (g_array_get_element_size (array) * i), sizeof (GIArgument));
                    _pygi_argument_release (&item, item_type_info, item_transfer, direction);
                }

                g_base_info_unref ( (GIBaseInfo *) item_type_info);
            }

            if ( (direction == GI_DIRECTION_IN && transfer == GI_TRANSFER_NOTHING)
                    || (direction == GI_DIRECTION_OUT && transfer != GI_TRANSFER_NOTHING)) {
                g_array_free (array, TRUE);
            }

            break;
        }
#if 0
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *info;
            GIInfoType info_type;

            info = g_type_info_get_interface (type_info);
            info_type = g_base_info_get_type (info);

            switch (info_type) {
                case GI_INFO_TYPE_CALLBACK:
                    /* TODO */
                    break;
                case GI_INFO_TYPE_BOXED:
                case GI_INFO_TYPE_STRUCT:
                case GI_INFO_TYPE_UNION:
                {
                    GType type;

                    if (arg->v_pointer == NULL) {
                        return;
                    }

                    type = g_registered_type_info_get_g_type ( (GIRegisteredTypeInfo *) info);

                    if (g_type_is_a (type, G_TYPE_VALUE)) {
                        GValue *value;

                        value = arg->v_pointer;

                        if ( (direction == GI_DIRECTION_IN && transfer != GI_TRANSFER_EVERYTHING)
                                || (direction == GI_DIRECTION_OUT && transfer == GI_TRANSFER_EVERYTHING)) {
                            g_value_unset (value);
                        }

                        if ( (direction == GI_DIRECTION_IN && transfer == GI_TRANSFER_NOTHING)
                                || (direction == GI_DIRECTION_OUT && transfer != GI_TRANSFER_NOTHING)) {
                            g_slice_free (GValue, value);
                        }
                    } else if (g_type_is_a (type, G_TYPE_CLOSURE)) {
                        if (direction == GI_DIRECTION_IN && transfer == GI_TRANSFER_NOTHING) {
                            g_closure_unref (arg->v_pointer);
                        }
                    } else if (info_type == GI_INFO_TYPE_STRUCT &&
                               g_struct_info_is_foreign ((GIStructInfo*) info)) {
                        if (direction == GI_DIRECTION_OUT && transfer == GI_TRANSFER_EVERYTHING) {
                            pygi_struct_foreign_release (info, arg->v_pointer);
                        }
                    } else if (g_type_is_a (type, G_TYPE_BOXED)) {
                    } else if (g_type_is_a (type, G_TYPE_POINTER) || type == G_TYPE_NONE) {
                        g_warn_if_fail (!g_type_info_is_pointer (type_info) || transfer == GI_TRANSFER_NOTHING);
                    }

                    break;
                }
#endif
                case GI_INFO_TYPE_ENUM:
                case GI_INFO_TYPE_FLAGS:
                    break;
                case GI_INFO_TYPE_INTERFACE:
                case GI_INFO_TYPE_OBJECT:
                    if (arg->v_pointer == NULL) {
                        return;
                    }
                    if (is_out && transfer == GI_TRANSFER_EVERYTHING) {
                        g_object_unref (arg->v_pointer);
                    }
                    break;
                default:
                    g_assert_not_reached();
            }

            g_base_info_unref (info);
            break;
        }
        case GI_TYPE_TAG_GLIST:
        case GI_TYPE_TAG_GSLIST:
        {
            GSList *list;

            if (arg->v_pointer == NULL) {
                return;
            }

            list = arg->v_pointer;

            if ( (direction == GI_DIRECTION_IN && transfer != GI_TRANSFER_EVERYTHING)
                    || (direction == GI_DIRECTION_OUT && transfer == GI_TRANSFER_EVERYTHING)) {
                GITypeInfo *item_type_info;
                GITransfer item_transfer;
                GSList *item;

                item_type_info = g_type_info_get_param_type (type_info, 0);
                g_assert (item_type_info != NULL);

                item_transfer = direction == GI_DIRECTION_IN ? GI_TRANSFER_NOTHING : GI_TRANSFER_EVERYTHING;

                /* Free the items */
                for (item = list; item != NULL; item = g_slist_next (item)) {
                    _pygi_argument_release ( (GIArgument *) &item->data, item_type_info,
                                             item_transfer, direction);
                }

                g_base_info_unref ( (GIBaseInfo *) item_type_info);
            }

            if ( (direction == GI_DIRECTION_IN && transfer == GI_TRANSFER_NOTHING)
                    || (direction == GI_DIRECTION_OUT && transfer != GI_TRANSFER_NOTHING)) {
                if (type_tag == GI_TYPE_TAG_GLIST) {
                    g_list_free ( (GList *) list);
                } else {
                    /* type_tag == GI_TYPE_TAG_GSLIST */
                    g_slist_free (list);
                }
            }

            break;
        }
        case GI_TYPE_TAG_GHASH:
        {
            GHashTable *hash_table;

            if (arg->v_pointer == NULL) {
                return;
            }

            hash_table = arg->v_pointer;

            if (direction == GI_DIRECTION_IN && transfer != GI_TRANSFER_EVERYTHING) {
                /* We created the table without a destroy function, so keys and
                 * values need to be released. */
                GITypeInfo *key_type_info;
                GITypeInfo *value_type_info;
                GITransfer item_transfer;
                GHashTableIter hash_table_iter;
                gpointer key;
                gpointer value;

                key_type_info = g_type_info_get_param_type (type_info, 0);
                g_assert (key_type_info != NULL);

                value_type_info = g_type_info_get_param_type (type_info, 1);
                g_assert (value_type_info != NULL);

                if (direction == GI_DIRECTION_IN) {
                    item_transfer = GI_TRANSFER_NOTHING;
                } else {
                    item_transfer = GI_TRANSFER_EVERYTHING;
                }

                g_hash_table_iter_init (&hash_table_iter, hash_table);
                while (g_hash_table_iter_next (&hash_table_iter, &key, &value)) {
                    _pygi_argument_release ( (GIArgument *) &key, key_type_info,
                                             item_transfer, direction);
                    _pygi_argument_release ( (GIArgument *) &value, value_type_info,
                                             item_transfer, direction);
                }

                g_base_info_unref ( (GIBaseInfo *) key_type_info);
                g_base_info_unref ( (GIBaseInfo *) value_type_info);
            } else if (direction == GI_DIRECTION_OUT && transfer == GI_TRANSFER_CONTAINER) {
                /* Be careful to avoid keys and values being freed if the
                 * callee gave a destroy function. */
                g_hash_table_steal_all (hash_table);
            }

            if ( (direction == GI_DIRECTION_IN && transfer == GI_TRANSFER_NOTHING)
                    || (direction == GI_DIRECTION_OUT && transfer != GI_TRANSFER_NOTHING)) {
                g_hash_table_unref (hash_table);
            }

            break;
        }
        case GI_TYPE_TAG_ERROR:
        {
            GError *error;

            if (arg->v_pointer == NULL) {
                return;
            }

            error = * (GError **) arg->v_pointer;

            if (error != NULL) {
                g_error_free (error);
            }

            g_slice_free (GError *, arg->v_pointer);
            break;
        }
#endif
        default:
            break;
    }
}
