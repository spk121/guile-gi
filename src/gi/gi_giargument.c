/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <stdint.h>
#include <glib.h>
#include <girepository.h>
#include <libguile.h>
#include "gi_gtype.h"
#include "gi_gobject.h"
#include "gi_basictype.h"
#include <math.h>
#include "gi_gvalue.h"
#include "gir_func2.h"
#include "gir_type.h"
#include "gi_gstruct.h"
#include "gi_giargument.h"
#include "gi_gboxed.h"
#include "gi_gstruct.h"

#ifndef FLT_MAX
#define FLT_MAX 3.402823466e+38F
#endif
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

static int
arg_struct_to_scm(GIArgument *arg,
                  GIInterfaceInfo *interface_info,
                  GType g_type,
                  GITransfer transfer,
                  gboolean is_allocated,
                  gboolean is_foreign,
                  SCM obj);

static int
TYPE_TAG_IS_EXACT_INTEGER (GITypeTag x)
{
    if ((x == GI_TYPE_TAG_INT8) || (x == GI_TYPE_TAG_UINT8)
        || (x == GI_TYPE_TAG_INT16) || (x == GI_TYPE_TAG_UINT16)
        || (x == GI_TYPE_TAG_INT32) || (x == GI_TYPE_TAG_UINT32)
        || (x == GI_TYPE_TAG_INT64) || (x == GI_TYPE_TAG_UINT64))
        return TRUE;
    return FALSE;
}

static int
TYPE_TAG_IS_SIGNED_INTEGER (GITypeTag x)
{
    if ((x == GI_TYPE_TAG_INT8) || (x == GI_TYPE_TAG_INT16) || (x == GI_TYPE_TAG_INT32) || (x == GI_TYPE_TAG_INT64))
        return TRUE;
    return FALSE;
}

static int
TYPE_TAG_IS_REAL_NUMBER (GITypeTag x)
{
    if ((x == GI_TYPE_TAG_FLOAT) || (x == GI_TYPE_TAG_DOUBLE))
        return TRUE;
    return FALSE;
}

typedef enum _GIArgumentStatus {
    GI_GIARGUMENT_OK,
    GI_GIARGUMENT_NON_CONST_VOID_POINTER,
    GI_GIARGUMENT_ARRAY_ELEMENT_TOO_BIG,
    GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE,
    GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE_CONVERSION,
    GI_GIARGUMENT_UNHANDLED_INTERFACE_TYPE,
    GI_GIARGUMENT_UNHANDLED_FOREIGN_TYPE,
    GI_GIARGUMENT_UNHANDLED_IMMEDIATE_TYPE,
    GI_GIARGUMENT_UNHANDLED_STRING_TYPE,
    GI_GIARGUMENT_UNHANDLED_TYPE,
    GI_GIARGUMENT_VOID,
    GI_GIARGUMENT_WRONG_TYPE_ARG,
    GI_GIARGUMENT_ERROR,
    GI_GIARGUMENT_N_ERRORS
} GIArgumentStatus;

const static char gi_giargument_error_messages[GI_GIARGUMENT_N_ERRORS][80] = {
    [GI_GIARGUMENT_OK] = "Conversion successful",
    [GI_GIARGUMENT_NON_CONST_VOID_POINTER] = "Cannot convert a non-const void pointer",
    [GI_GIARGUMENT_ARRAY_ELEMENT_TOO_BIG] = "The array element size is too big",
    [GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE] = "Cannot pack/unpack arrays of this element type",
    [GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE_CONVERSION] = "Cannot pack/unpack this type of scheme object to array elements of this type",
    [GI_GIARGUMENT_UNHANDLED_INTERFACE_TYPE] = "Cannot handle interfaces of this type",
    [GI_GIARGUMENT_UNHANDLED_FOREIGN_TYPE] = "Cannot handle non-GObject interfaces of this type",
    [GI_GIARGUMENT_UNHANDLED_IMMEDIATE_TYPE] = "Cannot handle immediate objects of this type",
    [GI_GIARGUMENT_UNHANDLED_STRING_TYPE] = "Unknown string type",
    [GI_GIARGUMENT_UNHANDLED_TYPE] = "Cannot handle this type",
    [GI_GIARGUMENT_VOID] = "Non-pointer void arguments cannot be unpacked",
    [GI_GIARGUMENT_WRONG_TYPE_ARG] = "Cannot pack/unpack this type of scheme object for this argument type",
    [GI_GIARGUMENT_ERROR] = "GIArgument conversion error"
};

static GIArgumentStatus gi_giargument_convert_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_immediate_object_to_arg(SCM obj, GITypeTag type_tag, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_interface_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_immediate_pointer_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_string_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_const_void_pointer_object_to_arg(SCM obj, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_interface_pointer_object_to_arg(SCM object, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg);
static GIArgumentStatus gi_giargument_convert_array_object_to_arg(SCM object, GIArgInfo *array_arg_info, unsigned *must_free, GIArgument *arg);

static GIArgumentStatus gi_giargument_convert_arg_to_object(GIArgument *arg, GIArgInfo *arg_info, SCM *obj);
static GIArgumentStatus gi_giargument_convert_immediate_arg_to_object(GIArgument *arg, GITypeTag type_tag, SCM* obj);

//////////////////////////////////////////////////////////
// CONVERTING SCM OBJECTS TO GIARGUMENTS
//////////////////////////////////////////////////////////

static GIArgumentStatus
gi_giargument_convert_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg)
{
    GITypeInfo *type_info = g_arg_info_get_type (arg_info); 
    GITypeTag type_tag = g_type_info_get_tag (type_info);
    gboolean is_ptr = g_type_info_is_pointer (type_info);
    GIArgumentStatus ret;

    g_base_info_unref (type_info);
    if (!is_ptr)
    {
        switch(type_tag)
        {
            case GI_TYPE_TAG_BOOLEAN:
            case GI_TYPE_TAG_DOUBLE:
            case GI_TYPE_TAG_FLOAT:
            case GI_TYPE_TAG_INT16:
            case GI_TYPE_TAG_INT32:
            case GI_TYPE_TAG_INT64:
            case GI_TYPE_TAG_INT8:
            case GI_TYPE_TAG_UINT16:
            case GI_TYPE_TAG_UINT32:
            case GI_TYPE_TAG_UINT64:
            case GI_TYPE_TAG_UINT8:
            case GI_TYPE_TAG_UNICHAR:
            case GI_TYPE_TAG_GTYPE:
                ret = gi_giargument_convert_immediate_object_to_arg(obj, type_tag, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_VOID:
            case GI_TYPE_TAG_ARRAY:
            case GI_TYPE_TAG_UTF8:
            case GI_TYPE_TAG_FILENAME:
            case GI_TYPE_TAG_GHASH:
            case GI_TYPE_TAG_GLIST:
            case GI_TYPE_TAG_GSLIST:
            case GI_TYPE_TAG_ERROR:
                g_assert_not_reached();
                break;

            case GI_TYPE_TAG_INTERFACE:
                // The non-pointer interfaces are FLAGS, ENUM, and CALLBACK only.
                // STRUCT and OBJECT interfaces are always pointer interfaces.
                ret = gi_giargument_convert_interface_to_arg(obj, arg_info, must_free, arg);
                break;
            default:
                g_assert_not_reached();
                break;
        }
    }
    else
    {
        switch(type_tag)
        {
            case GI_TYPE_TAG_BOOLEAN:
            case GI_TYPE_TAG_DOUBLE:
            case GI_TYPE_TAG_FLOAT:
            case GI_TYPE_TAG_INT16:
            case GI_TYPE_TAG_INT32:
            case GI_TYPE_TAG_INT64:
            case GI_TYPE_TAG_INT8:
            case GI_TYPE_TAG_UINT16:
            case GI_TYPE_TAG_UINT32:
            case GI_TYPE_TAG_UINT64:
            case GI_TYPE_TAG_UINT8:
            case GI_TYPE_TAG_UNICHAR:
                ret = gi_giargument_convert_immediate_pointer_object_to_arg(obj, arg_info, must_free, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_UTF8:
            case GI_TYPE_TAG_FILENAME:
                ret = gi_giargument_convert_string_object_to_arg(obj, arg_info, must_free, arg);
                break;

            case GI_TYPE_TAG_VOID:
                ret = gi_giargument_convert_const_void_pointer_object_to_arg(obj, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_GHASH:
            case GI_TYPE_TAG_GLIST:
            case GI_TYPE_TAG_GSLIST:
                // FIXME: unhandled
                g_critical("Unhandled argument type tag %d", type_tag);
                g_assert_not_reached();
                break;

            case GI_TYPE_TAG_INTERFACE:
                ret = gi_giargument_convert_interface_pointer_object_to_arg(obj, arg_info, must_free, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_GTYPE:
                // No GType pointer inputs as far as I can tell.
                g_assert_not_reached();
                break;
            
            case GI_TYPE_TAG_ERROR:
                // FIXME: unhandled
                g_assert_not_reached();
                //ret = gi_giargument_convert_error_to_arg(obj, arg_info, must_free, arg);
                break;

            case GI_TYPE_TAG_ARRAY:
                ret = gi_giargument_convert_array_object_to_arg(obj, arg_info, must_free, arg);
                break;

            default:
                g_assert_not_reached ();
        }
    }
    g_return_val_if_reached(GI_GIARGUMENT_ERROR);
}

static GIArgumentStatus
gi_giargument_convert_immedate_object_to_arg(SCM object, GITypeTag type_tag, GIArgument *arg)
{
    /* We can assume that GIArgument values are always valid */
    switch (type_tag)
    {
    case GI_TYPE_TAG_INT8:
        arg->v_int8 = scm_to_int8(object);
        break;
    case GI_TYPE_TAG_UINT8:
        arg->v_uint8 = scm_to_uint8(object);
        break;
    case GI_TYPE_TAG_INT16:
        arg->v_int16 = scm_to_int16(object);
        break;
    case GI_TYPE_TAG_UINT16:
        arg->v_uint16 = scm_to_uint16(object);
        break;
    case GI_TYPE_TAG_INT32:
        arg->v_int32 = scm_to_int32(object);
        break;
    case GI_TYPE_TAG_UINT32:
        arg->v_uint32 = scm_to_uint32(object);
        break;
    case GI_TYPE_TAG_INT64:
        arg->v_int64 = scm_to_int64(object);
        break;
    case GI_TYPE_TAG_UINT64:
        arg->v_uint64 = scm_to_uint64(object);
        break;
    case GI_TYPE_TAG_BOOLEAN:
        arg->v_boolean = scm_is_true(object);
        break;
    case GI_TYPE_TAG_FLOAT:
        arg->v_float = scm_to_double(object);
        break;
    case GI_TYPE_TAG_DOUBLE:
        arg->v_double = scm_to_double(object);
        break;
    case GI_TYPE_TAG_GTYPE:
        arg->v_size = scm_to_size_t(object);
        break;
    case GI_TYPE_TAG_UNICHAR:
        arg->v_uint32 = SCM_CHAR (object);
        break;
    default:
        return GI_GIARGUMENT_UNHANDLED_TYPE;
    }

    return GI_GIARGUMENT_OK;
}

static GIArgumentStatus
gi_giargument_convert_interface_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg)
{
    GIArgumentStatus ret;
    GITypeInfo *type_info = g_arg_info_get_type(arg_info);

    g_assert (g_type_info_get_tag (type_info) == GI_TYPE_TAG_INTERFACE);

    GIBaseInfo *referenced_base_info = g_type_info_get_interface (type_info);
    GIInfoType referenced_base_type = g_base_info_get_type (referenced_base_info);

    g_base_info_unref (type_info);

    if (referenced_base_type == GI_INFO_TYPE_ENUM || referenced_base_type == GI_INFO_TYPE_FLAGS)
    {
        arg->v_uint32 = scm_to_uint32(obj);
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_OK;
    }
    else if (referenced_base_type == GI_INFO_TYPE_CALLBACK)
    {
        // Somehow hook in a Scheme procedure + marshaller as a callback
        g_critical("Unimplemented callback conversion");
        arg->v_pointer = NULL;
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_UNHANDLED_TYPE;
    }
    else
    {
        g_assert_not_reached();        
    }
    return ret;
}

static GIArgumentStatus
gi_giargument_convert_immediate_pointer_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg)
{
    GIArgumentStatus ret;

    // This case is unfortunate because sometimes we're referring to a single value
    // like in 'g_atomic_int_add', and sometimes a C array like in 'g_utf16_to_ucs4',
    // and there is no way to tell.  We'll use bytevector pointers, I guess, since
    // they can apply to both cases.
    if (g_arg_info_may_be_null (arg_info) && scm_is_eq(obj, SCM_BOOL_F))
    {
        arg->v_pointer = NULL;
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_OK;
    }
    if (!scm_is_bytevector (obj))
    {
        arg->v_pointer = NULL;
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_WRONG_TYPE_ARG;
    }
    else
    {
        // FIXME: add bytevector minimum length checks.
        if (g_arg_info_get_ownership_transfer(arg_info) == GI_TRANSFER_EVERYTHING)
            arg->v_pointer = g_memdup(SCM_BYTEVECTOR_CONTENTS(obj), SCM_BYTEVECTOR_LENGTH(obj));
        else
            arg->v_pointer = SCM_BYTEVECTOR_CONTENTS(obj);
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_OK;
    }
    return ret;
}

static GIArgumentStatus
gi_giargument_convert_string_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg)
{
    GIArgumentStatus ret;

    // The scm_to_..._string always makes a new copy, so if transfer isn't EVERYTHING, we'll have to free
    // the string later.
    if (g_arg_info_may_be_null(arg_info) && scm_is_eq(obj, SCM_BOOL_F))
    {
        arg->v_string = NULL;
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_OK;
    }
    else if (!scm_is_string (obj))
    {
        arg->v_string = NULL;
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_WRONG_TYPE_ARG;
    }
    else
    {
        GITypeInfo *type_info = g_arg_info_get_type(arg_info);
        GITypeTag type_tag = g_type_info_get_tag(type_info);

        if (type_tag == GI_TYPE_TAG_FILENAME)
            arg->v_string = scm_to_locale_string(obj);
        else
            arg->v_string = scm_to_utf8_string(obj);
        if (g_arg_info_get_ownership_transfer(arg_info) == GI_TRANSFER_NOTHING)
            *must_free = GIR_FREE_SIMPLE;
        else
            *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_OK;
    }

    return ret;
}

static GIArgumentStatus
gi_giargument_convert_const_void_pointer_object_to_arg(SCM obj, GIArgument *arg)
{
    arg->v_pointer = scm_to_pointer (obj);
    return GI_GIARGUMENT_OK;
}

static GIArgumentStatus
gi_giargument_convert_interface_pointer_object_to_arg(SCM obj, GIArgInfo *arg_info, unsigned *must_free, GIArgument *arg)
{
    // Usually STRUCT, UNION, INTERFACE, OBJECT.  Handle NULL_OK
    GIArgumentStatus ret = GI_GIARGUMENT_ERROR;

    GITypeInfo *type_info = g_arg_info_get_type(arg_info);

    g_assert (g_type_info_get_tag (type_info) == GI_TYPE_TAG_INTERFACE);

    GIBaseInfo *referenced_base_info = g_type_info_get_interface (type_info);
    GIInfoType referenced_base_type = g_base_info_get_type (referenced_base_info);

    g_base_info_unref (type_info);
    if (scm_is_eq (obj, SCM_BOOL_F) && g_arg_info_is_optional (arg_info))
    {
        arg->v_pointer = NULL;
        *must_free = GIR_FREE_NONE;
        ret = GI_GIARGUMENT_OK;
    }
    else if (referenced_base_type == GI_INFO_TYPE_STRUCT || referenced_base_type == GI_INFO_TYPE_UNION)
    {
        if (!SCM_IS_A_P(obj, gir_gbox_type))
        {
            arg->v_pointer = NULL;
            *must_free = GIR_FREE_NONE;
            ret = GI_GIARGUMENT_WRONG_TYPE_ARG;
        }
        else
        {
            GType type = g_registered_type_info_get_g_type(referenced_base_info);
            GirSmartPtr *sptr = scm_foreign_object_ref(obj, 0);        
            if ((sptr->holds == SPTR_HOLDS_STRUCT && referenced_base_type == GI_INFO_TYPE_STRUCT)
                || (sptr->holds == SPTR_HOLDS_UNION && referenced_base_type == GI_INFO_TYPE_UNION))
            {
                if (!g_type_is_a (sptr->type, type))
                {
                    arg->v_pointer = NULL;
                    *must_free = GIR_FREE_NONE;
                    ret = GI_GIARGUMENT_WRONG_TYPE_ARG;    
                }
                else
                {
                    arg->v_pointer = sptr->ptr;
                    *must_free = GIR_FREE_NONE;
                    ret = GI_GIARGUMENT_OK;
                }
            }
        }
    }
    else if (referenced_base_type == GI_INFO_TYPE_OBJECT)
    {
        if (!SCM_IS_A_P(obj, gi_gobject_type))
        {
            arg->v_pointer = NULL;
            *must_free = GIR_FREE_NONE;
            ret = GI_GIARGUMENT_WRONG_TYPE_ARG;            
        }
        else
        {
            GType type = g_registered_type_info_get_g_type(referenced_base_info);
            if (!g_type_is_a (gi_gobject_get_ob_type(obj), type))
            {
                arg->v_pointer = NULL;
                *must_free = GIR_FREE_NONE;
                ret = GI_GIARGUMENT_WRONG_TYPE_ARG;                   
            }
            else
            {
                arg->v_pointer = gi_gobject_get_obj(obj);
                *must_free = GIR_FREE_NONE;
                ret = GI_GIARGUMENT_OK;    
            }
        }        
    }
    else
    {
        // FIXME: definitely need to handle INTERFACE types.
        g_critical("unimplemented");
        ret = GI_GIARGUMENT_UNHANDLED_TYPE;
    }
    return ret;
}

static GIArgumentStatus
gi_giargument_convert_array_object_to_arg(SCM object, GIArgInfo *array_arg_info, guint32 *must_free, GIArgument *arg)
{
    *must_free = GIR_FREE_NONE;
    GIArgumentStatus ret = GI_GIARGUMENT_ERROR;

    // So there are layers to all this ArgInfo stuff
    // LAYER 1: Let's start on layer 1, where this GIArgInfo tells
    // us we're an array.
    GITypeInfo *array_type_info = g_arg_info_get_type (array_arg_info);
    gboolean array_is_nullable = g_arg_info_is_optional (array_arg_info);
    GITransfer array_transfer = g_arg_info_get_ownership_transfer (array_arg_info);
    GITypeTag array_type_tag = g_type_info_get_tag (array_type_info);
    gboolean array_is_ptr = g_type_info_is_pointer (array_type_info);
    gboolean array_is_zero_terminated = g_type_info_is_zero_terminated (array_type_info);
    GIArrayType array_type = g_type_info_get_array_type (array_type_info);

    // Some obvious checks
    g_assert (array_type_tag == GI_TYPE_TAG_ARRAY);
    g_assert (array_is_ptr);

    // In Glib 2.0 and GTK 3.0, the common type is C.
    // FIXME: later consider also handling BYTES type arrays and PTR_ARRAY types
    g_return_val_if_fail (array_type == GI_ARRAY_TYPE_C, GI_GIARGUMENT_ERROR);

    // LAYER 2 is where we figure out what the element type of the array is.
    GITransfer item_transfer;
    if (array_transfer == GI_TRANSFER_CONTAINER || array_transfer == GI_TRANSFER_NOTHING)
        item_transfer = GI_TRANSFER_NOTHING;
    else
        item_transfer = GI_TRANSFER_EVERYTHING;
    GITypeInfo *item_type_info = g_type_info_get_param_type(array_type_info, 0);
    GITypeTag item_type_tag = g_type_info_get_tag (item_type_info);
    gboolean item_is_ptr = g_type_info_is_pointer (item_type_info);
    gsize item_size;
    if (item_is_ptr)
        item_size = sizeof(void *);

    // LAYER 3. If, in Layer 2, we discovered that the array holds an INTERFACE type
    // we need to dig into what type of interface
    // we're talking about, STRUCT, OBJECT, ENUM or FLAGS
    GIBaseInfo *referenced_base_info = NULL;
    GIInfoType referenced_base_type = GI_INFO_TYPE_UNRESOLVED;
    GType referenced_object_type = G_TYPE_NONE;

    if (item_type_tag == GI_TYPE_TAG_INTERFACE)
    {
        referenced_base_info = g_type_info_get_interface (item_type_info);
        referenced_base_type = g_base_info_get_type (referenced_base_info);
        if (referenced_base_type == GI_INFO_TYPE_ENUM || referenced_base_type == GI_INFO_TYPE_FLAGS)
        {
            // We haven't bothered to make a special flag or enum class on
            // the Scheme side of things.  On the scheme side, enums and flags are
            // just variables holding integers.
            item_type_tag = GI_TYPE_TAG_UINT32;
        }
        else if (referenced_base_type == GI_INFO_TYPE_STRUCT || referenced_base_type == GI_INFO_TYPE_OBJECT)
        {
            // If we are a Struct or Object, we need to look up our actual GType.
            const char *referenced_name = g_base_info_get_namespace(referenced_base_info);
            const char *referenced_namespace = g_base_info_get_namespace(referenced_base_info);
            referenced_object_type = gir_lookup_type(referenced_namespace, referenced_name);

            g_assert (referenced_object_type != G_TYPE_NONE);

            if (!item_is_ptr && referenced_base_type == GI_INFO_TYPE_STRUCT)
            {
                // If we have C pointer pointing to a C array of structs
                // (not struct pointers), we need to get the size of each
                // struct.
                GIStructInfo *referenced_struct_info = g_type_get_qdata(referenced_object_type, gtype_base_info_key);
                g_assert (referenced_struct_info != NULL);
                item_size = g_struct_info_get_size (referenced_struct_info);
            }
        }
    }

    if (scm_is_eq(object, SCM_BOOL_F) && array_is_nullable)
    {
        arg->v_pointer = NULL;
        ret = GI_GIARGUMENT_OK;
    }
    // IMMEDIATE TYPES.  It seems only boolean, double, and 8 and 32-bit integer arrays are ever used
    // Sometimes deep copy.  Sometimes zero terminated.
    // For SCM bytevectors and GI_TRANSFER_NOTHING and not zero-terminated, we can use the contents of a bytevector directly.
    // For SCM bytevectors and GI_TRANSFER_EVERYTHING, we need to make a deep copy.
    // If the argument is NULL_OK and the SCM is #f, we pass NULL.

    else if (TYPE_TAG_IS_EXACT_INTEGER(item_type_tag) || TYPE_TAG_IS_REAL_NUMBER(item_type_tag))
    {
        if (scm_is_bytevector (object))
        {
            gsize element_size;
            if (item_type_tag == GI_TYPE_TAG_INT8 || item_type_tag == GI_TYPE_TAG_UINT8)
                element_size = 1;
            else if (item_type_tag == GI_TYPE_TAG_INT16 || item_type_tag == GI_TYPE_TAG_UINT16)
                element_size = 2;
            else if (item_type_tag == GI_TYPE_TAG_INT32 || item_type_tag == GI_TYPE_TAG_UINT32)
                element_size = 4;
            else if (item_type_tag == GI_TYPE_TAG_INT64 || item_type_tag == GI_TYPE_TAG_UINT64)
                element_size = 8;
            else if (item_type_tag == GI_TYPE_TAG_FLOAT)
                element_size = sizeof(float);
            else if (item_type_tag == GI_TYPE_TAG_DOUBLE)
                element_size = sizeof(double);
            else
                return GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;
            if (item_transfer == GI_TRANSFER_NOTHING)
            {
                if (!array_is_zero_terminated)
                {
                    // The fast path
                    arg->v_pointer = SCM_BYTEVECTOR_CONTENTS(object);
                    ret = GI_GIARGUMENT_OK;
                }
                else
                {
                    size_t len = SCM_BYTEVECTOR_LENGTH(object);
                    // Adding null terminate element
                    arg->v_pointer = g_malloc0(len + element_size);
                    memcpy(arg->v_pointer, SCM_BYTEVECTOR_CONTENTS(object), len);
                    *must_free = GIR_FREE_SIMPLE;
                    ret = GI_GIARGUMENT_OK;
                }
            }
            else if (item_transfer == GI_TRANSFER_EVERYTHING)
            {
                if (!array_is_zero_terminated)
                {
                    arg->v_pointer = g_memdup(SCM_BYTEVECTOR_CONTENTS(object), SCM_BYTEVECTOR_LENGTH(object));
                    ret = GI_GIARGUMENT_OK;
                }
                else
                {
                    size_t len = SCM_BYTEVECTOR_LENGTH(object);
                    // Note, null terminated here.
                    arg->v_pointer = g_malloc0(len + element_size);
                    memcpy(arg->v_pointer, SCM_BYTEVECTOR_CONTENTS(object), len);
                    ret = GI_GIARGUMENT_OK;
                }
            }
        }
        // FIXME: maybe handle other vector or list types
        else
            ret = GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE_CONVERSION;
    }

    // UTF8 or FILENAME pointers.  It seems that arrays of type UTF8 can mean two things.
    // 1. If zero-terminated, it means a NULL-pointer-terminated list of gchar pointers to UTF8 strings.
    // 2. If not zero-terminated, it means a non-zero-terminated UTF8 string, but, only for GLib regex
    //    functions, which seems like a mistake, so won't handle those yet
    else if ((item_type_tag == GI_TYPE_TAG_UTF8 || item_type_tag == GI_TYPE_TAG_FILENAME) && !array_is_zero_terminated)
        ret = GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;
    else if ((item_type_tag == GI_TYPE_TAG_UTF8 || item_type_tag == GI_TYPE_TAG_FILENAME) && array_is_zero_terminated)
    {
        if (!item_is_ptr)
            ret = GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;

        else if (scm_is_true (scm_list_p (object)))
        {
            size_t len = scm_to_size_t (scm_length (object));
            gchar **strv = g_new0 (gchar *, len + 1);
            for (size_t i = 0; i < len; i ++)
            {
                SCM entry = scm_list_ref(object, scm_from_size_t (i));
                if (item_type_tag == GI_TYPE_TAG_FILENAME)
                    strv[i] = scm_to_locale_string (entry);
                else
                    strv[i] = scm_to_utf8_string (entry);
            }
            strv[len] = NULL;
            arg->v_pointer = strv;
            if (item_transfer == GI_TRANSFER_NOTHING)
                *must_free = GIR_FREE_STRV;
            ret = GI_GIARGUMENT_OK;
        }
        // FIXME: maybe handle other list-like types or string-like entries
        else
            return GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE_CONVERSION;
    }

    // Arrays of STRUCTS. Direct struct arrays, holding the complete structures
    // themselves (re gtk_tree_view_enable_model_drag_dest), which may be null terminated.
    else if (item_type_tag == GI_TYPE_TAG_INTERFACE && referenced_base_type == GI_INFO_TYPE_STRUCT && !item_is_ptr)
    {
        // On the Scheme size, an array of structs will be a list of GBox containing
        // smart pointers to GObject structs.
        size_t len = scm_to_size_t (scm_length (object));
        gpointer ptr;
        if (array_is_zero_terminated)
            ptr = g_malloc0_n(item_size, len + 1);
        else
            ptr = g_malloc0_n(item_size, len);
        for (gsize i = 0; i < len; i ++)
        {
            SCM entry = scm_list_ref (object, scm_from_size_t(i));
            // Entry should be a GBox
            gpointer entry_ptr = gi_gbox_peek_pointer (entry);
            memcpy((char *)ptr + i * item_size, entry_ptr, item_size);
        }
        if (item_transfer == GI_TRANSFER_NOTHING)
            *must_free = GIR_FREE_SIMPLE;
        ret = GI_GIARGUMENT_OK;
    }

    // Arrays of pointers to STRUCTS.
    else if (item_type_tag == GI_TYPE_TAG_INTERFACE && referenced_base_type == GI_INFO_TYPE_STRUCT && item_is_ptr)
    {
        // On the Scheme side, and array of pointers to structs will be a list of GBox containing
        // smart pointers to GObject structs.
        size_t len = scm_to_size_t (scm_length (object));
        gpointer *ptr;
        if (array_is_zero_terminated)
            ptr = g_malloc0_n(sizeof(gpointer), len + 1);
        else
            ptr = g_malloc0_n(sizeof(gpointer), len);
        for (gsize i = 0; i < len; i ++)
        {
            SCM entry = scm_list_ref (object, scm_from_size_t(i));
            // Entry should be a GBox
            ptr[i] = gi_gbox_peek_pointer (entry);
        }
        if (item_transfer == GI_TRANSFER_NOTHING)
        {
            if (array_is_zero_terminated)
                *must_free = GIR_FREE_STRV;
            else
                *must_free = GIR_FREE_PTR_ARRAY | len;
        }
        ret = GI_GIARGUMENT_OK;
    }

    // Arrays of OBJECTS. Direct object arrays, holding the complete structures
    // themselves.  The only example is the 'additions' argument of g_list_store_splice.
    // I don't know how to get the size of a GObject, though.
    else if (item_type_tag == GI_TYPE_TAG_INTERFACE && referenced_base_type == G_TYPE_OBJECT && !item_is_ptr)
    {
        g_assert_not_reached();
    }

    // Arrays of pointers to OBJECTS.  The only example I could find is g_socket_send_message.
    else if (item_type_tag == GI_TYPE_TAG_INTERFACE && referenced_base_type == G_TYPE_OBJECT && item_is_ptr)
    {
        // On the Scheme side, an array of pointers to objects will be a list of GObjects.
        size_t len = scm_to_size_t (scm_length (object));
        gpointer *ptr;
        if (array_is_zero_terminated)
            ptr = g_malloc0_n(sizeof(gpointer), len + 1);
        else
            ptr = g_malloc0_n(sizeof(gpointer), len);
        for (gsize i = 0; i < len; i ++)
        {
            SCM entry = scm_list_ref (object, scm_from_size_t(i));
            // Entry should be a GObject
            // I guess we're not increasing refcnt?  At least that
            // it the case for g_socket_send_message.
            ptr[i] = gi_gobject_get_obj(object);
        }
        if (item_transfer == GI_TRANSFER_NOTHING)
        {
            if (array_is_zero_terminated)
                *must_free = GIR_FREE_STRV;
            else
                *must_free = GIR_FREE_PTR_ARRAY | len;
        }
        ret = GI_GIARGUMENT_OK;
    }
    else
    {
        // Everything else is unhandled.
        ret = GI_GIARGUMENT_UNHANDLED_TYPE;
    }

    g_base_info_unref(array_type_info);
    g_base_info_unref(item_type_info);
    if (referenced_base_info)
        g_base_info_unref (referenced_base_info);
    return ret;
}

//////////////////////////////////////////////////////////
// CONVERTING GIARGUMENTS TO SCM OBJECTS
//////////////////////////////////////////////////////////

static GIArgumentStatus
gi_giargument_convert_arg_to_object(GIArgument *arg, GIArgInfo *arg_info, SCM *obj)
{
    GITypeInfo *type_info = g_arg_info_get_type (arg_info); 
    GITypeTag type_tag = g_type_info_get_tag (type_info);
    gboolean is_ptr = g_type_info_is_pointer (type_info);
    GIArgumentStatus ret;

    g_base_info_unref (type_info);
    if (!is_ptr)
    {
        switch(type_tag)
        {
            case GI_TYPE_TAG_BOOLEAN:
            case GI_TYPE_TAG_DOUBLE:
            case GI_TYPE_TAG_FLOAT:
            case GI_TYPE_TAG_INT16:
            case GI_TYPE_TAG_INT32:
            case GI_TYPE_TAG_INT64:
            case GI_TYPE_TAG_INT8:
            case GI_TYPE_TAG_UINT16:
            case GI_TYPE_TAG_UINT32:
            case GI_TYPE_TAG_UINT64:
            case GI_TYPE_TAG_UINT8:
            case GI_TYPE_TAG_UNICHAR:
            case GI_TYPE_TAG_GTYPE:
                ret = gi_giargument_convert_immediate_arg_to_object(obj, type_tag, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_VOID:
            case GI_TYPE_TAG_ARRAY:
            case GI_TYPE_TAG_UTF8:
            case GI_TYPE_TAG_FILENAME:
            case GI_TYPE_TAG_GHASH:
            case GI_TYPE_TAG_GLIST:
            case GI_TYPE_TAG_GSLIST:
            case GI_TYPE_TAG_ERROR:
                g_assert_not_reached();
                break;

            case GI_TYPE_TAG_INTERFACE:
                // The non-pointer interfaces are FLAGS, ENUM, and CALLBACK only.
                // STRUCT and OBJECT interfaces are always pointer interfaces.
                ret = gi_giargument_convert_interface_to_arg(obj, arg_info, must_free, arg);
                break;
            default:
                g_assert_not_reached();
                break;
        }
    }
    else
    {
        switch(type_tag)
        {
            case GI_TYPE_TAG_BOOLEAN:
            case GI_TYPE_TAG_DOUBLE:
            case GI_TYPE_TAG_FLOAT:
            case GI_TYPE_TAG_INT16:
            case GI_TYPE_TAG_INT32:
            case GI_TYPE_TAG_INT64:
            case GI_TYPE_TAG_INT8:
            case GI_TYPE_TAG_UINT16:
            case GI_TYPE_TAG_UINT32:
            case GI_TYPE_TAG_UINT64:
            case GI_TYPE_TAG_UINT8:
            case GI_TYPE_TAG_UNICHAR:
                ret = gi_giargument_convert_immediate_pointer_object_to_arg(obj, arg_info, must_free, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_UTF8:
            case GI_TYPE_TAG_FILENAME:
                ret = gi_giargument_convert_string_object_to_arg(obj, arg_info, must_free, arg);
                break;

            case GI_TYPE_TAG_VOID:
                ret = gi_giargument_convert_const_void_pointer_object_to_arg(obj, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_GHASH:
            case GI_TYPE_TAG_GLIST:
            case GI_TYPE_TAG_GSLIST:
                // FIXME: unhandled
                g_critical("Unhandled argument type tag %d", type_tag);
                g_assert_not_reached();
                break;

            case GI_TYPE_TAG_INTERFACE:
                ret = gi_giargument_convert_interface_pointer_object_to_arg(obj, arg_info, must_free, arg);
                *must_free = GIR_FREE_NONE;
                break;

            case GI_TYPE_TAG_GTYPE:
                // No GType pointer inputs as far as I can tell.
                g_assert_not_reached();
                break;
            
            case GI_TYPE_TAG_ERROR:
                // FIXME: unhandled
                g_assert_not_reached();
                //ret = gi_giargument_convert_error_to_arg(obj, arg_info, must_free, arg);
                break;

            case GI_TYPE_TAG_ARRAY:
                ret = gi_giargument_convert_array_object_to_arg(obj, arg_info, must_free, arg);
                break;

            default:
                g_assert_not_reached ();
        }
    }
    g_return_val_if_reached(GI_GIARGUMENT_ERROR);
}

static GIArgumentStatus
gi_giargument_convert_immediate_arg_to_object(GIArgument *arg, GITypeTag type_tag, SCM *obj)
{
    switch (type_tag)
    {
    case GI_TYPE_TAG_BOOLEAN:
        *obj = scm_from_bool(arg->v_boolean);
        break;
    case GI_TYPE_TAG_INT8:
        *obj = scm_from_int8(arg->v_int8);
        break;
    case GI_TYPE_TAG_UINT8:
        *obj = scm_from_uint8(arg->v_uint8);
        break;
    case GI_TYPE_TAG_INT16:
        *obj = scm_from_int16(arg->v_int16);
        break;
    case GI_TYPE_TAG_UINT16:
        *obj = scm_from_uint16(arg->v_uint16);
        break;
    case GI_TYPE_TAG_INT32:
        *obj = scm_from_int32(arg->v_int32);
        break;
    case GI_TYPE_TAG_UINT32:
        *obj = scm_from_uint32(arg->v_uint32);
        break;
    case GI_TYPE_TAG_INT64:
        *obj = scm_from_int64(arg->v_int64);
        break;
    case GI_TYPE_TAG_UINT64:
        *obj = scm_from_uint64(arg->v_uint64);
        break;
    case GI_TYPE_TAG_FLOAT:
        *obj = scm_from_double((double)arg->v_float);
        break;
    case GI_TYPE_TAG_DOUBLE:
        *obj = scm_from_double(arg->v_double);
        break;
    case GI_TYPE_TAG_GTYPE:
        *obj = gi_gtype_c2g((GType)arg->v_size);
        break;
    case GI_TYPE_TAG_UNICHAR:
        *obj = SCM_MAKE_CHAR(arg->v_uint32);
        break;
    default:
        return GI_GIARGUMENT_UNHANDLED_IMMEDIATE_TYPE;
    }
    return GI_GIARGUMENT_OK;
}

static GIArgumentStatus
gi_giargument_convert_string_to_object(GIArgument *arg, GITypeTag type_tag, GITransfer transfer, SCM *obj)
{
    // We can't transfer strings directly, since GObject and Guile use different internal
    // encodings.  So for GI_TRANSFER_EVERYTHGING, we just free.
    switch (type_tag)
    {
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
        if (!arg->v_string)
            *obj = scm_c_make_string(0, SCM_MAKE_CHAR(0));
        else
        {
            if (type_tag == GI_TYPE_TAG_UTF8)
                *obj = scm_from_utf8_string(arg->v_string);
            else
                *obj = scm_from_locale_string(arg->v_string);
            if (transfer == GI_TRANSFER_EVERYTHING)
            {
                g_free (arg->v_string);
                arg->v_string = NULL;
            }
        }
        break;
    default:
        return GI_GIARGUMENT_UNHANDLED_STRING_TYPE;
    }
    return GI_GIARGUMENT_OK;
}


static GIArgumentStatus
gi_giargument_convert_array_to_vector(GIArgument *arg, GITypeTag item_type_tag, GITransfer item_transfer, SCM *obj)
{
    /* Here we assume we've been passed a GArray packed in the GIArgument, and we convert that into
     * a plain Guile vector */
    GArray *array = arg->v_pointer;
    gsize item_size = g_array_get_element_size (array);

    //if (item_type_tag == GI_TYPE_TAG_UINT8 || item_type_tag)
    //{
    //    *obj = scm_c_make_bytevector (array->len);
    //    memcpy (SCM_BYTEVECTOR_CONTENTS(*obj), array->data, array->len);
    //}
    //else

    if (TYPE_TAG_IS_EXACT_INTEGER(item_type_tag) || TYPE_TAG_IS_REAL_NUMBER(item_type_tag))
    {
        *obj = scm_c_make_vector(array->len, scm_from_int(0));
        if (item_type_tag == GI_TYPE_TAG_INT8)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_int8 (g_array_index (array, gint8, i)));
        else if (item_type_tag == GI_TYPE_TAG_UINT8)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_uint8 (g_array_index (array, guint8, i)));
        else if (item_type_tag == GI_TYPE_TAG_INT16)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_int16 (g_array_index (array, gint16, i)));
        else if (item_type_tag == GI_TYPE_TAG_UINT16)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_uint16 (g_array_index (array, guint16, i)));
        else if (item_type_tag == GI_TYPE_TAG_INT32)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_int32 (g_array_index (array, gint32, i)));
        else if (item_type_tag == GI_TYPE_TAG_UINT32)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_uint32 (g_array_index (array, guint32, i)));
        else if (item_type_tag == GI_TYPE_TAG_INT64)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_int64 (g_array_index (array, gint64, i)));
        else if (item_type_tag == GI_TYPE_TAG_UINT64)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_uint64 (g_array_index (array, guint64, i)));
        else if (item_type_tag == GI_TYPE_TAG_FLOAT)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_double (g_array_index (array, gfloat, i)));
        else if (item_type_tag == GI_TYPE_TAG_DOUBLE)
            for (gsize i = 0; i < array->len; i ++)
                scm_c_vector_set_x(*obj, i, scm_from_double (g_array_index (array, gdouble, i)));
        else
            return GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;
    }
    else
        return GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;

    if (item_transfer == GI_TRANSFER_EVERYTHING)
    {
        g_array_free (array, TRUE);
        arg->v_pointer = NULL;
    }
    else if (item_transfer == GI_TRANSFER_CONTAINER)
    {
        g_array_free (array, FALSE);
        arg->v_pointer = NULL;
    }
    return GI_GIARGUMENT_OK;
}



static GIArgumentStatus
gi_giargument_convert_array_to_object(GIArgument *arg, GIArgInfo *array_arg_info, SCM *obj)
{
    GIArgumentStatus ret = GI_GIARGUMENT_ERROR;
    // Just like its gi_giargument_convert_array_object_to arg, this is a rats' nest
    // of types.

    // Array outputs include
    // - deep-copied arrays of string pointers
    // - arrays of const string pointers
    // - arrays of guint8 or gint8
    // - zero-terminated arrays of guint8
    // - caller-allocated arrays of guint8
    // - arrays of int32
    // - zero-terminated arrays of int32
    // - arrays of double
    // - arrays of structs
    // - arrays of struct pointers
    // - zero-terminated arrays of variants

    // LAYER 1: Let's start on layer 1, where this GIArgInfo tells
    // us we're an array.
    GITypeInfo *array_type_info = g_arg_info_get_type (array_arg_info);
    gboolean array_is_nullable = g_arg_info_is_optional (array_arg_info);
    GITransfer array_transfer = g_arg_info_get_ownership_transfer (array_arg_info);
    GITypeTag array_type_tag = g_type_info_get_tag (array_type_info);
    gboolean array_is_ptr = g_type_info_is_pointer (array_type_info);
    gboolean array_is_zero_terminated = g_type_info_is_zero_terminated (array_type_info);
    GIArrayType array_type = g_type_info_get_array_type (array_type_info);

    // Some obvious checks
    g_assert (array_type_tag == GI_TYPE_TAG_ARRAY);
    g_assert (array_is_ptr);

    // In Glib 2.0 and GTK 3.0, the common type is C.
    // FIXME: later consider also handling BYTES type arrays
    g_return_val_if_fail (array_type == GI_ARRAY_TYPE_C, GI_GIARGUMENT_ERROR);

    // LAYER 2 is where we figure out what the element type of the array is.
    GITransfer item_transfer;
    if (array_transfer == GI_TRANSFER_CONTAINER || array_transfer == GI_TRANSFER_NOTHING)
        item_transfer = GI_TRANSFER_NOTHING;
    else
        item_transfer = GI_TRANSFER_EVERYTHING;
    GITypeInfo *item_type_info = g_type_info_get_param_type(array_type_info, 0);
    GITypeTag item_type_tag = g_type_info_get_tag (item_type_info);
    gboolean item_is_ptr = g_type_info_is_pointer (item_type_info);
    gsize item_size;
    if (item_is_ptr)
        item_size = sizeof(void *);

    // LAYER 3. If, in Layer 2, we discovered that the array holds an INTERFACE type
    // we need to dig into what type of interface
    // we're talking about, STRUCT, OBJECT, ENUM or FLAGS
    GIBaseInfo *referenced_base_info = NULL;
    GIInfoType referenced_base_type = GI_INFO_TYPE_UNRESOLVED;
    GType referenced_object_type = G_TYPE_NONE;

    if (item_type_tag == GI_TYPE_TAG_INTERFACE)
    {
        referenced_base_info = g_type_info_get_interface (item_type_info);
        referenced_base_type = g_base_info_get_type (referenced_base_info);
        if (referenced_base_type == G_TYPE_ENUM || referenced_base_type == G_TYPE_FLAGS)
        {
            // We haven't bothered to make a special flag or enum class on
            // the Scheme side of things.  On the scheme side, enums and flags are
            // just variables holding integers.
            item_type_tag = GI_TYPE_TAG_UINT32;
        }
        else if (referenced_base_type == GI_INFO_TYPE_STRUCT || referenced_base_type == GI_INFO_TYPE_OBJECT)
        {
            // If we are a Struct or Object, we need to look up our actual GType.
            const char *referenced_name = g_base_info_get_namespace(referenced_base_info);
            const char *referenced_namespace = g_base_info_get_namespace(referenced_base_info);
            referenced_object_type = gir_lookup_type (referenced_namespace, referenced_name);

            g_assert (referenced_object_type != G_TYPE_NONE);

            if (!item_is_ptr && referenced_base_type == GI_INFO_TYPE_STRUCT)
            {
                // If we have C pointer pointing to a C array of structs
                // (not struct pointers), we need to get the size of each
                // struct.
                GIStructInfo *referenced_struct_info = g_type_get_qdata(referenced_object_type, gtype_base_info_key);
                g_assert (referenced_struct_info != NULL);
                item_size = g_struct_info_get_size (referenced_struct_info);
            }
        }
    }

    if (array_is_nullable && arg->v_pointer == NULL)
    {
        *obj = SCM_BOOL_F;
        ret = GI_GIARGUMENT_OK;
    }

    // IMMEDIATE TYPES.
    else if (TYPE_TAG_IS_EXACT_INTEGER(item_type_tag) || TYPE_TAG_IS_REAL_NUMBER(item_type_tag))
    {
        gsize element_size;
        if (item_type_tag == GI_TYPE_TAG_INT8 || item_type_tag == GI_TYPE_TAG_UINT8)
            element_size = 1;
        else if (item_type_tag == GI_TYPE_TAG_INT16 || item_type_tag == GI_TYPE_TAG_UINT16)
            element_size = 2;
        else if (item_type_tag == GI_TYPE_TAG_INT32 || item_type_tag == GI_TYPE_TAG_UINT32)
            element_size = 4;
        else if (item_type_tag == GI_TYPE_TAG_INT64 || item_type_tag == GI_TYPE_TAG_UINT64)
            element_size = 8;
        else if (item_type_tag == GI_TYPE_TAG_FLOAT)
            element_size = sizeof(float);
        else if (item_type_tag == GI_TYPE_TAG_DOUBLE)
            element_size = sizeof(double);
        else
            return GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;
    }

    g_critical("Unimplemented");
    *obj = SCM_BOOL_F;
    return GI_GIARGUMENT_ERROR;
}

static GIArgumentStatus
gi_giargument_convert_const_void_pointer_to_object(GIArgument *arg, SCM obj)
{
    obj = scm_from_pointer (arg->v_pointer, NULL);
    return GI_GIARGUMENT_OK;
}


#if 0
    GITypeInfo *type_info = g_arg_info_get_type(arg_info);
    // GIDirection direction = g_arg_info_get_direction(arg_info);
    GITransfer transfer = g_arg_info_get_ownership_transfer(arg_info);
    // gboolean may_be_null = g_arg_info_may_be_null(arg_info);
    GITypeTag type_tag = g_type_info_get_tag(type_info);
    int ret = GI_GIARGUMENT_OK;

    switch (type_tag)
    {
    case GI_TYPE_TAG_VOID:
    {
        /* Convert const void * to SCM pointers */
        if (g_type_info_is_pointer(type_info))
        {
            if (transfer != GI_TRANSFER_NOTHING)
                ret = GI_GIARGUMENT_NON_CONST_VOID_POINTER;
            else
                obj = scm_from_pointer(arg->v_pointer, NULL);
        }
        break;
    }
    case GI_TYPE_TAG_ARRAY:
    {
        /* Convert a GArray* of simple types to a SCM bytevector */
        /* Arrays are assumed to be packed in a GArray */
        GArray *array;
        GITypeInfo *item_type_info;
        GITypeTag item_type_tag;
        gsize item_size;

        if (arg->v_pointer == NULL)
            obj = scm_c_make_bytevector(0);

        item_type_info = g_type_info_get_param_type(type_info, 0);
        item_type_tag = g_type_info_get_tag(item_type_info);

        array = arg->v_pointer;
        item_size = g_array_get_element_size(array);

        if (item_size > sizeof(GIArgument))
            ret = GI_GIARGUMENT_ARRAY_ELEMENT_TOO_BIG;
        else if (TYPE_TAG_IS_EXACT_INTEGER(item_type_tag) || TYPE_TAG_IS_REAL_NUMBER(item_type_tag))
        {
            // This is a simple numerical array, so we'll put it into
            // a bytevector.  We need to put the contents in
            // GC-managed memory, so we can't keep the contents even
            // if it is GI_TRANSFER_EVERYTHING.
            obj = scm_c_make_bytevector(array->len * item_size);
            memcpy(SCM_BYTEVECTOR_CONTENTS(obj),
                   array->data,
                   array->len * item_size);

            // Since we've made a copy of the original array into a
            // bytevector, we can delete the original array
            if (transfer == GI_TRANSFER_EVERYTHING)
                g_array_free(array, TRUE);
            else if (transfer == GI_TRANSFER_CONTAINER)
                g_array_free(array, FALSE);
            arg->v_pointer = NULL;
        }
        else
            ret = GI_GIARGUMENT_UNHANDLED_ARRAY_ELEMENT_TYPE;

        g_base_info_unref(item_type_info);
        break;
    }
    case GI_TYPE_TAG_INTERFACE:
    {
        GIBaseInfo *info;
        GIInfoType info_type;

        info = g_type_info_get_interface(type_info);
        info_type = g_base_info_get_type(info);

        switch (info_type)
        {
        case GI_INFO_TYPE_CALLBACK:
            // Should be handled by invoke? Never reached?
            ret = GI_GIARGUMENT_UNHANDLED_INTERFACE_TYPE;
            break;
        case GI_INFO_TYPE_BOXED:
        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_UNION:
        {
            GType g_type = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *)info);
            gboolean is_foreign = (info_type == GI_INFO_TYPE_STRUCT) &&
                                  (g_struct_info_is_foreign((GIStructInfo *)info));

            ret = arg_struct_to_scm (arg, info, g_type, transfer, FALSE, is_foreign, obj);
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

                        obj = PyObject_CallFunction (py_type, "i", arg->v_int);

                        Py_DECREF (py_args);
                        Py_DECREF (py_type);

                    } else if (info_type == GI_INFO_TYPE_ENUM) {
                        object = pyg_enum_from_gtype (type, arg->v_int);
                    } else {
                        object = pyg_flags_from_gtype (type, arg->v_uint);
                    }

                    break;
                }
#endif
        case GI_INFO_TYPE_INTERFACE:
        case GI_INFO_TYPE_OBJECT:
            obj = gi_arg_gobject_to_scm_called_from_c(arg, transfer);

            break;
        default:
            g_assert_not_reached();
        }

        g_base_info_unref(info);
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
        obj = gi_marshal_to_scm_basic_type(arg, type_tag, transfer);
    }
    }

    return ret;
}
#endif

static int
arg_struct_to_scm(GIArgument *arg,
                  GIInterfaceInfo *interface_info,
                  GType g_type,
                  GITransfer transfer,
                  gboolean is_allocated,
                  gboolean is_foreign,
                  SCM obj)
{
    // Once we get here, INTERFACE_INFO says we are a struct or
    // union, and g_type is the GObject GType of that struct or
    // union.
    if (arg->v_pointer == NULL) {
        obj = SCM_BOOL_F;
        return 0;
    }

    // A struct/union/box containing a simple value?  Let's just unbox
    // that now.
    if (g_type_is_a (g_type, G_TYPE_VALUE)) {
        obj = gi_gvalue_as_scm(arg->v_pointer, FALSE);
        return 0;
    }

    // All the foreign types.
    else if (is_foreign) {
        // FIXME: this is where you look up a special handler for
        // Cairo types
        return GI_GIARGUMENT_UNHANDLED_FOREIGN_TYPE;
    }

    // All the rest of the boxed types get re-wrapped into a
    // Scheme-friendly refcounted box.
    else {
        gboolean copy_boxed = FALSE;
        gboolean own_ref = FALSE;
    if (g_type_is_a (g_type, G_TYPE_BOXED)) {
        if (transfer == GI_TRANSFER_EVERYTHING || is_allocated)
        copy_boxed = TRUE;
        if (is_allocated && g_struct_info_get_size (interface_info) > 0)
        own_ref = TRUE;
        obj = gir_new_gbox (SPTR_HOLDS_GBOXED, g_type, arg->v_pointer, copy_boxed);
    } else if (g_type_is_a (g_type, G_TYPE_POINTER)) {
        // Struct or union containing a pointer
        obj = gir_new_gbox (SPTR_HOLDS_POINTER, g_type, arg->v_pointer, transfer == GI_TRANSFER_EVERYTHING);
    } else if (g_type_is_a (g_type, G_TYPE_VARIANT)) {
        if (transfer == GI_TRANSFER_NOTHING) {
        g_variant_ref_sink (arg->v_pointer);
        }
        obj = gir_new_gbox (SPTR_HOLDS_STRUCT, g_type, arg->v_pointer, FALSE);
    } else if (g_type == G_TYPE_NONE) {
        if (transfer == GI_TRANSFER_EVERYTHING || is_allocated)
        obj = gir_new_gbox (SPTR_HOLDS_STRUCT, g_type, arg->v_pointer, TRUE);
        else
        obj = gir_new_gbox (SPTR_HOLDS_STRUCT, g_type, arg->v_pointer, FALSE);
    } else
        return GI_GIARGUMENT_UNHANDLED_TYPE;
    }

    return GI_GIARGUMENT_OK;
};


gboolean
gi_giargument_check_scm_type(SCM obj, GIArgInfo *ai, char **errstr)
{
    GITypeInfo *ti = g_arg_info_get_type(ai);
    // GITransfer transfer = g_arg_info_get_ownership_transfer(ai);
    GIDirection dir = g_arg_info_get_direction(ai);
    GITypeTag type_tag = g_type_info_get_tag(ti);
    gboolean is_ptr = g_type_info_is_pointer(ti);
    gboolean ok;

    g_assert(dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT);

    if (!is_ptr)
    {
        if (TYPE_TAG_IS_EXACT_INTEGER(type_tag))
        {
            if (!scm_is_exact_integer(obj))
            {
                *errstr = g_strdup_printf("expected exact integer");
                ok = FALSE;
            }
            else
            {
                if (TYPE_TAG_IS_SIGNED_INTEGER(type_tag))
                {
                    intmax_t val = scm_to_intmax(obj);
                    if (val < intmin[type_tag] || val > intmax[type_tag])
                    {
                        *errstr = g_strdup_printf("integer out of range");
                        ok = FALSE;
                    }
                    else
                        ok = TRUE;
                }
                else
                {
                    uintmax_t val = scm_to_uintmax(obj);
                    if (val > uintmax[type_tag])
                    {
                        *errstr = g_strdup_printf("unsigned integer out of range");
                        ok = FALSE;
                    }
                    else
                        ok = TRUE;
                }
            }
        }
        else if (TYPE_TAG_IS_REAL_NUMBER(type_tag))
        {
            if (!scm_is_real(obj))
            {
                *errstr = g_strdup_printf("expected real number");
                ok = FALSE;
            }
            else
            {
                // FIXME, if you really wanted to, you could make a scheme integer
                // bigger than DBL_MAX, so this would throw.
                double val = scm_to_double(obj);
                if (type_tag == GI_TYPE_TAG_FLOAT)
                {
                    if (val < -G_MAXFLOAT || val > G_MAXFLOAT)
                    {
                        *errstr = g_strdup_printf("real number out of range");
                        ok = FALSE;
                    }
                    else
                        ok = TRUE;
                }
                else
                    ok = TRUE;
            }
        }
        else if (type_tag == GI_TYPE_TAG_BOOLEAN)
        {
            if (!scm_is_eq(obj, SCM_BOOL_F) && !scm_is_eq(obj, SCM_BOOL_T))
            {
                *errstr = g_strdup_printf("expected boolean");
                ok = FALSE;
            }
            else
                ok = TRUE;
        }
        else
        {
            *errstr = g_strdup_printf("unhandled type %u", type_tag);
            ok = FALSE;
        }
    }
    else /* is_ptr */
    {
        if (TYPE_TAG_IS_EXACT_INTEGER(type_tag) || TYPE_TAG_IS_REAL_NUMBER(type_tag) || type_tag == GI_TYPE_TAG_UTF8 || type_tag == GI_TYPE_TAG_FILENAME || type_tag == GI_TYPE_TAG_VOID)
        {
            if (!scm_is_bytevector(obj) && !scm_is_string(obj))
            {
                *errstr = g_strdup_printf("expected bytevector or string");
                ok = FALSE;
            }
            else
                ok = TRUE;
        }
        else if (type_tag == GI_TYPE_TAG_INTERFACE)
        {
            ok = TRUE;
        }
    else if (type_tag == GI_TYPE_TAG_ARRAY)
    {
        ok = TRUE;
    }
        else
        {
            *errstr = g_strdup_printf("unhandled pointer type %u", type_tag);
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

#if 0
static GITypeTag
get_storage_type (GITypeInfo *type_info)
{
    GITypeTag type_tag = g_type_info_get_tag (type_info);

    if (type_tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *iface = g_type_info_get_interface (type_info);
        switch (g_base_info_get_type (iface)) {
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
        type_tag = g_enum_info_get_storage_type ((GIEnumInfo *)iface);
        break;
    default:
        /* FIXME: we might have something to do for other types */
        break;
        }
        g_base_info_unref (iface);
    }
    return type_tag;
}
#endif

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

    g_debug ("making GIArgument from object");
    memset(&arg, 0, sizeof(GIArgument));
    type_tag = g_type_info_get_tag (type_info);
    is_ptr = g_type_info_is_pointer (type_info);

    g_debug (" type1 %s",g_type_tag_to_string (type_tag));

    switch (type_tag) {
        case GI_TYPE_TAG_ARRAY:
        {
            ssize_t slength;
            guint length, i;
            gboolean is_zero_terminated;
            GITypeInfo *item_type_info;
            gsize item_size;
            GArray *array;
            GITransfer item_transfer;

            if (scm_is_eq (object, SCM_BOOL_F) || scm_is_eq (object, SCM_EOL))
            {
                arg.v_pointer = NULL;
                break;
            }

            /* Note, strings are sequences, but we cannot accept them here */
        if (!scm_is_vector (object))
           scm_misc_error (func, "expected vector", SCM_EOL);

            slength = scm_to_ssize_t (scm_vector_length (object));
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
        if (scm_is_bytevector (object)) {
            memcpy(array->data, SCM_BYTEVECTOR_CONTENTS(object), length);
            array->len = length;
            goto array_success;
        }
            }

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = 0; i < slength; i++) {
        SCM s_item;
                GIArgument item;

        s_item = scm_c_vector_ref (object, i);
                /* if (py_item == NULL) { */
                /*     goto array_item_error; */
                /* } */

                item = gi_argument_from_object (func, s_item, item_type_info, item_transfer);

                /* Py_DECREF (py_item); */

                /* if (PyErr_Occurred()) { */
                /*     goto array_item_error; */
                /* } */

                g_array_insert_val (array, i, item);
                continue;
#if 0

array_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &array, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                array = NULL;

                _PyGI_ERROR_PREFIX ("Item %u: ", i);
                break;
#endif
            }

array_success:
#if 0
            arg.v_pointer = array;
            g_base_info_unref ( (GIBaseInfo *) item_type_info);
#endif
            break;
        }
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *info;
            GIInfoType info_type;

            info = g_type_info_get_interface (type_info);
            info_type = g_base_info_get_type (info);
        g_debug ("  type2 %s", g_info_type_to_string (info_type));
        g_debug ("  type3 %s", g_base_info_get_name (info));
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
                    //s_type = gi_type_import_by_gi_info ( (GIBaseInfo *) info);
                    s_type = gi_gtype_c2g(g_type);

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
            g_critical ("In barely implemented OBJ->arg");
            arg.v_pointer = gi_gobject_get_obj (object);
            // g_assert_not_reached ();
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
SCM gi_giargument_to_object(GIArgument *arg,
                            GITypeInfo *type_info,
                            GITransfer transfer)
{
    GITypeTag type_tag;
    SCM object = SCM_BOOL_F;

    type_tag = g_type_info_get_tag(type_info);

    switch (type_tag)
    {
    case GI_TYPE_TAG_VOID:
    {
        if (g_type_info_is_pointer(type_info))
        {
            g_warn_if_fail(transfer == GI_TRANSFER_NOTHING);

            object = scm_from_pointer(arg->v_pointer, NULL);
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

        info = g_type_info_get_interface(type_info);
        info_type = g_base_info_get_type(info);

        switch (info_type)
        {
        case GI_INFO_TYPE_CALLBACK:
        {
            g_assert_not_reached();
        }
        case GI_INFO_TYPE_BOXED:
        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_UNION:
        {
            SCM s_type;
            GType g_type = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *)info);
            gboolean is_foreign = (info_type == GI_INFO_TYPE_STRUCT) &&
                                  (g_struct_info_is_foreign((GIStructInfo *)info));

        if (info_type == GI_INFO_TYPE_STRUCT) {
        object = gir_new_struct_gbox (g_type, arg->v_pointer, transfer == GI_TRANSFER_EVERYTHING);
        break;
        }
        else if (info_type == GI_INFO_TYPE_UNION) {
        object =  gir_new_union_gbox (g_type, arg->v_pointer, transfer == GI_TRANSFER_EVERYTHING);
        break;
        }

        /* Special case variant and none to force loading from py module. */
            if (g_type == G_TYPE_VARIANT || g_type == G_TYPE_NONE)
            {
                g_assert_not_reached();
                //py_type = pygi_type_import_by_gi_info (info);
            }
            else
            {
                // FIXME: make
            }

            object = scm_make_foreign_object_0(gi_gobject_type);
            gi_gobject_set_ob_type(object, g_type);
            gi_gobject_set_obj(object, arg->v_pointer);

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
#endif
        case GI_INFO_TYPE_INTERFACE:
        case GI_INFO_TYPE_OBJECT:
            object = gi_arg_gobject_to_scm_called_from_c(arg, transfer);

            break;
        default:
            g_assert_not_reached();
        }

        g_base_info_unref(info);
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
        object = gi_marshal_to_scm_basic_type(arg, type_tag);
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

static SCM
scm_immediate_giargument_to_object(SCM s_arg, SCM s_type)
{
    if (!SCM_IS_A_P(s_arg, gi_giargument_type))
        scm_wrong_type_arg ("immediate-giargument->object", SCM_ARG1, s_arg);
    GITypeTag type_tag = scm_to_int (s_type);
    SCM obj = SCM_BOOL_T;

    GIArgument *arg = gi_giargument_get_argument (s_arg);
    int ret = gi_giargument_convert_immediate_to_object(arg, type_tag, &obj);
    if (ret != GI_GIARGUMENT_OK)
        scm_misc_error("immediate-giargument->object",
            gi_giargument_error_messages[ret],
            SCM_EOL);
    return obj;
}

static SCM
scm_immediate_object_to_giargument(SCM s_obj, SCM s_type)
{
    GIArgumentStatus ret;

    GITypeTag type_tag = scm_to_int (s_type);
    GIArgument *arg = scm_gc_malloc (sizeof(GIArgument), "GIArgument");
    memset (arg, 0, sizeof (GIArgument));

    ret = gi_giargument_convert_immedate_object_to_arg(s_obj, type_tag, arg);
    if (ret != GI_GIARGUMENT_OK)
        scm_misc_error("immediate-object->giargument",
            gi_giargument_error_messages[ret],
            SCM_EOL);


    return scm_make_foreign_object_1 (gi_giargument_type, arg);
}

static SCM
scm_string_object_to_giargument(SCM s_obj, SCM s_type, SCM s_transfer)
{
    GIArgumentStatus ret;

    GITypeTag type_tag = scm_to_int (s_type);
    GITransfer transfer = scm_to_int (s_transfer);
    GIArgument *arg = scm_gc_malloc (sizeof(GIArgument), "GIArgument");
    memset (arg, 0, sizeof (GIArgument));

    ret = gi_giargument_convert_string_object_to_arg(s_obj, type_tag, transfer, arg);
    if (ret != GI_GIARGUMENT_OK)
        scm_misc_error("string-object->giargument",
            gi_giargument_error_messages[ret],
            SCM_EOL);


    return scm_make_foreign_object_1 (gi_giargument_type, arg);
}

static SCM
scm_string_giargument_to_object(SCM s_arg, SCM s_type, SCM s_transfer)
{
    if (!SCM_IS_A_P(s_arg, gi_giargument_type))
        scm_wrong_type_arg ("string-giargument->object", SCM_ARG1, s_arg);
    GITypeTag type_tag = scm_to_int (s_type);
    GITransfer transfer = scm_to_int (s_transfer);
    SCM obj = SCM_BOOL_T;

    GIArgument *arg = gi_giargument_get_argument (s_arg);
    int ret = gi_giargument_convert_string_to_object(arg, type_tag, transfer, &obj);
    if (ret != GI_GIARGUMENT_OK)
        scm_misc_error("string-giargument->object",
            gi_giargument_error_messages[ret],
            SCM_EOL);
    return obj;
}

static SCM
scm_giargument_to_pointer(SCM s_arg)
{
    if (!SCM_IS_A_P(s_arg, gi_giargument_type))
    scm_wrong_type_arg ("giargument->pointer", SCM_ARG1, s_arg);

    GIArgument *arg = gi_giargument_get_argument (s_arg);

    return scm_from_pointer (arg->v_pointer, NULL);
}

static SCM
scm_pointer_to_giargument(SCM s_obj)
{
    if (!SCM_POINTER_P (s_obj))
    scm_wrong_type_arg ("pointer->giargument", SCM_ARG1, s_obj);

    GIArgument *arg = scm_gc_malloc (sizeof(GIArgument), "GIArgument");
    memset (arg, 0, sizeof (GIArgument));
    arg->v_pointer = scm_to_pointer (s_obj);
    return scm_make_foreign_object_1(gi_giargument_type, arg);
}

#if 0
static SCM
scm_convert_giargument_to_object (SCM s_arg, SCM s_arg_info)
{
    if (!SCM_IS_A_P(gi_giargument_type, s_arg))
        scm_wrong_type_arg ("convert-giargument-to-object", SCM_ARG1, s_arg);
    if (!SCM_IS_A_P(s_GIArgInfo_type, s_arg_info))
        scm_wrong_type_arg ("convert-giargument-to-object", SCM_ARG2, s_arg_info);

    GIArgument *arg = gi_giargument_get_argument(s_arg);
    GIArgInfo *arg_info = scm_foreign_object_ref (s_arg_info, 0);

    SCM obj = SCM_BOOL_F;
    int err = gi_giargument_convert_to_object (arg, arg_info, obj);

    if (err)
        scm_misc_error ("convert-giargument-to-object", "marshalling error", SCM_EOL);
    return obj;
}
#endif


#define SCONSTX(NAME) scm_permanent_object (scm_c_define (#NAME, scm_from_int (NAME)))

void
gi_init_giargument (void)
{
    gi_init_giargument_type();

    SCONSTX(GI_TYPE_TAG_VOID);
    SCONSTX(GI_TYPE_TAG_BOOLEAN);
    SCONSTX(GI_TYPE_TAG_INT8);
    SCONSTX(GI_TYPE_TAG_UINT8);
    SCONSTX(GI_TYPE_TAG_INT16);
    SCONSTX(GI_TYPE_TAG_UINT16);
    SCONSTX(GI_TYPE_TAG_INT32);
    SCONSTX(GI_TYPE_TAG_UINT32);
    SCONSTX(GI_TYPE_TAG_INT64);
    SCONSTX(GI_TYPE_TAG_UINT64);
    SCONSTX(GI_TYPE_TAG_FLOAT);
    SCONSTX(GI_TYPE_TAG_DOUBLE);
    SCONSTX(GI_TYPE_TAG_GTYPE);
    SCONSTX(GI_TYPE_TAG_UTF8);
    SCONSTX(GI_TYPE_TAG_FILENAME);
    SCONSTX(GI_TYPE_TAG_ARRAY);
    SCONSTX(GI_TYPE_TAG_INTERFACE);
    SCONSTX(GI_TYPE_TAG_GLIST);
    SCONSTX(GI_TYPE_TAG_GSLIST);
    SCONSTX(GI_TYPE_TAG_GHASH);
    SCONSTX(GI_TYPE_TAG_ERROR);
    SCONSTX(GI_TYPE_TAG_UNICHAR);
    SCONSTX(GI_TRANSFER_NOTHING);
    SCONSTX(GI_TRANSFER_CONTAINER);
    SCONSTX(GI_TRANSFER_EVERYTHING);

    scm_c_define_gsubr ("immediate-giargument->object", 2, 0, 0, scm_immediate_giargument_to_object);
    scm_c_define_gsubr ("immediate-object->giargument", 2, 0, 0, scm_immediate_object_to_giargument);
    scm_c_define_gsubr ("string-giargument->object", 3, 0, 0, scm_string_giargument_to_object);
    scm_c_define_gsubr ("string-object->giargument", 3, 0, 0, scm_string_object_to_giargument);
    scm_c_define_gsubr ("giargument->pointer", 1, 0, 0, scm_giargument_to_pointer);
    scm_c_define_gsubr ("pointer->giargument", 1, 0, 0, scm_pointer_to_giargument);
}
