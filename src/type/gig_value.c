// Copyright (C) 2018, 2019, 2020, 2021, 2022 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <assert.h>
#include <girepository.h>
#include "gig_value.h"
#include "gig_type_priv.h"
#include "gig_flag_priv.h"

#ifndef FLT_MAX
#define FLT_MAX 3.402823466e+38F
#endif

/* GValue: a container holding a GType and an associated GValue. */

#define GIG_VALUE_WRONG_TYPE -1
#define GIG_VALUE_OUT_OF_RANGE -2

/**
 * gig_value_from_scm:
 * @value: the GValue object to store the converted value in.
 * @obj: the Scheme object to convert.
 *
 * This function converts a generic SCM value and stores the result in a
 * GValue.  The GValue must be initialised in advance with
 * g_value_init().  If the Scheme object can't be converted to the
 * type of the GValue, then an error is returned.
 *
 * Returns: 0 on success, non-zero on error.
 */
int
gig_value_from_scm(GValue *value, SCM obj)
{
    assert(value != NULL);

    GType value_type = G_VALUE_TYPE(value);

    switch (G_TYPE_FUNDAMENTAL(value_type)) {
    case G_TYPE_CHAR:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MININT8, G_MAXINT8))
            return GIG_VALUE_OUT_OF_RANGE;
        char temp = scm_to_int8(obj);
        g_value_set_schar(value, temp);
        return 0;
    }
    case G_TYPE_UCHAR:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXUINT8))
            return GIG_VALUE_OUT_OF_RANGE;
        unsigned char temp;
        temp = scm_to_uint8(obj);
        g_value_set_uchar(value, temp);
        return 0;
    }
    case G_TYPE_BOOLEAN:
    {
        if (!scm_is_boolean(obj))
            return GIG_VALUE_WRONG_TYPE;
        bool temp;
        temp = scm_is_true(obj);
        g_value_set_boolean(value, temp);
        return 0;
    }
    case G_TYPE_INT:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MININT, G_MAXINT))
            return GIG_VALUE_OUT_OF_RANGE;
        int temp;
        temp = scm_to_int(obj);
        g_value_set_int(value, temp);
        return 0;
    }
    case G_TYPE_UINT:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXUINT))
            return GIG_VALUE_OUT_OF_RANGE;
        unsigned temp;
        temp = scm_to_uint(obj);
        g_value_set_uint(value, temp);
        return 0;
    }
    case G_TYPE_LONG:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MINLONG, G_MAXLONG))
            return GIG_VALUE_OUT_OF_RANGE;
        long temp;
        temp = scm_to_long(obj);
        g_value_set_long(value, temp);
        return 0;
    }
    case G_TYPE_ULONG:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXULONG))
            return GIG_VALUE_OUT_OF_RANGE;
        unsigned long temp;
        temp = scm_to_ulong(obj);
        g_value_set_ulong(value, temp);
        return 0;
    }
    case G_TYPE_INT64:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MININT64, G_MAXINT64))
            return GIG_VALUE_OUT_OF_RANGE;
        int64_t temp;
        temp = scm_to_int64(obj);
        g_value_set_int64(value, temp);
        return 0;
    }
    case G_TYPE_UINT64:
    {
        if (!scm_is_exact_integer(obj))
            return GIG_VALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXUINT64))
            return GIG_VALUE_OUT_OF_RANGE;
        uint64_t temp;
        temp = scm_to_uint64(obj);
        g_value_set_uint64(value, temp);
        return 0;
    }
    case G_TYPE_ENUM:
    {
        if (SCM_IS_A_P(obj, gig_enum_type()))
            g_value_set_enum(value, gig_enum_to_int(obj));
        else if (scm_is_symbol(obj)) {
            SCM type = gig_type_get_scheme_type(value_type);
            obj = gig_symbol_to_enum(type, obj);
            g_value_set_enum(value, gig_enum_to_int(obj));
        }
        else
            return GIG_VALUE_WRONG_TYPE;
        return 0;
    }
    case G_TYPE_FLAGS:
    {
        if (SCM_IS_A_P(obj, gig_flags_type()))
            g_value_set_flags(value, gig_flags_to_uint(obj));
        else if (scm_is_list(obj)) {
            SCM type = gig_type_get_scheme_type(value_type);
            obj = gig_list_to_flags(type, obj);
            g_value_set_flags(value, gig_flags_to_uint(obj));
        }
        else
            return GIG_VALUE_WRONG_TYPE;
        return 0;
    }
    case G_TYPE_FLOAT:
    {
        if (!scm_is_real(obj))
            return GIG_VALUE_WRONG_TYPE;
        gdouble dval = scm_to_double(obj);
        if (dval < -G_MAXFLOAT || dval > G_MAXFLOAT)
            return GIG_VALUE_OUT_OF_RANGE;
        g_value_set_float(value, dval);
        return 0;
    }
    case G_TYPE_DOUBLE:
    {
        if (!scm_is_real(obj))
            return GIG_VALUE_WRONG_TYPE;
        gdouble temp;
        temp = scm_to_double(obj);
        g_value_set_double(value, temp);
        return 0;
    }
    case G_TYPE_STRING:
    {
        if (!scm_is_string(obj))
            return GIG_VALUE_WRONG_TYPE;
        char *temp = scm_to_utf8_string(obj);
        g_value_take_string(value, temp);
        return 0;
    }
    case G_TYPE_POINTER:
    {
        if (scm_is_pointer(obj)) {
            g_value_set_pointer(value, scm_to_pointer(obj));
            return 0;
        }
        else if (scm_is_true(scm_bytevector_p(obj))) {
            g_value_set_pointer(value, SCM_BYTEVECTOR_CONTENTS(obj));
            return 0;
        }
        else if (gig_type_get_gtype_from_obj(obj) > G_TYPE_INVALID) {
            g_value_set_object(value, gig_type_peek_object(obj));
            return 0;
        }
        return GIG_VALUE_WRONG_TYPE;
        break;
    }
    case G_TYPE_BOXED:
    {
        if (G_VALUE_HOLDS(value, G_TYPE_VALUE)) {
            GValue *n_value = g_value_get_boxed(value);
            return gig_value_from_scm(n_value, obj);
        }
        else if (G_VALUE_HOLDS(value, G_TYPE_GSTRING)) {
            gig_critical("unhandled value type");
            return GIG_VALUE_WRONG_TYPE;
        }
        else
            g_value_set_boxed(value, gig_type_peek_object(obj));
        break;
    }

    case G_TYPE_INTERFACE:
    case G_TYPE_OBJECT:
        /* we only handle interface types that have a GObject prereq */
        if (g_type_is_a(value_type, G_TYPE_OBJECT)) {
            if (scm_is_false(obj)) {
                g_value_set_object(value, NULL);
                return 0;
            }
            else if (!G_TYPE_CHECK_INSTANCE_TYPE(gig_type_peek_object(obj), value_type))
                return GIG_VALUE_WRONG_TYPE;
            else {
                g_value_set_object(value, gig_type_peek_object(obj));
                return 0;
            }
        }
        return GIG_VALUE_WRONG_TYPE;
    default:
        gig_critical("unhandled value type");
        return GIG_VALUE_WRONG_TYPE;
    }

    return 0;
}

void
gig_value_from_scm_with_error(GValue *value, SCM obj, const char *subr, int pos)
{
    int res = gig_value_from_scm(value, obj);
    switch (res) {
    case 0:
        return;
    case GIG_VALUE_WRONG_TYPE:
    {
        GType value_type = G_VALUE_TYPE(value);
        if (value_type != G_TYPE_NONE && g_type_name(value_type))
            scm_wrong_type_arg_msg(subr, pos, obj, g_type_name(value_type));
        else
            scm_wrong_type_arg(subr, pos, obj);
    }
    case GIG_VALUE_OUT_OF_RANGE:
        scm_out_of_range_pos(subr, obj, scm_from_int(pos));
    }
}

SCM
gig_value_param_as_scm(const GValue *gvalue, bool copy_boxed, const GParamSpec *pspec)
{
    if (G_IS_PARAM_SPEC_UNICHAR(pspec)) {
        scm_t_wchar u;

        u = g_value_get_uint(gvalue);
        return SCM_MAKE_CHAR(u);
    }
    else
        return gig_value_as_scm(gvalue, copy_boxed);

}

/**
 * gig_value_to_scm_basic_type:
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
gig_value_to_scm_basic_type(const GValue *value, GType fundamental, bool *handled)
{
    *handled = true;
    switch (fundamental) {
    case G_TYPE_CHAR:
        return scm_from_int8(g_value_get_schar(value));
    case G_TYPE_UCHAR:
        return scm_from_uint8(g_value_get_uchar(value));
    case G_TYPE_BOOLEAN:
        return scm_from_bool(g_value_get_boolean(value));
    case G_TYPE_INT:
        return scm_from_int(g_value_get_int(value));
    case G_TYPE_UINT:
        return scm_from_uint(g_value_get_uint(value));
    case G_TYPE_LONG:
        return scm_from_long(g_value_get_long(value));
    case G_TYPE_ULONG:
        return scm_from_ulong(g_value_get_ulong(value));
    case G_TYPE_INT64:
        return scm_from_int64(g_value_get_int64(value));
    case G_TYPE_UINT64:
        return scm_from_uint64(g_value_get_uint64(value));
    case G_TYPE_ENUM:
        return gig_int_to_enum(g_value_get_enum(value), G_VALUE_TYPE(value));
    case G_TYPE_FLAGS:
        return gig_uint_to_flags(g_value_get_flags(value), G_VALUE_TYPE(value));
    case G_TYPE_FLOAT:
        return scm_from_double(g_value_get_float(value));
    case G_TYPE_DOUBLE:
        return scm_from_double(g_value_get_double(value));
    case G_TYPE_STRING:
    {
        const char *str = g_value_get_string(value);
        if (str)
            return scm_from_utf8_string(str);
        else
            return SCM_BOOL_F;
    }
    default:
        *handled = false;
        return SCM_BOOL_F;
    }
    gig_return_val_if_reached(SCM_BOOL_F);
}

// This function creates and returns a Scheme value that
// represents the GValue passed as an argument.
static SCM
gig_value_to_scm_structured_type(const GValue *value, GType fundamental, bool copy_boxed)
{
    GITransfer transfer = copy_boxed ? GI_TRANSFER_NOTHING : GI_TRANSFER_EVERYTHING;
    switch (fundamental) {
    case G_TYPE_INTERFACE:
    {
        void *obj = g_value_get_object(value);
        if (!obj)
            return SCM_BOOL_F;
        else if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
            return gig_type_transfer_object(G_OBJECT_TYPE(obj), obj, transfer);
        else
            break;
    }
    case G_TYPE_POINTER:
        // If we get a simple pointer with no context information,
        // what can we do other than return a dumb pointer?
        return scm_from_pointer(g_value_get_pointer(value), NULL);
    case G_TYPE_PARAM:
    {
        GParamSpec *pspec = g_value_get_param(value);
        if (pspec)
            return gig_type_transfer_object(G_TYPE_PARAM, pspec, transfer);
        else
            return SCM_BOOL_F;
    }

    case G_TYPE_BOXED:
    {
        if (G_VALUE_HOLDS(value, G_TYPE_VALUE)) {
            GValue *n_value = g_value_get_boxed(value);
            return gig_value_as_scm(n_value, copy_boxed);
        }
        else if (G_VALUE_HOLDS(value, G_TYPE_GSTRING)) {
            GString *string = (GString *)g_value_get_boxed(value);
            return scm_from_utf8_stringn(string->str, string->len);
        }
        else {
            void *boxed = g_value_get_boxed(value);
            if (boxed)
                return gig_type_transfer_object(G_VALUE_TYPE(value), g_value_get_boxed(value),
                                                transfer);
            else
                return SCM_BOOL_F;
        }
    }

    case G_TYPE_OBJECT:
    {
        void *obj = g_value_get_object(value);
        if (obj)
            return gig_type_transfer_object(G_OBJECT_TYPE(obj), obj, transfer);
        else
            return SCM_BOOL_F;
    }
    case G_TYPE_VARIANT:
    {
        GVariant *v = g_value_get_variant(value);
        if (v)
            return gig_type_transfer_object(G_TYPE_VARIANT, g_variant_ref(v), transfer);
        else
            return SCM_BOOL_F;
    }
    default:
    {
        // assert_not_reached ();
        /* PyGTypeMarshal *bm; */
        /* if ((bm = pyg_type_lookup(G_VALUE_TYPE(value)))) */
        /*  return bm->fromvalue(value); */
        break;
    }
    }

    const char *type_name = g_type_name(G_VALUE_TYPE(value));
    if (type_name == NULL)
        type_name = "(null)";
    scm_misc_error("gig_value_to_scm", "unknown type ~S",
                   scm_list_1(scm_from_utf8_string(type_name)));
    gig_return_val_if_reached(SCM_BOOL_F);
}


/* Returns an SCM version of the GValue.  If COPY_BOXED,
   try to make a deep copy of the object. */
SCM
gig_value_as_scm(const GValue *value, bool copy_boxed)
{
    SCM guobj;
    bool handled;
    GType fundamental = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value));

#if 0
    if (fundamental == G_TYPE_CHAR)
        return SCM_MAKE_CHAR(g_value_get_schar(value));
    else if (fundamental == G_TYPE_UCHAR)
        return SCM_MAKE_CHAR(g_value_get_uchar(value));
#endif

    guobj = gig_value_to_scm_basic_type(value, fundamental, &handled);
    if (!handled)
        guobj = gig_value_to_scm_structured_type(value, fundamental, copy_boxed);
    return guobj;
}

SCM
gig_value_get(SCM value)
{
    GValue *gvalue = gig_type_peek_typed_object(value, gig_value_type());
    return gig_value_as_scm(gvalue, FALSE);
}

SCM
gig_value_get_type(SCM value)
{
    GValue *gvalue = gig_type_peek_typed_object(value, gig_value_type());
    return gig_type_get_scheme_type(G_VALUE_TYPE(gvalue));
}

SCM
gig_value_set(SCM where, SCM what)
{
    GValue *value = gig_type_peek_typed_object(where, gig_value_type());
    gig_value_from_scm_with_error(value, what, "%set", SCM_ARG2);
    return SCM_UNSPECIFIED;
}

SCM
gig_value_set_type(SCM where, SCM what)
{
    GType type = scm_to_gtype(what);
    SCM_ASSERT_TYPE(!G_TYPE_IS_ABSTRACT(type), what, SCM_ARG2, "%set-type!", "instantiable GType");
    GValue *value = gig_type_peek_typed_object(where, gig_value_type());
    g_value_unset(value);
    g_value_init(value, type);
    return SCM_UNSPECIFIED;
}

SCM
gig_value_transform(SCM val, SCM type)
{
    GValue *old_val = gig_type_peek_typed_object(val, gig_value_type());
    GValue *new_val = xcalloc(1, sizeof(GValue));
    g_value_init(new_val, scm_to_gtype(type));
    if (g_value_transform(old_val, new_val))
        return gig_type_transfer_object(G_TYPE_VALUE, new_val, GI_TRANSFER_EVERYTHING);
    else {
        free(new_val);
        scm_misc_error("%transform", "failed to transform ~A into ~A", scm_list_2(val, type));
    }
}

void
gig_init_value(void)
{
    scm_c_define_gsubr("%get", 1, 0, 0, gig_value_get);
    scm_c_define_gsubr("%get-type", 1, 0, 0, gig_value_get_type);
    scm_c_define_gsubr("%set!", 2, 0, 0, gig_value_set);
    scm_c_define_gsubr("%set-type!", 2, 0, 0, gig_value_set_type);
    scm_c_define_gsubr("%transform", 2, 0, 0, gig_value_transform);
}
