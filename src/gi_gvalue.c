// Copyright (C) 2018, 2019 Michael L. Gran

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
#include <girepository.h>
#include "gi_gvalue.h"
#include "gir_type.h"
#include "gi_gobject.h"
#include "gir_type.h"

#ifndef FLT_MAX
#define FLT_MAX 3.402823466e+38F
#endif

/* GValue: a container holding a GType and an associated GValue. */

#define GI_GVALUE_WRONG_TYPE -1
#define GI_GVALUE_OUT_OF_RANGE -2

SCM
gi_gvalue_c2g(GValue *val)
{
    if (val)
        return scm_make_foreign_object_1(gi_gvalue_type, val);

    g_return_val_if_reached(SCM_BOOL_F);
}

void
gi_gvalue_finalizer(SCM self)
{
    GValue *val;
    val = gi_gvalue_get_value(self);
    if (val) {
        g_value_unset(val);
        g_free(val);
        gi_gvalue_set_value(self, (GValue *)NULL);
    }
}

/**
 * gi_gvalue_from_scm_with_error:
 * @value: the GValue object to store the converted value in.
 * @obj: the Python object to convert.
 *
 * This function converts a generic SCM value and stores the result in a
 * GValue.  The GValue must be initialised in advance with
 * g_value_init().  If the Python object can't be converted to the
 * type of the GValue, then an error is returned.
 *
 * Returns: 0 on success, -1 on error.
 */
int
gi_gvalue_from_scm(GValue *value, SCM obj)
{
    g_assert(value != NULL);

    GType value_type = G_VALUE_TYPE(value);

    switch (G_TYPE_FUNDAMENTAL(value_type)) {
    case G_TYPE_CHAR:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MININT8, G_MAXINT8))
            return GI_GVALUE_OUT_OF_RANGE;
        gint8 temp = scm_to_int8(obj);
        g_value_set_schar(value, temp);
        return 0;
    }
    case G_TYPE_UCHAR:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXUINT8))
            return GI_GVALUE_OUT_OF_RANGE;
        guchar temp;
        temp = scm_to_uint8(obj);
        g_value_set_uchar(value, temp);
        return 0;
    }
    case G_TYPE_BOOLEAN:
    {
        if (!scm_is_eq(obj, SCM_BOOL_T) && !scm_is_eq(obj, SCM_BOOL_F))
            return GI_GVALUE_WRONG_TYPE;
        gboolean temp;
        temp = scm_is_true(obj);
        g_value_set_boolean(value, temp);
        return 0;
    }
    case G_TYPE_INT:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MININT, G_MAXINT))
            return GI_GVALUE_OUT_OF_RANGE;
        gint temp;
        temp = scm_to_int(obj);
        g_value_set_int(value, temp);
        return 0;
    }
    case G_TYPE_UINT:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXUINT))
            return GI_GVALUE_OUT_OF_RANGE;
        guint temp;
        temp = scm_to_uint(obj);
        g_value_set_uint(value, temp);
        return 0;
    }
    case G_TYPE_LONG:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MINLONG, G_MAXLONG))
            return GI_GVALUE_OUT_OF_RANGE;
        glong temp;
        temp = scm_to_long(obj);
        g_value_set_long(value, temp);
        return 0;
    }
    case G_TYPE_ULONG:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXULONG))
            return GI_GVALUE_OUT_OF_RANGE;
        gulong temp;
        temp = scm_to_ulong(obj);
        g_value_set_ulong(value, temp);
        return 0;
    }
    case G_TYPE_INT64:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_signed_integer(obj, G_MININT64, G_MAXINT64))
            return GI_GVALUE_OUT_OF_RANGE;
        gint64 temp;
        temp = scm_to_int64(obj);
        g_value_set_int64(value, temp);
        return 0;
    }
    case G_TYPE_UINT64:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXUINT64))
            return GI_GVALUE_OUT_OF_RANGE;
        guint64 temp;
        temp = scm_to_uint64(obj);
        g_value_set_uint64(value, temp);
        return 0;
    }
    case G_TYPE_ENUM:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXULONG))
            return GI_GVALUE_OUT_OF_RANGE;
        gint val;
        val = scm_to_ulong(obj);
        g_value_set_enum(value, val);
    }
        break;
    case G_TYPE_FLAGS:
    {
        if (!scm_is_exact_integer(obj))
            return GI_GVALUE_WRONG_TYPE;
        if (!scm_is_unsigned_integer(obj, 0, G_MAXULONG))
            return GI_GVALUE_OUT_OF_RANGE;
        guint val = 0;
        val = scm_to_ulong(obj);
        g_value_set_flags(value, val);
        return 0;
    }
        break;
    case G_TYPE_FLOAT:
    {
        if (!scm_is_true(scm_real_p(obj)))
            return GI_GVALUE_WRONG_TYPE;
        gdouble dval = scm_to_double(obj);
        if (dval < -G_MAXFLOAT || dval > G_MAXFLOAT)
            return GI_GVALUE_OUT_OF_RANGE;
        g_value_set_float(value, dval);
        return 0;
    }
    case G_TYPE_DOUBLE:
    {
        if (!scm_is_true(scm_real_p(obj)))
            return GI_GVALUE_WRONG_TYPE;
        gdouble temp;
        temp = scm_to_double(obj);
        g_value_set_double(value, temp);
        return 0;
    }
    case G_TYPE_STRING:
    {
        if (!scm_is_string(obj))
            return GI_GVALUE_WRONG_TYPE;
        gchar *temp = scm_to_utf8_string(obj);
        g_value_take_string(value, temp);
        return 0;
    }
    case G_TYPE_POINTER:
    {
        if (SCM_POINTER_P(obj))
            g_value_set_pointer(value, scm_to_pointer(obj));
        else if (scm_is_true(scm_bytevector_p(obj)))
            g_value_set_pointer(value, SCM_BYTEVECTOR_CONTENTS(obj));
        else if (gir_type_get_gtype_from_obj(obj) > G_TYPE_INVALID)
            g_value_set_object(value, scm_foreign_object_ref(obj, OBJ_SLOT));
        else
            return GI_GVALUE_WRONG_TYPE;
    }

    case G_TYPE_INTERFACE:
        /* we only handle interface types that have a GObject prereq */
        if (g_type_is_a(value_type, G_TYPE_OBJECT)) {
            if (scm_is_false(obj)) {
                g_value_set_object(value, NULL);
                return 0;
            }
            else if (!SCM_IS_A_P(obj, gi_gobject_type))
                return GI_GVALUE_WRONG_TYPE;
            else if (!G_TYPE_CHECK_INSTANCE_TYPE(gi_gobject_get_obj(obj), value_type))
                return GI_GVALUE_WRONG_TYPE;
            else {
                g_value_set_object(value, gi_gobject_get_obj(obj));
                return 0;
            }
        }
        else
            return GI_GVALUE_WRONG_TYPE;
        break;
    default:
        g_critical("unhandled value type");
        return GI_GVALUE_WRONG_TYPE;
        break;
    }

    return 0;
}

void
gi_gvalue_from_scm_with_error(const char *subr, GValue *value, SCM obj, int pos)
{
    int res = gi_gvalue_from_scm(value, obj);
    switch (res) {
    case 0:
        return;
    case GI_GVALUE_WRONG_TYPE:
        scm_wrong_type_arg(subr, pos, obj);
        break;
    case GI_GVALUE_OUT_OF_RANGE:
        scm_out_of_range_pos(subr, obj, scm_from_int(pos));
    }
}


SCM
gi_param_gvalue_as_scm(const GValue *gvalue, gboolean copy_boxed, const GParamSpec *pspec)
{
    if (G_IS_PARAM_SPEC_UNICHAR(pspec)) {
        scm_t_wchar u;

        u = g_value_get_uint(gvalue);
        return SCM_MAKE_CHAR(u);
    }
    else
        return gi_gvalue_as_scm(gvalue, copy_boxed);

}


GIArgument
gi_giargument_from_g_value(const GValue *value, GITypeInfo *type_info)
{
    GIArgument arg = { 0, };

    GITypeTag type_tag = g_type_info_get_tag(type_info);

    /* For the long handling: long can be equivalent to
     * int32 or int64, depending on the architecture, but
     * gi doesn't tell us (and same for ulong)
     */
    switch (type_tag) {
    case GI_TYPE_TAG_BOOLEAN:
        arg.v_boolean = g_value_get_boolean(value);
        break;
    case GI_TYPE_TAG_INT8:
        arg.v_int8 = g_value_get_schar(value);
        break;
    case GI_TYPE_TAG_INT16:
    case GI_TYPE_TAG_INT32:
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_LONG))
            arg.v_int32 = (gint32) g_value_get_long(value);
        else
            arg.v_int32 = (gint32) g_value_get_int(value);
        break;
    case GI_TYPE_TAG_INT64:
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_LONG))
            arg.v_int64 = g_value_get_long(value);
        else
            arg.v_int64 = g_value_get_int64(value);
        break;
    case GI_TYPE_TAG_UINT8:
        arg.v_uint8 = g_value_get_uchar(value);
        break;
    case GI_TYPE_TAG_UINT16:
    case GI_TYPE_TAG_UINT32:
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_ULONG))
            arg.v_uint32 = (guint32) g_value_get_ulong(value);
        else
            arg.v_uint32 = (guint32) g_value_get_uint(value);
        break;
    case GI_TYPE_TAG_UINT64:
        if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_ULONG))
            arg.v_uint64 = g_value_get_ulong(value);
        else
            arg.v_uint64 = g_value_get_uint64(value);
        break;
    case GI_TYPE_TAG_UNICHAR:
        arg.v_uint32 = g_value_get_schar(value);
        break;
    case GI_TYPE_TAG_FLOAT:
        arg.v_float = g_value_get_float(value);
        break;
    case GI_TYPE_TAG_DOUBLE:
        arg.v_double = g_value_get_double(value);
        break;
    case GI_TYPE_TAG_GTYPE:
        arg.v_size = g_value_get_gtype(value);
        break;
    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
        /* Callers are responsible for ensuring the GValue stays alive
         * long enough for the string to be copied. */
        arg.v_string = (char *)g_value_get_string(value);
        break;
    case GI_TYPE_TAG_GLIST:
    case GI_TYPE_TAG_GSLIST:
    case GI_TYPE_TAG_ARRAY:
    case GI_TYPE_TAG_GHASH:
        if (G_VALUE_HOLDS_BOXED(value))
            arg.v_pointer = g_value_get_boxed(value);
        else
            /* e. g. GSettings::change-event */
            arg.v_pointer = g_value_get_pointer(value);
        break;
    case GI_TYPE_TAG_INTERFACE:
    {
        GIBaseInfo *info;
        GIInfoType info_type;

        info = g_type_info_get_interface(type_info);
        info_type = g_base_info_get_type(info);

        g_base_info_unref(info);

        switch (info_type) {
        case GI_INFO_TYPE_FLAGS:
            arg.v_uint = g_value_get_flags(value);
            break;
        case GI_INFO_TYPE_ENUM:
            arg.v_int = g_value_get_enum(value);
            break;
        case GI_INFO_TYPE_INTERFACE:
        case GI_INFO_TYPE_OBJECT:
            if (G_VALUE_HOLDS_PARAM(value))
                arg.v_pointer = g_value_get_param(value);
            else
                arg.v_pointer = g_value_get_object(value);
            break;
        case GI_INFO_TYPE_BOXED:
        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_UNION:
            if (G_VALUE_HOLDS(value, G_TYPE_BOXED)) {
                arg.v_pointer = g_value_get_boxed(value);
            }
            else if (G_VALUE_HOLDS(value, G_TYPE_VARIANT)) {
                arg.v_pointer = g_value_get_variant(value);
            }
            else if (G_VALUE_HOLDS(value, G_TYPE_POINTER)) {
                arg.v_pointer = g_value_get_pointer(value);
            }
            else {
                /* PyErr_Format (PyExc_NotImplementedError, */
                /*               "Converting GValue's of type '%s' is not implemented.", */
                /*               g_type_name (G_VALUE_TYPE (value))); */
                g_error("Converting GValue's of type '%s' is not implemented.",
                        g_type_name(G_VALUE_TYPE(value)));
            }
            break;
        default:
            /* PyErr_Format (PyExc_NotImplementedError, */
            /*               "Converting GValue's of type '%s' is not implemented.", */
            /*               g_info_type_to_string (info_type)); */
            g_error("Converting GValue's of type '%s' is not implemented.",
                    g_info_type_to_string(info_type));

            break;
        }
        break;
    }
    case GI_TYPE_TAG_ERROR:
        arg.v_pointer = g_value_get_boxed(value);
        break;
    case GI_TYPE_TAG_VOID:
        arg.v_pointer = g_value_get_pointer(value);
        break;
    default:
        break;
    }

    return arg;
}

static int
gi_gvalue_array_from_scm_list(GValue *value, SCM list)
{
    ssize_t len, i;
    GArray *array;

    len = scm_to_size_t(scm_length(list));

    array = g_array_new(FALSE, TRUE, sizeof(GValue));

    for (i = 0; i < len; ++i) {
        SCM item = scm_list_ref(list, scm_from_size_t(i));
        GType type;
        GValue item_value = { 0, };

        type = gir_type_get_gtype_from_obj(item);

        g_value_init(&item_value, type);
        gi_gvalue_from_scm(&item_value, item);

        g_array_append_val(array, item_value);
    }

    g_value_take_boxed(value, array);
    return 0;
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
gi_gvalue_to_scm_basic_type(const GValue *value, GType fundamental, gboolean *handled)
{
    *handled = TRUE;
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
        /* return gi_genum_from_gtype (G_VALUE_TYPE (value), */
        /*                             g_value_get_enum (value)); */
        return scm_from_ulong(g_value_get_enum(value));
    case G_TYPE_FLAGS:
        /* return gi_gflags_from_gtype (G_VALUE_TYPE (value), */
        /*                              g_value_get_flags (value)); */
        return scm_from_ulong(g_value_get_flags(value));
    case G_TYPE_FLOAT:
        return scm_from_double(g_value_get_float(value));
    case G_TYPE_DOUBLE:
        return scm_from_double(g_value_get_double(value));
    case G_TYPE_STRING:
    {
        const gchar *str = g_value_get_string(value);
        if (str)
            return scm_from_utf8_string(str);
        else
            return SCM_BOOL_F;
    }
    default:
        *handled = FALSE;
        return SCM_BOOL_F;
    }
    g_return_val_if_reached(SCM_BOOL_F);
}

// This function creates and returns a Scheme value that
// represents the GValue passed as an argument.
static SCM
gi_gvalue_to_scm_structured_type(const GValue *value, GType fundamental, gboolean copy_boxed)
{
    switch (fundamental) {
    case G_TYPE_INTERFACE:
    {
        gpointer obj = g_value_get_object(value);
        if (!obj)
            return SCM_BOOL_F;
        else if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
            return gir_type_make_object(G_VALUE_TYPE(value), obj, 0);
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
            return gir_type_make_object(G_VALUE_TYPE(value), pspec, 0);
        else
            return SCM_BOOL_F;
    }

    case G_TYPE_BOXED:
    {
        if (G_VALUE_HOLDS(value, G_TYPE_VALUE)) {
            GValue *n_value = g_value_get_boxed(value);
            return gi_gvalue_as_scm(n_value, copy_boxed);
        }
        else if (G_VALUE_HOLDS(value, G_TYPE_GSTRING)) {
            GString *string = (GString *)g_value_get_boxed(value);
            return scm_from_utf8_stringn(string->str, string->len);
        }
        else {
            return gir_type_make_object(G_VALUE_TYPE(value), g_value_get_boxed(value), copy_boxed);
        }
    }

    case G_TYPE_OBJECT:
    {
        gpointer obj = g_value_get_object(value);
        if (obj)
            return gir_type_make_object(G_VALUE_TYPE(value), obj, 0);
        else
            return SCM_BOOL_F;
    }

#if 0
    case G_TYPE_VARIANT:
    {
        GVariant *v = g_value_get_variant(value);
        if (v == NULL) {
            Py_INCREF(Py_None);
            return Py_None;
        }
        return pygi_struct_new_from_g_type(G_TYPE_VARIANT, g_variant_ref(v), FALSE);
    }
#endif
    default:
    {
        // g_assert_not_reached ();
        /* PyGTypeMarshal *bm; */
        /* if ((bm = pyg_type_lookup(G_VALUE_TYPE(value)))) */
        /*  return bm->fromvalue(value); */
        break;
    }
    }

    const char *type_name = g_type_name(G_VALUE_TYPE(value));
    if (type_name == NULL)
        type_name = "(null)";
    scm_misc_error("gi_gvalue_to_scm", "unknown type ~S",
                   scm_list_1(scm_from_utf8_string(type_name)));
    g_return_val_if_reached(SCM_BOOL_F);
}


/* Returns an SCM version of the GValue.  If COPY_BOXED,
   try to make a deep copy of the object. */
SCM
gi_gvalue_as_scm(const GValue *value, gboolean copy_boxed)
{
    SCM guobj;
    gboolean handled;
    GType fundamental = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value));

#if 0
    if (fundamental == G_TYPE_CHAR)
        return SCM_MAKE_CHAR(g_value_get_schar(value));
    else if (fundamental == G_TYPE_UCHAR)
        return SCM_MAKE_CHAR(g_value_get_uchar(value));
#endif

    guobj = gi_gvalue_to_scm_basic_type(value, fundamental, &handled);
    if (!handled)
        guobj = gi_gvalue_to_scm_structured_type(value, fundamental, copy_boxed);
    return guobj;
}


static SCM
scm_gvalue_set_x(SCM self, SCM x)
{
    GValue *val;

    if (!SCM_IS_A_P(self, gi_gvalue_type))
        scm_wrong_type_arg_msg("gvalue-set!", SCM_ARG1, self, "GValue");

    val = gi_gvalue_get_value(self);
    if (val)
        gi_gvalue_from_scm_with_error("gvalue_set!", val, x, SCM_ARG2);
    return SCM_UNSPECIFIED;
}

static SCM
scm_gvalue_get(SCM self)
{
    GValue *val;

    scm_assert_foreign_object_type(gi_gvalue_type, self);
    val = gi_gvalue_get_value(self);
    return gi_gvalue_as_scm(val, TRUE);
}

static SCM
scm_make_gvalue(SCM gtype)
{
    GType type;
    GValue *val;

    //scm_assert_foreign_object_type (gi_gtype_type, gtype);

    type = scm_to_gtype(gtype);
    val = g_new0(GValue, 1);
    g_value_init(val, type);
    return gi_gvalue_c2g(val);

}

static SCM
scm_gvalue_type_name(SCM self)
{
    GValue *val;
    const gchar *name;

    if (!SCM_IS_A_P(self, gi_gvalue_type))
        scm_wrong_type_arg_msg("gvalue-type-name", SCM_ARG1, self, "GValue");

    val = gi_gvalue_get_value(self);
    if (val) {
        name = G_VALUE_TYPE_NAME(val);
        if (name)
            return scm_from_utf8_string(name);
        else
            return scm_from_utf8_string("(unknown)");
    }
    g_return_val_if_reached(SCM_BOOL_F);
}

static SCM
scm_gvalue_to_gtype(SCM self)
{
    GValue *val;
    GType type;

    if (!SCM_IS_A_P(self, gi_gvalue_type))
        scm_wrong_type_arg_msg("gvalue->gtype", SCM_ARG1, self, "GValue");

    val = gi_gvalue_get_value(self);
    if (val) {
        type = G_VALUE_TYPE(val);
        gir_type_register(type);
        return scm_from_size_t(type);
    }
    return SCM_BOOL_F;
}

static SCM
scm_gvalue_holds_p(SCM self, SCM gtype)
{
    GValue *val;
    GType type;
    gboolean ret;

    if (!SCM_IS_A_P(self, gi_gvalue_type))
        scm_wrong_type_arg_msg("gvalue-holds?", SCM_ARG1, self, "GValue");

    val = gi_gvalue_get_value(self);
    type = scm_to_gtype(gtype);
    if (val) {
        ret = G_VALUE_HOLDS(val, type);
        return scm_from_bool(ret);
    }
    return SCM_BOOL_F;
}

static SCM
scm_gvalue_valid_p(SCM self)
{
    GValue *val;
    gboolean ret;

    scm_assert_foreign_object_type(gi_gvalue_type, self);
    val = gi_gvalue_get_value(self);
    if (val) {
        ret = G_IS_VALUE(val);
        return scm_from_bool(ret);
    }
    return SCM_BOOL_F;
}

void
gi_init_gvalue(void)
{
    gi_init_gvalue_type();

    scm_c_define_gsubr("gvalue-get", 1, 0, 0, scm_gvalue_get);
    scm_c_define_gsubr("gvalue-set!", 2, 0, 0, scm_gvalue_set_x);
    scm_c_define_gsubr("make-gvalue", 1, 0, 0, scm_make_gvalue);
    scm_c_define_gsubr("gvalue-type-name", 1, 0, 0, scm_gvalue_type_name);
    scm_c_define_gsubr("gvalue->gtype", 1, 0, 0, scm_gvalue_to_gtype);
    scm_c_define_gsubr("gvalue-holds?", 2, 0, 0, scm_gvalue_holds_p);
    scm_c_define_gsubr("gvalue-valid?", 1, 0, 0, scm_gvalue_valid_p);

    scm_c_export("make-gvalue",
                 "gvalue-type-name", "gvalue->gtype", "gvalue-holds?", "gvalue-valid?", NULL);
}
