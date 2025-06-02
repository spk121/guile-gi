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
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <libguile.h>
#include <glib.h>
#include "../core.h"
#include "../type.h"
#include "../gig_glib.h"
#include "gig_argument.h"
#include "gig_callback_priv.h"

#define TRACE_C2S() gig_debug_transfer("[C2S] On line %d while handing %s of %s.", __LINE__, gig_type_meta_describe(meta), subr)
#define TRACE_S2C() gig_debug_transfer("[S2C] On line %d while handing %s of %s.", __LINE__, gig_type_meta_describe(meta), subr)

#define SURPRISING                                                      \
    do {                                                                \
        gig_warning("Unusual argument type '%s' %s:%d", gig_type_meta_describe(meta), __FILE__, __LINE__); \
    } while(false)

#define UNHANDLED                                                       \
    do {                                                                \
        gig_error("unhandled argument type '%s' %s:%d", gig_type_meta_describe(meta), __FILE__, __LINE__); \
    } while(false)

static void *later_free(slist_t **must_free, GigTypeMeta *meta, void *ptr);

// Fundamental types
static void scm_to_c_interface(S2C_ARG_DECL);
static void scm_to_c_char(S2C_ARG_DECL);
static void scm_to_c_boolean(S2C_ARG_DECL);
static void scm_to_c_integer(S2C_ARG_DECL);
static void scm_to_c_enum(S2C_ARG_DECL);
static void scm_to_c_real(S2C_ARG_DECL);
static void scm_to_c_string(S2C_ARG_DECL);
static void scm_to_c_pointer(S2C_ARG_DECL);
static void scm_to_c_boxed(S2C_ARG_DECL);
// static void scm_to_c_param(S2C_ARG_DECL);
static void scm_to_c_object(S2C_ARG_DECL);
static void scm_to_c_unichar(S2C_ARG_DECL);
static void scm_to_c_variant(S2C_ARG_DECL);

// Derived types
static void scm_to_c_native_array(S2C_ARG_DECL);
static void scm_to_c_native_unichar_array(S2C_ARG_DECL);
static void scm_to_c_native_boolean_array(S2C_ARG_DECL);
static void scm_to_c_native_immediate_array(S2C_ARG_DECL);
static void scm_to_c_native_string_array(S2C_ARG_DECL);
static void scm_to_c_native_interface_array(S2C_ARG_DECL);
static void scm_to_c_native_gtype_array(S2C_ARG_DECL);
static void scm_to_c_garray(S2C_ARG_DECL);
static void scm_to_c_byte_array(S2C_ARG_DECL);
static void scm_to_c_ptr_array(S2C_ARG_DECL);
static void scm_to_c_ghashtable(S2C_ARG_DECL);

// Fundamental types
static void c_interface_to_scm(C2S_ARG_DECL);
static void c_char_to_scm(C2S_ARG_DECL);
static void c_boolean_to_scm(C2S_ARG_DECL);
static void c_integer_to_scm(C2S_ARG_DECL);
static void c_unichar_to_scm(C2S_ARG_DECL);
static void c_enum_to_scm(C2S_ARG_DECL);
static void c_real_to_scm(C2S_ARG_DECL);
static void c_string_to_scm(C2S_ARG_DECL);
static void c_pointer_to_scm(C2S_ARG_DECL);
static void c_boxed_to_scm(C2S_ARG_DECL);
static void c_param_to_scm(C2S_ARG_DECL);
static void c_object_to_scm(C2S_ARG_DECL);
static void c_variant_to_scm(C2S_ARG_DECL);

// Derived types
static void c_byte_array_to_scm(C2S_ARG_DECL);
static void c_native_array_to_scm(C2S_ARG_DECL);
static void c_garray_to_scm(C2S_ARG_DECL);
static void c_gptrarray_to_scm(C2S_ARG_DECL);
static void c_ghashtable_to_scm(C2S_ARG_DECL);
static void c_list_to_scm(C2S_ARG_DECL);

// Use this to register allocated data to be freed after use.
static void *
later_free(slist_t **must_free, GigTypeMeta *meta, void *ptr)
{
    if ((must_free != NULL) && meta->transfer != GIG_TRANSFER_EVERYTHING)
        slist_prepend(must_free, ptr);
    return ptr;
}

#define LATER_FREE(_ptr) later_free(must_free, meta, _ptr)

static GType
child_type(GigTypeMeta *meta, GigArgument *arg)
{
    if ((arg != NULL) && (arg->v_pointer != NULL)
        && G.type_check_instance_is_fundamentally_a(arg->v_pointer, G_TYPE_OBJECT)
        && G.type_is_a(G_OBJECT_TYPE(arg->v_pointer), meta->gtype)) {
        return G_OBJECT_TYPE(arg->v_pointer);
    }
    return meta->gtype;
}

// This returns the number of elements (not necessarily bytes) in ARG.
static size_t
zero_terminated_array_length(GigTypeMeta *meta, GigArgument *arg)
{
    assert(meta != NULL);
    assert(arg != NULL);

    if (arg->v_pointer == NULL)
        return 0;
    else if (meta->params[0].arg_type == GIG_ARG_TYPE_UTF8_STRING
             || meta->params[0].arg_type == GIG_ARG_TYPE_LOCALE_STRING)
        return strvlen((const char **)arg->v_pointer);
    else {
        size_t item_size = gig_meta_real_item_size(&meta->params[0]);

        assert(item_size > 0);

        switch (item_size) {
        case 0:
            assert_not_reached();
        case 1:
            return strlen(arg->v_string);
        case 2:
        {
            int16_t *ptr = arg->v_pointer;
            size_t length = 0;
            while (*ptr++ != 0)
                length++;
            return length;
        }
        case 4:
        {
            int32_t *ptr = arg->v_pointer;
            size_t length = 0;
            while (*ptr++ != 0)
                length++;
            return length;
        }
        case 8:
        {
            int64_t *ptr = arg->v_pointer;
            size_t length = 0;
            while (*ptr++ != 0)
                length++;
            return length;
        }
        default:
        {
            char *ptr = arg->v_pointer;
            bool non_null;
            size_t length = -1;
            do {
                length++;
                non_null = false;
                for (size_t i = 0; i <= item_size; i++)
                    if (ptr + i != 0) {
                        non_null = true;
                        break;
                    }
                ptr += item_size;
            } while (non_null);

            return length;
        }
        }
    }
    gig_return_val_if_reached(GIG_ARRAY_SIZE_UNKNOWN);
}

//////////////////////////////////////////////////////////
// CONVERTING SCM OBJECTS TO GIGARGUMENTS
//////////////////////////////////////////////////////////

// This is the main entry point of the conversion of SCM objects to
// GigArguments.
void
gig_argument_scm_to_c(S2C_ARG_DECL)
{
    TRACE_S2C();
    arg->v_pointer = NULL;
    if (size)
        *size = 0;

    switch (meta->arg_type) {
    case GIG_ARG_TYPE_UNKNOWN:
        UNHANDLED;
        break;
    case GIG_ARG_TYPE_VOID:
        SURPRISING;
        arg->v_pointer = NULL;
        break;
    case GIG_ARG_TYPE_INTERFACE:
    case GIG_ARG_TYPE_OTHER:
    case GIG_ARG_TYPE_PARAM:
        scm_to_c_interface(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_INT8:
    case GIG_ARG_TYPE_UINT8:
        scm_to_c_char(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_GBOOLEAN:
        scm_to_c_boolean(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_INT16:
    case GIG_ARG_TYPE_UINT16:
    case GIG_ARG_TYPE_INT32:
    case GIG_ARG_TYPE_UINT32:
    case GIG_ARG_TYPE_INT64:
    case GIG_ARG_TYPE_UINT64:
        scm_to_c_integer(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_UNICHAR:
        scm_to_c_unichar(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_ENUM:
    case GIG_ARG_TYPE_FLAGS:
        scm_to_c_enum(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_FLOAT:
    case GIG_ARG_TYPE_DOUBLE:
        scm_to_c_real(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_UTF8_STRING:
    case GIG_ARG_TYPE_LOCALE_STRING:
        scm_to_c_string(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_ARRAY:
    case GIG_ARG_TYPE_CALLBACK:
    case GIG_ARG_TYPE_GLIST:
    case GIG_ARG_TYPE_GSLIST:
    case GIG_ARG_TYPE_GTYPE:
    case GIG_ARG_TYPE_POINTER:
        scm_to_c_pointer(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_BOXED:
    case GIG_ARG_TYPE_GARRAY:
    case GIG_ARG_TYPE_GBYTEARRAY:
    case GIG_ARG_TYPE_GHASH:
    case GIG_ARG_TYPE_GPTRARRAY:
    case GIG_ARG_TYPE_VALUE:
        scm_to_c_boxed(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_OBJECT:
        scm_to_c_object(S2C_ARGS);
        break;
    case GIG_ARG_TYPE_VARIANT:
        scm_to_c_variant(S2C_ARGS);
        break;
    default:
        UNHANDLED;
        break;
    }
}

static void
scm_to_c_interface(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->is_nullable && scm_is_false(object))
        arg->v_pointer = NULL;
    else
        arg->v_pointer = G.type_check_instance_cast(gig_type_peek_object(object), meta->gtype);
}

static void
scm_to_c_char(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgType t = meta->arg_type;

    if (!scm_is_integer(object) && !scm_is_char(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "int8");

    if (t == GIG_ARG_TYPE_INT8) {
        if (scm_is_char(object)) {
            if (SCM_CHAR(object) > 255)
                scm_out_of_range(subr, object);
            else
                arg->v_int8 = (uint8_t)SCM_CHAR(object);
        }
        else if (!scm_is_signed_integer(object, INT8_MIN, INT8_MAX))
            scm_out_of_range(subr, object);
        else
            arg->v_int8 = scm_to_int8(object);
    }
    else if (t == GIG_ARG_TYPE_UINT8) {
        if (scm_is_char(object)) {
            if (SCM_CHAR(object) > 255)
                scm_out_of_range(subr, object);
            else
                arg->v_uint8 = (uint8_t)SCM_CHAR(object);
        }
        else if (!scm_is_unsigned_integer(object, 0, UINT8_MAX))
            scm_out_of_range(subr, object);
        else
            arg->v_uint8 = scm_to_uint8(object);
    }
}

static void
scm_to_c_boolean(S2C_ARG_DECL)
{
    TRACE_S2C();

    if (!scm_is_boolean(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "boolean");
    arg->v_boolean = scm_is_true(object);
}

static void
scm_to_c_integer(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgType t = meta->arg_type;
    if (!scm_is_integer(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "integer");

#define T(t,min,max)                                                    \
    do {                                                                \
        if (!scm_is_signed_integer(object, min, max))                   \
            scm_out_of_range(subr, object);                             \
        arg->v_ ## t = scm_to_ ## t(object);                            \
    } while (0)

    if (t == GIG_ARG_TYPE_INT16)
        T(int16, INT16_MIN, INT16_MAX);
    else if (t == GIG_ARG_TYPE_INT32)
        T(int32, INT32_MIN, INT32_MAX);
    else if (t == GIG_ARG_TYPE_INT64)
        T(int64, INT64_MIN, INT64_MAX);

#undef T
#define T(t,min,max)                                                    \
    do {                                                                \
        if (!scm_is_unsigned_integer(object, min, max))                 \
            scm_out_of_range(subr, object);                             \
        arg->v_ ## t = scm_to_ ## t(object);                            \
    } while (0)

    if (t == GIG_ARG_TYPE_UINT16)
        T(uint16, 0, UINT16_MAX);
    else if (t == GIG_ARG_TYPE_UINT32)
        T(uint32, 0, UINT32_MAX);
    else if (t == GIG_ARG_TYPE_UINT64)
        T(uint64, 0, UINT64_MAX);
#undef T
}

static void
scm_to_c_unichar(S2C_ARG_DECL)
{
    if (!scm_is_char(object) && !scm_is_integer(object))
        scm_wrong_type_arg(subr, argpos, object);
    if (scm_is_char(object))
        arg->v_uint32 = SCM_CHAR(object);
    else if (scm_is_unsigned_integer(object, 0, SCM_CODEPOINT_MAX))
        arg->v_uint32 = scm_to_uint32(object);
    else
        scm_out_of_range(subr, object);
}

static void
scm_to_c_enum(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->arg_type == GIG_ARG_TYPE_ENUM)
        arg->v_int32 = gig_enum_to_int(object);
    else
        arg->v_uint32 = gig_flags_to_uint(object);
}

static void
scm_to_c_real(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgType t = meta->arg_type;

    if (!scm_is_real(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "real");

    if (t == GIG_ARG_TYPE_FLOAT) {
        double dtmp = scm_to_double(object);
        if (dtmp > FLT_MAX || dtmp < -FLT_MAX)
            scm_out_of_range(subr, object);
        arg->v_float = (float)dtmp;
    }
    else
        arg->v_double = scm_to_double(object);
}

static void
scm_to_c_string(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->is_nullable && scm_is_false(object)) {
        arg->v_pointer = NULL;
        return;
    }

    if (!scm_is_string(object) && !scm_is_bytevector(object))
        scm_wrong_type_arg(subr, argpos, object);

    // Here we convert an input string into an argument.  The input
    // string is either UTF8 or locale encoded.
    if (scm_is_bytevector(object)) {
        // Some methods expect passed-in strings to be overwriteable,
        // like g_date_strftime(char *s, ...) expects 's' to be a
        // place to store an output.  Since Glib strings and Guile
        // strings have no encoding in common, we can use
        // bytevectors...

        // FIXME: is there any guarantee, that the bytevector
        // has the correct locale? This seems like a very ugly hack.
        if (meta->transfer != GIG_TRANSFER_EVERYTHING) {
            // But when we're using bytevectors as a possibly writable
            // location, they do need to be null terminated.
            bool terminated = false;
            for (size_t i = 0; i < SCM_BYTEVECTOR_LENGTH(object); i++)
                if (SCM_BYTEVECTOR_CONTENTS(object)[i] == 0)
                    terminated = true;
            if (!terminated)
                scm_misc_error(subr, "input bytevector is not null-terminated", SCM_EOL);
            arg->v_string = (char *)SCM_BYTEVECTOR_CONTENTS(object);
        }
        else
            // But when we're copying the contents of the string, the
            // null termination can be enforced here.
            arg->v_string = xstrndup((const char *)SCM_BYTEVECTOR_CONTENTS(object),
                                     SCM_BYTEVECTOR_LENGTH(object));
    }
    else {
        if (meta->arg_type == GIG_ARG_TYPE_LOCALE_STRING)
            arg->v_string = scm_to_locale_string(object);
        else
            arg->v_string = scm_to_utf8_string(object);
        LATER_FREE(arg->v_string);
    }
}

static void
scm_to_c_pointer(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (scm_is_false(object) && meta->is_nullable)
        arg->v_pointer = NULL;
    else if (meta->arg_type == GIG_ARG_TYPE_ARRAY)
        scm_to_c_native_array(S2C_ARGS);
    else if (meta->arg_type == GIG_ARG_TYPE_CALLBACK) {
        if (scm_is_procedure(object))
            arg->v_pointer = gig_callback_to_c(subr, meta->callable_arg_map, object);
        else
            scm_wrong_type_arg_msg(subr, argpos, object, "a procedure");
    }
    else if (meta->arg_type == GIG_ARG_TYPE_GTYPE)
        arg->v_size = scm_to_gtype_full(object, subr, argpos);
    else if (scm_is_pointer(object))
        arg->v_pointer = scm_to_pointer(object);
    else if (scm_is_bytevector(object))
        arg->v_pointer = SCM_BYTEVECTOR_CONTENTS(object);
    else
        scm_wrong_type_arg_msg(subr, argpos, object, "a pointer");
}

static void
scm_to_c_boxed(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgType t = meta->arg_type;
    if (meta->is_nullable && scm_is_false(object)) {
        arg->v_pointer = NULL;
    }
    else if (t == GIG_ARG_TYPE_GARRAY)
        scm_to_c_garray(S2C_ARGS);
    else if (t == GIG_ARG_TYPE_GBYTEARRAY)
        scm_to_c_byte_array(S2C_ARGS);
    else if (t == GIG_ARG_TYPE_GPTRARRAY)
        scm_to_c_ptr_array(S2C_ARGS);
    else if (t == GIG_ARG_TYPE_GHASH)
        scm_to_c_ghashtable(S2C_ARGS);
    else if (!meta->is_ptr && !meta->is_caller_allocates)
        scm_misc_error("object->boxed",
                       "passing object ~S by value to a c function is not supported",
                       scm_list_1(scm_from_utf8_string(G.type_name(meta->gtype))));

    else
        arg->v_pointer = gig_type_peek_object(object);
}

static void
scm_to_c_object(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->is_nullable && scm_is_false(object)) {
        arg->v_pointer = NULL;
    }
    else {
        if (!gig_type_check_typed_object(object, scm_from_gtype(meta->gtype)))
            scm_wrong_type_arg_msg(subr, argpos, object, G.type_name(meta->gtype));

        arg->v_pointer = gig_type_peek_object(object);
    }
}

static void
scm_to_c_variant(S2C_ARG_DECL)
{
    TRACE_S2C();
    arg->v_pointer = gig_type_peek_object(object);
}

static void
scm_to_c_native_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgType item_type = meta->params[0].arg_type;

    if (item_type == GIG_ARG_TYPE_GBOOLEAN)
        scm_to_c_native_boolean_array(S2C_ARGS);
    else if (item_type == GIG_ARG_TYPE_UNICHAR)
        scm_to_c_native_unichar_array(S2C_ARGS);
    else if (item_type == GIG_ARG_TYPE_INT8
             || item_type == GIG_ARG_TYPE_UINT8
             || item_type == GIG_ARG_TYPE_INT16
             || item_type == GIG_ARG_TYPE_UINT16
             || item_type == GIG_ARG_TYPE_INT32
             || item_type == GIG_ARG_TYPE_UINT32
             || item_type == GIG_ARG_TYPE_INT64
             || item_type == GIG_ARG_TYPE_UINT64
             || item_type == GIG_ARG_TYPE_FLOAT
             || item_type == GIG_ARG_TYPE_DOUBLE || item_type == GIG_ARG_TYPE_POINTER)
        scm_to_c_native_immediate_array(S2C_ARGS);
    else if (item_type == GIG_ARG_TYPE_GTYPE)
        scm_to_c_native_gtype_array(S2C_ARGS);
    else if (item_type == GIG_ARG_TYPE_UTF8_STRING || item_type == GIG_ARG_TYPE_LOCALE_STRING)
        scm_to_c_native_string_array(S2C_ARGS);
    else if (item_type == GIG_ARG_TYPE_BOXED
             || item_type == GIG_ARG_TYPE_VARIANT
             || item_type == GIG_ARG_TYPE_ENUM || item_type == GIG_ARG_TYPE_FLAGS)
        scm_to_c_native_interface_array(S2C_ARGS);
    else
        UNHANDLED;
}

static void
scm_to_c_native_boolean_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    // For booleans, we expect a vector of booleans
    if (!scm_is_vector(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "vector of booleans");
    *size = scm_c_vector_length(object);
    if (meta->is_zero_terminated) {
        arg->v_pointer = malloc(sizeof(gboolean) * (*size + 1));
        ((gboolean *)arg->v_pointer)[*size] = 0;
        LATER_FREE(arg->v_pointer);
    }
    else {
        arg->v_pointer = malloc(sizeof(gboolean) * *size);
        LATER_FREE(arg->v_pointer);
    }
    for (size_t i = 0; i < *size; i++)
        ((gboolean *)(arg->v_pointer))[i] = (gboolean)scm_is_true(scm_c_vector_ref(object, i));
}

static void
scm_to_c_native_unichar_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    // When asked to convert an SCM to an array of unichars,
    // we expect that SCM to be a string.
    if (!scm_is_string(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "string");
    *size = scm_c_string_length(object);
    if (meta->is_zero_terminated) {
        arg->v_pointer = malloc(sizeof(gunichar) * (*size + 1));
        ((gunichar *)arg->v_pointer)[*size] = 0;
        LATER_FREE(arg->v_pointer);
    }
    else {
        arg->v_pointer = malloc(sizeof(gunichar) * *size);
        LATER_FREE(arg->v_pointer);
    }
    for (size_t i = 0; i < *size; i++)
        ((gunichar *)(arg->v_pointer))[i] = (gunichar)SCM_CHAR(scm_c_string_ref(object, i));
}

static void
scm_to_c_native_immediate_array(S2C_ARG_DECL)
{
    TRACE_S2C();
#define FUNC_NAME "%object->c-native-immediate-array-arg"
    // IMMEDIATE TYPES.  It seems only double, and 8 and 32-bit
    // integer arrays are commonly used. Sometimes deep copy.
    // Sometimes zero terminated.

    if (!scm_is_bytevector(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "bytevector");

    GigArgType t = meta->params[0].arg_type;
    size_t item_size;
    if (t == GIG_ARG_TYPE_INT8 || t == GIG_ARG_TYPE_UINT8)
        item_size = 1;
    else if (t == GIG_ARG_TYPE_INT16 || t == GIG_ARG_TYPE_UINT16)
        item_size = 2;
    else if (t == GIG_ARG_TYPE_INT32 || t == GIG_ARG_TYPE_UINT32)
        item_size = 4;
    else if (t == GIG_ARG_TYPE_INT64 || t == GIG_ARG_TYPE_UINT64)
        item_size = 8;
    else if (t == GIG_ARG_TYPE_FLOAT)
        item_size = 4;
    else if (t == GIG_ARG_TYPE_DOUBLE)
        item_size = 8;
    else
        item_size = sizeof(void *);

    *size = SCM_BYTEVECTOR_LENGTH(object) / item_size;
    if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
        if (meta->is_zero_terminated) {
            size_t len = SCM_BYTEVECTOR_LENGTH(object);
            // Note, null terminated here.
            arg->v_pointer = xcalloc(*size + 1, item_size);
            memcpy(arg->v_pointer, SCM_BYTEVECTOR_CONTENTS(object), len);
        }
        else {
            arg->v_pointer = xmemdup(SCM_BYTEVECTOR_CONTENTS(object),
                                     SCM_BYTEVECTOR_LENGTH(object));
        }
    }
    else {
        if (meta->is_zero_terminated) {
            size_t len = SCM_BYTEVECTOR_LENGTH(object);
            // Adding null terminator element.
            arg->v_pointer = xcalloc(*size + 1, item_size);
            LATER_FREE(arg->v_pointer);
            memcpy(arg->v_pointer, SCM_BYTEVECTOR_CONTENTS(object), len);
        }
        else
            // The fast path
            arg->v_pointer = SCM_BYTEVECTOR_CONTENTS(object);
    }
#undef FUNC_NAME
}

static void
scm_to_c_byte_array(S2C_ARG_DECL)
{
    TRACE_S2C();

    if (!scm_is_bytevector(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "bytevector");

    void *contents = SCM_BYTEVECTOR_CONTENTS(object);
    size_t len = SCM_BYTEVECTOR_LENGTH(object);
    *size = len;
    if (meta->transfer == GIG_TRANSFER_EVERYTHING)
        arg->v_pointer = G.byte_array_new_take(contents, len);
    else
        arg->v_pointer = G.byte_array_new_take(xmemdup(contents, len), len);
}

static void
scm_to_c_ptr_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgument _arg;
    GigTypeMeta _meta = *meta;

    _meta.arg_type = GIG_ARG_TYPE_ARRAY;

    // Make a C array from this SCM.
    gig_argument_scm_to_c(subr, argpos, &_meta, object, NULL, &_arg, size);

    // Move the pointers inside of the C array into a GPtrArray.
    GPtrArray *ptrarray = G.ptr_array_new();
    for (size_t i = 0; i < *size; i++)
        G.ptr_array_add(ptrarray, ((void **)(_arg.v_pointer))[i]);
    arg->v_pointer = ptrarray;
    assert(arg->v_pointer != NULL);

    // Free the C array without clobbering the pointers it used to
    // contain.
    free(_arg.v_pointer);
}

typedef enum _gig_hash_key_type
{
    GIG_HASH_INVALID,
    GIG_HASH_INT,
    GIG_HASH_INT64,
    GIG_HASH_REAL,
    GIG_HASH_STRING,
    GIG_HASH_POINTER
} GigHashKeyType;

// This procedure converts a GHashTable's key or value to a
// GigArgument.
static void
c_hash_pointer_to_arg(GigTypeMeta *meta, void **p, GigArgument *arg)
{
    GigArgType t = meta->arg_type;

    if (!meta->is_ptr) {
        // 4-byte INT types are packed into the pointer storage,
        // but with intptr_t sign extension.
        if (t == GIG_ARG_TYPE_INT8)
            arg->v_int8 = GPOINTER_TO_INT(p);
        else if (t == GIG_ARG_TYPE_UINT8)
            arg->v_uint8 = GPOINTER_TO_INT(p);
        else if (t == GIG_ARG_TYPE_INT16)
            arg->v_int16 = GPOINTER_TO_INT(p);
        else if (t == GIG_ARG_TYPE_UINT16)
            arg->v_uint16 = GPOINTER_TO_INT(p);
        else if (t == GIG_ARG_TYPE_INT32)
            arg->v_int32 = GPOINTER_TO_INT(p);
        else if (t == GIG_ARG_TYPE_UINT32)
            arg->v_uint32 = GPOINTER_TO_INT(p);
        // 8-byte INT, INT64, DOUBLE and FLOAT are stored by
        // reference, even if they would fit in a pointer.
        else if (t == GIG_ARG_TYPE_INT64)
            arg->v_int64 = *(int64_t *)p;
        else if (t == GIG_ARG_TYPE_UINT64)
            arg->v_uint64 = *(uint64_t *)p;
        else if (t == GIG_ARG_TYPE_FLOAT)
            arg->v_float = *(float *)p;
        else if (t == GIG_ARG_TYPE_DOUBLE)
            arg->v_double = *(double *)p;
    }
    else {
        if (t == GIG_ARG_TYPE_UTF8_STRING || t == GIG_ARG_TYPE_LOCALE_STRING)
            arg->v_string = (char *)p;
        else
            arg->v_pointer = p;
    }
}

// This procedure converts a GigArgument to a gpointer to be used as a
// GHashTable's key or value.
static void *
arg_to_c_hash_pointer(GigTypeMeta *meta, GigHashKeyType key_type, GigArgument *arg)
{
    GigArgType t = meta->arg_type;

    if (key_type == GIG_HASH_INT) {
        // GHashTables apparently expect that negative integers have
        // intptr_t sign extension.
        if (t == GIG_ARG_TYPE_INT8)
            return GINT_TO_POINTER(arg->v_int8);
        else if (t == GIG_ARG_TYPE_INT16)
            return GINT_TO_POINTER(arg->v_int16);
        else if (t == GIG_ARG_TYPE_INT32)
            return GINT_TO_POINTER(arg->v_int32);

        return arg->v_pointer;
    }
    else if (key_type == GIG_HASH_INT64) {
        // GHashTables expect int64_t to be passed by reference, even
        // if they fit in a pointer.
        int64_t *p = xmalloc(sizeof(int64_t));
        *p = arg->v_int64;
        return p;
    }
    else if (key_type == GIG_HASH_REAL) {
        // GHashTables expect double and float to be passed by
        // reference, even if they fit in a pointer.
        if (t == GIG_ARG_TYPE_FLOAT) {
            float *p = xmalloc(sizeof(float));
            *p = arg->v_float;
            return p;
        }
        double *p = xmalloc(sizeof(double));
        *p = arg->v_double;
        return p;
    }
    // else is GIG_HASH_STRING or GIG_HASH_POINTER which don't require
    // special handling.
    return arg->v_pointer;
}

static void
scm_to_c_ghashtable(S2C_ARG_DECL)
{
    TRACE_S2C();
    bool is_ptr;
    GHashFunc hash_func;
    GEqualFunc equal_func;
    GigHashKeyType key_type = GIG_HASH_INVALID;
    GigHashKeyType val_type = GIG_HASH_INVALID;
    GDestroyNotify key_destroy_func = NULL;
    GDestroyNotify val_destroy_func = NULL;
    GigArgType t;

    is_ptr = meta->params[0].is_ptr;
    t = meta->params[0].arg_type;
    if (!is_ptr) {
        if (t == GIG_ARG_TYPE_INT8 || t == GIG_ARG_TYPE_INT16 || t == GIG_ARG_TYPE_INT32) {
            // INT types are packed into the pointer storage
            key_type = GIG_HASH_INT;
            hash_func = G.direct_hash;
            equal_func = G.direct_equal;
        }
        // INT64, DOUBLE are stored by reference, even if they would
        // fit in a pointer.
        else if (t == GIG_ARG_TYPE_INT64) {
            key_type = GIG_HASH_INT64;
            hash_func = G.int64_hash;
            equal_func = G.int64_equal;
            key_destroy_func = free;
        }
        else if (t == GIG_ARG_TYPE_DOUBLE || t == GIG_ARG_TYPE_FLOAT) {
            key_type = GIG_HASH_REAL;
            hash_func = G.double_hash;
            equal_func = G.double_equal;
            key_destroy_func = free;
        }
        else {
            // Should be unreachable
            gig_warn_if_reached();
            key_type = GIG_HASH_POINTER;
            hash_func = NULL;
            equal_func = NULL;
        }
    }
    else {
        // POINTER TYPES
        // For pointer types, strings are special because they have their own
        // comparison function
        if (t == GIG_ARG_TYPE_UTF8_STRING || t == GIG_ARG_TYPE_LOCALE_STRING) {
            key_type = GIG_HASH_STRING;
            hash_func = G.str_hash;
            equal_func = G.str_equal;
            key_destroy_func = free;
        }
        else {
            // All other pointer type are a straight pointer comparison
            key_type = GIG_HASH_POINTER;
            hash_func = NULL;
            equal_func = NULL;
        }
    }
    if (key_type == GIG_HASH_INVALID)
        scm_misc_error("object->hashtable", "unsupported key type ~S",
                       scm_list_1(scm_from_int(meta->params[0].arg_type)));

    is_ptr = meta->params[1].is_ptr;
    t = meta->params[1].arg_type;
    if (!is_ptr) {
        if (t == GIG_ARG_TYPE_INT8 || t == GIG_ARG_TYPE_UINT8
            || t == GIG_ARG_TYPE_INT16 || t == GIG_ARG_TYPE_UINT16
            || t == GIG_ARG_TYPE_INT32 || t == GIG_ARG_TYPE_UINT32)
            val_type = GIG_HASH_INT;
        else if (t == GIG_ARG_TYPE_INT64 || t == GIG_ARG_TYPE_UINT64) {
            val_type = GIG_HASH_INT64;
            val_destroy_func = free;
        }
        else if (t == GIG_ARG_TYPE_DOUBLE || t == GIG_ARG_TYPE_FLOAT) {
            val_type = GIG_HASH_REAL;
            val_destroy_func = free;
        }
    }
    else {
        if (t == GIG_ARG_TYPE_LOCALE_STRING || t == GIG_ARG_TYPE_UTF8_STRING) {
            val_type = GIG_HASH_STRING;
            val_destroy_func = free;
        }
        else
            val_type = GIG_HASH_POINTER;
    }
    if (val_type == GIG_HASH_INVALID)
        scm_misc_error("object->hashtable", "unsupported value type ~S",
                       scm_list_1(scm_from_utf8_string(G.type_name(meta->gtype))));

    GHashTable *hash =
        G.hash_table_new_full(hash_func, equal_func, key_destroy_func, val_destroy_func);

    SCM buckets = SCM_HASHTABLE_VECTOR(object);
    long n = SCM_SIMPLE_VECTOR_LENGTH(buckets);
    for (long i = 0; i < n; ++i) {
        SCM ls, handle;

        for (ls = SCM_SIMPLE_VECTOR_REF(buckets, i); !scm_is_null(ls); ls = SCM_CDR(ls)) {
            handle = scm_car(ls);
            GigArgument _keyval[2];
            SCM keyval[2];
            keyval[0] = scm_car(handle);
            keyval[1] = scm_cdr(handle);

            for (int j = 0; j < 2; j++) {
                size_t _size = GIG_ARRAY_SIZE_UNKNOWN;
                GigTypeMeta _meta = meta->params[j];
                _meta.transfer = GIG_TRANSFER_EVERYTHING;

                gig_argument_scm_to_c(subr, argpos, &_meta, keyval[j], must_free, &_keyval[j],
                                      &_size);
                _keyval[j].v_pointer =
                    arg_to_c_hash_pointer(&_meta, ((j == 0) ? key_type : val_type), &_keyval[j]);
            }
            G.hash_table_insert(hash, _keyval[0].v_pointer, _keyval[1].v_pointer);
        }
    }
    if (meta->transfer == GIG_TRANSFER_NOTHING) {
        // Later free the hash table
        ;
    }
    arg->v_pointer = hash;
}

static void
scm_to_c_native_gtype_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (!scm_is_vector(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "vector of gtype-ables");
    *size = scm_c_vector_length(object);
    if (meta->is_zero_terminated) {
        arg->v_pointer = malloc(sizeof(GType) * (*size + 1));
        ((GType *)arg->v_pointer)[*size] = 0;
        LATER_FREE(arg->v_pointer);
    }
    else {
        arg->v_pointer = malloc(sizeof(GType) * *size);
        LATER_FREE(arg->v_pointer);
    }
    for (size_t i = 0; i < *size; i++)
        ((GType *)(arg->v_pointer))[i] = scm_to_gtype(scm_c_vector_ref(object, i));
}

static void
scm_to_c_garray(S2C_ARG_DECL)
{
    TRACE_S2C();
    GigArgument _arg;
    GigTypeMeta _meta = *meta;

    _meta.arg_type = GIG_ARG_TYPE_ARRAY;
    // The GArray is going to take ownership of the array contents
    _meta.transfer = GIG_TRANSFER_EVERYTHING;

    gig_argument_scm_to_c(subr, argpos, &_meta, object, NULL, &_arg, size);
    arg->v_pointer = G.array_new(_meta.is_zero_terminated, FALSE, _meta.params[0].item_size);
    assert(arg->v_pointer != NULL);
    ((GArray *)(arg->v_pointer))->len = *size;
    ((GArray *)(arg->v_pointer))->data = _arg.v_pointer;
}

static void
scm_to_c_native_interface_array(S2C_ARG_DECL)
{
    TRACE_S2C();
#define FUNC_NAME "%object->c-native-interface-array-arg"
    GType item_type = meta->params[0].gtype;
    GType fundamental_item_type = G.type_fundamental(item_type);
    if (fundamental_item_type == G_TYPE_OBJECT || fundamental_item_type == G_TYPE_BOXED
        || fundamental_item_type == G_TYPE_INTERFACE || fundamental_item_type == G_TYPE_VARIANT) {
        // If we are a Struct or Object, we need to look up
        // our actual GType.
        if (!scm_is_vector(object))
            scm_wrong_type_arg_msg(subr, argpos, object, "vector of objects");
        *size = scm_c_vector_length(object);
        if (meta->params[0].is_ptr) {
            if (meta->is_zero_terminated) {
                arg->v_pointer = malloc(sizeof(void *) * (*size + 1));
                ((void **)arg->v_pointer)[*size] = 0;
            }
            else {
                arg->v_pointer = malloc(sizeof(void *) * *size);
            }
            for (size_t i = 0; i < *size; i++) {
                void *p = gig_type_peek_object(scm_c_vector_ref(object, i));
                if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
                    if (fundamental_item_type == G_TYPE_BOXED) {
                        ((void **)(arg->v_pointer))[i] =
                            xmemdup(p, gig_meta_real_item_size(&meta->params[0]));
                    }
                    else if (fundamental_item_type == G_TYPE_VARIANT) {
                        ((void **)(arg->v_pointer))[i] = p;
                        G.variant_ref(p);
                    }
                    else {
                        ((void **)(arg->v_pointer))[i] = p;
                        G.object_ref(p);
                    }
                }
                else
                    ((void **)(arg->v_pointer))[i] = p;
            }
        }
        else {
            GigTypeMeta *item_meta = &meta->params[0];
            size_t real_item_size = gig_meta_real_item_size(item_meta);
            if (meta->is_zero_terminated)
                arg->v_pointer = xcalloc(*size + 1, real_item_size);
            else
                arg->v_pointer = xcalloc(*size, real_item_size);
            for (size_t i = 0; i < *size; i++) {
                void *p = gig_type_peek_object(scm_c_vector_ref(object, i));
                if (meta->transfer == GIG_TRANSFER_EVERYTHING)
                    memcpy((char *)(arg->v_pointer) + i * real_item_size,
                           xmemdup(p, real_item_size), real_item_size);
                else
                    memcpy((char *)(arg->v_pointer) + i * real_item_size, p, real_item_size);
            }
        }
    }
    else if (meta->params[0].arg_type == GIG_ARG_TYPE_ENUM
             || meta->params[0].arg_type == GIG_ARG_TYPE_FLAGS) {
        // TODO: coerce to vector as above?
        if (scm_is_list(object)) {
            size_t length = scm_c_length(object);
            *size = length;
            int *ptr;
            if (meta->is_zero_terminated)
                ptr = xcalloc(length + 1, sizeof(int));
            else
                ptr = xcalloc(length, sizeof(int));
            arg->v_pointer = ptr;
            LATER_FREE(ptr);
            SCM iter = object;

            for (size_t i = 0; i < length; i++, iter = scm_cdr(iter))
                switch (meta->params[0].arg_type) {
                case GIG_ARG_TYPE_ENUM:
                    ptr[i] = gig_enum_to_int(scm_car(iter));
                    break;
                case GIG_ARG_TYPE_FLAGS:
                    ptr[i] = (int)gig_flags_to_uint(scm_car(iter));
                    break;
                default:
                    assert_not_reached();
                    break;
                }
        }
        else
            assert_not_reached();
    }
    else {
        // Everything else is unhandled.
        UNHANDLED;
        assert_not_reached();
    }
#undef FUNC_NAME
}

static void
scm_to_c_native_string_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    // This is a C array of string pointers, e.g. char **

    // We're adding a NULL termination to the list of strings
    // regardless of the value of array_is_zero_terminated, because it
    // does no harm.
    if (scm_is_vector(object)) {
        scm_t_array_handle handle;
        size_t len;
        ssize_t inc;
        const SCM *elt;

        elt = scm_vector_elements(object, &handle, &len, &inc);
        *size = len;
        char **strv = xcalloc(len + 1, sizeof(char *));
        LATER_FREE(strv);

        for (size_t i = 0; i < len; i++, elt += inc) {
            if (meta->params[0].arg_type == GIG_ARG_TYPE_LOCALE_STRING)
                strv[i] = scm_to_locale_string(*elt);
            else
                strv[i] = scm_to_utf8_string(*elt);
            LATER_FREE(strv[i]);
        }
        strv[len] = NULL;
        arg->v_pointer = strv;

        scm_array_handle_release(&handle);
    }
    else if (scm_is_list(object)) {
        size_t len = scm_c_length(object);
        *size = len;
        char **strv = xcalloc(len + 1, sizeof(char *));
        LATER_FREE(strv);
        SCM iter = object;
        for (size_t i = 0; i < len; i++) {
            SCM elt = scm_car(iter);
            if (meta->params[0].arg_type == GIG_ARG_TYPE_LOCALE_STRING)
                strv[i] = scm_to_locale_string(elt);
            else
                strv[i] = scm_to_utf8_string(elt);
            iter = scm_cdr(iter);
            LATER_FREE(strv[i]);
        }
        strv[len] = NULL;
        arg->v_pointer = strv;
    }
    else
        scm_wrong_type_arg_msg(subr, argpos, object, "list or vector of strings");
}

//////////////////////////////////////////////////////////
// CONVERTING GIGARGUMENTS TO SCM OBJECTS
//////////////////////////////////////////////////////////


void
gig_argument_c_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigArgType t = meta->arg_type;

    if (meta->arg_type == GIG_ARG_TYPE_VOID) {
        *object = SCM_UNSPECIFIED;
        return;
    }

    if (arg->v_pointer == NULL && meta->is_nullable) {
        *object = SCM_BOOL_F;
        return;
    }

    switch (t) {
    case GIG_ARG_TYPE_INTERFACE:
    case GIG_ARG_TYPE_OTHER:
        c_interface_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_INT8:
    case GIG_ARG_TYPE_UINT8:
        c_char_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_GBOOLEAN:
        c_boolean_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_INT16:
    case GIG_ARG_TYPE_UINT16:
    case GIG_ARG_TYPE_INT32:
    case GIG_ARG_TYPE_UINT32:
    case GIG_ARG_TYPE_INT64:
    case GIG_ARG_TYPE_UINT64:
        c_integer_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_UNICHAR:
        c_unichar_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_ENUM:
    case GIG_ARG_TYPE_FLAGS:
        c_enum_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_FLOAT:
    case GIG_ARG_TYPE_DOUBLE:
        c_real_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_LOCALE_STRING:
    case GIG_ARG_TYPE_UTF8_STRING:
        c_string_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_POINTER:
    case GIG_ARG_TYPE_ARRAY:
    case GIG_ARG_TYPE_GTYPE:
    case GIG_ARG_TYPE_GLIST:
    case GIG_ARG_TYPE_GSLIST:
    case GIG_ARG_TYPE_CALLBACK:
        c_pointer_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_GARRAY:
    case GIG_ARG_TYPE_GPTRARRAY:
    case GIG_ARG_TYPE_GBYTEARRAY:
    case GIG_ARG_TYPE_GHASH:
    case GIG_ARG_TYPE_BOXED:
    case GIG_ARG_TYPE_VALUE:
        c_boxed_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_PARAM:
        c_param_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_OBJECT:
        c_object_to_scm(C2S_ARGS);
        break;
    case GIG_ARG_TYPE_VARIANT:
        c_variant_to_scm(C2S_ARGS);
        break;
    default:
        UNHANDLED;
        break;
    }
}

static void
c_char_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->arg_type == GIG_ARG_TYPE_INT8)
        *object = scm_from_int8(arg->v_int8);
    else
        *object = scm_from_uint8(arg->v_uint8);
}

static void
c_boolean_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    *object = scm_from_bool(arg->v_boolean);
}

static void
c_integer_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigArgType t = meta->arg_type;
    if (t == GIG_ARG_TYPE_INT16)
        *object = scm_from_int16(arg->v_int16);
    else if (t == GIG_ARG_TYPE_UINT16)
        *object = scm_from_uint16(arg->v_int16);
    else if (t == GIG_ARG_TYPE_INT32)
        *object = scm_from_int32(arg->v_int32);
    else if (t == GIG_ARG_TYPE_UINT32)
        *object = scm_from_uint32(arg->v_int32);
    else if (t == GIG_ARG_TYPE_INT64)
        *object = scm_from_int64(arg->v_int64);
    else if (t == GIG_ARG_TYPE_UINT64)
        *object = scm_from_uint64(arg->v_int64);
}

static void
c_unichar_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    *object = SCM_MAKE_CHAR(arg->v_int32);
}

static void
c_real_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->arg_type == GIG_ARG_TYPE_FLOAT)
        *object = scm_from_double((double)arg->v_float);
    else if (meta->arg_type == GIG_ARG_TYPE_DOUBLE)
        *object = scm_from_double(arg->v_double);
    else
        UNHANDLED;
}

static void
c_boxed_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->arg_type == GIG_ARG_TYPE_GBYTEARRAY)
        c_byte_array_to_scm(C2S_ARGS);
    else if (meta->arg_type == GIG_ARG_TYPE_GARRAY)
        c_garray_to_scm(C2S_ARGS);
    else if (meta->arg_type == GIG_ARG_TYPE_GPTRARRAY)
        c_gptrarray_to_scm(C2S_ARGS);
    else if (meta->arg_type == GIG_ARG_TYPE_GHASH)
        c_ghashtable_to_scm(C2S_ARGS);
    else
        *object = gig_type_transfer_object(meta->gtype, arg->v_pointer, meta->transfer);
}

static void
c_interface_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();

    // If the argument has a more specialized type than the function
    // argument required, try to keep that more specialized type.
    GType orig_type = child_type(meta, arg);
    if (!gig_type_is_registered(orig_type))
        gig_type_define(orig_type);

    *object = gig_type_transfer_object(orig_type, arg->v_pointer, meta->transfer);
}

static void
c_object_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (arg->v_pointer == NULL)
        // The valid is_nullable NULL pointers should already have been handled.
        // Any NULLs here must be an error.
        scm_misc_error("%object-arg->scm", "cannot convert a NULL pointer to an object of type ~S",
                       scm_list_1(scm_from_utf8_string(G.type_name(meta->gtype))));

    *object = gig_type_transfer_object(child_type(meta, arg), arg->v_pointer, meta->transfer);
}

static void
c_variant_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    *object = gig_type_transfer_object(meta->gtype, arg->v_pointer, meta->transfer);
}

static void
c_param_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    *object = gig_type_transfer_object(meta->gtype, arg->v_pointer, meta->transfer);
}

static void
c_enum_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigArgType t = meta->arg_type;
    if (t == GIG_ARG_TYPE_ENUM) {
        if (meta->gtype == 0)
            *object = gig_int_to_enum_with_qname(arg->v_int32, meta->qname);
        else
            *object = gig_int_to_enum(arg->v_int32, meta->gtype);
    }
    else {
        if (meta->gtype == 0)
            *object = gig_uint_to_flags_with_qname(arg->v_uint32, meta->qname);
        else
            *object = gig_uint_to_flags(arg->v_uint32, meta->gtype);
    }
}

static void
c_string_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigArgType t = meta->arg_type;
    // We can't transfer strings directly, since GObject and Guile use
    // different internal encodings.  So for GIG_TRANSFER_EVERYTHGING,
    // we just free.

    if (!arg->v_string)
        *object = scm_c_make_string(0, SCM_MAKE_CHAR(0));
    else {
        if (t == GIG_ARG_TYPE_UTF8_STRING) {
            if (size != GIG_ARRAY_SIZE_UNKNOWN)
                *object = scm_from_utf8_stringn(arg->v_string, size);
            else
                *object = scm_from_utf8_string(arg->v_string);
        }
        else {
            if (size != GIG_ARRAY_SIZE_UNKNOWN)
                *object = scm_from_locale_stringn(arg->v_string, size);
            else
                *object = scm_from_locale_string(arg->v_string);
        }
        if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
            free(arg->v_string);
            arg->v_string = NULL;
        }
    }
}

#if 0
static void
c_array_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->gtype == G_TYPE_LENGTH_CARRAY)
        meta->length = size;
    if (meta->gtype == G_TYPE_LENGTH_CARRAY
        || meta->gtype == G_TYPE_FIXED_SIZE_CARRAY || meta->gtype == G_TYPE_ZERO_TERMINATED_CARRAY)
        c_native_array_to_scm(C2S_ARGS);
    else if (meta->gtype == G_TYPE_PTR_ARRAY)
        c_gptrarray_to_scm(C2S_ARGS);
    else
        UNHANDLED;
}
#endif

static void
c_native_array_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    size_t length = 0;

    if (meta->has_fixed_size)
        length = meta->fixed_size;
    else if (meta->is_zero_terminated)
        length = zero_terminated_array_length(meta, arg);
    else if (meta->has_length_arg)
        length = size;
    if (length == 0 && meta->is_nullable) {
        *object = SCM_BOOL_F;
        return;
    }

#define TRANSFER(_type,_short_type)                                     \
    do {                                                                \
        size_t sz;                                                       \
        if (!g_size_checked_mul(&sz, length, gig_meta_real_item_size(&meta->params[0])) || sz == G_MAXSIZE) \
            scm_misc_error(subr, "array size overflow", SCM_EOL);       \
        if (sz == 0)                                                    \
            *object = scm_make_ ## _short_type ## vector (scm_from_int(0), scm_from_int(0)); \
        else if (meta->transfer == GIG_TRANSFER_EVERYTHING)              \
            *object = scm_take_ ## _short_type ## vector((_type *)(arg->v_pointer), length); \
        else                                                            \
            *object = scm_take_ ## _short_type ## vector((_type *)xmemdup(arg->v_pointer, sz), length); \
    } while(0)

    GigArgType t = meta->params[0].arg_type;
    switch (t) {
    case GIG_ARG_TYPE_INT8:
        TRANSFER(gint8, s8);
        break;
    case GIG_ARG_TYPE_UINT8:
        TRANSFER(guint8, u8);
        break;
    case GIG_ARG_TYPE_INT16:
        TRANSFER(gint16, s16);
        break;
    case GIG_ARG_TYPE_UINT16:
        TRANSFER(guint16, u16);
        break;
    case GIG_ARG_TYPE_INT32:
        TRANSFER(gint32, s32);
        break;
    case GIG_ARG_TYPE_UINT32:
        TRANSFER(guint32, u32);
        break;
    case GIG_ARG_TYPE_INT64:
        TRANSFER(gint64, s64);
        break;
    case GIG_ARG_TYPE_UINT64:
        TRANSFER(guint64, u64);
        break;
    case GIG_ARG_TYPE_FLOAT:
        TRANSFER(gfloat, f32);
        break;
    case GIG_ARG_TYPE_DOUBLE:
        TRANSFER(gdouble, f64);
        break;
    case GIG_ARG_TYPE_UNICHAR:
        *object = scm_c_make_string(length, SCM_MAKE_CHAR(0));
        for (size_t k = 0; k < length; k++)
            scm_c_string_set_x(*object, k, SCM_MAKE_CHAR(((uint32_t *)(arg->v_pointer))[k]));
        if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
            free(arg->v_pointer);
            arg->v_pointer = 0;
        }
        break;
    case GIG_ARG_TYPE_GBOOLEAN:
    {
        *object = scm_c_make_vector(length, SCM_BOOL_F);
        scm_t_array_handle handle;
        size_t len;
        ssize_t inc;
        SCM *elt;
        elt = scm_vector_writable_elements(*object, &handle, &len, &inc);
        for (size_t k = 0; k < len; k++, elt += inc)
            *elt = ((gboolean *)(arg->v_pointer))[k] ? SCM_BOOL_T : SCM_BOOL_F;
        scm_array_handle_release(&handle);
        if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
            free(arg->v_pointer);
            arg->v_pointer = 0;
        }
        break;
    }
    case GIG_ARG_TYPE_BOXED:
    case GIG_ARG_TYPE_VARIANT:
    case GIG_ARG_TYPE_VALUE:
    {
        *object = scm_c_make_vector(length, SCM_BOOL_F);
        scm_t_array_handle handle;
        size_t len;
        ssize_t inc;
        SCM *elt;
        elt = scm_vector_writable_elements(*object, &handle, &len, &inc);

        assert(arg->v_pointer != NULL);

        GigArgument _arg = *arg;
        GigTypeMeta _meta = meta->params[0];

        if (t == GIG_ARG_TYPE_VALUE) {
            // value arrays are weird, man

            assert(meta->is_ptr);
            for (size_t k = 0; k < len; k++, _arg.v_pointer += sizeof(GValue), elt += inc)
                gig_argument_c_to_scm(subr, argpos, &_meta, &_arg, elt, -1);
        }
        else {
            // this used to be the VARIANT conversion, but it turns out, that other boxed
            // types are packed just like this
            bool is_pointer = _meta.is_ptr;
            _meta.is_ptr = true;
            uint8_t *iter = arg->v_pointer;

            for (size_t k = 0; k < len; k++, elt += inc, iter += gig_meta_real_item_size(meta)) {
                if (is_pointer)
                    _arg.v_pointer = *(void **)iter;
                else
                    _arg.v_pointer = iter;
                gig_argument_c_to_scm(subr, argpos, &_meta, &_arg, elt, -1);
            }
        }

        scm_array_handle_release(&handle);

        if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
            free(arg->v_pointer);
            arg->v_pointer = 0;
        }
        break;
    }
    case GIG_ARG_TYPE_UTF8_STRING:
    case GIG_ARG_TYPE_LOCALE_STRING:
    {
        *object = scm_c_make_vector(length, SCM_BOOL_F);
        scm_t_array_handle handle;
        size_t len;
        ssize_t inc;
        SCM *elt;

        elt = scm_vector_writable_elements(*object, &handle, &len, &inc);
        assert(len == length);

        for (size_t i = 0; i < length; i++, elt += inc) {
            char *str = ((char **)(arg->v_pointer))[i];
            if (str) {
                if (meta->params[0].arg_type == GIG_ARG_TYPE_UTF8_STRING)
                    *elt = scm_from_utf8_string(str);
                else
                    *elt = scm_from_locale_string(str);
            }
            if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
                free(((char **)(arg->v_pointer))[i]);
                ((char **)(arg->v_pointer))[i] = NULL;
            }
        }
        if (meta->transfer != GIG_TRANSFER_NOTHING) {
            free(arg->v_pointer);
            arg->v_pointer = NULL;
        }
        scm_array_handle_release(&handle);
        break;
    }
    default:
        UNHANDLED;
    }
    assert(!SCM_UNBNDP(*object));
#undef TRANSFER
}

static void
c_byte_array_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GByteArray *byte_array = arg->v_pointer;
    *object = scm_c_make_bytevector(byte_array->len);
    memcpy(SCM_BYTEVECTOR_CONTENTS(*object), byte_array->data, byte_array->len);
    if (meta->transfer == GIG_TRANSFER_EVERYTHING)
        G.byte_array_free(byte_array, TRUE);
    else
        G.byte_array_free(byte_array, FALSE);
}

static void
deep_free(void *x)
{
    char *p = *(char **)x;
    free(p);
}

static void
c_garray_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigTypeMeta _meta = *meta;
    GigArgument _arg;
    GArray *array = arg->v_pointer;
    _arg.v_pointer = array->data;
    _meta.arg_type = GIG_ARG_TYPE_ARRAY;
    _meta.has_fixed_size = true;
    _meta.fixed_size = array->len;
    // The GArray retains ownership of the conents.
    _meta.transfer = GIG_TRANSFER_NOTHING;

    size = 0;

    c_native_array_to_scm(subr, argpos, &_meta, &_arg, object, size);

    // Since the GArray retained ownership of the contents, we free as
    // necessary here.
    if (meta->transfer == GIG_TRANSFER_EVERYTHING) {
        if (meta->params[0].arg_type == GIG_ARG_TYPE_LOCALE_STRING
            || meta->params[0].arg_type == GIG_ARG_TYPE_UTF8_STRING)
            G.array_set_clear_func(array, deep_free);
        G.array_free(array, TRUE);
    }
    else if (meta->transfer == GIG_TRANSFER_CONTAINER)
        G.array_free(array, FALSE);
}

static void
c_gptrarray_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigTypeMeta _meta = *meta;
    GigArgument _arg;
    GPtrArray *array = arg->v_pointer;
    _meta.arg_type = GIG_ARG_TYPE_ARRAY;
    _meta.has_fixed_size = true;
    _meta.fixed_size = array->len;

    // Transfer the contents out of the GPtrArray into a
    // native C array, and then on to an SCM
    size = 0;
    _arg.v_pointer = xmemdup(array->pdata, array->len * sizeof(void *));
    c_native_array_to_scm(subr, argpos, &_meta, &_arg, object, size);

    // Free the GPtrArray without deleting the contents
    G.ptr_array_free(array, FALSE);
}

static void
c_ghashtable_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GHashTable *hash = arg->v_pointer;
    GHashTableIter iter;
    void *key, *value;

    *object = scm_c_make_hash_table(G.hash_table_size(hash));

    G.hash_table_iter_init(&iter, hash);
    while (G.hash_table_iter_next(&iter, &key, &value)) {
        SCM keyval[2];
        for (int i = 0; i < 2; i++) {
            GigTypeMeta _meta = meta->params[i];
            GigArgument _arg;
            void *p = ((i == 0) ? key : value);

            c_hash_pointer_to_arg(&_meta, p, &_arg);

            size_t _size = GIG_ARRAY_SIZE_UNKNOWN;
            gig_argument_c_to_scm(subr, argpos, &_meta, &_arg, &keyval[i], _size);
        }
        scm_hash_set_x(*object, keyval[0], keyval[1]);
    }
    if (meta->transfer != GIG_TRANSFER_NOTHING)
        G.hash_table_unref(hash);
}

static void
c_list_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    // Actual conversion
    GSList *slist = arg->v_pointer;
    size_t length = G.slist_length(slist);
    *object = scm_make_list(scm_from_size_t(length), SCM_UNDEFINED);

    for (SCM out_iter = *object; slist != NULL;
         slist = g_slist_next(slist), out_iter = scm_cdr(out_iter)) {
        GigArgument _arg;
        SCM _obj;
        _arg.v_pointer = slist->data;
        gig_argument_c_to_scm(subr, argpos, &meta->params[0], &_arg, &_obj,
                              GIG_ARRAY_SIZE_UNKNOWN);
        scm_set_car_x(out_iter, _obj);
    }
    if (meta->transfer != GIG_TRANSFER_NOTHING) {
        if (meta->arg_type == GIG_ARG_TYPE_GLIST)
            G.list_free(arg->v_pointer);
        else
            G.slist_free(arg->v_pointer);
        arg->v_pointer = NULL;
    }
}

static void
c_pointer_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GigArgType t = meta->arg_type;
    if (t == GIG_ARG_TYPE_ARRAY) {
        if (meta->has_length_arg || meta->has_fixed_size || meta->is_zero_terminated)
            c_native_array_to_scm(C2S_ARGS);
    }
    else if (t == GIG_ARG_TYPE_GTYPE)
        *object = scm_from_gtype(arg->v_size);
    else if (t == GIG_ARG_TYPE_CALLBACK) {
        void *cb = meta->is_ptr ? *(void **)arg->v_pointer : arg->v_pointer;
        *object = gig_callback_to_scm(subr, meta->callable_arg_map, cb);
    }
    else if (t == GIG_ARG_TYPE_GLIST || t == GIG_ARG_TYPE_GSLIST)
        c_list_to_scm(C2S_ARGS);
    else if (size != GIG_ARRAY_SIZE_UNKNOWN) {
        SCM bv = scm_c_make_bytevector(size);
        memcpy(SCM_BYTEVECTOR_CONTENTS(bv), arg->v_pointer, size);
        *object = bv;
    }
    else
        *object = scm_from_pointer(arg->v_pointer, NULL);
}

#define SCONSTX(NAME) scm_permanent_object(scm_c_define(#NAME, scm_from_int(NAME)))
