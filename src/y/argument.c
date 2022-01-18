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
#include <girepository.h>
#include <glib.h>
#include <libguile.h>
#include "x.h"
#include "y/argument.h"
#include "y/callback.h"
#include "y/flag.h"
#include "y/type.h"
#include "y/arg.h"
#include "y/guile.h"


#define TRACE_C2S() debug_transfer("[C2S] On line %d while handing %s of %s.", __LINE__, arg_describe(meta), subr)
#define TRACE_S2C() debug_transfer("[S2C] On line %d while handing %s of %s.", __LINE__, arg_describe(meta), subr)

#define SURPRISING                                                      \
    do {                                                                \
        warning_transfer("Unusual argument type '%s' %s:%d", arg_describe(meta), __FILE__, __LINE__); \
    } while(FALSE)

#define UNHANDLED                                                       \
    do {                                                                \
        error_transfer("unhandled argument type '%s' %s:%d", arg_describe(meta), __FILE__, __LINE__); \
    } while(FALSE)

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

static GType
child_type(Arg *meta, GIArgument *arg)
{
    if ((arg != NULL) && (arg->v_pointer != NULL) && G_IS_OBJECT(arg->v_pointer)
        && g_type_is_a(G_OBJECT_TYPE(arg->v_pointer), meta->_gtype)) {
        return G_OBJECT_TYPE(arg->v_pointer);
    }
    return meta->_gtype;
}

// This returns the number of elements (not necessarily bytes) in ARG.
static size_t
zero_terminated_array_length(Arg *meta, GIArgument *arg)
{
    assert(meta != NULL);
    assert(arg != NULL);

    if (arg->v_pointer == NULL)
        return 0;
    else if (meta->params[0].payload == TYPE_UTF8_STRING
             || meta->params[0].payload == TYPE_LOCALE_STRING)
        return strvlen((const char **)arg->v_pointer);
    else {
        size_t item_size = arg_item_size(&meta->params[0]);
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
            gboolean non_null;
            size_t length = -1;
            do {
                length++;
                non_null = FALSE;
                for (size_t i = 0; i <= item_size; i++)
                    if (ptr + i != 0) {
                        non_null = TRUE;
                        break;
                    }
                ptr += item_size;
            } while (non_null);

            return length;
        }
        }
    }
    return_val_if_reached(ARG_ARRAY_SIZE_UNKNOWN);
}

//////////////////////////////////////////////////////////
// CONVERTING SCM OBJECTS TO GIARGUMENTS
//////////////////////////////////////////////////////////

// This is the main entry point of the conversion of SCM objects to
// GIArguments.
void
argument_scm_to_c(S2C_ARG_DECL)
{
    TRACE_S2C();
    arg->v_pointer = NULL;
    if (size)
        *size = 0;

    switch (meta->payload) {
    case TYPE_INVALID:
        UNHANDLED;
        break;
     case TYPE_NONE:
        SURPRISING;
        arg->v_pointer = NULL;
        break;
    case TYPE_INTERFACE:
        scm_to_c_interface(S2C_ARGS);
        break;
    case TYPE_INT8:
    case TYPE_UINT8:
        scm_to_c_char(S2C_ARGS);
        break;
    case TYPE_BOOLEAN:
        scm_to_c_boolean(S2C_ARGS);
        break;
    case TYPE_INT16:
    case TYPE_UINT16:
    case TYPE_INT32:
    case TYPE_UINT32:
    case TYPE_INT64:
    case TYPE_UINT64:
        scm_to_c_integer(S2C_ARGS);
        break;
    case TYPE_ENUM:
    case TYPE_FLAGS:
        scm_to_c_enum(S2C_ARGS);
        break;
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        scm_to_c_real(S2C_ARGS);
        break;
    case TYPE_UTF8_STRING:
    case TYPE_LOCALE_STRING:
        scm_to_c_string(S2C_ARGS);
        break;
    case TYPE_POINTER:
        scm_to_c_pointer(S2C_ARGS);
        break;
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_ARRAY:
    case TYPE_GARRAY:
    case TYPE_GBYTEARRAY:
    case TYPE_GPTRARRAY:
    case TYPE_GHASH:
        scm_to_c_boxed(S2C_ARGS);
        break;
    case TYPE_PARAM:
        //scm_to_c_param(S2C_ARGS);
        UNHANDLED;
        break;
    case TYPE_OBJECT:
        scm_to_c_object(S2C_ARGS);
        break;
    case TYPE_VARIANT:
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
        arg->v_pointer = G_TYPE_CHECK_INSTANCE_CAST(peek_object(object),
                                                    meta->_gtype, void);
}

static void
scm_to_c_char(S2C_ARG_DECL)
{
    TRACE_S2C();

    if (!scm_is_integer(object) && !SCM_CHARP(object))
        scm_wrong_type_arg_msg(subr, argpos, object, "int8");

    if (meta->payload == TYPE_INT8) {
        if (SCM_CHARP(object)) {
            if (SCM_CHAR(object) > 255)
                scm_out_of_range(subr, object);
            else
                arg->v_int8 = (uint8_t) SCM_CHAR(object);
        }
        else if (!scm_is_signed_integer(object, INT8_MIN, INT8_MAX))
            scm_out_of_range(subr, object);
        else
            arg->v_int8 = scm_to_int8(object);
    }
    else if (meta->payload == TYPE_UINT8) {
        if (SCM_CHARP(object)) {
            if (SCM_CHAR(object) > 255)
                scm_out_of_range(subr, object);
            else
                arg->v_uint8 = (uint8_t) SCM_CHAR(object);
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

    if (!scm_is_eq(object, SCM_BOOL_T) && !scm_is_eq(object, SCM_BOOL_F))
        scm_wrong_type_arg_msg(subr, argpos, object, "boolean");
    arg->v_boolean = scm_is_true(object);
}

static void
scm_to_c_integer(S2C_ARG_DECL)
{
    TRACE_S2C();

#define ST(t,min,max)                                                    \
    do {                                                                \
        if (!scm_is_signed_integer(object, min, max))                   \
            scm_wrong_type_arg_msg(subr, argpos, object, #t);           \
        arg->v_ ## t = scm_to_ ## t(object);                            \
    } while (0)                                                         
#define UT(t,min,max)                                                    \
    do {                                                                \
        if (!scm_is_unsigned_integer(object, min, max))                 \
            scm_wrong_type_arg_msg(subr, argpos, object, #t);           \
        arg->v_ ## t = scm_to_ ## t(object);                            \
    } while (0)                                                     
        
    switch (meta->payload) {
    case TYPE_INT8:
        ST(int8, INT8_MIN, INT8_MAX);
        break;
    case TYPE_INT16:
        ST(int16, INT16_MIN, INT16_MAX);
        break;
    case TYPE_INT32:
        ST(int32, INT32_MIN, INT32_MAX);
        break;
    case TYPE_INT64:
        ST(int64, INT64_MIN, INT64_MAX);
        break;
    case TYPE_UINT8:
        ST(uint8, 0, UINT8_MAX);
        break;
    case TYPE_UINT16:
        ST(uint16, 0, UINT16_MAX);
        break;
    case TYPE_UINT32:
        ST(uint32, 0, UINT32_MAX);
        break;
    case TYPE_UINT64:
        ST(uint64, 0, UINT64_MAX);
        break;
    default:
        UNHANDLED;
    }
#undef ST
#undef UT
}

static void
scm_to_c_enum(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->payload == TYPE_ENUM)
        arg->v_int = enum_to_int(object);
    else if (meta->payload == TYPE_FLAGS)
        arg->v_uint = flags_to_uint(object);
    else
        UNHANDLED;
}

static void
scm_to_c_real(S2C_ARG_DECL)
{
    TRACE_S2C();

    if (meta->payload == TYPE_FLOAT) {
        if (!scm_is_real(object))
            scm_wrong_type_arg_msg(subr, argpos, object, "float32");
        gdouble dtmp = scm_to_double(object);
        if (dtmp > FLT_MAX || dtmp < -FLT_MAX)
            scm_out_of_range(subr, object);
        arg->v_float = (float)dtmp;
    }
    else if (meta->payload == TYPE_DOUBLE) {
        if (!scm_is_real(object))
            scm_wrong_type_arg_msg(subr, argpos, object, "float64");
        arg->v_double = scm_to_double(object);
    }
    else
        UNHANDLED;
}

static void
scm_to_c_string(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->is_nullable && scm_is_false(object)) {
        arg->v_pointer = NULL;
        return;
    }

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
        if (meta->transfer != GI_TRANSFER_EVERYTHING) {
            // But when we're using bytevectors as a possibly writable
            // location, they do need to be null terminated.
            int terminated = FALSE;
            for (size_t i = 0; i < SCM_BYTEVECTOR_LENGTH(object); i++)
                if (SCM_BYTEVECTOR_CONTENTS(object)[i] == 0)
                    terminated = TRUE;
            if (!terminated)
                scm_wrong_type_arg_msg(subr, argpos, object, "null-terminated bytevector");
            arg->v_string = (char *)SCM_BYTEVECTOR_CONTENTS(object);
        }
        else
            // But when we're copying the contents of the string, the
            // null termination can be enforced here.
            arg->v_string = xstrndup((const char *)SCM_BYTEVECTOR_CONTENTS(object),
                                     SCM_BYTEVECTOR_LENGTH(object));
    }
    else if (scm_is_string(object)) {
        if (meta->payload == TYPE_LOCALE_STRING)
            arg->v_string = scm_to_locale_string(object);
        else
            arg->v_string = scm_to_utf8_string(object);
        if (meta->transfer != GI_TRANSFER_EVERYTHING)
            slist_prepend(must_free, arg->v_string);
    }
    else
        scm_wrong_type_arg_msg(subr, argpos, object, "string or bytevector");
}

static void
scm_to_c_pointer(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (scm_is_false(object) && meta->is_nullable)
        arg->v_pointer = NULL;
    else if (meta->payload == TYPE_CALLBACK) {
        if (scm_is_true(scm_procedure_p(object)))
            arg->v_pointer = callback_to_c(subr, meta->callable_info, object);
        else
            scm_wrong_type_arg_msg(subr, argpos, object, "a procedure");
    }
    else if (meta->payload == TYPE_GTYPE)
        arg->v_size = get_gtype_full(object, subr, argpos);
    else if (SCM_POINTER_P(object))
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
    if (meta->is_nullable && scm_is_false(object)) {
        arg->v_pointer = NULL;
    }
    else if (meta->payload == TYPE_ARRAY)
        scm_to_c_native_array(S2C_ARGS);
    else if (meta->payload == TYPE_GARRAY)
        scm_to_c_garray(S2C_ARGS);
    else if (meta->payload == TYPE_GBYTEARRAY)
        scm_to_c_byte_array(S2C_ARGS);
    else if (meta->payload == TYPE_GPTRARRAY)
        scm_to_c_ptr_array(S2C_ARGS);
    else if (meta->payload == TYPE_GHASH)
        scm_to_c_ghashtable(S2C_ARGS);
    else if (!meta->is_ptr && !meta->is_caller_allocates)
        scm_misc_error("object->boxed",
                       "passing object ~S by value to a c function is not supported",
                       scm_list_1(scm_from_utf8_string(g_type_name(meta->_gtype))));

    else
        arg->v_pointer = peek_object(object);
}

static void
scm_to_c_object(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (meta->is_nullable && scm_is_false(object)) {
        arg->v_pointer = NULL;
    }
    else {
        if (!check_typed_object(object, get_scheme_type(meta->_gtype)))
            scm_wrong_type_arg_msg(subr, argpos, object, g_type_name(meta->_gtype));

        arg->v_pointer = peek_object(object);
    }
}

static void
scm_to_c_variant(S2C_ARG_DECL)
{
    TRACE_S2C();
    arg->v_pointer = peek_object(object);
}

static void
scm_to_c_native_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    Type_tag item_type = meta->params[0].payload;

    if (item_type == TYPE_BOOLEAN)
        scm_to_c_native_boolean_array(S2C_ARGS);
    else if (item_type == TYPE_UNICHAR)
        scm_to_c_native_unichar_array(S2C_ARGS);
    else if (item_type == TYPE_INT8
             || item_type == TYPE_UINT8
             || item_type == TYPE_INT16
             || item_type == TYPE_UINT16
             || item_type == TYPE_INT32
             || item_type == TYPE_UINT32
             || item_type == TYPE_INT64
             || item_type == TYPE_UINT64
             || item_type == TYPE_FLOAT || item_type == TYPE_DOUBLE)
        scm_to_c_native_immediate_array(S2C_ARGS);
    else if (item_type == TYPE_GTYPE)
        scm_to_c_native_gtype_array(S2C_ARGS);
    else if (item_type == TYPE_UTF8_STRING || item_type == TYPE_LOCALE_STRING)
        scm_to_c_native_string_array(S2C_ARGS);
    else if (item_type == TYPE_STRUCT || item_type == TYPE_UNION || item_type == TYPE_INTERFACE 
             || item_type == TYPE_ENUM || item_type == TYPE_FLAGS)
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
    if (meta->is_zero_terminated)
        arg->v_pointer = xcalloc(*size + 1, sizeof(gboolean));
    else
        arg->v_pointer = xcalloc(*size, sizeof(gboolean));
    if (meta->transfer != GI_TRANSFER_EVERYTHING)
        slist_prepend(must_free, arg->v_pointer);
    for (size_t i = 0; i < *size; i++)
        ((gboolean *)(arg->v_pointer))[i] = (int)scm_is_true(scm_c_vector_ref(object, i));
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
    if (meta->is_zero_terminated)
        arg->v_pointer = xcalloc(*size + 1, sizeof(gunichar));
    else
        arg->v_pointer = xcalloc(*size, sizeof(gunichar));
    if (meta->transfer != GI_TRANSFER_EVERYTHING)
        slist_prepend(must_free, arg->v_pointer);
    for (size_t i = 0; i < *size; i++)
        ((gunichar *)(arg->v_pointer))[i] = (gunichar)SCM_CHAR(scm_c_string_ref(object, i));
}

static void
scm_to_c_native_immediate_array(S2C_ARG_DECL)
{
    TRACE_S2C();
#define FUNC_NAME "%object->c-native-immediate-array-arg"
    // IMMEDIATE TYPES.  It seems only double, and 8 and 32-bit
    // integer arrays are ever used. Sometimes deep copy.  Sometimes
    // zero terminated.

    size_t item_size = arg_item_size(&meta->params[0]);
    assert(item_size != 0);

    if (scm_is_bytevector(object)) {
        *size = SCM_BYTEVECTOR_LENGTH(object) / item_size;
        if (meta->transfer == GI_TRANSFER_EVERYTHING) {
            if (meta->is_zero_terminated) {
                // Note, null terminated here.
                arg->v_pointer = xcalloc(*size + 1, item_size);
                memcpy(arg->v_pointer, SCM_BYTEVECTOR_CONTENTS(object),
                       SCM_BYTEVECTOR_LENGTH(object));
            }
            else {
                arg->v_pointer = xmemdup(SCM_BYTEVECTOR_CONTENTS(object),
                                         SCM_BYTEVECTOR_LENGTH(object));
            }
        }
        else {
            if (meta->is_zero_terminated) {
                // Adding null terminator element.
                arg->v_pointer = xcalloc(*size + 1, item_size);
                slist_prepend(must_free, arg->v_pointer);
                memcpy(arg->v_pointer, SCM_BYTEVECTOR_CONTENTS(object),
                       SCM_BYTEVECTOR_LENGTH(object));
            }
            else
                // The fast path
                arg->v_pointer = SCM_BYTEVECTOR_CONTENTS(object);

        }
    }
    else
        scm_wrong_type_arg_msg(subr, argpos, object, "bytevector");
#undef FUNC_NAME
}

static void
scm_to_c_byte_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    if (scm_is_bytevector(object)) {
        void *contents = SCM_BYTEVECTOR_CONTENTS(object);
        size_t len = SCM_BYTEVECTOR_LENGTH(object);
        *size = len;
        if (meta->transfer == GI_TRANSFER_EVERYTHING)
            arg->v_pointer = g_byte_array_new_take(contents, len);
        else
            arg->v_pointer = g_byte_array_new_take(xmemdup(contents, len), len);
    }
    else
        scm_wrong_type_arg_msg(subr, argpos, object, "bytevector");
}

static void
scm_to_c_ptr_array(S2C_ARG_DECL)
{
    TRACE_S2C();
    GIArgument _arg;
    Arg _meta = *meta;

    _meta.is_raw_array = TRUE;

    // Make a C array from this SCM.
    argument_scm_to_c(subr, argpos, &_meta, object, NULL, &_arg, size);

    // Move the pointers inside of the C array into a GPtrArray.
    GPtrArray *ptrarray = g_ptr_array_new();
    for (size_t i = 0; i < *size; i++)
        g_ptr_array_add(ptrarray, ((void **)(_arg.v_pointer))[i]);
    arg->v_pointer = ptrarray;
    assert(arg->v_pointer != NULL);

    // Free the C array without clobbering the pointers it used to
    // contain.
    free(_arg.v_pointer);
}

typedef enum _hash_key_type
{
    HASH_INVALID,
    HASH_INT,
    HASH_INT64,
    HASH_REAL,
    HASH_STRING,
    HASH_POINTER
} GigHashKeyType;

// This procedure converts a GHashTable's key or value to a
// GIArgument.
static void
c_hash_pointer_to_arg(Arg *meta, void **p, GIArgument *arg)
{
    if (!meta->is_ptr) {
        // Signed integers less that 4 bytes are packed into the
        // pointer storage, but with intptr_t sign extension.
        if (meta->payload == TYPE_INT8)
            arg->v_int8 = (int8_t)(intptr_t) (p);
        else if (meta->payload == TYPE_INT16)
            arg->v_int16 = (int16_t)(intptr_t) (p);
        else if (meta->payload == TYPE_INT32)
            arg->v_int32 = (int32_t)(intptr_t) (p);
        
        if (meta->payload == TYPE_UINT8)
            arg->v_uint8 = (uint8_t)(uintptr_t) (p);
        else if (meta->payload == TYPE_INT16)
            arg->v_uint16 = (uint16_t)(uintptr_t) (p);
        else if (meta->payload == TYPE_INT32)
            arg->v_uint32 = (uint32_t)(uintptr_t) (p);
        
        // 8-byte INT, INT64, DOUBLE and FLOAT are stored by
        // reference, even if they would fit in a pointer.
        else if (meta->payload == TYPE_INT64)
            arg->v_int64 = *(int64_t *) p;
        else if (meta->payload == G_TYPE_UINT64)
            arg->v_uint64 = *(uint64_t *) p;
        else if (meta->payload == G_TYPE_FLOAT)
            arg->v_float = *(gfloat *)p;
        else if (meta->payload == G_TYPE_DOUBLE)
            arg->v_double = *(gdouble *)p;
    }
    else {
        if (meta->payload == TYPE_UTF8_STRING || meta->payload == TYPE_LOCALE_STRING)
            arg->v_string = (char *)p;
        else
            arg->v_pointer = p;
    }
}

// This procedure converts a GIArgument to a void * to be used as a
// GHashTable's key or value.
static void *
arg_to_c_hash_pointer(Arg *meta, GigHashKeyType key_type, GIArgument *arg)
{
    if (key_type == HASH_INT) {
        // GHashTables apparently expect that negative integers have
        // intptr_t sign extension.
        if (meta->payload == TYPE_INT8)
            return GINT_TO_POINTER(arg->v_int8);
        else if (meta->payload == TYPE_INT16)
            return GINT_TO_POINTER(arg->v_int16);
        else if (meta->payload == TYPE_INT32)
            return GINT_TO_POINTER(arg->v_int32);
        else
            assert_not_reached();
    }
    else if (key_type == HASH_INT64) {
        // GHashTables expect int64_t to be passed by reference, even
        // if they fit in a pointer.
        if (meta->payload == TYPE_INT64) {
            int64_t *p = xmalloc(sizeof(int64_t));
            *p = arg->v_int64;
            return p;
        }
        else if (meta->payload == TYPE_UINT64) {
            uint64_t *p = xmalloc(sizeof(uint64_t));
            *p = arg->v_uint64;
            return p;
        }
    }
    else if (key_type == HASH_REAL) {
        // GHashTables expect double and float to be passed by
        // reference, even if they fit in a pointer.
        if (meta->payload == TYPE_DOUBLE) {
            double *p = xmalloc(sizeof(double));
            *p = arg->v_double;
            return p;
        }
        else if (meta->payload == TYPE_FLOAT) {
            gfloat *p = xmalloc(sizeof(float));
            *p = arg->v_float;
            return p;
        }
        else
            assert_not_reached();
    }
    // else is HASH_STRING or HASH_POINTER which don't require
    // special handling.
    return arg->v_pointer;
}

static void
scm_to_c_ghashtable(S2C_ARG_DECL)
{
    TRACE_S2C();
    int is_ptr;
    GHashFunc hash_func;
    GEqualFunc equal_func;
    GigHashKeyType key_type = HASH_INVALID;
    GigHashKeyType val_type = HASH_INVALID;
    GDestroyNotify key_destroy_func = NULL;
    GDestroyNotify val_destroy_func = NULL;

    is_ptr = meta->params[0].is_ptr;
    int type = meta->params[0].payload;
    if (!is_ptr) {
        if (type == TYPE_INT8 || type == TYPE_INT16 || type == TYPE_INT32) {
            // INT types are packed into the pointer storage
            key_type = HASH_INT;
            hash_func = g_direct_hash;
            equal_func = g_direct_equal;
        }
        // INT64, DOUBLE are stored by reference, even if they would
        // fit in a pointer.
        else if (type == TYPE_INT64) {
            key_type = HASH_INT64;
            hash_func = g_int64_hash;
            equal_func = g_int64_equal;
            key_destroy_func = free;
        }
        else if (type == TYPE_DOUBLE || type == TYPE_FLOAT) {
            key_type = HASH_REAL;
            hash_func = g_double_hash;
            equal_func = g_double_equal;
            key_destroy_func = free;
        }
        else {
            // Should be unreachable
            g_warn_if_reached();
            key_type = HASH_POINTER;
            hash_func = NULL;
            equal_func = NULL;
        }
    }
    else {
        // POINTER TYPES
        // For pointer types, strings are special because they have their own
        // comparison function
        if (meta->params[0].payload == TYPE_UTF8_STRING
            || meta->params[0].payload == TYPE_LOCALE_STRING) {
            key_type = HASH_STRING;
            hash_func = g_str_hash;
            equal_func = g_str_equal;
            key_destroy_func = free;
        }
        else {
            // All other pointer type are a straight pointer comparison
            key_type = HASH_POINTER;
            hash_func = NULL;
            equal_func = NULL;
        }
    }
    if (key_type == HASH_INVALID)
        scm_misc_error("object->hashtable", "unsupported key type ~S",
                       scm_list_1(scm_from_utf8_string(g_type_name(type))));

    is_ptr = meta->params[1].is_ptr;
    type = meta->params[1].payload;
    if (!is_ptr) {
        if (type == TYPE_INT8 || type == TYPE_INT16 || type == TYPE_INT32
            || type == TYPE_UINT8 || type == TYPE_UINT16 || type == TYPE_UINT32)
            val_type = HASH_INT;
        else if (type == TYPE_INT64 || type == G_TYPE_UINT64) {
            val_type = HASH_INT64;
            val_destroy_func = free;
        }
        else if (type == TYPE_DOUBLE || type == TYPE_FLOAT) {
            val_type = HASH_REAL;
            val_destroy_func = free;
        }
    }
    else {
        if (type == TYPE_UTF8_STRING || type == TYPE_LOCALE_STRING) {
            val_type = HASH_STRING;
            val_destroy_func = free;
        }
        else
            val_type = HASH_POINTER;
    }
    if (val_type == HASH_INVALID)
        scm_misc_error("object->hashtable", "unsupported value type ~S",
                       scm_list_1(scm_from_utf8_string(g_type_name(type))));

    GHashTable *hash =
        g_hash_table_new_full(hash_func, equal_func, key_destroy_func, val_destroy_func);

    SCM buckets = SCM_HASHTABLE_VECTOR(object);
    long n = SCM_SIMPLE_VECTOR_LENGTH(buckets);
    for (long i = 0; i < n; ++i) {
        SCM ls, handle;

        for (ls = SCM_SIMPLE_VECTOR_REF(buckets, i); !scm_is_null(ls); ls = SCM_CDR(ls)) {
            handle = scm_car(ls);
            GIArgument _keyval[2];
            SCM keyval[2];
            keyval[0] = scm_car(handle);
            keyval[1] = scm_cdr(handle);

            for (int j = 0; j < 2; j++) {
                size_t _size = ARG_ARRAY_SIZE_UNKNOWN;
                Arg _meta = meta->params[j];
                _meta.transfer = GI_TRANSFER_EVERYTHING;

                argument_scm_to_c(subr, argpos, &_meta, keyval[j], must_free, &_keyval[j],
                                      &_size);
                _keyval[j].v_pointer =
                    arg_to_c_hash_pointer(&_meta, ((j == 0) ? key_type : val_type), &_keyval[j]);
            }
            g_hash_table_insert(hash, _keyval[0].v_pointer, _keyval[1].v_pointer);
        }
    }
    if (meta->transfer == GI_TRANSFER_NOTHING) {
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
    if (meta->is_zero_terminated)
        arg->v_pointer = xcalloc(*size + 1, sizeof(GType));
    else
        arg->v_pointer = xcalloc(*size, sizeof(GType));
    if (meta->transfer != GI_TRANSFER_EVERYTHING)
        slist_prepend(must_free, arg->v_pointer);

    for (size_t i = 0; i < *size; i++)
        ((GType *) (arg->v_pointer))[i] = scm_to_gtype(scm_c_vector_ref(object, i));
}

static void
scm_to_c_garray(S2C_ARG_DECL)
{
    TRACE_S2C();
    GIArgument _arg;
    Arg _meta = *meta;

    _meta.is_raw_array = TRUE;
    // The GArray is going to take ownership of the array contents
    _meta.transfer = GI_TRANSFER_EVERYTHING;

    argument_scm_to_c(subr, argpos, &_meta, object, NULL, &_arg, size);
    arg->v_pointer = g_array_new(_meta.is_zero_terminated, FALSE, _meta.params[0].item_size);
    assert(arg->v_pointer != NULL);
    ((GArray *)(arg->v_pointer))->len = *size;
    ((GArray *)(arg->v_pointer))->data = _arg.v_pointer;
}

static void
scm_to_c_native_interface_array(S2C_ARG_DECL)
{
    TRACE_S2C();
#define FUNC_NAME "%object->c-native-interface-array-arg"
    Type_tag item_type = meta->params[0].payload;
    if (item_type == TYPE_OBJECT || item_type == TYPE_STRUCT || item_type == TYPE_UNION
        || item_type == TYPE_INTERFACE || item_type == TYPE_VARIANT) {
        // If we are a Struct or Object, we need to look up
        // our actual GType.
        if (!scm_is_vector(object))
            scm_wrong_type_arg_msg(subr, argpos, object, "vector of objects");
        *size = scm_c_vector_length(object);
        if (meta->params[0].is_ptr) {
            if (meta->is_zero_terminated)
                arg->v_pointer = xcalloc(*size + 1, sizeof(void *));
            else
                arg->v_pointer = xcalloc(*size, sizeof(void *));
            for (size_t i = 0; i < *size; i++) {
                void *p = peek_object(scm_c_vector_ref(object, i));
                if (meta->transfer == GI_TRANSFER_EVERYTHING) {
                    if (item_type == TYPE_STRUCT || item_type == TYPE_UNION) {
                        ((void **)(arg->v_pointer))[i] =
                            xmemdup(p, arg_item_size(&meta->params[0]));
                    }
                    else if (item_type == TYPE_VARIANT) {
                        ((void **)(arg->v_pointer))[i] = p;
                        g_variant_ref(p);
                    }
                    else {
                        ((void **)(arg->v_pointer))[i] = p;
                        g_object_ref(p);
                    }
                }
                else
                    ((void **)(arg->v_pointer))[i] = p;
            }
        }
        else {
            Arg *item_meta = &meta->params[0];
            size_t real_item_size = arg_item_size(item_meta);
            if (meta->is_zero_terminated)
                arg->v_pointer = xcalloc(*size + 1, real_item_size);
            else
                arg->v_pointer = xcalloc(*size, real_item_size);
            for (size_t i = 0; i < *size; i++) {
                void *p = peek_object(scm_c_vector_ref(object, i));
                if (meta->transfer == GI_TRANSFER_EVERYTHING)
                    memcpy((char *)(arg->v_pointer) + i * real_item_size,
                           xmemdup(p, real_item_size), real_item_size);
                else
                    memcpy((char *)(arg->v_pointer) + i * real_item_size, p, real_item_size);
            }
        }
    }
    else if (item_type == TYPE_ENUM || item_type == TYPE_FLAGS) {
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
            if (meta->transfer != GI_TRANSFER_EVERYTHING)
                slist_prepend(must_free, arg->v_pointer);

            SCM iter = object;

            for (size_t i = 0; i < length; i++, iter = scm_cdr(iter))
                switch (item_type) {
                case TYPE_ENUM:
                    ptr[i] = enum_to_int(scm_car(iter));
                    break;
                case TYPE_FLAGS:
                    ptr[i] = (int)flags_to_uint(scm_car(iter));
                    break;
                }
        }
        else
            assert_not_reached();
    }
    else {
        // Everything else is unhandled.
        UNHANDLED;
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
        gssize inc;
        const SCM *elt;

        elt = scm_vector_elements(object, &handle, &len, &inc);
        *size = len;
        char **strv = xcalloc(len + 1, sizeof(gchar *));
        if (meta->transfer != GI_TRANSFER_EVERYTHING)
            slist_prepend(must_free, strv);

        for (size_t i = 0; i < len; i++, elt += inc) {
            if (meta->params[0].payload == TYPE_LOCALE_STRING)
                strv[i] = scm_to_locale_string(*elt);
            else
                strv[i] = scm_to_utf8_string(*elt);
            if (meta->transfer != GI_TRANSFER_EVERYTHING)
                slist_prepend(must_free, strv[i]);
        }
        strv[len] = NULL;
        arg->v_pointer = strv;

        scm_array_handle_release(&handle);
    }
    else if (scm_is_list(object)) {
        size_t len = scm_c_length(object);
        *size = len;
        char **strv = xcalloc(len + 1, sizeof(char *));
        if (meta->transfer != GI_TRANSFER_EVERYTHING)
            slist_prepend(must_free, strv);

        SCM iter = object;
        for (size_t i = 0; i < len; i++) {
            SCM elt = scm_car(iter);
            if (meta->params[0].payload == TYPE_LOCALE_STRING)
                strv[i] = scm_to_locale_string(elt);
            else
                strv[i] = scm_to_utf8_string(elt);
            iter = scm_cdr(iter);
            if (meta->transfer != GI_TRANSFER_EVERYTHING)
                slist_prepend(must_free, strv[i]);
        }
        strv[len] = NULL;
        arg->v_pointer = strv;
    }
    else
        scm_wrong_type_arg_msg(subr, argpos, object, "list or vector of strings");
}

//////////////////////////////////////////////////////////
// CONVERTING GIARGUMENTS TO SCM OBJECTS
//////////////////////////////////////////////////////////


void
argument_c_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    Type_tag type = meta->payload;

    // Check this up front because arg may be unitialized when
    // returning void.
    if (type == TYPE_NONE) {
        *object = SCM_UNSPECIFIED;
        return;
    }

    if (arg->v_pointer == NULL && meta->is_nullable) {
        *object = SCM_BOOL_F;
        return;
    }

    switch (type) {
    case TYPE_NONE:
        *object = SCM_UNSPECIFIED;
        break;
    case TYPE_INTERFACE:
        c_interface_to_scm(C2S_ARGS);
        break;
    case TYPE_INT8:
    case TYPE_UINT8:
        c_char_to_scm(C2S_ARGS);
        break;
    case TYPE_BOOLEAN:
        c_boolean_to_scm(C2S_ARGS);
        break;
    case TYPE_INT16:
    case TYPE_UINT16:
    case TYPE_INT32:
    case TYPE_UINT32:
    case TYPE_INT64:
    case TYPE_UINT64:
        c_integer_to_scm(C2S_ARGS);
        break;
    case TYPE_ENUM:
    case TYPE_FLAGS:
        c_enum_to_scm(C2S_ARGS);
        break;
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        c_real_to_scm(C2S_ARGS);
        break;
    case TYPE_UTF8_STRING:
    case TYPE_LOCALE_STRING:
        c_string_to_scm(C2S_ARGS);
        break;
    case TYPE_POINTER:
        c_pointer_to_scm(C2S_ARGS);
        break;
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_ARRAY:
    case TYPE_GARRAY:
    case TYPE_GBYTEARRAY:
    case TYPE_GPTRARRAY:
    case TYPE_GHASH:
        c_boxed_to_scm(C2S_ARGS);
        break;
    case TYPE_PARAM:
        c_param_to_scm(C2S_ARGS);
        break;
    case TYPE_OBJECT:
        c_object_to_scm(C2S_ARGS);
        break;
    case TYPE_VARIANT:
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
    if (meta->payload == TYPE_INT8)
        *object = scm_from_int8(arg->v_int8);
    else if (meta->payload == TYPE_UINT8)
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
    if (meta->payload == TYPE_INT8)
        *object = scm_from_int8(arg->v_int8);
    else if (meta->payload == TYPE_INT16)
        *object = scm_from_int16(arg->v_int16);
    else if (meta->payload == TYPE_INT32)
        *object = scm_from_int32(arg->v_int32);
    else if (meta->payload == TYPE_INT64)
        *object = scm_from_int64(arg->v_int64);
    else if (meta->payload == TYPE_UINT8)
        *object = scm_from_int8(arg->v_uint8);
    else if (meta->payload == TYPE_UINT16)
        *object = scm_from_int16(arg->v_uint16);
    else if (meta->payload == TYPE_UINT32)
        *object = scm_from_int32(arg->v_uint32);
    else if (meta->payload == TYPE_UINT64)
        *object = scm_from_int64(arg->v_uint64);
    else
        UNHANDLED;
}

static void
c_real_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->payload == TYPE_FLOAT)
        *object = scm_from_double((double)arg->v_float);
    else if (meta->payload == TYPE_DOUBLE)
        *object = scm_from_double(arg->v_double);
    else
        UNHANDLED;
}

static void
c_boxed_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->payload == TYPE_GBYTEARRAY)
        c_byte_array_to_scm(C2S_ARGS);
    else if (meta->payload == TYPE_ARRAY) {
        if (meta->has_size)
            meta->length = size;
        c_native_array_to_scm(C2S_ARGS);
    }
    else if (meta->payload == TYPE_GARRAY)
        c_garray_to_scm(C2S_ARGS);
    else if (meta->payload == TYPE_GPTRARRAY)
        c_gptrarray_to_scm(C2S_ARGS);
    else if (meta->payload == TYPE_GHASH)
        c_ghashtable_to_scm(C2S_ARGS);
    else
        *object = transfer_object(meta->_gtype, arg->v_pointer, meta->transfer);
}

static void
c_interface_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();

    *object = transfer_object(child_type(meta, arg), arg->v_pointer, meta->transfer);
}

static void
c_object_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (arg->v_pointer == NULL)
        // The valid is_nullable NULL pointers should already have been handled.
        // Any NULLs here must be an error.
        scm_misc_error("%object-arg->scm", "cannot convert a NULL pointer to an object of type ~S",
                       scm_list_1(scm_from_utf8_string(g_type_name(meta->_gtype))));

    *object = transfer_object(child_type(meta, arg), arg->v_pointer, meta->transfer);
}

static void
c_variant_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    *object = transfer_object(meta->_gtype, arg->v_pointer, meta->transfer);
}

static void
c_param_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    *object = transfer_object(meta->_gtype, arg->v_pointer, meta->transfer);
}

static void
c_enum_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->payload == TYPE_ENUM) {
        if (meta->_gtype == G_TYPE_ENUM)
            *object = int_to_enum_with_info(arg->v_int32, meta->enum_info);
        else
            *object = int_to_enum(arg->v_int32, meta->_gtype);
    } else if (meta->payload == TYPE_FLAGS) {
        if (meta->_gtype == G_TYPE_FLAGS)
            *object = uint_to_flags_with_info(arg->v_uint32, meta->enum_info);
        else
            *object = uint_to_flags(arg->v_uint32, meta->_gtype);
    } else
        UNHANDLED;
}

static void
c_string_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    // We can't transfer strings directly, since GObject and Guile use
    // different internal encodings.  So for GI_TRANSFER_EVERYTHGING,
    // we just free.
    if (!arg->v_string)
        *object = scm_c_make_string(0, SCM_MAKE_CHAR(0));
    else {
        if (meta->payload == TYPE_UTF8_STRING) {
            if (size != ARG_ARRAY_SIZE_UNKNOWN)
                *object = scm_from_utf8_stringn(arg->v_string, size);
            else
                *object = scm_from_utf8_string(arg->v_string);
        }
        else {
            if (size != ARG_ARRAY_SIZE_UNKNOWN)
                *object = scm_from_locale_stringn(arg->v_string, size);
            else
                *object = scm_from_locale_string(arg->v_string);
        }
        if (meta->transfer == GI_TRANSFER_EVERYTHING) {
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
    size_t length;

    if (meta->length != ARG_ARRAY_SIZE_UNKNOWN)
        length = meta->length;
    else if (meta->is_zero_terminated)
        length = zero_terminated_array_length(meta, arg);
    else
        length = size;
    if (length == 0 && meta->is_nullable) {
        *object = SCM_BOOL_F;
        return;
    }

#define TRANSFER(_type,_short_type)                                     \
    do {                                                                \
        size_t sz;                                                       \
        if (!g_size_checked_mul(&sz, length, arg_item_size(&meta->params[0])) || sz == G_MAXSIZE) \
            scm_misc_error(subr, "array size overflow", SCM_EOL);       \
        if (sz == 0)                                                    \
            *object = scm_make_ ## _short_type ## vector (scm_from_int(0), scm_from_int(0)); \
        else if (meta->transfer == GI_TRANSFER_EVERYTHING)              \
            *object = scm_take_ ## _short_type ## vector((_type *)(arg->v_pointer), length); \
        else                                                            \
            *object = scm_take_ ## _short_type ## vector((_type *)xmemdup(arg->v_pointer, sz), length); \
    } while(0)

    Type_tag item_type = meta->params[0].payload;
    switch (item_type) {
    case TYPE_INT8:
        TRANSFER(int8_t, s8);
        break;
    case TYPE_UINT8:
        TRANSFER(uint8_t, u8);
        break;
    case TYPE_INT16:
        TRANSFER(int16_t, s16);
        break;
    case TYPE_UINT16:
        TRANSFER(uint16_t, u16);
        break;
    case TYPE_INT32:
        TRANSFER(int32_t, s32);
        break;
    case TYPE_UINT32:
        TRANSFER(uint32_t, u32);
        break;
    case TYPE_INT64:
        TRANSFER(int64_t, s64);
        break;
    case TYPE_UINT64:
        TRANSFER(uint64_t, u64);
        break;
    case TYPE_UNICHAR:
        *object = scm_c_make_string(length, SCM_MAKE_CHAR(0));
        for (size_t k = 0; k < length; k++)
            scm_c_string_set_x(*object, k, SCM_MAKE_CHAR(((gunichar *)(arg->v_pointer))[k]));
        if (meta->transfer == GI_TRANSFER_EVERYTHING) {
            free(arg->v_pointer);
            arg->v_pointer = 0;
        }
         break;
    case TYPE_FLOAT:
        TRANSFER(gfloat, f32);
        break;
    case TYPE_DOUBLE:
        TRANSFER(gdouble, f64);
        break;
    case TYPE_BOOLEAN:
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
        if (meta->transfer == GI_TRANSFER_EVERYTHING) {
            free(arg->v_pointer);
            arg->v_pointer = 0;
        }
        break;
    }
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_ARRAY:
    case TYPE_GARRAY:
    case TYPE_GPTRARRAY:
    case TYPE_GBYTEARRAY:
    case TYPE_GHASH:
    case TYPE_VARIANT:
    {
        *object = scm_c_make_vector(length, SCM_BOOL_F);
        scm_t_array_handle handle;
        size_t len;
        ssize_t inc;
        SCM *elt;
        elt = scm_vector_writable_elements(*object, &handle, &len, &inc);

        assert(arg->v_pointer != NULL);

        GIArgument _arg = *arg;
        Arg _meta = meta->params[0];

        if (item_type == G_TYPE_VALUE) {
            // value arrays are weird, man
            assert(meta->is_ptr);
            for (size_t k = 0; k < len; k++, _arg.v_pointer += sizeof(GValue), elt += inc)
                argument_c_to_scm(subr, argpos, &_meta, &_arg, elt, -1);
        }
        else {
            // this used to be the VARIANT conversion, but it turns out, that other boxed
            // types are packed just like this
            int is_pointer = _meta.is_ptr;
            _meta.is_ptr = TRUE;
            uint8_t *iter = arg->v_pointer;

            for (size_t k = 0; k < len; k++, elt += inc, iter += arg_item_size(meta)) {
                if (is_pointer)
                    _arg.v_pointer = *(void **)iter;
                else
                    _arg.v_pointer = iter;
                argument_c_to_scm(subr, argpos, &_meta, &_arg, elt, -1);
            }
        }

        scm_array_handle_release(&handle);

        if (meta->transfer == GI_TRANSFER_EVERYTHING) {
            free(arg->v_pointer);
            arg->v_pointer = 0;
        }
        break;
    }
    case G_TYPE_STRING:
    {
        *object = scm_c_make_vector(length, SCM_BOOL_F);
        scm_t_array_handle handle;
        size_t len;
        gssize inc;
        SCM *elt;

        elt = scm_vector_writable_elements(*object, &handle, &len, &inc);
        assert(len == length);

        for (size_t i = 0; i < length; i++, elt += inc) {
            char *str = ((char **)(arg->v_pointer))[i];
            if (str) {
                if (meta->params[0].payload == TYPE_UTF8_STRING)
                    *elt = scm_from_utf8_string(str);
                else
                    *elt = scm_from_locale_string(str);
            }
            if (meta->transfer == GI_TRANSFER_EVERYTHING) {
                free(((char **)(arg->v_pointer))[i]);
                ((char **)(arg->v_pointer))[i] = NULL;
            }
        }
        if (meta->transfer != GI_TRANSFER_NOTHING) {
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
    if (meta->transfer == GI_TRANSFER_EVERYTHING)
        g_byte_array_free(byte_array, TRUE);
    else
        g_byte_array_free(byte_array, FALSE);
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
    Arg _meta = *meta;
    GIArgument _arg;
    GArray *array = arg->v_pointer;
    _arg.v_pointer = array->data;
    _meta.is_raw_array = TRUE;
    _meta.length = array->len;
    // The GArray retains ownership of the conents.
    _meta.transfer = GI_TRANSFER_NOTHING;

    size = array->len;

    c_native_array_to_scm(subr, argpos, &_meta, &_arg, object, size);

    // Since the GArray retained ownership of the contents, we free as
    // necessary here.
    if (meta->transfer == GI_TRANSFER_EVERYTHING) {
        if (meta->params[0].payload == TYPE_UTF8_STRING
            || meta->params[0].payload == TYPE_LOCALE_STRING)
            g_array_set_clear_func(array, deep_free);
        g_array_free(array, TRUE);
    }
    else if (meta->transfer == GI_TRANSFER_CONTAINER)
        g_array_free(array, FALSE);
}

static void
c_gptrarray_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    Arg _meta = *meta;
    GIArgument _arg;
    GPtrArray *array = arg->v_pointer;
    _meta.is_raw_array = TRUE;
    _meta.length = array->len;

    // Transfer the contents out of the GPtrArray into a
    // native C array, and then on to an SCM
    size = array->len;
    _arg.v_pointer = xmemdup(array->pdata, array->len * sizeof(void *));
    c_native_array_to_scm(subr, argpos, &_meta, &_arg, object, size);

    // Free the GPtrArray without deleting the contents
    g_ptr_array_free(array, FALSE);
}

static void
c_ghashtable_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    GHashTable *hash = arg->v_pointer;
    GHashTableIter iter;
    void *key, *value;

    *object = scm_c_make_hash_table(g_hash_table_size(hash));

    g_hash_table_iter_init(&iter, hash);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        SCM keyval[2];
        for (int i = 0; i < 2; i++) {
            Arg _meta = meta->params[i];
            GIArgument _arg;
            void *p = ((i == 0) ? key : value);

            c_hash_pointer_to_arg(&_meta, p, &_arg);

            size_t _size = ARG_ARRAY_SIZE_UNKNOWN;
            argument_c_to_scm(subr, argpos, &_meta, &_arg, &keyval[i], _size);
        }
        scm_hash_set_x(*object, keyval[0], keyval[1]);
    }
    if (meta->transfer != GI_TRANSFER_NOTHING)
        g_hash_table_unref(hash);
}

static void
c_list_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    // Actual conversion
    GSList *slist = arg->v_pointer;
    size_t length = g_slist_length(slist);
    *object = scm_make_list(scm_from_size_t(length), SCM_UNDEFINED);

    for (SCM out_iter = *object; slist != NULL;
         slist = g_slist_next(slist), out_iter = scm_cdr(out_iter)) {
        GIArgument _arg;
        SCM _obj;
        _arg.v_pointer = slist->data;
        argument_c_to_scm(subr, argpos, &meta->params[0], &_arg, &_obj,
                              ARG_ARRAY_SIZE_UNKNOWN);
        scm_set_car_x(out_iter, _obj);
    }
    if (meta->transfer != GI_TRANSFER_NOTHING) {
        if (meta->payload == TYPE_GLIST)
            g_list_free(arg->v_pointer);
        else
            g_slist_free(arg->v_pointer);
        arg->v_pointer = NULL;
    }
}

static void
c_pointer_to_scm(C2S_ARG_DECL)
{
    TRACE_C2S();
    if (meta->payload == TYPE_GTYPE)
        *object = scm_from_gtype(arg->v_size);
    else if (meta->payload == TYPE_CALLBACK) {
        void *cb = meta->is_ptr ? *(void **)arg->v_pointer : arg->v_pointer;
        *object = callback_to_scm(subr, meta->callable_info, cb);
    }
    else if (meta->payload == TYPE_GLIST || meta->payload == TYPE_GSLIST)
        c_list_to_scm(C2S_ARGS);
    else if (size != ARG_ARRAY_SIZE_UNKNOWN) {
        SCM bv = scm_c_make_bytevector(size);
        memcpy(SCM_BYTEVECTOR_CONTENTS(bv), arg->v_pointer, size);
        *object = bv;
    }
    else
        *object = scm_from_pointer(arg->v_pointer, NULL);
}

#define SCONSTX(NAME) scm_permanent_object(scm_c_define(#NAME, scm_from_int(NAME)))

