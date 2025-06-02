// Copyright (C) 2019, 2022 Michael L. Gran

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

#ifndef GIG_DATA_TYPE_H
#define GIG_DATA_TYPE_H

#include "../type.h"

#define GIG_ARRAY_SIZE_UNKNOWN ((size_t)-1)

/*
gboolean
GBoxed
  GArray
  GByteArray
  GError
  GHashTable
  GPtrArray
  GValue
gchararray
  (utf8, filename)
gchar
gdouble
GEnum
GFlags
gfloat
GInterface
  GdkEvent
  GeeLazy
  GeePromise
  GskRenderNode
  GtkExpression
gint - 16, 32, 64
GObject
GParam
gpointer
  CArray
  gpointer - glist, gslist, GType
guchar
guint
 16, 32, 64, unichar
GVariant


 */

// All the categories of type conversions necessary for our FFI and
// SCM-to-C function argument conversion.
typedef enum GigArgType_
{
    GIG_ARG_TYPE_UNKNOWN = 0,
    GIG_ARG_TYPE_VOID,

    // Immediate C types
    GIG_ARG_TYPE_INT8,
    GIG_ARG_TYPE_UINT8,
    GIG_ARG_TYPE_INT16,
    GIG_ARG_TYPE_UINT16,
    GIG_ARG_TYPE_INT32,
    GIG_ARG_TYPE_UINT32,
    GIG_ARG_TYPE_INT64,
    GIG_ARG_TYPE_UINT64,
    GIG_ARG_TYPE_UNICHAR,
    GIG_ARG_TYPE_FLOAT,
    GIG_ARG_TYPE_DOUBLE,
    GIG_ARG_TYPE_POINTER,

    // C non-immediate types
    GIG_ARG_TYPE_UTF8_STRING,
    GIG_ARG_TYPE_LOCALE_STRING,
    GIG_ARG_TYPE_ARRAY,         /* A C array. Has 1 param type */

    // Immediate GLib/GObject types
    GIG_ARG_TYPE_GBOOLEAN,      /* GLib's int-sized boolean */
    GIG_ARG_TYPE_GTYPE,
    GIG_ARG_TYPE_GERROR,
    GIG_ARG_TYPE_BOXED,         /* Has a GType */
    GIG_ARG_TYPE_INTERFACE,     /* Has a GType */
    GIG_ARG_TYPE_OBJECT,        /* Has a GType */
    GIG_ARG_TYPE_ENUM,          /* Has a GType or qname */
    GIG_ARG_TYPE_FLAGS,         /* Has a GType or qname */
    GIG_ARG_TYPE_VARIANT,
    GIG_ARG_TYPE_VALUE,
    GIG_ARG_TYPE_PARAM,
    GIG_ARG_TYPE_CALLBACK,
    GIG_ARG_TYPE_OTHER,

    // GLib/GObject non-immediate types
    GIG_ARG_TYPE_GARRAY,        /* Has 1 param type */
    GIG_ARG_TYPE_GPTRARRAY,
    GIG_ARG_TYPE_GBYTEARRAY,
    GIG_ARG_TYPE_GLIST,         /* Has 1 param type */
    GIG_ARG_TYPE_GSLIST,        /* Has 1 param type */
    GIG_ARG_TYPE_GHASH,         /* Has 2 param types */
} GigArgType;

#define GIG_ARG_TYPE_N_ARGS (GIG_ARG_TYPE_GHASH + 1)


typedef enum _GigPointerType
{
    GIG_DATA_VOID = 0,
    GIG_DATA_UTF8_STRING,
    GIG_DATA_LOCALE_STRING,
    GIG_DATA_LIST,
    GIG_DATA_SLIST,
    GIG_DATA_CALLBACK
} GigPointerType;

// Similar to GIArgument, but, only has the types necessary for our
// FFI and SCM-to-C argument conversion.
typedef union GigArgument_
{
    int8_t v_int8;
    uint8_t v_uint8;
    int16_t v_int16;
    uint16_t v_uint16;
    int32_t v_int32;
    uint32_t v_uint32;
    int64_t v_int64;
    uint64_t v_uint64;
    uint32_t v_unichar;
    float v_float;
    double v_double;
    int v_boolean;
    void *v_pointer;
    char *v_string;
    int v_gboolean;
    size_t v_size;
    long v_long;
    unsigned long v_ulong;
} GigArgument;

typedef struct GigTypeMeta_ GigTypeMeta;
struct GigTypeMeta_
{
    GigArgType arg_type;
    GType gtype;
    char *qname;                // For flags, enums w/o a specialized GType
    uint16_t is_ptr:1;

    // For argument and return values
    uint16_t is_in:1;
    uint16_t is_out:1;

    // For return values
    uint16_t is_skip:1;         // TRUE when output is ignored

    // For pointers
    uint16_t is_caller_allocates:1;
    uint16_t is_optional:1;     // Out-only. Pass in NULL to ignore this.
    uint16_t is_nullable:1;     // For in, can pass in NULL. For out, may return NULL.

    // Error status
    uint16_t is_invalid:1;      // True when one of the arguments has invalid type
    uint16_t is_zero_terminated:1;
    uint16_t has_length_arg:1;
    uint16_t has_fixed_size:1;
    uint16_t is_unichar:1;
    uint16_t padding1:4;

    // For C array types
    int length_arg;
    int fixed_size;

    // For C element types
    size_t item_size;

    GigTransfer transfer;

    // Subtypes and callables
    uint16_t n_params;
    struct GigTypeMeta_ *params;

    void *callable_arg_map;     // A GigArgMap : void to avoid circular ref
};

const char *gig_arg_type_name(GigArgType type);
GigArgType gig_arg_type_from_name(const char *name);

void gig_meta_add_params(GigTypeMeta *meta, int n);
SCM gig_type_meta_to_il(GigTypeMeta *meta);
void gig_type_meta_from_il(SCM il, GigTypeMeta *meta);

size_t gig_meta_real_item_size(const GigTypeMeta *meta);
const char *gig_type_meta_describe(const GigTypeMeta *meta);
void gig_data_type_free(GigTypeMeta *meta);

#endif
