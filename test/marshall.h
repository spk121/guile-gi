/* -*- mode: C; c-file-style: "gnu"; indent-tabs-mode: nil; -*-
 * vim: tabstop=4 shiftwidth=4 expandtab
 */

#include <glib-object.h>

#include "gitestmacros.h"

#ifndef __MARSHALL_H__
#define __MARSHALL_H__

typedef struct _MarshallSimpleStruct MarshallSimpleStruct;
typedef struct _MarshallBoxedStruct MarshallBoxedStruct;

/* Constants */

#define MARSHALL_CONSTANT_NUMBER 42
#define MARSHALL_CONSTANT_UTF8   "const \xe2\x99\xa5 utf8"
#define MARSHALL_CONSTANT_UCS4   { 0x63, 0x6f, 0x6e, 0x73, 0x74, \
                                               0x20, 0x2665, 0x20, 0x75, 0x74, \
                                               0x66, 0x38 }

/* Booleans */

_GI_TEST_EXTERN
gboolean marshall_boolean_return_true (void);

_GI_TEST_EXTERN
gboolean marshall_boolean_return_false (void);


_GI_TEST_EXTERN
void marshall_boolean_in_true (gboolean v);

_GI_TEST_EXTERN
void marshall_boolean_in_false (gboolean v);


_GI_TEST_EXTERN
void marshall_boolean_out_true (gboolean *v);

_GI_TEST_EXTERN
void marshall_boolean_out_false (gboolean *v);


_GI_TEST_EXTERN
void marshall_boolean_inout_true_false (gboolean *v);

_GI_TEST_EXTERN
void marshall_boolean_inout_false_true (gboolean *v);


/* Integers */

_GI_TEST_EXTERN
gint8 marshall_int8_return_max (void);

_GI_TEST_EXTERN
gint8 marshall_int8_return_min (void);


_GI_TEST_EXTERN
void marshall_int8_in_max (gint8 v);

_GI_TEST_EXTERN
void marshall_int8_in_min (gint8 v);


_GI_TEST_EXTERN
void marshall_int8_out_max (gint8 *v);

_GI_TEST_EXTERN
void marshall_int8_out_min (gint8 *v);


_GI_TEST_EXTERN
void marshall_int8_inout_max_min (gint8 *v);

_GI_TEST_EXTERN
void marshall_int8_inout_min_max (gint8 *v);



_GI_TEST_EXTERN
guint8 marshall_uint8_return (void);


_GI_TEST_EXTERN
void marshall_uint8_in (guint8 v);


_GI_TEST_EXTERN
void marshall_uint8_out (guint8 *v);

_GI_TEST_EXTERN
void marshall_uint8_inout (guint8 *v);


_GI_TEST_EXTERN
gint16 marshall_int16_return_max (void);

_GI_TEST_EXTERN
gint16 marshall_int16_return_min (void);


_GI_TEST_EXTERN
void marshall_int16_in_max (gint16 v);

_GI_TEST_EXTERN
void marshall_int16_in_min (gint16 v);


_GI_TEST_EXTERN
void marshall_int16_out_max (gint16 *v);

_GI_TEST_EXTERN
void marshall_int16_out_min (gint16 *v);


_GI_TEST_EXTERN
void marshall_int16_inout_max_min (gint16 *v);

_GI_TEST_EXTERN
void marshall_int16_inout_min_max (gint16 *v);



_GI_TEST_EXTERN
guint16 marshall_uint16_return (void);


_GI_TEST_EXTERN
void marshall_uint16_in (guint16 v);


_GI_TEST_EXTERN
void marshall_uint16_out (guint16 *v);

_GI_TEST_EXTERN
void marshall_uint16_inout (guint16 *v);



_GI_TEST_EXTERN
gint32 marshall_int32_return_max (void);

_GI_TEST_EXTERN
gint32 marshall_int32_return_min (void);


_GI_TEST_EXTERN
void marshall_int32_in_max (gint32 v);

_GI_TEST_EXTERN
void marshall_int32_in_min (gint32 v);


_GI_TEST_EXTERN
void marshall_int32_out_max (gint32 *v);

_GI_TEST_EXTERN
void marshall_int32_out_min (gint32 *v);


_GI_TEST_EXTERN
void marshall_int32_inout_max_min (gint32 *v);

_GI_TEST_EXTERN
void marshall_int32_inout_min_max (gint32 *v);



_GI_TEST_EXTERN
guint32 marshall_uint32_return (void);


_GI_TEST_EXTERN
void marshall_uint32_in (guint32 v);


_GI_TEST_EXTERN
void marshall_uint32_out (guint32 *v);

_GI_TEST_EXTERN
void marshall_uint32_inout (guint32 *v);



_GI_TEST_EXTERN
gint64 marshall_int64_return_max (void);

_GI_TEST_EXTERN
gint64 marshall_int64_return_min (void);


_GI_TEST_EXTERN
void marshall_int64_in_max (gint64 v);

_GI_TEST_EXTERN
void marshall_int64_in_min (gint64 v);


_GI_TEST_EXTERN
void marshall_int64_out_max (gint64 *v);

_GI_TEST_EXTERN
void marshall_int64_out_min (gint64 *v);


_GI_TEST_EXTERN
void marshall_int64_inout_max_min (gint64 *v);

_GI_TEST_EXTERN
void marshall_int64_inout_min_max (gint64 *v);



_GI_TEST_EXTERN
guint64 marshall_uint64_return (void);


_GI_TEST_EXTERN
void marshall_uint64_in (guint64 v);


_GI_TEST_EXTERN
void marshall_uint64_out (guint64 *v);

_GI_TEST_EXTERN
void marshall_uint64_inout (guint64 *v);



_GI_TEST_EXTERN
gshort marshall_short_return_max (void);

_GI_TEST_EXTERN
gshort marshall_short_return_min (void);


_GI_TEST_EXTERN
void marshall_short_in_max (gshort short_);

_GI_TEST_EXTERN
void marshall_short_in_min (gshort short_);


_GI_TEST_EXTERN
void marshall_short_out_max (gshort *short_);

_GI_TEST_EXTERN
void marshall_short_out_min (gshort *short_);


_GI_TEST_EXTERN
void marshall_short_inout_max_min (gshort *short_);

_GI_TEST_EXTERN
void marshall_short_inout_min_max (gshort *short_);



_GI_TEST_EXTERN
gushort marshall_ushort_return (void);


_GI_TEST_EXTERN
void marshall_ushort_in (gushort ushort_);


_GI_TEST_EXTERN
void marshall_ushort_out (gushort *ushort_);

_GI_TEST_EXTERN
void marshall_ushort_inout (gushort *ushort_);



_GI_TEST_EXTERN
gint marshall_int_return_max (void);

_GI_TEST_EXTERN
gint marshall_int_return_min (void);


_GI_TEST_EXTERN
void marshall_int_in_max (gint int_);

_GI_TEST_EXTERN
void marshall_int_in_min (gint int_);


_GI_TEST_EXTERN
void marshall_int_out_max (gint *int_);

_GI_TEST_EXTERN
void marshall_int_out_min (gint *int_);


_GI_TEST_EXTERN
void marshall_int_inout_max_min (gint *int_);

_GI_TEST_EXTERN
void marshall_int_inout_min_max (gint *int_);


_GI_TEST_EXTERN
guint marshall_uint_return (void);


_GI_TEST_EXTERN
void marshall_uint_in (guint uint_);


_GI_TEST_EXTERN
void marshall_uint_out (guint *uint_);

_GI_TEST_EXTERN
void marshall_uint_inout (guint *uint_);


_GI_TEST_EXTERN
glong marshall_long_return_max (void);

_GI_TEST_EXTERN
glong marshall_long_return_min (void);


_GI_TEST_EXTERN
void marshall_long_in_max (glong long_);

_GI_TEST_EXTERN
void marshall_long_in_min (glong long_);


_GI_TEST_EXTERN
void marshall_long_out_max (glong *long_);

_GI_TEST_EXTERN
void marshall_long_out_min (glong *long_);


_GI_TEST_EXTERN
void marshall_long_inout_max_min (glong *long_);

_GI_TEST_EXTERN
void marshall_long_inout_min_max (glong *long_);


_GI_TEST_EXTERN
gulong marshall_ulong_return (void);


_GI_TEST_EXTERN
void marshall_ulong_in (gulong ulong_);


_GI_TEST_EXTERN
void marshall_ulong_out (gulong *ulong_);

_GI_TEST_EXTERN
void marshall_ulong_inout (gulong *ulong_);


_GI_TEST_EXTERN
gssize marshall_ssize_return_max (void);

_GI_TEST_EXTERN
gssize marshall_ssize_return_min (void);


_GI_TEST_EXTERN
void marshall_ssize_in_max (gssize ssize);

_GI_TEST_EXTERN
void marshall_ssize_in_min (gssize ssize);


_GI_TEST_EXTERN
void marshall_ssize_out_max (gssize *ssize);

_GI_TEST_EXTERN
void marshall_ssize_out_min (gssize *ssize);


_GI_TEST_EXTERN
void marshall_ssize_inout_max_min (gssize *ssize);

_GI_TEST_EXTERN
void marshall_ssize_inout_min_max (gssize *ssize);



_GI_TEST_EXTERN
gsize marshall_size_return (void);


_GI_TEST_EXTERN
void marshall_size_in (gsize size);


_GI_TEST_EXTERN
void marshall_size_out (gsize *size);

_GI_TEST_EXTERN
void marshall_size_inout (gsize *size);


/* Floating-point */

_GI_TEST_EXTERN
gfloat marshall_float_return (void);


_GI_TEST_EXTERN
void marshall_float_in (gfloat v);


_GI_TEST_EXTERN
void marshall_float_out (gfloat *v);


_GI_TEST_EXTERN
void marshall_float_inout (gfloat *v);



_GI_TEST_EXTERN
gdouble marshall_double_return (void);


_GI_TEST_EXTERN
void marshall_double_in (gdouble v);


_GI_TEST_EXTERN
void marshall_double_out (gdouble *v);


_GI_TEST_EXTERN
void marshall_double_inout (gdouble *v);


/* Timestamps */

_GI_TEST_EXTERN
time_t marshall_time_t_return (void);


_GI_TEST_EXTERN
void marshall_time_t_in (time_t v);


_GI_TEST_EXTERN
void marshall_time_t_out (time_t *v);


_GI_TEST_EXTERN
void marshall_time_t_inout (time_t *v);


/* GType */

_GI_TEST_EXTERN
GType marshall_gtype_return (void);


_GI_TEST_EXTERN
GType marshall_gtype_string_return (void);


_GI_TEST_EXTERN
void marshall_gtype_in (GType gtype);


_GI_TEST_EXTERN
void marshall_gtype_string_in (GType gtype);


_GI_TEST_EXTERN
void marshall_gtype_out (GType *gtype);


_GI_TEST_EXTERN
void marshall_gtype_string_out (GType *gtype);


_GI_TEST_EXTERN
void marshall_gtype_inout (GType *gtype);


/* UTF-8 */

_GI_TEST_EXTERN
const gchar *marshall_utf8_none_return (void);

_GI_TEST_EXTERN
gchar *marshall_utf8_full_return (void);


_GI_TEST_EXTERN
void marshall_utf8_none_in (const gchar *utf8);

_GI_TEST_EXTERN
void marshall_utf8_full_in (gchar *utf8);


_GI_TEST_EXTERN
void marshall_utf8_none_out (const gchar **utf8);

_GI_TEST_EXTERN
void marshall_utf8_full_out (gchar **utf8);


_GI_TEST_EXTERN
void marshall_utf8_dangling_out (gchar **utf8);


_GI_TEST_EXTERN
void marshall_utf8_none_inout (const gchar **utf8);

_GI_TEST_EXTERN
void marshall_utf8_full_inout (gchar **utf8);


_GI_TEST_EXTERN
GSList *marshall_filename_list_return (void);


_GI_TEST_EXTERN
void marshall_utf8_as_uint8array_in (const guint8 *array,
                                                 gsize         len);


/* Enum */

typedef enum
{
  MARSHALL_ENUM_VALUE1,
  MARSHALL_ENUM_VALUE2,
  MARSHALL_ENUM_VALUE3 = 42
} MarshallEnum;

typedef enum
{
  MARSHALL_SECOND_ENUM_SECONDVALUE1,
  MARSHALL_SECOND_ENUM_SECONDVALUE2,
} MarshallSecondEnum;


_GI_TEST_EXTERN
MarshallEnum marshall_enum_returnv (void);


_GI_TEST_EXTERN
void marshall_enum_in (MarshallEnum v);


_GI_TEST_EXTERN
void marshall_enum_out (MarshallEnum *v);


_GI_TEST_EXTERN
void marshall_enum_inout (MarshallEnum *v);


/* GEnum */

typedef enum
{
  MARSHALL_GENUM_VALUE1,
  MARSHALL_GENUM_VALUE2,
  MARSHALL_GENUM_VALUE3 = 42
} MarshallGEnum;

_GI_TEST_EXTERN
GType marshall_genum_get_type (void) G_GNUC_CONST;
#define MARSHALL_TYPE_GENUM (marshall_genum_get_type ())


_GI_TEST_EXTERN
MarshallGEnum marshall_genum_returnv (void);


_GI_TEST_EXTERN
void marshall_genum_in (MarshallGEnum v);


_GI_TEST_EXTERN
void marshall_genum_out (MarshallGEnum *v);


_GI_TEST_EXTERN
void marshall_genum_inout (MarshallGEnum *v);


/* GFlags */

typedef enum
{
  MARSHALL_FLAGS_VALUE1 = 1 << 0,
  MARSHALL_FLAGS_VALUE2 = 1 << 1,
  MARSHALL_FLAGS_VALUE3 = 1 << 2,
  MARSHALL_FLAGS_MASK = MARSHALL_FLAGS_VALUE1 |
                                    MARSHALL_FLAGS_VALUE2,
  MARSHALL_FLAGS_MASK2 = MARSHALL_FLAGS_MASK
} MarshallFlags;

_GI_TEST_EXTERN
GType marshall_flags_get_type (void) G_GNUC_CONST;
#define MARSHALL_TYPE_FLAGS (marshall_flags_get_type ())


_GI_TEST_EXTERN
MarshallFlags marshall_flags_returnv (void);


_GI_TEST_EXTERN
void marshall_flags_in (MarshallFlags v);

_GI_TEST_EXTERN
void marshall_flags_in_zero (MarshallFlags v);


_GI_TEST_EXTERN
void marshall_flags_out (MarshallFlags *v);


_GI_TEST_EXTERN
void marshall_flags_inout (MarshallFlags *v);

/* Flags with no GType */

typedef enum
{
  MARSHALL_NO_TYPE_FLAGS_VALUE1 = 1 << 0,
  MARSHALL_NO_TYPE_FLAGS_VALUE2 = 1 << 1,
  MARSHALL_NO_TYPE_FLAGS_VALUE3 = 1 << 2,
  MARSHALL_NO_TYPE_FLAGS_MASK = MARSHALL_NO_TYPE_FLAGS_VALUE1 |
                                            MARSHALL_NO_TYPE_FLAGS_VALUE2,
  MARSHALL_NO_TYPE_FLAGS_MASK2 = MARSHALL_FLAGS_MASK
} MarshallNoTypeFlags;


_GI_TEST_EXTERN
MarshallNoTypeFlags marshall_no_type_flags_returnv (void);


_GI_TEST_EXTERN
void marshall_no_type_flags_in (MarshallNoTypeFlags v);

_GI_TEST_EXTERN
void marshall_no_type_flags_in_zero (MarshallNoTypeFlags v);


_GI_TEST_EXTERN
void marshall_no_type_flags_out (MarshallNoTypeFlags *v);


_GI_TEST_EXTERN
void marshall_no_type_flags_inout (MarshallNoTypeFlags *v);

/* Arrays */


_GI_TEST_EXTERN
gboolean marshall_init_function (gint *n_args, char ***argv);

/* Fixed-size */

_GI_TEST_EXTERN
const gint *marshall_array_fixed_int_return (void);

_GI_TEST_EXTERN
const gshort *marshall_array_fixed_short_return (void);


_GI_TEST_EXTERN
void marshall_array_fixed_int_in (const gint *ints);

_GI_TEST_EXTERN
void marshall_array_fixed_short_in (const gshort *shorts);


_GI_TEST_EXTERN
void marshall_array_fixed_out (gint **ints);


_GI_TEST_EXTERN
void marshall_array_fixed_out_struct (MarshallSimpleStruct **structs);


_GI_TEST_EXTERN
void marshall_array_fixed_inout (gint **ints);

/* Variable-size */


_GI_TEST_EXTERN
const gint *marshall_array_return (gint *length);

_GI_TEST_EXTERN
const gint *marshall_array_return_etc (gint first, gint *length, gint last, gint *sum);


_GI_TEST_EXTERN
void marshall_array_in (const gint *ints, gint length);

_GI_TEST_EXTERN
void marshall_array_in_len_before (gint length, const gint *ints);

_GI_TEST_EXTERN
void marshall_array_in_len_zero_terminated (const gint *ints, gint length);

_GI_TEST_EXTERN
void marshall_array_string_in (const gchar **strings, gint length);

_GI_TEST_EXTERN
void marshall_array_uint8_in (const guint8 *chars, gint length);

_GI_TEST_EXTERN
void marshall_array_int64_in (const gint64 *ints, gint length);

_GI_TEST_EXTERN
void marshall_array_uint64_in (const guint64 *ints, gint length);

_GI_TEST_EXTERN
void marshall_array_unichar_in (const gunichar *chars, gint length);

_GI_TEST_EXTERN
void marshall_array_bool_in (const gboolean *bools, gint length);

_GI_TEST_EXTERN
void marshall_array_struct_in (MarshallBoxedStruct **structs, gint length);

_GI_TEST_EXTERN
void marshall_array_struct_value_in (MarshallBoxedStruct *structs, gint length);

_GI_TEST_EXTERN
void marshall_array_struct_take_in (MarshallBoxedStruct **structs, gint length);

_GI_TEST_EXTERN
void marshall_array_simple_struct_in (MarshallSimpleStruct *structs, gint length);

_GI_TEST_EXTERN
void marshall_multi_array_key_value_in (gint length, const gchar **keys, const GValue *values);

_GI_TEST_EXTERN
void marshall_array_enum_in (MarshallEnum *_enum, gint length);

_GI_TEST_EXTERN
void marshall_array_in_guint64_len (const gint *ints, guint64 length);

_GI_TEST_EXTERN
void marshall_array_in_guint8_len (const gint *ints, guint8 length);


_GI_TEST_EXTERN
void marshall_array_out (gint **ints, gint *length);

_GI_TEST_EXTERN
void marshall_array_out_etc (gint first, gint **ints, gint *length, gint last, gint *sum);

_GI_TEST_EXTERN
void marshall_array_bool_out (const gboolean **bools, gint *length);

_GI_TEST_EXTERN
void marshall_array_unichar_out (const gunichar **chars, gint *length);

_GI_TEST_EXTERN
void marshall_array_inout (gint **ints, gint *length);

_GI_TEST_EXTERN
void marshall_array_inout_etc (gint first, gint **ints, gint *length, gint last, gint *sum);


_GI_TEST_EXTERN
void marshall_array_in_nonzero_nonlen (gint first, const guint8 *chars);

/* Zero-terminated */


_GI_TEST_EXTERN
const gchar **marshall_array_zero_terminated_return (void);

_GI_TEST_EXTERN
gchar **marshall_array_zero_terminated_return_null (void);

_GI_TEST_EXTERN
MarshallBoxedStruct **marshall_array_zero_terminated_return_struct (void);

_GI_TEST_EXTERN
gunichar *marshall_array_zero_terminated_return_unichar (void);


_GI_TEST_EXTERN
void marshall_array_zero_terminated_in (gchar **utf8s);


_GI_TEST_EXTERN
void marshall_array_zero_terminated_out (const gchar ***utf8s);


_GI_TEST_EXTERN
void marshall_array_zero_terminated_inout (const gchar ***utf8s);


_GI_TEST_EXTERN
GVariant **marshall_array_gvariant_none_in (GVariant **variants);


_GI_TEST_EXTERN
GVariant **marshall_array_gvariant_container_in (GVariant **variants);


_GI_TEST_EXTERN
GVariant **marshall_array_gvariant_full_in (GVariant **variants);


/* GArray */

_GI_TEST_EXTERN
GArray *marshall_garray_int_none_return (void);

_GI_TEST_EXTERN
GArray *marshall_garray_uint64_none_return (void);

_GI_TEST_EXTERN
GArray *marshall_garray_utf8_none_return (void);

_GI_TEST_EXTERN
GArray *marshall_garray_utf8_container_return (void);

_GI_TEST_EXTERN
GArray *marshall_garray_utf8_full_return (void);


_GI_TEST_EXTERN
void marshall_garray_int_none_in (GArray *array_);

_GI_TEST_EXTERN
void marshall_garray_uint64_none_in (GArray *array_);

_GI_TEST_EXTERN
void marshall_garray_utf8_none_in (GArray *array_);


_GI_TEST_EXTERN
void marshall_garray_utf8_none_out (GArray **array_);

_GI_TEST_EXTERN
void marshall_garray_utf8_container_out (GArray **array_);

_GI_TEST_EXTERN
void marshall_garray_utf8_full_out (GArray **array_);

_GI_TEST_EXTERN
void marshall_garray_utf8_full_out_caller_allocated (GArray *array_);


_GI_TEST_EXTERN
void marshall_garray_utf8_none_inout (GArray **array_);

_GI_TEST_EXTERN
void marshall_garray_utf8_container_inout (GArray **array_);

_GI_TEST_EXTERN
void marshall_garray_utf8_full_inout (GArray **array_);

_GI_TEST_EXTERN
void marshall_garray_bool_none_in (GArray *array_);

_GI_TEST_EXTERN
void marshall_garray_unichar_none_in (GArray *array_);

/* GPtrArray */

_GI_TEST_EXTERN
GPtrArray *marshall_gptrarray_utf8_none_return (void);

_GI_TEST_EXTERN
GPtrArray *marshall_gptrarray_utf8_container_return (void);

_GI_TEST_EXTERN
GPtrArray *marshall_gptrarray_utf8_full_return (void);


_GI_TEST_EXTERN
void marshall_gptrarray_utf8_none_in (GPtrArray *parray_);


_GI_TEST_EXTERN
void marshall_gptrarray_utf8_none_out (GPtrArray **parray_);

_GI_TEST_EXTERN
void marshall_gptrarray_utf8_container_out (GPtrArray **parray_);

_GI_TEST_EXTERN
void marshall_gptrarray_utf8_full_out (GPtrArray **parray_);


_GI_TEST_EXTERN
void marshall_gptrarray_utf8_none_inout (GPtrArray **parray_);

_GI_TEST_EXTERN
void marshall_gptrarray_utf8_container_inout (GPtrArray **parray_);

_GI_TEST_EXTERN
void marshall_gptrarray_utf8_full_inout (GPtrArray **parray_);

/* GByteArray */

_GI_TEST_EXTERN
GByteArray *marshall_bytearray_full_return (void);

_GI_TEST_EXTERN
void marshall_bytearray_none_in (GByteArray* v);

/* GBytes */

_GI_TEST_EXTERN
GBytes *marshall_gbytes_full_return (void);

_GI_TEST_EXTERN
void marshall_gbytes_none_in (GBytes* v);

/* GStrv */

_GI_TEST_EXTERN
GStrv marshall_gstrv_return (void);

_GI_TEST_EXTERN
void marshall_gstrv_in (GStrv g_strv);

_GI_TEST_EXTERN
void marshall_gstrv_out (GStrv *g_strv);

_GI_TEST_EXTERN
void marshall_gstrv_inout (GStrv *g_strv);

/* GList */

_GI_TEST_EXTERN
GList *marshall_glist_int_none_return (void);

_GI_TEST_EXTERN
GList *marshall_glist_uint32_none_return (void);

_GI_TEST_EXTERN
GList *marshall_glist_utf8_none_return (void);

_GI_TEST_EXTERN
GList *marshall_glist_utf8_container_return (void);

_GI_TEST_EXTERN
GList *marshall_glist_utf8_full_return (void);


_GI_TEST_EXTERN
void marshall_glist_int_none_in (GList *list);

_GI_TEST_EXTERN
void marshall_glist_uint32_none_in (GList *list);

_GI_TEST_EXTERN
void marshall_glist_utf8_none_in (GList *list);


_GI_TEST_EXTERN
void marshall_glist_utf8_none_out (GList **list);

_GI_TEST_EXTERN
void marshall_glist_utf8_container_out (GList **list);

_GI_TEST_EXTERN
void marshall_glist_utf8_full_out (GList **list);


_GI_TEST_EXTERN
void marshall_glist_utf8_none_inout (GList **list);

_GI_TEST_EXTERN
void marshall_glist_utf8_container_inout (GList **list);

_GI_TEST_EXTERN
void marshall_glist_utf8_full_inout (GList **list);


/* GSList */

_GI_TEST_EXTERN
GSList *marshall_gslist_int_none_return (void);

_GI_TEST_EXTERN
GSList *marshall_gslist_utf8_none_return (void);

_GI_TEST_EXTERN
GSList *marshall_gslist_utf8_container_return (void);

_GI_TEST_EXTERN
GSList *marshall_gslist_utf8_full_return (void);


_GI_TEST_EXTERN
void marshall_gslist_int_none_in (GSList *list);

_GI_TEST_EXTERN
void marshall_gslist_utf8_none_in (GSList *list);


_GI_TEST_EXTERN
void marshall_gslist_utf8_none_out (GSList **list);

_GI_TEST_EXTERN
void marshall_gslist_utf8_container_out (GSList **list);

_GI_TEST_EXTERN
void marshall_gslist_utf8_full_out (GSList **list);


_GI_TEST_EXTERN
void marshall_gslist_utf8_none_inout (GSList **list);

_GI_TEST_EXTERN
void marshall_gslist_utf8_container_inout (GSList **list);

_GI_TEST_EXTERN
void marshall_gslist_utf8_full_inout (GSList **list);


/* GHashTable */

_GI_TEST_EXTERN
GHashTable *marshall_ghashtable_int_none_return (void);

_GI_TEST_EXTERN
GHashTable *marshall_ghashtable_utf8_none_return (void);

_GI_TEST_EXTERN
GHashTable *marshall_ghashtable_utf8_container_return (void);

_GI_TEST_EXTERN
GHashTable *marshall_ghashtable_utf8_full_return (void);


_GI_TEST_EXTERN
void marshall_ghashtable_int_none_in (GHashTable *hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_utf8_none_in (GHashTable *hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_double_in (GHashTable *hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_float_in (GHashTable *hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_int64_in (GHashTable *hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_uint64_in (GHashTable *hash_table);


_GI_TEST_EXTERN
void marshall_ghashtable_utf8_container_in (GHashTable *hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_utf8_full_in (GHashTable *hash_table);


_GI_TEST_EXTERN
void marshall_ghashtable_utf8_none_out (GHashTable **hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_utf8_container_out (GHashTable **hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_utf8_full_out (GHashTable **hash_table);


_GI_TEST_EXTERN
void marshall_ghashtable_utf8_none_inout (GHashTable **hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_utf8_container_inout (GHashTable **hash_table);

_GI_TEST_EXTERN
void marshall_ghashtable_utf8_full_inout (GHashTable **hash_table);


/* GValue */

_GI_TEST_EXTERN
GValue *marshall_gvalue_return (void);


_GI_TEST_EXTERN
void marshall_gvalue_in (GValue *value);

_GI_TEST_EXTERN
void marshall_gvalue_int64_in (GValue *value);

_GI_TEST_EXTERN
void marshall_gvalue_in_with_type (GValue *value, GType type);

_GI_TEST_EXTERN
void marshall_gvalue_in_with_modification (GValue *value);


_GI_TEST_EXTERN
void marshall_gvalue_in_enum (GValue *value);


_GI_TEST_EXTERN
void marshall_gvalue_out (GValue **value);

_GI_TEST_EXTERN
void marshall_gvalue_int64_out (GValue **value);

_GI_TEST_EXTERN
void marshall_gvalue_out_caller_allocates (GValue *value);


_GI_TEST_EXTERN
void marshall_gvalue_inout (GValue **value);


_GI_TEST_EXTERN
void marshall_gvalue_flat_array (guint         n_values,
                                             const GValue *values);


_GI_TEST_EXTERN
GValue *marshall_return_gvalue_flat_array (void);


_GI_TEST_EXTERN
GValue *marshall_gvalue_flat_array_round_trip (const GValue one,
                                                           const GValue two,
                                                           const GValue three);

/* GClosure */

_GI_TEST_EXTERN
void marshall_gclosure_in (GClosure *closure);

_GI_TEST_EXTERN
GClosure *marshall_gclosure_return (void);

/* Callback return values */

/**
 * MarshallCallbackReturnValueOnly:
 */
typedef glong (* MarshallCallbackReturnValueOnly) (void);


_GI_TEST_EXTERN
glong marshall_callback_return_value_only (MarshallCallbackReturnValueOnly callback);

/**
 * marshall_new_callback_return_value_only:
 * @callback: (out) (scope call):
 */
_GI_TEST_EXTERN
void marshall_new_callback_return_value_only(MarshallCallbackReturnValueOnly *callback);

/**
 * MarshallCallbackOneOutParameter:
 * @a: (out):
 */
typedef void (* MarshallCallbackOneOutParameter) (gfloat *a);


_GI_TEST_EXTERN
void marshall_callback_one_out_parameter (MarshallCallbackOneOutParameter  callback,
                                                      gfloat                                    *a);

/**
 * MarshallCallbackMultipleOutParameters:
 * @a: (out):
 * @b: (out):
 */
typedef void (* MarshallCallbackMultipleOutParameters) (gfloat *a, gfloat *b);


_GI_TEST_EXTERN
void marshall_callback_multiple_out_parameters (MarshallCallbackMultipleOutParameters  callback,
                                                            gfloat                                          *a,
                                                            gfloat                                          *b);

/**
 * MarshallCallbackReturnValueAndOneOutParameter:
 * @a: (out):
 */
typedef glong (* MarshallCallbackReturnValueAndOneOutParameter) (glong *a);


_GI_TEST_EXTERN
glong marshall_callback_return_value_and_one_out_parameter (MarshallCallbackReturnValueAndOneOutParameter  callback,
                                                                        glong                                                   *a);

/**
 * MarshallCallbackReturnValueAndMultipleOutParameters:
 * @a: (out):
 * @b: (out):
 */
typedef glong (* MarshallCallbackReturnValueAndMultipleOutParameters) (glong *a, glong *b);


_GI_TEST_EXTERN
glong marshall_callback_return_value_and_multiple_out_parameters (MarshallCallbackReturnValueAndMultipleOutParameters  callback,
                                                                              glong                                                         *a,
                                                                              glong                                                         *b);

/**
 * MarshallCallbackOwnedBoxed
* @box: (transfer none): the boxed structure.
 */
typedef void (* MarshallCallbackOwnedBoxed) (MarshallBoxedStruct *box,
						       void                      *user_data);


_GI_TEST_EXTERN
glong marshall_callback_owned_boxed (MarshallCallbackOwnedBoxed  callback,
                                                 void *callback_data);

/* Pointer */


_GI_TEST_EXTERN
gpointer marshall_pointer_in_return (gpointer pointer);

/* Structure */

struct _MarshallSimpleStruct {
    glong long_;
    gint8 int8;
};

typedef struct {
    MarshallSimpleStruct simple_struct;
} MarshallNestedStruct;

typedef struct {
    MarshallNestedStruct *pointer;
} MarshallNotSimpleStruct;


_GI_TEST_EXTERN
MarshallSimpleStruct *marshall_simple_struct_returnv (void);


_GI_TEST_EXTERN
void marshall_simple_struct_inv (MarshallSimpleStruct *struct_);


_GI_TEST_EXTERN
void marshall_simple_struct_method (MarshallSimpleStruct *struct_);


typedef struct {
    glong long_;
} MarshallPointerStruct;


_GI_TEST_EXTERN
GType marshall_pointer_struct_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
MarshallPointerStruct *marshall_pointer_struct_returnv (void);


_GI_TEST_EXTERN
void marshall_pointer_struct_inv (MarshallPointerStruct *struct_);

struct _MarshallBoxedStruct {
    glong long_;
    gchar *string_;
    GStrv g_strv;
};


_GI_TEST_EXTERN
GType marshall_boxed_struct_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
MarshallBoxedStruct *marshall_boxed_struct_new (void);


_GI_TEST_EXTERN
MarshallBoxedStruct *marshall_boxed_struct_returnv (void);


_GI_TEST_EXTERN
void marshall_boxed_struct_inv (MarshallBoxedStruct *struct_);


_GI_TEST_EXTERN
void marshall_boxed_struct_out (MarshallBoxedStruct **struct_);


_GI_TEST_EXTERN
void marshall_boxed_struct_inout (MarshallBoxedStruct **struct_);

typedef union {
    glong long_;
} MarshallUnion;


_GI_TEST_EXTERN
GType marshall_union_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
MarshallUnion *marshall_union_returnv (void);


_GI_TEST_EXTERN
void marshall_union_inv (MarshallUnion *union_);


_GI_TEST_EXTERN
void marshall_union_method (MarshallUnion *union_);

 /* Object */

#define MARSHALL_TYPE_OBJECT             (marshall_object_get_type ())
#define MARSHALL_OBJECT(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), MARSHALL_TYPE_OBJECT, MarshallObject))
#define MARSHALL_OBJECT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), MARSHALL_TYPE_OBJECT, MarshallObjectClass))
#define MARSHALL_IS_OBJECT(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MARSHALL_TYPE_OBJECT))
#define MARSHALL_IS_OBJECT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), MARSHALL_TYPE_OBJECT))
#define MARSHALL_OBJECT_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), MARSHALL_TYPE_OBJECT, MarshallObjectClass))

typedef struct _MarshallObjectClass MarshallObjectClass;
typedef struct _MarshallObject MarshallObject;

typedef int (* MarshallCallbackIntInt) (int val, void *user_data);

struct _MarshallObjectClass
{
	GObjectClass parent_class;

    /**
     * MarshallObjectClass::method_int8_in:
     * @in: (in):
     */
    void (* method_int8_in) (MarshallObject *self, gint8 in);

    /**
     * MarshallObjectClass::method_int8_out:
     * @out: (out):
     */
    void (* method_int8_out) (MarshallObject *self, gint8 *out);

    /**
     * MarshallObjectClass::method_int8_arg_and_out_caller:
     * @out: (out caller-allocates):
     */
    void (* method_int8_arg_and_out_caller) (MarshallObject *self, gint8 arg, gint8 *out);

    /**
     * MarshallObjectClass::method_int8_arg_and_out_callee:
     * @out: (out):
     */
    void (* method_int8_arg_and_out_callee) (MarshallObject *self, gint8 arg, gint8 **out);

    /**
     * MarshallObjectClass::method_str_arg_out_ret:
     * @out: (out caller-allocates):
     *
     * Returns: (transfer none)
     */
    const gchar* (* method_str_arg_out_ret) (MarshallObject *self, const gchar* arg, guint *out);

    /**
     * MarshallObjectClass::method_with_default_implementation:
     * @in: (in):
     */
    void (* method_with_default_implementation) (MarshallObject *self, gint8 in);

    /**
     * MarshallObjectClass::method_deep_hierarchy:
     * @in: (in):
     */
    void (* method_deep_hierarchy) (MarshallObject *self, gint8 in);

    void (* vfunc_with_callback) (MarshallObject *self,
                                  MarshallCallbackIntInt callback,
                                  void *callback_data);

    /**
     * MarshallObjectClass::vfunc_return_value_only:
     */
    glong (* vfunc_return_value_only) (MarshallObject *self);

    /**
     * MarshallObjectClass::vfunc_one_out_parameter:
     * @a: (out):
     */
    void  (* vfunc_one_out_parameter) (MarshallObject *self, gfloat *a);

    /**
     * MarshallObjectClass::vfunc_multiple_out_parameters:
     * @a: (out):
     * @b: (out):
     */
    void  (* vfunc_multiple_out_parameters) (MarshallObject *self, gfloat *a, gfloat *b);

    /**
     * MarshallObjectClass::vfunc_caller_allocated_out_parameter:
     * @a: (out):
     */
    void  (* vfunc_caller_allocated_out_parameter) (MarshallObject *self, GValue *a);

    /**
     * MarshallObjectClass::vfunc_array_out_parameter:
     * @a: (out) (array zero-terminated):
     */
    void  (* vfunc_array_out_parameter) (MarshallObject *self, gfloat **a);

    /**
     * MarshallObjectClass::vfunc_return_value_and_one_out_parameter:
     * @a: (out):
     */
    glong (* vfunc_return_value_and_one_out_parameter) (MarshallObject *self, glong *a);

    /**
     * MarshallObjectClass::vfunc_return_value_and_multiple_out_parameters:
     * @a: (out):
     * @b: (out):
     */
    glong (* vfunc_return_value_and_multiple_out_parameters) (MarshallObject *self, glong *a, glong *b);

    /**
     * MarshallObjectClass::vfunc_meth_with_err:
     * @x:
     * @error: A #GError
     */
    gboolean (*vfunc_meth_with_err) (MarshallObject *object, gint x, GError **error);

    /**
     * MarshallObjectClass::vfunc_return_enum:
     */
    MarshallEnum (* vfunc_return_enum) (MarshallObject *self);

    /**
     * MarshallObjectClass::vfunc_out_enum:
     * @_enum: (out):
     */
    void (* vfunc_out_enum) (MarshallObject *self, MarshallEnum *_enum);

    /**
     * MarshallObjectClass::vfunc_return_object_transfer_none:
     *
     * Returns: (transfer none)
     */
    GObject* (* vfunc_return_object_transfer_none) (MarshallObject *self);

    /**
     * MarshallObjectClass::vfunc_return_object_transfer_full:
     *
     * Returns: (transfer full)
     */
    GObject* (* vfunc_return_object_transfer_full) (MarshallObject *self);

    /**
     * MarshallObjectClass::vfunc_out_object_transfer_none:
     * @object: (out) (transfer none):
     */
    void (* vfunc_out_object_transfer_none) (MarshallObject *self, GObject **object);

    /**
     * MarshallObjectClass::vfunc_out_object_transfer_full:
     * @object: (out) (transfer full):
     */
    void (* vfunc_out_object_transfer_full) (MarshallObject *self, GObject **object);

    /**
     * MarshallObjectClass::vfunc_in_object_transfer_none:
     * @object: (in) (transfer none):
     */
    void (* vfunc_in_object_transfer_none) (MarshallObject *self, GObject *object);

    /**
     * MarshallObjectClass::vfunc_in_object_transfer_full:
     * @object: (in) (transfer full):
     */
    void (* vfunc_in_object_transfer_full) (MarshallObject *self, GObject *object);
};

struct _MarshallObject
{
	GObject parent_instance;

    gint int_;
};


_GI_TEST_EXTERN
GType marshall_object_get_type (void) G_GNUC_CONST;

_GI_TEST_EXTERN
void marshall_object_static_method (void);

_GI_TEST_EXTERN
void marshall_object_method (MarshallObject *object);

_GI_TEST_EXTERN
void marshall_object_overridden_method (MarshallObject *object);

_GI_TEST_EXTERN
MarshallObject *marshall_object_new (gint int_);
MarshallObject *marshall_object_new_fail (gint int_, GError **error);


_GI_TEST_EXTERN
void marshall_object_method_array_in (MarshallObject *object, const gint *ints, gint length);

_GI_TEST_EXTERN
void marshall_object_method_array_out (MarshallObject *object, gint **ints, gint *length);

_GI_TEST_EXTERN
void marshall_object_method_array_inout (MarshallObject *object, gint **ints, gint *length);

_GI_TEST_EXTERN
const gint *marshall_object_method_array_return (MarshallObject *object, gint *length);


_GI_TEST_EXTERN
void marshall_object_method_int8_in (MarshallObject *object, gint8 in);

_GI_TEST_EXTERN
void marshall_object_method_int8_out (MarshallObject *object, gint8 *out);

_GI_TEST_EXTERN
void marshall_object_method_int8_arg_and_out_caller (MarshallObject *object, gint8 arg, gint8 *out);

_GI_TEST_EXTERN
void marshall_object_method_int8_arg_and_out_callee (MarshallObject *object, gint8 arg, gint8 **out);

_GI_TEST_EXTERN
const gchar* marshall_object_method_str_arg_out_ret (MarshallObject *object, const gchar* arg, guint *out);

_GI_TEST_EXTERN
void marshall_object_method_with_default_implementation (MarshallObject *object, gint8 in);

_GI_TEST_EXTERN
void marshall_object_method_variant_array_in (MarshallObject *object, GVariant **in, gsize n_in);


_GI_TEST_EXTERN
glong marshall_object_vfunc_return_value_only (MarshallObject *self);

_GI_TEST_EXTERN
void marshall_object_vfunc_one_out_parameter (MarshallObject *self, gfloat *a);

_GI_TEST_EXTERN
void marshall_object_vfunc_multiple_out_parameters (MarshallObject *self, gfloat *a, gfloat *b);

_GI_TEST_EXTERN
void marshall_object_vfunc_caller_allocated_out_parameter (MarshallObject *self, GValue *a);

_GI_TEST_EXTERN
void marshall_object_vfunc_array_out_parameter (MarshallObject *self, gfloat **a);

_GI_TEST_EXTERN
glong marshall_object_vfunc_return_value_and_one_out_parameter (MarshallObject *self, glong *a);

_GI_TEST_EXTERN
glong marshall_object_vfunc_return_value_and_multiple_out_parameters (MarshallObject *self, glong *a, glong *b);

_GI_TEST_EXTERN
gboolean marshall_object_vfunc_meth_with_error (MarshallObject *object, gint x, GError **error);


_GI_TEST_EXTERN
MarshallEnum marshall_object_vfunc_return_enum (MarshallObject *self);

_GI_TEST_EXTERN
void marshall_object_vfunc_out_enum (MarshallObject *self, MarshallEnum *_enum);


_GI_TEST_EXTERN
void marshall_object_get_ref_info_for_vfunc_return_object_transfer_none (MarshallObject *self, guint *ref_count, gboolean *is_floating);

_GI_TEST_EXTERN
void marshall_object_get_ref_info_for_vfunc_return_object_transfer_full (MarshallObject *self, guint *ref_count, gboolean *is_floating);

_GI_TEST_EXTERN
void marshall_object_get_ref_info_for_vfunc_out_object_transfer_none (MarshallObject *self, guint *ref_count, gboolean *is_floating);

_GI_TEST_EXTERN
void marshall_object_get_ref_info_for_vfunc_out_object_transfer_full (MarshallObject *self, guint *ref_count, gboolean *is_floating);

_GI_TEST_EXTERN
void marshall_object_get_ref_info_for_vfunc_in_object_transfer_none (MarshallObject *self, GType type, guint *ref_count, gboolean *is_floating);

_GI_TEST_EXTERN
void marshall_object_get_ref_info_for_vfunc_in_object_transfer_full (MarshallObject *self, GType type, guint *ref_count, gboolean *is_floating);


_GI_TEST_EXTERN
MarshallObject *marshall_object_none_return (void);

_GI_TEST_EXTERN
MarshallObject *marshall_object_full_return (void);


_GI_TEST_EXTERN
void marshall_object_none_in (MarshallObject *object);

_GI_TEST_EXTERN
void marshall_object_full_in (MarshallObject *object);


_GI_TEST_EXTERN
void marshall_object_none_out (MarshallObject **object);

_GI_TEST_EXTERN
void marshall_object_full_out (MarshallObject **object);


_GI_TEST_EXTERN
void marshall_object_none_inout (MarshallObject **object);

_GI_TEST_EXTERN
void marshall_object_full_inout (MarshallObject **object);


_GI_TEST_EXTERN
void marshall_object_int8_in (MarshallObject *object, gint8 in);

_GI_TEST_EXTERN
void marshall_object_int8_out (MarshallObject *object, gint8 *out);


_GI_TEST_EXTERN
void marshall_object_vfunc_with_callback (MarshallObject *object,
                                                      MarshallCallbackIntInt callback,
                                                      void *callback_data);

_GI_TEST_EXTERN
void marshall_object_call_vfunc_with_callback (MarshallObject *object);

#define MARSHALL_TYPE_SUB_OBJECT             (marshall_sub_object_get_type ())
#define MARSHALL_SUB_OBJECT(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), MARSHALL_TYPE_SUB_OBJECT, MarshallSubObject))
#define MARSHALL_SUB_OBJECT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), MARSHALL_TYPE_SUB_OBJECT, MarshallSubObjectClass))
#define MARSHALL_IS_SUB_OBJECT(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MARSHALL_TYPE_SUB_OBJECT))
#define MARSHALL_IS_SUB_OBJECT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), MARSHALL_TYPE_SUB_OBJECT))
#define MARSHALL_SUB_OBJECT_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), MARSHALL_TYPE_SUB_OBJECT, MarshallSubObjectClass))

typedef struct _MarshallSubObjectClass MarshallSubObjectClass;
typedef struct _MarshallSubObject MarshallSubObject;

struct _MarshallSubObjectClass
{
	MarshallObjectClass parent_class;
};

struct _MarshallSubObject
{
	MarshallObject parent_instance;
};


_GI_TEST_EXTERN
GType marshall_sub_object_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
void marshall_sub_object_sub_method (MarshallSubObject *object);

_GI_TEST_EXTERN
void marshall_sub_object_overwritten_method (MarshallSubObject *object);

#define MARSHALL_TYPE_SUB_SUB_OBJECT             (marshall_sub_sub_object_get_type ())
#define MARSHALL_SUB_SUB_OBJECT(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), MARSHALL_TYPE_SUB_SUB_OBJECT, MarshallSubSubObject))
#define MARSHALL_SUB_SUB_OBJECT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), MARSHALL_TYPE_SUB_SUB_OBJECT, MarshallSubSubObjectClass))
#define MARSHALL_IS_SUB_SUB_OBJECT(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MARSHALL_TYPE_SUB_SUB_OBJECT))
#define MARSHALL_IS_SUB_SUB_OBJECT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), MARSHALL_TYPE_SUB_SUB_OBJECT))
#define MARSHALL_SUB_SUB_OBJECT_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), MARSHALL_TYPE_SUB_SUB_OBJECT, MarshallSubSubObjectClass))

typedef struct _MarshallSubSubObjectClass MarshallSubSubObjectClass;
typedef struct _MarshallSubSubObject MarshallSubSubObject;

struct _MarshallSubSubObjectClass
{
	MarshallSubObjectClass parent_class;
};

struct _MarshallSubSubObject
{
	MarshallSubObject parent_instance;
};


_GI_TEST_EXTERN
GType marshall_sub_sub_object_get_type (void) G_GNUC_CONST;

/* Interfaces */

#define MARSHALL_TYPE_INTERFACE              (marshall_interface_get_type ())
#define MARSHALL_INTERFACE(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), MARSHALL_TYPE_INTERFACE, MarshallInterface))
#define MARSHALL_IS_INTERFACE(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), MARSHALL_TYPE_INTERFACE))
#define MARSHALL_INTERFACE_GET_IFACE(obj)    (G_TYPE_INSTANCE_GET_INTERFACE ((obj), MARSHALL_TYPE_INTERFACE, MarshallInterfaceIface))

typedef struct _MarshallInterface MarshallInterface;
typedef struct _MarshallInterfaceIface MarshallInterfaceIface;

struct _MarshallInterfaceIface {
    GTypeInterface base_iface;

    /**
     * MarshallInterfaceIface::test_int8_in:
     * @in: (in):
     */
    void (* test_int8_in) (MarshallInterface *self, gint8 in);
};


_GI_TEST_EXTERN
GType marshall_interface_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
void marshall_interface_test_int8_in (MarshallInterface *self, gint8 in);


_GI_TEST_EXTERN
void marshall_test_interface_test_int8_in (MarshallInterface *test_iface, gint8 in);

/* MarshallInterfaceImpl is a class that implements
   MarshallInterface */

#define MARSHALL_TYPE_INTERFACE_IMPL     (marshall_interface_impl_get_type ())
#define MARSHALL_INTERFACE_IMPL(obj)     (G_TYPE_CHECK_INSTANCE_CAST ((obj), MARSHALL_TYPE_INTERFACE_IMPL, MarshallInterfaceImpl))
#define MARSHALL_INTERFACE_IMPL_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), MARSHALL_TYPE_INTERFACE_IMPL, MarshallInterfaceImplClass))
#define MARSHALL_IS_INTERFACE_IMPL(obj)  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MARSHALL_TYPE_INTERFACE_IMPL))
#define MARSHALL_IS_INTERFACE_IMPL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), MARSHALL_TYPE_INTERFACE_IMPL))
#define MARSHALL_INTERFACE_IMPL_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), MARSHALL_TYPE_INTERFACE_IMPL, MarshallInterfaceImplClass))


typedef struct _MarshallInterfaceImplClass MarshallInterfaceImplClass;
typedef struct _MarshallInterfaceImpl MarshallInterfaceImpl;

struct _MarshallInterfaceImplClass
{
    GObjectClass parent_class;
};

struct _MarshallInterfaceImpl
{
    GObject parent_instance;

    gint int_;
};


_GI_TEST_EXTERN
GType marshall_interface_impl_get_type (void) G_GNUC_CONST;

_GI_TEST_EXTERN
MarshallInterface *marshall_interface_impl_get_as_interface (MarshallInterfaceImpl *self);

/* MarshallInterface2 allows us testing vfunc clashes when a class'
   vfunc implementation ambiguously relates to its prototype */

#define MARSHALL_TYPE_INTERFACE2              (marshall_interface2_get_type ())
#define MARSHALL_INTERFACE2(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), MARSHALL_TYPE_INTERFACE2, MarshallInterface2))
#define MARSHALL_IS_INTERFACE2(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), MARSHALL_TYPE_INTERFACE2))
#define MARSHALL_INTERFACE2_GET_IFACE(obj)    (G_TYPE_INSTANCE_GET_INTERFACE ((obj), MARSHALL_TYPE_INTERFACE2, MarshallInterface2Iface))

typedef struct _MarshallInterface2 MarshallInterface2;
typedef struct _MarshallInterface2Iface MarshallInterface2Iface;

struct _MarshallInterface2Iface {
    GTypeInterface base_iface;

    /**
     * MarshallInterface2Iface::test_int8_in:
     * @in: (in):
     */
    void (* test_int8_in) (MarshallInterface2 *self, gint8 in);
};


_GI_TEST_EXTERN
GType marshall_interface2_get_type (void) G_GNUC_CONST;

/* MarshallInterface3 tests passing arrays of variants from C to @lang */

#define MARSHALL_TYPE_INTERFACE3              (marshall_interface3_get_type ())
#define MARSHALL_INTERFACE3(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), MARSHALL_TYPE_INTERFACE3, MarshallInterface3))
#define MARSHALL_IS_INTERFACE3(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), MARSHALL_TYPE_INTERFACE3))
#define MARSHALL_INTERFACE3_GET_IFACE(obj)    (G_TYPE_INSTANCE_GET_INTERFACE ((obj), MARSHALL_TYPE_INTERFACE3, MarshallInterface3Iface))

typedef struct _MarshallInterface3 MarshallInterface3;
typedef struct _MarshallInterface3Iface MarshallInterface3Iface;

struct _MarshallInterface3Iface {
    GTypeInterface base_iface;

    /**
     * MarshallInterface3::test_variant_array_in:
     * @in: (in) (array length=n_in):
     */
    void (* test_variant_array_in) (MarshallInterface3 *self, GVariant **in, gsize n_in);
};


_GI_TEST_EXTERN
GType marshall_interface3_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
void marshall_interface3_test_variant_array_in (MarshallInterface3 *self, GVariant **in, gsize n_in);

/* Multiple output arguments */


_GI_TEST_EXTERN
void marshall_int_out_out (gint *int0, gint *int1);

_GI_TEST_EXTERN
void marshall_int_three_in_three_out(gint a, gint b, gint c,
                                                 gint *out0, gint *out1, gint *out2);

_GI_TEST_EXTERN
gint marshall_int_return_out (gint *int_);

/* Default arguments */
_GI_TEST_EXTERN
void marshall_int_two_in_utf8_two_in_with_allow_none  (gint a, gint b, const gchar *c, const gchar *d);

_GI_TEST_EXTERN
void marshall_int_one_in_utf8_two_in_one_allows_none  (gint a, const gchar *b, const gchar *c);

_GI_TEST_EXTERN
void marshall_array_in_utf8_two_in (const gint *ints, gint length, const gchar *a, const gchar *b);

_GI_TEST_EXTERN
void marshall_array_in_utf8_two_in_out_of_order (gint length, const gchar *a, const gint *ints, const gchar *b);

/* GError */

#define MARSHALL_CONSTANT_GERROR_DOMAIN "gi-marshalling-tests-gerror-domain"
#define MARSHALL_CONSTANT_GERROR_CODE 5
#define MARSHALL_CONSTANT_GERROR_MESSAGE "gi-marshalling-tests-gerror-message"
#define MARSHALL_CONSTANT_GERROR_DEBUG_MESSAGE "we got an error, life is shit"


_GI_TEST_EXTERN
void marshall_gerror(GError **error);

_GI_TEST_EXTERN
void marshall_gerror_array_in(gint *in_ints, GError **error);

_GI_TEST_EXTERN
void marshall_gerror_out(GError **error, gchar **debug);

_GI_TEST_EXTERN
void marshall_gerror_out_transfer_none(GError **err, const gchar **debug);

_GI_TEST_EXTERN
GError *marshall_gerror_return(void);

/* GParamSpec */
_GI_TEST_EXTERN
void marshall_param_spec_in_bool(const GParamSpec *param);

_GI_TEST_EXTERN
GParamSpec *marshall_param_spec_return (void);

_GI_TEST_EXTERN
void marshall_param_spec_out(GParamSpec **param);

/* Overrides */

#define MARSHALL_OVERRIDES_CONSTANT 42


typedef struct {
    glong long_;
} MarshallOverridesStruct;

_GI_TEST_EXTERN
GType marshall_overrides_struct_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
MarshallOverridesStruct *marshall_overrides_struct_new (void);


_GI_TEST_EXTERN
glong marshall_overrides_struct_method (MarshallOverridesStruct *struct_);


_GI_TEST_EXTERN
MarshallOverridesStruct *marshall_overrides_struct_returnv (void);


#define MARSHALL_TYPE_OVERRIDES_OBJECT             (marshall_overrides_object_get_type ())
#define MARSHALL_OVERRIDES_OBJECT(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), MARSHALL_TYPE_OVERRIDES_OBJECT, MarshallOverridesObject))
#define MARSHALL_OVERRIDES_OBJECT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), MARSHALL_TYPE_OVERRIDES_OBJECT, MarshallOverridesObjectClass))
#define MARSHALL_IS_OVERRIDES_OBJECT(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MARSHALL_TYPE_OVERRIDES_OBJECT))
#define MARSHALL_IS_OVERRIDES_OBJECT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), MARSHALL_TYPE_OVERRIDES_OBJECT))
#define MARSHALL_OVERRIDES_OBJECT_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), MARSHALL_TYPE_OVERRIDES_OBJECT, MarshallOverridesObjectClass))

typedef struct _MarshallOverridesObjectClass MarshallOverridesObjectClass;
typedef struct _MarshallOverridesObject MarshallOverridesObject;

struct _MarshallOverridesObjectClass
{
    GObjectClass parent_class;
};

struct _MarshallOverridesObject
{
    GObject parent_instance;

    glong long_;
};

_GI_TEST_EXTERN
GType marshall_overrides_object_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
MarshallOverridesObject *marshall_overrides_object_new (void);


_GI_TEST_EXTERN
glong marshall_overrides_object_method (MarshallOverridesObject *object);


_GI_TEST_EXTERN
MarshallOverridesObject *marshall_overrides_object_returnv (void);

/* Properties Object */

#define MARSHALL_TYPE_PROPERTIES_OBJECT (marshall_properties_object_get_type ())
#define MARSHALL_PROPERTIES_OBJECT(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), MARSHALL_TYPE_PROPERTIES_OBJECT, MarshallPropertiesObject))
#define MARSHALL_PROPERTIES_OBJECT_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), MARSHALL_TYPE_PROPERTIES_OBJECT, MarshallPropertiesObjectClass))
#define MARSHALL_IS_PROPERTIES_OBJECT(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MARSHALL_TYPE_PROPERTIES_OBJECT))
#define MARSHALL_IS_PROPERTIES_OBJECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), MARSHALL_TYPE_PROPERTIES_OBJECT))
#define MARSHALL_PROPERTIES_OBJECT_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), MARSHALL_TYPE_PROPERTIES_OBJECT, MarshallPropertiesObjectClass))

typedef struct _MarshallPropertiesObject MarshallPropertiesObject;
typedef struct _MarshallPropertiesObjectClass MarshallPropertiesObjectClass;

struct _MarshallPropertiesObject {
    GObject parent_instance;

    gboolean some_boolean;
    gchar some_char;
    guchar some_uchar;
    gint some_int;
    guint some_uint;
    glong some_long;
    gulong some_ulong;
    gint64 some_int64;
    guint64 some_uint64;
    gfloat some_float;
    gdouble some_double;
    gchar **some_strv;
    MarshallBoxedStruct* some_boxed_struct;
    GList* some_boxed_glist;
    GValue *some_gvalue;
    GVariant *some_variant;
    GObject *some_object;
    MarshallFlags some_flags;
    MarshallGEnum some_enum;
    GByteArray *some_byte_array;
};

struct _MarshallPropertiesObjectClass {
    GObjectClass parent_class;
};


_GI_TEST_EXTERN
GType marshall_properties_object_get_type (void) G_GNUC_CONST;


_GI_TEST_EXTERN
MarshallPropertiesObject *marshall_properties_object_new (void);

/**
 * MarshallA21:
 * @a:
 * @b:
 * @c:
 * @d:
 * @e:
 * @f:
 * @g:
 * @i:
 * @j:
 * @k:
 * @l:
 * @m:
 * @n:
 * @o:
 * @p:
 * @q:
 * @r:
 * @s:
 * @t:
 * @u:
 */
typedef void (*MarshallA21)(gint a, gint b, gint c, gint d, gint e, gint f, gint g, gint h,
                            gint i, gint j, gint k, gint l, gint m, gint n, gint o, gint p,
                            gint q, gint r, gint s, gint t, gint u);

_GI_TEST_EXTERN
void
marshall_a21(gint a, gint b, gint c, gint d, gint e, gint f, gint g, gint h, gint i, gint j,
             gint k, gint l, gint m, gint n, gint o, gint p, gint q, gint r, gint s, gint t,
             gint u);


/**
 * marshall_callback_a21:
 * @a21: (scope call):
 */
_GI_TEST_EXTERN
void
marshall_callback_a21(MarshallA21 a21);


#endif /* _MARSHALL_H_ */
