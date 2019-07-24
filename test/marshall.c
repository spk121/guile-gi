#include "marshall.h"

#include <string.h>

static void marshall_boxed_struct_free (MarshallBoxedStruct *v);

/* Booleans */

gboolean
marshall_boolean_return_true (void)
{
  return TRUE;
}

gboolean
marshall_boolean_return_false (void)
{
  return FALSE;
}

void
marshall_boolean_in_true (gboolean v)
{
  g_assert (v == TRUE);
}

void
marshall_boolean_in_false (gboolean v)
{
  g_assert (v == FALSE);
}

/**
 * marshall_boolean_out_true:
 * @v: (out):
 */
void
marshall_boolean_out_true (gboolean *v)
{
  *v = TRUE;
}

/**
 * marshall_boolean_out_false:
 * @v: (out):
 */
void
marshall_boolean_out_false (gboolean *v)
{
  *v = FALSE;
}

/**
 * marshall_boolean_inout_true_false:
 * @v: (inout):
 */
void
marshall_boolean_inout_true_false (gboolean *v)
{
  g_assert (*v == TRUE);
  *v = FALSE;
}

/**
 * marshall_boolean_inout_false_true:
 * @v: (inout):
 */
void
marshall_boolean_inout_false_true (gboolean *v)
{
  g_assert (*v == FALSE);
  *v = TRUE;
}


/* Integers */

gint8
marshall_int8_return_max (void)
{
  return G_MAXINT8;
}

gint8
marshall_int8_return_min (void)
{
  return G_MININT8;
}

void
marshall_int8_in_max (gint8 v)
{
  g_assert_cmpint (v, ==, G_MAXINT8);
}

void
marshall_int8_in_min (gint8 v)
{
  g_assert_cmpint (v, ==, G_MININT8);
}

/**
 * marshall_int8_out_max:
 * @v: (out):
 */
void
marshall_int8_out_max (gint8 *v)
{
  *v = G_MAXINT8;
}

/**
 * marshall_int8_out_min:
 * @v: (out):
 */
void
marshall_int8_out_min (gint8 *v)
{
  *v = G_MININT8;
}

/**
 * marshall_int8_inout_max_min:
 * @v: (inout):
 */
void
marshall_int8_inout_max_min (gint8 *v)
{
  g_assert_cmpint (*v, ==, G_MAXINT8);
  *v = G_MININT8;
}

/**
 * marshall_int8_inout_min_max:
 * @v: (inout):
 */
void
marshall_int8_inout_min_max (gint8 *v)
{
  g_assert_cmpint (*v, ==, G_MININT8);
  *v = G_MAXINT8;
}


guint8
marshall_uint8_return (void)
{
  return G_MAXUINT8;
}

void
marshall_uint8_in (guint8 v)
{
  g_assert_cmpuint (v, ==, G_MAXUINT8);
}

/**
 * marshall_uint8_out:
 * @v: (out):
 */
void
marshall_uint8_out (guint8 *v)
{
  *v = G_MAXUINT8;
}

/**
 * marshall_uint8_inout:
 * @v: (inout):
 */
void
marshall_uint8_inout (guint8 *v)
{
  g_assert_cmpuint (*v, ==, G_MAXUINT8);
  *v = 0;
}


gint16
marshall_int16_return_max (void)
{
  return G_MAXINT16;
}

gint16
marshall_int16_return_min (void)
{
  return G_MININT16;
}

void
marshall_int16_in_max (gint16 v)
{
  g_assert_cmpint (v, ==, G_MAXINT16);
}

void
marshall_int16_in_min (gint16 v)
{
  g_assert_cmpint (v, ==, G_MININT16);
}

/**
 * marshall_int16_out_max:
 * @v: (out):
 */
void
marshall_int16_out_max (gint16 *v)
{
  *v = G_MAXINT16;
}

/**
 * marshall_int16_out_min:
 * @v: (out):
 */
void
marshall_int16_out_min (gint16 *v)
{
  *v = G_MININT16;
}

/**
 * marshall_int16_inout_max_min:
 * @v: (inout):
 */
void
marshall_int16_inout_max_min (gint16 *v)
{
  g_assert_cmpint (*v, ==, G_MAXINT16);
  *v = G_MININT16;
}

/**
 * marshall_int16_inout_min_max:
 * @v: (inout):
 */
void
marshall_int16_inout_min_max (gint16 *v)
{
  g_assert_cmpint (*v, ==, G_MININT16);
  *v = G_MAXINT16;
}


guint16
marshall_uint16_return (void)
{
  return G_MAXUINT16;
}

void
marshall_uint16_in (guint16 v)
{
  g_assert_cmpuint (v, ==, G_MAXUINT16);
}

/**
 * marshall_uint16_out:
 * @v: (out):
 */
void
marshall_uint16_out (guint16 *v)
{
  *v = G_MAXUINT16;
}

/**
 * marshall_uint16_inout:
 * @v: (inout):
 */
void
marshall_uint16_inout (guint16 *v)
{
  g_assert_cmpuint (*v, ==, G_MAXUINT16);
  *v = 0;
}


gint32
marshall_int32_return_max (void)
{
  return G_MAXINT32;
}

gint32
marshall_int32_return_min (void)
{
  return G_MININT32;
}

void
marshall_int32_in_max (gint32 v)
{
  g_assert_cmpint (v, ==, G_MAXINT32);
}

void
marshall_int32_in_min (gint32 v)
{
  g_assert_cmpint (v, ==, G_MININT32);
}

/**
 * marshall_int32_out_max:
 * @v: (out):
 */
void
marshall_int32_out_max (gint32 *v)
{
  *v = G_MAXINT32;
}

/**
 * marshall_int32_out_min:
 * @v: (out):
 */
void
marshall_int32_out_min (gint32 *v)
{
  *v = G_MININT32;
}

/**
 * marshall_int32_inout_max_min:
 * @v: (inout):
 */
void
marshall_int32_inout_max_min (gint32 *v)
{
  g_assert_cmpint (*v, ==, G_MAXINT32);
  *v = G_MININT32;
}

/**
 * marshall_int32_inout_min_max:
 * @v: (inout):
 */
void
marshall_int32_inout_min_max (gint32 *v)
{
  g_assert_cmpint (*v, ==, G_MININT32);
  *v = G_MAXINT32;
}


guint32
marshall_uint32_return (void)
{
  return G_MAXUINT32;
}

void
marshall_uint32_in (guint32 v)
{
  g_assert_cmpuint (v, ==, G_MAXUINT32);
}

/**
 * marshall_uint32_out:
 * @v: (out):
 */
void
marshall_uint32_out (guint32 *v)
{
  *v = G_MAXUINT32;
}

/**
 * marshall_uint32_inout:
 * @v: (inout):
 */
void
marshall_uint32_inout (guint32 *v)
{
  g_assert_cmpuint (*v, ==, G_MAXUINT32);
  *v = 0;
}


gint64
marshall_int64_return_max (void)
{
  return G_MAXINT64;
}

gint64
marshall_int64_return_min (void)
{
  return G_MININT64;
}

void
marshall_int64_in_max (gint64 v)
{
  g_assert_cmpint (v, ==, G_MAXINT64);
}

void
marshall_int64_in_min (gint64 v)
{
  g_assert_cmpint (v, ==, G_MININT64);
}

/**
 * marshall_int64_out_max:
 * @v: (out):
 */
void
marshall_int64_out_max (gint64 *v)
{
  *v = G_MAXINT64;
}

/**
 * marshall_int64_out_min:
 * @v: (out):
 */
void
marshall_int64_out_min (gint64 *v)
{
  *v = G_MININT64;
}

/**
 * marshall_int64_inout_max_min:
 * @v: (inout):
 */
void
marshall_int64_inout_max_min (gint64 *v)
{
  g_assert_cmpint (*v, ==, G_MAXINT64);
  *v = G_MININT64;
}

/**
 * marshall_int64_inout_min_max:
 * @v: (inout):
 */
void
marshall_int64_inout_min_max (gint64 *v)
{
  g_assert_cmpint (*v, ==, G_MININT64);
  *v = G_MAXINT64;
}


guint64
marshall_uint64_return (void)
{
  return G_MAXUINT64;
}

void
marshall_uint64_in (guint64 v)
{
  g_assert_cmpuint (v, ==, G_MAXUINT64);
}

/**
 * marshall_uint64_out:
 * @v: (out):
 */
void
marshall_uint64_out (guint64 *v)
{
  *v = G_MAXUINT64;
}

/**
 * marshall_uint64_inout:
 * @v: (inout):
 */
void
marshall_uint64_inout (guint64 *v)
{
  g_assert_cmpuint (*v, ==, G_MAXUINT64);
  *v = 0;
}


gshort
marshall_short_return_max (void)
{
  return G_MAXSHORT;
}

gshort
marshall_short_return_min (void)
{
  return G_MINSHORT;
}

void
marshall_short_in_max (gshort short_)
{
  g_assert_cmpint (short_, ==, G_MAXSHORT);
}

void
marshall_short_in_min (gshort short_)
{
  g_assert_cmpint (short_, ==, G_MINSHORT);
}

/**
 * marshall_short_out_max:
 * @short_: (out):
 */
void
marshall_short_out_max (gshort *short_)
{
  *short_ = G_MAXSHORT;
}

/**
 * marshall_short_out_min:
 * @short_: (out):
 */
void
marshall_short_out_min (gshort *short_)
{
  *short_ = G_MINSHORT;
}

/**
 * marshall_short_inout_max_min:
 * @short_: (inout):
 */
void
marshall_short_inout_max_min (gshort *short_)
{
  g_assert_cmpint (*short_, ==, G_MAXSHORT);
  *short_ = G_MINSHORT;
}

/**
 * marshall_short_inout_min_max:
 * @short_: (inout):
 */
void
marshall_short_inout_min_max (gshort *short_)
{
  g_assert_cmpint (*short_, ==, G_MINSHORT);
  *short_ = G_MAXSHORT;
}


gushort
marshall_ushort_return (void)
{
  return G_MAXUSHORT;
}

void
marshall_ushort_in (gushort ushort_)
{
  g_assert_cmpuint (ushort_, ==, G_MAXUSHORT);
}

/**
 * marshall_ushort_out:
 * @ushort_: (out):
 */
void
marshall_ushort_out (gushort *ushort_)
{
  *ushort_ = G_MAXUSHORT;
}

/**
 * marshall_ushort_inout:
 * @ushort_: (inout):
 */
void
marshall_ushort_inout (gushort *ushort_)
{
  g_assert_cmpuint (*ushort_, ==, G_MAXUSHORT);
  *ushort_ = 0;
}


gint
marshall_int_return_max (void)
{
  return G_MAXINT;
}

gint
marshall_int_return_min (void)
{
  return G_MININT;
}

void
marshall_int_in_max (gint int_)
{
  g_assert_cmpint (int_, ==, G_MAXINT);
}

void
marshall_int_in_min (gint int_)
{
  g_assert_cmpint (int_, ==, G_MININT);
}

/**
 * marshall_int_out_max:
 * @int_: (out):
 */
void
marshall_int_out_max (gint *int_)
{
  *int_ = G_MAXINT;
}

/**
 * marshall_int_out_min:
 * @int_: (out):
 */
void
marshall_int_out_min (gint *int_)
{
  *int_ = G_MININT;
}

/**
 * marshall_int_inout_max_min:
 * @int_: (inout):
 */
void
marshall_int_inout_max_min (gint *int_)
{
  g_assert_cmpint (*int_, ==, G_MAXINT);
  *int_ = G_MININT;
}

/**
 * marshall_int_inout_min_max:
 * @int_: (inout):
 */
void
marshall_int_inout_min_max (gint *int_)
{
  g_assert_cmpint (*int_, ==, G_MININT);
  *int_ = G_MAXINT;
}


guint
marshall_uint_return (void)
{
  return G_MAXUINT;
}

void
marshall_uint_in (guint uint_)
{
  g_assert_cmpuint (uint_, ==, G_MAXUINT);
}

/**
 * marshall_uint_out:
 * @uint_: (out):
 */
void
marshall_uint_out (guint *uint_)
{
  *uint_ = G_MAXUINT;
}

/**
 * marshall_uint_inout:
 * @uint_: (inout):
 */
void
marshall_uint_inout (guint *uint_)
{
  g_assert_cmpuint (*uint_, ==, G_MAXUINT);
  *uint_ = 0;
}


glong
marshall_long_return_max (void)
{
  return G_MAXLONG;
}

glong
marshall_long_return_min (void)
{
  return G_MINLONG;
}

void
marshall_long_in_max (glong long_)
{
  g_assert_cmpint (long_, ==, G_MAXLONG);
}

void
marshall_long_in_min (glong long_)
{
  g_assert_cmpint (long_, ==, G_MINLONG);
}

/**
 * marshall_long_out_max:
 * @long_: (out):
 */
void
marshall_long_out_max (glong *long_)
{
  *long_ = G_MAXLONG;
}

/**
 * marshall_long_out_min:
 * @long_: (out):
 */
void
marshall_long_out_min (glong *long_)
{
  *long_ = G_MINLONG;
}

/**
 * marshall_long_inout_max_min:
 * @long_: (inout):
 */
void
marshall_long_inout_max_min (glong *long_)
{
  g_assert_cmpint (*long_, ==, G_MAXLONG);
  *long_ = G_MINLONG;
}

/**
 * marshall_long_inout_min_max:
 * @long_: (inout):
 */
void
marshall_long_inout_min_max (glong *long_)
{
  g_assert_cmpint (*long_, ==, G_MINLONG);
  *long_ = G_MAXLONG;
}


gulong
marshall_ulong_return (void)
{
  return G_MAXULONG;
}

void
marshall_ulong_in (gulong ulong_)
{
  g_assert_cmpuint (ulong_, ==, G_MAXULONG);
}

/**
 * marshall_ulong_out:
 * @ulong_: (out):
 */
void
marshall_ulong_out (gulong *ulong_)
{
  *ulong_ = G_MAXULONG;
}

/**
 * marshall_ulong_inout:
 * @ulong_: (inout):
 */
void
marshall_ulong_inout (gulong *ulong_)
{
  g_assert_cmpuint (*ulong_, ==, G_MAXULONG);
  *ulong_ = 0;
}


gssize
marshall_ssize_return_max (void)
{
  return G_MAXSSIZE;
}

gssize
marshall_ssize_return_min (void)
{
  return G_MINSSIZE;
}

void
marshall_ssize_in_max (gssize ssize)
{
  g_assert_cmpint (ssize, ==, G_MAXSSIZE);
}

void
marshall_ssize_in_min (gssize ssize)
{
  g_assert_cmpint (ssize, ==, G_MINSSIZE);
}

/**
 * marshall_ssize_out_max:
 * @ssize: (out):
 */
void
marshall_ssize_out_max (gssize *ssize)
{
  *ssize = G_MAXSSIZE;
}

/**
 * marshall_ssize_out_min:
 * @ssize: (out):
 */
void
marshall_ssize_out_min (gssize *ssize)
{
  *ssize = G_MINSSIZE;
}

/**
 * marshall_ssize_inout_max_min:
 * @ssize: (inout):
 */
void
marshall_ssize_inout_max_min (gssize *ssize)
{
  g_assert_cmpint (*ssize, ==, G_MAXSSIZE);
  *ssize = G_MINSSIZE;
}

/**
 * marshall_ssize_inout_min_max:
 * @ssize: (inout):
 */
void
marshall_ssize_inout_min_max (gssize *ssize)
{
  g_assert_cmpint (*ssize, ==, G_MINSSIZE);
  *ssize = G_MAXSSIZE;
}


gsize
marshall_size_return (void)
{
  return G_MAXSIZE;
}

void
marshall_size_in (gsize size)
{
  g_assert_cmpuint (size, ==, G_MAXSIZE);
}

/**
 * marshall_size_out:
 * @size: (out):
 */
void
marshall_size_out (gsize *size)
{
  *size = G_MAXSIZE;
}

/**
 * marshall_size_inout:
 * @size: (inout):
 */
void
marshall_size_inout (gsize *size)
{
  g_assert_cmpuint (*size, ==, G_MAXSIZE);
  *size = 0;
}


gfloat
marshall_float_return (void)
{
  return G_MAXFLOAT;
}

void
marshall_float_in (gfloat v)
{
  g_assert_cmpfloat (v, ==, G_MAXFLOAT);
}

/**
 * marshall_float_out:
 * @v: (out):
 */
void
marshall_float_out (gfloat *v)
{
  *v = G_MAXFLOAT;
}

/**
 * marshall_float_inout:
 * @v: (inout):
 */
void
marshall_float_inout (gfloat *v)
{
  g_assert_cmpfloat (*v, ==, G_MAXFLOAT);
  *v = G_MINFLOAT;
}


gdouble
marshall_double_return (void)
{
  return G_MAXDOUBLE;
}

void
marshall_double_in (gdouble v)
{
  g_assert_cmpfloat (v, ==, G_MAXDOUBLE);
}

/**
 * marshall_double_out:
 * @v: (out):
 */
void
marshall_double_out (gdouble *v)
{
  *v = G_MAXDOUBLE;
}

/**
 * marshall_double_inout:
 * @v: (inout):
 */
void
marshall_double_inout (gdouble *v)
{
  g_assert_cmpfloat (*v, ==, G_MAXDOUBLE);
  *v = G_MINDOUBLE;
}


time_t
marshall_time_t_return (void)
{
  return 1234567890;
}

void
marshall_time_t_in (time_t v)
{
  g_assert_cmpuint (v, ==, 1234567890);
}

/**
 * marshall_time_t_out:
 * @v: (out):
 */
void
marshall_time_t_out (time_t *v)
{
  *v = 1234567890;
}

/**
 * marshall_time_t_inout:
 * @v: (inout):
 */
void
marshall_time_t_inout (time_t *v)
{
  g_assert_cmpuint (*v, ==, 1234567890);
  *v = 0;
}


GType
marshall_gtype_return (void)
{
  return G_TYPE_NONE;
}

GType
marshall_gtype_string_return (void)
{
  return G_TYPE_STRING;
}

void
marshall_gtype_in (GType gtype)
{
  g_assert (gtype == G_TYPE_NONE);
}

void
marshall_gtype_string_in (GType gtype)
{
  g_assert (gtype == G_TYPE_STRING);
}


/**
 * marshall_gtype_out:
 * @gtype: (out):
 */
void
marshall_gtype_out (GType *gtype)
{
  *gtype = G_TYPE_NONE;
}

/**
 * marshall_gtype_string_out:
 * @gtype: (out):
 */
void
marshall_gtype_string_out (GType *gtype)
{
  *gtype = G_TYPE_STRING;
}

/**
 * marshall_gtype_inout:
 * @gtype: (inout):
 */
void
marshall_gtype_inout (GType *gtype)
{
  g_assert (*gtype == G_TYPE_NONE);
  *gtype = G_TYPE_INT;
}


const gchar *
marshall_utf8_none_return (void)
{
  return MARSHALL_CONSTANT_UTF8;
}

gchar *
marshall_utf8_full_return (void)
{
  return g_strdup (MARSHALL_CONSTANT_UTF8);
}

void
marshall_utf8_none_in (const gchar *utf8)
{
  g_assert_cmpstr (MARSHALL_CONSTANT_UTF8, ==, utf8);
}

/**
 * marshall_utf8_as_uint8array_in:
 * @array: (array length=len) (element-type guint8): Byte data that happens to be UTF-8
 * @len: Length
 *
 * Takes data that happens to be UTF-8 as a byte array, to test
 * binding conversion from their string type (e.g. JavaScript's
 * UTF-16) to UTF-8.
 */
void
marshall_utf8_as_uint8array_in (const guint8 *array, gsize len)
{
  gsize orig_len = strlen (MARSHALL_CONSTANT_UTF8);
  g_assert_cmpint (orig_len, ==, len);
  g_assert (memcmp (MARSHALL_CONSTANT_UTF8, array, len) == 0);
}

/**
 * marshall_utf8_none_out:
 * @utf8: (out) (transfer none):
 */
void
marshall_utf8_none_out (const gchar **utf8)
{
  *utf8 = MARSHALL_CONSTANT_UTF8;
}

/**
 * marshall_utf8_full_out:
 * @utf8: (out) (transfer full):
 */
void
marshall_utf8_full_out (gchar **utf8)
{
  *utf8 = g_strdup (MARSHALL_CONSTANT_UTF8);
}

/**
 * marshall_utf8_dangling_out:
 * @utf8: (out) (transfer full):
 */
void
marshall_utf8_dangling_out (gchar **utf8)
{
  /* Intentionally don't touch the pointer to see how
     the bindings handle this case.  Bindings should be
     robust against broken C functions and can initialize
     even OUT vlues to NULL.
   */
}

/**
 * marshall_utf8_none_inout:
 * @utf8: (inout) (transfer none):
 */
void
marshall_utf8_none_inout (const gchar **utf8)
{
  g_assert_cmpstr (MARSHALL_CONSTANT_UTF8, ==, *utf8);
  *utf8 = "";
}

/**
 * marshall_utf8_full_inout:
 * @utf8: (inout) (transfer full):
 */
void
marshall_utf8_full_inout (gchar **utf8)
{
  g_assert_cmpstr (MARSHALL_CONSTANT_UTF8, ==, *utf8);
  g_free (*utf8);
  *utf8 = g_strdup ("");
}


/**
 * marshall_init_function:
 * @n_args: (inout) (allow-none): number of args
 * @argv: (inout) (array length=n_args) (allow-none): args
 *
 * This is like gtk_init().
 */
gboolean
marshall_init_function (gint *n_args, char ***argv)
{
  if (n_args == NULL)
    return TRUE;

  if (*n_args == 0)
    return TRUE;
  (*n_args)--;
  g_assert (argv != NULL);
  /* we have transfer ownership full, so we need to free the element ourself */
  g_free ((*argv)[*n_args]);
  (*argv)[*n_args] = NULL;
  return TRUE;
}

/**
 * marshall_array_fixed_int_return:
 *
 * Returns: (array fixed-size=4):
 */
const gint *
marshall_array_fixed_int_return (void)
{
  static gint ints[] = { -1, 0, 1, 2 };
  return ints;
}

/**
 * marshall_array_fixed_short_return:
 *
 * Returns: (array fixed-size=4):
 */
const gshort *
marshall_array_fixed_short_return (void)
{
  static gshort shorts[] = { -1, 0, 1, 2 };
  return shorts;
}

/**
 * marshall_array_fixed_int_in:
 * @ints: (array fixed-size=4):
 */
void
marshall_array_fixed_int_in (const gint *ints)
{
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);
}

/**
 * marshall_array_fixed_short_in:
 * @shorts: (array fixed-size=4):
 */
void
marshall_array_fixed_short_in (const gshort *shorts)
{
  g_assert_cmpint (shorts[0], ==, -1);
  g_assert_cmpint (shorts[1], ==, 0);
  g_assert_cmpint (shorts[2], ==, 1);
  g_assert_cmpint (shorts[3], ==, 2);
}

/**
 * marshall_array_fixed_out:
 * @ints: (out) (array fixed-size=4) (transfer none):
 */
void
marshall_array_fixed_out (gint **ints)
{
  static gint values[] = { -1, 0, 1, 2 };
  *ints = values;
}

/**
 * marshall_array_fixed_out_struct:
 * @structs: (out) (array fixed-size=2) (transfer none):
 */
void
marshall_array_fixed_out_struct (MarshallSimpleStruct **structs)
{
  static MarshallSimpleStruct *values;

  if (values == NULL)
    {
      values = g_new (MarshallSimpleStruct, 2);

      values[0].long_ = 7;
      values[0].int8 = 6;

      values[1].long_ = 6;
      values[1].int8 = 7;
    }

  *structs = values;
}

/**
 * marshall_array_fixed_inout:
 * @ints: (inout) (array fixed-size=4) (transfer none):
 */
void
marshall_array_fixed_inout (gint **ints)
{
  static gint values[] = { 2, 1, 0, -1 };

  g_assert_cmpint ((*ints)[0], ==, -1);
  g_assert_cmpint ((*ints)[1], ==, 0);
  g_assert_cmpint ((*ints)[2], ==, 1);
  g_assert_cmpint ((*ints)[3], ==, 2);

  *ints = values;
}


/**
 * marshall_array_return:
 *
 * Returns: (array length=length):
 */
const gint *
marshall_array_return (gint *length)
{
  static gint ints[] = { -1, 0, 1, 2 };

  *length = 4;
  return ints;
}

/**
 * marshall_array_return_etc:
 * @first:
 * @length: (out):
 * @last:
 * @sum: (out):
 *
 * Returns: (array length=length):
 */
const gint *
marshall_array_return_etc (gint first, gint *length, gint last, gint *sum)
{
  static gint ints[] = { -1, 0, 1, 2 };

  ints[0] = first;
  ints[3] = last;
  *sum = first + last;
  *length = 4;
  return ints;
}

/**
 * marshall_array_in:
 * @ints: (array length=length):
 * @length:
 */
void
marshall_array_in (const gint *ints, gint length)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);
}

/**
 * marshall_array_in_len_before:
 * @length:
 * @ints: (array length=length):
 */
void
marshall_array_in_len_before (gint length, const gint *ints)
{
  marshall_array_in (ints, length);
}

/**
 * marshall_array_in_len_zero_terminated:
 * @ints: (array length=length zero-terminated):
 * @length:
 */
void
marshall_array_in_len_zero_terminated (const gint *ints, gint length)
{
  g_assert_cmpint (length, ==, 4);

  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);

  /* One past the end, null terminator */
  g_assert_cmpint (ints[4], ==, 0);
}

/**
 * marshall_array_string_in:
 * @strings: (array length=length):
 */
void
marshall_array_string_in (const gchar **strings, gint length)
{
  g_assert_cmpint (length, ==, 2);
  g_assert_cmpstr (strings[0], ==, "foo");
  g_assert_cmpstr (strings[1], ==, "bar");
}

/**
 * marshall_array_uint8_in:
 * @chars: (array length=length):
 */
void
marshall_array_uint8_in (const guint8 *chars, gint length)
{
  g_assert_cmpint (length, ==, 4);
  g_assert (chars[0] == 'a');
  g_assert (chars[1] == 'b');
  g_assert (chars[2] == 'c');
  g_assert (chars[3] == 'd');
}

/**
 * marshall_array_int64_in:
 * @ints: (array length=length):
 * @length:
 */
void
marshall_array_int64_in (const gint64 *ints, gint length)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);
}

/**
 * marshall_array_uint64_in:
 * @ints: (array length=length):
 * @length:
 */
void
marshall_array_uint64_in (const guint64 *ints, gint length)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);
}

/**
 * marshall_array_unichar_in:
 * @chars: (array length=length):
 * @length:
 */
void
marshall_array_unichar_in (const gunichar *chars, gint length)
{
  int ix;
  static const gunichar expected[] = MARSHALL_CONSTANT_UCS4;
  g_assert_cmpint (length, ==, 12);
  for (ix = 0; ix < length; ix++)
    g_assert_cmpuint (chars[ix], ==, expected[ix]);
}

/**
 * marshall_array_bool_in:
 * @bools: (array length=length):
 * @length:
 */
void
marshall_array_bool_in (const gboolean *bools, gint length)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (bools[0], ==, TRUE);
  g_assert_cmpint (bools[1], ==, FALSE);
  g_assert_cmpint (bools[2], ==, TRUE);
  g_assert_cmpint (bools[3], ==, TRUE);
}

/**
 * marshall_array_struct_in:
 * @structs: (array length=length):
 */
void
marshall_array_struct_in (MarshallBoxedStruct **structs, gint length)
{
  g_assert_cmpint (length, ==, 3);
  g_assert_cmpint (structs[0]->long_, ==, 1);
  g_assert_cmpint (structs[1]->long_, ==, 2);
  g_assert_cmpint (structs[2]->long_, ==, 3);
}

/**
 * marshall_array_struct_value_in:
 * @structs: (array length=length):
 */
void
marshall_array_struct_value_in (MarshallBoxedStruct *structs, gint length)
{
  g_assert_cmpint (length, ==, 3);
  g_assert_cmpint (structs[0].long_, ==, 1);
  g_assert_cmpint (structs[1].long_, ==, 2);
  g_assert_cmpint (structs[2].long_, ==, 3);
}

/**
 * marshall_array_simple_struct_in:
 * @structs: (array length=length):
 */
void
marshall_array_simple_struct_in (MarshallSimpleStruct *structs, gint length)
{
  g_assert_cmpint (length, ==, 3);
  g_assert_cmpint (structs[0].long_, ==, 1);
  g_assert_cmpint (structs[1].long_, ==, 2);
  g_assert_cmpint (structs[2].long_, ==, 3);
}

/**
 * marshall_multi_array_key_value_in:
 * @keys: (array length=length):
 * @values: (array length=length):
 */
void
marshall_multi_array_key_value_in (gint length, const gchar **keys, const GValue *values)
{
  g_assert_cmpint (length, ==, 3);
  g_assert_cmpstr ("one", ==, keys[0]);
  g_assert_cmpint (g_value_get_int (&values[0]), ==, 1);
  g_assert_cmpstr ("two", ==, keys[1]);
  g_assert_cmpint (g_value_get_int (&values[1]), ==, 2);
  g_assert_cmpstr ("three", ==, keys[2]);
  g_assert_cmpint (g_value_get_int (&values[2]), ==, 3);

}

/**
 * marshall_array_struct_take_in:
 * @structs: (array length=length) (transfer full):
 */
void
marshall_array_struct_take_in (MarshallBoxedStruct **structs, gint length)
{
  marshall_array_struct_in (structs, length);

  /* only really useful if run in valgrind actually */
  marshall_boxed_struct_free (structs[0]);
  marshall_boxed_struct_free (structs[1]);
  marshall_boxed_struct_free (structs[2]);
  g_free (structs);
}

/**
 * marshall_array_enum_in:
 * @_enum: (array length=length) (transfer none):
 * @length:
 */
void
marshall_array_enum_in (MarshallEnum *v, gint length)
{
  g_assert_cmpint (length, ==, 3);
  g_assert_cmpint (v[0], ==, MARSHALL_ENUM_VALUE1);
  g_assert_cmpint (v[1], ==, MARSHALL_ENUM_VALUE2);
  g_assert_cmpint (v[2], ==, MARSHALL_ENUM_VALUE3);
}

/**
 * marshall_array_in_guint64_len:
 * @ints: (array length=length) (transfer none):
 * @length:
 */
void
marshall_array_in_guint64_len (const gint *ints, guint64 length)
{
  g_assert_cmpint (length, ==, 4);

  marshall_array_in (ints, length);
}

/**
 * marshall_array_in_guint8_len:
 * @ints: (array length=length) (transfer none):
 * @length:
 */
void
marshall_array_in_guint8_len (const gint *ints, guint8 length)
{
  g_assert_cmpint (length, ==, 4);

  marshall_array_in (ints, length);
}

/**
 * marshall_array_out:
 * @ints: (out) (array length=length) (transfer none):
 */
void
marshall_array_out (gint **ints, gint *length)
{
  static gint values[] = { -1, 0, 1, 2 };

  *length = 4;
  *ints = values;
}

/**
 * marshall_array_out_etc:
 * @first:
 * @ints: (out) (array length=length) (transfer none):
 * @length: (out):
 * @last:
 * @sum: (out):
 */
void
marshall_array_out_etc (gint first, gint **ints, gint *length, gint last, gint *sum)
{
  static gint values[] = { -1, 0, 1, 2 };

  values[0] = first;
  values[3] = last;
  *sum = first + last;
  *length = 4;
  *ints = values;
}

/**
 * marshall_array_bool_out:
 * @bools: (out) (array length=length) (transfer none):
 */
void
marshall_array_bool_out (const gboolean **bools, gint *length)
{
  static const gboolean values[] = { TRUE, FALSE, TRUE, TRUE };

  *length = 4;
  *bools = values;
}

/**
 * marshall_array_unichar_out:
 * @chars: (out) (array length=length) (transfer none):
 */
void
marshall_array_unichar_out (const gunichar **chars, gint *length)
{
  static const gunichar values[] = MARSHALL_CONSTANT_UCS4;
  *length = 12;
  *chars = values;
}

/**
 * marshall_array_inout:
 * @ints: (inout) (array length=length) (transfer none):
 * @length: (inout):
 */
void
marshall_array_inout (gint **ints, gint *length)
{
  static gint values[] = { -2, -1, 0, 1, 2 };

  g_assert_cmpint (*length, ==, 4);
  g_assert_cmpint ((*ints)[0], ==, -1);
  g_assert_cmpint ((*ints)[1], ==, 0);
  g_assert_cmpint ((*ints)[2], ==, 1);
  g_assert_cmpint ((*ints)[3], ==, 2);

  *length = 5;
  *ints = values;
}

/**
 * marshall_array_inout_etc:
 * @first:
 * @ints: (inout) (array length=length) (transfer none):
 * @length: (inout):
 * @last:
 * @sum: (out):
 */
void
marshall_array_inout_etc (gint first, gint **ints, gint *length, gint last, gint *sum)
{
  static gint values[] = { -2, -1, 0, 1, 2 };

  g_assert_cmpint (*length, ==, 4);
  g_assert_cmpint ((*ints)[0], ==, -1);
  g_assert_cmpint ((*ints)[1], ==, 0);
  g_assert_cmpint ((*ints)[2], ==, 1);
  g_assert_cmpint ((*ints)[3], ==, 2);

  values[0] = first;
  values[4] = last;
  *sum = first + last;
  *length = 5;
  *ints = values;
}

/**
 * marshall_array_in_nonzero_nonlen:
 * @first:
 * @chars: (array):
 */
void
marshall_array_in_nonzero_nonlen (gint first, const guint8 *chars)
{
  g_assert (chars[0] == 'a');
  g_assert (chars[1] == 'b');
  g_assert (chars[2] == 'c');
  g_assert (chars[3] == 'd');
}

/**
 * marshall_array_zero_terminated_return:
 *
 * Returns: (array zero-terminated) (transfer none):
 */
const gchar **
marshall_array_zero_terminated_return (void)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  return values;
}

/**
 * marshall_array_zero_terminated_return_null:
 *
 * Returns: (array zero-terminated) (transfer none):
 */
gchar **
marshall_array_zero_terminated_return_null (void)
{
  return NULL;
}

/**
 * marshall_array_zero_terminated_return_struct:
 *
 * Returns: (array zero-terminated) (transfer full):
 */
MarshallBoxedStruct **
marshall_array_zero_terminated_return_struct (void)
{
  MarshallBoxedStruct **ret = (MarshallBoxedStruct **) g_new (gpointer, 4);

  ret[0] = marshall_boxed_struct_new ();
  ret[0]->long_ = 42;

  ret[1] = marshall_boxed_struct_new ();
  ret[1]->long_ = 43;

  ret[2] = marshall_boxed_struct_new ();
  ret[2]->long_ = 44;

  ret[3] = NULL;

  return ret;
}

/**
 * marshall_array_zero_terminated_return_unichar:
 *
 * Returns: (array zero-terminated) (transfer full):
 */
gunichar *
marshall_array_zero_terminated_return_unichar (void)
{
  static const gunichar value[] = MARSHALL_CONSTANT_UCS4;
  gunichar *retval = g_new0(gunichar, 13);
  memcpy (retval, value, 12 * sizeof (gunichar));
  return retval;
}

/**
 * marshall_array_zero_terminated_in:
 * @utf8s: (array zero-terminated) (transfer none):
 */
void
marshall_array_zero_terminated_in (gchar **utf8s)
{
  g_assert (g_strv_length (utf8s));
  g_assert_cmpstr (utf8s[0], ==, "0");
  g_assert_cmpstr (utf8s[1], ==, "1");
  g_assert_cmpstr (utf8s[2], ==, "2");
}

/**
 * marshall_array_zero_terminated_out:
 * @utf8s: (out) (array zero-terminated) (transfer none):
 */
void
marshall_array_zero_terminated_out (const gchar ***utf8s)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  *utf8s = values;
}

/**
 * marshall_array_zero_terminated_inout:
 * @utf8s: (inout) (array zero-terminated) (transfer none):
 */
void
marshall_array_zero_terminated_inout (const gchar ***utf8s)
{
  static const gchar *values[] = { "-1", "0", "1", "2", NULL };

  g_assert (g_strv_length ((gchar **) (*utf8s)));
  g_assert_cmpstr ((*utf8s)[0], ==, "0");
  g_assert_cmpstr ((*utf8s)[1], ==, "1");
  g_assert_cmpstr ((*utf8s)[2], ==, "2");

  *utf8s = values;
}

/**
 * marshall_array_gvariant_none_in:
 * @variants: (array zero-terminated) (transfer none):
 *
 * Returns: (array zero-terminated) (transfer none):
 */
GVariant **
marshall_array_gvariant_none_in (GVariant **variants)
{
  /* Use a static container to detect if someone tries to free it */
  static GVariant *private_container[3] = { NULL, NULL, NULL };

  if (private_container[0] == NULL)
    {
      private_container[0] = g_variant_new_int32 (27);
      private_container[1] = g_variant_new_string ("Hello");
    }

  g_assert (variants != NULL);
  g_assert_cmpint (g_variant_get_int32 (variants[0]), ==, 27);
  g_assert_cmpstr (g_variant_get_string (variants[1], NULL), ==, "Hello");
  g_assert (variants[2] == NULL);

  return private_container;
}

/**
 * marshall_array_gvariant_container_in:
 * @variants: (array zero-terminated) (transfer container):
 *
 * Returns: (array zero-terminated) (transfer container):
 */
GVariant **
marshall_array_gvariant_container_in (GVariant **variants)
{
  GVariant **container;

  g_assert (variants != NULL);
  g_assert_cmpint (g_variant_get_int32 (variants[0]), ==, 27);
  g_assert_cmpstr (g_variant_get_string (variants[1], NULL), ==, "Hello");
  g_assert (variants[2] == NULL);

  container = g_new0 (GVariant *, 3);
  container[0] = variants[0];
  container[1] = variants[1];
  g_free (variants);

  return container;
}

/**
 * marshall_array_gvariant_full_in:
 * @variants: (array zero-terminated) (transfer full):
 *
 * Returns: (array zero-terminated) (transfer full):
 */
GVariant **
marshall_array_gvariant_full_in (GVariant **variants)
{
  GVariant **container;

  g_assert (variants != NULL);
  g_assert_cmpint (g_variant_get_int32 (variants[0]), ==, 27);
  g_assert_cmpstr (g_variant_get_string (variants[1], NULL), ==, "Hello");
  g_assert (variants[2] == NULL);

  /* To catch different behaviors we reconstruct one variant from scratch,
   * while leaving the other untouched. Both approaches are legal with full
   * transfer in and out */
  container = g_new0 (GVariant *, 3);
  container[0] = g_variant_new_int32 (g_variant_get_int32 (variants[0]));
  g_variant_unref (variants[0]);
  container[1] = variants[1];
  g_free (variants);

  return container;
}

/**
 * marshall_garray_int_none_return:
 *
 * Returns: (element-type gint) (transfer none):
 */
GArray *
marshall_garray_int_none_return (void)
{
  static GArray *v = NULL;
  gint i;

  if (v == NULL)
    {
      v = g_array_new (TRUE, TRUE, sizeof (gint));
      for (i = -1; i < 3; i++)
        g_array_append_val (v, i);
    }

  return v;
}

/**
 * marshall_garray_uint64_none_return:
 *
 * Returns: (element-type guint64) (transfer none):
 */
GArray *
marshall_garray_uint64_none_return (void)
{
  static GArray *array = NULL;
  guint64 i;

  if (array == NULL)
    {
      array = g_array_new (TRUE, TRUE, sizeof (guint64));
      i = 0;
      g_array_append_val (array, i);
      i = G_MAXUINT64;
      g_array_append_val (array, i);
    }

  return array;
}

/**
 * marshall_garray_utf8_none_return:
 *
 * Returns: (element-type utf8) (transfer none):
 */
GArray *
marshall_garray_utf8_none_return (void)
{
  static GArray *array = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  if (array == NULL)
    {
      array = g_array_new (TRUE, TRUE, sizeof (gchar *));
      for (i = 0; values[i]; i++)
        g_array_append_val (array, values[i]);
    }

  return array;
}

/**
 * marshall_garray_utf8_container_return:
 *
 * Returns: (element-type utf8) (transfer container):
 */
GArray *
marshall_garray_utf8_container_return (void)
{
  GArray *array = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  array = g_array_new (TRUE, TRUE, sizeof (gchar *));
  for (i = 0; values[i]; i++)
    g_array_append_val (array, values[i]);

  return array;
}

/**
 * marshall_garray_utf8_full_return:
 *
 * Returns: (element-type utf8) (transfer full):
 */
GArray *
marshall_garray_utf8_full_return (void)
{
  GArray *array = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  array = g_array_new (TRUE, TRUE, sizeof (gchar *));
  for (i = 0; values[i]; i++)
    {
      gchar *str = g_strdup (values[i]);
      g_array_append_val (array, str);
    }

  return array;
}

/**
 * marshall_garray_int_none_in:
 * @array_: (element-type gint) (transfer none):
 */
void
marshall_garray_int_none_in (GArray *array_)
{
  g_assert_cmpint (array_->len, ==, 4);
  g_assert_cmpint (g_array_index (array_, gint, 0), ==, -1);
  g_assert_cmpint (g_array_index (array_, gint, 1), ==, 0);
  g_assert_cmpint (g_array_index (array_, gint, 2), ==, 1);
  g_assert_cmpint (g_array_index (array_, gint, 3), ==, 2);
}

/**
 * marshall_garray_uint64_none_in:
 * @array_: (element-type guint64) (transfer none):
 */
void
marshall_garray_uint64_none_in (GArray *array_)
{
  g_assert_cmpint (array_->len, ==, 2);
  g_assert_cmpint (g_array_index (array_, guint64, 0), ==, 0);
  g_assert_cmpint (g_array_index (array_, guint64, 1), ==, G_MAXUINT64);
}

/**
 * marshall_garray_utf8_none_in:
 * @array_: (element-type utf8) (transfer none):
 */
void
marshall_garray_utf8_none_in (GArray *array_)
{
  g_assert_cmpint (array_->len, ==, 3);
  g_assert_cmpstr (g_array_index (array_, gchar *, 0), ==, "0");
  g_assert_cmpstr (g_array_index (array_, gchar *, 1), ==, "1");
  g_assert_cmpstr (g_array_index (array_, gchar *, 2), ==, "2");
}

/**
 * marshall_garray_utf8_none_out:
 * @array_: (out) (element-type utf8) (transfer none):
 */
void
marshall_garray_utf8_none_out (GArray **array_)
{
  static GArray *internal = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  if (internal == NULL)
    {
      internal = g_array_new (TRUE, TRUE, sizeof (gchar *));
      for (i = 0; values[i]; i++)
        g_array_append_val (internal, values[i]);
    }

  *array_ = internal;
}

/**
 * marshall_garray_utf8_container_out:
 * @array_: (out) (element-type utf8) (transfer container):
 */
void
marshall_garray_utf8_container_out (GArray **array_)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  *array_ = NULL;

  *array_ = g_array_new (TRUE, TRUE, sizeof (gchar *));
  for (i = 0; values[i]; i++)
    g_array_append_val (*array_, values[i]);
}

/**
 * marshall_garray_utf8_full_out:
 * @array_: (out) (element-type utf8) (transfer full):
 */
void
marshall_garray_utf8_full_out (GArray **array_)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  *array_ = NULL;

  *array_ = g_array_new (TRUE, TRUE, sizeof (gchar *));
  for (i = 0; values[i]; i++)
    {
      gchar *str = g_strdup (values[i]);
      g_array_append_val (*array_, str);
    }
}

/**
 * marshall_garray_utf8_full_out_caller_allocated:
 * @array_: (out caller-allocates) (array) (element-type utf8) (transfer full):
 */
void
marshall_garray_utf8_full_out_caller_allocated (GArray *array_)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  g_array_set_size (array_, 0);
  for (i = 0; values[i]; i++)
    {
      gchar *str = g_strdup (values[i]);
      g_array_append_val (array_, str);
    }
}

/**
 * marshall_garray_utf8_none_inout:
 * @array_: (inout) (element-type utf8) (transfer none):
 */
void
marshall_garray_utf8_none_inout (GArray **array_)
{
  static GArray *internal = NULL;
  static const gchar *values[] = { "-2", "-1", "0", "1", NULL };
  gint i;

  g_assert_cmpint ((*array_)->len, ==, 3);
  g_assert_cmpstr (g_array_index (*array_, gchar *, 0), ==, "0");
  g_assert_cmpstr (g_array_index (*array_, gchar *, 1), ==, "1");
  g_assert_cmpstr (g_array_index (*array_, gchar *, 2), ==, "2");

  if (internal == NULL)
    {
      internal = g_array_new (TRUE, TRUE, sizeof (gchar *));
      for (i = 0; values[i]; i++)
        g_array_append_val (internal, values[i]);
    }

  *array_ = internal;
}

/**
 * marshall_garray_utf8_container_inout:
 * @array_: (inout) (element-type utf8) (transfer container):
 */
void
marshall_garray_utf8_container_inout (GArray **array_)
{
  static const gchar *val1 = "-2";
  static const gchar *val2 = "-1";
  static const gchar *val3 = "0";
  static const gchar *val4 = "1";
  GArray *result;

  g_assert_cmpint ((*array_)->len, ==, 3);
  g_assert_cmpstr (g_array_index (*array_, gchar *, 0), ==, "0");
  g_assert_cmpstr (g_array_index (*array_, gchar *, 1), ==, "1");
  g_assert_cmpstr (g_array_index (*array_, gchar *, 2), ==, "2");

  result = g_array_new (TRUE, TRUE, sizeof (gchar *));
  g_array_append_val (result, val1);
  g_array_append_val (result, val2);
  g_array_append_val (result, val3);
  g_array_append_val (result, val4);

  g_array_unref (*array_);
  *array_ = result;
}

/**
 * marshall_garray_utf8_full_inout:
 * @array_: (inout) (element-type utf8) (transfer full):
 */
void
marshall_garray_utf8_full_inout (GArray **array_)
{
  static const gchar *val1 = "-1";
  static const gchar *val2 = "-2";
  gchar *val;
  GArray *result;

  g_assert_cmpint ((*array_)->len, ==, 3);
  g_assert_cmpstr (g_array_index (*array_, gchar *, 0), ==, "0");
  g_assert_cmpstr (g_array_index (*array_, gchar *, 1), ==, "1");
  g_assert_cmpstr (g_array_index (*array_, gchar *, 2), ==, "2");

  result = g_array_new (TRUE, TRUE, sizeof (gchar *));
  val = g_strdup (val2);
  g_array_append_val (result, val);
  val = g_strdup (val1);
  g_array_append_val (result, val);
  val = g_strdup ("0");
  g_array_append_val (result, val);
  val = g_strdup ("1");
  g_array_append_val (result, val);

  g_array_unref (*array_);
  *array_ = result;
}

/**
 * marshall_garray_bool_none_in:
 * @array_: (element-type gboolean) (transfer none):
 */
void
marshall_garray_bool_none_in (GArray *array_)
{
  g_assert_cmpint (array_->len, ==, 4);
  g_assert_cmpint (g_array_index (array_, gboolean, 0), ==, TRUE);
  g_assert_cmpint (g_array_index (array_, gboolean, 1), ==, FALSE);
  g_assert_cmpint (g_array_index (array_, gboolean, 2), ==, TRUE);
  g_assert_cmpint (g_array_index (array_, gboolean, 3), ==, TRUE);
}

/**
 * marshall_garray_unichar_none_in:
 * @array_: (element-type gunichar) (transfer none):
 */
void
marshall_garray_unichar_none_in (GArray *array_)
{
  unsigned ix;
  static const gunichar expected[] = MARSHALL_CONSTANT_UCS4;
  g_assert_cmpint (array_->len, ==, 12);
  for (ix = 0; ix < array_->len; ix++)
    g_assert_cmpuint (g_array_index (array_, gunichar, ix), ==, expected[ix]);
}

/**
 * marshall_gptrarray_utf8_none_return:
 *
 * Returns: (element-type utf8) (transfer none):
 */
GPtrArray *
marshall_gptrarray_utf8_none_return (void)
{
  static GPtrArray *parray = NULL;
  static const gchar *values[] = { "0", "1", "2" };
  gint i;

  if (parray == NULL)
    {
      parray = g_ptr_array_new ();
      for (i = 0; i < 3; i++)
        g_ptr_array_add (parray, (gpointer) values[i]);
    }

  return parray;
}

/**
 * marshall_gptrarray_utf8_container_return:
 *
 * Returns: (element-type utf8) (transfer container):
 */
GPtrArray *
marshall_gptrarray_utf8_container_return (void)
{
  GPtrArray *parray = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  parray = g_ptr_array_new ();
  for (i = 0; values[i]; i++)
    g_ptr_array_add (parray, (gpointer) values[i]);

  return parray;
}

/**
 * marshall_gptrarray_utf8_full_return:
 *
 * Returns: (element-type utf8) (transfer full):
 */
GPtrArray *
marshall_gptrarray_utf8_full_return (void)
{
  GPtrArray *parray = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  parray = g_ptr_array_new ();
  for (i = 0; values[i]; i++)
    {
      gchar *str = g_strdup (values[i]);
      g_ptr_array_add (parray, (gpointer) str);
    }

  return parray;
}

/**
 * marshall_gptrarray_utf8_none_in:
 * @parray_: (element-type utf8) (transfer none):
 */
void
marshall_gptrarray_utf8_none_in (GPtrArray *parray_)
{
  g_assert_cmpint (parray_->len, ==, 3);
  g_assert_cmpstr (g_ptr_array_index (parray_, 0), ==, "0");
  g_assert_cmpstr (g_ptr_array_index (parray_, 1), ==, "1");
  g_assert_cmpstr (g_ptr_array_index (parray_, 2), ==, "2");
}

/**
 * marshall_gptrarray_utf8_none_out:
 * @parray_: (out) (element-type utf8) (transfer none):
 */
void
marshall_gptrarray_utf8_none_out (GPtrArray **parray_)
{
  static GPtrArray *internal = NULL;
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  if (internal == NULL)
    {
      internal = g_ptr_array_new ();
      for (i = 0; values[i]; i++)
        g_ptr_array_add (internal, (gpointer) values[i]);
    }

  *parray_ = internal;
}

/**
 * marshall_gptrarray_utf8_container_out:
 * @parray_: (out) (element-type utf8) (transfer container):
 */
void
marshall_gptrarray_utf8_container_out (GPtrArray **parray_)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  *parray_ = NULL;

  *parray_ = g_ptr_array_new ();
  for (i = 0; values[i]; i++)
    g_ptr_array_add (*parray_, (gpointer) values[i]);
}

/**
 * marshall_gptrarray_utf8_full_out:
 * @parray_: (out) (element-type utf8) (transfer full):
 */
void
marshall_gptrarray_utf8_full_out (GPtrArray **parray_)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  gint i;

  *parray_ = NULL;

  *parray_ = g_ptr_array_new ();
  for (i = 0; values[i]; i++)
    {
      gchar *str = g_strdup (values[i]);
      g_ptr_array_add (*parray_, (gpointer) str);
    }
}

/**
 * marshall_gptrarray_utf8_none_inout:
 * @parray_: (inout) (element-type utf8) (transfer none):
 */
void
marshall_gptrarray_utf8_none_inout (GPtrArray **parray_)
{
  static GPtrArray *internal = NULL;
  static const gchar *values[] = { "-2", "-1", "0", "1", NULL };
  gint i;

  g_assert_cmpint ((*parray_)->len, ==, 3);
  g_assert_cmpstr (g_ptr_array_index (*parray_, 0), ==, "0");
  g_assert_cmpstr (g_ptr_array_index (*parray_, 1), ==, "1");
  g_assert_cmpstr (g_ptr_array_index (*parray_, 2), ==, "2");

  if (internal == NULL)
    {
      internal = g_ptr_array_new ();
      for (i = 0; values[i]; i++)
        g_ptr_array_add (internal, (gpointer) values[i]);
    }

  *parray_ = internal;
}

/**
 * marshall_gptrarray_utf8_container_inout:
 * @parray_: (inout) (element-type utf8) (transfer container):
 */
void
marshall_gptrarray_utf8_container_inout (GPtrArray **parray_)
{
  static const gchar *val1 = "-2";
  static const gchar *val2 = "-1";
  static const gchar *val3 = "0";
  static const gchar *val4 = "1";
  GPtrArray *result;

  g_assert_cmpint ((*parray_)->len, ==, 3);
  g_assert_cmpstr (g_ptr_array_index (*parray_, 0), ==, "0");
  g_assert_cmpstr (g_ptr_array_index (*parray_, 1), ==, "1");
  g_assert_cmpstr (g_ptr_array_index (*parray_, 2), ==, "2");

  result = g_ptr_array_new ();
  g_ptr_array_add (result, (gpointer) val1);
  g_ptr_array_add (result, (gpointer) val2);
  g_ptr_array_add (result, (gpointer) val3);
  g_ptr_array_add (result, (gpointer) val4);

  g_ptr_array_unref (*parray_);
  *parray_ = result;
}

/**
 * marshall_gptrarray_utf8_full_inout:
 * @parray_: (inout) (element-type utf8) (transfer full):
 */
void
marshall_gptrarray_utf8_full_inout (GPtrArray **parray_)
{
  static const gchar *val1 = "-1";
  static const gchar *val2 = "-2";
  gchar *val;
  GPtrArray *result;

  g_assert_cmpint ((*parray_)->len, ==, 3);
  g_assert_cmpstr (g_ptr_array_index (*parray_, 0), ==, "0");
  g_assert_cmpstr (g_ptr_array_index (*parray_, 1), ==, "1");
  g_assert_cmpstr (g_ptr_array_index (*parray_, 2), ==, "2");

  result = g_ptr_array_new ();
  val = g_strdup (val2);
  g_ptr_array_add (result, (gpointer) val);
  val = g_strdup (val1);
  g_ptr_array_add (result, (gpointer) val);
  val = g_strdup ("0");
  g_ptr_array_add (result, (gpointer) val);
  val = g_strdup ("1");
  g_ptr_array_add (result, (gpointer) val);

  g_ptr_array_unref (*parray_);
  *parray_ = result;
}

/**
 * marshall_bytearray_full_return:
 *
 * Returns: (transfer full):
 */
GByteArray *
marshall_bytearray_full_return (void)
{
  GByteArray *array = NULL;
  guint8 data[] = { '\0', '1', '\xFF', '3' };

  array = g_byte_array_new ();
  g_byte_array_append (array, (const guint8 *) data, G_N_ELEMENTS (data));

  return array;

}

/**
 * marshall_bytearray_none_in:
 * @v: (element-type gint8) (transfer none):
 */
void
marshall_bytearray_none_in (GByteArray *v)
{
  g_assert_cmpuint (v->len, ==, 4);
  g_assert_cmpuint (g_array_index (v, unsigned char, 0), ==, 0);
  g_assert_cmpuint (g_array_index (v, unsigned char, 1), ==, 49);
  g_assert_cmpuint (g_array_index (v, unsigned char, 2), ==, 0xFF);
  g_assert_cmpuint (g_array_index (v, unsigned char, 3), ==, 51);
}

/**
 * marshall_gbytes_full_return:
 *
 * Returns: (transfer full):
 */
GBytes *
marshall_gbytes_full_return (void)
{
  static guint8 data[] = { 0, 49, 0xFF, 51 };

  return g_bytes_new_static (data, G_N_ELEMENTS (data));
}

/**
 * marshall_gbytes_none_in:
 */
void
marshall_gbytes_none_in (GBytes *v)
{
  const guint8 *data;
  gsize len;
  data = g_bytes_get_data (v, &len);

  g_assert_cmpuint (len, ==, 4);
  g_assert_cmpuint (data[0], ==, 0);
  g_assert_cmpuint (data[1], ==, 49);
  g_assert_cmpuint (data[2], ==, 0xFF);
  g_assert_cmpuint (data[3], ==, 51);
}

/**
 * marshall_gstrv_return:
 *
 * Returns: (transfer full): an array of strings
 */
GStrv
marshall_gstrv_return (void)
{
  GStrv values = g_new0 (gchar *, 4);
  values[0] = g_strdup ("0");
  values[1] = g_strdup ("1");
  values[2] = g_strdup ("2");
  values[3] = NULL;
  return values;
}

/**
 * marshall_gstrv_in:
 * @g_strv:
 */
void
marshall_gstrv_in (GStrv g_strv)
{
  g_assert_cmpint (g_strv_length (g_strv), ==, 3);
  g_assert_cmpstr (g_strv[0], ==, "0");
  g_assert_cmpstr (g_strv[1], ==, "1");
  g_assert_cmpstr (g_strv[2], ==, "2");
}

/**
 * marshall_gstrv_out:
 * @g_strv: (out) (transfer none):
 */
void
marshall_gstrv_out (GStrv *g_strv)
{
  static const gchar *values[] = { "0", "1", "2", NULL };
  *g_strv = (gchar **) values;
}

/**
 * marshall_gstrv_inout:
 * @g_strv: (inout) (transfer none):
 */
void
marshall_gstrv_inout (GStrv *g_strv)
{
  static const gchar *values[] = { "-1", "0", "1", "2", NULL };

  g_assert (g_strv_length (*g_strv) == 3);
  g_assert (strcmp ((*g_strv)[0], "0") == 0);
  g_assert (strcmp ((*g_strv)[1], "1") == 0);
  g_assert (strcmp ((*g_strv)[2], "2") == 0);

  *g_strv = (gchar **) values;
}

/**
 * marshall_glist_int_none_return:
 *
 * Returns: (element-type gint) (transfer none):
 */
GList *
marshall_glist_int_none_return (void)
{
  static GList *list = NULL;

  if (list == NULL)
    {
      list = g_list_append (list, GINT_TO_POINTER (-1));
      list = g_list_append (list, GINT_TO_POINTER (0));
      list = g_list_append (list, GINT_TO_POINTER (1));
      list = g_list_append (list, GINT_TO_POINTER (2));
    }

  return list;
}

/**
 * marshall_glist_uint32_none_return:
 *
 * Returns: (element-type guint32) (transfer none):
 */
GList *
marshall_glist_uint32_none_return (void)
{
  static GList *list = NULL;

  if (list == NULL)
    {
      list = g_list_append (list, GUINT_TO_POINTER (0));
      list = g_list_append (list, GUINT_TO_POINTER (G_MAXUINT32));
    }

  return list;
}

/**
 * marshall_glist_utf8_none_return:
 *
 * Returns: (element-type utf8) (transfer none):
 */
GList *
marshall_glist_utf8_none_return (void)
{
  static GList *list = NULL;

  if (list == NULL)
    {
      list = g_list_append (list, (gpointer) "0");
      list = g_list_append (list, (gpointer) "1");
      list = g_list_append (list, (gpointer) "2");
    }

  return list;
}

/**
 * marshall_glist_utf8_container_return:
 *
 * Returns: (element-type utf8) (transfer container):
 */
GList *
marshall_glist_utf8_container_return (void)
{
  GList *list = NULL;

  list = g_list_append (list, (gpointer) "0");
  list = g_list_append (list, (gpointer) "1");
  list = g_list_append (list, (gpointer) "2");

  return list;
}

/**
 * marshall_glist_utf8_full_return:
 *
 * Returns: (element-type utf8) (transfer full):
 */
GList *
marshall_glist_utf8_full_return (void)
{
  GList *list = NULL;

  list = g_list_append (list, g_strdup ("0"));
  list = g_list_append (list, g_strdup ("1"));
  list = g_list_append (list, g_strdup ("2"));

  return list;
}

/**
 * marshall_glist_int_none_in:
 * @list: (element-type gint) (transfer none):
 */
void
marshall_glist_int_none_in (GList *list)
{
  g_assert_cmpint (g_list_length (list), ==, 4);
  g_assert_cmpint (GPOINTER_TO_INT (g_list_nth_data (list, 0)), ==, -1);
  g_assert_cmpint (GPOINTER_TO_INT (g_list_nth_data (list, 1)), ==, 0);
  g_assert_cmpint (GPOINTER_TO_INT (g_list_nth_data (list, 2)), ==, 1);
  g_assert_cmpint (GPOINTER_TO_INT (g_list_nth_data (list, 3)), ==, 2);
}

/**
 * marshall_glist_uint32_none_in:
 * @list: (element-type guint32) (transfer none):
 */
void
marshall_glist_uint32_none_in (GList *list)
{
  g_assert_cmpint (g_list_length (list), ==, 2);
  g_assert_cmpint (GPOINTER_TO_UINT (g_list_nth_data (list, 0)), ==, 0);
  g_assert_cmpint (GPOINTER_TO_UINT (g_list_nth_data (list, 1)), ==, G_MAXUINT32);
}

/**
 * marshall_glist_utf8_none_in:
 * @list: (element-type utf8) (transfer none):
 */
void
marshall_glist_utf8_none_in (GList *list)
{
  g_assert_cmpint (g_list_length (list), ==, 3);
  g_assert_cmpint (strcmp (g_list_nth_data (list, 0), "0"), ==, 0);
  g_assert_cmpint (strcmp (g_list_nth_data (list, 1), "1"), ==, 0);
  g_assert_cmpint (strcmp (g_list_nth_data (list, 2), "2"), ==, 0);
}

/**
 * marshall_glist_utf8_none_out:
 * @list: (out) (element-type utf8) (transfer none):
 */
void
marshall_glist_utf8_none_out (GList **list)
{
  static GList *values = NULL;

  if (values == NULL)
    {
      values = g_list_append (values, (gpointer) "0");
      values = g_list_append (values, (gpointer) "1");
      values = g_list_append (values, (gpointer) "2");
    }

  *list = values;
}

/**
 * marshall_glist_utf8_container_out:
 * @list: (out) (element-type utf8) (transfer container):
 */
void
marshall_glist_utf8_container_out (GList **list)
{
  *list = NULL;

  *list = g_list_append (*list, (gpointer) "0");
  *list = g_list_append (*list, (gpointer) "1");
  *list = g_list_append (*list, (gpointer) "2");
}

/**
 * marshall_glist_utf8_full_out:
 * @list: (out) (element-type utf8) (transfer full):
 */
void
marshall_glist_utf8_full_out (GList **list)
{
  *list = NULL;

  *list = g_list_append (*list, g_strdup ("0"));
  *list = g_list_append (*list, g_strdup ("1"));
  *list = g_list_append (*list, g_strdup ("2"));
}

/**
 * marshall_glist_utf8_none_inout:
 * @list: (inout) (element-type utf8) (transfer none):
 */
void
marshall_glist_utf8_none_inout (GList **list)
{
  static GList *values = NULL;

  g_assert_cmpint (g_list_length (*list), ==, 3);
  g_assert_cmpstr (g_list_nth_data (*list, 0), ==, "0");
  g_assert_cmpstr (g_list_nth_data (*list, 1), ==, "1");
  g_assert_cmpstr (g_list_nth_data (*list, 2), ==, "2");

  if (values == NULL)
    {
      values = g_list_append (values, (gpointer) "-2");
      values = g_list_append (values, (gpointer) "-1");
      values = g_list_append (values, (gpointer) "0");
      values = g_list_append (values, (gpointer) "1");
    }

  *list = values;
}

/**
 * marshall_glist_utf8_container_inout:
 * @list: (inout) (element-type utf8) (transfer container):
 */
void
marshall_glist_utf8_container_inout (GList **list)
{
  GList *result = NULL;

  g_assert_cmpint (g_list_length (*list), ==, 3);
  g_assert_cmpstr (g_list_nth_data (*list, 0), ==, "0");
  g_assert_cmpstr (g_list_nth_data (*list, 1), ==, "1");
  g_assert_cmpstr (g_list_nth_data (*list, 2), ==, "2");

  result = g_list_prepend (result, (gpointer) "1");
  result = g_list_prepend (result, (gpointer) "0");
  result = g_list_prepend (result, (gpointer) "-1");
  result = g_list_prepend (result, (gpointer) "-2");

  g_list_free (*list);
  *list = result;
}

/**
 * marshall_glist_utf8_full_inout:
 * @list: (inout) (element-type utf8) (transfer full):
 */
void
marshall_glist_utf8_full_inout (GList **list)
{
  GList *result = NULL;

  g_assert_cmpint (g_list_length (*list), ==, 3);
  g_assert_cmpstr (g_list_nth_data (*list, 0), ==, "0");
  g_assert_cmpstr (g_list_nth_data (*list, 1), ==, "1");
  g_assert_cmpstr (g_list_nth_data (*list, 2), ==, "2");

  result = g_list_prepend (result, g_strdup ("1"));
  result = g_list_prepend (result, g_strdup ("0"));
  result = g_list_prepend (result, g_strdup ("-1"));
  result = g_list_prepend (result, g_strdup ("-2"));

  g_list_free_full (*list, g_free);
  *list = result;
}


/**
 * marshall_gslist_int_none_return:
 *
 * Returns: (element-type gint) (transfer none):
 */
GSList *
marshall_gslist_int_none_return (void)
{
  static GSList *list = NULL;

  if (list == NULL)
    {
      list = g_slist_prepend (list, GINT_TO_POINTER (-1));
      list = g_slist_prepend (list, GINT_TO_POINTER (0));
      list = g_slist_prepend (list, GINT_TO_POINTER (1));
      list = g_slist_prepend (list, GINT_TO_POINTER (2));
      list = g_slist_reverse (list);
    }

  return list;
}

/**
 * marshall_gslist_utf8_none_return:
 *
 * Returns: (element-type utf8) (transfer none):
 */
GSList *
marshall_gslist_utf8_none_return (void)
{
  static GSList *list = NULL;

  if (list == NULL)
    {
      list = g_slist_prepend (list, (gpointer) "0");
      list = g_slist_prepend (list, (gpointer) "1");
      list = g_slist_prepend (list, (gpointer) "2");
      list = g_slist_reverse (list);
    }

  return list;
}

/**
 * marshall_gslist_utf8_container_return:
 *
 * Returns: (element-type utf8) (transfer container):
 */
GSList *
marshall_gslist_utf8_container_return (void)
{
  GSList *list = NULL;

  list = g_slist_prepend (list, (gpointer) "0");
  list = g_slist_prepend (list, (gpointer) "1");
  list = g_slist_prepend (list, (gpointer) "2");
  list = g_slist_reverse (list);

  return list;
}

/**
 * marshall_gslist_utf8_full_return:
 *
 * Returns: (element-type utf8) (transfer full):
 */
GSList *
marshall_gslist_utf8_full_return (void)
{
  GSList *list = NULL;

  list = g_slist_prepend (list, g_strdup ("0"));
  list = g_slist_prepend (list, g_strdup ("1"));
  list = g_slist_prepend (list, g_strdup ("2"));
  list = g_slist_reverse (list);

  return list;
}

/**
 * marshall_gslist_int_none_in:
 * @list: (element-type gint) (transfer none):
 */
void
marshall_gslist_int_none_in (GSList *list)
{
  g_assert_cmpint (g_slist_length (list), ==, 4);
  g_assert_cmpint (GPOINTER_TO_INT (g_slist_nth_data (list, 0)), ==, -1);
  g_assert_cmpint (GPOINTER_TO_INT (g_slist_nth_data (list, 1)), ==, 0);
  g_assert_cmpint (GPOINTER_TO_INT (g_slist_nth_data (list, 2)), ==, 1);
  g_assert_cmpint (GPOINTER_TO_INT (g_slist_nth_data (list, 3)), ==, 2);
}

/**
 * marshall_gslist_utf8_none_in:
 * @list: (element-type utf8) (transfer none):
 */
void
marshall_gslist_utf8_none_in (GSList *list)
{
  g_assert_cmpint (g_slist_length (list), ==, 3);
  g_assert_cmpstr (g_slist_nth_data (list, 0), ==, "0");
  g_assert_cmpstr (g_slist_nth_data (list, 1), ==, "1");
  g_assert_cmpstr (g_slist_nth_data (list, 2), ==, "2");
}

/**
 * marshall_gslist_utf8_none_out:
 * @list: (out) (element-type utf8) (transfer none):
 */
void
marshall_gslist_utf8_none_out (GSList **list)
{
  static GSList *values = NULL;

  if (values == NULL)
    {
      values = g_slist_prepend (values, (gpointer) "0");
      values = g_slist_prepend (values, (gpointer) "1");
      values = g_slist_prepend (values, (gpointer) "2");
      values = g_slist_reverse (values);
    }

  *list = values;
}

/**
 * marshall_gslist_utf8_container_out:
 * @list: (out) (element-type utf8) (transfer container):
 */
void
marshall_gslist_utf8_container_out (GSList **list)
{
  *list = NULL;

  *list = g_slist_prepend (*list, (gpointer) "0");
  *list = g_slist_prepend (*list, (gpointer) "1");
  *list = g_slist_prepend (*list, (gpointer) "2");
  *list = g_slist_reverse (*list);
}

/**
 * marshall_gslist_utf8_full_out:
 * @list: (out) (element-type utf8) (transfer full):
 */
void
marshall_gslist_utf8_full_out (GSList **list)
{
  *list = NULL;

  *list = g_slist_prepend (*list, g_strdup ("0"));
  *list = g_slist_prepend (*list, g_strdup ("1"));
  *list = g_slist_prepend (*list, g_strdup ("2"));
  *list = g_slist_reverse (*list);
}

/**
 * marshall_gslist_utf8_none_inout:
 * @list: (inout) (element-type utf8) (transfer none):
 */
void
marshall_gslist_utf8_none_inout (GSList **list)
{
  static GSList *values = NULL;

  g_assert_cmpint (g_slist_length (*list), ==, 3);
  g_assert_cmpstr (g_slist_nth_data (*list, 0), ==, "0");
  g_assert_cmpstr (g_slist_nth_data (*list, 1), ==, "1");
  g_assert_cmpstr (g_slist_nth_data (*list, 2), ==, "2");

  if (values == NULL)
    {
      values = g_slist_prepend (values, (gpointer) "-2");
      values = g_slist_prepend (values, (gpointer) "-1");
      values = g_slist_prepend (values, (gpointer) "0");
      values = g_slist_prepend (values, (gpointer) "1");
      values = g_slist_reverse (values);
    }

  *list = values;
}

/**
 * marshall_gslist_utf8_container_inout:
 * @list: (inout) (element-type utf8) (transfer container):
 */
void
marshall_gslist_utf8_container_inout (GSList **list)
{
  GSList *result = NULL;

  g_assert_cmpint (g_slist_length (*list), ==, 3);
  g_assert_cmpstr (g_slist_nth_data (*list, 0), ==, "0");
  g_assert_cmpstr (g_slist_nth_data (*list, 1), ==, "1");
  g_assert_cmpstr (g_slist_nth_data (*list, 2), ==, "2");

  result = g_slist_prepend (result, (gpointer) "1");
  result = g_slist_prepend (result, (gpointer) "0");
  result = g_slist_prepend (result, (gpointer) "-1");
  result = g_slist_prepend (result, (gpointer) "-2");

  g_slist_free (*list);
  *list = result;
}

/**
 * marshall_gslist_utf8_full_inout:
 * @list: (inout) (element-type utf8) (transfer full):
 */
void
marshall_gslist_utf8_full_inout (GSList **list)
{
  GSList *result = NULL;

  g_assert_cmpint (g_slist_length (*list), ==, 3);
  g_assert_cmpstr (g_slist_nth_data (*list, 0), ==, "0");
  g_assert_cmpstr (g_slist_nth_data (*list, 1), ==, "1");
  g_assert_cmpstr (g_slist_nth_data (*list, 2), ==, "2");

  result = g_slist_prepend (result, g_strdup ("1"));
  result = g_slist_prepend (result, g_strdup ("0"));
  result = g_slist_prepend (result, g_strdup ("-1"));
  result = g_slist_prepend (result, g_strdup ("-2"));

  g_slist_free_full (*list, g_free);
  *list = result;
}


/**
 * marshall_ghashtable_int_none_return:
 *
 * Returns: (element-type gint gint) (transfer none):
 */
GHashTable *
marshall_ghashtable_int_none_return (void)
{
  static GHashTable *hash_table = NULL;

  if (hash_table == NULL)
    {
      hash_table = g_hash_table_new (NULL, NULL);
      g_hash_table_insert (hash_table, GINT_TO_POINTER (-1), GINT_TO_POINTER (1));
      g_hash_table_insert (hash_table, GINT_TO_POINTER (0), GINT_TO_POINTER (0));
      g_hash_table_insert (hash_table, GINT_TO_POINTER (1), GINT_TO_POINTER (-1));
      g_hash_table_insert (hash_table, GINT_TO_POINTER (2), GINT_TO_POINTER (-2));
    }

  return hash_table;
}

/**
 * marshall_ghashtable_utf8_none_return:
 *
 * Returns: (element-type utf8 utf8) (transfer none):
 */
GHashTable *
marshall_ghashtable_utf8_none_return (void)
{
  static GHashTable *hash_table = NULL;

  if (hash_table == NULL)
    {
      hash_table = g_hash_table_new (g_str_hash, g_str_equal);
      g_hash_table_insert (hash_table, (gpointer) "-1", (gpointer) "1");
      g_hash_table_insert (hash_table, (gpointer) "0",  (gpointer) "0");
      g_hash_table_insert (hash_table, (gpointer) "1",  (gpointer) "-1");
      g_hash_table_insert (hash_table, (gpointer) "2",  (gpointer) "-2");
    }

  return hash_table;
}

/**
 * marshall_ghashtable_utf8_container_return:
 *
 * Returns: (element-type utf8 utf8) (transfer container):
 */
GHashTable *
marshall_ghashtable_utf8_container_return (void)
{
  GHashTable *hash_table = NULL;

  hash_table = g_hash_table_new (g_str_hash, g_str_equal);
  g_hash_table_insert (hash_table, (gpointer) "-1", (gpointer) "1");
  g_hash_table_insert (hash_table, (gpointer) "0",  (gpointer) "0");
  g_hash_table_insert (hash_table, (gpointer) "1",  (gpointer) "-1");
  g_hash_table_insert (hash_table, (gpointer) "2",  (gpointer) "-2");

  return hash_table;
}

/**
 * marshall_ghashtable_utf8_full_return:
 *
 * Returns: (element-type utf8 utf8) (transfer full):
 */
GHashTable *
marshall_ghashtable_utf8_full_return (void)
{
  GHashTable *hash_table = NULL;

  hash_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  g_hash_table_insert (hash_table, g_strdup ("-1"), g_strdup ("1"));
  g_hash_table_insert (hash_table, g_strdup ("0"), g_strdup ("0"));
  g_hash_table_insert (hash_table, g_strdup ("1"), g_strdup ("-1"));
  g_hash_table_insert (hash_table, g_strdup ("2"), g_strdup ("-2"));

  return hash_table;
}

/**
 * marshall_ghashtable_int_none_in:
 * @hash_table: (element-type gint gint) (transfer none):
 */
void
marshall_ghashtable_int_none_in (GHashTable *hash_table)
{
  g_assert_cmpint (GPOINTER_TO_INT (g_hash_table_lookup (hash_table, GINT_TO_POINTER (-1))), ==, 1);
  g_assert_cmpint (GPOINTER_TO_INT (g_hash_table_lookup (hash_table, GINT_TO_POINTER (0))), ==, 0);
  g_assert_cmpint (GPOINTER_TO_INT (g_hash_table_lookup (hash_table, GINT_TO_POINTER (1))), ==, -1);
  g_assert_cmpint (GPOINTER_TO_INT (g_hash_table_lookup (hash_table, GINT_TO_POINTER (2))), ==, -2);
}

/**
 * marshall_ghashtable_utf8_none_in:
 * @hash_table: (element-type utf8 utf8) (transfer none):
 */
void
marshall_ghashtable_utf8_none_in (GHashTable *hash_table)
{
  g_assert_cmpstr (g_hash_table_lookup (hash_table, "-1"), ==, "1");
  g_assert_cmpstr (g_hash_table_lookup (hash_table, "0"), ==, "0");
  g_assert_cmpstr (g_hash_table_lookup (hash_table, "1"), ==, "-1");
  g_assert_cmpstr (g_hash_table_lookup (hash_table, "2"), ==, "-2");
}

/**
 * marshall_ghashtable_double_in:
 * @hash_table: (element-type utf8 double) (transfer none):
 *
 * Meant to test a value type that doesn't fit inside a pointer.
 */
void
marshall_ghashtable_double_in (GHashTable *hash_table)
{
  double *value;

  value = g_hash_table_lookup (hash_table, "-1");
  g_assert_cmpfloat (*value, ==, -0.1);
  value = g_hash_table_lookup (hash_table, "0");
  g_assert_cmpfloat (*value, ==, 0.0);
  value = g_hash_table_lookup (hash_table, "1");
  g_assert_cmpfloat (*value, ==, 0.1);
  value = g_hash_table_lookup (hash_table, "2");
  g_assert_cmpfloat (*value, ==, 0.2);
}

/**
 * marshall_ghashtable_float_in:
 * @hash_table: (element-type utf8 float) (transfer none):
 *
 * Meant to test a value type that doesn't fit inside a pointer.
 */
void
marshall_ghashtable_float_in (GHashTable *hash_table)
{
  float *value;

  value = g_hash_table_lookup (hash_table, "-1");
  g_assert_cmpfloat (*value, ==, -0.1f);
  value = g_hash_table_lookup (hash_table, "0");
  g_assert_cmpfloat (*value, ==, 0.0f);
  value = g_hash_table_lookup (hash_table, "1");
  g_assert_cmpfloat (*value, ==, 0.1f);
  value = g_hash_table_lookup (hash_table, "2");
  g_assert_cmpfloat (*value, ==, 0.2f);
}

/**
 * marshall_ghashtable_int64_in:
 * @hash_table: (element-type utf8 gint64) (transfer none):
 *
 * Meant to test a value type that doesn't fit inside a pointer.
 */
void
marshall_ghashtable_int64_in (GHashTable *hash_table)
{
  gint64 *value;

  value = g_hash_table_lookup (hash_table, "-1");
  g_assert_cmpint (*value, ==, -1);
  value = g_hash_table_lookup (hash_table, "0");
  g_assert_cmpint (*value, ==, 0);
  value = g_hash_table_lookup (hash_table, "1");
  g_assert_cmpint (*value, ==, 1);
  value = g_hash_table_lookup (hash_table, "2");
  g_assert_cmpint (*value, ==, (gint64) G_MAXUINT32 + 1);
}

/**
 * marshall_ghashtable_uint64_in:
 * @hash_table: (element-type utf8 guint64) (transfer none):
 *
 * Meant to test a value type that doesn't fit inside a pointer.
 */
void
marshall_ghashtable_uint64_in (GHashTable *hash_table)
{
  guint64 *value;

  value = g_hash_table_lookup (hash_table, "-1");
  g_assert_cmpuint (*value, ==, (guint64) G_MAXUINT32 + 1);
  value = g_hash_table_lookup (hash_table, "0");
  g_assert_cmpuint (*value, ==, 0);
  value = g_hash_table_lookup (hash_table, "1");
  g_assert_cmpuint (*value, ==, 1);
  value = g_hash_table_lookup (hash_table, "2");
  g_assert_cmpuint (*value, ==, 2);
}

/**
 * marshall_ghashtable_utf8_none_out:
 * @hash_table: (out) (element-type utf8 utf8) (transfer none):
 */
void
marshall_ghashtable_utf8_none_out (GHashTable **hash_table)
{
  static GHashTable *new_hash_table = NULL;

  if (new_hash_table == NULL)
    {
      new_hash_table = g_hash_table_new (g_str_hash, g_str_equal);
      g_hash_table_insert (new_hash_table, (gpointer) "-1", (gpointer) "1");
      g_hash_table_insert (new_hash_table, (gpointer) "0",  (gpointer) "0");
      g_hash_table_insert (new_hash_table, (gpointer) "1",  (gpointer) "-1");
      g_hash_table_insert (new_hash_table, (gpointer) "2",  (gpointer) "-2");
    }

  *hash_table = new_hash_table;
}

/**
 * marshall_ghashtable_utf8_container_out:
 * @hash_table: (out) (element-type utf8 utf8) (transfer container):
 */
void
marshall_ghashtable_utf8_container_out (GHashTable **hash_table)
{
  *hash_table = g_hash_table_new (g_str_hash, g_str_equal);
  g_hash_table_insert (*hash_table, (gpointer) "-1", (gpointer) "1");
  g_hash_table_insert (*hash_table, (gpointer) "0",  (gpointer) "0");
  g_hash_table_insert (*hash_table, (gpointer) "1",  (gpointer) "-1");
  g_hash_table_insert (*hash_table, (gpointer) "2",  (gpointer) "-2");
}

/**
 * marshall_ghashtable_utf8_full_out:
 * @hash_table: (out) (element-type utf8 utf8) (transfer full):
 */
void
marshall_ghashtable_utf8_full_out (GHashTable **hash_table)
{
  *hash_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  g_hash_table_insert (*hash_table, g_strdup ("-1"), g_strdup ("1"));
  g_hash_table_insert (*hash_table, g_strdup ("0"), g_strdup ("0"));
  g_hash_table_insert (*hash_table, g_strdup ("1"), g_strdup ("-1"));
  g_hash_table_insert (*hash_table, g_strdup ("2"), g_strdup ("-2"));
}

/**
 * marshall_ghashtable_utf8_none_inout:
 * @hash_table: (inout) (element-type utf8 utf8) (transfer none):
 */
void
marshall_ghashtable_utf8_none_inout (GHashTable **hash_table)
{
  static GHashTable *new_hash_table = NULL;

  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "-1"), ==, "1");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "0"), ==, "0");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "1"), ==, "-1");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "2"), ==, "-2");

  if (new_hash_table == NULL)
    {
      new_hash_table = g_hash_table_new (g_str_hash, g_str_equal);
      g_hash_table_insert (new_hash_table, (gpointer) "-1", (gpointer) "1");
      g_hash_table_insert (new_hash_table, (gpointer) "0",  (gpointer) "0");
      g_hash_table_insert (new_hash_table, (gpointer) "1",  (gpointer) "1");
    }

  *hash_table = new_hash_table;
}

/**
 * marshall_ghashtable_utf8_container_inout:
 * @hash_table: (inout) (element-type utf8 utf8) (transfer container):
 */
void
marshall_ghashtable_utf8_container_inout (GHashTable **hash_table)
{
  GHashTable *result = g_hash_table_new (g_str_hash, g_str_equal);

  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "-1"), ==, "1");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "0"), ==, "0");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "1"), ==, "-1");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "2"), ==, "-2");

  g_hash_table_insert (result, (gpointer) "-1", (gpointer) "1");
  g_hash_table_insert (result, (gpointer) "0",  (gpointer) "0");
  g_hash_table_insert (result, (gpointer) "1",  (gpointer) "1");

  g_hash_table_unref (*hash_table);
  *hash_table = result;
}

/**
 * marshall_ghashtable_utf8_full_inout:
 * @hash_table: (inout) (element-type utf8 utf8) (transfer full):
 */
void
marshall_ghashtable_utf8_full_inout (GHashTable **hash_table)
{
  GHashTable *result = g_hash_table_new_full (g_str_hash, g_str_equal,
                                              g_free, g_free);

  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "-1"), ==, "1");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "0"), ==, "0");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "1"), ==, "-1");
  g_assert_cmpstr (g_hash_table_lookup (*hash_table, "2"), ==, "-2");

  g_hash_table_insert (result, g_strdup ("-1"), g_strdup ("1"));
  g_hash_table_insert (result, g_strdup ("0"), g_strdup ("0"));
  g_hash_table_insert (result, g_strdup ("1"), g_strdup ("1"));

  g_hash_table_unref (*hash_table);
  *hash_table = result;
}


/**
 * marshall_gvalue_return:
 *
 * Returns: (transfer none):
 */
GValue *
marshall_gvalue_return (void)
{
  static GValue *value = NULL;

  if (value == NULL)
    {
      value = g_new0 (GValue, 1);
      g_value_init (value, G_TYPE_INT);
      g_value_set_int (value, 42);
    }

  return value;
}

/**
 * marshall_gvalue_in:
 * @value: (transfer none):
 */
void
marshall_gvalue_in (GValue *value)
{
  g_assert_cmpint (g_value_get_int (value), ==, 42);
}

/**
 * marshall_gvalue_int64_in:
 * @value: (transfer none):
 */
void
marshall_gvalue_int64_in (GValue *value)
{
  g_assert_cmpint (g_value_get_int64 (value), ==, G_MAXINT64);
}

/**
 * marshall_gvalue_in_with_type:
 * @value: (transfer none):
 * @type:
 */
void
marshall_gvalue_in_with_type (GValue *value, GType type)
{
  g_assert (g_type_is_a (G_VALUE_TYPE (value), type));
}

/**
 * marshall_gvalue_in_with_modification:
 * @value: (transfer none):
 *
 * Expects a GValue passed by reference which is then modified by
 * this function.
 */
void
marshall_gvalue_in_with_modification (GValue *value)
{
  g_assert_cmpint (g_value_get_int (value), ==, 42);
  g_value_set_int (value, 24);
}

/**
 * marshall_gvalue_in_enum:
 * @value: (transfer none):
 */
void
marshall_gvalue_in_enum (GValue *value)
{
  g_assert (g_value_get_enum (value) == MARSHALL_ENUM_VALUE3);
}

/**
 * marshall_gvalue_out:
 * @value: (out) (transfer none):
 */
void
marshall_gvalue_out (GValue **value)
{
  static GValue *new_value = NULL;

  if (new_value == NULL)
    {
      new_value = g_new0 (GValue, 1);
      g_value_init (new_value, G_TYPE_INT);
      g_value_set_int (new_value, 42);
    }

  *value = new_value;
}

/**
 * marshall_gvalue_int64_out:
 * @value: (out) (transfer none):
 */
void
marshall_gvalue_int64_out (GValue **value)
{
  static GValue *new_value = NULL;

  if (new_value == NULL)
    {
      new_value = g_new0 (GValue, 1);
      g_value_init (new_value, G_TYPE_INT64);
      g_value_set_int64 (new_value, G_MAXINT64);
    }

  *value = new_value;
}

/**
 * marshall_gvalue_out_caller_allocates:
 * @value: (out) (transfer none):
 */
void
marshall_gvalue_out_caller_allocates (GValue *value)
{
  g_value_init (value, G_TYPE_INT);
  g_value_set_int (value, 42);
}

/**
 * marshall_gvalue_inout:
 * @value: (inout) (transfer none):
 */
void
marshall_gvalue_inout (GValue **value)
{
  g_assert_cmpint (g_value_get_int (*value), ==, 42);
  g_value_unset (*value);
  g_value_init (*value, G_TYPE_STRING);
  g_value_set_string (*value, "42");
}

/**
 * marshall_gvalue_flat_array:
 * @n_values: number of values
 * @values: (array length=n_values): an array containing values
 */
void
marshall_gvalue_flat_array (guint n_values, const GValue *values)
{
  g_assert (n_values == 3);

  g_assert_cmpint (g_value_get_int (&values[0]), ==, 42);
  g_assert_cmpstr (g_value_get_string (&values[1]), ==, "42");
  g_assert_cmpint (g_value_get_boolean (&values[2]), ==, TRUE);
}

/**
 * marshall_return_gvalue_flat_array:
 *
 * Returns: (array fixed-size=3) (transfer full): a flat GValue array
 */
GValue *
marshall_return_gvalue_flat_array (void)
{
  GValue *array = g_new0 (GValue, 3);

  g_value_init (&array[0], G_TYPE_INT);
  g_value_set_int (&array[0], 42);

  g_value_init (&array[1], G_TYPE_STRING);
  g_value_set_static_string (&array[1], "42");

  g_value_init (&array[2], G_TYPE_BOOLEAN);
  g_value_set_boolean (&array[2], TRUE);

  return array;
}

/**
 * marshall_gvalue_flat_array_round_trip:
 * @one: The first GValue
 * @two: The second GValue
 * @three: The third GValue
 *
 * Returns: (array fixed-size=3) (transfer full): a flat array of [@one, @two, @three]
 */
GValue *
marshall_gvalue_flat_array_round_trip (const GValue one, const GValue two, const GValue three)
{
  GValue *array = g_new (GValue, 3);
  array[0] = one;
  array[1] = two;
  array[2] = three;

  return array;
}

/**
 * marshall_gclosure_in:
 * @closure: (transfer none):
 */
void
marshall_gclosure_in (GClosure *closure)
{
  GValue return_value = { 0, };

  g_value_init (&return_value, G_TYPE_INT);

  g_closure_invoke (closure, &return_value, 0, NULL, NULL);

  g_assert_cmpint (g_value_get_int (&return_value), ==, 42);

  g_value_unset (&return_value);
}

static gint
_closure_return_42 (void)
{
  return 42;
}

static void
_marshal_INT__VOID (GClosure *closure,
                    GValue *return_value,
                    guint n_param_values, const GValue *param_values, gpointer invocation_hint, gpointer marshal_data)
{
  typedef gint (*GMarshalFunc_INT__VOID) (void);
  register GMarshalFunc_INT__VOID callback;
  register GCClosure *cc = (GCClosure *) closure;

  callback = (GMarshalFunc_INT__VOID) cc->callback;
  g_value_set_int (return_value, callback ());
}

/**
 * marshall_gclosure_return:
 *
 * Return: a #GClosure
 */
GClosure *
marshall_gclosure_return (void)
{
  GClosure *closure = g_cclosure_new ((GCallback) _closure_return_42,
                                      NULL, NULL);
  g_closure_set_marshal (closure, _marshal_INT__VOID);

  return closure;
}


/**
 * marshall_callback_return_value_only:
 * @callback: (scope call):
 */
glong marshall_callback_return_value_only (MarshallCallbackReturnValueOnly callback)
{
  return callback ();
}

/**
 * marshall_callback_one_out_parameter:
 * @callback: (scope call):
 * @a: (out):
 */
void marshall_callback_one_out_parameter (MarshallCallbackOneOutParameter callback, gfloat *a)
{
  callback (a);
}

/**
 * marshall_callback_multiple_out_parameters:
 * @callback: (scope call):
 * @a: (out):
 * @b: (out):
 */
void
  marshall_callback_multiple_out_parameters
  (MarshallCallbackMultipleOutParameters callback, gfloat *a, gfloat *b)
{
  callback (a, b);
}

/**
 * marshall_callback_return_value_and_one_out_parameter:
 * @callback: (scope call):
 * @a: (out):
 */
glong
  marshall_callback_return_value_and_one_out_parameter
  (MarshallCallbackReturnValueAndOneOutParameter callback, glong *a)
{
  return callback (a);
}

/**
 * marshall_callback_return_value_and_multiple_out_parameters:
 * @callback: (scope call):
 * @a: (out):
 * @b: (out):
 */
glong
  marshall_callback_return_value_and_multiple_out_parameters
  (MarshallCallbackReturnValueAndMultipleOutParameters callback, glong *a, glong *b)
{
  return callback (a, b);
}



/**
 * marshall_pointer_in_return:
 *
 * Returns: The same pointer
 */
gpointer
marshall_pointer_in_return (gpointer pointer)
{
  return pointer;
}

GType
marshall_genum_get_type (void)
{
  static GType type = 0;
  if (G_UNLIKELY (type == 0))
    {
      static const GEnumValue values[] = {
        {MARSHALL_GENUM_VALUE1,
         "MARSHALL_GENUM_VALUE1", "value1"},
        {MARSHALL_GENUM_VALUE2,
         "MARSHALL_GENUM_VALUE2", "value2"},
        {MARSHALL_GENUM_VALUE3,
         "MARSHALL_GENUM_VALUE3", "value3"},
        {0, NULL, NULL}
      };
      type = g_enum_register_static (g_intern_static_string ("MarshallGEnum"), values);
    }

  return type;
}

MarshallGEnum
marshall_genum_returnv (void)
{
  return MARSHALL_GENUM_VALUE3;
}

void
marshall_genum_in (MarshallGEnum v)
{
  g_assert_cmpint (v, ==, MARSHALL_GENUM_VALUE3);
}

/**
 * marshall_genum_out:
 * @v: (out):
 */
void
marshall_genum_out (MarshallGEnum *v)
{
  *v = MARSHALL_GENUM_VALUE3;
}

/**
 * marshall_genum_inout:
 * @v: (inout):
 */
void
marshall_genum_inout (MarshallGEnum *v)
{
  g_assert_cmpint (*v, ==, MARSHALL_GENUM_VALUE3);
  *v = MARSHALL_GENUM_VALUE1;
}


MarshallEnum
marshall_enum_returnv (void)
{
  return MARSHALL_ENUM_VALUE3;
}

void
marshall_enum_in (MarshallEnum v)
{
  g_assert_cmpint (v, ==, MARSHALL_ENUM_VALUE3);
}

/**
 * marshall_enum_out:
 * @v: (out):
 */
void
marshall_enum_out (MarshallEnum *v)
{
  *v = MARSHALL_ENUM_VALUE3;
}

/**
 * marshall_enum_inout:
 * @v: (inout):
 */
void
marshall_enum_inout (MarshallEnum *v)
{
  g_assert_cmpint (*v, ==, MARSHALL_ENUM_VALUE3);
  *v = MARSHALL_ENUM_VALUE1;
}


GType
marshall_flags_get_type (void)
{
  static GType type = 0;
  if (G_UNLIKELY (type == 0))
    {
      static const GFlagsValue values[] = {
        {MARSHALL_FLAGS_VALUE1,
         "MARSHALL_FLAGS_VALUE1", "value1"},
        {MARSHALL_FLAGS_VALUE2,
         "MARSHALL_FLAGS_VALUE2", "value2"},
        {MARSHALL_FLAGS_VALUE3,
         "MARSHALL_FLAGS_VALUE3", "value3"},
        {MARSHALL_FLAGS_MASK, "MARSHALL_FLAGS_MASK",
         "mask"},
        {MARSHALL_FLAGS_MASK2, "MARSHALL_FLAGS_MASK2",
         "mask2"},
        {0, NULL, NULL}
      };
      type = g_flags_register_static (g_intern_static_string ("MarshallFlags"), values);
    }

  return type;
}

MarshallFlags
marshall_flags_returnv (void)
{
  return MARSHALL_FLAGS_VALUE2;
}

void
marshall_flags_in (MarshallFlags v)
{
  g_assert (v == MARSHALL_FLAGS_VALUE2);
}

void
marshall_flags_in_zero (MarshallFlags v)
{
  g_assert (v == 0);
}

/**
 * marshall_flags_out:
 * @v: (out):
 */
void
marshall_flags_out (MarshallFlags *v)
{
  *v = MARSHALL_FLAGS_VALUE2;
}

/**
 * marshall_flags_inout:
 * @v: (inout):
 */
void
marshall_flags_inout (MarshallFlags *v)
{
  g_assert (*v == MARSHALL_FLAGS_VALUE2);
  *v = MARSHALL_FLAGS_VALUE1;
}


MarshallNoTypeFlags
marshall_no_type_flags_returnv (void)
{
  return MARSHALL_NO_TYPE_FLAGS_VALUE2;
}

void
marshall_no_type_flags_in (MarshallNoTypeFlags v)
{
  g_assert (v == MARSHALL_NO_TYPE_FLAGS_VALUE2);
}

void
marshall_no_type_flags_in_zero (MarshallNoTypeFlags v)
{
  g_assert (v == 0);
}

/**
 * marshall_no_type_flags_out:
 * @v: (out):
 */
void
marshall_no_type_flags_out (MarshallNoTypeFlags *v)
{
  *v = MARSHALL_NO_TYPE_FLAGS_VALUE2;
}

/**
 * marshall_no_type_flags_inout:
 * @v: (inout):
 */
void
marshall_no_type_flags_inout (MarshallNoTypeFlags *v)
{
  g_assert (*v == MARSHALL_NO_TYPE_FLAGS_VALUE2);
  *v = MARSHALL_NO_TYPE_FLAGS_VALUE1;
}


/**
 * marshall_simple_struct_returnv:
 *
 * Returns: (transfer none):
 */
MarshallSimpleStruct *
marshall_simple_struct_returnv (void)
{
  static MarshallSimpleStruct *struct_ = NULL;

  if (struct_ == NULL)
    {
      struct_ = g_new (MarshallSimpleStruct, 1);

      struct_->long_ = 6;
      struct_->int8 = 7;
    }

  return struct_;
}

/**
 * marshall_simple_struct_inv:
 * @struct_: (transfer none):
 */
void
marshall_simple_struct_inv (MarshallSimpleStruct *struct_)
{
  g_assert_cmpint (struct_->long_, ==, 6);
  g_assert_cmpint (struct_->int8, ==, 7);
}

void
marshall_simple_struct_method (MarshallSimpleStruct *struct_)
{
  g_assert_cmpint (struct_->long_, ==, 6);
  g_assert_cmpint (struct_->int8, ==, 7);
}


GType
marshall_pointer_struct_get_type (void)
{
  static GType type = 0;

  if (type == 0)
    {
      type = g_pointer_type_register_static ("MarshallPointerStruct");
    }

  return type;
}

/**
 * marshall_pointer_struct_returnv:
 *
 * Returns: (transfer none):
 */
MarshallPointerStruct *
marshall_pointer_struct_returnv (void)
{
  static MarshallPointerStruct *struct_ = NULL;

  if (struct_ == NULL)
    {
      struct_ = g_new (MarshallPointerStruct, 1);

      struct_->long_ = 42;
    }

  return struct_;
}

/**
 * marshall_pointer_struct_inv:
 * @struct_: (transfer none):
 */
void
marshall_pointer_struct_inv (MarshallPointerStruct *struct_)
{
  g_assert_cmpint (struct_->long_, ==, 42);
}

static MarshallBoxedStruct *
marshall_boxed_struct_copy (MarshallBoxedStruct *struct_)
{
  MarshallBoxedStruct *new_struct;

  if (struct_ == NULL)
    return NULL;

  new_struct = g_slice_new (MarshallBoxedStruct);

  *new_struct = *struct_;
  new_struct->string_ = g_strdup (struct_->string_);

  return new_struct;
}

static void
marshall_boxed_struct_free (MarshallBoxedStruct *struct_)
{
  if (struct_ != NULL)
    {
      g_free (struct_->string_);
      g_slice_free (MarshallBoxedStruct, struct_);
    }
}

GType
marshall_boxed_struct_get_type (void)
{
  static GType type = 0;

  if (type == 0)
    {
      type = g_boxed_type_register_static ("MarshallBoxedStruct",
                                           (GBoxedCopyFunc)
                                           marshall_boxed_struct_copy,
                                           (GBoxedFreeFunc) marshall_boxed_struct_free);
    }

  return type;
}

static GType
marshall_boxed_glist_get_type (void)
{
  static GType type = 0;

  if (type == 0)
    {
      type = g_boxed_type_register_static ("MarshallBoxedGList",
                                           (GBoxedCopyFunc) g_list_copy, (GBoxedFreeFunc) g_list_free);
    }

  return type;
}

MarshallBoxedStruct *
marshall_boxed_struct_new (void)
{
  return g_slice_new0 (MarshallBoxedStruct);
}

/**
 * marshall_boxed_struct_returnv:
 *
 * Returns: (transfer none):
 */
MarshallBoxedStruct *
marshall_boxed_struct_returnv (void)
{
  static MarshallBoxedStruct *struct_ = NULL;

  if (struct_ == NULL)
    {
      struct_ = g_new (MarshallBoxedStruct, 1);

      struct_->long_ = 42;
      struct_->string_ = g_strdup ("hello");
      struct_->g_strv = g_new0 (gchar *, 4);
      struct_->g_strv[0] = g_strdup ("0");
      struct_->g_strv[1] = g_strdup ("1");
      struct_->g_strv[2] = g_strdup ("2");
      struct_->g_strv[3] = NULL;
    }

  return struct_;
}

/**
 * marshall_boxed_struct_inv:
 * @struct_: (transfer none):
 */
void
marshall_boxed_struct_inv (MarshallBoxedStruct *struct_)
{
  g_assert_cmpint (struct_->long_, ==, 42);
}

/**
 * marshall_boxed_struct_out:
 * @struct_: (out) (transfer none):
 */
void
marshall_boxed_struct_out (MarshallBoxedStruct **struct_)
{
  static MarshallBoxedStruct *new_struct = NULL;

  if (new_struct == NULL)
    {
      new_struct = g_new0 (MarshallBoxedStruct, 1);

      new_struct->long_ = 42;
    }

  *struct_ = new_struct;
}

/**
 * marshall_boxed_struct_inout:
 * @struct_: (inout) (transfer full):
 */
void
marshall_boxed_struct_inout (MarshallBoxedStruct **struct_)
{
  g_assert_cmpint ((*struct_)->long_, ==, 42);

  g_boxed_free (marshall_boxed_struct_get_type(), *struct_);
  (*struct_) = g_slice_new0 (MarshallBoxedStruct);
  (*struct_)->long_ = 0;
}

static MarshallUnion *
marshall_union_copy (MarshallUnion *union_)
{
  MarshallUnion *new_union;

  new_union = g_slice_new (MarshallUnion);

  *new_union = *union_;

  return new_union;
}

static void
marshall_union_free (MarshallUnion *union_)
{
  g_slice_free (MarshallUnion, union_);
}

GType
marshall_union_get_type (void)
{
  static GType type = 0;

  if (type == 0)
    {
      type = g_boxed_type_register_static ("MarshallUnion",
                                           (GBoxedCopyFunc)
                                           marshall_union_copy,
                                           (GBoxedFreeFunc) marshall_union_free);
    }

  return type;
}

/**
 * marshall_union_returnv:
 *
 * Returns: (transfer none):
 */
MarshallUnion *
marshall_union_returnv (void)
{
  static MarshallUnion *union_ = NULL;

  if (union_ == NULL)
    {
      union_ = g_new (MarshallUnion, 1);

      union_->long_ = 42;
    }

  return union_;
}

/**
 * marshall_union_inv:
 * @union_: (transfer none):
 */
void
marshall_union_inv (MarshallUnion *union_)
{
  g_assert_cmpint (union_->long_, ==, 42);
}

void
marshall_union_method (MarshallUnion *union_)
{
  g_assert_cmpint (union_->long_, ==, 42);
}



enum
{
  PROP_0,
  PROP_INT_
};

static void
  marshall_object_real_method_with_default_implementation (MarshallObject *self, gint8 in);

G_DEFINE_TYPE (MarshallObject, marshall_object, G_TYPE_OBJECT);

static void
marshall_object_init (MarshallObject *object)
{
}

static void
marshall_object_finalize (GObject *object)
{
  G_OBJECT_CLASS (marshall_object_parent_class)->finalize (object);
}

static void
marshall_object_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
  g_return_if_fail (MARSHALL_IS_OBJECT (object));

  switch (prop_id)
    {
    case PROP_INT_:
      MARSHALL_OBJECT (object)->int_ = g_value_get_int (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
marshall_object_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
  g_return_if_fail (MARSHALL_IS_OBJECT (object));

  switch (prop_id)
    {
    case PROP_INT_:
      g_value_set_int (value, MARSHALL_OBJECT (object)->int_);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
marshall_object_class_init (MarshallObjectClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
#if 0
  GObjectClass *parent_class = G_OBJECT_CLASS (klass);
#endif

  object_class->finalize = marshall_object_finalize;
  object_class->set_property = marshall_object_set_property;
  object_class->get_property = marshall_object_get_property;

  g_object_class_install_property (object_class, PROP_INT_,
                                   g_param_spec_int ("int", "Integer",
                                                     "An integer", G_MININT,
                                                     G_MAXINT, 0,
                                                     G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  klass->method_with_default_implementation = marshall_object_real_method_with_default_implementation;
}


void
marshall_object_static_method (void)
{
}

void
marshall_object_method (MarshallObject *object)
{
  g_return_if_fail (MARSHALL_IS_OBJECT (object));
  g_assert_cmpint (object->int_, ==, 42);
}

void
marshall_object_overridden_method (MarshallObject *object)
{
  g_return_if_fail (MARSHALL_IS_OBJECT (object));
  g_assert_cmpint (object->int_, ==, 0);
}

MarshallObject *
marshall_object_new (gint int_)
{
  return g_object_new (MARSHALL_TYPE_OBJECT, "int", int_, NULL);
}

MarshallObject *
marshall_object_new_fail (gint int_, GError **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, NULL);

  g_set_error_literal (error,
                       g_quark_from_static_string (MARSHALL_CONSTANT_GERROR_DOMAIN),
                       MARSHALL_CONSTANT_GERROR_CODE,
                       MARSHALL_CONSTANT_GERROR_MESSAGE);

  return NULL;
}

/**
 * marshall_object_method_array_in:
 * @ints: (array length=length):
 */
void
marshall_object_method_array_in (MarshallObject *object, const gint *ints, gint length)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);
}

/**
 * marshall_object_method_array_out:
 * @ints: (out) (array length=length) (transfer none):
 */
void
marshall_object_method_array_out (MarshallObject *object, gint **ints, gint *length)
{
  static gint values[] = { -1, 0, 1, 2 };

  *length = 4;
  *ints = values;
}

/**
 * marshall_object_method_array_inout:
 * @ints: (inout) (array length=length) (transfer none):
 * @length: (inout):
 */
void
marshall_object_method_array_inout (MarshallObject *object, gint **ints, gint *length)
{
  static gint values[] = { -2, -1, 0, 1, 2 };

  g_assert_cmpint (*length, ==, 4);
  g_assert_cmpint ((*ints)[0], ==, -1);
  g_assert_cmpint ((*ints)[1], ==, 0);
  g_assert_cmpint ((*ints)[2], ==, 1);
  g_assert_cmpint ((*ints)[3], ==, 2);

  *length = 5;
  *ints = values;
}

/**
 * marshall_object_method_array_return:
 *
 * Returns: (array length=length):
 */
const gint *
marshall_object_method_array_return (MarshallObject *object, gint *length)
{
  static gint ints[] = { -1, 0, 1, 2 };

  *length = 4;
  return ints;
}

/**
 * marshall_object_method_int8_in:
 * @in: (in):
 */
void
marshall_object_method_int8_in (MarshallObject *self, gint8 in)
{
  MARSHALL_OBJECT_GET_CLASS (self)->method_int8_in (self, in);
}

/**
 * marshall_object_method_int8_out:
 * @out: (out):
 */
void
marshall_object_method_int8_out (MarshallObject *self, gint8 *out)
{
  MARSHALL_OBJECT_GET_CLASS (self)->method_int8_out (self, out);
}

/**
 * marshall_object_method_int8_arg_and_out_caller:
 * @out: (out caller-allocates):
 */
void
  marshall_object_method_int8_arg_and_out_caller (MarshallObject *self, gint8 arg, gint8 *out)
{
  MARSHALL_OBJECT_GET_CLASS (self)->method_int8_arg_and_out_caller (self, arg, out);
}

/**
 * marshall_object_method_int8_arg_and_out_callee:
 * @out: (out):
 */
void
  marshall_object_method_int8_arg_and_out_callee (MarshallObject *self, gint8 arg, gint8 **out)
{
  MARSHALL_OBJECT_GET_CLASS (self)->method_int8_arg_and_out_callee (self, arg, out);
}

/**
 * marshall_object_method_str_arg_out_ret:
 * @out: (out caller-allocates):
 *
 * Returns: (transfer none)
 */
const gchar *
marshall_object_method_str_arg_out_ret (MarshallObject *self, const gchar *arg, guint *out)
{
  return MARSHALL_OBJECT_GET_CLASS (self)->method_str_arg_out_ret (self, arg, out);
}

/**
 * marshall_object_method_with_default_implementation:
 * @in: (in):
 */
void marshall_object_method_with_default_implementation (MarshallObject *self, gint8 in)
{
  MARSHALL_OBJECT_GET_CLASS (self)->method_with_default_implementation (self, in);
}

static void
  marshall_object_real_method_with_default_implementation (MarshallObject *self, gint8 in)
{
  GValue val = { 0, };
  g_value_init (&val, G_TYPE_INT);
  g_value_set_int (&val, in);
  g_object_set_property (G_OBJECT (self), "int", &val);
}

/**
 * marshall_object_vfunc_with_callback: (virtual vfunc_with_callback)
 * @callback: (scope call) (closure callback_data):
 * @callback_data: (allow-none):
 */
void
marshall_object_vfunc_with_callback (MarshallObject *
                                                 object, MarshallCallbackIntInt callback, void *callback_data)
{

}

static int
_callback (int val, void *user_data)
{
  g_assert (user_data == (gpointer) 0xdeadbeef);
  return val;
}

void
marshall_object_call_vfunc_with_callback (MarshallObject *object)
{
  MARSHALL_OBJECT_GET_CLASS (object)->vfunc_with_callback (object, _callback, (void *) 0xdeadbeef);
}

/**
 * marshall_object_none_return:
 *
 * Returns: (transfer none):
 */
MarshallObject *
marshall_object_none_return (void)
{
  static MarshallObject *object = NULL;

  if (object == NULL)
    {
      object = g_object_new (MARSHALL_TYPE_OBJECT, NULL);
    }

  return object;
}

/**
 * marshall_object_full_return:
 *
 * Returns: (transfer full):
 */
MarshallObject *
marshall_object_full_return (void)
{
  return g_object_new (MARSHALL_TYPE_OBJECT, NULL);
}

/**
 * marshall_object_none_in:
 * @object: (transfer none):
 */
void
marshall_object_none_in (MarshallObject *object)
{
  g_assert_cmpint (object->int_, ==, 42);
}

/**
 * marshall_object_none_out:
 * @object: (out) (transfer none):
 */
void
marshall_object_none_out (MarshallObject **object)
{
  static MarshallObject *new_object = NULL;

  if (new_object == NULL)
    {
      new_object = g_object_new (MARSHALL_TYPE_OBJECT, NULL);
    }

  *object = new_object;
}

/**
 * marshall_object_full_out:
 * @object: (out) (transfer full):
 */
void
marshall_object_full_out (MarshallObject **object)
{
  *object = g_object_new (MARSHALL_TYPE_OBJECT, NULL);
}

/**
 * marshall_object_none_inout:
 * @object: (inout) (transfer none):
 */
void
marshall_object_none_inout (MarshallObject **object)
{
  static MarshallObject *new_object = NULL;

  g_assert_cmpint ((*object)->int_, ==, 42);

  if (new_object == NULL)
    {
      new_object = g_object_new (MARSHALL_TYPE_OBJECT, NULL);
      new_object->int_ = 0;
    }

  *object = new_object;
}

/**
 * marshall_object_full_inout:
 * @object: (inout) (transfer full):
 */
void
marshall_object_full_inout (MarshallObject **object)
{
  g_assert_cmpint ((*object)->int_, ==, 42);

  g_object_unref (*object);
  *object = g_object_new (MARSHALL_TYPE_OBJECT, NULL);
}

/**
 * marshall_object_int8_in:
 * @in: (in):
 */
void
marshall_object_int8_in (MarshallObject *object, gint8 in)
{
  marshall_object_method_int8_in (object, in);
}

/**
 * marshall_object_int8_out:
 * @out: (out):
 */
void
marshall_object_int8_out (MarshallObject *object, gint8 *out)
{
  marshall_object_method_int8_out (object, out);
}

/**
 * marshall_object_vfunc_return_value_only:
 */
glong
marshall_object_vfunc_return_value_only (MarshallObject *self)
{
  /* make sure that local variables don't get smashed */
  glong return_value;
  gulong local = 0x12345678;
  return_value = MARSHALL_OBJECT_GET_CLASS (self)->vfunc_return_value_only (self);
  g_assert_cmpint (local, ==, 0x12345678);
  return return_value;
}

/**
 * marshall_object_vfunc_one_out_parameter:
 * @a: (out):
 */
void
marshall_object_vfunc_one_out_parameter (MarshallObject *self, gfloat *a)
{
  /* make sure that local variables don't get smashed */
  gulong local = 0x12345678;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_one_out_parameter (self, a);
  g_assert_cmpint (local, ==, 0x12345678);
}

/**
 * marshall_object_vfunc_multiple_out_parameters:
 * @a: (out):
 * @b: (out):
 */
void marshall_object_vfunc_multiple_out_parameters (MarshallObject *self, gfloat *a, gfloat *b)
{
  /* make sure that local variables don't get smashed */
  gulong local = 0x12345678;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_multiple_out_parameters (self, a, b);
  g_assert_cmpint (local, ==, 0x12345678);
}

/**
 * marshall_object_vfunc_caller_allocated_out_parameter:
 * @a: (out):
 */
void marshall_object_vfunc_caller_allocated_out_parameter (MarshallObject *self, GValue *a)
{
  /* make sure that local variables don't get smashed */
  gulong local = 0x12345678;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_caller_allocated_out_parameter (self, a);
  g_assert_cmpint (local, ==, 0x12345678);
}

/**
 * marshall_object_vfunc_array_out_parameter:
 * @a: (out) (array zero-terminated):
 */
void marshall_object_vfunc_array_out_parameter (MarshallObject *self, gfloat **a)
{
  /* make sure that local variables don't get smashed */
  gulong local = 0x12345678;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_array_out_parameter (self, a);
  g_assert_cmpint (local, ==, 0x12345678);
}

/**
 * marshall_object_vfunc_return_value_and_one_out_parameter:
 * @a: (out):
 */
glong marshall_object_vfunc_return_value_and_one_out_parameter (MarshallObject *self, glong *a)
{
  /* make sure that local variables don't get smashed */
  gulong return_value;
  gulong local = 0x12345678;
  return_value = MARSHALL_OBJECT_GET_CLASS (self)->vfunc_return_value_and_one_out_parameter (self, a);
  g_assert_cmpint (local, ==, 0x12345678);
  return return_value;
}

/**
 * marshall_object_vfunc_return_value_and_multiple_out_parameters:
 * @a: (out):
 * @b: (out):
 */
glong
  marshall_object_vfunc_return_value_and_multiple_out_parameters
  (MarshallObject *self, glong *a, glong *b)
{
  gulong return_value;
  gulong local = 0x12345678;
  return_value =
    MARSHALL_OBJECT_GET_CLASS (self)->vfunc_return_value_and_multiple_out_parameters (self, a, b);
  g_assert_cmpint (local, ==, 0x12345678);
  return return_value;
}

/**
 * marshall_callback_owned_boxed:
 * @callback: (scope call) (closure callback_data):
 * @callback_data: (allow-none):
 */
glong
marshall_callback_owned_boxed (MarshallCallbackOwnedBoxed callback,
                                           void *callback_data)
{
  static MarshallBoxedStruct *box = NULL;
  glong ret;

  if (!box)
    box = marshall_boxed_struct_new ();
  box->long_++;
  callback (box, callback_data);
  ret = box->long_;
  return ret;
}

gboolean
marshall_object_vfunc_meth_with_error (MarshallObject *self, gint x, GError **error)
{
  gulong local = 0x12345678;
  gboolean ret = MARSHALL_OBJECT_GET_CLASS (self)->vfunc_meth_with_err (self,
                                                                                    x,
                                                                                    error);
  g_assert_cmpint (local, ==, 0x12345678);
  return ret;
}

/**
 * marshall_object_vfunc_return_enum:
 */
MarshallEnum
marshall_object_vfunc_return_enum (MarshallObject *self)
{
  /* make sure that local variables don't get smashed */
  MarshallEnum return_value;
  glong local = 0x12345678;
  return_value = MARSHALL_OBJECT_GET_CLASS (self)->vfunc_return_enum (self);
  g_assert_cmpint (local, ==, 0x12345678);
  return return_value;
}

/**
 * marshall_object_vfunc_out_enum:
 * @_enum: (out):
 */
void
marshall_object_vfunc_out_enum (MarshallObject *self, MarshallEnum *_enum)
{
  /* make sure that local variables don't get smashed */
  gulong local = 0x12345678;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_out_enum (self, _enum);
  g_assert_cmpint (local, ==, 0x12345678);
}


/* NOTE:
 *
 * The following (get_ref_info_for_*) methods are designed to call vfuncs related
 * to object argument marshaling. They do not pass the resulting objects through them
 * as regular vfunc wrapper method do, but rather return reference count and floating
 * information back to the callers. This is useful because callers can do testing of
 * expected reference counts in isolation and from the perspective of C. This is important
 * because if there are bugs in the reverse marshaling, they can obfuscate or compound
 * bugs in marshaling from the vfuncs.
 */

/**
 * marshall_object_get_ref_info_for_vfunc_return_object_transfer_none:
 * @ref_count: (out): Ref count of the object returned from the vfunc directly after vfunc call.
 * @is_floating: (out): Floating state object returned from the vfunc directly after vfunc call.
 */
void
  marshall_object_get_ref_info_for_vfunc_return_object_transfer_none
  (MarshallObject *self, guint *ref_count, gboolean *is_floating)
{
  GObject *object = MARSHALL_OBJECT_GET_CLASS (self)->vfunc_return_object_transfer_none (self);
  *ref_count = object->ref_count;
  *is_floating = g_object_is_floating (object);

  /* Attempt to sink and unref the returned object and avoid any potential leaks */
  g_object_ref_sink (object);
  g_object_unref (object);
}

/**
 * marshall_object_get_ref_info_for_vfunc_return_object_transfer_full:
 * @ref_count: (out): Ref count of the object returned from the vfunc directly after vfunc call.
 * @is_floating: (out): Floating state object returned from the vfunc directly after vfunc call.
 */
void
  marshall_object_get_ref_info_for_vfunc_return_object_transfer_full
  (MarshallObject *self, guint *ref_count, gboolean *is_floating)
{
  GObject *object = MARSHALL_OBJECT_GET_CLASS (self)->vfunc_return_object_transfer_full (self);
  *ref_count = object->ref_count;
  *is_floating = g_object_is_floating (object);
  g_object_unref (object);
}

/**
 * marshall_object_get_ref_info_for_vfunc_out_object_transfer_none:
 * @ref_count: (out): Ref count of the object returned from the vfunc directly after vfunc call.
 * @is_floating: (out): Floating state object returned from the vfunc directly after vfunc call.
 */
void
  marshall_object_get_ref_info_for_vfunc_out_object_transfer_none
  (MarshallObject *self, guint *ref_count, gboolean *is_floating)
{
  GObject *object = NULL;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_out_object_transfer_none (self, &object);
  *ref_count = object->ref_count;
  *is_floating = g_object_is_floating (object);

  /* Attempt to sink and unref the returned object and avoid any potential leaks */
  g_object_ref_sink (object);
  g_object_unref (object);
}

/**
 * marshall_object_get_ref_info_for_vfunc_out_object_transfer_full:
 * @ref_count: (out): Ref count of the object returned from the vfunc directly after vfunc call.
 * @is_floating: (out): Floating state object returned from the vfunc directly after vfunc call.
 */
void
  marshall_object_get_ref_info_for_vfunc_out_object_transfer_full
  (MarshallObject *self, guint *ref_count, gboolean *is_floating)
{
  GObject *object = NULL;
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_out_object_transfer_full (self, &object);
  *ref_count = object->ref_count;
  *is_floating = g_object_is_floating (object);
  g_object_unref (object);
}

static void
_vfunc_in_object_destroy_callback (gboolean *destroy_called, GObject *where_the_object_was)
{
  *destroy_called = TRUE;
}

/**
 * marshall_object_get_ref_info_for_vfunc_in_object_transfer_none:
 * @type: GType of object to create and pass as in argument to the vfunc
 * @ref_count: (out): Ref count of the in object directly after vfunc call.
 * @is_floating: (out): Floating state of in object directly after vfunc call.
 *
 * Calls vfunc_in_object_transfer_none with a new object of the given type.
 */
void
  marshall_object_get_ref_info_for_vfunc_in_object_transfer_none
  (MarshallObject *self, GType type, guint *ref_count, gboolean *is_floating)
{
  static gboolean destroy_called;
  GObject *object;
  destroy_called = FALSE;

  object = g_object_new (type, NULL);
  g_object_weak_ref (object, (GWeakNotify) _vfunc_in_object_destroy_callback, &destroy_called);

  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_in_object_transfer_none (self, object);
  if (destroy_called)
    {
      *ref_count = 0;
      *is_floating = FALSE;
    }
  else
    {
      *ref_count = object->ref_count;
      *is_floating = g_object_is_floating (object);
      g_object_unref (object);
    }
}


/**
 * marshall_object_get_ref_info_for_vfunc_in_object_transfer_full:
 * @type: GType of object to create and pass as in argument to the vfunc
 * @ref_count: (out): Ref count of the in object directly after vfunc call.
 * @is_floating: (out): Floating state of in object directly after vfunc call.
 */
void
  marshall_object_get_ref_info_for_vfunc_in_object_transfer_full
  (MarshallObject *self, GType type, guint *ref_count, gboolean *is_floating)
{
  static gboolean destroy_called;
  GObject *object;
  destroy_called = FALSE;

  object = g_object_new (type, NULL);
  g_object_weak_ref (object, (GWeakNotify) _vfunc_in_object_destroy_callback, &destroy_called);

  /* Calling the vfunc takes ownership of the object, so we use a weak_ref to determine
   * if the object gets destroyed after the call and appropriately return 0 as the ref count.
   */
  MARSHALL_OBJECT_GET_CLASS (self)->vfunc_in_object_transfer_full (self, object);
  if (destroy_called)
    {
      *ref_count = 0;
      *is_floating = FALSE;
    }
  else
    {
      *ref_count = object->ref_count;
      *is_floating = g_object_is_floating (object);
    }
}


G_DEFINE_TYPE (MarshallSubObject, marshall_sub_object, MARSHALL_TYPE_OBJECT);

static void
marshall_sub_object_init (MarshallSubObject *object)
{
}

static void
marshall_sub_object_finalize (GObject *object)
{
  G_OBJECT_CLASS (marshall_sub_object_parent_class)->finalize (object);
}

static void
method_deep_hierarchy (MarshallObject *self, gint8 in)
{
  GValue val = { 0, };
  g_value_init (&val, G_TYPE_INT);
  g_value_set_int (&val, in);
  g_object_set_property (G_OBJECT (self), "int", &val);
}

static void
marshall_sub_object_class_init (MarshallSubObjectClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = marshall_sub_object_finalize;
  MARSHALL_OBJECT_CLASS (klass)->method_deep_hierarchy = method_deep_hierarchy;
}

void
marshall_sub_object_sub_method (MarshallSubObject *object)
{
  g_assert_cmpint (MARSHALL_OBJECT (object)->int_, ==, 0);
}

void marshall_sub_object_overwritten_method (MarshallSubObject *object)
{
  g_assert_cmpint (MARSHALL_OBJECT (object)->int_, ==, 0);
}

G_DEFINE_TYPE (MarshallSubSubObject,
               marshall_sub_sub_object, MARSHALL_TYPE_SUB_OBJECT);

static void
marshall_sub_sub_object_init (MarshallSubSubObject *object)
{
}

static void marshall_sub_sub_object_class_init (MarshallSubSubObjectClass *klass)
{
}

/* Interfaces */

static void
marshall_interface_class_init (void *g_iface, void *data)
{
}

GType
marshall_interface_get_type (void)
{
  static GType type = 0;
  if (type == 0)
    {
      /* Not adding prerequisite here for test purposes */
      type = g_type_register_static_simple (G_TYPE_INTERFACE,
                                            "MarshallInterface",
                                            sizeof
                                            (MarshallInterfaceIface),
                                            (GClassInitFunc) marshall_interface_class_init, 0, NULL, 0);
    }

  return type;
}

/**
 * marshall_interface_test_int8_in:
 * @in: (in):
 */
void
marshall_interface_test_int8_in (MarshallInterface *self, gint8 in)
{
  MARSHALL_INTERFACE_GET_IFACE (self)->test_int8_in (self, in);
}

/**
 * marshall_test_interface_test_int8_in:
 * @in: (in):
 */
void
marshall_test_interface_test_int8_in (MarshallInterface *test_iface, gint8 in)
{
  marshall_interface_test_int8_in (test_iface, in);
}


static void test_interface_init (MarshallInterfaceIface *iface);

G_DEFINE_TYPE_WITH_CODE (MarshallInterfaceImpl, marshall_interface_impl, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE(MARSHALL_TYPE_INTERFACE, test_interface_init))

static void
marshall_interface_impl_test_int8_in (MarshallInterface *self, gint8 in)
{
}

static void test_interface_init (MarshallInterfaceIface *iface)
{
  iface->test_int8_in = marshall_interface_impl_test_int8_in;
}

static void
marshall_interface_impl_init (MarshallInterfaceImpl *object)
{
}

static void
marshall_interface_impl_class_init (MarshallInterfaceImplClass *klass)
{
}

/**
 * marshall_interface_impl_get_as_interface:
 *
 * Returns: (transfer none):
 */
MarshallInterface *
marshall_interface_impl_get_as_interface (MarshallInterfaceImpl *self)
{
  return (MarshallInterface *) self;
}

static void
marshall_interface2_class_init (void *g_iface, void *data)
{
}

GType
marshall_interface2_get_type (void)
{
  static GType type = 0;
  if (type == 0)
    {
      type = g_type_register_static_simple (G_TYPE_INTERFACE,
                                            "MarshallInterface2",
                                            sizeof
                                            (MarshallInterface2Iface),
                                            (GClassInitFunc) marshall_interface2_class_init, 0, NULL, 0);
    }

  return type;
}

static void
marshall_interface3_class_init (void *g_iface, void *data)
{
}

GType
marshall_interface3_get_type (void)
{
  static GType type = 0;
  if (type == 0)
    {
      type = g_type_register_static_simple (G_TYPE_INTERFACE,
                                            "MarshallInterface3",
                                            sizeof
                                            (MarshallInterface3Iface),
                                            (GClassInitFunc) marshall_interface3_class_init, 0, NULL, 0);
    }

  return type;
}

/**
 * marshall_interface3_test_variant_array_in:
 * @in: (array length=n_in):
 * @n_in:
 */
void
  marshall_interface3_test_variant_array_in
  (MarshallInterface3 *self, GVariant **in, gsize n_in)
{
  MARSHALL_INTERFACE3_GET_IFACE (self)->test_variant_array_in (self, in, n_in);
}

/**
 * marshall_int_out_out:
 * @int0: (out):
 * @int1: (out):
 */
void
marshall_int_out_out (gint *int0, gint *int1)
{
  *int0 = 6;
  *int1 = 7;
}

/**
 * marshall_int_three_in_three_out:
 * @a: (in):
 * @b: (in):
 * @c: (in):
 * @out0: (out):
 * @out1: (out):
 * @out2: (out):
 */
void
marshall_int_three_in_three_out (gint a, gint b, gint c, gint *out0, gint *out1, gint *out2)
{
  *out0 = a;
  *out1 = b;
  *out2 = c;
}

/**
 * marshall_int_return_out:
 * @int_: (out):
 */
gint
marshall_int_return_out (gint *int_)
{
  *int_ = 7;
  return 6;
}

/**
* marshall_int_two_in_utf8_two_in_with_allow_none:
* @a: (in): Must be 1
* @b: (in): Must be 2
* @c: (in) (allow-none): Must be "3" or NULL
* @d: (in) (allow-none): Must be "4" or NULL
*/
void
marshall_int_two_in_utf8_two_in_with_allow_none (gint a, gint b, const gchar *c, const gchar *d)
{
    g_assert_cmpint (a, ==, 1);
    g_assert_cmpint (b, ==, 2);
    if (c != NULL)
        g_assert_cmpstr (c, ==, "3");
    if (d != NULL)
        g_assert_cmpstr (d, ==, "4");
}

/**
* marshall_int_one_in_utf8_two_in_one_allows_none:
* @a: (in): Must be 1
* @b: (in) (allow-none): Must be "2" or NULL
* @c: (in): Must be "3"
*/
void
marshall_int_one_in_utf8_two_in_one_allows_none (gint a, const gchar *b, const gchar *c)
{
    g_assert_cmpint (a, ==, 1);
    if (b != NULL)
        g_assert_cmpstr (b, ==, "2");
    g_assert_cmpstr (c, ==, "3");
}

/**
 * marshall_array_in_utf8_two_in:
 * @ints: (array length=length):
 * @length:
 * @a: (in) (allow-none): Must be "1" or NULL
 * @b: (in) (allow-none): Must be "2" or NULL
 */
void
marshall_array_in_utf8_two_in (const gint *ints, gint length, const gchar *a, const gchar *b)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);

  if (a != NULL)
      g_assert_cmpstr (a, ==, "1");
  if (b != NULL)
      g_assert_cmpstr (b, ==, "2");
}

/**
 * marshall_array_in_utf8_two_in_out_of_order:
 * @length:
 * @a: (in) (allow-none): Must be "1" or NULL
 * @ints: (array length=length):
 * @b: (in) (allow-none): Must be "2" or NULL
 */
void
marshall_array_in_utf8_two_in_out_of_order (gint length, const gchar *a, const gint *ints, const gchar *b)
{
  g_assert_cmpint (length, ==, 4);
  g_assert_cmpint (ints[0], ==, -1);
  g_assert_cmpint (ints[1], ==, 0);
  g_assert_cmpint (ints[2], ==, 1);
  g_assert_cmpint (ints[3], ==, 2);

  if (a != NULL)
      g_assert_cmpstr (a, ==, "1");
  if (b != NULL)
      g_assert_cmpstr (b, ==, "2");
}

/* GError */

void
marshall_gerror (GError **error)
{
  GQuark quark = g_quark_from_static_string (MARSHALL_CONSTANT_GERROR_DOMAIN);
  g_set_error_literal (error, quark,
                       MARSHALL_CONSTANT_GERROR_CODE, MARSHALL_CONSTANT_GERROR_MESSAGE);
}

/**
 * marshall_gerror_array_in:
 * @in_ints: (array zero-terminated):
 */
void
marshall_gerror_array_in (gint *in_ints, GError **error)
{
  GQuark quark = g_quark_from_static_string (MARSHALL_CONSTANT_GERROR_DOMAIN);
  g_set_error_literal (error, quark,
                       MARSHALL_CONSTANT_GERROR_CODE, MARSHALL_CONSTANT_GERROR_MESSAGE);
}

/**
 * marshall_gerror_out:
 * @error: (out) (allow-none) (transfer full): location for the GError.
 * @debug: (out) (allow-none) (transfer full): location for the debug message
 *
 * Inspired by gst_message_parse_error.
 */
void
marshall_gerror_out (GError **error, gchar **debug)
{
  GQuark quark = g_quark_from_static_string (MARSHALL_CONSTANT_GERROR_DOMAIN);
  g_set_error_literal (error, quark,
                       MARSHALL_CONSTANT_GERROR_CODE, MARSHALL_CONSTANT_GERROR_MESSAGE);

  if (debug != NULL)
    {
      *debug = g_strdup (MARSHALL_CONSTANT_GERROR_DEBUG_MESSAGE);
    }
}

/**
 * marshall_gerror_out_transfer_none:
 * @err: (out) (allow-none) (transfer none): location for the GError.
 * @debug: (out) (allow-none) (transfer none): location for the debug message
 *
 * A variant of marshall_gerror_out() which returns data the caller
 * must not free.
 */
void
marshall_gerror_out_transfer_none (GError **err, const gchar **debug)
{
  static GError error = { 0,
    MARSHALL_CONSTANT_GERROR_CODE,
    (gchar *) MARSHALL_CONSTANT_GERROR_MESSAGE
  };
  error.domain = g_quark_from_static_string (MARSHALL_CONSTANT_GERROR_DOMAIN);
  *err = &error;
  *debug = MARSHALL_CONSTANT_GERROR_DEBUG_MESSAGE;
}

/**
 * marshall_gerror_return:
 *
 * Yet another variant of marshall_gerror_out().
 *
 * Returns: (transfer full): a GError
 */
GError *
marshall_gerror_return (void)
{
  GQuark quark = g_quark_from_static_string (MARSHALL_CONSTANT_GERROR_DOMAIN);

  return g_error_new_literal (quark, MARSHALL_CONSTANT_GERROR_CODE, MARSHALL_CONSTANT_GERROR_MESSAGE);
}

static MarshallOverridesStruct *
marshall_overrides_struct_copy (MarshallOverridesStruct *struct_)
{
  MarshallOverridesStruct *new_struct;

  new_struct = g_slice_new (MarshallOverridesStruct);

  *new_struct = *struct_;

  return new_struct;
}

static void
marshall_overrides_struct_free (MarshallOverridesStruct *struct_)
{
  g_slice_free (MarshallOverridesStruct, struct_);
}

GType
marshall_overrides_struct_get_type (void)
{
  static GType type = 0;

  if (type == 0)
    {
      type =
        g_boxed_type_register_static ("MarshallOverridesStruct",
                                      (GBoxedCopyFunc)
                                      marshall_overrides_struct_copy,
                                      (GBoxedFreeFunc) marshall_overrides_struct_free);
    }

  return type;
}

MarshallOverridesStruct *
marshall_overrides_struct_new (void)
{
  return g_slice_new (MarshallOverridesStruct);
}

glong marshall_overrides_struct_method (MarshallOverridesStruct *struct_)
{
  return 42;
}


/**
 * marshall_overrides_struct_returnv:
 *
 * Returns: (transfer full):
 */
MarshallOverridesStruct *
marshall_overrides_struct_returnv (void)
{
  return marshall_overrides_struct_new ();
}


G_DEFINE_TYPE (MarshallOverridesObject, marshall_overrides_object, G_TYPE_OBJECT);

static void
marshall_overrides_object_init (MarshallOverridesObject *object)
{
}

static void
marshall_overrides_object_finalize (GObject *object)
{
  G_OBJECT_CLASS (marshall_overrides_object_parent_class)->finalize (object);
}

static void marshall_overrides_object_class_init (MarshallOverridesObjectClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
#if 0
  GObjectClass *parent_class = G_OBJECT_CLASS (klass);
#endif

  object_class->finalize = marshall_overrides_object_finalize;
}

MarshallOverridesObject *
marshall_overrides_object_new (void)
{
  return g_object_new (MARSHALL_TYPE_OVERRIDES_OBJECT, NULL);
}

glong marshall_overrides_object_method (MarshallOverridesObject *object)
{
  return 42;
}

/**
 * marshall_overrides_object_returnv:
 *
 * Returns: (transfer full):
 */
MarshallOverridesObject *
marshall_overrides_object_returnv (void)
{
  return g_object_new (MARSHALL_TYPE_OVERRIDES_OBJECT, NULL);
}

/**
 * marshall_filename_list_return:
 *
 * Returns: (transfer none) (element-type filename): List of filenames
 */
GSList *
marshall_filename_list_return (void)
{
  return NULL;
}

/**
 * marshall_param_spec_in_bool:
 */
void
marshall_param_spec_in_bool (const GParamSpec *param)
{
  g_assert (G_IS_PARAM_SPEC (param));
  g_assert_cmpint (G_PARAM_SPEC_VALUE_TYPE (param), ==, G_TYPE_BOOLEAN);
  g_assert_cmpstr (g_param_spec_get_name ((GParamSpec *) param), ==, "mybool");
}

/**
 * marshall_param_spec_return:
 *
 * Returns: (transfer full): a #GParamSpec
 */
GParamSpec *
marshall_param_spec_return (void)
{
  return g_param_spec_string ("test-param", "test", "This is a test", "42", G_PARAM_READABLE);
}

/**
 * marshall_param_spec_out:
 * @param: (out):
 */
void
marshall_param_spec_out (GParamSpec **param)
{
  *param = g_param_spec_string ("test-param", "test", "This is a test", "42", G_PARAM_READABLE);
}


enum
{
  DUMMY_PROPERTY,
  SOME_BOOLEAN_PROPERTY,
  SOME_CHAR_PROPERTY,
  SOME_UCHAR_PROPERTY,
  SOME_INT_PROPERTY,
  SOME_UINT_PROPERTY,
  SOME_LONG_PROPERTY,
  SOME_ULONG_PROPERTY,
  SOME_INT64_PROPERTY,
  SOME_UINT64_PROPERTY,
  SOME_FLOAT_PROPERTY,
  SOME_DOUBLE_PROPERTY,
  SOME_STRV_PROPERTY,
  SOME_BOXED_STRUCT_PROPERTY,
  SOME_VARIANT_PROPERTY,
  SOME_BOXED_GLIST_PROPERTY,
  SOME_GVALUE_PROPERTY,
  SOME_OBJECT_PROPERTY,
  SOME_FLAGS_PROPERTY,
  SOME_ENUM_PROPERTY,
  SOME_BYTE_ARRAY_PROPERTY,
  SOME_READONLY_PROPERTY,
};

G_DEFINE_TYPE (MarshallPropertiesObject, marshall_properties_object, G_TYPE_OBJECT);

static void marshall_properties_object_init (MarshallPropertiesObject *self)
{
}

static void
marshall_properties_object_finalize (GObject *obj)
{
  MarshallPropertiesObject *self;
  self = MARSHALL_PROPERTIES_OBJECT (obj);

  if (self->some_gvalue) {
    g_boxed_free (G_TYPE_VALUE, self->some_gvalue);
    self->some_gvalue = NULL;
  }

  g_clear_pointer (&self->some_strv, g_strfreev);
  g_clear_pointer (&self->some_boxed_struct, marshall_boxed_struct_free);
  g_clear_pointer (&self->some_variant, g_variant_unref);
  g_clear_pointer (&self->some_boxed_glist, g_list_free);
  g_clear_object (&self->some_object);

  G_OBJECT_CLASS (marshall_properties_object_parent_class)->finalize (obj);
}

static void
marshall_properties_object_get_property (GObject *object,
                                                     guint property_id, GValue *value, GParamSpec *pspec)
{
  MarshallPropertiesObject *self;
  self = MARSHALL_PROPERTIES_OBJECT (object);
  switch (property_id)
    {
    case SOME_BOOLEAN_PROPERTY:
      g_value_set_boolean (value, self->some_boolean);
      break;
    case SOME_CHAR_PROPERTY:
      g_value_set_schar (value, self->some_char);
      break;
    case SOME_UCHAR_PROPERTY:
      g_value_set_uchar (value, self->some_uchar);
      break;
    case SOME_INT_PROPERTY:
      g_value_set_int (value, self->some_int);
      break;
    case SOME_UINT_PROPERTY:
      g_value_set_uint (value, self->some_uint);
      break;
    case SOME_LONG_PROPERTY:
      g_value_set_long (value, self->some_long);
      break;
    case SOME_ULONG_PROPERTY:
      g_value_set_ulong (value, self->some_ulong);
      break;
    case SOME_INT64_PROPERTY:
      g_value_set_int64 (value, self->some_int64);
      break;
    case SOME_UINT64_PROPERTY:
      g_value_set_uint64 (value, self->some_uint64);
      break;
    case SOME_FLOAT_PROPERTY:
      g_value_set_float (value, self->some_float);
      break;
    case SOME_DOUBLE_PROPERTY:
      g_value_set_double (value, self->some_double);
      break;
    case SOME_STRV_PROPERTY:
      g_value_set_boxed (value, self->some_strv);
      break;
    case SOME_BOXED_STRUCT_PROPERTY:
      g_value_set_boxed (value, self->some_boxed_struct);
      break;
    case SOME_BOXED_GLIST_PROPERTY:
      g_value_set_boxed (value, self->some_boxed_glist);
      break;
    case SOME_GVALUE_PROPERTY:
      g_value_set_boxed (value, self->some_gvalue);
      break;
    case SOME_VARIANT_PROPERTY:
      g_value_set_variant (value, self->some_variant);
      break;
    case SOME_OBJECT_PROPERTY:
      g_value_set_object (value, self->some_object);
      break;
    case SOME_FLAGS_PROPERTY:
      g_value_set_flags (value, self->some_flags);
      break;
    case SOME_ENUM_PROPERTY:
      g_value_set_enum (value, self->some_enum);
      break;
    case SOME_BYTE_ARRAY_PROPERTY:
      g_value_set_boxed (value, self->some_byte_array);
      break;
    case SOME_READONLY_PROPERTY:
      g_value_set_int (value, 42);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
marshall_properties_object_set_property (GObject *object,
                                                     guint property_id, const GValue *value, GParamSpec *pspec)
{
  MarshallPropertiesObject *self;
  self = MARSHALL_PROPERTIES_OBJECT (object);
  switch (property_id)
    {
    case SOME_BOOLEAN_PROPERTY:
      self->some_boolean = g_value_get_boolean (value);
      break;
    case SOME_CHAR_PROPERTY:
      self->some_char = g_value_get_schar (value);
      break;
    case SOME_UCHAR_PROPERTY:
      self->some_uchar = g_value_get_uchar (value);
      break;
    case SOME_INT_PROPERTY:
      self->some_int = g_value_get_int (value);
      break;
    case SOME_UINT_PROPERTY:
      self->some_uint = g_value_get_uint (value);
      break;
    case SOME_LONG_PROPERTY:
      self->some_long = g_value_get_long (value);
      break;
    case SOME_ULONG_PROPERTY:
      self->some_ulong = g_value_get_ulong (value);
      break;
    case SOME_INT64_PROPERTY:
      self->some_int64 = g_value_get_int64 (value);
      break;
    case SOME_UINT64_PROPERTY:
      self->some_uint64 = g_value_get_uint64 (value);
      break;
    case SOME_FLOAT_PROPERTY:
      self->some_float = g_value_get_float (value);
      break;
    case SOME_DOUBLE_PROPERTY:
      self->some_double = g_value_get_double (value);
      break;
    case SOME_STRV_PROPERTY:
      g_strfreev (self->some_strv);
      self->some_strv = g_strdupv (g_value_get_boxed (value));
      break;
    case SOME_BOXED_STRUCT_PROPERTY:
      marshall_boxed_struct_free (self->some_boxed_struct);
      self->some_boxed_struct = marshall_boxed_struct_copy (g_value_get_boxed (value));
      break;
    case SOME_BOXED_GLIST_PROPERTY:
      g_list_free (self->some_boxed_glist);
      self->some_boxed_glist = g_list_copy (g_value_get_boxed (value));
      break;
    case SOME_GVALUE_PROPERTY:
      if (self->some_gvalue)
        g_boxed_free (G_TYPE_VALUE, self->some_gvalue);
      self->some_gvalue = g_value_dup_boxed (value);
      break;
    case SOME_VARIANT_PROPERTY:
      if (self->some_variant != NULL)
        g_variant_unref (self->some_variant);
      self->some_variant = g_value_get_variant (value);
      if (self->some_variant != NULL)
        g_variant_ref (self->some_variant);
      break;
    case SOME_OBJECT_PROPERTY:
      if (self->some_object != NULL)
        g_object_unref (self->some_object);
      self->some_object = g_value_dup_object (value);
      break;
    case SOME_FLAGS_PROPERTY:
      self->some_flags = g_value_get_flags (value);
      break;
    case SOME_ENUM_PROPERTY:
      self->some_enum = g_value_get_enum (value);
      break;
    case SOME_BYTE_ARRAY_PROPERTY:
      if (self->some_byte_array != NULL)
        g_byte_array_unref (self->some_byte_array);
      self->some_byte_array = g_value_dup_boxed (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void marshall_properties_object_class_init (MarshallPropertiesObjectClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = marshall_properties_object_finalize;
  object_class->get_property = marshall_properties_object_get_property;
  object_class->set_property = marshall_properties_object_set_property;

  g_object_class_install_property (object_class, SOME_BOOLEAN_PROPERTY,
                                   g_param_spec_boolean ("some-boolean",
                                                         "some-boolean",
                                                         "some-boolean",
                                                         FALSE,
                                                         G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_CHAR_PROPERTY,
                                   g_param_spec_char ("some-char",
                                                      "some-char",
                                                      "some-char", G_MININT8,
                                                      G_MAXINT8, 0,
                                                      G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_UCHAR_PROPERTY,
                                   g_param_spec_uchar ("some-uchar",
                                                       "some-uchar",
                                                       "some-uchar", 0,
                                                       G_MAXUINT8, 0,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_INT_PROPERTY,
                                   g_param_spec_int ("some-int", "some-int",
                                                     "some-int", G_MININT,
                                                     G_MAXINT, 0,
                                                     G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_UINT_PROPERTY,
                                   g_param_spec_uint ("some-uint",
                                                      "some-uint",
                                                      "some-uint", 0,
                                                      G_MAXUINT, 0,
                                                      G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_LONG_PROPERTY,
                                   g_param_spec_long ("some-long",
                                                      "some-long",
                                                      "some-long", G_MINLONG,
                                                      G_MAXLONG, 0,
                                                      G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_ULONG_PROPERTY,
                                   g_param_spec_ulong ("some-ulong",
                                                       "some-ulong",
                                                       "some-ulong", 0,
                                                       G_MAXULONG, 0,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_INT64_PROPERTY,
                                   g_param_spec_int64 ("some-int64",
                                                       "some-int64",
                                                       "some-int64",
                                                       G_MININT64, G_MAXINT64,
                                                       0, G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_UINT64_PROPERTY,
                                   g_param_spec_uint64 ("some-uint64",
                                                        "some-uint64",
                                                        "some-uint64", 0,
                                                        G_MAXUINT64, 0,
                                                        G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_FLOAT_PROPERTY,
                                   g_param_spec_float ("some-float",
                                                       "some-float",
                                                       "some-float",
                                                       -1 * G_MAXFLOAT,
                                                       G_MAXFLOAT, 0,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_DOUBLE_PROPERTY,
                                   g_param_spec_double ("some-double",
                                                        "some-double",
                                                        "some-double",
                                                        -1 * G_MAXDOUBLE,
                                                        G_MAXDOUBLE, 0,
                                                        G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_STRV_PROPERTY,
                                   g_param_spec_boxed ("some-strv",
                                                       "some-strv",
                                                       "some-strv",
                                                       G_TYPE_STRV,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_BOXED_STRUCT_PROPERTY,
                                   g_param_spec_boxed ("some-boxed-struct",
                                                       "some-boxed-struct",
                                                       "some-boxed-struct",
                                                       marshall_boxed_struct_get_type
                                                       (), G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

    /**
     * MarshallPropertiesObject:some-boxed-glist: (type GLib.List(gint)) (transfer none):
     */
  g_object_class_install_property (object_class, SOME_BOXED_GLIST_PROPERTY,
                                   g_param_spec_boxed ("some-boxed-glist",
                                                       "some-boxed-glist",
                                                       "some-boxed-glist",
                                                       marshall_boxed_glist_get_type
                                                       (), G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_GVALUE_PROPERTY,
                                   g_param_spec_boxed ("some-gvalue",
                                                       "some-gvalue",
                                                       "some-gvalue",
                                                       G_TYPE_VALUE,
                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_VARIANT_PROPERTY,
                                   g_param_spec_variant ("some-variant",
                                                         "some-variant",
                                                         "some-variant",
                                                         G_VARIANT_TYPE_ANY,
                                                         NULL,
                                                         G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_OBJECT_PROPERTY,
                                   g_param_spec_object ("some-object",
                                                        "some-object",
                                                        "some-object",
                                                        G_TYPE_OBJECT,
                                                        G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_FLAGS_PROPERTY,
                                   g_param_spec_flags ("some-flags",
                                                       "some-flags",
                                                       "some-flags",
                                                       MARSHALL_TYPE_FLAGS,
                                                       MARSHALL_FLAGS_VALUE1,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_ENUM_PROPERTY,
                                   g_param_spec_enum ("some-enum",
                                                      "some-enum",
                                                      "some-enum",
                                                      MARSHALL_TYPE_GENUM,
                                                      MARSHALL_GENUM_VALUE1,
                                                      G_PARAM_READABLE | G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_BYTE_ARRAY_PROPERTY,
                                   g_param_spec_boxed ("some-byte-array",
                                                       "some-byte-array",
                                                       "some-byte-array",
                                                       G_TYPE_BYTE_ARRAY,
                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class, SOME_READONLY_PROPERTY,
                                   g_param_spec_int ("some-readonly",
                                                     "some-readonly",
                                                     "some-readonly",
                                                     G_MININT, G_MAXINT, 0,
                                                     G_PARAM_READABLE));
}

MarshallPropertiesObject *
marshall_properties_object_new (void)
{
  return g_object_new (MARSHALL_TYPE_PROPERTIES_OBJECT, NULL);
}
