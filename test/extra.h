#include <glib-object.h>

#ifndef __EXTRA_H__
#define __EXTRA_H__

#ifndef _GI_EXTERN
#define _GI_EXTERN extern
#endif

#define _GI_TEST_EXTERN _GI_EXTERN

/* Booleans */

_GI_TEST_EXTERN
gboolean
extra_zero_terminated_uint8_array_input (const guint8 *v);

_GI_TEST_EXTERN
gboolean
extra_zero_terminated_int8_array_input (const gint8 *v);

_GI_TEST_EXTERN
gboolean
extra_zero_terminated_int16_array_input (const gint16 *v);

_GI_TEST_EXTERN
gboolean
extra_zero_terminated_int16_array_input_full (gint16 *v);

_GI_TEST_EXTERN
void
extra_zero_terminated_int8_array_output_full (gint8 **v);

_GI_TEST_EXTERN
void
extra_zero_terminated_int16_array_output_full (gint16 **v);

/**
 * ExtraCallbackChars
 */
typedef gboolean (* ExtraCallbackChars) (gchar s8, guchar u8, gunichar u32);

_GI_TEST_EXTERN
gboolean
extra_call_callback_chars(ExtraCallbackChars func, gchar s8, guchar u8, gunichar u32);

/**
 * ExtraCallbackSignedInts
 */
typedef gboolean (* ExtraCallbackSignedInts) (gint x, gshort sx, glong lx, gint8 x8, gint16 x16, gint32 x32, gint64 x64);

_GI_TEST_EXTERN
gboolean
extra_call_callback_signed_ints(ExtraCallbackSignedInts func, gint x, gshort sx, glong lx, gint8 x8, gint16 x16, gint32 x32, gint64 x64);

/**
 * ExtraCallbackUnsignedInts
 */
typedef gboolean (* ExtraCallbackUnsignedInts) (guint x, gushort sx, gulong lx, guint8 x8, guint16 x16, guint32 x32, guint64 x64);

_GI_TEST_EXTERN
gboolean
extra_call_callback_unsigned_ints(ExtraCallbackUnsignedInts func, guint x, gushort sx, gulong lx, guint8 x8, guint16 x16, guint32 x32, guint64 x64);

/**
 * ExtraCallbackFloats
 */
typedef gboolean (* ExtraCallbackFloats) (float f32, double f64);

_GI_TEST_EXTERN
gboolean
extra_call_callback_floats(ExtraCallbackFloats func, float f32, double f64);

/**
 * ExtraCallbackConstStrings
 * @localestring: (type filename):
 */
typedef gboolean (* ExtraCallbackConstStrings) (const gchar *utf8string, const gchar* localestring);

/**
 * ExtraCallbackStrings
 * @localestring: (type filename):
 */
typedef gboolean (* ExtraCallbackStrings) (gchar *utf8string, gchar* localestring);

_GI_TEST_EXTERN
gboolean
extra_call_callback_const_strings_const(ExtraCallbackConstStrings func, const gchar *utf8string, const gchar *localestring);

_GI_TEST_EXTERN
gboolean
extra_call_callback_const_strings(ExtraCallbackConstStrings func, gchar *utf8string, gchar *localestring);

_GI_TEST_EXTERN
gboolean
extra_call_callback_strings(ExtraCallbackStrings func, gchar *utf8string, gchar *localestring);

/**
 * ExtraCallbackPointers
 */
typedef gboolean (* ExtraCallbackPointers) (GObject *obj, gpointer ptr);

_GI_TEST_EXTERN
gboolean
extra_call_callback_pointers(ExtraCallbackPointers func, GObject *obj, gpointer ptr);

/**
 * ExtraCharCallbackChar
 */
typedef gchar (* ExtraCharCallbackChar) (gchar c);

_GI_TEST_EXTERN
gchar
extra_call_callback_char_passthrough(ExtraCharCallbackChar func, gchar c);

/**
 * ExtraCallbackOutSignedInts
 * @s: (out):
 * @ss: (out):
 * @ls: (out):
 * @s8: (out):
 * @s16: (out):
 * @s32: (out):
 * @s64: (out):
 */
typedef void (* ExtraCallbackOutSignedInts) (gint *s, gshort *ss, glong *ls, gint8 *s8, gint16 *s16, gint32 *s32, gint64* s64);

_GI_TEST_EXTERN
gint64
extra_call_callback_out_signed_ints(ExtraCallbackOutSignedInts func);

/**
 * ExtraCallbackOutUnsignedInts
 * @u8: (out):
 * @u16: (out):
 * @u32: (out):
 * @u64: (out):
 */
typedef void (* ExtraCallbackOutUnsignedInts) (guint8 *u8, guint16 *u16, guint32 *u32, guint64* u64);

_GI_TEST_EXTERN
guint64
extra_call_callback_out_unsigned_ints(ExtraCallbackOutUnsignedInts func);

/**
 * ExtraCallbackOutStrings
 * @utf8string: (out) (type utf8):
 * @localestring: (out) (type filename):
 */
typedef void (* ExtraCallbackOutStrings) (gchar **utf8string, gchar **localestring);

_GI_TEST_EXTERN
gboolean
extra_call_callback_out_strings(ExtraCallbackOutStrings func);

/**
 * ExtraCallbackPointerPassthrough
 */
typedef gpointer (* ExtraCallbackPointerPassthrough) (gpointer ptr);

_GI_TEST_EXTERN
gpointer
extra_call_callback_pointer_passthrough(ExtraCallbackPointerPassthrough func, gpointer ptr);

/**
 * ExtraIntCallbackInt
 */
typedef gint (* ExtraIntCallbackInt) (gint c);

/**
 * ExtraCallbackOutFloats
 * @f32: (out):
 * @f64: (out):
 */
typedef void (* ExtraCallbackOutFloats) (float *f32, double *f64);
_GI_TEST_EXTERN

gdouble
extra_call_callback_out_floats(ExtraCallbackOutFloats func);

_GI_TEST_EXTERN
void
extra_return_callback(ExtraIntCallbackInt *func);

#endif /* _EXTRA_H_ */
