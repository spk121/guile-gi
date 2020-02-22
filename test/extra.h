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
typedef gboolean (* ExtraCallbackSignedInts) (gint8 x8, gint16 x16, gint32 x32, gint64 x64);

_GI_TEST_EXTERN
gboolean
extra_call_callback_signed_ints(ExtraCallbackSignedInts func, gint8 x8, gint16 x16, gint32 x32, gint64 x64);

/**
 * ExtraCallbackUnsignedInts
 */
typedef gboolean (* ExtraCallbackUnsignedInts) (guint8 x8, guint16 x16, guint32 x32, guint64 x64);

_GI_TEST_EXTERN
gboolean
extra_call_callback_unsigned_ints(ExtraCallbackUnsignedInts func, guint8 x8, guint16 x16, guint32 x32, guint64 x64);

/**
 * ExtraCallbackFloats
 */
typedef gboolean (* ExtraCallbackFloats) (float f32, double f64);

_GI_TEST_EXTERN
gboolean
extra_call_callback_floats(ExtraCallbackFloats func, float f32, double f64);

/**
 * ExtraCharCallbackChar
 */
typedef gchar (* ExtraCharCallbackChar) (gchar c);

_GI_TEST_EXTERN
gchar
extra_call_callback_char_passthrough(ExtraCharCallbackChar func, gchar c);

/**
 * ExtraCallbackOutSignedInts
 * @s8: (out):
 * @s16: (out):
 * @s32: (out):
 * @s64: (out):
 */
typedef void (* ExtraCallbackOutSignedInts) (gint8 *s8, gint16 *s16, gint32 *s32, gint64* s64);

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

#endif /* _EXTRA_H_ */
