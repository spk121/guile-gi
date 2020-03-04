#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "extra.h"

#include <string.h>

/**
 * extra_zero_terminated_uint8_array_input
 * @v: (array zero-terminated) (transfer none):
 */
gboolean
extra_zero_terminated_uint8_array_input (const guint8 *v)
{
    return (v[0] == 1) && (v[1] == 2) && (v[2] == 3) && (v[3] == 0);
}

/**
 * extra_zero_terminated_int8_array_input
 * @v: (array zero-terminated) (transfer none):
 */
gboolean
extra_zero_terminated_int8_array_input (const gint8 *v)
{
    return (v[0] == 1) && (v[1] == 2) && (v[2] == 3) && (v[3] == 0);
}

/**
 * extra_zero_terminated_int16_array_input
 * @v: (array zero-terminated) (transfer none):
 */
gboolean
extra_zero_terminated_int16_array_input (const gint16 *v)
{
    return (v[0] == 1) && (v[1] == 2) && (v[2] == 3) && (v[3] == 0);
}

/**
 * extra_zero_terminated_int16_array_input_full
 * @v: (array zero-terminated) (transfer full):
 */
gboolean
extra_zero_terminated_int16_array_input_full (gint16 *v)
{
    // So the zero-terminated array 'v' belongs to this procedure,
    // so I should be able to free it or mutate it.
    gboolean test1 = (v[0] == 1) && (v[1] == 2) && (v[2] == 3) && (v[3] == 0);
    v[0] = 0;
    free(v);
    v = NULL;
    return test1;
}

/**
 * extra_zero_terminated_int8_array_output_full
 * @v: (out) (array zero-terminated) (transfer full):
 */
void
extra_zero_terminated_int8_array_output_full (gint8 **v)
{
    *v = (gint8 *) g_malloc0_n(10, sizeof(gint8));
    for (gint8 i = 0; i < 9; i ++)
        (*v)[i] = 9 - i;

}

/**
 * extra_zero_terminated_int16_array_output_full
 * @v: (out) (array zero-terminated) (transfer full):
 */
void
extra_zero_terminated_int16_array_output_full (gint16 **v)
{
    *v = (gint16 *) g_malloc0_n(10, sizeof(gint16));
    for (gint16 i = 0; i < 9; i ++)
        (*v)[i] = 9 - i;
}

/**
 * extra_call_callback_chars:
 * @func: (scope call):
 */
gboolean
extra_call_callback_chars(ExtraCallbackChars func, gchar s8, guchar u8, gunichar u32)
{
    return func(s8, u8, u32);
}

/**
 * extra_call_callback_signed_ints:
 * @func: (scope call):
 */
gboolean
extra_call_callback_signed_ints(ExtraCallbackSignedInts func, gint x, gshort sx, glong lx, gint8 x8, gint16 x16, gint32 x32, gint64 x64)
{
    return func(x, sx, lx, x8, x16, x32, x64);
}

/**
 * extra_call_callback_unsigned_ints:
 * @func: (scope call):
 */
gboolean
extra_call_callback_unsigned_ints(ExtraCallbackUnsignedInts func, guint x, gushort sx, gulong lx, guint8 x8, guint16 x16, guint32 x32, guint64 x64)
{
    return func(x, sx, lx, x8, x16, x32, x64);
}

/**
 * extra_call_callback_floats:
 * @func: (scope call):
 */
gboolean
extra_call_callback_floats(ExtraCallbackFloats func, float f32, double f64)
{
    return func(f32, f64);
}

/**
 * extra_call_callback_const_strings_const:
 * @func: (scope call)
 * @utf8string: (type utf8)
 * @localestring: (type filename)
 */
gboolean
extra_call_callback_const_strings_const(ExtraCallbackConstStrings func, const gchar *utf8string, const gchar *localestring)
{
    return func(utf8string, localestring);
}

/**
 * extra_call_callback_const_strings:
 * @func: (scope call)
 * @utf8string: (type utf8)
 * @localestring: (type filename)
 */
gboolean
extra_call_callback_const_strings(ExtraCallbackConstStrings func, gchar *utf8string, gchar *localestring)
{
    return func(utf8string, localestring);
}
/**
 * extra_call_callback_strings:
 * @func: (scope call)
 * @utf8string: (type utf8)
 * @localestring: (type filename)
 */
gboolean
extra_call_callback_strings(ExtraCallbackStrings func, gchar *utf8string, gchar *localestring)
{
    return func(utf8string, localestring);
}

/**
 * extra_call_callback_out_signed_ints:
 * @func: (scope call):
 */
gint64
extra_call_callback_out_signed_ints(ExtraCallbackOutSignedInts func)
{
    gint s;
    gshort ss;
    glong ls;
    gint8 s8;
    gint16 s16;
    gint32 s32;
    gint64 s64;
    func(&s, &ss, &ls, &s8, &s16, &s32, &s64);
    return s + ss + ls + s8 + s16 + s32 + s64;
}

/**
 * extra_call_callback_out_unsigned_ints:
 * @func: (scope call):
 */
guint64
extra_call_callback_out_unsigned_ints(ExtraCallbackOutUnsignedInts func)
{
    guint8 u8;
    guint16 u16;
    guint32 u32;
    guint64 u64;
    func(&u8, &u16, &u32, &u64);
    return u8 + u16 + u32 + u64;
}

/**
 * extra_call_callback_out_floats:
 * @func: (scope call):
 */
gdouble
extra_call_callback_out_floats(ExtraCallbackOutFloats func)
{
    gfloat f32;
    gdouble f64;
    func(&f32, &f64);
    return f32 + f64;
}

/**
 * extra_call_callback_out_strings:
 * @func: (scope call):
 */
gboolean
extra_call_callback_out_strings(ExtraCallbackOutStrings func)
{
    gchar *utf8;
    gchar *locale;
    func(&utf8, &locale);
    if (strcmp(utf8, "foo") == 0 && strcmp(locale, "bar") == 0)
        return TRUE;
    return FALSE;
}

/**
 * extra_call_callback_pointers:
 * @func: (scope call):
 */
gboolean
extra_call_callback_pointers(ExtraCallbackPointers func, GObject *obj, gpointer ptr)
{
    return func(obj, ptr);
}

/**
 * extra_call_callback_pointer_passthrough:
 * @func: (scope call):
 */
gpointer
extra_call_callback_pointer_passthrough(ExtraCallbackPointerPassthrough func, gpointer ptr)
{
    return func(ptr);
}

/**
 * extra_call_callback_char_passthrough:
 * @func: (scope call):
 */
gchar
extra_call_callback_char_passthrough(ExtraCharCallbackChar func, gchar c)
{
    return func(c);
}

gint
integer_passthrough(gint x)
{
    return x;
}

/**
 * extra_return_callback:
 * @func: (out) (scope local):
 */
void
extra_return_callback(ExtraIntCallbackInt *func)
{
    *func = integer_passthrough;
}
