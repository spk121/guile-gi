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
