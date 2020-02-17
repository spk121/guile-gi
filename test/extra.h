#include <glib-object.h>

#include "gitestmacros.h"

#ifndef __EXTRA_H__
#define __EXTRA_H__

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

#endif /* _EXTRA_H_ */
