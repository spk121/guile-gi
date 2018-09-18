#ifndef _GIR_G_VALUE_H_
#define _GIR_G_VALUE_H_

extern SCM GuGValue_Type;
typedef SCM SCM_GVALUE;
int gug_value_from_scm_with_error(GValue *value, SCM obj);
void gir_init_g_value (void);
#endif
