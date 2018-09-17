#ifndef _GIR_GTYPE_TYPE_H_
#define _GIR_GTYPE_TYPE_H

extern SCM GuGType_Type;
extern SCM GuGTypeWrapper_Type;

GType gu_g_type_get_type (SCM gtype);

void gir_init_g_type(void);
#endif
