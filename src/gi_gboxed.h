#ifndef _GI_BOXED_H_
#define _GI_BOXED_H_
#include "__gi_gboxed.h"

SCM gi_gboxed_new(GType boxed_type, gpointer boxed, gboolean copy_boxed, gboolean own_ref);
void gi_init_gboxed(void);
#endif
