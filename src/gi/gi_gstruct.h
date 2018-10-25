#ifndef _GI_GSTRUCT_H_
#define _GI_GSTRUCT_H_
#include "__gi_gstruct.h"
SCM  gi_gstruct_new_from_gtype (GType type, void *ptr, gboolean free_on_dealloc);
void gi_gstruct_finalizer (SCM self);
void gi_gstruct_pointer_finalizer(void *ptr);
void gi_init_gstruct (void);
#endif