#ifndef _GIR_FUNC2_H
#define _GIR_FUNC2_H

// SCM
// gi_type_import_by_gi_info (GIBaseInfo *info);
GType gir_lookup_type(const char *name);
void gir_unref_object(SCM s_object);
void gir_init_func2(void);
#endif
