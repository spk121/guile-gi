#ifndef _GIR_XGUILE_H_
#define _GIR_XGUILE_H_

#include <libguile.h>
extern SCM SCM_NONE;

#define scm_list_append(_list,_entry) \
  (scm_append(scm_list_2((_list),scm_list_1(_entry))))

SCM scm_c_list_ref (SCM list, size_t k);
int scm_is_foreign_object_type (SCM type);
int scm_is_gobject (SCM x);
int scm_is_hash_table (SCM x);
int scm_is_list (SCM x);
int scm_is_none (SCM x);
int scm_is_procedure (SCM x);
int scm_is_struct (SCM x);
void gir_init_xguile (void);
#endif
