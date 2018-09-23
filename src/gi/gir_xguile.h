#ifndef _GIR_XGUILE_H_
#define _GIR_XGUILE_H_

#include <libguile.h>
extern SCM SCM_NONE;

#define scm_list_append(_list,_entry) \
  (scm_append(scm_list_2((_list),scm_list_1(_entry))))

void scm_add_int_constant (const char *name, long val);
void scm_add_string_constant (const char *name, const char *val);
SCM scm_c_list_ref (SCM list, size_t k);
int scm_is_foreign_object_type (SCM type);
int scm_is_gobject (SCM x);
int scm_is_hash_table (SCM x);
int scm_is_list (SCM x);
int scm_is_none (SCM x);
int scm_is_procedure (SCM x);
int scm_is_struct (SCM x);
SCM scm_list_of_utf8_symbols(const char *sym, ...);
SCM scm_make_class (SCM name, SCM dsupers, SCM dslots);
void scm_module_add_int_constant (SCM module, const char *name, long val);
void scm_module_add_string_constant (SCM module, const char *name, const char *val);
const char *scm_get_parse_error (void);
int scm_parse_list (SCM args, const char *format, ...);

void gir_init_xguile (void);
#endif
