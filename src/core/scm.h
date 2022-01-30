#ifndef G_GUILE_H
#define G_GUILE_H

#include <libguile.h>

int scm_is_boolean(SCM x);
int scm_is_bytevector(SCM x);
int scm_is_char(SCM x);
int scm_is_class(SCM x);
int scm_is_empty_hook(SCM x);
int scm_is_keyword(SCM x);
int scm_is_list(SCM x);
int scm_is_pointer(SCM x);
int scm_is_procedure(SCM x);
int scm_is_real(SCM x);

size_t scm_c_length(SCM x);
SCM scm_c_list_ref(SCM lst, size_t i);
void scm_c_activate_hook_2(SCM hook, SCM a, SCM b);
void scm_c_activate_hook_3(SCM hook, SCM a, SCM b, SCM c);
SCM scm_current_module_definition(SCM name);
void *scm_dynfree(void *x);
int scm_is_equal(SCM a, SCM b);
void scm_printf(SCM port, const char *fmt, ...);
SCM scm_sad_quit(void);
char *scm_write_to_utf8_stringn(SCM x, size_t max_len);

SCM scm_keyword_to_string(SCM x);
char *scm_to_utf8_symbol(SCM sym);

SCM guile_get_default_definition(SCM name);

#define SCM_UNBND_TO_BOOL_F(obj) \
    do {                         \
        if (SCM_UNBNDP (obj))    \
            obj = SCM_BOOL_F;    \
    } while (0)

void init_core_guile(void);

#endif
