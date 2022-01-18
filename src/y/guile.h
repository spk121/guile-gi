#ifndef Y_GUILE_H
#define Y_GUILE_H

void *dynfree(void *mem);
size_t scm_c_length(SCM lst);
int scm_is_list(SCM x);
SCM scm_class_set_x(SCM cls, SCM slot, SCM val);
SCM scm_drop_right_1(SCM lst);
SCM scm_c_list_ref(SCM lst, size_t k);
char *scm_write_to_utf8_stringn(SCM x, size_t max_len);
SCM scm_class_ref(SCM cls, SCM slot);
int scm_is_equal(SCM a, SCM b);
SCM scm_drop_right_1(SCM lst);
#define SCM_UNBND_TO_BOOL_F(obj) \
    do {                         \
        if (SCM_UNBNDP (obj))    \
            obj = SCM_BOOL_F;    \
    } while (0)                  \







#endif
