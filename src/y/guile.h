#ifndef Y_GUILE_H
#define Y_GUILE_H

void *dynfree(void *mem);
size_t scm_c_length(SCM lst);
int scm_is_list(SCM x);
SCM scm_class_set_x(SCM cls, SCM slot, SCM val);
SCM scm_drop_right_1(SCM lst);
SCM scm_is_equal(SCM a, SCM b);


#endif
