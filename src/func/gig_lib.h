#ifndef GIG_LIB_H
#define GIG_LIB_H

SCM gig_il_library(SCM s_namespace_, SCM s_path_list);
void *gig_lib_lookup(const char *namespace_, const char *symbol);

void gig_lib_init(void);

#endif
