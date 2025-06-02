#ifndef GIG_LIB_H
#define GIG_LIB_H

void **gig_lib_all_handles(void);
void *gig_lib_lookup(const char *namespace_, const char *symbol);

void gig_init_lib(void);

#endif
