#ifndef GIG_LIB_H
#define GIG_LIB_H

void gig_lib_add(const char *namespace_, const char *version, const char **so_list, size_t n);
void *gig_lib_lookup(const char *namespace_, const char *symbol);

#endif
