#ifndef GIG_MEM_LIST
#define GIG_MEM_LIST

typedef struct _gig_mem_list
{
    void *ptr;
    struct _gig_mem_list *next;
} GigMemList;

void gig_mem_list_add(GigMemList **lst, void *ptr);
void gig_mem_list_free(GigMemList **lst);

#endif
