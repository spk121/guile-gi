#ifndef CORE_MEM_LIST
#define CORE_MEM_LIST

typedef struct _MemList
{
    void *ptr;
    struct _MemList *next;
} MemList;

void mem_list_add(MemList **lst, void *ptr);
void mem_list_free(MemList **lst);

#endif
