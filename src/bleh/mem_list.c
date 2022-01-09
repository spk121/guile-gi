#include <stdlib.h>
#include "mem_list.h"
#include "util.h"

void
mem_list_add(MemList **lst, void *ptr)
{
    MemList *cur;
    cur = xmalloc(sizeof(MemList));
    cur->ptr = ptr;
    cur->next = *lst;
    *lst = cur;
}

void
mem_list_free(MemList **lst)
{
    MemList *cur, *next;
    cur = *lst;
    do {
        if (cur == NULL)
            break;
        free(cur->ptr);
        next = cur->next;
        free(cur);
        cur = next;
    } while (1);
    *lst = NULL;
}
