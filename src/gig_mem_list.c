#include <stdlib.h>
#include "gig_mem_list.h"

void
gig_mem_list_add(GigMemList **lst, void *ptr)
{
    GigMemList *cur;
    cur = malloc(sizeof(GigMemList));
    if (cur == NULL)
        abort();
    cur->ptr = ptr;
    cur->next = *lst;
    *lst = cur;
}

void
gig_mem_list_free(GigMemList **lst)
{
    GigMemList *cur, *next;
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
