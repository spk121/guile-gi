#include <stdio.h>
#include <stdlib.h>
#include "x/slist.h"

void slist_prepend(slist_t **lst, void *data)
{
    slist_t *cur;
    cur = malloc(sizeof(slist_t));
    if (cur == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    cur->data = data;
    cur->next = *lst;
    *lst = cur;
}

void slist_free(slist_t **lst, void (*free_func)(void *))
{
    slist_t *cur, *next;
    cur = *lst;
    do {
        if (cur == NULL)
            break;
        if (free_func)
            free_func(cur->data);
        next = cur->next;
        free(cur);
        cur = next;
    } while (1);
    *lst = NULL;
}
