#ifndef X_SLIST_H
#define X_SLIST_H

typedef struct _slist_t
{
    void *data;
    struct _slist_t *next;
} slist_t;

void slist_prepend(slist_t **lst, void *data);
void slist_free(slist_t **lst, void (*free_func)(void *));

#endif
