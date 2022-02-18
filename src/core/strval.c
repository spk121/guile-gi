#define _XOPEN_SOURCE 700       /* For strdup, strndup */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "strval.h"

strval_t *
strval_new()
{
    strval_t *sv = calloc(1, sizeof(strval_t));
    if (sv == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    sv->entries = calloc(20, sizeof(strval_item_t));
    if (sv->entries == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    sv->alloc = 20;
    sv->len = 0;
    return sv;
}

void
strval_free(strval_t *sv, void (*valfree)(uint64_t val))
{
    for(int i = 0; i < sv->len; i++) {
        free(sv->entries[i].key);
        if (valfree)
            valfree(sv->entries[i].val);
    }
    free(sv->entries);
    free(sv);
}

static int
compare(const void *pA, const void *pB)
{
    const strval_item_t *A = pA;
    const strval_item_t *B = pB;
    return strcmp(A->key, B->key);
}

void
strval_add_entry(strval_t *sv, const char *key, uint64_t val)
{
    strval_item_t A;
    A.key = (char *)key;
    A.val = val;

    void *pB;
    pB = bsearch(&A, sv->entries, sv->len, sizeof(strval_item_t), compare);
    if (pB == NULL) {
        if (sv->alloc == sv->len) {
            sv->alloc += 20;
            sv->entries = realloc(sv->entries, sv->alloc * sizeof(strval_item_t));
            if (sv->entries == NULL) {
                fprintf(stderr, "Out of memory\n");
                exit(1);
            }
        }
        sv->entries[sv->len].key = strdup(key);
        sv->entries[sv->len].val = val;
        sv->len++;
        qsort(sv->entries, sv->len, sizeof(strval_item_t), compare);
    }
    else {
        strval_item_t *B = pB;
        B->val = val;
    }
}

uint64_t
strval_find_entry(strval_t *sv, const char *key)
{
    void *pB;
    strval_item_t A;
    A.key = (char *)key;
    A.val = 0;

    pB = bsearch(&A, sv->entries, sv->len, sizeof(strval_item_t), compare);
    if (pB == NULL)
        return 0;

    return ((strval_item_t *) pB)->val;
}

int
strval_size(strval_t *sv)
{
    return sv->len;
}
