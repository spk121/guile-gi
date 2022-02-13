#include <stdio.h>
#include <stdlib.h>
#include "keyval.h"

keyval_t *
keyval_new()
{
    keyval_t *kv = calloc(1, sizeof(keyval_t));
    if (kv == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    kv->entries = calloc(20, sizeof(keyval_item_t));
    if (kv->entries == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    kv->alloc = 20;
    kv->len = 0;
    return kv;
}

void
keyval_free(keyval_t *kv, void (*keyfree)(uint64_t key), void(*valfree)(uint64_t val))
{
    for(int i = 0; i < kv->len; i++) {
        if (keyfree)
            keyfree(kv->entries[i].key);
        if (valfree)
            valfree(kv->entries[i].val);
    }
    free(kv->entries);
    free(kv);
}

static int
compare(const void *pA, const void *pB)
{
    const keyval_item_t *A = pA;
    const keyval_item_t *B = pB;
    return (A->key > B->key) - (A->key < B->key);
}

void
keyval_add_entry(keyval_t *kv, uint64_t key, uint64_t val)
{
    keyval_item_t A;
    A.key = key;
    A.val = val;

    void *pB;
    pB = bsearch(&A, kv->entries, kv->len, sizeof(keyval_item_t), compare);
    if (pB == NULL) {
        if (kv->alloc == kv->len) {
            kv->alloc += 20;
            kv->entries = realloc(kv->entries, kv->alloc * sizeof(keyval_item_t));
            if (kv->entries == NULL) {
                fprintf(stderr, "Out of memory\n");
                exit(1);
            }
        }
        kv->entries[kv->len].key = key;
        kv->entries[kv->len].val = val;
        kv->len++;
        qsort(kv->entries, kv->len, sizeof(keyval_item_t), compare);
    }
    else {
        keyval_item_t *B = pB;
        B->val = val;
    }
}

__attribute__((pure)) uint64_t
keyval_find_entry(keyval_t *kv, uint64_t key)
{
    void *pB;
    keyval_item_t A;
    A.key = key;
    A.val = 0;

    pB = bsearch(&A, kv->entries, kv->len, sizeof(keyval_item_t), compare);
    if (pB == NULL)
        return 0;

    return ((keyval_item_t *) pB)->val;
}


__attribute__((pure)) int
keyval_size(keyval_t *kv)
{
    return kv->len;
}
