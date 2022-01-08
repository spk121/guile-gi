#define _XOPEN_SOURCE 700       /* for strdup */
#include <stdlib.h>
#include <string.h>
#include "gig_keyval.h"
#include "gig_util.h"

GigKeyVal *
gig_keyval_new(void)
{
    GigKeyVal *kv = xcalloc(1, sizeof(GigKeyVal));
    kv->entries = xcalloc(20, sizeof(GigKeyValItem));
    kv->alloc = 20;
    kv->len = 0;
    return kv;
}

void
gig_keyval_free(GigKeyVal * kv, GigKeyValFreeFunc keyfree, GigKeyValFreeFunc valfree)
{
    for (int i = 0; i < kv->len; i++) {
        if (keyfree)
            keyfree(kv->entries[i].key);
        if (valfree)
            valfree(kv->entries[i].val);
    }
    free(kv->entries);
    free(kv);
}

static int
keyval_comparison(const void *pA, const void *pB)
{
    const GigKeyValItem *A = pA;
    const GigKeyValItem *B = pB;
    return (A->key > B->key) - (A->key < B->key);
}

int64_t
gig_keyval_add_entry(GigKeyVal * kv, int64_t key, int64_t val)
{
    GigKeyValItem A;
    A.key = key;
    A.val = val;

    void *pB;
    pB = bsearch(&A, kv->entries, kv->len, sizeof(GigKeyValItem), keyval_comparison);
    if (pB == NULL) {
        if (kv->alloc == kv->len) {
            kv->alloc += 20;
            kv->entries = realloc(kv->entries, kv->alloc * sizeof(GigKeyValItem));
        }
        kv->entries[kv->len].key = key;
        kv->entries[kv->len].val = val;
        kv->len++;
        qsort(kv->entries, kv->len, sizeof(GigKeyValItem), keyval_comparison);
        return 0;
    }

    GigKeyValItem *B = pB;
    int64_t old_val = B->val;
    B->val = val;
    return old_val;
}

int64_t
gig_keyval_find_entry(GigKeyVal * kv, int64_t key)
{
    void *pB;
    GigKeyValItem A;
    A.key = key;
    A.val = 0;

    pB = bsearch(&A, kv->entries, kv->len, sizeof(GigKeyValItem), keyval_comparison);
    if (pB == NULL)
        return 0;

    return ((GigKeyValItem *) pB)->val;
}

bool
gig_keyval_find_entry_extended(GigKeyVal * kv, int64_t key, int64_t * outkey, int64_t * outval)
{
    void *pB;
    GigKeyValItem A;
    A.key = key;
    A.val = 0;

    pB = bsearch(&A, kv->entries, kv->len, sizeof(GigKeyValItem), keyval_comparison);
    if (pB == NULL)
        return false;

    *outkey = ((GigKeyValItem *) pB)->key;
    *outval = ((GigKeyValItem *) pB)->val;
    return true;
}

int
gig_keyval_size(GigKeyVal * kv)
{
    return kv->len;
}

static int
strval_comparison(const void *pA, const void *pB)
{
    const GigStrValItem *A = pA;
    const GigStrValItem *B = pB;
    return strcmp(A->str, B->str);
}

GigStrVal *
gig_strval_new(void)
{
    GigStrVal *sv = xcalloc(1, sizeof(GigStrVal));
    sv->entries = xcalloc(20, sizeof(GigStrValItem));
    sv->alloc = 20;
    sv->len = 0;
    return sv;
}

int64_t
gig_strval_find_entry(GigStrVal * sv, const char *str)
{
    void *pB;
    GigStrValItem A;
    A.str = str;
    A.val = 0;

    pB = bsearch(&A, sv->entries, sv->len, sizeof(GigStrValItem), strval_comparison);
    if (pB == NULL)
        return 0;

    return ((GigStrValItem *) pB)->val;
}

void
gig_strval_add_entry(GigStrVal * sv, const char *str, int64_t val)
{
    GigStrValItem A;
    A.str = str;
    A.val = val;

    void *pB;
    pB = bsearch(&A, sv->entries, sv->len, sizeof(GigStrValItem), strval_comparison);
    if (pB == NULL) {
        if (sv->alloc == sv->len) {
            sv->alloc += 20;
            sv->entries = realloc(sv->entries, sv->alloc * sizeof(GigStrValItem));
        }
        sv->entries[sv->len].str = strdup(str);
        sv->entries[sv->len].val = val;
        sv->len++;
        qsort(sv->entries, sv->len, sizeof(GigStrValItem), strval_comparison);
    }
    else {
        GigStrValItem *B = pB;
        B->val = val;
    }
}

void
gig_strval_free(GigStrVal * sv, GigKeyValFreeFunc valfree)
{
    for (int i = 0; i < sv->len; i++) {
        free(sv->entries[i].str);
        if (valfree)
            valfree(sv->entries[i].val);
    }
    free(sv->entries);
    free(sv);
}
