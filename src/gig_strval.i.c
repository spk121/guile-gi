TABLE *
FUNC_new(void)
{
    TABLE *kv = calloc(1, sizeof(TABLE));
    kv->entries = calloc(20, sizeof(ITEM));
    kv->alloc = 20;
    kv->len = 0;
    return kv;
}

void
FUNC_free(TABLE *kv, TABLEValFreeFunc valfree)
{
    for (int i = 0; i < kv->len; i++) {
        free(kv->entries[i].str);
        if (valfree)
            valfree(kv->entries[i].val);
    }
    free(kv->entries);
    free(kv);
}

static int
FUNC_comparison(const void *pA, const void *pB)
{
    const ITEM *A = pA;
    const ITEM *B = pB;
    return strcmp(A->str, B->str);
}

void
FUNC_add_entry(TABLE *kv, const char *str, TYPE2 val)
{
    ITEM A;
    A.str = str;
    A.val = val;

    void *pB;
    pB = bsearch(&A, kv->entries, kv->len, sizeof(ITEM), FUNC_comparison);
    if (pB == NULL) {
        if (kv->alloc == kv->len) {
            kv->alloc += 20;
            kv->entries = realloc(kv->entries, kv->alloc * sizeof(ITEM));
        }
        kv->entries[kv->len].str = strdup(str);
        kv->entries[kv->len].val = val;
        kv->len++;
        qsort(kv->entries, kv->len, sizeof(ITEM), FUNC_comparison);
    }
    else {
        ITEM *B = pB;
        B->val = val;
    }
}

TYPE2
FUNC_find_entry(TABLE* kv, const char *str)
{
    void *pB;
    ITEM A;
    A.str = str;
    A.val = (TYPE2) 0;

    pB = bsearch(&A, kv->entries, kv->len, sizeof(ITEM), FUNC_comparison);
    if (pB == NULL)
        return (TYPE2) 0;

    return ((ITEM *) pB)->val;
}

int
FUNC_size(TABLE *kv)
{
    return kv->len;
}

