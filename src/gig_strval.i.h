typedef void (* TABLEValFreeFunc)(TYPE2 x);

typedef struct _ITEM
{
    char *str;
    TYPE2 val;
} ITEM;

typedef struct _TABLE
{
    ITEM *entries;
    int len;
    int alloc;
} TABLE;

TABLE *FUNC_new(void);
TYPE2 FUNC_find_entry(TABLE *kv, const char *key);
void FUNC_add_entry(TABLE *kv, const char *key, TYPE2 val);
void FUNC_free(TABLE *kv, TABLEValFreeFunc valfree);
int FUNC_size(TABLE *kv);
