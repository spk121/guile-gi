typedef void (* TABLEKeyFreeFunc)(TYPE1 x);
typedef void (* TABLEValFreeFunc)(TYPE2 x);

typedef struct _ITEM
{
    TYPE1 key;
    TYPE2 val;
} ITEM;

typedef struct _TABLE
{
    ITEM *entries;
    int len;
    int alloc;
} TABLE;

TABLE *FUNC_new(void);
TYPE2 FUNC_find_entry(TABLE *kv, TYPE1 key);
void FUNC_add_entry(TABLE *kv, TYPE1 key, TYPE2 val);
void FUNC_free(TABLE *kv, TABLEKeyFreeFunc keyfree, TABLEValFreeFunc valfree);
int FUNC_size(TABLE *kv);
