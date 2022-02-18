#ifndef X_KEYVAL_H
#define X_KEYVAL_H

#include <stdint.h>

typedef struct _keyval_item_t
{
    uint64_t key;
    uint64_t val;
} keyval_item_t;

typedef struct _keyval_t
{
    keyval_item_t *entries;
    int len;
    int alloc;
} keyval_t;

keyval_t *keyval_new(void);
uint64_t keyval_find_entry(keyval_t *kv, uint64_t key);
void keyval_add_entry(keyval_t *kv, uint64_t key, uint64_t val);
void keyval_free(keyval_t *kv, void (*keyfree)(uint64_t key), void(*valfree)(uint64_t val));
int keyval_size(keyval_t *kv);

#endif
