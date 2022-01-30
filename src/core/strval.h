#ifndef X_STRVAL_H
#define X_STRVAL_H

#include <stdint.h>

typedef struct _strval_item_t
{
    char *key;
    uint64_t val;
} strval_item_t;

typedef struct _strval_t
{
    strval_item_t *entries;
    int len;
    int alloc;
} strval_t;

strval_t *strval_new(void);
uint64_t strval_find_entry(strval_t *kv, const char *key);
void strval_add_entry(strval_t *kv, const char *key, uint64_t val);
void strval_free(strval_t *kv, void (*valfree)(uint64_t val));
int strval_size(strval_t *kv);

#endif
