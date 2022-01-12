
// Autogenerated file
#ifndef GIG_STRVAL_H
#define GIG_STRVAL_H

#include <stdbool.h>
#include <stdint.h>
#include <libguile.h>

typedef void (* NameHashValFreeFunc)(SCM x);

typedef struct _NameItem
{
    char *str;
    SCM val;
} NameItem;

typedef struct _NameHash
{
    NameItem *entries;
    int len;
    int alloc;
} NameHash;

NameHash *name_hash_new(void);
SCM name_hash_find_entry(NameHash *kv, const char *key);
void name_hash_add_entry(NameHash *kv, const char *key, SCM val);
void name_hash_free(NameHash *kv, NameHashValFreeFunc valfree);
int name_hash_size(NameHash *kv);

#endif

