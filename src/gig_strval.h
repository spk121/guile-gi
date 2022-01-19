
// Autogenerated file
#ifndef GIG_STRVAL_H
#define GIG_STRVAL_H

#include <stdbool.h>
#include <stdint.h>
#include <libguile.h>

typedef void (* NameHashValFreeFunc)(scm_t_bits x);

typedef struct _NameItem
{
    char *str;
    scm_t_bits val;
} NameItem;

typedef struct _NameHash
{
    NameItem *entries;
    int len;
    int alloc;
} NameHash;

NameHash *name_hash_new(void);
scm_t_bits name_hash_find_entry(NameHash *kv, const char *key);
void name_hash_add_entry(NameHash *kv, const char *key, scm_t_bits val);
void name_hash_free(NameHash *kv, NameHashValFreeFunc valfree);
int name_hash_size(NameHash *kv);

#endif
