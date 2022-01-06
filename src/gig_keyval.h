#ifndef GIG_KEYVAL_H
#define GIG_KEYVAL_H

#include <stdbool.h>
#include <stdint.h>

typedef struct _GigKeyValItem
{
    int64_t key;
    int64_t val;
} GigKeyValItem;

typedef struct _GigKeyVal
{
    GigKeyValItem *entries;
    int len;
    int alloc;
} GigKeyVal;

typedef void (*GigKeyValFreeFunc)(int64_t x);

GigKeyVal *gig_keyval_new(void);
int64_t gig_keyval_add_entry(GigKeyVal * kv, int64_t key, int64_t val);
int64_t gig_keyval_find_entry(GigKeyVal * kv, int64_t key);
bool
gig_keyval_find_entry_extended(GigKeyVal * hash, int64_t key, int64_t * outkey, int64_t * outval);
void gig_keyval_free(GigKeyVal * kv, GigKeyValFreeFunc keyfree, GigKeyValFreeFunc valfree);
int gig_keyval_size(GigKeyVal * kv);

typedef struct _GigStrValItem
{
    char *str;
    int64_t val;
} GigStrValItem;

typedef struct _GigStrVal
{
    GigStrValItem *entries;
    int len;
    int alloc;
} GigStrVal;

GigStrVal *gig_strval_new(void);
int64_t gig_strval_find_entry(GigStrVal * sv, const char *str);
void gig_strval_add_entry(GigStrVal * sv, const char *str, int64_t val);
void gig_strval_free(GigStrVal * sv, GigKeyValFreeFunc valfree);

#endif
