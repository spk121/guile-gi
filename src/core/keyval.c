// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - High-performance hash table implementation with safety and generics

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Platform-specific feature detection
#if defined(__GLIBC__)
#define _GNU_SOURCE
#elif defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
#define _CRT_SECURE_NO_WARNINGS
#else
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#include "keyval.h"
#include "mem.h"
#include "logging.h"
#include "str.h"

// Configuration constants
#define DEFAULT_INITIAL_CAPACITY 16
#define DEFAULT_MAX_LOAD_FACTOR 0.75
#define GROWTH_FACTOR 2
#define MAX_CHAIN_LENGTH 8

// Hash table entry structure
typedef struct hash_entry {
    void *key;
    void *value;
    uint32_t hash;
    struct hash_entry *next;
    bool is_occupied;
} HashEntry;

// Generic hash map structure
struct hashmap {
    HashEntry *buckets;
    size_t capacity;
    size_t size;
    size_t key_size;
    size_t value_size;
    double max_load_factor;
    
    HashFunc hash_func;
    CompareFunc key_compare;
    CopyFunc key_copy;
    CopyFunc value_copy;
    FreeFunc key_free;
    FreeFunc value_free;
    
    bool keys_are_owned;
    bool values_are_owned;
    
    // Statistics
    size_t collision_count;
    size_t resize_count;
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_t mutex;
#endif
};

// Legacy structures for backward compatibility
struct keyval {
    HashMap *map;
};

struct strval {
    HashMap *map;
};

// Specialized map implementations
struct str_map {
    HashMap *map;
};

struct int_map {
    HashMap *map;
};

struct ptr_map {
    HashMap *map;
};

// Global error state
static HashMapError g_last_error = HASHMAP_ERROR_NONE;

// ============================================================================
// Hash Functions
// ============================================================================

uint32_t hash_bytes(const void *data, size_t size) {
    return hash_fnv1a(data, size);
}

uint32_t hash_string(const char *str) {
    if (!str) return 0;
    return hash_fnv1a(str, strlen(str));
}

uint32_t hash_string_case_insensitive(const char *str) {
    if (!str) return 0;
    
    uint32_t hash = 2166136261u;
    while (*str) {
        char ch = *str >= 'A' && *str <= 'Z' ? *str + 32 : *str;
        hash ^= (unsigned char)ch;
        hash *= 16777619u;
        str++;
    }
    return hash;
}

uint32_t hash_int32(uint32_t value) {
    // Thomas Wang's 32-bit hash
    value = ~value + (value << 15);
    value = value ^ (value >> 12);
    value = value + (value << 2);
    value = value ^ (value >> 4);
    value = value * 2057;
    value = value ^ (value >> 16);
    return value;
}

uint32_t hash_int64(uint64_t value) {
    // Thomas Wang's 64-bit to 32-bit hash
    value = (~value) + (value << 18);
    value = value ^ (value >> 31);
    value = value * 21;
    value = value ^ (value >> 11);
    value = value + (value << 6);
    value = value ^ (value >> 22);
    return (uint32_t)value;
}

uint32_t hash_pointer(const void *ptr) {
    uintptr_t addr = (uintptr_t)ptr;
    return hash_int64((uint64_t)addr);
}

uint32_t hash_fnv1a(const void *data, size_t size) {
    const uint8_t *bytes = (const uint8_t *)data;
    uint32_t hash = 2166136261u;
    
    for (size_t i = 0; i < size; i++) {
        hash ^= bytes[i];
        hash *= 16777619u;
    }
    return hash;
}

uint32_t hash_djb2(const void *data, size_t size) {
    const uint8_t *bytes = (const uint8_t *)data;
    uint32_t hash = 5381;
    
    for (size_t i = 0; i < size; i++) {
        hash = ((hash << 5) + hash) + bytes[i];
    }
    return hash;
}

uint32_t hash_murmur3(const void *data, size_t size, uint32_t seed) {
    const uint8_t *bytes = (const uint8_t *)data;
    const uint32_t c1 = 0xcc9e2d51;
    const uint32_t c2 = 0x1b873593;
    const uint32_t r1 = 15;
    const uint32_t r2 = 13;
    const uint32_t m = 5;
    const uint32_t n = 0xe6546b64;
    
    uint32_t hash = seed;
    
    // Process 4-byte chunks
    for (size_t i = 0; i < size / 4; i++) {
        uint32_t k;
        memcpy(&k, bytes + i * 4, sizeof(k));
        
        k *= c1;
        k = (k << r1) | (k >> (32 - r1));
        k *= c2;
        
        hash ^= k;
        hash = ((hash << r2) | (hash >> (32 - r2))) * m + n;
    }
    
    // Process remaining bytes
    uint32_t k1 = 0;
    switch (size & 3) {
        case 3: k1 ^= bytes[(size & ~3) + 2] << 16;
        case 2: k1 ^= bytes[(size & ~3) + 1] << 8;
        case 1: k1 ^= bytes[size & ~3];
                k1 *= c1;
                k1 = (k1 << r1) | (k1 >> (32 - r1));
                k1 *= c2;
                hash ^= k1;
    }
    
    // Finalization
    hash ^= size;
    hash ^= hash >> 16;
    hash *= 0x85ebca6b;
    hash ^= hash >> 13;
    hash *= 0xc2b2ae35;
    hash ^= hash >> 16;
    
    return hash;
}

// ============================================================================
// Helper Functions
// ============================================================================

static void set_error(HashMapError error) {
    g_last_error = error;
}

static int default_key_compare(const void *a, const void *b) {
    return memcmp(a, b, sizeof(void*));
}

static int string_key_compare(const void *a, const void *b) {
    const char *str_a = *(const char **)a;
    const char *str_b = *(const char **)b;
    
    if (str_a == str_b) return 0;
    if (!str_a) return -1;
    if (!str_b) return 1;
    
    return strcmp(str_a, str_b);
}

static int int_key_compare(const void *a, const void *b) {
    uint64_t val_a = *(const uint64_t *)a;
    uint64_t val_b = *(const uint64_t *)b;
    
    return (val_a > val_b) - (val_a < val_b);
}

static void *default_key_copy(const void *key) {
    // For simple types, just return the pointer
    return (void *)key;
}

static void *string_key_copy(const void *key) {
    const char *str = *(const char **)key;
    return str ? xstrdup(str) : NULL;
}

static void default_key_free(void *key) {
    // For non-owned keys, do nothing
    (void)key;
}

static void string_key_free(void *key) {
    char *str = (char *)key;
    xfree(str);
}

static uint32_t compute_hash(const HashMap *map, const void *key) {
    if (map->hash_func) {
        return map->hash_func(key, map->key_size);
    } else if (map->key_size == sizeof(char*)) {
        // Assume string key
        const char *str = *(const char **)key;
        return hash_string(str);
    } else if (map->key_size == sizeof(uint64_t)) {
        // Assume integer key
        return hash_int64(*(const uint64_t *)key);
    } else {
        // Generic byte hash
        return hash_bytes(key, map->key_size);
    }
}

static bool keys_equal(const HashMap *map, const void *key1, const void *key2) {
    if (map->key_compare) {
        return map->key_compare(key1, key2) == 0;
    } else {
        return memcmp(key1, key2, map->key_size) == 0;
    }
}

static bool resize_hashmap(HashMap *map, size_t new_capacity) {
    if (new_capacity < map->size) {
        set_error(HASHMAP_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
    HashEntry *old_buckets = map->buckets;
    size_t old_capacity = map->capacity;
    
    // Allocate new bucket array
    map->buckets = xcalloc(new_capacity, sizeof(HashEntry));
    if (!map->buckets) {
        map->buckets = old_buckets;
        set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
        return false;
    }
    
    map->capacity = new_capacity;
    size_t old_size = map->size;
    map->size = 0;
    map->collision_count = 0;
    
    // Rehash all entries
    for (size_t i = 0; i < old_capacity; i++) {
        HashEntry *entry = &old_buckets[i];
        while (entry && entry->is_occupied) {
            HashEntry *next = entry->next;
            
            // Insert into new table
            uint32_t hash = entry->hash;
            size_t bucket = hash % new_capacity;
            HashEntry *new_entry = &map->buckets[bucket];
            
            if (!new_entry->is_occupied) {
                // Direct insertion
                *new_entry = *entry;
                new_entry->next = NULL;
                map->size++;
            } else {
                // Collision - create chain
                HashEntry *chain_entry = xmalloc(sizeof(HashEntry));
                if (!chain_entry) {
                    // Cleanup and restore old table
                    xfree(map->buckets);
                    map->buckets = old_buckets;
                    map->capacity = old_capacity;
                    map->size = old_size;
                    set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
                    return false;
                }
                
                *chain_entry = *entry;
                chain_entry->next = new_entry->next;
                new_entry->next = chain_entry;
                map->size++;
                map->collision_count++;
            }
            
            entry = next;
        }
    }
    
    // Free old bucket array
    for (size_t i = 0; i < old_capacity; i++) {
        HashEntry *entry = old_buckets[i].next;
        while (entry) {
            HashEntry *next = entry->next;
            xfree(entry);
            entry = next;
        }
    }
    xfree(old_buckets);
    
    map->resize_count++;
    return true;
}

// ============================================================================
// Generic HashMap Implementation
// ============================================================================

HashMap *hashmap_create(size_t key_size, size_t value_size, const HashMapConfig *config) {
    if (key_size == 0 || value_size == 0) {
        set_error(HASHMAP_ERROR_INVALID_ARGUMENT);
        return NULL;
    }
    
    HashMap *map = XNEW(HashMap);
    if (!map) {
        set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
        return NULL;
    }
    
    // Set defaults
    map->capacity = config && config->initial_capacity > 0 ? 
                   config->initial_capacity : DEFAULT_INITIAL_CAPACITY;
    map->size = 0;
    map->key_size = key_size;
    map->value_size = value_size;
    map->max_load_factor = config && config->max_load_factor > 0 ? 
                          config->max_load_factor : DEFAULT_MAX_LOAD_FACTOR;
    
    // Function pointers
    map->hash_func = config ? config->hash_func : NULL;
    map->key_compare = config ? config->key_compare : default_key_compare;
    map->key_copy = config ? config->key_copy : default_key_copy;
    map->value_copy = config ? config->value_copy : NULL;
    map->key_free = config ? config->key_free : default_key_free;
    map->value_free = config ? config->value_free : NULL;
    
    map->keys_are_owned = config ? config->keys_are_owned : false;
    map->values_are_owned = config ? config->values_are_owned : false;
    
    map->collision_count = 0;
    map->resize_count = 0;
    
    // Allocate buckets
    map->buckets = xcalloc(map->capacity, sizeof(HashEntry));
    if (!map->buckets) {
        xfree(map);
        set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
        return NULL;
    }
    
#ifdef HASHMAP_THREAD_SAFE
    if (pthread_mutex_init(&map->mutex, NULL) != 0) {
        xfree(map->buckets);
        xfree(map);
        set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
        return NULL;
    }
#endif
    
    set_error(HASHMAP_ERROR_NONE);
    return map;
}

HashMap *hashmap_create_string_keys(size_t value_size, const HashMapConfig *config) {
    HashMapConfig string_config = config ? *config : hashmap_config_string_keys();
    return hashmap_create(sizeof(char*), value_size, &string_config);
}

HashMap *hashmap_create_int_keys(size_t value_size, const HashMapConfig *config) {
    HashMapConfig int_config = config ? *config : hashmap_config_int_keys();
    return hashmap_create(sizeof(uint64_t), value_size, &int_config);
}

void hashmap_destroy(HashMap *map) {
    if (!map) return;
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_lock(&map->mutex);
#endif
    
    // Free all entries
    for (size_t i = 0; i < map->capacity; i++) {
        HashEntry *entry = &map->buckets[i];
        while (entry && entry->is_occupied) {
            HashEntry *next = entry->next;
            
            // Free key and value if owned
            if (map->keys_are_owned && map->key_free && entry->key) {
                map->key_free(entry->key);
            }
            if (map->values_are_owned && map->value_free && entry->value) {
                map->value_free(entry->value);
            }
            
            // Free key and value memory
            xfree(entry->key);
            xfree(entry->value);
            
            // Free chain entry (but not the bucket entry itself)
            if (entry != &map->buckets[i]) {
                xfree(entry);
            }
            
            entry = next;
        }
    }
    
    xfree(map->buckets);
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_unlock(&map->mutex);
    pthread_mutex_destroy(&map->mutex);
#endif
    
    xfree(map);
}

bool hashmap_insert(HashMap *map, const void *key, const void *value) {
    if (!map || !key || !value) {
        set_error(HASHMAP_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_lock(&map->mutex);
#endif
    
    // Check if resize is needed
    double current_load = (double)map->size / map->capacity;
    if (current_load >= map->max_load_factor) {
        if (!resize_hashmap(map, map->capacity * GROWTH_FACTOR)) {
#ifdef HASHMAP_THREAD_SAFE
            pthread_mutex_unlock(&map->mutex);
#endif
            return false;
        }
    }
    
    uint32_t hash = compute_hash(map, key);
    size_t bucket = hash % map->capacity;
    HashEntry *entry = &map->buckets[bucket];
    
    // Check if bucket is empty
    if (!entry->is_occupied) {
        // Direct insertion
        entry->key = xmalloc(map->key_size);
        entry->value = xmalloc(map->value_size);
        if (!entry->key || !entry->value) {
            xfree(entry->key);
            xfree(entry->value);
            set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
#ifdef HASHMAP_THREAD_SAFE
            pthread_mutex_unlock(&map->mutex);
#endif
            return false;
        }
        
        memcpy(entry->key, key, map->key_size);
        memcpy(entry->value, value, map->value_size);
        entry->hash = hash;
        entry->next = NULL;
        entry->is_occupied = true;
        
        map->size++;
        set_error(HASHMAP_ERROR_NONE);
#ifdef HASHMAP_THREAD_SAFE
        pthread_mutex_unlock(&map->mutex);
#endif
        return true;
    }
    
    // Handle collision - search chain for existing key
    HashEntry *current = entry;
    while (current) {
        if (current->hash == hash && keys_equal(map, current->key, key)) {
            // Update existing entry
            memcpy(current->value, value, map->value_size);
            set_error(HASHMAP_ERROR_NONE);
#ifdef HASHMAP_THREAD_SAFE
            pthread_mutex_unlock(&map->mutex);
#endif
            return true;
        }
        
        if (!current->next) break;
        current = current->next;
    }
    
    // Add new entry to chain
    HashEntry *new_entry = xmalloc(sizeof(HashEntry));
    if (!new_entry) {
        set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
#ifdef HASHMAP_THREAD_SAFE
        pthread_mutex_unlock(&map->mutex);
#endif
        return false;
    }
    
    new_entry->key = xmalloc(map->key_size);
    new_entry->value = xmalloc(map->value_size);
    if (!new_entry->key || !new_entry->value) {
        xfree(new_entry->key);
        xfree(new_entry->value);
        xfree(new_entry);
        set_error(HASHMAP_ERROR_OUT_OF_MEMORY);
#ifdef HASHMAP_THREAD_SAFE
        pthread_mutex_unlock(&map->mutex);
#endif
        return false;
    }
    
    memcpy(new_entry->key, key, map->key_size);
    memcpy(new_entry->value, value, map->value_size);
    new_entry->hash = hash;
    new_entry->next = NULL;
    new_entry->is_occupied = true;
    
    current->next = new_entry;
    map->size++;
    map->collision_count++;
    
    set_error(HASHMAP_ERROR_NONE);
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_unlock(&map->mutex);
#endif
    return true;
}

bool hashmap_get(const HashMap *map, const void *key, void *value) {
    if (!map || !key || !value) {
        set_error(HASHMAP_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_lock((pthread_mutex_t*)&map->mutex);
#endif
    
    uint32_t hash = compute_hash(map, key);
    size_t bucket = hash % map->capacity;
    HashEntry *entry = &map->buckets[bucket];
    
    while (entry && entry->is_occupied) {
        if (entry->hash == hash && keys_equal(map, entry->key, key)) {
            memcpy(value, entry->value, map->value_size);
            set_error(HASHMAP_ERROR_NONE);
#ifdef HASHMAP_THREAD_SAFE
            pthread_mutex_unlock((pthread_mutex_t*)&map->mutex);
#endif
            return true;
        }
        entry = entry->next;
    }
    
    set_error(HASHMAP_ERROR_KEY_NOT_FOUND);
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_unlock((pthread_mutex_t*)&map->mutex);
#endif
    return false;
}

void *hashmap_get_ref(const HashMap *map, const void *key) {
    if (!map || !key) {
        set_error(HASHMAP_ERROR_INVALID_ARGUMENT);
        return NULL;
    }
    
    uint32_t hash = compute_hash(map, key);
    size_t bucket = hash % map->capacity;
    HashEntry *entry = &map->buckets[bucket];
    
    while (entry && entry->is_occupied) {
        if (entry->hash == hash && keys_equal(map, entry->key, key)) {
            set_error(HASHMAP_ERROR_NONE);
            return entry->value;
        }
        entry = entry->next;
    }
    
    set_error(HASHMAP_ERROR_KEY_NOT_FOUND);
    return NULL;
}

bool hashmap_remove(HashMap *map, const void *key) {
    if (!map || !key) {
        set_error(HASHMAP_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_lock(&map->mutex);
#endif
    
    uint32_t hash = compute_hash(map, key);
    size_t bucket = hash % map->capacity;
    HashEntry *entry = &map->buckets[bucket];
    HashEntry *prev = NULL;
    
    while (entry && entry->is_occupied) {
        if (entry->hash == hash && keys_equal(map, entry->key, key)) {
            // Free key and value if owned
            if (map->keys_are_owned && map->key_free) {
                map->key_free(entry->key);
            }
            if (map->values_are_owned && map->value_free) {
                map->value_free(entry->value);
            }
            
            xfree(entry->key);
            xfree(entry->value);
            
            if (prev) {
                // Remove from chain
                prev->next = entry->next;
                xfree(entry);
                map->collision_count--;
            } else if (entry->next) {
                // Replace bucket entry with next in chain
                HashEntry *next = entry->next;
                *entry = *next;
                xfree(next);
                map->collision_count--;
            } else {
                // Clear bucket entry
                entry->is_occupied = false;
                entry->next = NULL;
            }
            
            map->size--;
            set_error(HASHMAP_ERROR_NONE);
#ifdef HASHMAP_THREAD_SAFE
            pthread_mutex_unlock(&map->mutex);
#endif
            return true;
        }
        
        prev = entry;
        entry = entry->next;
    }
    
    set_error(HASHMAP_ERROR_KEY_NOT_FOUND);
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_unlock(&map->mutex);
#endif
    return false;
}

bool hashmap_contains(const HashMap *map, const void *key) {
    if (!map || !key) {
        return false;
    }
    
    uint32_t hash = compute_hash(map, key);
    size_t bucket = hash % map->capacity;
    HashEntry *entry = &map->buckets[bucket];
    
    while (entry && entry->is_occupied) {
        if (entry->hash == hash && keys_equal(map, entry->key, key)) {
            return true;
        }
        entry = entry->next;
    }
    
    return false;
}

size_t hashmap_size(const HashMap *map) {
    return map ? map->size : 0;
}

bool hashmap_is_empty(const HashMap *map) {
    return !map || map->size == 0;
}

void hashmap_clear(HashMap *map) {
    if (!map) return;
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_lock(&map->mutex);
#endif
    
    // Free all entries
    for (size_t i = 0; i < map->capacity; i++) {
        HashEntry *entry = &map->buckets[i];
        while (entry && entry->is_occupied) {
            HashEntry *next = entry->next;
            
            // Free key and value if owned
            if (map->keys_are_owned && map->key_free && entry->key) {
                map->key_free(entry->key);
            }
            if (map->values_are_owned && map->value_free && entry->value) {
                map->value_free(entry->value);
            }
            
            xfree(entry->key);
            xfree(entry->value);
            
            if (entry != &map->buckets[i]) {
                xfree(entry);
            } else {
                entry->is_occupied = false;
                entry->next = NULL;
            }
            
            entry = next;
        }
    }
    
    map->size = 0;
    map->collision_count = 0;
    
#ifdef HASHMAP_THREAD_SAFE
    pthread_mutex_unlock(&map->mutex);
#endif
}

// ============================================================================
// Configuration Functions
// ============================================================================

HashMapConfig hashmap_config_default(void) {
    HashMapConfig config = {0};
    config.initial_capacity = DEFAULT_INITIAL_CAPACITY;
    config.max_load_factor = DEFAULT_MAX_LOAD_FACTOR;
    config.hash_func = NULL;
    config.key_compare = default_key_compare;
    config.key_copy = default_key_copy;
    config.value_copy = NULL;
    config.key_free = default_key_free;
    config.value_free = NULL;
    config.keys_are_owned = false;
    config.values_are_owned = false;
    return config;
}

HashMapConfig hashmap_config_string_keys(void) {
    HashMapConfig config = hashmap_config_default();
    config.hash_func = (HashFunc)hash_string;
    config.key_compare = string_key_compare;
    config.key_copy = string_key_copy;
    config.key_free = string_key_free;
    config.keys_are_owned = true;
    return config;
}

HashMapConfig hashmap_config_int_keys(void) {
    HashMapConfig config = hashmap_config_default();
    config.hash_func = (HashFunc)hash_int64;
    config.key_compare = int_key_compare;
    config.keys_are_owned = false;
    return config;
}

// ============================================================================
// Legacy KeyVal Implementation (backward compatible)
// ============================================================================

KeyVal *keyval_new(void) {
    KeyVal *kv = XNEW(KeyVal);
    if (!kv) return NULL;
    
    HashMapConfig config = hashmap_config_int_keys();
    kv->map = hashmap_create_int_keys(sizeof(uint64_t), &config);
    if (!kv->map) {
        xfree(kv);
        return NULL;
    }
    
    return kv;
}

void keyval_free(KeyVal *kv, void (*key_free)(uint64_t key), void (*val_free)(uint64_t val)) {
    if (!kv) return;
    
    if (key_free || val_free) {
        // Iterate through all entries and call free functions
        for (size_t i = 0; i < kv->map->capacity; i++) {
            HashEntry *entry = &kv->map->buckets[i];
            while (entry && entry->is_occupied) {
                if (key_free) {
                    key_free(*(uint64_t*)entry->key);
                }
                if (val_free) {
                    val_free(*(uint64_t*)entry->value);
                }
                entry = entry->next;
            }
        }
    }
    
    hashmap_destroy(kv->map);
    xfree(kv);
}

bool keyval_add_entry(KeyVal *kv, uint64_t key, uint64_t val) {
    if (!kv) return false;
    return hashmap_insert_int(kv->map, key, &val);
}

uint64_t keyval_find_entry(const KeyVal *kv, uint64_t key) {
    if (!kv) return 0;
    
    uint64_t value;
    if (hashmap_get_int(kv->map, key, &value)) {
        return value;
    }
    return 0;
}

bool keyval_remove_entry(KeyVal *kv, uint64_t key) {
    if (!kv) return false;
    return hashmap_remove_int(kv->map, key);
}

int keyval_size(const KeyVal *kv) {
    return kv ? (int)hashmap_size(kv->map) : 0;
}

// ============================================================================
// Legacy StrVal Implementation (backward compatible)
// ============================================================================

StrVal *strval_new(void) {
    StrVal *sv = XNEW(StrVal);
    if (!sv) return NULL;
    
    HashMapConfig config = hashmap_config_string_keys();
    sv->map = hashmap_create_string_keys(sizeof(uint64_t), &config);
    if (!sv->map) {
        xfree(sv);
        return NULL;
    }
    
    return sv;
}

void strval_free(StrVal *sv, void (*val_free)(uint64_t val)) {
    if (!sv) return;
    
    if (val_free) {
        // Iterate through all entries and call free function
        for (size_t i = 0; i < sv->map->capacity; i++) {
            HashEntry *entry = &sv->map->buckets[i];
            while (entry && entry->is_occupied) {
                val_free(*(uint64_t*)entry->value);
                entry = entry->next;
            }
        }
    }
    
    hashmap_destroy(sv->map);
    xfree(sv);
}

bool strval_add_entry(StrVal *sv, const char *key, uint64_t val) {
    if (!sv || !key) return false;
    return hashmap_insert_string(sv->map, key, &val);
}

uint64_t strval_find_entry(const StrVal *sv, const char *key) {
    if (!sv || !key) return 0;
    
    uint64_t value;
    if (hashmap_get_string(sv->map, key, &value)) {
        return value;
    }
    return 0;
}

bool strval_remove_entry(StrVal *sv, const char *key) {
    if (!sv || !key) return false;
    return hashmap_remove_string(sv->map, key);
}

int strval_size(const StrVal *sv) {
    return sv ? (int)hashmap_size(sv->map) : 0;
}

// ============================================================================
// Convenience Functions
// ============================================================================

bool hashmap_insert_string(HashMap *map, const char *key, const void *value) {
    return hashmap_insert(map, &key, value);
}

bool hashmap_get_string(const HashMap *map, const char *key, void *value) {
    return hashmap_get(map, &key, value);
}

void *hashmap_get_string_ref(const HashMap *map, const char *key) {
    return hashmap_get_ref(map, &key);
}

bool hashmap_remove_string(HashMap *map, const char *key) {
    return hashmap_remove(map, &key);
}

bool hashmap_contains_string(const HashMap *map, const char *key) {
    return hashmap_contains(map, &key);
}

bool hashmap_insert_int(HashMap *map, uint64_t key, const void *value) {
    return hashmap_insert(map, &key, value);
}

bool hashmap_get_int(const HashMap *map, uint64_t key, void *value) {
    return hashmap_get(map, &key, value);
}

void *hashmap_get_int_ref(const HashMap *map, uint64_t key) {
    return hashmap_get_ref(map, &key);
}

bool hashmap_remove_int(HashMap *map, uint64_t key) {
    return hashmap_remove(map, &key);
}

bool hashmap_contains_int(const HashMap *map, uint64_t key) {
    return hashmap_contains(map, &key);
}

// ============================================================================
// Error Handling
// ============================================================================

HashMapError hashmap_get_last_error(void) {
    return g_last_error;
}

const char *hashmap_error_string(HashMapError error) {
    switch (error) {
        case HASHMAP_ERROR_NONE:
            return "No error";
        case HASHMAP_ERROR_OUT_OF_MEMORY:
            return "Out of memory";
        case HASHMAP_ERROR_INVALID_ARGUMENT:
            return "Invalid argument";
        case HASHMAP_ERROR_KEY_NOT_FOUND:
            return "Key not found";
        case HASHMAP_ERROR_DUPLICATE_KEY:
            return "Duplicate key";
        default:
            return "Unknown error";
    }
}
