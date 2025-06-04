// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced key-value stores with hash tables, generics, and safety

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

#ifndef CORE_KEYVAL_H
#define CORE_KEYVAL_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations for better encapsulation
typedef struct keyval KeyVal;
typedef struct strval StrVal;
typedef struct hashmap HashMap;

// Original integer key-value structures (for backward compatibility)
typedef struct keyval_item {
    uint64_t key;
    uint64_t val;
} KeyValItem;

typedef struct strval_item {
    char *key;
    uint64_t val;
} StrValItem;

// Function pointer types for generic operations
typedef uint32_t (*HashFunc)(const void *key, size_t key_size);
typedef int (*CompareFunc)(const void *a, const void *b);
typedef void (*FreeFunc)(void *ptr);
typedef void *(*CopyFunc)(const void *ptr);

// Iterator callback function
typedef bool (*IteratorFunc)(const void *key, size_t key_size, void *value, 
                            size_t value_size, void *user_data);

// Hash map configuration
typedef struct hashmap_config {
    size_t initial_capacity;     // Initial number of buckets (0 = default)
    double max_load_factor;      // Resize when load > this (0 = default 0.75)
    HashFunc hash_func;          // Custom hash function (NULL = default)
    CompareFunc key_compare;     // Key comparison function (NULL = memcmp)
    CopyFunc key_copy;           // Key copy function (NULL = memcpy)
    CopyFunc value_copy;         // Value copy function (NULL = memcpy) 
    FreeFunc key_free;           // Key destructor (NULL = no cleanup)
    FreeFunc value_free;         // Value destructor (NULL = no cleanup)
    bool keys_are_owned;         // Whether map owns key memory
    bool values_are_owned;       // Whether map owns value memory
} HashMapConfig;

// Statistics for performance monitoring
typedef struct hashmap_stats {
    size_t size;                 // Number of entries
    size_t capacity;             // Number of buckets
    size_t collisions;           // Total collision count
    size_t max_chain_length;     // Longest collision chain
    double load_factor;          // Current load factor
    size_t memory_used;          // Approximate memory usage
    size_t resize_count;         // Number of resize operations
} HashMapStats;

// ============================================================================
// Original API (modernized with better error handling)
// ============================================================================

// Integer key-value store (backward compatible)
GIG_API KeyVal *keyval_new(void) GIG_MALLOC;
GIG_API void keyval_free(KeyVal *kv, void (*key_free)(uint64_t key), 
                        void (*val_free)(uint64_t val));
GIG_API bool keyval_add_entry(KeyVal *kv, uint64_t key, uint64_t val);
GIG_API uint64_t keyval_find_entry(const KeyVal *kv, uint64_t key);
GIG_API bool keyval_remove_entry(KeyVal *kv, uint64_t key);
GIG_API int keyval_size(const KeyVal *kv) GIG_PURE;

// String key-value store (backward compatible)
GIG_API StrVal *strval_new(void) GIG_MALLOC;
GIG_API void strval_free(StrVal *sv, void (*val_free)(uint64_t val));
GIG_API bool strval_add_entry(StrVal *sv, const char *key, uint64_t val);
GIG_API uint64_t strval_find_entry(const StrVal *sv, const char *key);
GIG_API bool strval_remove_entry(StrVal *sv, const char *key);
GIG_API int strval_size(const StrVal *sv) GIG_PURE;

// Enhanced operations for original types
GIG_API bool keyval_contains(const KeyVal *kv, uint64_t key) GIG_PURE;
GIG_API bool keyval_update(KeyVal *kv, uint64_t key, uint64_t val);
GIG_API void keyval_clear(KeyVal *kv);
GIG_API uint64_t *keyval_get_keys(const KeyVal *kv, size_t *count) GIG_MALLOC;
GIG_API uint64_t *keyval_get_values(const KeyVal *kv, size_t *count) GIG_MALLOC;

GIG_API bool strval_contains(const StrVal *sv, const char *key) GIG_PURE;
GIG_API bool strval_update(StrVal *sv, const char *key, uint64_t val);
GIG_API void strval_clear(StrVal *sv);
GIG_API char **strval_get_keys(const StrVal *sv, size_t *count) GIG_MALLOC;
GIG_API uint64_t *strval_get_values(const StrVal *sv, size_t *count) GIG_MALLOC;

// ============================================================================
// Generic Hash Map API (new, high-performance implementation)
// ============================================================================

// Creation and destruction
GIG_API HashMap *hashmap_create(size_t key_size, size_t value_size, 
                               const HashMapConfig *config) GIG_MALLOC;
GIG_API HashMap *hashmap_create_string_keys(size_t value_size, 
                                           const HashMapConfig *config) GIG_MALLOC;
GIG_API HashMap *hashmap_create_int_keys(size_t value_size, 
                                        const HashMapConfig *config) GIG_MALLOC;
GIG_API void hashmap_destroy(HashMap *map);

// Basic operations
GIG_API bool hashmap_insert(HashMap *map, const void *key, const void *value);
GIG_API bool hashmap_get(const HashMap *map, const void *key, void *value);
GIG_API void *hashmap_get_ref(const HashMap *map, const void *key);
GIG_API bool hashmap_remove(HashMap *map, const void *key);
GIG_API bool hashmap_contains(const HashMap *map, const void *key) GIG_PURE;
GIG_API bool hashmap_update(HashMap *map, const void *key, const void *value);

// Bulk operations
GIG_API void hashmap_clear(HashMap *map);
GIG_API size_t hashmap_size(const HashMap *map) GIG_PURE;
GIG_API bool hashmap_is_empty(const HashMap *map) GIG_PURE;
GIG_API void hashmap_get_stats(const HashMap *map, HashMapStats *stats);

// Iteration
GIG_API bool hashmap_foreach(const HashMap *map, IteratorFunc func, void *user_data);
GIG_API void **hashmap_get_keys(const HashMap *map, size_t *count) GIG_MALLOC;
GIG_API void **hashmap_get_values(const HashMap *map, size_t *count) GIG_MALLOC;

// String-specific convenience functions
GIG_API bool hashmap_insert_string(HashMap *map, const char *key, const void *value);
GIG_API bool hashmap_get_string(const HashMap *map, const char *key, void *value);
GIG_API void *hashmap_get_string_ref(const HashMap *map, const char *key);
GIG_API bool hashmap_remove_string(HashMap *map, const char *key);
GIG_API bool hashmap_contains_string(const HashMap *map, const char *key) GIG_PURE;

// Integer-specific convenience functions
GIG_API bool hashmap_insert_int(HashMap *map, uint64_t key, const void *value);
GIG_API bool hashmap_get_int(const HashMap *map, uint64_t key, void *value);
GIG_API void *hashmap_get_int_ref(const HashMap *map, uint64_t key);
GIG_API bool hashmap_remove_int(HashMap *map, uint64_t key);
GIG_API bool hashmap_contains_int(const HashMap *map, uint64_t key) GIG_PURE;

// ============================================================================
// Specialized Hash Maps for Common Use Cases
// ============================================================================

// String-to-string mapping
typedef struct str_map StrMap;
GIG_API StrMap *strmap_create(void) GIG_MALLOC;
GIG_API void strmap_destroy(StrMap *map);
GIG_API bool strmap_insert(StrMap *map, const char *key, const char *value);
GIG_API const char *strmap_get(const StrMap *map, const char *key) GIG_PURE;
GIG_API bool strmap_remove(StrMap *map, const char *key);
GIG_API size_t strmap_size(const StrMap *map) GIG_PURE;

// Integer-to-pointer mapping
typedef struct int_map IntMap;
GIG_API IntMap *intmap_create(void) GIG_MALLOC;
GIG_API void intmap_destroy(IntMap *map);
GIG_API bool intmap_insert(IntMap *map, uint64_t key, void *value);
GIG_API void *intmap_get(const IntMap *map, uint64_t key) GIG_PURE;
GIG_API bool intmap_remove(IntMap *map, uint64_t key);
GIG_API size_t intmap_size(const IntMap *map) GIG_PURE;

// Pointer-to-pointer mapping (useful for object associations)
typedef struct ptr_map PtrMap;
GIG_API PtrMap *ptrmap_create(void) GIG_MALLOC;
GIG_API void ptrmap_destroy(PtrMap *map);
GIG_API bool ptrmap_insert(PtrMap *map, const void *key, void *value);
GIG_API void *ptrmap_get(const PtrMap *map, const void *key) GIG_PURE;
GIG_API bool ptrmap_remove(PtrMap *map, const void *key);
GIG_API size_t ptrmap_size(const PtrMap *map) GIG_PURE;

// ============================================================================
// Hash Functions
// ============================================================================

GIG_API uint32_t hash_bytes(const void *data, size_t size) GIG_PURE;
GIG_API uint32_t hash_string(const char *str) GIG_PURE;
GIG_API uint32_t hash_string_case_insensitive(const char *str) GIG_PURE;
GIG_API uint32_t hash_int32(uint32_t value) GIG_PURE;
GIG_API uint32_t hash_int64(uint64_t value) GIG_PURE;
GIG_API uint32_t hash_pointer(const void *ptr) GIG_PURE;

// High-quality hash functions
GIG_API uint32_t hash_fnv1a(const void *data, size_t size) GIG_PURE;
GIG_API uint32_t hash_djb2(const void *data, size_t size) GIG_PURE;
GIG_API uint32_t hash_murmur3(const void *data, size_t size, uint32_t seed) GIG_PURE;

// ============================================================================
// Configuration and Utilities
// ============================================================================

// Default configuration helpers
GIG_API HashMapConfig hashmap_config_default(void);
GIG_API HashMapConfig hashmap_config_string_keys(void);
GIG_API HashMapConfig hashmap_config_int_keys(void);
GIG_API HashMapConfig hashmap_config_ptr_keys(void);

// Error handling
typedef enum {
    HASHMAP_ERROR_NONE = 0,
    HASHMAP_ERROR_OUT_OF_MEMORY,
    HASHMAP_ERROR_INVALID_ARGUMENT,
    HASHMAP_ERROR_KEY_NOT_FOUND,
    HASHMAP_ERROR_DUPLICATE_KEY
} HashMapError;

GIG_API HashMapError hashmap_get_last_error(void);
GIG_API const char *hashmap_error_string(HashMapError error) GIG_PURE;

// Memory usage optimization
GIG_API void hashmap_optimize(HashMap *map);
GIG_API void hashmap_reserve(HashMap *map, size_t capacity);
GIG_API void hashmap_shrink_to_fit(HashMap *map);

// Debugging and diagnostics
GIG_API void hashmap_print_stats(const HashMap *map, FILE *stream);
GIG_API bool hashmap_validate(const HashMap *map);

// Thread safety (when enabled)
#ifdef HASHMAP_THREAD_SAFE
GIG_API void hashmap_lock(HashMap *map);
GIG_API void hashmap_unlock(HashMap *map);
GIG_API bool hashmap_try_lock(HashMap *map);
#endif

// ============================================================================
// Utility Macros
// ============================================================================

// Type-safe insertion macros
#define HASHMAP_INSERT_VALUE(map, key, value_type, value) \
    do { \
        value_type _temp_val = (value); \
        hashmap_insert((map), &(key), &_temp_val); \
    } while(0)

#define HASHMAP_GET_VALUE(map, key, value_type, default_val) \
    ({ \
        value_type _temp_val; \
        hashmap_get((map), &(key), &_temp_val) ? _temp_val : (default_val); \
    })

// Convenient iteration macros
#define HASHMAP_FOREACH(map, key_var, value_var, user_data) \
    hashmap_foreach((map), _hashmap_foreach_helper, (user_data))

// Deprecated aliases for backward compatibility
GIG_DEPRECATED_API typedef KeyValItem keyval_item_t;
GIG_DEPRECATED_API typedef KeyVal keyval_t;
GIG_DEPRECATED_API typedef StrValItem strval_item_t;
GIG_DEPRECATED_API typedef StrVal strval_t;

#ifdef __cplusplus
}
#endif

#endif // CORE_KEYVAL_H
