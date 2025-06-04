// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced singly-linked list with safety, performance, and rich API

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

#ifndef CORE_SLIST_H
#define CORE_SLIST_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include "visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations for better encapsulation
typedef struct slist_node SListNode;
typedef struct slist SList;

// Legacy structure for backward compatibility
typedef struct slist_node {
    void *data;
    struct slist_node *next;
} slist_t;

// Function pointer types
typedef void (*SListFreeFunc)(void *data);
typedef void *(*SListCopyFunc)(const void *data);
typedef int (*SListCompareFunc)(const void *a, const void *b);
typedef bool (*SListFilterFunc)(const void *data, void *user_data);
typedef void (*SListForeachFunc)(void *data, void *user_data);
typedef void *(*SListMapFunc)(const void *data, void *user_data);

// List configuration
typedef struct slist_config {
    SListCopyFunc copy_func;     // Function to copy data (NULL = shallow copy)
    SListFreeFunc free_func;     // Function to free data (NULL = no cleanup)
    SListCompareFunc compare_func; // Function to compare data (NULL = pointer comparison)
    bool owns_data;              // Whether list owns the data memory
} SListConfig;

// Iterator structure for safe traversal
typedef struct slist_iterator {
    SListNode *current;
    SListNode *previous;
    SList *list;
    bool is_valid;
} SListIterator;

// Statistics for performance monitoring
typedef struct slist_stats {
    size_t length;
    size_t memory_used;
    size_t node_size;
    size_t data_size_hint;
} SListStats;

// ============================================================================
// Original API (modernized with better error handling)
// ============================================================================

// Legacy functions (backward compatible)
GIG_API void slist_prepend(slist_t **lst, void *data);
GIG_API void slist_free(slist_t **lst, void (*free_func)(void *));

// Enhanced legacy functions
GIG_API bool slist_append(slist_t **lst, void *data);
GIG_API bool slist_insert_at(slist_t **lst, size_t index, void *data);
GIG_API bool slist_remove(slist_t **lst, void *data, SListCompareFunc compare_func);
GIG_API bool slist_remove_at(slist_t **lst, size_t index);
GIG_API void *slist_get_at(const slist_t *lst, size_t index) GIG_PURE;
GIG_API size_t slist_length(const slist_t *lst) GIG_PURE;
GIG_API slist_t *slist_reverse(slist_t *lst);

// ============================================================================
// Modern SList API (enhanced implementation)
// ============================================================================

// Creation and destruction
GIG_API SList *slist_create(const SListConfig *config) GIG_MALLOC;
GIG_API SList *slist_create_simple(void) GIG_MALLOC;
GIG_API void slist_destroy(SList *list);
GIG_API SList *slist_copy(const SList *list) GIG_MALLOC;
GIG_API SList *slist_clone_deep(const SList *list) GIG_MALLOC;

// Basic operations
GIG_API bool slist_push_front(SList *list, void *data);
GIG_API bool slist_push_back(SList *list, void *data);
GIG_API void *slist_pop_front(SList *list);
GIG_API void *slist_pop_back(SList *list);

GIG_API bool slist_insert(SList *list, size_t index, void *data);
GIG_API void *slist_remove_first(SList *list);
GIG_API void *slist_remove_last(SList *list);
GIG_API bool slist_remove_data(SList *list, const void *data);
GIG_API void *slist_remove_index(SList *list, size_t index);

// Access operations
GIG_API void *slist_get(const SList *list, size_t index) GIG_PURE;
GIG_API void *slist_get_first(const SList *list) GIG_PURE;
GIG_API void *slist_get_last(const SList *list) GIG_PURE;
GIG_API bool slist_set(SList *list, size_t index, void *data);

// Search operations
GIG_API ssize_t slist_find(const SList *list, const void *data) GIG_PURE;
GIG_API ssize_t slist_find_custom(const SList *list, const void *data, 
                                 SListCompareFunc compare_func) GIG_PURE;
GIG_API bool slist_contains(const SList *list, const void *data) GIG_PURE;
GIG_API size_t slist_count(const SList *list, const void *data) GIG_PURE;

// List properties
GIG_API size_t slist_size(const SList *list) GIG_PURE;
GIG_API bool slist_is_empty(const SList *list) GIG_PURE;
GIG_API void slist_clear(SList *list);
GIG_API void slist_get_stats(const SList *list, SListStats *stats);

// List manipulation
GIG_API SList *slist_reverse_new(const SList *list) GIG_MALLOC;
GIG_API void slist_reverse_inplace(SList *list);
GIG_API SList *slist_sort(SList *list, SListCompareFunc compare_func) GIG_MALLOC;
GIG_API void slist_sort_inplace(SList *list, SListCompareFunc compare_func);
GIG_API SList *slist_concat(const SList *list1, const SList *list2) GIG_MALLOC;
GIG_API void slist_concat_inplace(SList *dest, const SList *src);

// Functional operations
GIG_API void slist_foreach(const SList *list, SListForeachFunc func, void *user_data);
GIG_API SList *slist_filter(const SList *list, SListFilterFunc filter_func, 
                           void *user_data) GIG_MALLOC;
GIG_API SList *slist_map(const SList *list, SListMapFunc map_func, 
                        void *user_data) GIG_MALLOC;
GIG_API void *slist_reduce(const SList *list, void *(*reduce_func)(void *acc, const void *data, void *user_data),
                          void *initial, void *user_data);

// Sublist operations
GIG_API SList *slist_slice(const SList *list, size_t start, size_t end) GIG_MALLOC;
GIG_API SList *slist_take(const SList *list, size_t count) GIG_MALLOC;
GIG_API SList *slist_drop(const SList *list, size_t count) GIG_MALLOC;
GIG_API SList *slist_take_while(const SList *list, SListFilterFunc predicate, 
                               void *user_data) GIG_MALLOC;
GIG_API SList *slist_drop_while(const SList *list, SListFilterFunc predicate, 
                               void *user_data) GIG_MALLOC;

// Set operations (treating list as set)
GIG_API SList *slist_union(const SList *list1, const SList *list2) GIG_MALLOC;
GIG_API SList *slist_intersection(const SList *list1, const SList *list2) GIG_MALLOC;
GIG_API SList *slist_difference(const SList *list1, const SList *list2) GIG_MALLOC;
GIG_API SList *slist_unique(const SList *list) GIG_MALLOC;
GIG_API void slist_unique_inplace(SList *list);

// ============================================================================
// Iterator API
// ============================================================================

GIG_API SListIterator slist_iter(const SList *list);
GIG_API SListIterator slist_iter_at(const SList *list, size_t index);
GIG_API bool slist_iter_has_next(const SListIterator *iter) GIG_PURE;
GIG_API void *slist_iter_next(SListIterator *iter);
GIG_API void *slist_iter_peek(const SListIterator *iter) GIG_PURE;
GIG_API bool slist_iter_remove(SListIterator *iter);
GIG_API bool slist_iter_insert_before(SListIterator *iter, void *data);
GIG_API bool slist_iter_insert_after(SListIterator *iter, void *data);

// ============================================================================
// Specialized Lists
// ============================================================================

// String list
typedef struct string_list StringList;
GIG_API StringList *string_list_create(void) GIG_MALLOC;
GIG_API void string_list_destroy(StringList *list);
GIG_API bool string_list_add(StringList *list, const char *str);
GIG_API const char *string_list_get(const StringList *list, size_t index) GIG_PURE;
GIG_API bool string_list_contains(const StringList *list, const char *str) GIG_PURE;
GIG_API char **string_list_to_array(const StringList *list, size_t *count) GIG_MALLOC;
GIG_API StringList *string_list_from_array(const char * const *strings, size_t count) GIG_MALLOC;

// Integer list
typedef struct int_list IntList;
GIG_API IntList *int_list_create(void) GIG_MALLOC;
GIG_API void int_list_destroy(IntList *list);
GIG_API bool int_list_add(IntList *list, int64_t value);
GIG_API int64_t int_list_get(const IntList *list, size_t index);
GIG_API bool int_list_contains(const IntList *list, int64_t value) GIG_PURE;
GIG_API int64_t *int_list_to_array(const IntList *list, size_t *count) GIG_MALLOC;
GIG_API IntList *int_list_from_array(const int64_t *values, size_t count) GIG_MALLOC;

// Pointer list (for managing object collections)
typedef struct ptr_list PtrList;
GIG_API PtrList *ptr_list_create(SListFreeFunc free_func) GIG_MALLOC;
GIG_API void ptr_list_destroy(PtrList *list);
GIG_API bool ptr_list_add(PtrList *list, void *ptr);
GIG_API void *ptr_list_get(const PtrList *list, size_t index) GIG_PURE;
GIG_API bool ptr_list_remove(PtrList *list, void *ptr);
GIG_API void **ptr_list_to_array(const PtrList *list, size_t *count) GIG_MALLOC;

// ============================================================================
// Configuration and Utilities
// ============================================================================

// Default configuration helpers
GIG_API SListConfig slist_config_default(void);
GIG_API SListConfig slist_config_strings(void);
GIG_API SListConfig slist_config_owned_data(SListFreeFunc free_func);

// Built-in comparison functions
GIG_API int slist_compare_int(const void *a, const void *b) GIG_PURE;
GIG_API int slist_compare_string(const void *a, const void *b) GIG_PURE;
GIG_API int slist_compare_pointer(const void *a, const void *b) GIG_PURE;

// Built-in copy functions
GIG_API void *slist_copy_string(const void *str) GIG_MALLOC;
GIG_API void *slist_copy_int(const void *value) GIG_MALLOC;

// Error handling
typedef enum {
    SLIST_ERROR_NONE = 0,
    SLIST_ERROR_OUT_OF_MEMORY,
    SLIST_ERROR_INVALID_ARGUMENT,
    SLIST_ERROR_INDEX_OUT_OF_BOUNDS,
    SLIST_ERROR_EMPTY_LIST,
    SLIST_ERROR_KEY_NOT_FOUND
} SListError;

GIG_API SListError slist_get_last_error(void);
GIG_API const char *slist_error_string(SListError error) GIG_PURE;

// Performance optimization
GIG_API void slist_optimize(SList *list);
GIG_API void slist_reserve_hint(SList *list, size_t expected_size);

// Debugging and validation
GIG_API bool slist_validate(const SList *list);
GIG_API void slist_print_stats(const SList *list, FILE *stream);

// ============================================================================
// Utility Macros
// ============================================================================

// Safe iteration macros
#define SLIST_FOREACH(list, var) \
    for (SListIterator _iter = slist_iter(list); \
         slist_iter_has_next(&_iter) && ((var) = slist_iter_next(&_iter), true); )

#define SLIST_FOREACH_SAFE(list, var, next_var) \
    for (slist_t *var = (list), *next_var = var ? var->next : NULL; \
         var != NULL; \
         var = next_var, next_var = var ? var->next : NULL)

// Type-safe list creation macros
#define SLIST_CREATE_TYPED(type, free_func) \
    slist_create(&(SListConfig){ \
        .copy_func = NULL, \
        .free_func = (free_func), \
        .compare_func = NULL, \
        .owns_data = ((free_func) != NULL) \
    })

// Convenient value insertion macros
#define slist_push_int(list, value) \
    do { \
        int64_t *_val = xmalloc(sizeof(int64_t)); \
        *_val = (value); \
        slist_push_back((list), _val); \
    } while(0)

#define slist_push_string(list, str) \
    slist_push_back((list), xstrdup(str))

// List comprehension-style macros
#define SLIST_COLLECT(list, var, expr) \
    ({ \
        SList *_result = slist_create_simple(); \
        SLIST_FOREACH(list, var) { \
            slist_push_back(_result, (expr)); \
        } \
        _result; \
    })

// Deprecated aliases for backward compatibility
GIG_DEPRECATED_API typedef slist_t slist_node_t;

#ifdef __cplusplus
}
#endif

#endif // CORE_SLIST_H
