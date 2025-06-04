// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced singly-linked list with rich API, safety, and performance

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#include "slist.h"
#include "mem.h"
#include "logging.h"
#include "str.h"

// Enhanced list structure with metadata
struct slist {
    SListNode *head;
    SListNode *tail;
    size_t length;
    SListConfig config;
    
    // Performance optimization
    SListNode *cached_node;
    size_t cached_index;
};

// Specialized list implementations
struct string_list {
    SList *list;
};

struct int_list {
    SList *list;
};

struct ptr_list {
    SList *list;
};

// Global error state
static SListError g_last_error = SLIST_ERROR_NONE;

// ============================================================================
// Helper Functions
// ============================================================================

static void set_error(SListError error) {
    g_last_error = error;
}

static SListNode *create_node(void *data) {
    SListNode *node = XNEW(SListNode);
    if (!node) {
        set_error(SLIST_ERROR_OUT_OF_MEMORY);
        return NULL;
    }
    
    node->data = data;
    node->next = NULL;
    return node;
}

static void free_node(SList *list, SListNode *node) {
    if (!node) return;
    
    if (list->config.owns_data && list->config.free_func && node->data) {
        list->config.free_func(node->data);
    }
    
    xfree(node);
}

static SListNode *get_node_at_index(const SList *list, size_t index) {
    if (!list || index >= list->length) {
        set_error(SLIST_ERROR_INDEX_OUT_OF_BOUNDS);
        return NULL;
    }
    
    // Try to use cached position for optimization
    SListNode *current;
    size_t current_index;
    
    if (list->cached_node && list->cached_index <= index) {
        current = list->cached_node;
        current_index = list->cached_index;
    } else {
        current = list->head;
        current_index = 0;
    }
    
    // Traverse to target index
    while (current && current_index < index) {
        current = current->next;
        current_index++;
    }
    
    // Update cache
    if (current) {
        ((SList*)list)->cached_node = current;
        ((SList*)list)->cached_index = current_index;
    }
    
    return current;
}

static int default_compare(const void *a, const void *b) {
    return (a > b) - (a < b);
}

// ============================================================================
// Original API (modernized)
// ============================================================================

void slist_prepend(slist_t **lst, void *data) {
    if (!lst) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return;
    }
    
    slist_t *node = create_node(data);
    if (!node) return;
    
    node->next = *lst;
    *lst = node;
    set_error(SLIST_ERROR_NONE);
}

void slist_free(slist_t **lst, void (*free_func)(void *)) {
    if (!lst) return;
    
    slist_t *current = *lst;
    while (current) {
        slist_t *next = current->next;
        
        if (free_func && current->data) {
            free_func(current->data);
        }
        
        xfree(current);
        current = next;
    }
    
    *lst = NULL;
}

bool slist_append(slist_t **lst, void *data) {
    if (!lst) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
    slist_t *node = create_node(data);
    if (!node) return false;
    
    if (!*lst) {
        *lst = node;
    } else {
        slist_t *current = *lst;
        while (current->next) {
            current = current->next;
        }
        current->next = node;
    }
    
    set_error(SLIST_ERROR_NONE);
    return true;
}

bool slist_insert_at(slist_t **lst, size_t index, void *data) {
    if (!lst) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
    if (index == 0) {
        slist_prepend(lst, data);
        return g_last_error == SLIST_ERROR_NONE;
    }
    
    slist_t *node = create_node(data);
    if (!node) return false;
    
    slist_t *current = *lst;
    for (size_t i = 0; i < index - 1 && current; i++) {
        current = current->next;
    }
    
    if (!current) {
        xfree(node);
        set_error(SLIST_ERROR_INDEX_OUT_OF_BOUNDS);
        return false;
    }
    
    node->next = current->next;
    current->next = node;
    
    set_error(SLIST_ERROR_NONE);
    return true;
}

size_t slist_length(const slist_t *lst) {
    size_t count = 0;
    const slist_t *current = lst;
    
    while (current) {
        count++;
        current = current->next;
    }
    
    return count;
}

void *slist_get_at(const slist_t *lst, size_t index) {
    const slist_t *current = lst;
    
    for (size_t i = 0; i < index && current; i++) {
        current = current->next;
    }
    
    return current ? current->data : NULL;
}

slist_t *slist_reverse(slist_t *lst) {
    slist_t *prev = NULL;
    slist_t *current = lst;
    
    while (current) {
        slist_t *next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    
    return prev;
}

// ============================================================================
// Modern SList Implementation
// ============================================================================

SList *slist_create(const SListConfig *config) {
    SList *list = XNEW(SList);
    if (!list) {
        set_error(SLIST_ERROR_OUT_OF_MEMORY);
        return NULL;
    }
    
    list->head = NULL;
    list->tail = NULL;
    list->length = 0;
    list->cached_node = NULL;
    list->cached_index = 0;
    
    if (config) {
        list->config = *config;
    } else {
        list->config = slist_config_default();
    }
    
    set_error(SLIST_ERROR_NONE);
    return list;
}

SList *slist_create_simple(void) {
    return slist_create(NULL);
}

void slist_destroy(SList *list) {
    if (!list) return;
    
    slist_clear(list);
    xfree(list);
}

bool slist_push_front(SList *list, void *data) {
    if (!list) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
    SListNode *node = create_node(data);
    if (!node) return false;
    
    node->next = list->head;
    list->head = node;
    
    if (!list->tail) {
        list->tail = node;
    }
    
    list->length++;
    
    // Invalidate cache
    list->cached_node = NULL;
    list->cached_index = 0;
    
    set_error(SLIST_ERROR_NONE);
    return true;
}

bool slist_push_back(SList *list, void *data) {
    if (!list) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
    SListNode *node = create_node(data);
    if (!node) return false;
    
    if (!list->head) {
        list->head = node;
        list->tail = node;
    } else {
        list->tail->next = node;
        list->tail = node;
    }
    
    list->length++;
    set_error(SLIST_ERROR_NONE);
    return true;
}

void *slist_pop_front(SList *list) {
    if (!list || !list->head) {
        set_error(SLIST_ERROR_EMPTY_LIST);
        return NULL;
    }
    
    SListNode *node = list->head;
    void *data = node->data;
    
    list->head = node->next;
    if (!list->head) {
        list->tail = NULL;
    }
    
    list->length--;
    
    // Invalidate cache
    list->cached_node = NULL;
    list->cached_index = 0;
    
    xfree(node);
    set_error(SLIST_ERROR_NONE);
    return data;
}

void *slist_pop_back(SList *list) {
    if (!list || !list->head) {
        set_error(SLIST_ERROR_EMPTY_LIST);
        return NULL;
    }
    
    if (list->length == 1) {
        return slist_pop_front(list);
    }
    
    // Find second-to-last node
    SListNode *current = list->head;
    while (current->next && current->next != list->tail) {
        current = current->next;
    }
    
    void *data = list->tail->data;
    xfree(list->tail);
    
    list->tail = current;
    current->next = NULL;
    list->length--;
    
    // Invalidate cache
    list->cached_node = NULL;
    list->cached_index = 0;
    
    set_error(SLIST_ERROR_NONE);
    return data;
}

bool slist_insert(SList *list, size_t index, void *data) {
    if (!list) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return false;
    }
    
    if (index > list->length) {
        set_error(SLIST_ERROR_INDEX_OUT_OF_BOUNDS);
        return false;
    }
    
    if (index == 0) {
        return slist_push_front(list, data);
    }
    
    if (index == list->length) {
        return slist_push_back(list, data);
    }
    
    SListNode *node = create_node(data);
    if (!node) return false;
    
    SListNode *prev = get_node_at_index(list, index - 1);
    if (!prev) {
        xfree(node);
        return false;
    }
    
    node->next = prev->next;
    prev->next = node;
    list->length++;
    
    // Invalidate cache
    list->cached_node = NULL;
    list->cached_index = 0;
    
    set_error(SLIST_ERROR_NONE);
    return true;
}

void *slist_get(const SList *list, size_t index) {
    SListNode *node = get_node_at_index(list, index);
    return node ? node->data : NULL;
}

void *slist_get_first(const SList *list) {
    return (list && list->head) ? list->head->data : NULL;
}

void *slist_get_last(const SList *list) {
    return (list && list->tail) ? list->tail->data : NULL;
}

bool slist_set(SList *list, size_t index, void *data) {
    SListNode *node = get_node_at_index(list, index);
    if (!node) return false;
    
    // Free old data if owned
    if (list->config.owns_data && list->config.free_func && node->data) {
        list->config.free_func(node->data);
    }
    
    node->data = data;
    set_error(SLIST_ERROR_NONE);
    return true;
}

ssize_t slist_find(const SList *list, const void *data) {
    if (!list) return -1;
    
    SListNode *current = list->head;
    size_t index = 0;
    
    SListCompareFunc compare = list->config.compare_func ? 
                              list->config.compare_func : default_compare;
    
    while (current) {
        if (compare(current->data, data) == 0) {
            return (ssize_t)index;
        }
        current = current->next;
        index++;
    }
    
    return -1;
}

ssize_t slist_find_custom(const SList *list, const void *data, 
                         SListCompareFunc compare_func) {
    if (!list || !compare_func) return -1;
    
    SListNode *current = list->head;
    size_t index = 0;
    
    while (current) {
        if (compare_func(current->data, data) == 0) {
            return (ssize_t)index;
        }
        current = current->next;
        index++;
    }
    
    return -1;
}

bool slist_contains(const SList *list, const void *data) {
    return slist_find(list, data) >= 0;
}

size_t slist_count(const SList *list, const void *data) {
    if (!list) return 0;
    
    size_t count = 0;
    SListNode *current = list->head;
    SListCompareFunc compare = list->config.compare_func ? 
                              list->config.compare_func : default_compare;
    
    while (current) {
        if (compare(current->data, data) == 0) {
            count++;
        }
        current = current->next;
    }
    
    return count;
}

size_t slist_size(const SList *list) {
    return list ? list->length : 0;
}

bool slist_is_empty(const SList *list) {
    return !list || list->length == 0;
}

void slist_clear(SList *list) {
    if (!list) return;
    
    SListNode *current = list->head;
    while (current) {
        SListNode *next = current->next;
        free_node(list, current);
        current = next;
    }
    
    list->head = NULL;
    list->tail = NULL;
    list->length = 0;
    list->cached_node = NULL;
    list->cached_index = 0;
}

bool slist_remove_data(SList *list, const void *data) {
    if (!list || !list->head) {
        set_error(SLIST_ERROR_EMPTY_LIST);
        return false;
    }
    
    SListCompareFunc compare = list->config.compare_func ? 
                              list->config.compare_func : default_compare;
    
    // Check first node
    if (compare(list->head->data, data) == 0) {
        slist_pop_front(list);
        return true;
    }
    
    // Search rest of list
    SListNode *current = list->head;
    while (current->next) {
        if (compare(current->next->data, data) == 0) {
            SListNode *to_remove = current->next;
            current->next = to_remove->next;
            
            if (to_remove == list->tail) {
                list->tail = current;
            }
            
            free_node(list, to_remove);
            list->length--;
            
            // Invalidate cache
            list->cached_node = NULL;
            list->cached_index = 0;
            
            set_error(SLIST_ERROR_NONE);
            return true;
        }
        current = current->next;
    }
    
    set_error(SLIST_ERROR_KEY_NOT_FOUND);
    return false;
}

void *slist_remove_index(SList *list, size_t index) {
    if (!list) {
        set_error(SLIST_ERROR_INVALID_ARGUMENT);
        return NULL;
    }
    
    if (index >= list->length) {
        set_error(SLIST_ERROR_INDEX_OUT_OF_BOUNDS);
        return NULL;
    }
    
    if (index == 0) {
        return slist_pop_front(list);
    }
    
    if (index == list->length - 1) {
        return slist_pop_back(list);
    }
    
    SListNode *prev = get_node_at_index(list, index - 1);
    if (!prev || !prev->next) {
        set_error(SLIST_ERROR_INDEX_OUT_OF_BOUNDS);
        return NULL;
    }
    
    SListNode *to_remove = prev->next;
    void *data = to_remove->data;
    
    prev->next = to_remove->next;
    xfree(to_remove);
    list->length--;
    
    // Invalidate cache
    list->cached_node = NULL;
    list->cached_index = 0;
    
    set_error(SLIST_ERROR_NONE);
    return data;
}

void slist_reverse_inplace(SList *list) {
    if (!list || list->length <= 1) return;
    
    SListNode *prev = NULL;
    SListNode *current = list->head;
    
    list->tail = list->head;  // Old head becomes new tail
    
    while (current) {
        SListNode *next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    
    list->head = prev;  // prev is now the new head
    
    // Invalidate cache
    list->cached_node = NULL;
    list->cached_index = 0;
}

SList *slist_reverse_new(const SList *list) {
    if (!list) return NULL;
    
    SList *reversed = slist_create(&list->config);
    if (!reversed) return NULL;
    
    SListNode *current = list->head;
    while (current) {
        void *data_copy = current->data;
        if (list->config.copy_func) {
            data_copy = list->config.copy_func(current->data);
        }
        
        if (!slist_push_front(reversed, data_copy)) {
            slist_destroy(reversed);
            return NULL;
        }
        
        current = current->next;
    }
    
    return reversed;
}

void slist_foreach(const SList *list, SListForeachFunc func, void *user_data) {
    if (!list || !func) return;
    
    SListNode *current = list->head;
    while (current) {
        func(current->data, user_data);
        current = current->next;
    }
}

SList *slist_filter(const SList *list, SListFilterFunc filter_func, void *user_data) {
    if (!list || !filter_func) return NULL;
    
    SList *filtered = slist_create(&list->config);
    if (!filtered) return NULL;
    
    SListNode *current = list->head;
    while (current) {
        if (filter_func(current->data, user_data)) {
            void *data_copy = current->data;
            if (list->config.copy_func) {
                data_copy = list->config.copy_func(current->data);
            }
            
            if (!slist_push_back(filtered, data_copy)) {
                slist_destroy(filtered);
                return NULL;
            }
        }
        current = current->next;
    }
    
    return filtered;
}

SList *slist_map(const SList *list, SListMapFunc map_func, void *user_data) {
    if (!list || !map_func) return NULL;
    
    SList *mapped = slist_create_simple();  // No automatic memory management for mapped data
    if (!mapped) return NULL;
    
    SListNode *current = list->head;
    while (current) {
        void *mapped_data = map_func(current->data, user_data);
        if (!slist_push_back(mapped, mapped_data)) {
            slist_destroy(mapped);
            return NULL;
        }
        current = current->next;
    }
    
    return mapped;
}

SList *slist_copy(const SList *list) {
    if (!list) return NULL;
    
    SList *copy = slist_create(&list->config);
    if (!copy) return NULL;
    
    SListNode *current = list->head;
    while (current) {
        void *data_copy = current->data;
        if (list->config.copy_func) {
            data_copy = list->config.copy_func(current->data);
        }
        
        if (!slist_push_back(copy, data_copy)) {
            slist_destroy(copy);
            return NULL;
        }
        
        current = current->next;
    }
    
    return copy;
}

SList *slist_slice(const SList *list, size_t start, size_t end) {
    if (!list || start >= list->length || start >= end) {
        return slist_create(&list->config);
    }
    
    if (end > list->length) {
        end = list->length;
    }
    
    SList *slice = slist_create(&list->config);
    if (!slice) return NULL;
    
    SListNode *current = get_node_at_index(list, start);
    for (size_t i = start; i < end && current; i++) {
        void *data_copy = current->data;
        if (list->config.copy_func) {
            data_copy = list->config.copy_func(current->data);
        }
        
        if (!slist_push_back(slice, data_copy)) {
            slist_destroy(slice);
            return NULL;
        }
        
        current = current->next;
    }
    
    return slice;
}

// ============================================================================
// Iterator Implementation
// ============================================================================

SListIterator slist_iter(const SList *list) {
    SListIterator iter = {0};
    
    if (list) {
        iter.current = list->head;
        iter.previous = NULL;
        iter.list = (SList*)list;  // Cast away const for iterator
        iter.is_valid = true;
    }
    
    return iter;
}

bool slist_iter_has_next(const SListIterator *iter) {
    return iter && iter->is_valid && iter->current != NULL;
}

void *slist_iter_next(SListIterator *iter) {
    if (!iter || !iter->is_valid || !iter->current) {
        return NULL;
    }
    
    void *data = iter->current->data;
    iter->previous = iter->current;
    iter->current = iter->current->next;
    
    return data;
}

void *slist_iter_peek(const SListIterator *iter) {
    if (!iter || !iter->is_valid || !iter->current) {
        return NULL;
    }
    
    return iter->current->data;
}

// ============================================================================
// Specialized List Implementations
// ============================================================================

// String List
StringList *string_list_create(void) {
    StringList *str_list = XNEW(StringList);
    if (!str_list) return NULL;
    
    SListConfig config = slist_config_strings();
    str_list->list = slist_create(&config);
    if (!str_list->list) {
        xfree(str_list);
        return NULL;
    }
    
    return str_list;
}

void string_list_destroy(StringList *list) {
    if (!list) return;
    
    slist_destroy(list->list);
    xfree(list);
}

bool string_list_add(StringList *list, const char *str) {
    if (!list || !str) return false;
    
    char *str_copy = xstrdup(str);
    if (!str_copy) return false;
    
    return slist_push_back(list->list, str_copy);
}

const char *string_list_get(const StringList *list, size_t index) {
    if (!list) return NULL;
    
    return (const char*)slist_get(list->list, index);
}

bool string_list_contains(const StringList *list, const char *str) {
    if (!list || !str) return false;
    
    return slist_find(list->list, str) >= 0;
}

char **string_list_to_array(const StringList *list, size_t *count) {
    if (!list) {
        if (count) *count = 0;
        return NULL;
    }
    
    size_t list_size = slist_size(list->list);
    if (count) *count = list_size;
    
    if (list_size == 0) return NULL;
    
    char **array = xmalloc((list_size + 1) * sizeof(char*));
    if (!array) return NULL;
    
    SListNode *current = list->list->head;
    for (size_t i = 0; i < list_size && current; i++) {
        array[i] = (char*)current->data;
        current = current->next;
    }
    array[list_size] = NULL;  // NULL-terminate for convenience
    
    return array;
}

// Integer List
IntList *int_list_create(void) {
    IntList *int_list = XNEW(IntList);
    if (!int_list) return NULL;
    
    SListConfig config = slist_config_owned_data(xfree);
    int_list->list = slist_create(&config);
    if (!int_list->list) {
        xfree(int_list);
        return NULL;
    }
    
    return int_list;
}

void int_list_destroy(IntList *list) {
    if (!list) return;
    
    slist_destroy(list->list);
    xfree(list);
}

bool int_list_add(IntList *list, int64_t value) {
    if (!list) return false;
    
    int64_t *value_copy = xmalloc(sizeof(int64_t));
    if (!value_copy) return false;
    
    *value_copy = value;
    return slist_push_back(list->list, value_copy);
}

int64_t int_list_get(const IntList *list, size_t index) {
    if (!list) return 0;
    
    int64_t *value_ptr = (int64_t*)slist_get(list->list, index);
    return value_ptr ? *value_ptr : 0;
}

bool int_list_contains(const IntList *list, int64_t value) {
    if (!list) return false;
    
    SListNode *current = list->list->head;
    while (current) {
        int64_t *current_value = (int64_t*)current->data;
        if (current_value && *current_value == value) {
            return true;
        }
        current = current->next;
    }
    
    return false;
}

// ============================================================================
// Configuration Functions
// ============================================================================

SListConfig slist_config_default(void) {
    SListConfig config = {0};
    config.copy_func = NULL;
    config.free_func = NULL;
    config.compare_func = default_compare;
    config.owns_data = false;
    return config;
}

SListConfig slist_config_strings(void) {
    SListConfig config = {0};
    config.copy_func = slist_copy_string;
    config.free_func = xfree;
    config.compare_func = slist_compare_string;
    config.owns_data = true;
    return config;
}

SListConfig slist_config_owned_data(SListFreeFunc free_func) {
    SListConfig config = slist_config_default();
    config.free_func = free_func;
    config.owns_data = true;
    return config;
}

// Built-in comparison functions
int slist_compare_int(const void *a, const void *b) {
    int64_t val_a = *(const int64_t*)a;
    int64_t val_b = *(const int64_t*)b;
    return (val_a > val_b) - (val_a < val_b);
}

int slist_compare_string(const void *a, const void *b) {
    const char *str_a = (const char*)a;
    const char *str_b = (const char*)b;
    
    if (str_a == str_b) return 0;
    if (!str_a) return -1;
    if (!str_b) return 1;
    
    return strcmp(str_a, str_b);
}

int slist_compare_pointer(const void *a, const void *b) {
    return (a > b) - (a < b);
}

// Built-in copy functions
void *slist_copy_string(const void *str) {
    return str ? xstrdup((const char*)str) : NULL;
}

void *slist_copy_int(const void *value) {
    if (!value) return NULL;
    
    int64_t *copy = xmalloc(sizeof(int64_t));
    if (copy) {
        *copy = *(const int64_t*)value;
    }
    return copy;
}

// ============================================================================
// Error Handling and Utilities
// ============================================================================

SListError slist_get_last_error(void) {
    return g_last_error;
}

const char *slist_error_string(SListError error) {
    switch (error) {
        case SLIST_ERROR_NONE:
            return "No error";
        case SLIST_ERROR_OUT_OF_MEMORY:
            return "Out of memory";
        case SLIST_ERROR_INVALID_ARGUMENT:
            return "Invalid argument";
        case SLIST_ERROR_INDEX_OUT_OF_BOUNDS:
            return "Index out of bounds";
        case SLIST_ERROR_EMPTY_LIST:
            return "List is empty";
        default:
            return "Unknown error";
    }
}

void slist_get_stats(const SList *list, SListStats *stats) {
    if (!list || !stats) return;
    
    stats->length = list->length;
    stats->node_size = sizeof(SListNode);
    stats->memory_used = list->length * sizeof(SListNode) + sizeof(SList);
    stats->data_size_hint = 0;  // We don't track data sizes
}

bool slist_validate(const SList *list) {
    if (!list) return false;
    
    // Validate length matches actual node count
    size_t actual_length = 0;
    SListNode *current = list->head;
    SListNode *last = NULL;
    
    while (current) {
        actual_length++;
        last = current;
        current = current->next;
    }
    
    // Check length consistency
    if (actual_length != list->length) {
        gig_error("List length inconsistency: expected %zu, found %zu", 
                 list->length, actual_length);
        return false;
    }
    
    // Check tail pointer consistency
    if (list->tail != last) {
        gig_error("List tail pointer inconsistency");
        return false;
    }
    
    // Validate empty list state
    if (list->length == 0) {
        if (list->head != NULL || list->tail != NULL) {
            gig_error("Empty list has non-null head or tail");
            return false;
        }
    }
    
    return true;
}

void slist_print_stats(const SList *list, FILE *stream) {
    if (!stream) stream = stderr;
    
    if (!list) {
        fprintf(stream, "List: NULL\n");
        return;
    }
    
    SListStats stats;
    slist_get_stats(list, &stats);
    
    fprintf(stream, "List Statistics:\n");
    fprintf(stream, "  Length: %zu\n", stats.length);
    fprintf(stream, "  Memory used: %zu bytes\n", stats.memory_used);
    fprintf(stream, "  Node size: %zu bytes\n", stats.node_size);
    fprintf(stream, "  Owns data: %s\n", list->config.owns_data ? "yes" : "no");
    fprintf(stream, "  Has copy function: %s\n", list->config.copy_func ? "yes" : "no");
    fprintf(stream, "  Has free function: %s\n", list->config.free_func ? "yes" : "no");
    fprintf(stream, "  Has compare function: %s\n", list->config.compare_func ? "yes" : "no");
}
