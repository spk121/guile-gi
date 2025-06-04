// Copyright (C) 2022 Michael L. Gran

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

#ifndef CORE_MEM_H
#define CORE_MEM_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations for custom allocator support
typedef void *(*MemAllocFunc)(size_t size, void *user_data);
typedef void *(*MemCallocFunc)(size_t nmemb, size_t size, void *user_data);
typedef void *(*MemReallocFunc)(void *ptr, size_t size, void *user_data);
typedef void (*MemFreeFunc)(void *ptr, void *user_data);
typedef void (*MemErrorFunc)(const char *operation, size_t size, const char *file, 
                            int line, const char *func);

// Memory allocator configuration
typedef struct {
    MemAllocFunc alloc_func;
    MemCallocFunc calloc_func; 
    MemReallocFunc realloc_func;
    MemFreeFunc free_func;
    MemErrorFunc error_func;
    void *user_data;
    bool track_allocations;
    size_t alignment;
} MemAllocator;

// Memory statistics (when tracking is enabled)
typedef struct {
    size_t total_allocated;
    size_t total_freed;
    size_t current_allocated;
    size_t peak_allocated;
    size_t allocation_count;
    size_t free_count;
    size_t failed_allocations;
} MemStats;

// Core allocation functions (never return NULL - abort on failure)
void *xmalloc(size_t size) __attribute__((malloc, alloc_size(1)));
void *xcalloc(size_t nmemb, size_t size) __attribute__((malloc, alloc_size(1,2)));
void *xrealloc(void *ptr, size_t size) __attribute__((alloc_size(2)));
void xfree(void *ptr);

// String duplication functions
char *xstrdup(const char *str) __attribute__((malloc, nonnull(1)));
char *xstrndup(const char *str, size_t max_len) __attribute__((malloc, nonnull(1)));

// Memory duplication and operations
void *xmemdup(const void *mem, size_t len) __attribute__((malloc, nonnull(1), alloc_size(2)));
void *xmemdup0(const void *mem, size_t len) __attribute__((malloc, nonnull(1)));

// Aligned allocation (C11 aligned_alloc wrapper)
void *xaligned_alloc(size_t alignment, size_t size) __attribute__((malloc, alloc_align(1), alloc_size(2)));

// Safe allocation functions (return NULL on failure instead of aborting)
void *safe_malloc(size_t size) __attribute__((malloc, alloc_size(1)));
void *safe_calloc(size_t nmemb, size_t size) __attribute__((malloc, alloc_size(1,2)));
void *safe_realloc(void *ptr, size_t size) __attribute__((alloc_size(2)));
char *safe_strdup(const char *str) __attribute__((malloc));
char *safe_strndup(const char *str, size_t max_len) __attribute__((malloc));

// Array allocation with overflow checking
void *xmalloc_array(size_t nmemb, size_t size) __attribute__((malloc, alloc_size(1,2)));
void *xcalloc_array(size_t nmemb, size_t size) __attribute__((malloc, alloc_size(1,2)));
void *xrealloc_array(void *ptr, size_t nmemb, size_t size) __attribute__((alloc_size(2,3)));

// Memory pool allocation for efficient bulk operations
typedef struct MemPool MemPool;
MemPool *mem_pool_create(size_t block_size, size_t initial_blocks);
void *mem_pool_alloc(MemPool *pool, size_t size);
void mem_pool_free_all(MemPool *pool);
void mem_pool_destroy(MemPool *pool);

// Custom allocator management
void mem_set_allocator(const MemAllocator *allocator);
void mem_get_allocator(MemAllocator *allocator);
void mem_reset_allocator(void);

// Memory tracking and debugging
void mem_enable_tracking(bool enable);
bool mem_is_tracking_enabled(void);
void mem_get_stats(MemStats *stats);
void mem_reset_stats(void);
void mem_print_stats(FILE *stream);

// Memory validation and debugging
bool mem_is_valid_ptr(const void *ptr);
size_t mem_get_allocation_size(const void *ptr);
void mem_check_leaks(void);

// Utility macros for better ergonomics
#define XNEW(type) ((type *)xmalloc(sizeof(type)))
#define XNEW_ARRAY(type, n) ((type *)xmalloc_array((n), sizeof(type)))
#define XNEW0(type) ((type *)xcalloc(1, sizeof(type)))
#define XNEW0_ARRAY(type, n) ((type *)xcalloc_array((n), sizeof(type)))
#define XRENEW_ARRAY(ptr, type, n) ((type *)xrealloc_array((ptr), (n), sizeof(type)))

// Safe versions that return NULL on failure
#define SAFE_NEW(type) ((type *)safe_malloc(sizeof(type)))
#define SAFE_NEW_ARRAY(type, n) ((type *)safe_malloc_array((n), sizeof(type)))

// Memory clearing utilities
#define CLEAR_STRUCT(ptr) memset((ptr), 0, sizeof(*(ptr)))
#define CLEAR_ARRAY(ptr, n) memset((ptr), 0, (n) * sizeof(*(ptr)))

// Secure memory operations
void *xmalloc_secure(size_t size) __attribute__((malloc, alloc_size(1)));
void xfree_secure(void *ptr, size_t size);
char *xstrdup_secure(const char *str) __attribute__((malloc, nonnull(1)));

// Memory alignment utilities
#define ALIGN_UP(x, align) (((x) + (align) - 1) & ~((align) - 1))
#define ALIGN_DOWN(x, align) ((x) & ~((align) - 1))
#define IS_ALIGNED(x, align) (((x) & ((align) - 1)) == 0)

// Common alignment values
#define MEM_ALIGN_DEFAULT sizeof(void*)
#define MEM_ALIGN_CACHE_LINE 64
#define MEM_ALIGN_PAGE 4096

// Memory barriers and atomic operations support detection
bool mem_has_atomic_support(void);

// Error handling customization
typedef enum {
    MEM_ERROR_ABORT,    // Default: abort on error
    MEM_ERROR_RETURN,   // Return NULL on error  
    MEM_ERROR_CALLBACK  // Call custom error handler
} MemErrorMode;

void mem_set_error_mode(MemErrorMode mode);
MemErrorMode mem_get_error_mode(void);

#ifdef __cplusplus
}
#endif

#endif // CORE_MEM_H
