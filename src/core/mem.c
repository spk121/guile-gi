// Copyright (C) 2019, 2020, 2021, 2022 Michael L. Gran
// Modernized 2025 - Enhanced memory management with tracking, custom allocators, and safety features

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

#define _XOPEN_SOURCE 700
#define _DEFAULT_SOURCE

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>
#include <limits.h>
#include "mem.h"
#include "logging.h"

// Platform-specific includes
#ifdef __GLIBC__
#include <malloc.h>
#endif

#if defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#define HAVE_UNISTD_H 1
#define HAVE_MLOCK 1
#elif defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
#include <windows.h>
#include <io.h>
#define HAVE_WINDOWS_H 1
// Windows doesn't have mlock, but has VirtualLock
#define HAVE_VIRTUALLOCK 1
#else
// Other platforms - assume POSIX compliance
#include <unistd.h>
#define HAVE_UNISTD_H 1
#endif

#ifdef HAVE_ALIGNED_ALLOC
#include <stdalign.h>
#endif

// Platform-specific secure memory clearing
#ifdef HAVE_SECURE_MEMSET
#ifdef __GLIBC__
#include <string.h>
#define secure_memset(ptr, val, len) explicit_bzero(ptr, len)
#elif defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
#define secure_memset(ptr, val, len) SecureZeroMemory(ptr, len)
#else
// Fallback implementation that hopefully won't be optimized away
static void secure_memset(void *ptr, int val, size_t len) {
    volatile unsigned char *p = ptr;
    while (len--) *p++ = val;
}
#endif
#else
#define secure_memset(ptr, val, len) memset(ptr, val, len)
#endif

// Cross-platform function implementations
#if defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)

// Windows implementation of strnlen (if not available)
#define HAVE_STRNLEN
#ifndef HAVE_STRNLEN
static size_t strnlen(const char *s, size_t maxlen) {
    size_t len = 0;
    while (len < maxlen && s[len] != '\0') {
        len++;
    }
    return len;
}
#endif

// Windows implementation of strndup
static char *strndup(const char *s, size_t n) {
    size_t len = strnlen(s, n);
    char *new_str = malloc(len + 1);
    
    if (new_str == NULL) return NULL;
    
    new_str[len] = '\0';
    return memcpy(new_str, s, len);
}

// Windows workaround for isatty-like functionality
static int is_console(int fd) {
    HANDLE handle;
    DWORD mode;
    
    if (fd == 0) {
        handle = GetStdHandle(STD_INPUT_HANDLE);
    } else if (fd == 1) {
        handle = GetStdHandle(STD_OUTPUT_HANDLE);
    } else if (fd == 2) {
        handle = GetStdHandle(STD_ERROR_HANDLE);
    } else {
        return 0;
    }
    
    return GetConsoleMode(handle, &mode) != 0;
}

#define isatty(fd) is_console(fd)

// Define standard file descriptor constants for Windows
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

#endif // Windows platform

// Configuration constants
#define MEM_CANARY_VALUE 0xDEADBEEF
#define MEM_FREED_PATTERN 0xDD
#define MEM_ALLOC_PATTERN 0xCC
#define MEM_MAX_ALIGN 4096

// Memory tracking structures
typedef struct MemHeader {
    uint32_t canary;
    size_t size;
    const char *file;
    int line;
    const char *func;
    struct MemHeader *next;
    struct MemHeader *prev;
} MemHeader;

typedef struct MemFooter {
    uint32_t canary;
} MemFooter;

// Memory pool implementation  
struct MemPool {
    size_t block_size;
    size_t blocks_per_chunk;
    size_t chunk_size;
    void *current_chunk;
    size_t current_offset;
    void **chunks;
    size_t num_chunks;
    size_t max_chunks;
};

// Global state
static struct {
    MemAllocator allocator;
    MemStats stats;
    MemHeader *allocation_list;
    bool tracking_enabled;
    MemErrorMode error_mode;
    bool initialized;
} g_mem_state = {
    .allocator = {0},
    .stats = {0},
    .allocation_list = NULL,
    .tracking_enabled = false,
    .error_mode = MEM_ERROR_ABORT,
    .initialized = false
};

// Thread safety (can be customized based on threading library)
#ifndef MEM_USE_LOCKS
#define MEM_USE_LOCKS 0
#define MEM_LOCK(x)
#define MEM_UNLOCK(x)
#endif

// Default error handler
static void default_error_handler(const char *operation, size_t size, 
                                 const char *file, int line, const char *func) {
    if (file && func) {
        gig_error("Memory %s failed: size=%zu at %s:%d in %s()", 
                 operation, size, file, line, func);
    } else {
        gig_error("Memory %s failed: size=%zu", operation, size);
    }
}

// Initialize memory subsystem
static void mem_init(void) {
    if (g_mem_state.initialized) return;
    
    g_mem_state.allocator.alloc_func = (MemAllocFunc)malloc;
    g_mem_state.allocator.calloc_func = (MemCallocFunc)calloc;
    g_mem_state.allocator.realloc_func = (MemReallocFunc)realloc;
    g_mem_state.allocator.free_func = (MemFreeFunc)free;
    g_mem_state.allocator.error_func = default_error_handler;
    g_mem_state.allocator.user_data = NULL;
    g_mem_state.allocator.track_allocations = false;
    g_mem_state.allocator.alignment = MEM_ALIGN_DEFAULT;
    
    g_mem_state.initialized = true;
}

// Overflow-safe multiplication check
static bool mul_overflow_check(size_t a, size_t b, size_t *result) {
    if (a == 0 || b == 0) {
        *result = 0;
        return false;
    }
    
    if (a > SIZE_MAX / b) {
        return true; // Overflow
    }
    
    *result = a * b;
    return false;
}

// Add allocation to tracking list
static void track_allocation(void *ptr, size_t size, const char *file, 
                           int line, const char *func) {
    if (!g_mem_state.tracking_enabled || !ptr) return;
    
    MemHeader *header = (MemHeader *)((char *)ptr - sizeof(MemHeader));
    header->canary = MEM_CANARY_VALUE;
    header->size = size;
    header->file = file;
    header->line = line;
    header->func = func;
    
    MEM_LOCK();
    
    // Add to doubly-linked list
    header->next = g_mem_state.allocation_list;
    header->prev = NULL;
    if (g_mem_state.allocation_list) {
        g_mem_state.allocation_list->prev = header;
    }
    g_mem_state.allocation_list = header;
    
    // Update statistics
    g_mem_state.stats.total_allocated += size;
    g_mem_state.stats.current_allocated += size;
    g_mem_state.stats.allocation_count++;
    
    if (g_mem_state.stats.current_allocated > g_mem_state.stats.peak_allocated) {
        g_mem_state.stats.peak_allocated = g_mem_state.stats.current_allocated;
    }
    
    // Add footer canary
    MemFooter *footer = (MemFooter *)((char *)ptr + size);
    footer->canary = MEM_CANARY_VALUE;
    
    MEM_UNLOCK();
}

// Remove allocation from tracking list
static void untrack_allocation(void *ptr) {
    if (!g_mem_state.tracking_enabled || !ptr) return;
    
    MemHeader *header = (MemHeader *)((char *)ptr - sizeof(MemHeader));
    
    MEM_LOCK();
    
    // Validate canaries
    if (header->canary != MEM_CANARY_VALUE) {
        gig_error("Memory corruption detected: invalid header canary at %p", ptr);
        abort();
    }
    
    MemFooter *footer = (MemFooter *)((char *)ptr + header->size);
    if (footer->canary != MEM_CANARY_VALUE) {
        gig_error("Memory corruption detected: invalid footer canary at %p", ptr);
        abort();
    }
    
    // Remove from list
    if (header->prev) {
        header->prev->next = header->next;
    } else {
        g_mem_state.allocation_list = header->next;
    }
    
    if (header->next) {
        header->next->prev = header->prev;
    }
    
    // Update statistics
    g_mem_state.stats.total_freed += header->size;
    g_mem_state.stats.current_allocated -= header->size;
    g_mem_state.stats.free_count++;
    
    MEM_UNLOCK();
    
    // Clear the freed memory
    memset(ptr, MEM_FREED_PATTERN, header->size);
}

// Handle memory allocation errors
static void handle_error(const char *operation, size_t size, 
                        const char *file, int line, const char *func) {
    MEM_LOCK();
    g_mem_state.stats.failed_allocations++;
    MEM_UNLOCK();
    
    switch (g_mem_state.error_mode) {
        case MEM_ERROR_ABORT:
            if (g_mem_state.allocator.error_func) {
                g_mem_state.allocator.error_func(operation, size, file, line, func);
            }
            abort();
            break;
            
        case MEM_ERROR_RETURN:
            // Just return - caller handles NULL
            break;
            
        case MEM_ERROR_CALLBACK:
            if (g_mem_state.allocator.error_func) {
                g_mem_state.allocator.error_func(operation, size, file, line, func);
            }
            break;
    }
}

// Internal allocation function
static void *internal_alloc(size_t size, bool zero_init, const char *file, 
                           int line, const char *func) {
    if (!g_mem_state.initialized) mem_init();
    
    if (size == 0) size = 1; // Avoid zero-size allocations
    
    size_t total_size = size;
    size_t offset = 0;
    
    // Add space for tracking headers if enabled
    if (g_mem_state.tracking_enabled) {
        total_size += sizeof(MemHeader) + sizeof(MemFooter);
        offset = sizeof(MemHeader);
    }
    
    void *raw_ptr;
    if (zero_init) {
        raw_ptr = g_mem_state.allocator.calloc_func(1, total_size, 
                                                   g_mem_state.allocator.user_data);
    } else {
        raw_ptr = g_mem_state.allocator.alloc_func(total_size, 
                                                  g_mem_state.allocator.user_data);
    }
    
    if (!raw_ptr) {
        handle_error(zero_init ? "calloc" : "malloc", size, file, line, func);
        return NULL;
    }
    
    void *user_ptr = (char *)raw_ptr + offset;
    
    // Initialize allocated memory with pattern (for debugging)
    if (!zero_init && g_mem_state.tracking_enabled) {
        memset(user_ptr, MEM_ALLOC_PATTERN, size);
    }
    
    if (g_mem_state.tracking_enabled) {
        track_allocation(user_ptr, size, file, line, func);
    }
    
    return user_ptr;
}

// Core allocation functions
void *xmalloc(size_t size) {
    void *ptr = internal_alloc(size, false, __FILE__, __LINE__, __func__);
    if (!ptr && g_mem_state.error_mode == MEM_ERROR_ABORT) {
        abort(); // Should never reach here, but just in case
    }
    return ptr;
}

void *xcalloc(size_t nmemb, size_t size) {
    size_t total;
    if (mul_overflow_check(nmemb, size, &total)) {
        handle_error("calloc", SIZE_MAX, __FILE__, __LINE__, __func__);
        return NULL;
    }
    
    void *ptr = internal_alloc(total, true, __FILE__, __LINE__, __func__);
    if (!ptr && g_mem_state.error_mode == MEM_ERROR_ABORT) {
        abort();
    }
    return ptr;
}

void *xrealloc(void *ptr, size_t size) {
    if (!g_mem_state.initialized) mem_init();
    
    if (!ptr) {
        return xmalloc(size);
    }
    
    if (size == 0) {
        xfree(ptr);
        return NULL;
    }
    
    size_t old_size = 0;
    void *raw_ptr = ptr;
    size_t offset = 0;
    
    if (g_mem_state.tracking_enabled) {
        untrack_allocation(ptr);
        raw_ptr = (char *)ptr - sizeof(MemHeader);
        offset = sizeof(MemHeader);
        MemHeader *header = (MemHeader *)raw_ptr;
        old_size = header->size;
    }
    
    size_t total_size = size;
    if (g_mem_state.tracking_enabled) {
        total_size += sizeof(MemHeader) + sizeof(MemFooter);
    }
    
    void *new_raw_ptr = g_mem_state.allocator.realloc_func(raw_ptr, total_size,
                                                          g_mem_state.allocator.user_data);
    
    if (!new_raw_ptr) {
        handle_error("realloc", size, __FILE__, __LINE__, __func__);
        return NULL;
    }
    
    void *new_user_ptr = (char *)new_raw_ptr + offset;
    
    if (g_mem_state.tracking_enabled) {
        track_allocation(new_user_ptr, size, __FILE__, __LINE__, __func__);
        
        // Clear newly allocated memory if size increased
        if (size > old_size) {
            memset((char *)new_user_ptr + old_size, MEM_ALLOC_PATTERN, size - old_size);
        }
    }
    
    return new_user_ptr;
}

void xfree(void *ptr) {
    if (!ptr) return;
    
    if (!g_mem_state.initialized) mem_init();
    
    void *raw_ptr = ptr;
    
    if (g_mem_state.tracking_enabled) {
        untrack_allocation(ptr);
        raw_ptr = (char *)ptr - sizeof(MemHeader);
    }
    
    g_mem_state.allocator.free_func(raw_ptr, g_mem_state.allocator.user_data);
}

// String duplication functions
char *xstrdup(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str) + 1;
    char *new_str = xmalloc(len);
    if (new_str) {
        memcpy(new_str, str, len);
    }
    return new_str;
}

char *xstrndup(const char *str, size_t max_len) {
    if (!str) return NULL;
    
    size_t len = strnlen(str, max_len);
    char *new_str = xmalloc(len + 1);
    if (new_str) {
        memcpy(new_str, str, len);
        new_str[len] = '\0';
    }
    return new_str;
}

// Memory duplication functions
void *xmemdup(const void *mem, size_t len) {
    if (!mem || len == 0) return NULL;
    
    void *new_mem = xmalloc(len);
    if (new_mem) {
        memcpy(new_mem, mem, len);
    }
    return new_mem;
}

void *xmemdup0(const void *mem, size_t len) {
    if (!mem) return NULL;
    
    void *new_mem = xmalloc(len + 1);
    if (new_mem) {
        memcpy(new_mem, mem, len);
        ((char *)new_mem)[len] = '\0';
    }
    return new_mem;
}

// Array allocation with overflow checking
void *xmalloc_array(size_t nmemb, size_t size) {
    size_t total;
    if (mul_overflow_check(nmemb, size, &total)) {
        handle_error("malloc_array", SIZE_MAX, __FILE__, __LINE__, __func__);
        return NULL;
    }
    return xmalloc(total);
}

void *xcalloc_array(size_t nmemb, size_t size) {
    return xcalloc(nmemb, size);
}

void *xrealloc_array(void *ptr, size_t nmemb, size_t size) {
    size_t total;
    if (mul_overflow_check(nmemb, size, &total)) {
        handle_error("realloc_array", SIZE_MAX, __FILE__, __LINE__, __func__);
        return NULL;
    }
    return xrealloc(ptr, total);
}

// Safe allocation functions (return NULL instead of aborting)
void *safe_malloc(size_t size) {
    MemErrorMode old_mode = g_mem_state.error_mode;
    g_mem_state.error_mode = MEM_ERROR_RETURN;
    void *ptr = internal_alloc(size, false, __FILE__, __LINE__, __func__);
    g_mem_state.error_mode = old_mode;
    return ptr;
}

void *safe_calloc(size_t nmemb, size_t size) {
    MemErrorMode old_mode = g_mem_state.error_mode;
    g_mem_state.error_mode = MEM_ERROR_RETURN;
    void *ptr = xcalloc(nmemb, size);
    g_mem_state.error_mode = old_mode;
    return ptr;
}

void *safe_realloc(void *ptr, size_t size) {
    MemErrorMode old_mode = g_mem_state.error_mode;
    g_mem_state.error_mode = MEM_ERROR_RETURN;
    void *new_ptr = xrealloc(ptr, size);
    g_mem_state.error_mode = old_mode;
    return new_ptr;
}

char *safe_strdup(const char *str) {
    MemErrorMode old_mode = g_mem_state.error_mode;
    g_mem_state.error_mode = MEM_ERROR_RETURN;
    char *new_str = xstrdup(str);
    g_mem_state.error_mode = old_mode;
    return new_str;
}

char *safe_strndup(const char *str, size_t max_len) {
    MemErrorMode old_mode = g_mem_state.error_mode;
    g_mem_state.error_mode = MEM_ERROR_RETURN;
    char *new_str = xstrndup(str, max_len);
    g_mem_state.error_mode = old_mode;
    return new_str;
}

// Configuration functions
void mem_set_allocator(const MemAllocator *allocator) {
    if (!allocator) return;
    
    MEM_LOCK();
    g_mem_state.allocator = *allocator;
    g_mem_state.initialized = true;
    MEM_UNLOCK();
}

void mem_get_allocator(MemAllocator *allocator) {
    if (!allocator) return;
    
    if (!g_mem_state.initialized) mem_init();
    
    MEM_LOCK();
    *allocator = g_mem_state.allocator;
    MEM_UNLOCK();
}

void mem_reset_allocator(void) {
    MEM_LOCK();
    g_mem_state.initialized = false;
    MEM_UNLOCK();
    mem_init();
}

// Memory tracking functions
void mem_enable_tracking(bool enable) {
    MEM_LOCK();
    g_mem_state.tracking_enabled = enable;
    MEM_UNLOCK();
}

bool mem_is_tracking_enabled(void) {
    return g_mem_state.tracking_enabled;
}

void mem_get_stats(MemStats *stats) {
    if (!stats) return;
    
    MEM_LOCK();
    *stats = g_mem_state.stats;
    MEM_UNLOCK();
}

void mem_reset_stats(void) {
    MEM_LOCK();
    memset(&g_mem_state.stats, 0, sizeof(g_mem_state.stats));
    MEM_UNLOCK();
}

void mem_print_stats(FILE *stream) {
    if (!stream) stream = stderr;
    
    MemStats stats;
    mem_get_stats(&stats);
    
    fprintf(stream, "Memory Statistics:\n");
    fprintf(stream, "  Total allocated: %zu bytes\n", stats.total_allocated);
    fprintf(stream, "  Total freed: %zu bytes\n", stats.total_freed);
    fprintf(stream, "  Currently allocated: %zu bytes\n", stats.current_allocated);
    fprintf(stream, "  Peak allocated: %zu bytes\n", stats.peak_allocated);
    fprintf(stream, "  Allocation count: %zu\n", stats.allocation_count);
    fprintf(stream, "  Free count: %zu\n", stats.free_count);
    fprintf(stream, "  Failed allocations: %zu\n", stats.failed_allocations);
}

void mem_check_leaks(void) {
    if (!g_mem_state.tracking_enabled) {
        gig_warning("Memory tracking is not enabled");
        return;
    }
    
    MEM_LOCK();
    
    if (!g_mem_state.allocation_list) {
        gig_debug("No memory leaks detected");
        MEM_UNLOCK();
        return;
    }
    
    size_t leak_count = 0;
    size_t leak_bytes = 0;
    
    for (MemHeader *header = g_mem_state.allocation_list; header; header = header->next) {
        leak_count++;
        leak_bytes += header->size;
        
        gig_warning("Memory leak: %zu bytes allocated at %s:%d in %s()",
                   header->size, 
                   header->file ? header->file : "unknown",
                   header->line,
                   header->func ? header->func : "unknown");
    }
    
    gig_error("Memory leaks detected: %zu allocations, %zu bytes total", 
              leak_count, leak_bytes);
    
    MEM_UNLOCK();
}

// Error mode management
void mem_set_error_mode(MemErrorMode mode) {
    MEM_LOCK();
    g_mem_state.error_mode = mode;
    MEM_UNLOCK();
}

MemErrorMode mem_get_error_mode(void) {
    return g_mem_state.error_mode;
}

// Memory validation functions
bool mem_is_valid_ptr(const void *ptr) {
    if (!ptr || !g_mem_state.tracking_enabled) {
        return ptr != NULL;
    }
    
    MEM_LOCK();
    
    for (MemHeader *header = g_mem_state.allocation_list; header; header = header->next) {
        void *user_ptr = (char *)header + sizeof(MemHeader);
        if (user_ptr == ptr) {
            bool valid = (header->canary == MEM_CANARY_VALUE);
            if (valid) {
                MemFooter *footer = (MemFooter *)((char *)ptr + header->size);
                valid = (footer->canary == MEM_CANARY_VALUE);
            }
            MEM_UNLOCK();
            return valid;
        }
    }
    
    MEM_UNLOCK();
    return false; // Not found in allocation list
}

size_t mem_get_allocation_size(const void *ptr) {
    if (!ptr || !g_mem_state.tracking_enabled) {
        return 0;
    }
    
    MemHeader *header = (MemHeader *)((char *)ptr - sizeof(MemHeader));
    
    if (header->canary != MEM_CANARY_VALUE) {
        return 0; // Invalid pointer
    }
    
    return header->size;
}

// Aligned allocation support
void *xaligned_alloc(size_t alignment, size_t size) {
    if (!g_mem_state.initialized) mem_init();
    
    // Validate alignment (must be power of 2)
    if (alignment == 0 || (alignment & (alignment - 1)) != 0 || alignment > MEM_MAX_ALIGN) {
        handle_error("aligned_alloc", size, __FILE__, __LINE__, __func__);
        return NULL;
    }
    
    // Align size to alignment boundary
    size = ALIGN_UP(size, alignment);
    
#ifdef HAVE_ALIGNED_ALLOC
    void *ptr = aligned_alloc(alignment, size);
    if (!ptr) {
        handle_error("aligned_alloc", size, __FILE__, __LINE__, __func__);
        return NULL;
    }
    
    if (g_mem_state.tracking_enabled) {
        // Note: tracking aligned allocations is complex due to header placement
        // For now, we skip tracking for aligned allocations
        gig_warning("Memory tracking not supported for aligned allocations");
    }
    
    return ptr;
#else
    // Fallback implementation using overallocation
    size_t total_size = size + alignment + sizeof(void*);
    void *raw_ptr = xmalloc(total_size);
    if (!raw_ptr) return NULL;
    
    // Find aligned position
    uintptr_t addr = (uintptr_t)raw_ptr + sizeof(void*);
    uintptr_t aligned_addr = ALIGN_UP(addr, alignment);
    void *aligned_ptr = (void*)aligned_addr;
    
    // Store original pointer before aligned pointer
    *((void**)aligned_ptr - 1) = raw_ptr;
    
    return aligned_ptr;
#endif
}

// Secure memory functions
void *xmalloc_secure(size_t size) {
    void *ptr = xmalloc(size);
    if (ptr) {
        // Lock pages in memory if possible (platform-specific)
#ifdef HAVE_MLOCK
        if (mlock(ptr, size) != 0) {
            gig_warning("Failed to lock secure memory pages: %s", strerror(errno));
        }
#elif defined(HAVE_VIRTUALLOCK)
        if (!VirtualLock(ptr, size)) {
            gig_warning("Failed to lock secure memory pages: error %lu", GetLastError());
        }
#endif
    }
    return ptr;
}

void xfree_secure(void *ptr, size_t size) {
    if (!ptr) return;
    
    // Clear memory securely
    secure_memset(ptr, 0, size);
    
#ifdef HAVE_MLOCK
    munlock(ptr, size);
#elif defined(HAVE_VIRTUALLOCK)
    VirtualUnlock(ptr, size);
#endif
    
    xfree(ptr);
}

char *xstrdup_secure(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    char *secure_str = xmalloc_secure(len + 1);
    if (secure_str) {
        memcpy(secure_str, str, len + 1);
    }
    return secure_str;
}

// Memory pool implementation
MemPool *mem_pool_create(size_t block_size, size_t initial_blocks) {
    if (block_size == 0 || initial_blocks == 0) return NULL;
    
    MemPool *pool = XNEW(MemPool);
    if (!pool) return NULL;
    
    pool->block_size = ALIGN_UP(block_size, sizeof(void*));
    pool->blocks_per_chunk = initial_blocks;
    pool->chunk_size = pool->block_size * pool->blocks_per_chunk;
    pool->current_chunk = NULL;
    pool->current_offset = pool->chunk_size; // Force allocation of first chunk
    pool->chunks = NULL;
    pool->num_chunks = 0;
    pool->max_chunks = 0;
    
    return pool;
}

void *mem_pool_alloc(MemPool *pool, size_t size) {
    if (!pool || size == 0 || size > pool->block_size) return NULL;
    
    // Check if we need a new chunk
    if (pool->current_offset + pool->block_size > pool->chunk_size) {
        // Allocate new chunk
        void *new_chunk = xmalloc(pool->chunk_size);
        if (!new_chunk) return NULL;
        
        // Expand chunks array if needed
        if (pool->num_chunks >= pool->max_chunks) {
            size_t new_max = pool->max_chunks ? pool->max_chunks * 2 : 8;
            void **new_chunks = xrealloc_array(pool->chunks, new_max, sizeof(void*));
            if (!new_chunks) {
                xfree(new_chunk);
                return NULL;
            }
            pool->chunks = new_chunks;
            pool->max_chunks = new_max;
        }
        
        pool->chunks[pool->num_chunks++] = new_chunk;
        pool->current_chunk = new_chunk;
        pool->current_offset = 0;
    }
    
    void *ptr = (char*)pool->current_chunk + pool->current_offset;
    pool->current_offset += pool->block_size;
    
    return ptr;
}

void mem_pool_free_all(MemPool *pool) {
    if (!pool) return;
    
    pool->current_offset = pool->chunk_size; // Reset to force new chunk allocation
    // Note: We don't actually free the chunks here for performance
    // They'll be reused for future allocations
}

void mem_pool_destroy(MemPool *pool) {
    if (!pool) return;
    
    // Free all chunks
    for (size_t i = 0; i < pool->num_chunks; i++) {
        xfree(pool->chunks[i]);
    }
    
    xfree(pool->chunks);
    xfree(pool);
}

// Utility functions
bool mem_has_atomic_support(void) {
#ifdef __STDC_NO_ATOMICS__
    return false;
#else
    return true;
#endif
}

