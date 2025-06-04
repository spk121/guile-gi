// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced string utilities with safety, performance, and cross-platform support

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
#if defined(__GLIBC__) || defined(__NEWLIB__)
#define _GNU_SOURCE
#define HAVE_STRCASESTR 1
#define HAVE_STRCHRNUL 1
#elif defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
#define _CRT_SECURE_NO_WARNINGS
#define HAVE_STRICMP 1
#else
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700
#endif

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
#include <errno.h>

// Platform-specific includes
#if defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
#include <windows.h>
#include <shlwapi.h>
#else
#include <libgen.h>
#endif

#include "str.h"
#include "mem.h"
#include "logging.h"

// String builder implementation
struct string_builder {
    char *data;
    size_t length;
    size_t capacity;
    bool owns_data;
};

// String interning implementation
#define INTERN_TABLE_SIZE 1024

typedef struct intern_entry {
    char *string;
    size_t length;
    uint32_t hash;
    struct intern_entry *next;
} InternEntry;

struct string_intern {
    InternEntry *table[INTERN_TABLE_SIZE];
    size_t count;
    size_t memory_used;
};

// Global error handling state
static struct {
    StrErrorMode error_mode;
    StrErrorFunc error_handler;
} g_str_state = {
    .error_mode = STR_ERROR_ABORT,
    .error_handler = NULL
};

// Cross-platform function implementations
#if defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)

#ifndef HAVE_STRCASESTR
static char *strcasestr(const char *haystack, const char *needle) {
    if (!needle || !*needle) return (char *)haystack;
    
    size_t needle_len = strlen(needle);
    while (*haystack) {
        if (strnicmp(haystack, needle, needle_len) == 0) {
            return (char *)haystack;
        }
        haystack++;
    }
    return NULL;
}
#endif

#ifndef HAVE_STRCHRNUL
static char *strchrnul(const char *s, int c) {
    while (*s && *s != c) s++;
    return (char *)s;
}
#endif

// Windows path separator handling
#define PATH_SEPARATOR '\\'
#define PATH_SEPARATOR_STR "\\"
#define IS_PATH_SEPARATOR(c) ((c) == '\\' || (c) == '/')

#else // Unix-like systems

#define PATH_SEPARATOR '/'
#define PATH_SEPARATOR_STR "/"
#define IS_PATH_SEPARATOR(c) ((c) == '/')

// Provide strcasestr if not available
#ifndef HAVE_STRCASESTR
static char *strcasestr(const char *haystack, const char *needle) {
    if (!needle || !*needle) return (char *)haystack;
    
    size_t needle_len = strlen(needle);
    while (*haystack) {
        if (strncasecmp(haystack, needle, needle_len) == 0) {
            return (char *)haystack;
        }
        haystack++;
    }
    return NULL;
}
#endif

#endif

// Error handling functions
static void handle_string_error(const char *operation, const char *message) {
    switch (g_str_state.error_mode) {
        case STR_ERROR_ABORT:
            gig_error("String operation '%s' failed: %s", operation, message);
            abort();
            break;
            
        case STR_ERROR_RETURN:
            // Just return - caller handles NULL
            break;
            
        case STR_ERROR_CALLBACK:
            if (g_str_state.error_handler) {
                g_str_state.error_handler(operation, message);
            }
            break;
    }
}

// Helper function for safe string allocation
static char *safe_string_alloc(size_t size, const char *operation) {
    char *result = xmalloc(size);
    if (!result) {
        handle_string_error(operation, "memory allocation failed");
        return NULL;
    }
    return result;
}

// Overflow-safe string length calculation
static bool safe_add_lengths(size_t *result, size_t a, size_t b) {
    if (a > SIZE_MAX - b) {
        return false; // Overflow
    }
    *result = a + b;
    return true;
}

// ============================================================================
// Original functions (modernized)
// ============================================================================

char *bracketize(const char *str) {
    if (!str || !*str) {
        return NULL;
    }
    
    size_t str_len = strlen(str);
    size_t total_len;
    if (!safe_add_lengths(&total_len, str_len, 3)) { // 2 for "<>" + 1 for null terminator
        handle_string_error("bracketize", "string too long");
        return NULL;
    }
    
    char *result = safe_string_alloc(total_len, "bracketize");
    if (!result) return NULL;
    
    snprintf(result, total_len, "<%s>", str);
    return result;
}

char *concatenate(const char *str1, const char *str2) {
    if (!str1 && !str2) {
        return NULL;
    }
    if (!str1) {
        return xstrdup(str2);
    }
    if (!str2) {
        return xstrdup(str1);
    }
    
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    size_t total_len;
    
    if (!safe_add_lengths(&total_len, len1, len2 + 1)) {
        handle_string_error("concatenate", "combined string too long");
        return NULL;
    }
    
    char *result = safe_string_alloc(total_len, "concatenate");
    if (!result) return NULL;
    
    memcpy(result, str1, len1);
    memcpy(result + len1, str2, len2 + 1); // Include null terminator
    
    return result;
}

char *concatenate3(const char *str1, const char *str2, const char *str3) {
    // Use the more efficient variadic version
    return str_concat(str1, str2, str3, NULL);
}

char *concatenate4(const char *str1, const char *str2, const char *str3, const char *str4) {
    // Use the more efficient variadic version
    return str_concat(str1, str2, str3, str4, NULL);
}

char *decorate_string(const char *fmt, const char *str) {
    if (!fmt || !str) {
        return NULL;
    }
    
    // Calculate required length
    int len = snprintf(NULL, 0, fmt, str);
    if (len < 0) {
        handle_string_error("decorate_string", "format string error");
        return NULL;
    }
    
    char *result = safe_string_alloc(len + 1, "decorate_string");
    if (!result) return NULL;
    
    snprintf(result, len + 1, fmt, str);
    return result;
}

char *make_scm_name(const char *gname) {
    if (!gname || !*gname) {
        handle_string_error("make_scm_name", "input string is null or empty");
        return NULL;
    }
    
    size_t len = strlen(gname);
    // Worst case: every character could need a dash prefix
    char *result = safe_string_alloc(2 * len + 1, "make_scm_name");
    if (!result) return NULL;
    
    bool was_lower = false;
    size_t j = 0;
    
    for (size_t i = 0; i < len; i++) {
        char ch = gname[i];
        
        if (islower(ch)) {
            result[j++] = ch;
            was_lower = true;
        } else if (ch == '_' || ch == '-') {
            result[j++] = '-';
            was_lower = false;
        } else if (ch == '?' || ch == ':' || ch == '%') {
            result[j++] = ch;
            was_lower = false;
        } else if (isdigit(ch)) {
            result[j++] = ch;
            was_lower = false;
        } else if (isupper(ch)) {
            if (was_lower) {
                result[j++] = '-';
            }
            result[j++] = tolower(ch);
            was_lower = false;
        }
        // Skip other characters
    }
    
    result[j] = '\0';
    
    // Resize to actual length to save memory
    char *final_result = xrealloc(result, j + 1);
    return final_result ? final_result : result;
}

const char *skip_prefix(const char *name, const char *prefix) {
    if (!name || !prefix) {
        return name;
    }
    
    size_t prefix_len = strlen(prefix);
    
    // Check if name starts with prefix
    for (size_t i = 0; i < prefix_len; i++) {
        if (name[i] != prefix[i] && name[i] != '_') {
            return &name[i];
        }
    }
    
    // Strip off prefix from value name, while keeping it a valid identifier
    for (size_t i = prefix_len + 1; i > 0; i--) {
        if (isalpha(name[i - 1]) || name[i - 1] == '_') {
            return &name[i - 1];
        }
    }
    
    return name;
}

size_t strvlen(const char * const *x) {
    if (!x) return 0;
    
    size_t count = 0;
    while (x[count] != NULL) {
        count++;
    }
    return count;
}

// ============================================================================
// Enhanced string functions
// ============================================================================

char *str_concat(const char *first, ...) {
    if (!first) return NULL;
    
    va_list args;
    va_start(args, first);
    
    // First pass: calculate total length
    size_t total_len = strlen(first);
    const char *str;
    
    while ((str = va_arg(args, const char *)) != NULL) {
        size_t str_len = strlen(str);
        if (!safe_add_lengths(&total_len, total_len, str_len)) {
            va_end(args);
            handle_string_error("str_concat", "combined string too long");
            return NULL;
        }
    }
    va_end(args);
    
    // Allocate result buffer
    char *result = safe_string_alloc(total_len + 1, "str_concat");
    if (!result) return NULL;
    
    // Second pass: copy strings
    size_t pos = 0;
    size_t first_len = strlen(first);
    memcpy(result + pos, first, first_len);
    pos += first_len;
    
    va_start(args, first);
    while ((str = va_arg(args, const char *)) != NULL) {
        size_t str_len = strlen(str);
        memcpy(result + pos, str, str_len);
        pos += str_len;
    }
    va_end(args);
    
    result[total_len] = '\0';
    return result;
}

char *str_concatv(const char * const *strings) {
    if (!strings || !strings[0]) return NULL;
    
    // Calculate total length
    size_t total_len = 0;
    for (size_t i = 0; strings[i]; i++) {
        size_t str_len = strlen(strings[i]);
        if (!safe_add_lengths(&total_len, total_len, str_len)) {
            handle_string_error("str_concatv", "combined string too long");
            return NULL;
        }
    }
    
    char *result = safe_string_alloc(total_len + 1, "str_concatv");
    if (!result) return NULL;
    
    // Copy strings
    size_t pos = 0;
    for (size_t i = 0; strings[i]; i++) {
        size_t str_len = strlen(strings[i]);
        memcpy(result + pos, strings[i], str_len);
        pos += str_len;
    }
    
    result[total_len] = '\0';
    return result;
}

// Safe versions that return NULL instead of aborting
char *safe_bracketize(const char *str) {
    StrErrorMode old_mode = g_str_state.error_mode;
    g_str_state.error_mode = STR_ERROR_RETURN;
    char *result = bracketize(str);
    g_str_state.error_mode = old_mode;
    return result;
}

char *safe_concatenate(const char *str1, const char *str2) {
    StrErrorMode old_mode = g_str_state.error_mode;
    g_str_state.error_mode = STR_ERROR_RETURN;
    char *result = concatenate(str1, str2);
    g_str_state.error_mode = old_mode;
    return result;
}

char *safe_decorate_string(const char *fmt, const char *str) {
    StrErrorMode old_mode = g_str_state.error_mode;
    g_str_state.error_mode = STR_ERROR_RETURN;
    char *result = decorate_string(fmt, str);
    g_str_state.error_mode = old_mode;
    return result;
}

// String formatting functions
char *str_printf(const char *fmt, ...) {
    if (!fmt) return NULL;
    
    va_list args;
    va_start(args, fmt);
    char *result = str_vprintf(fmt, args);
    va_end(args);
    
    return result;
}

char *str_vprintf(const char *fmt, va_list args) {
    if (!fmt) return NULL;
    
    va_list args_copy;
    va_copy(args_copy, args);
    
    int len = vsnprintf(NULL, 0, fmt, args);
    if (len < 0) {
        va_end(args_copy);
        handle_string_error("str_vprintf", "format string error");
        return NULL;
    }
    
    char *result = safe_string_alloc(len + 1, "str_vprintf");
    if (!result) {
        va_end(args_copy);
        return NULL;
    }
    
    vsnprintf(result, len + 1, fmt, args_copy);
    va_end(args_copy);
    
    return result;
}

// String trimming functions
char *str_trim(const char *str) {
    return str_trim_chars(str, " \t\n\r\f\v");
}

char *str_trim_left(const char *str) {
    if (!str) return NULL;
    
    // Skip leading whitespace
    while (*str && isspace((unsigned char)*str)) {
        str++;
    }
    
    return xstrdup(str);
}

char *str_trim_right(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    if (len == 0) return xstrdup("");
    
    // Find last non-whitespace character
    const char *end = str + len - 1;
    while (end > str && isspace((unsigned char)*end)) {
        end--;
    }
    
    size_t trimmed_len = end - str + 1;
    char *result = safe_string_alloc(trimmed_len + 1, "str_trim_right");
    if (!result) return NULL;
    
    memcpy(result, str, trimmed_len);
    result[trimmed_len] = '\0';
    
    return result;
}

char *str_trim_chars(const char *str, const char *chars) {
    if (!str || !chars) return str ? xstrdup(str) : NULL;
    
    const char *end = str + strlen(str);
    
    // Skip leading characters to trim
    while (*str && strchr(chars, *str)) {
        str++;
    }
    
    size_t len = strlen(str);
    if (len == 0) return xstrdup("");
    
    // Find last character not in trim set
    size_t trimmed_len = end - str + 1;
    char *result = safe_string_alloc(trimmed_len + 1, "str_trim_chars");
    if (!result) return NULL;
    
    memcpy(result, str, trimmed_len);
    result[trimmed_len] = '\0';
    
    return result;
}

// String case conversion functions
char *str_upper(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    char *result = safe_string_alloc(len + 1, "str_upper");
    if (!result) return NULL;
    
    for (size_t i = 0; i <= len; i++) {
        result[i] = toupper((unsigned char)str[i]);
    }
    
    return result;
}

char *str_lower(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    char *result = safe_string_alloc(len + 1, "str_lower");
    if (!result) return NULL;
    
    for (size_t i = 0; i <= len; i++) {
        result[i] = tolower((unsigned char)str[i]);
    }
    
    return result;
}

char *str_capitalize(const char *str) {
    if (!str || !*str) return str ? xstrdup(str) : NULL;
    
    char *result = str_lower(str);
    if (result && *result) {
        result[0] = toupper((unsigned char)result[0]);
    }
    
    return result;
}

char *str_title_case(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    char *result = safe_string_alloc(len + 1, "str_title_case");
    if (!result) return NULL;
    
    bool capitalize_next = true;
    for (size_t i = 0; i <= len; i++) {
        if (str[i] == '\0') {
            result[i] = '\0';
            break;
        } else if (isalpha((unsigned char)str[i])) {
            result[i] = capitalize_next ? toupper((unsigned char)str[i]) : tolower((unsigned char)str[i]);
            capitalize_next = false;
        } else {
            result[i] = str[i];
            capitalize_next = !isalnum((unsigned char)str[i]);
        }
    }
    
    return result;
}

// String manipulation functions
char *str_reverse(const char *str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    char *result = safe_string_alloc(len + 1, "str_reverse");
    if (!result) return NULL;
    
    for (size_t i = 0; i < len; i++) {
        result[i] = str[len - 1 - i];
    }
    result[len] = '\0';
    
    return result;
}

char *str_repeat(const char *str, size_t count) {
    if (!str || count == 0) return xstrdup("");
    
    size_t str_len = strlen(str);
    size_t total_len;
    
    if (str_len == 0) return xstrdup("");
    
    // Check for overflow
    if (count > SIZE_MAX / str_len) {
        handle_string_error("str_repeat", "result would be too large");
        return NULL;
    }
    
    total_len = str_len * count;
    char *result = safe_string_alloc(total_len + 1, "str_repeat");
    if (!result) return NULL;
    
    for (size_t i = 0; i < count; i++) {
        memcpy(result + i * str_len, str, str_len);
    }
    result[total_len] = '\0';
    
    return result;
}

char *str_pad_left(const char *str, size_t width, char pad_char) {
    if (!str) return NULL;
    
    size_t str_len = strlen(str);
    if (str_len >= width) return xstrdup(str);
    
    size_t pad_len = width - str_len;
    char *result = safe_string_alloc(width + 1, "str_pad_left");
    if (!result) return NULL;
    
    memset(result, pad_char, pad_len);
    memcpy(result + pad_len, str, str_len);
    result[width] = '\0';
    
    return result;
}

char *str_pad_right(const char *str, size_t width, char pad_char) {
    if (!str) return NULL;
    
    size_t str_len = strlen(str);
    if (str_len >= width) return xstrdup(str);
    
    size_t pad_len = width - str_len;
    char *result = safe_string_alloc(width + 1, "str_pad_right");
    if (!result) return NULL;
    
    memcpy(result, str, str_len);
    memset(result + str_len, pad_char, pad_len);
    result[width] = '\0';
    
    return result;
}

char *str_pad_center(const char *str, size_t width, char pad_char) {
    if (!str) return NULL;
    
    size_t str_len = strlen(str);
    if (str_len >= width) return xstrdup(str);
    
    size_t total_pad = width - str_len;
    size_t left_pad = total_pad / 2;
    size_t right_pad = total_pad - left_pad;
    
    char *result = safe_string_alloc(width + 1, "str_pad_center");
    if (!result) return NULL;
    
    memset(result, pad_char, left_pad);
    memcpy(result + left_pad, str, str_len);
    memset(result + left_pad + str_len, pad_char, right_pad);
    result[width] = '\0';
    
    return result;
}

// String searching and comparison functions
bool str_starts_with(const char *str, const char *prefix) {
    if (!str || !prefix) return false;
    
    size_t prefix_len = strlen(prefix);
    return strncmp(str, prefix, prefix_len) == 0;
}

bool str_ends_with(const char *str, const char *suffix) {
    if (!str || !suffix) return false;
    
    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);
    
    if (suffix_len > str_len) return false;
    
    return strcmp(str + str_len - suffix_len, suffix) == 0;
}

bool str_contains(const char *str, const char *substring) {
    if (!str || !substring) return false;
    return strstr(str, substring) != NULL;
}

bool str_equals(const char *str1, const char *str2) {
    if (str1 == str2) return true;
    if (!str1 || !str2) return false;
    return strcmp(str1, str2) == 0;
}

bool str_equals_ignore_case(const char *str1, const char *str2) {
    if (str1 == str2) return true;
    if (!str1 || !str2) return false;
    
#if defined(HAVE_STRICMP)
    return stricmp(str1, str2) == 0;
#else
    return strcasecmp(str1, str2) == 0;
#endif
}

// String replacement functions
char *str_replace(const char *str, const char *old_substr, const char *new_substr) {
    if (!str || !old_substr) return str ? xstrdup(str) : NULL;
    if (!new_substr) new_substr = "";
    
    const char *pos = strstr(str, old_substr);
    if (!pos) return xstrdup(str);
    
    size_t old_len = strlen(old_substr);
    size_t new_len = strlen(new_substr);
    size_t prefix_len = pos - str;
    size_t suffix_len = strlen(pos + old_len);
    
    size_t result_len = prefix_len + new_len + suffix_len;
    char *result = safe_string_alloc(result_len + 1, "str_replace");
    if (!result) return NULL;
    
    memcpy(result, str, prefix_len);
    memcpy(result + prefix_len, new_substr, new_len);
    memcpy(result + prefix_len + new_len, pos + old_len, suffix_len);
    result[result_len] = '\0';
    
    return result;
}

char *str_replace_all(const char *str, const char *old_substr, const char *new_substr) {
    if (!str || !old_substr || strlen(old_substr) == 0) {
        return str ? xstrdup(str) : NULL;
    }
    if (!new_substr) new_substr = "";
    
    // Count occurrences
    size_t count = str_count_occurrences(str, old_substr);
    if (count == 0) return xstrdup(str);
    
    size_t old_len = strlen(old_substr);
    size_t new_len = strlen(new_substr);
    size_t str_len = strlen(str);
    
    // Calculate result length
    size_t result_len = str_len + count * (new_len - old_len);
    if (new_len > old_len && result_len < str_len) {
        // Overflow check
        handle_string_error("str_replace_all", "result would be too large");
        return NULL;
    }
    
    char *result = safe_string_alloc(result_len + 1, "str_replace_all");
    if (!result) return NULL;
    
    const char *src = str;
    char *dst = result;
    const char *pos;
    
    while ((pos = strstr(src, old_substr)) != NULL) {
        size_t prefix_len = pos - src;
        memcpy(dst, src, prefix_len);
        dst += prefix_len;
        
        memcpy(dst, new_substr, new_len);
        dst += new_len;
        
        src = pos + old_len;
    }
    
    // Copy remaining string
    strcpy(dst, src);
    
    return result;
}

char *str_replace_char(const char *str, char old_char, char new_char) {
    if (!str) return NULL;
    
    char *result = xstrdup(str);
    if (!result) return NULL;
    
    for (char *p = result; *p; p++) {
        if (*p == old_char) {
            *p = new_char;
        }
    }
    
    return result;
}

// String validation functions
bool str_is_empty(const char *str) {
    return !str || *str == '\0';
}

bool str_is_whitespace(const char *str) {
    if (!str) return true;
    
    while (*str) {
        if (!isspace((unsigned char)*str)) {
            return false;
        }
        str++;
    }
    return true;
}

bool str_is_numeric(const char *str) {
    if (!str || !*str) return false;
    
    // Skip leading whitespace
    while (isspace((unsigned char)*str)) str++;
    
    // Check for optional sign
    if (*str == '+' || *str == '-') str++;
    
    bool has_digits = false;
    bool has_decimal = false;
    
    while (*str) {
        if (isdigit((unsigned char)*str)) {
            has_digits = true;
        } else if (*str == '.' && !has_decimal) {
            has_decimal = true;
        } else if (isspace((unsigned char)*str)) {
            // Allow trailing whitespace
            while (*str && isspace((unsigned char)*str)) str++;
            break;
        } else {
            return false;
        }
        str++;
    }
    
    return has_digits;
}

bool str_is_integer(const char *str) {
    if (!str || !*str) return false;
    
    // Skip leading whitespace
    while (isspace((unsigned char)*str)) str++;
    
    // Check for optional sign
    if (*str == '+' || *str == '-') str++;
    
    bool has_digits = false;
    while (*str) {
        if (isdigit((unsigned char)*str)) {
            has_digits = true;
        } else if (isspace((unsigned char)*str)) {
            // Allow trailing whitespace
            while (*str && isspace((unsigned char)*str)) str++;
            break;
        } else {
            return false;
        }
        str++;
    }
    
    return has_digits;
}

// String utility functions  
size_t str_count_occurrences(const char *str, const char *substring) {
    if (!str || !substring || strlen(substring) == 0) return 0;
    
    size_t count = 0;
    size_t substr_len = strlen(substring);
    const char *pos = str;
    
    while ((pos = strstr(pos, substring)) != NULL) {
        count++;
        pos += substr_len;
    }
    
    return count;
}

// String builder implementation
StringBuilder *sb_create(size_t initial_capacity) {
    if (initial_capacity == 0) initial_capacity = 64;
    
    StringBuilder *sb = XNEW(StringBuilder);
    if (!sb) return NULL;
    
    sb->data = xmalloc(initial_capacity);
    if (!sb->data) {
        xfree(sb);
        return NULL;
    }
    
    sb->data[0] = '\0';
    sb->length = 0;
    sb->capacity = initial_capacity;
    sb->owns_data = true;
    
    return sb;
}

void sb_destroy(StringBuilder *sb) {
    if (!sb) return;
    
    if (sb->owns_data && sb->data) {
        xfree(sb->data);
    }
    xfree(sb);
}

bool sb_append(StringBuilder *sb, const char *str) {
    if (!sb || !str) return false;
    
    size_t str_len = strlen(str);
    if (str_len == 0) return true;
    
    size_t required = sb->length + str_len + 1;
    if (required > sb->capacity) {
        size_t new_capacity = sb->capacity;
        while (new_capacity < required) {
            new_capacity *= 2;
        }
        
        char *new_data = xrealloc(sb->data, new_capacity);
        if (!new_data) return false;
        
        sb->data = new_data;
        sb->capacity = new_capacity;
    }
    
    memcpy(sb->data + sb->length, str, str_len);
    sb->length += str_len;
    sb->data[sb->length] = '\0';
    
    return true;
}

bool sb_append_char(StringBuilder *sb, char ch) {
    char str[2] = {ch, '\0'};
    return sb_append(sb, str);
}

const char *sb_get_string(const StringBuilder *sb) {
    return sb ? sb->data : NULL;
}

char *sb_to_string(StringBuilder *sb) {
    if (!sb || !sb->data) return NULL;
    
    char *result = xstrdup(sb->data);
    return result;
}

size_t sb_length(const StringBuilder *sb) {
    return sb ? sb->length : 0;
}

void sb_clear(StringBuilder *sb) {
    if (!sb) return;
    
    sb->length = 0;
    if (sb->data) {
        sb->data[0] = '\0';
    }
}

// Hash functions
uint32_t str_hash_djb2(const char *str) {
    if (!str) return 0;
    
    uint32_t hash = 5381;
    while (*str) {
        hash = ((hash << 5) + hash) + (unsigned char)*str;
        str++;
    }
    return hash;
}

uint32_t str_hash_fnv1a(const char *str) {
    if (!str) return 0;
    
    uint32_t hash = 2166136261u;
    while (*str) {
        hash ^= (unsigned char)*str;
        hash *= 16777619u;
        str++;
    }
    return hash;
}

// Configuration functions
void str_set_error_mode(StrErrorMode mode) {
    g_str_state.error_mode = mode;
}

void str_set_error_handler(StrErrorFunc handler) {
    g_str_state.error_handler = handler;
}

StrErrorMode str_get_error_mode(void) {
    return g_str_state.error_mode;
}
