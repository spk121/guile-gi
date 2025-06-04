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

#ifndef CORE_STRING_H
#define CORE_STRING_H

#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdint.h>
#include "visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations for better type safety
typedef struct string_builder StringBuilder;

// Original functions (modernized with better error handling)
GIG_API char *bracketize(const char *str) GIG_MALLOC;
GIG_API char *concatenate(const char *str1, const char *str2) GIG_MALLOC;
GIG_API char *concatenate3(const char *str1, const char *str2, const char *str3) GIG_MALLOC;
GIG_API char *concatenate4(const char *str1, const char *str2, const char *str3, const char *str4) GIG_MALLOC;
GIG_API char *decorate_string(const char *fmt, const char *str) GIG_MALLOC GIG_PRINTF(1, 0);
GIG_API char *make_scm_name(const char *gname) GIG_MALLOC;
GIG_API const char *skip_prefix(const char *name, const char *prefix) GIG_PURE;
GIG_API size_t strvlen(const char * const *x) GIG_PURE;

// Enhanced concatenation with variadic arguments
GIG_API char *str_concat(const char *first, ...) GIG_MALLOC;
GIG_API char *str_concatv(const char * const *strings) GIG_MALLOC;

// Safe versions that return NULL instead of aborting
GIG_API char *safe_bracketize(const char *str) GIG_MALLOC;
GIG_API char *safe_concatenate(const char *str1, const char *str2) GIG_MALLOC;
GIG_API char *safe_decorate_string(const char *fmt, const char *str) GIG_MALLOC GIG_PRINTF(1, 0);

// String formatting functions
GIG_API char *str_printf(const char *fmt, ...) GIG_MALLOC GIG_PRINTF(1, 2);
GIG_API char *str_vprintf(const char *fmt, va_list args) GIG_MALLOC GIG_PRINTF(1, 0);

// String manipulation functions
GIG_API char *str_trim(const char *str) GIG_MALLOC;
GIG_API char *str_trim_left(const char *str) GIG_MALLOC;
GIG_API char *str_trim_right(const char *str) GIG_MALLOC;
GIG_API char *str_trim_chars(const char *str, const char *chars) GIG_MALLOC;

GIG_API char *str_upper(const char *str) GIG_MALLOC;
GIG_API char *str_lower(const char *str) GIG_MALLOC;
GIG_API char *str_capitalize(const char *str) GIG_MALLOC;
GIG_API char *str_title_case(const char *str) GIG_MALLOC;

GIG_API char *str_reverse(const char *str) GIG_MALLOC;
GIG_API char *str_repeat(const char *str, size_t count) GIG_MALLOC;
GIG_API char *str_pad_left(const char *str, size_t width, char pad_char) GIG_MALLOC;
GIG_API char *str_pad_right(const char *str, size_t width, char pad_char) GIG_MALLOC;
GIG_API char *str_pad_center(const char *str, size_t width, char pad_char) GIG_MALLOC;

// String searching and comparison
GIG_API bool str_starts_with(const char *str, const char *prefix) GIG_PURE;
GIG_API bool str_ends_with(const char *str, const char *suffix) GIG_PURE;
GIG_API bool str_contains(const char *str, const char *substring) GIG_PURE;
GIG_API bool str_equals(const char *str1, const char *str2) GIG_PURE;
GIG_API bool str_equals_ignore_case(const char *str1, const char *str2) GIG_PURE;

GIG_API int str_compare_natural(const char *str1, const char *str2) GIG_PURE;
GIG_API size_t str_count_occurrences(const char *str, const char *substring) GIG_PURE;
GIG_API const char *str_find_nth(const char *str, const char *substring, size_t n) GIG_PURE;

// String replacement and substitution
GIG_API char *str_replace(const char *str, const char *old_substr, const char *new_substr) GIG_MALLOC;
GIG_API char *str_replace_all(const char *str, const char *old_substr, const char *new_substr) GIG_MALLOC;
GIG_API char *str_replace_char(const char *str, char old_char, char new_char) GIG_MALLOC;
GIG_API char *str_remove_char(const char *str, char ch) GIG_MALLOC;
GIG_API char *str_remove_chars(const char *str, const char *chars) GIG_MALLOC;

// String splitting and joining
GIG_API char **str_split(const char *str, const char *delimiter, size_t *count) GIG_MALLOC;
GIG_API char **str_split_lines(const char *str, size_t *count) GIG_MALLOC;
GIG_API char **str_split_whitespace(const char *str, size_t *count) GIG_MALLOC;
GIG_API char *str_join(const char * const *strings, const char *separator) GIG_MALLOC;
GIG_API void str_free_array(char **strings, size_t count);

// String validation and classification
GIG_API bool str_is_empty(const char *str) GIG_PURE;
GIG_API bool str_is_whitespace(const char *str) GIG_PURE;
GIG_API bool str_is_numeric(const char *str) GIG_PURE;
GIG_API bool str_is_integer(const char *str) GIG_PURE;
GIG_API bool str_is_alpha(const char *str) GIG_PURE;
GIG_API bool str_is_alnum(const char *str) GIG_PURE;
GIG_API bool str_is_ascii(const char *str) GIG_PURE;

// String encoding and escaping
GIG_API char *str_escape_c(const char *str) GIG_MALLOC;
GIG_API char *str_unescape_c(const char *str) GIG_MALLOC;
GIG_API char *str_escape_html(const char *str) GIG_MALLOC;
GIG_API char *str_escape_xml(const char *str) GIG_MALLOC;
GIG_API char *str_escape_json(const char *str) GIG_MALLOC;
GIG_API char *str_escape_regex(const char *str) GIG_MALLOC;

// Path and filename utilities
GIG_API char *str_basename(const char *path) GIG_MALLOC;
GIG_API char *str_dirname(const char *path) GIG_MALLOC;
GIG_API char *str_extension(const char *filename) GIG_MALLOC;
GIG_API char *str_remove_extension(const char *filename) GIG_MALLOC;
GIG_API char *str_normalize_path(const char *path) GIG_MALLOC;
GIG_API char *str_join_path(const char *dir, const char *filename) GIG_MALLOC;

// String builder for efficient concatenation
GIG_API StringBuilder *sb_create(size_t initial_capacity);
GIG_API StringBuilder *sb_create_from_string(const char *str);
GIG_API void sb_destroy(StringBuilder *sb);

GIG_API bool sb_append(StringBuilder *sb, const char *str);
GIG_API bool sb_append_char(StringBuilder *sb, char ch);
GIG_API bool sb_append_printf(StringBuilder *sb, const char *fmt, ...) GIG_PRINTF(2, 3);
GIG_API bool sb_append_vprintf(StringBuilder *sb, const char *fmt, va_list args) GIG_PRINTF(2, 0);
GIG_API bool sb_prepend(StringBuilder *sb, const char *str);

GIG_API const char *sb_get_string(const StringBuilder *sb) GIG_PURE;
GIG_API char *sb_to_string(StringBuilder *sb) GIG_MALLOC;
GIG_API size_t sb_length(const StringBuilder *sb) GIG_PURE;
GIG_API size_t sb_capacity(const StringBuilder *sb) GIG_PURE;
GIG_API void sb_clear(StringBuilder *sb);
GIG_API bool sb_reserve(StringBuilder *sb, size_t capacity);

// Hash functions for strings
GIG_API uint32_t str_hash_djb2(const char *str) GIG_PURE;
GIG_API uint32_t str_hash_fnv1a(const char *str) GIG_PURE;
GIG_API uint64_t str_hash_sip(const char *str, const uint8_t key[16]) GIG_PURE;

// Unicode and UTF-8 utilities (basic support)
GIG_API bool str_is_valid_utf8(const char *str) GIG_PURE;
GIG_API size_t str_utf8_length(const char *str) GIG_PURE;
GIG_API char *str_utf8_substring(const char *str, size_t start, size_t length) GIG_MALLOC;

// Memory-safe string operations
GIG_API size_t str_copy(char *dest, size_t dest_size, const char *src);
GIG_API size_t str_concat_safe(char *dest, size_t dest_size, const char *src);
GIG_API int str_compare_len(const char *str1, const char *str2, size_t max_len) GIG_PURE;

// String interning (for memory efficiency with repeated strings)
typedef struct string_intern StringIntern;
GIG_API StringIntern *str_intern_create(void);
GIG_API void str_intern_destroy(StringIntern *intern);
GIG_API const char *str_intern_add(StringIntern *intern, const char *str);
GIG_API void str_intern_stats(const StringIntern *intern, size_t *count, size_t *memory_used);

// Error handling configuration
typedef enum {
    STR_ERROR_ABORT,    // Default: abort on error
    STR_ERROR_RETURN,   // Return NULL on error
    STR_ERROR_CALLBACK  // Call custom error handler
} StrErrorMode;

typedef void (*StrErrorFunc)(const char *operation, const char *message);

GIG_API void str_set_error_mode(StrErrorMode mode);
GIG_API void str_set_error_handler(StrErrorFunc handler);
GIG_API StrErrorMode str_get_error_mode(void);

// Utility macros for common operations
#define STR_EMPTY(s) ((s) == NULL || *(s) == '\0')
#define STR_EQUALS(s1, s2) (str_equals((s1), (s2)))
#define STR_STARTS_WITH(s, prefix) (str_starts_with((s), (prefix)))
#define STR_ENDS_WITH(s, suffix) (str_ends_with((s), (suffix)))

// Convenient macros for string building
#define STR_BUILD_BEGIN() sb_create(256)
#define STR_BUILD_END(sb) sb_to_string(sb); sb_destroy(sb)

// Deprecated aliases for backward compatibility
GIG_DEPRECATED_API char *string_concat(const char *str1, const char *str2);
#define string_concat(s1, s2) concatenate((s1), (s2))

#ifdef __cplusplus
}
#endif

#endif // CORE_STRING_H
